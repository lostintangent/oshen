//! Job control: background and stopped job management
//!
//! Manages the lifecycle of background (&) and stopped (Ctrl-Z) jobs.
//! Jobs are tracked in a fixed-size table with unique IDs.

const std = @import("std");
const io = @import("../terminal/io.zig");

/// Job status
pub const JobStatus = enum {
    running,
    stopped,
    done,

    pub fn str(self: JobStatus) []const u8 {
        return switch (self) {
            .running => "Running",
            .stopped => "Stopped",
            .done => "Done",
        };
    }
};

/// A background or stopped job
pub const Job = struct {
    id: u16,
    pgid: std.posix.pid_t,
    pids: []std.posix.pid_t,
    cmd: []const u8,
    status: JobStatus,
};

/// Maximum number of concurrent jobs.
/// This limit prevents unbounded memory growth while supporting typical
/// interactive usage (most users have fewer than 10 concurrent jobs).
const MAX_JOBS = 64;

/// Job table for managing background and stopped jobs
pub const JobTable = struct {
    allocator: std.mem.Allocator,
    jobs: [MAX_JOBS]?Job = [_]?Job{null} ** MAX_JOBS,
    next_id: u16 = 1,

    pub fn init(allocator: std.mem.Allocator) JobTable {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *JobTable) void {
        for (&self.jobs) |*slot| {
            if (slot.*) |*job| {
                self.allocator.free(job.pids);
                self.allocator.free(job.cmd);
                slot.* = null;
            }
        }
    }

    /// Add a new job to the table
    pub fn add(self: *JobTable, pgid: std.posix.pid_t, pids: []const std.posix.pid_t, cmd: []const u8, status: JobStatus) !u16 {
        // Find empty slot
        var slot_idx: ?usize = null;
        for (self.jobs, 0..) |job, i| {
            if (job == null) {
                slot_idx = i;
                break;
            }
        }

        const idx = slot_idx orelse return error.TooManyJobs;

        // Copy pids
        const pids_copy = try self.allocator.alloc(std.posix.pid_t, pids.len);
        @memcpy(pids_copy, pids);

        // Copy command string
        const cmd_copy = try self.allocator.dupe(u8, cmd);

        const job_id = self.next_id;
        // Increment with wraparound. Job ID 0 is reserved/invalid,
        // so we skip it when wrapping from max u16 back to 1.
        self.next_id +%= 1;
        if (self.next_id == 0) self.next_id = 1;

        self.jobs[idx] = Job{
            .id = job_id,
            .pgid = pgid,
            .pids = pids_copy,
            .cmd = cmd_copy,
            .status = status,
        };

        return job_id;
    }

    /// Get a job by ID
    pub fn get(self: *JobTable, job_id: u16) ?*Job {
        for (&self.jobs) |*slot| {
            if (slot.*) |*job| {
                if (job.id == job_id) return job;
            }
        }
        return null;
    }

    /// Get the most recent job (highest ID)
    pub fn getMostRecent(self: *JobTable) ?*Job {
        var best: ?*Job = null;
        var best_id: u16 = 0;
        for (&self.jobs) |*slot| {
            if (slot.*) |*job| {
                if (job.id >= best_id) {
                    best_id = job.id;
                    best = job;
                }
            }
        }
        return best;
    }

    /// Remove a job from the table
    pub fn remove(self: *JobTable, job_id: u16) void {
        for (&self.jobs) |*slot| {
            if (slot.*) |*job| {
                if (job.id == job_id) {
                    self.allocator.free(job.pids);
                    self.allocator.free(job.cmd);
                    slot.* = null;
                    return;
                }
            }
        }
    }

    /// Check if a PID belongs to any job in the table
    pub fn findByPid(self: *JobTable, pid: std.posix.pid_t) ?*Job {
        for (&self.jobs) |*slot| {
            if (slot.*) |*job| {
                for (job.pids) |job_pid| {
                    if (job_pid == pid) return job;
                }
            }
        }
        return null;
    }

    /// Update job status based on waitpid results
    pub fn updateStatus(self: *JobTable, pid: std.posix.pid_t, wait_status: u32) void {
        if (self.findByPid(pid)) |job| {
            if (std.posix.W.IFSTOPPED(wait_status)) {
                job.status = .stopped;
            } else {
                // Process exited or was killed
                job.status = .done;
            }
        }
    }

    /// Count active (non-done) jobs
    pub fn countActive(self: *JobTable) usize {
        var count: usize = 0;
        for (self.jobs) |slot| {
            if (slot) |job| {
                if (job.status != .done) count += 1;
            }
        }
        return count;
    }

    /// Iterate over all jobs
    pub fn iter(self: *JobTable) Iterator {
        return .{ .jobs = &self.jobs, .index = 0 };
    }

    pub const Iterator = struct {
        jobs: *[MAX_JOBS]?Job,
        index: usize,

        pub fn next(self: *Iterator) ?*Job {
            while (self.index < MAX_JOBS) {
                const i = self.index;
                self.index += 1;
                if (self.jobs[i]) |*job| {
                    return job;
                }
            }
            return null;
        }
    };

    /// Resolve a job ID from command arguments, or default to the most recent job
    pub fn resolveJob(self: *JobTable, job_spec: []const u8, comptime cmd_name: []const u8, stopped_only: bool) ?u16 {
        if (job_spec.len == 0) {
            // No argument - use most recent (optionally stopped) job
            if (stopped_only) {
                var iterator = self.iter();
                var best: ?*Job = null;
                while (iterator.next()) |job| {
                    if (job.status == .stopped) {
                        if (best == null or job.id > best.?.id) {
                            best = job;
                        }
                    }
                }
                if (best) |job| return job.id;
            } else {
                if (self.getMostRecent()) |job| return job.id;
            }
            io.printError("oshen: " ++ cmd_name ++ ": no current job\n", .{});
            return null;
        }

        return std.fmt.parseInt(u16, job_spec, 10) catch {
            io.printError("oshen: " ++ cmd_name ++ ": {s}: invalid job ID\n", .{job_spec});
            return null;
        };
    }
};

// =============================================================================
// Tests
// =============================================================================

const testing = std.testing;

// -----------------------------------------------------------------------------
// Basic Operations
// -----------------------------------------------------------------------------

test "JobTable: add, get, and remove" {
    var table = JobTable.init(testing.allocator);
    defer table.deinit();

    // Add single job
    const pids = [_]std.posix.pid_t{1234};
    const job_id = try table.add(1234, &pids, "sleep 10 &", .running);
    try testing.expectEqual(@as(u16, 1), job_id);

    const job = table.get(job_id).?;
    try testing.expectEqual(@as(std.posix.pid_t, 1234), job.pgid);
    try testing.expectEqualStrings("sleep 10 &", job.cmd);
    try testing.expectEqual(JobStatus.running, job.status);

    // Add pipeline job (multiple pids)
    const pipeline_pids = [_]std.posix.pid_t{ 100, 200, 300 };
    const pipeline_id = try table.add(100, &pipeline_pids, "cat | grep | wc", .running);
    try testing.expectEqual(@as(usize, 3), table.get(pipeline_id).?.pids.len);

    // Remove job
    table.remove(job_id);
    try testing.expect(table.get(job_id) == null);

    // Get/remove nonexistent is safe
    try testing.expect(table.get(999) == null);
    table.remove(999);
}

test "JobTable: ID increments and slot reuse" {
    var table = JobTable.init(testing.allocator);
    defer table.deinit();

    const pids1 = [_]std.posix.pid_t{100};
    const pids2 = [_]std.posix.pid_t{200};
    const pids3 = [_]std.posix.pid_t{300};

    const id1 = try table.add(100, &pids1, "cmd1", .running);
    const id2 = try table.add(200, &pids2, "cmd2", .running);
    try testing.expectEqual(@as(u16, 1), id1);
    try testing.expectEqual(@as(u16, 2), id2);

    // Remove and add - slot reused but ID continues
    table.remove(id1);
    const id3 = try table.add(300, &pids3, "cmd3", .running);
    try testing.expectEqual(@as(u16, 3), id3);
    try testing.expectEqual(@as(usize, 2), table.countActive());
}

// -----------------------------------------------------------------------------
// Queries
// -----------------------------------------------------------------------------

test "JobTable: getMostRecent and iteration" {
    var table = JobTable.init(testing.allocator);
    defer table.deinit();

    // Empty table
    try testing.expect(table.getMostRecent() == null);

    const pids1 = [_]std.posix.pid_t{100};
    const pids2 = [_]std.posix.pid_t{200};
    const pids3 = [_]std.posix.pid_t{300};

    _ = try table.add(100, &pids1, "job1", .running);
    _ = try table.add(200, &pids2, "job2", .running);
    const id3 = try table.add(300, &pids3, "job3", .stopped);

    // getMostRecent returns highest ID
    try testing.expectEqual(id3, table.getMostRecent().?.id);

    // Iterate counts all jobs
    var count: usize = 0;
    var iter = table.iter();
    while (iter.next()) |_| count += 1;
    try testing.expectEqual(@as(usize, 3), count);

    // countActive excludes done jobs
    _ = try table.add(400, &[_]std.posix.pid_t{400}, "done", .done);
    try testing.expectEqual(@as(usize, 3), table.countActive());
}

// -----------------------------------------------------------------------------
// Status Updates
// -----------------------------------------------------------------------------

test "JobTable: updateStatus" {
    var table = JobTable.init(testing.allocator);
    defer table.deinit();

    // Single process job
    const pids = [_]std.posix.pid_t{1234};
    const job_id = try table.add(1234, &pids, "sleep", .running);

    table.updateStatus(1234, 0); // exit status
    try testing.expectEqual(JobStatus.done, table.get(job_id).?.status);

    // Pipeline job - any pid updates status
    const pipeline_pids = [_]std.posix.pid_t{ 100, 200, 300 };
    const pipeline_id = try table.add(100, &pipeline_pids, "cat | grep", .running);
    table.updateStatus(200, 0);
    try testing.expectEqual(JobStatus.done, table.get(pipeline_id).?.status);

    // Unknown pid is safe
    table.updateStatus(999, 0);
}

// -----------------------------------------------------------------------------
// Edge Cases
// -----------------------------------------------------------------------------

test "JobTable: ID wraparound skips zero" {
    var table = JobTable.init(testing.allocator);
    defer table.deinit();

    table.next_id = 65535;

    const pids1 = [_]std.posix.pid_t{100};
    const pids2 = [_]std.posix.pid_t{200};

    try testing.expectEqual(@as(u16, 65535), try table.add(100, &pids1, "cmd1", .running));
    try testing.expectEqual(@as(u16, 1), try table.add(200, &pids2, "cmd2", .running));
}

test "JobStatus: str returns correct strings" {
    const cases = [_]struct { JobStatus, []const u8 }{
        .{ .running, "Running" },
        .{ .stopped, "Stopped" },
        .{ .done, "Done" },
    };
    for (cases) |c| try testing.expectEqualStrings(c[1], c[0].str());
}
