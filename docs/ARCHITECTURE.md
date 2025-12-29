# 🌊 Oshen Architecture

This document describes Oshen's internal architecture and execution flow.

## Table of Contents

- [Subsystems](#subsystems)
  - [Language](#1-language-srclanguage)
  - [Interpreter](#2-interpreter-srcinterpreter)
  - [Runtime](#3-runtime-srcruntime)
  - [REPL](#4-repl-srcrepl)
- [Execution Pipeline](#execution-pipeline)
  - [The Three Stages](#the-three-stages)
  - [Pipeline Diagram](#pipeline-diagram)
- [Process & Job Control](#process--job-control)
  - [In-Process Builtin Capture](#in-process-builtin-capture)
- [Key Design Decisions](#key-design-decisions)

---

## Subsystems

Oshen is organized into four subsystems, each in its own directory:

```
src/
├── language/          Syntax: lexer, parser, AST, tokens
├── interpreter/       Execution: expand then execute
│   ├── expansion/     "What to do": variable/glob/tilde expansion
│   └── execution/     "Do it": fork, exec, pipes, redirects
├── runtime/           State: variables, jobs, builtins
├── repl/              Interactive: line editing, prompts, history
└── terminal/          Terminal primitives: I/O, ANSI codes, raw mode
```

Data flows: **Language → Interpreter (Expansion → Execution) → Runtime**, with **REPL** wrapping everything for interactive use. **Terminal** provides shared primitives for the REPL and interactive builtins.

---

### 1. Language (`src/language/`)

The language subsystem converts source text into a structured AST. It knows nothing about execution — only syntax.

| File | Purpose |
|------|---------|
| `tokens.zig` | Token types, `WordPart` (word segments with quoting context), `TokenSpan` (source location with byte indices) |
| `lexer.zig` | Tokenization with quote/escape handling |
| `ast.zig` | AST node definitions (`Program`, `Statement`, `Pipeline`, `Command`) |
| `parser.zig` | Token stream → AST conversion |

**Lexer** handles:
- Quoted strings (single, double, with escape sequences)
- Operators (`|`, `>`, `>>`, `&&`, `||`, `;`)
- Word boundaries and whitespace
- Comments (`#`)

**Parser** recognizes:
- Pipelines (`cmd1 | cmd2`)
- Conditionals (`&&`, `||`, `and`, `or`)
- Control flow (`if`/`else if`/`else`/`for`/`while`/`fun` blocks with `end`)
- Loop control (`break`, `continue`)
- Function control (`return [status]`)
- Redirections (`>`, `>>`, `2>`, `&>`, `2>&1`)
- Command substitution (`$(...)`)
- Output capture (`=>`, `=>@`)
- Background execution (`&`)

**Important**: The parser treats words as opaque — it doesn't interpret `$`, `~`, `*`, or `[...]` inside them. A word like `$var[1]` is passed through as-is. Expansion syntax is handled later by the expander.

### 2. Interpreter (`src/interpreter/`)

The interpreter subsystem handles execution, with expansion happening just-in-time.

```
interpreter/
├── interpreter.zig        Orchestrates: lex → parse → execute
├── expansion/             Word and pipeline expansion
│   ├── statement.zig      Pipeline expansion (AST Pipeline → ExpandedPipeline)
│   ├── expanded.zig       Expanded types (ExpandedCmd, ExpandedPipeline, etc.)
│   ├── word.zig           Variable, tilde, command substitution expansion
│   └── glob.zig           Glob pattern matching (*, **, ?, [abc], [a-z])
└── execution/             Process execution
    ├── exec.zig           Statement dispatch, control flow, process spawning
    ├── pipeline.zig       Pipeline wiring and execution
    ├── capture.zig        Output capture for `=>`, `=>@`, and `$(...)`
    └── redirect.zig       File descriptor manipulation for redirections
```

#### Expansion (`expansion/`)

Expansion happens **just-in-time during execution**, not as a separate phase. The AST is executed directly, and pipelines are expanded immediately before being run.

**What gets expanded:**
- **Variables** (`$x`), **globs** (`*.txt`), **tilde** (`~`), **command substitution** (`$(...)`)
- **Command arguments** - expanded from `[]WordPart` to `[]const u8` (argv)
- **Environment assignments** - values expanded
- **Redirections** - targets expanded (e.g., `> $outfile` → `> result.txt`)

**Why just-in-time expansion?** Each command in a chain sees the **current** shell state. This ensures `set x 1 && echo $x` works correctly - the variable is set before `$x` is expanded in the second command.

**Control flow statements** (`if`, `for`, `while`, `fun`) store their bodies as strings and are re-parsed at execution time. This creates natural recursion boundaries.

#### Execution (`execution/`)

The executor runs AST statements directly:

- **Statement dispatch** - routes to appropriate handler (command, if, for, while, function, etc.)
- **Pipeline expansion** - converts `ast.Pipeline` → `ExpandedPipeline` just before execution
- **Process management** - `fork()`, `execvpe()`, process groups
- **Pipeline wiring** - connects commands with pipes
- **Redirections** - applies file descriptors for `>`, `>>`, `2>&1`, etc.
- **Job control** - manages background jobs, foreground/background switching

**Key functions:**
```zig
// Execute oshen code from a string
interpreter.execute(allocator, state, code) !u8

// Execute oshen code from a file
interpreter.executeFile(allocator, state, path) !u8

// Execute oshen code and capture stdout (for command substitution, custom prompts)
interpreter.executeAndCapture(allocator, state, code) ![]const u8
```

---

### 3. Runtime (`src/runtime/`)

The runtime maintains shell state that persists across commands.

| File | Purpose |
|------|---------|
| `state.zig` | Central state: variables, exports, functions, cwd |
| `scope.zig` | Lexical scope chain for block-local variables |
| `jobs.zig` | Job table: background/stopped process management |
| `builtins.zig` | Builtin command registry and dispatch |
| `builtins/*.zig` | Individual builtin implementations |

**State contains:**
- Scope chain for shell variables (block-local semantics)
- Environment variables (`export`) — passed to child processes
- Aliases — command name expansions
- User-defined functions
- Job table for background/stopped processes
- Current working directory
- Exit status

#### Lexical Scoping

Variables use a **scope chain** for proper lexical scoping. Each block (if, while, each, function) pushes a new scope, and variables follow these rules:

1. **New variables** are created in the current (innermost) scope
2. **Setting existing variables** updates them in the scope where they're defined
3. **Reading variables** walks up the scope chain until found

```
┌─────────────────────────────────────┐
│         Global Scope                │
│   count = "0"                       │
│   name = "Alice"                    │
├─────────────────────────────────────┤
│    ↑ parent                         │
│  ┌──────────────────────────────┐   │
│  │    If Branch Scope           │   │
│  │  x = "local"  (new, local)   │   │
│  │  count → updates global      │   │
│  └──────────────────────────────┘   │
└─────────────────────────────────────┘
```

Each scope owns an **arena allocator** for its variables. When a scope is popped (block exits), the arena is freed — O(1) cleanup regardless of how many variables were created.

**Loop optimization**: For loops, the scope is pushed once and **reset** each iteration rather than push/pop per iteration. This reuses memory and avoids allocation overhead:

```zig
const loop_scope = state.pushScope();
defer state.popScope();

for (items) |item| {
    loop_scope.reset();  // O(1) clear, retain memory
    loop_scope.setLocalScalar("item", item);
    // execute body...
}
```

**Scope pooling**: For if statements and function calls (which can't use the loop reset pattern), oshen maintains a **scope pool** to avoid repeated allocation/deallocation in hot loops:

```zig
// Instead of allocating a new scope each time:
const scope = state.acquireScope();  // Get from pool (or allocate if empty)
defer state.releaseScope();           // Return to pool for reuse
```

The pool works like a library book system:
- First time you need a scope, it's allocated (no choice)
- When done, return it to the pool (`releaseScope()` resets variables and adds to pool)
- Next time, borrow from pool instead of allocating
- Pool is capped at 16 scopes to avoid unbounded growth

This makes function calls and if statements inside loops dramatically faster — up to 17x improvement — by eliminating syscalls for memory allocation.

**Builtins:**

| Command | Purpose |
|---------|---------|
| `alias` | Define command aliases |
| `bg` | Continue job in background |
| `cd` | Change directory |
| `echo` | Print arguments |
| `exit` | Exit the shell |
| `export` | Set environment variables |
| `false` | Return failure (exit 1) |
| `fg` | Bring job to foreground |
| `jobs` | List background jobs |
| `pwd` | Print working directory |
| `set` | Set shell variables |
| `source` | Execute a file |
| `true` | Return success (exit 0) |
| `type` | Show command type (alias/builtin/function/external) |
| `unalias` | Remove command aliases |
| `unset` | Remove shell variables |

Builtins run in the shell process by default for performance. However, if a builtin has redirections (e.g., `echo "x" > file`) or is part of a pipeline (e.g., `echo "x" | cat`), it runs in a forked child to properly isolate file descriptor changes.

---

### 4. REPL (`src/repl/`)

The REPL provides the interactive shell experience.

| File | Purpose |
|------|---------|
| `repl.zig` | Main read-eval-print loop |
| `prompt.zig` | Prompt generation (default or custom function) |
| `editor/editor.zig` | Line editing, cursor movement, key handling |
| `editor/history.zig` | Command history with rich metadata (CWD, timestamp, frequency) |
| `editor/highlight.zig` | Real-time syntax highlighting |
| `editor/suggest.zig` | Context-aware autosuggestions with weighted scoring |

**REPL loop:**
```zig
while (true) {
    const prompt_str = prompt.build(allocator, state, &buf);
    const line = editor.readLine(prompt_str);
    const status = execute(allocator, state, line);
    state.status = status;
}
```

**Key features:**

| Feature | Implementation |
|---------|----------------|
| Syntax highlighting | Real-time coloring as you type |
| Autosuggestions | Context-aware scoring: directory (40%) + recency (30%) + frequency (25%) + success (5%) |
| Custom prompt | User-defined `prompt` function (stdout captured) |
| Line editing | Emacs-style keybindings |
| History storage | Binary format with 10K entries, tracking CWD/timestamp/exit status per command |

### 5. Terminal (`src/terminal/`)

Shared primitives for terminal interaction, used by both the REPL and interactive builtins.

| File | Purpose |
|------|---------|
| `io.zig` | Output helpers (writeStdout, printError, etc.) |
| `ansi.zig` | ANSI color codes, text styling, cursor movement, display length |
| `tui.zig` | Raw mode, key reading, and interactive terminal features |

When oshen processes input, it goes through three stages:

#### 1. Lexer

Converts raw text into tokens:

```
"echo $name *.txt"  →  [word:"echo"] [word:"$name"] [word:"*.txt"]
```

#### 2. Parser

Builds an Abstract Syntax Tree from tokens:

```
Program
└── Statement (command)
    └── Pipeline
        ├── Command: [echo, $name, *.txt]
        └── Redirections: []
```

#### 3. Executor (with just-in-time expansion)

The executor runs AST statements directly. When it encounters a pipeline, it expands word contents just before execution.

Word expansion is a **second parsing layer** that interprets expansion syntax within words. While the structural parser handles command grammar, the expander has its own mini-parser for:

```
$var        → parse identifier characters
${var}      → parse until closing }
$var[1]     → parse identifier, then parse [...]
$(cmd)      → parse until matching )
$1, $#, $*  → single special character
~           → tilde at word start
*.txt       → glob metacharacters
{a,b,c}     → brace expansion (comma-separated list)
{*.txt}     → braced glob pattern
```

**Two-Phase Expansion**: The expander uses a two-phase design to prevent conflicts between variable indexing (`$xs[2]`) and glob patterns (`file[abc].txt`):

1. **Phase 1 - Text Expansion** (`expandText`): A unified left-to-right character scan that processes:
   - **Tilde** (`~`) - at word start, expands to home directory
   - **Variables** (`$var`, `${var}`, `$var[1]`) - when `$` is encountered, parses the variable name AND any indexing syntax, consuming the entire expression including brackets
   - **Command substitution** (`$(cmd)`) - when `$(` is encountered, executes command and captures output
   - **Escape sequences** (`\n`, `\t`, `\$`) - when `\` is encountered, interprets escape codes
   - **Literal text** - everything else between special characters

   All of these are handled in a **single pass**, not separate sub-phases. When `$xs[2]` is encountered, the entire expression including `[2]` is parsed and consumed before moving to the next character.

2. **Phase 2 - Glob Expansion** (`hasGlobChars` + `expandGlob`): Only runs on the **result** of Phase 1. By this point, all `$xs[2]` expressions have been replaced with their values (e.g., `"b"`). Any remaining `[` characters must be glob patterns since all variable-related brackets were already consumed. Glob detection is simple: scan for `*`, `?`, or `[` metacharacters. Only bare (unquoted) words with glob characters trigger filesystem matching.

This sequential design means `$xs[2]_file[abc].txt` works correctly: the `[2]` is consumed during the text expansion pass, while `[abc]` survives to Phase 2 and becomes a glob pattern.

**Quote-aware expansion**: The `expand_glob` flag is controlled by quoting context:
- Bare words (`*.txt`) → full expansion including globs
- Double-quoted (`"*.txt"`) → variables expand, globs don't
- Single-quoted (`'*.txt'`) → no expansion at all, literal text

Expansion results:

```
$name          →  ["Alice"]                     (from state.variables)
$1             →  ["arg1"]                      (positional parameter)
$#             →  ["3"]                         (argument count)
$*             →  ["a", "b", "c"]               (all arguments)
$xs[1]         →  ["first"]                     (array index, 1-based)
$xs[-1]        →  ["last"]                      (negative index from end)
$xs[2..4]      →  ["b", "c", "d"]               (array slice, inclusive)
*.txt          →  ["a.txt", "b.txt"]            (from filesystem)
**/*.zig       →  ["src/main.zig", "src/util.zig"]  (recursive glob)
[a-z].md       →  ["a.md", "b.md"]              (character class glob)
$(whoami)      →  ["alice"]                     (from subprocess)
~              →  ["/home/alice"]               (from $HOME)
{a,b,c}        →  ["a", "b", "c"]               (explicit list)
{*.txt}_backup →  ["a.txt_backup", "b.txt_backup"]  (glob + suffix)
{$items}_test  →  ["x_test", "y_test"]          (variable + suffix)
{a,b}_{1,2}    →  ["a_1", "a_2", "b_1", "b_2"]  (nested cartesian)
```

**List expansion**: A variable holding `["a", "b"]` expands to two separate arguments, not one string with spaces.

The expander combines AST structure with expanded values to produce:

```zig
ExpandedCmd {
    .argv = ["echo", "Alice", "a.txt", "b.txt"],
    .env = [],
    .redirects = [],
}
```

After expansion, the executor spawns processes, wires pipes, and handles redirections:

```
fork() → child: exec("echo", args)
       → parent: wait for exit status
```

---

### Pipeline Diagram

**Key insight**: Parse once, but execute each statement individually with just-in-time expansion. This allows `set x 1; echo $x` to work — the variable is set before `$x` is expanded.

```
┌─────────────────────────────────────────────────────────────────────┐
│                    Input: "set x 1; echo $x"                        │
└─────────────────────────────────────────────────────────────────────┘
                                │
                    ┌───────────┴───────────┐
                    │    LANGUAGE SUBSYSTEM │
                    │      Lex → Parse      │
                    └───────────┬───────────┘
                                │
                    ┌───────────┴───────────┐
                    │         AST           │
                    │  [Statement, Statement]│
                    └───────────┬───────────┘
                                │
              ┌─────────────────┼─────────────────┐
              │                                   │
              ▼                                   ▼
     ┌─────────────────┐                ┌─────────────────┐
     │   Statement 1   │                │   Statement 2   │
     │   "set x 1"     │                │   "echo $x"     │
     └────────┬────────┘                └────────┬────────┘
              │                                   │
              ▼                                   │
     ┌─────────────────┐                          │
     │    EXECUTOR     │                          │
     │ state.x = "1"   │  ← Variable now exists   │
     └────────┬────────┘                          │
              │                                   │
              └──────────── NEXT ─────────────────┘
                                                  │
                                                  ▼
                                         ┌─────────────────┐
                                         │    EXECUTOR     │
                                         │ expand: $x → "1"│
                                         │ exec: echo "1"  │
                                         └─────────────────┘
```

---

## Process & Job Control

Oshen spawns external processes via `fork()` + `exec()`. Each pipeline runs in its own process group for proper job control.

### When Processes Are Spawned

| Scenario | What Happens |
|----------|--------------|
| Simple command (`ls`) | Fork, exec in child, parent waits |
| Pipeline (`ls \| grep`) | Fork per command, wire pipes, wait for all |
| Background (`sleep &`) | Fork, don't wait, add to job table |
| Builtin (`cd`, `set`) | No fork — runs in shell process |
| Builtin with stdout redirect (`echo > file`) | No fork — in-process with fd save/restore |
| Builtin with complex redirects (`cmd 2>&1`) | Fork to apply redirects safely |
| Builtin starting pipeline (`echo \| cat`) | No fork — in-process with stdout to pipe |
| Builtin mid/end of pipeline (`cat \| echo`) | Fork (required for concurrent pipe flow) |
| Command substitution (`$(cmd)`) | Fork, capture stdout, wait |

**Builtin execution model**: Builtins run in-process for performance whenever safely possible. The shell uses in-process execution with file descriptor save/restore for simple cases, falling back to fork only when necessary:

```
echo "hello"           →  In-process (fast path)
cd /tmp                →  In-process (must affect parent)
set x y                →  In-process (must affect parent state)
echo "hello" > file    →  In-process: save stdout, open file, run, restore
echo "hello" >> file   →  In-process: same as above with append mode
echo "hello" | cat     →  In-process: save stdout, dup2 to pipe, run, restore
cat file | echo "x"    →  Fork for echo (mid-pipeline needs concurrent flow)
echo 2>&1 > file       →  Fork (complex redirects need isolated fd table)
jobs | grep sleep      →  Fork, run builtin with shell state access, exit
```

**Why some cases still fork:**
- **Complex redirects** (stderr, fd duplication): Safely isolating multiple fd changes is error-prone in-process
- **Builtins mid/end of pipeline**: Pipes have limited buffer (~64KB). If we run a mid-pipeline builtin in-process, we block the parent while previous stages might fill the pipe buffer and deadlock. Forking allows concurrent execution.
- **Process group leadership**: The first forked process becomes the process group leader for job control

This design optimizes the common cases (`echo > file`, `echo | grep`) while maintaining correctness for complex scenarios.

### In-Process Builtin Capture

Output capture (`=>`, `=>@`) and command substitution (`$(...)`) normally require forking a child process to capture stdout. However, for simple builtin commands, oshen captures output **in-process** — avoiding fork overhead entirely.

```
# These use the fast path (no fork):
echo hello => greeting           # Simple builtin capture
var user (whoami)                # Wait, whoami is external...
var x (= 1 + 2)                  # calc/= is a builtin!
var cwd (pwd)                    # pwd is a builtin

# These use the slow path (fork required):
var files (ls)                   # External command
var result (echo hi | cat)       # Pipeline
var out (echo hello > /tmp/x)    # Has redirects
```

**How it works:**

1. Before forking, check if the command is a "simple builtin":
   - Single command (no pipeline)
   - No redirects
   - Command name is a builtin

2. If yes, redirect stdout to a pipe, run the builtin in-process, read the output, restore stdout

3. If no, fall back to fork-based capture

**Performance impact:** In-process capture is ~100x faster than fork-based capture. This matters for prompts (which often use `$(pwd)` or `$(git branch)`) and scripts with many command substitutions.

The fast path is implemented in `capture.zig`:
- `tryExpandSimpleBuiltin()` - detects when the fast path is usable
- `captureBuiltin()` - runs the builtin with stdout redirected to a pipe
- `forkWithPipe()` - the slow path for external commands and complex cases

Both `interpreter.executeAndCapture()` (for command substitution) and `exec.executeCommandWithCapture()` (for `=>` operators) use this same infrastructure.

### Job Table

Background and stopped processes are tracked in a job table:

```zig
Job {
    id: u16,           // Job number ([1], [2], etc.)
    pgid: pid_t,       // Process group ID
    pids: []pid_t,     // All PIDs in the pipeline
    cmd: []const u8,   // Original command string
    status: JobStatus, // running, stopped, done
}
```

### Foreground vs Background

```
┌──────────────────────────────────────────────────────────────────┐
│                          Terminal                                │
│  ┌────────────────────────────────────────────────────────────┐  │
│  │              Foreground Process Group                      │  │
│  │  • Receives keyboard input (stdin)                         │  │
│  │  • Receives Ctrl+C (SIGINT), Ctrl+Z (SIGTSTP)              │  │
│  │  • Only ONE at a time                                      │  │
│  └────────────────────────────────────────────────────────────┘  │
│                                                                  │
│  ┌────────────────────────────────────────────────────────────┐  │
│  │             Background Process Groups                      │  │
│  │  • No terminal input                                       │  │
│  │  • Continue running while shell prompts                    │  │
│  │  • ZERO or more at a time                                  │  │
│  └────────────────────────────────────────────────────────────┘  │
└──────────────────────────────────────────────────────────────────┘
```

### Signal Handling

| Signal | Trigger | Action |
|--------|---------|--------|
| `SIGCHLD` | Child exits/stops | Reap child, update job status |
| `SIGINT` | Ctrl+C | Kill foreground job |
| `SIGTSTP` | Ctrl+Z | Stop foreground job, add to job table |
| `SIGCONT` | `fg`/`bg` | Resume stopped job |

### Job Control Flow

```
User: sleep 30 &
         │
         ▼
    ┌─────────┐
    │  fork() │
    └────┬────┘
         │
    ┌────┴────┐
    │         │
    ▼         ▼
  Parent    Child
    │         │
    │    setpgid(0,0)  ← New process group
    │         │
    │    exec("sleep")
    │
Add to job table
    │
Print [1] 12345
    │
Return to prompt (child runs in background)
```

---

## Key Design Decisions

### 1. Statement-by-Statement Execution

Parse all statements upfront, but expand/execute one at a time:

```zig
for (ast.statements) |stmt| {
    _ = try executeStatement(allocator, state, stmt, input);
}
```

This ensures side effects are visible to subsequent statements. Expansion happens inside `executeStatement` just before each pipeline runs.

### 2. List Variables

Variables are arrays, not strings:

```zig
// state.variables["files"] = ["a.txt", "b.txt", "c.txt"]
// $files expands to 3 separate arguments, not "a.txt b.txt c.txt"
```

No word-splitting surprises.

### 3. AST Executed Directly

The executor runs AST statements directly — there's no separate "expanded statement" type. This keeps the type system simple and avoids redundant data structures.

**AST types** (`src/language/ast.zig`) are used throughout:
- `Statement` - dispatched directly to the executor
- `CommandStatement` - chains, background, capture
- `FunctionDefinition` - name and body string
- `IfStatement`, `ForStatement`, `WhileStatement` - control flow with body strings

**Expanded types** (`src/interpreter/expansion/expanded.zig`) exist only for pipelines:
- `ExpandedCmd` - argv, env, redirects (all fully resolved, returned as `[]const ExpandedCmd`)
- `ExpandedRedir` - fd, kind, path/target
- Uses `ast.Assignment` for environment variables (with expanded values)

**Just-in-time pipeline expansion:** Each `ast.Pipeline` is expanded to `[]const ExpandedCmd` using **current** shell state right before execution:
```zig
set x 1 && echo $x    // Works! $x expanded after 'set' executes
cd /tmp && pwd        // Works! pwd sees new directory
```

This design minimizes types - we only create expanded types when transformation actually happens (e.g., `[]WordPart` → `[]const u8` for command argv).

### 4. Arena Allocation Per Command

Each command gets its own arena allocator:

```zig
var arena = std.heap.ArenaAllocator.init(allocator);
defer arena.deinit();  // Free everything at once
```

Simple, fast, no leaks.

### 5. Control Flow via Body Strings

`if`, `for`, `while`, and `fun` store their bodies as strings in the AST:

```zig
// For-loop in AST
ast.ForStatement {
    .variable = "x",
    .items_source = "a b c",
    .body = "echo $x",
}

// If-statement with branches for if/else-if chains
ast.IfStatement {
    .branches = &.{
        .{ .condition = "test $x -lt 10", .body = "echo small" },
        .{ .condition = "test $x -lt 100", .body = "echo large" },
    },
    .else_body = "echo huge",
}
```

Bodies are re-parsed and executed via `interpreter.execute()`. This creates a natural recursion boundary and simplifies the execution model.

### 6. Loop Control and Return via State Flags

`break`, `continue`, and `return` use state flags to communicate with loop/function executors:

```zig
// In state.zig
loop_break: bool = false,
loop_continue: bool = false,
fn_return: bool = false,

// In execution:
// 1. break sets state.loop_break = true
// 2. Loop executor checks flag after each iteration/statement
// 3. Flag is reset at end of loop iteration (continue) or loop exit (break)
// 4. return sets state.fn_return = true and state.status
// 5. Function executor checks fn_return and resets it after function completes
```

This propagation model allows break/continue/return to work correctly even when nested inside `if` statements within a loop or function body.

### 7. Two-Layer Parsing

Oshen separates **structural parsing** from **expansion parsing**:

| Layer | Location | Handles |
|-------|----------|--------|
| Structural | `parser.zig` | Commands, pipelines, control flow, redirections |
| Expansion | `expansion/expand.zig` | `$var`, `$var[n]`, `~`, `*`, `$(...)` inside words |

The structural parser treats words as opaque text — it doesn't know about variables or globs. The expander interprets the content of each word segment.

**Why this design?**

1. **Simpler grammar**: The shell grammar doesn't need rules for every expansion feature
2. **Easy to extend**: Adding `$var[1..2]` indexing required zero parser changes
3. **Testable**: Expansion logic can be unit-tested independently of parsing
4. **Quote-aware**: The expander knows which segments were quoted, enabling proper suppression of expansion in single quotes

The alternative — tokenizing `$var[1]` as `VAR_REF` + `INDEX` — would tightly couple the grammar to expansion rules and require parser changes for every new feature.

### 8. Scope Pooling for Hot Loops

Control flow and function calls need scopes, but allocating/deallocating them in tight loops is expensive. Oshen uses two complementary strategies:

| Pattern | Strategy | How It Works |
|---------|----------|--------------|
| Loops (`for`, `while`) | Scope reset | Push once, `reset()` each iteration (O(1) clear) |
| If/Functions | Scope pool | `acquireScope()` from pool, `releaseScope()` back to pool |

The scope pool maintains up to 16 pre-allocated scopes. When a function returns or an if-branch completes, the scope is reset and returned to the pool instead of being freed. The next function call or if-statement grabs it from the pool instead of allocating.

**Performance impact**: In benchmarks with 100k function calls in a loop, scope pooling improved performance from 1.7s to 0.1s — a 17x speedup — by eliminating system calls for memory allocation.
