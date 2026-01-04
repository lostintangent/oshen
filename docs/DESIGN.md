# Oshen Design

At a high-level, Oshen tries to balance the needs of four interrelates design goals:

1. **Commands all the way down** — Everything looks like shell commands. No special syntax for "fast" operations. Users shouldn't need to know what's a builtin vs. external.

2. **High-performance scripting** — Loops with 10k iterations should be fast. Command substitution in hot paths shouldn't kill performance.

3. **Modern developer experience** — Block scoping, defer statements, rich builtins. The shell should feel like a real programming language.

4. **No surprises** — Predictable over clever. Variables don't word-split. Globs don't match variable contents. Quote semantics are consistent.

These pull in different directions. "Commands all the way down" suggests forking processes. "High performance" demands avoiding forks. "Modern DX" adds runtime overhead. "No surprises" means extra work to prevent edge cases.

**Our approach:** Embrace the simplest solution that achieves all four. Single-threaded execution. No features we don't need. Clarity over cleverness. The optimizations exist to make the shell *feel* simple while being fast underneath.

---

## Mental Models

Four concepts before diving deeper:

**1. The Four-Stage Pipeline**
```
Input → Lex → Parse → Expand → Execute → Output
```
Lexing and parsing happen once. Expansion and execution happen per-statement, just-in-time. This is why `set x 1; echo $x` works—`$x` expands after the assignment runs.

**2. Variables Are Lists**

All variables are arrays. `set items a b c` creates `["a", "b", "c"]`. When expanded, `echo $items` becomes three separate arguments—not one concatenated string. No word-splitting surprises.

**3. Block Scoping**

Variables created inside blocks (`if`, `while`, `for`, `fun`) stay local. Setting an existing variable updates it where defined. When a block ends, its variables disappear. Nothing leaks.

**4. Parse Once, Execute Many**

Control flow bodies are stored as source strings, not sub-ASTs. Parsed on first execution, then cached:

- **Loop bodies** — Cached for the loop's duration, freed on exit. Loops are often one-shot; caching forever would leak memory.
- **Function bodies** — Cached permanently. Functions are finite and called repeatedly; permanent caching pays off.

---

## Execution Flow

### Entry Points

`main.zig` branches into three modes: **Interactive** (load config, run REPL), **Command** (`-c`, execute and exit), or **Script** (execute file and exit).

### Lexer (`language/lexer.zig`)

Raw text to tokens. Recognizes operators (`>`, `|`, etc.), quoted strings, command substitution (`(...)`), brace expansion (`{...}`), separators (`;`, `\n`), and comments. Nested delimiters are tracked so `file_(echo {a,b}).zig` correctly identifies boundaries.

**Multipart words:** A single word can contain multiple parts with different quoting contexts. This enables prefix/suffix wrapping around constructs that expand to multiple values—command substitution, brace expansion, and globs.

Parts always combine via Cartesian product. This is what makes multipart words more powerful than string interpolation: instead of producing one string, they can produce N strings without a loop.

- **Command substitution** — each output line becomes a separate value (`file_(echo a b c).zig` → `file_a.zig file_b.zig file_c.zig`)
- **Brace expansion** — comma lists and ranges produce multiple values (`src/{lib,bin,test}/main.zig` → three paths)
- **Globs** — pattern matches produce multiple values (`backup_*.tar.gz` → matching files)

When multiple parts each produce multiple values, results multiply:

```
{debug,release}_$(uname -m)  →  debug_arm64 release_arm64
```

**What it doesn't do:**

- Expand variables, globs, or tildes (expander)
- Distinguish keywords from bare words (parser)

### Parser (`language/parser.zig`)

Tokens to AST. Recognizes commands, pipelines, chains (`&&`/`||`), redirects, control flow, functions, capture operators (`=>`/`=>@`).

Control flow bodies are stored as strings, not parsed recursively. See `ast.zig` for types.

### Expansion (`interpreter/expansion/`)

AST words to final arguments, immediately before execution.

**Phase 1 — Text:** Variables, tilde, command substitution, brace expansion, escapes. Produces string arrays (variables can be multi-valued).

**Phase 2 — Globs:** Only on bare words from phase 1. Matches `*`, `**`, `?`, `[a-z]`.

Quote context matters: bare words get full expansion, double-quoted get variables only, single-quoted get nothing.

### Statement Execution (`interpreter/execution/statement.zig`)

Dispatches by statement type: commands expand then run; functions register in state; control flow evaluates conditions and executes body strings; `break`/`continue`/`return` set flags that propagate up.

### Pipeline Execution (`interpreter/execution/pipeline.zig`)

Single commands try to run in-process. Pipelines fork all commands and wire pipes. Background commands (`&`) fork without waiting.

---

## Runtime State

### The State Struct (`runtime/state.zig`)

Central shell state: scope chain, exports, functions, aliases, jobs, cwd, control flow flags. Threaded through the entire execution stack.

### Scope Chain (`runtime/scope.zig`)

Linked list from current scope to global. Each scope owns an arena—pop the scope, free all its variables instantly.

Two optimizations for hot paths:

- **Scope reset** (loops) — Push once, `reset()` each iteration. Clears the HashMap but keeps memory. O(1) per iteration.
- **Scope pooling** (if/functions) — Pre-allocated pool of scopes. Grab on entry, return on exit. No allocation churn.

Result: 17x faster for tight loops with function calls.

### Functions

Store source, cache parsed AST after first call. Cache uses `page_allocator` for stable lifetime across the shell session.

### Built-ins (`runtime/builtins/`)

30+ commands running in-process: `cd`, `set`, `echo`, `test`, `calc`, `string`, etc. See `builtins.zig`.

Builtins fork only when necessary: non-first pipeline position, or fd duplication like `2>&1`. Otherwise, zero fork overhead.

**Why this matters:** Forking costs ~1ms. In loops, that adds up. Complex builtins like `string` support operation chaining (`string upper | trim | split ","`) so multi-step transforms stay in-process. Everything feels like commands; nothing secretly forks.

---

## Performance Architecture

### Tiered Output Capture

`$(...)` and `=> var` capture stdout. We optimize aggressively:

```
$(= $x + 1)    → Direct eval, skip lex/parse     ~1000x faster
$(pwd)         → Direct builtin, skip lex/parse  ~1000x faster
$(echo $var)   → Parse, run in-process            ~100x faster
$(ls | grep)   → Fork                             baseline
```

Pipes, redirects, and quotes bail to fork (they need full parsing or separate processes). The check costs nanoseconds; the savings are milliseconds. See `execution/capture.zig`.

### Scope Reuse and Function Caching

Covered in [Scope Chain](#scope-chain-runtimescopezig) and [Functions](#functions) above.

---

## Signals & Job Control

### Interactive Initialization (`execution/signals.zig`)

Shell takes terminal control via `tcsetpgrp()`. Handlers installed for `SIGINT` (Ctrl+C), `SIGTSTP` (Ctrl+Z), `SIGCHLD` (child status).

Signal handlers use a global `g_state` pointer—POSIX handlers can't take context. Single-threading is a simplification we embrace.

### Job Table (`runtime/jobs.zig`)

Background and stopped jobs tracked by process group. `fg` brings to foreground, `bg` resumes in background. `SIGCHLD` updates status asynchronously.

### Forked Processes

Children reset signal handlers and join appropriate process groups. Parent waits (foreground) or continues (background).

---

## REPL (`repl/`)

The interactive layer, supporting modern DX:

- **Line editor** — Raw mode, Emacs keybindings, multi-line
- **Syntax highlighting** — Real-time, validates against PATH
- **Autosuggestions** — History-based, scored by directory + recency + frequency
- **Tab completion** — Commands and paths
- **History** — Binary format, 10K entries, per-command metadata
- **Custom prompts** — User-defined `prompt` function

See `editor/` and `editor/ui/` for implementation.

---

## Codebase Map

```
src/
├── main.zig              Entry point
├── cli.zig               Argument parsing
│
├── language/             Syntax (no execution knowledge)
│   ├── tokens.zig        Token types
│   ├── lexer.zig         Text → tokens
│   ├── ast.zig           AST definitions
│   └── parser.zig        Tokens → AST
│
├── interpreter/          Execution
│   ├── interpreter.zig   Orchestration
│   ├── expansion/        Variables, globs, substitution
│   └── execution/        Pipelines, redirects, capture, signals
│
├── runtime/              State
│   ├── state.zig         Central struct
│   ├── scope.zig         Scope chain
│   ├── jobs.zig          Job table
│   └── builtins/         30+ commands
│
├── repl/                 Interactive
│   ├── repl.zig          Main loop
│   ├── prompt.zig        Prompts
│   └── editor/           Editing, history, suggestions
│
└── terminal/             I/O primitives
```

**Data flow:** Language → Interpreter → Runtime. REPL wraps for interactive use.

---

## Design Decisions

Each decision involves tradeoffs. We optimize for the balance of performance, simplicity, and experience.

### Deferred parsing for control flow

Bodies stored as strings, parsed when executed.

*Tradeoff:* Re-parsing overhead (mitigated by caching).

*Worth it:* Simpler AST, natural scope boundaries, traditional shell semantics. Loop bodies cache on first iteration.

### Two-phase expansion

Variables expand first, then globs.

*Tradeoff:* Two passes.

*Worth it:* If a variable contains `*`, it won't accidentally glob. Phase 2 only touches bare words from original input. No surprises.

### Variables as lists

All variables are arrays. Scalars are single-element arrays.

*Tradeoff:* Slightly more memory.

*Worth it:* No word-splitting bugs. `echo $items` naturally produces multiple arguments. Indexing and slicing work uniformly.

### Tiered output capture

Fast paths before fork.

*Tradeoff:* Multiple code paths.

*Worth it:* 1000x speedup for common patterns. Detection costs nanoseconds.

### Scope pooling

Reuse scopes, reset in O(1).

*Tradeoff:* Pool management.

*Worth it:* 17x faster in tight loops. Stable memory under load.

### In-process builtins

Fork only when necessary.

*Tradeoff:* Special-case logic.

*Worth it:* Near-zero overhead for common operations. Noticeably better interactive feel.

### Recursion limit

Hard limit on call depth.

*Tradeoff:* No deep recursion.

*Worth it:* Prevents stack overflow. TCO planned; limit temporary. Prefer iteration for now.

---

## Further Reading

- `docs/KNOWN_GAPS.md` — Intentional limitations
- Source files contain detailed inline comments
