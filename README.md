# 🌊 Oshen

A modern shell with a great developer experience out-of-the-box:

- **Zero-setup REPL** — Syntax highlighting, ghost text, tab completions, and a git-aware default prompt.
- **Clean syntax** — Familiar control flow with [block-scoped variables](#variable-scoping), plus [`defer`](#defer) for automatic cleanup.
- **Lists by default** — Variables, [globs](#glob-patterns), and [brace expansion](#brace-expansion) all produce lists, which can be enumerated, indexed, or sliced.
- **Modern builtins** — [Colored output](#colored-output-with-print), [output capture](#output-capture), [arithmetic (`calc`/`=`)](#calc-expressions), [`increment`](#increment), [`range`](#generating-sequences-with-range), [`string`](#string-manipulation-with-string), [`path_prepend`](#path_prepend), and more!
- **Fast** — Carefully optimized to make real-world scripts and interactive use snappy.

---

## Installation

### macOS (Homebrew)

```sh
brew tap lostintangent/oshen https://github.com/lostintangent/oshen
brew install lostintangent/oshen/oshen
```

> [!TIP]
> And when you want to grab an [update](https://github.com/lostintangent/oshen/releases) later, simply run `brew upgrade lostintangent/oshen/oshen`.

### Linux / WSL

Download the latest release:

```sh
curl -LO https://github.com/lostintangent/oshen/releases/latest/download/oshen-linux-x86_64.tar.gz
tar -xzf oshen-linux-x86_64.tar.gz
sudo mv oshen /usr/local/bin/
```

### Set as Default Shell

```sh
# Add to /etc/shells
echo "$(which oshen)" | sudo tee -a /etc/shells

# Change your default shell
chsh -s $(which oshen)
```

---

## Quick Start

```sh
# Run interactive shell
oshen

# Run a command
oshen -c "print --magenta hello world"

# Run a script
oshen script.wave
```

---

### Interactive Shell

- [Syntax Highlighting](#syntax-highlighting)
- [Autosuggestions](#autosuggestions)
- [Tab Completion](#tab-completion)
- [Line Editing](#line-editing)
- [History](#history)
- [Configuration](#configuration)

### Core Language

- [Variables](#variables)
- [Environment Variables](#environment-variables)
- [Quoting & Escaping](#quoting--escaping)
- [Comments](#comments)

### Control Flow

- [If Statements](#if-statements)
- [Each Loops](#each-loops)
- [While Loops](#while-loops)
- [Conditional Chaining](#conditional-chaining)
- [Functions](#functions)
- [Defer](#defer)

### Commands & I/O

- [Pipelines](#pipelines)
- [Redirections](#redirections)
- [Output Capture](#output-capture)
- [Command Substitution](#command-substitution)
- [Job Control](#job-control)
- [Glob Patterns](#glob-patterns)
- [Brace Expansion](#brace-expansion)

### Reference

- [Builtins](#builtins)
- [Command Line Options](#command-line-options)

---

# Interactive Shell

Oshen's interactive mode provides a modern editing experience out of the box—no plugins or configuration required.

## Syntax Highlighting

Commands, strings, operators, and variables are color-coded as you type:

- **Commands** — Valid commands (builtins, functions, aliases, and executables) shown in bold
- **Invalid commands** — Unknown commands shown in red
- **Keywords** — Control flow (`if`, `for`, `while`, etc.) in blue
- **Strings** — Single and double-quoted strings in green
- **Variables** — `$var` expansions highlighted in purple
- **Operators** — Pipes, redirections, and logical operators in cyan

Highlighting updates in real-time as you edit, giving instant feedback on syntax errors.

## Autosuggestions

As you type, Oshen shows suggestions from your command history in dimmed text:

```shell
$ git com                           # You type this
$ git commit -m "Update docs"       # Suggestion appears dimmed
```

- Press **→** or **End** to accept the full suggestion
- Press **Alt+→** to accept word-by-word
- Continue typing to refine or ignore

Suggestions are **context-aware**, scored by:
- **Directory** — Commands from your current directory rank highest
- **Recency** — Recent commands rank higher (exponential decay)
- **Frequency** — Frequently-used commands rank higher
- **Success** — Successful commands get a small bonus over failures

This means `cd src` in project A suggests differently than in project B.

## Tab Completion

Press **Tab** to complete commands and paths:

### Command Completion

At the start of a line, Tab completes from:
- All builtin commands (`cd`, `set`, `export`, etc.)
- Executables found in your `$PATH`

```shell
$ ec<Tab>        →  $ echo
$ gi<Tab>        →  $ git
```

### Path Completion

In argument positions (or when the word contains `/`, `.`, or `~`), Tab completes file and directory paths:

```shell
$ cat ~/Doc<Tab>      →  $ cat ~/Documents/
$ ls src/*.z<Tab>     →  $ ls src/main.zig
```

- Directories get a trailing `/` automatically
- Hidden files (starting with `.`) are shown only when the prefix starts with `.`

### Multiple Matches

When multiple completions exist, Tab inserts the common prefix. Press Tab again to see all options.

## Line Editing

Oshen provides Emacs-style line editing keybindings:

| Key | Action |
|-----|--------|
| **←** / **→** | Move cursor left/right |
| **Ctrl+A** / **Home** | Move to beginning of line |
| **Ctrl+E** / **End** | Move to end of line |
| **Ctrl+W** | Delete word before cursor |
| **Ctrl+U** | Delete from cursor to beginning |
| **Ctrl+K** | Delete from cursor to end |
| **Backspace** | Delete character before cursor |
| **Delete** | Delete character at cursor |
| **Ctrl+L** | Clear screen |
| **Ctrl+C** | Cancel current line |
| **Ctrl+D** | Exit shell (on empty line) |

## History

Command history is automatically saved to `~/.oshen_history` and persists across sessions.

| Key | Action |
|-----|--------|
| **↑** / **Ctrl+P** | Previous command in history |
| **↓** / **Ctrl+N** | Next command in history |

History features:
- **10,000 entries** — Retains months of usage
- **Context-aware** — Each command records the working directory, timestamp, and exit status
- **Frequency tracking** — Same command in the same directory increments frequency (no duplicates)
- **Binary format** — Fast load/save with `~/.oshen_history`

## Configuration

Oshen loads `~/.oshen_floor` for every interactive shell. Like fish, there's no separate "profile" vs "rc" file—just one config that runs each time.

### Example Configuration

```sh
# ~/.oshen_floor

# Homebrew (macOS)
export HOMEBREW_PREFIX "/opt/homebrew"
export HOMEBREW_CELLAR "/opt/homebrew/Cellar"
export HOMEBREW_REPOSITORY "/opt/homebrew"
path_prepend PATH /opt/homebrew/bin /opt/homebrew/sbin

# Node.js (nvm)
export NVM_DIR "$HOME/.nvm"
path_prepend PATH $NVM_DIR/versions/node/v22.17.1/bin

# Other tools
path_prepend PATH $HOME/.local/bin $HOME/bin

# Aliases
alias ll 'ls -la'
alias gs 'git status'
alias gp 'git push'

# Shell variables
var EDITOR vim
var PAGER less

# Custom functions
fun mkcd
    mkdir -p $1 && cd $1
end
```

### path_prepend

The `path_prepend` builtin adds paths to a variable while **deduplicating**—safe to run on every shell without duplicating entries:

```sh
path_prepend PATH /opt/homebrew/bin /opt/homebrew/sbin
path_prepend MANPATH /opt/homebrew/share/man
```

### Custom Prompt

The default prompt shows your current directory in green, and if you're in a git repository, the current branch in magenta:

```
~/projects/oshen (main) #
```

To customize this, define a function named `prompt`. The function's stdout becomes the prompt string:

```sh
# Simple custom prompt
fun prompt
    print "oshen > "
end

# Show git branch
fun prompt
    var branch (git branch --show-current 2>/dev/null)
    if test -n "$branch"
        print "[$branch] $ "
    else
        print "$ "
    end
end

# Colorful prompt with path
fun prompt
    print -n --green (pwd -t) --reset " # "
end
```

---

## Colored Output with `print`

The `print` builtin works like `echo` but supports inline color flags for expressive terminal output:

```sh
# Simple text
print "Hello, world!"

# Colored output
print --green "✓ Build succeeded"
print --red "✗ Build failed"
print --yellow "⚠ Warning: disk space low"

# Interleaved colors in one line
print --green "PASS" --reset "tests/unit.zig"
print --blue "info:" --reset "Server started on port" --cyan "8080"

# Styled text
print --bold "Important:" --reset "Read carefully"
print --dim "Last updated: "(date)

# Combining styles
print --bold --red "ERROR" --reset "Connection refused"

# In scripts
if zig build
    print --green "✓" --reset "Build completed"
else
    print --red "✗" --reset "Build failed"
end
```

**Supported flags:**
- Colors: `--red`, `--green`, `--yellow`, `--blue`, `--magenta`, `--purple`, `--cyan`, `--gray`
- Styles: `--bold`, `--dim`, `--reset`
- Options: `-n` (no newline)

Colors auto-reset at the end of each `print`—no need to add `--reset` at the end.

---

# Core Language

## Variables

Use `var` to create shell variables:

```sh
var name "Alice"
var colors red green blue    # List with multiple values

echo $name                   # Alice
echo $colors                 # red green blue
```

### Variable Expansion

```sh
var greeting hello
echo $greeting               # Simple: hello
echo ${greeting}             # Braced: hello
echo "$greeting world"       # In double quotes: hello world
echo '$greeting'             # Single quotes: literal $greeting
```

### List Variables

Variables in Oshen are **lists by default**:

```sh
var files a.txt b.txt c.txt

# Each item becomes a separate argument
echo $files                  # Equivalent to: echo a.txt b.txt c.txt

# Prefix/suffix applies to each item (cartesian product)
echo test_$files             # test_a.txt test_b.txt test_c.txt
```

### Array Indexing

Access individual elements or slices of a list using bracket notation. Indices are **1-based**, and negative indices count from the end:

```sh
var colors red green blue yellow

echo $colors[1]              # red (first element)
echo $colors[2]              # green
echo $colors[-1]             # yellow (last element)
echo $colors[-2]             # blue (second-to-last)
```

#### Slicing

Use `..` to extract a range of elements (both bounds inclusive):

```sh
var nums a b c d e

echo $nums[2..4]             # b c d
echo $nums[3..]              # c d e (from index 3 to end)
echo $nums[..2]              # a b (from start to index 2)
echo $nums[-2..-1]           # d e (last two elements)
```

#### With argv

Commonly used to access function arguments:

```sh
fun greet
    print "Hello, $argv[1]!"
    print "Remaining args: $argv[2..]"
end

greet Alice Bob Carol
# Hello, Alice!
# Remaining args: Bob Carol
```

| Syntax | Description |
|--------|-------------|
| `$var[n]` | Element at index n (1-based) |
| `$var[-n]` | Element n from the end |
| `$var[a..b]` | Elements from a to b (inclusive) |
| `$var[n..]` | Elements from n to end |
| `$var[..n]` | Elements from start to n |

### Variable Scoping

Variables in oshen use **block scoping** — new variables created inside blocks (if, while, each, functions) are local to that block:

```sh
# New variables in blocks are local
if true
    var x "only inside if"
end
echo $x                      # Empty - x doesn't exist here

# But setting an EXISTING variable updates it
var count 0
if true
    var count 1              # Updates outer count
end
echo $count                  # 1

# Function arguments ($argv) are always local
var argv "original"
fun greet
    echo "Hello $argv[1]"
end
greet Alice                  # Hello Alice
echo $argv                   # original (restored)
```

This scoping model matches elvish and prevents variables from "leaking" out of blocks. Unlike zsh/bash/fish where variables in if-blocks are visible outside, oshen keeps block-local variables contained.

| Block Type | New vars visible outside? | Updates outer vars? |
|------------|--------------------------|---------------------|
| `if`/`else` | No | Yes |
| `while` | No | Yes |
| `each`/`for` | No | Yes |
| `fun` | No | Yes |

### Special Expansions

```sh
~                            # Home directory
~/projects                   # Home + path
$?                           # Last exit status
$status                      # Same as $?
```

### Positional Parameters

Inside functions, access arguments by position:

```sh
fun greet
    print "Hello, $1!"       # First argument
    print "You said: $2"     # Second argument
    print "All args: $*"     # All arguments
    print "Count: $#"        # Number of arguments
end

greet Alice "good morning"
# Hello, Alice!
# You said: good morning
# All args: Alice good morning
# Count: 2
```

| Variable | Description |
|----------|-------------|
| `$1`-`$9` | Positional arguments |
| `$#` | Argument count |
| `$*` | All arguments (as separate words) |
| `$argv` | All arguments (as a list) |

---

## Environment Variables

Use `export` to make variables available to child processes:

```sh
export PATH "$HOME/bin:$PATH"
export EDITOR vim

# Or use = syntax
export EDITOR=vim

# Export an existing shell variable
var MY_VAR value
export MY_VAR
```

---

## Quoting & Escaping

### Single Quotes — Literal

```sh
echo 'No $expansion here'    # No $expansion here
echo 'Preserves "quotes"'    # Preserves "quotes"
```

### Double Quotes — Interpolated

```sh
set name Alice
echo "Hello, $name"          # Hello, Alice
echo "Tab:\there"            # Tab:    here
```

Supported escapes: `\n`, `\t`, `\\`, `\"`, `\$`

### Backslash Escaping

```sh
echo \$literal               # $literal
echo hello\ world            # hello world (single argument)
```

---

## Comments

```sh
# This is a comment
echo hello  # inline comment
```

---

# Control Flow

## If Statements

```sh
if test -f config.txt
    print "Config found"
else
    print "No config"
end

# Inline form
if test $x -eq 0; print "zero"; end
```

The condition is any command — exit status 0 means true.

### Else If Chains

Use `else if` to chain multiple conditions:

```sh
var x 15

if test $x -lt 10
    print "small"
else if test $x -lt 20
    print "medium"
else if test $x -lt 100
    print "large"
else
    print "huge"
end
```

Only the first matching branch executes. The final `else` is optional.

---

## Each Loops

The `each` statement iterates over items, providing automatic `$item` and `$index` variables:

```sh
# Simple form - items available as $item, index as $index
each *.txt
    print "[$index] Processing $item"
end

# Named form - custom variable name
each file in *.txt
    print "Processing $file (index: $index)"
end

# Loop over a variable list
var names Alice Bob Carol
each name in $names
    print "Hello, $name"
end

# Inline form
each a b c; print $item; end
```

### The `for` Keyword

The `for` keyword is an alias for `each` with identical behavior:

```sh
# These are equivalent:
each x in a b c
    print $x
end

for x in a b c
    print $x
end

# for also supports implicit $item and $index:
for *.txt
    print "[$index] $item"
end
```

Use `each` as the primary syntax—`for` is provided for familiarity with other shells.

---

## While Loops

Repeat while a condition is true:

```sh
var count 5
while test $count -gt 0
    print "Countdown: $count"
    var count (= $count - 1)
end

# Wait for a file to appear
while test ! -f ready.txt
    print "Waiting..."
    sleep 1
end

# Inline form
while true; print "loop"; end
```

The condition is any command — exit status 0 means continue.

---

## Break and Continue

Control loop execution with `break` and `continue`:

### Break

Exit the loop immediately:

```sh
for i in 1 2 3 4 5
    print $i
    if test $i -eq 3
        break
    end
end
# Output: 1 2 3
```

### Continue

Skip the rest of the current iteration and continue with the next:

```sh
for i in 1 2 3 4 5
    if test $i -eq 3
        continue
    end
    print $i
end
# Output: 1 2 4 5
```

Both `break` and `continue` work in `each` and `while` loops.

---

## Conditional Chaining

```sh
test -f file.txt && print "exists"
test -f file.txt || print "missing"

# Word forms
test -f file.txt and print "exists"
test -f file.txt or  print "missing"
```

---

## Functions

Define reusable commands:

```sh
fun greet
    print "Hello, $argv[1]!"
end

greet Alice                  # Hello, Alice!

# Inline definition (single statement)
fun hi; print "Hi there"; end
```

Functions receive arguments in `$argv`. Use `return` to exit early with a specific exit code:

```sh
fun check_file
    if test ! -f $1
        return 1        # Exit with status 1 (failure)
    end
    cat $1
    return 0            # Exit with status 0 (success)
end

# Check the exit status
check_file config.txt
echo $?                 # Prints the return code
```

---

## Defer

Schedule cleanup commands that run automatically when a function exits. Inspired by Go and Zig, `defer` ensures resources are released regardless of how the function exits—normal completion, early return, or error.

```sh
fun with_tempdir
    var tmpdir (mktemp -d)
    defer rm -rf $tmpdir           # Runs when function exits

    print "Working in $tmpdir"
    cp *.txt $tmpdir
    tar -czf archive.tar.gz $tmpdir
end

with_tempdir                       # Temp dir is automatically cleaned up
```

### LIFO Execution Order

Multiple defers execute in reverse order (last in, first out):

```sh
fun setup_and_teardown
    defer print "3. final cleanup"
    defer print "2. close connection"
    defer print "1. release lock"
    print "doing work..."
end

setup_and_teardown
# Output:
# doing work...
# 1. release lock
# 2. close connection
# 3. final cleanup
```

### Works with Early Returns

Deferred commands run even when returning early:

```sh
fun safe_process
    var tmpfile (mktemp)
    defer rm $tmpfile

    if test ! -f input.txt
        return 1                   # Cleanup still happens
    end

    cat input.txt > $tmpfile
    wc -l $tmpfile
end
```

---

# Commands & I/O

## Pipelines

```sh
cat file.txt | grep error | wc -l

# Alternative syntax (reads like data flow)
cat file.txt |> grep error |> wc -l
```

---

## Redirections

### Output Redirection

| Operator | Description |
|----------|-------------|
| `>`      | Write stdout (truncate) |
| `>>`     | Write stdout (append) |
| `2>`     | Write stderr (truncate) |
| `2>>`    | Write stderr (append) |
| `&>`     | Write both stdout and stderr |
| `2>&1`   | Redirect stderr to stdout |

### Input Redirection

```sh
sort < unsorted.txt
wc -l < data.txt
```

### Examples

```sh
# Save output to file
ls -la > listing.txt

# Append to log
echo "Done" >> log.txt

# Redirect errors
./script.sh 2> errors.log

# Combine stderr with stdout in pipeline
./script.sh 2>&1 | grep ERROR
```

---

## Output Capture

Capture command output directly into variables:

### String Capture (`=>`)

```sh
git rev-parse --short HEAD => sha
print "Current commit: $sha"

whoami => user
print "Logged in as $user"
```

### Lines Capture (`=>@`)

Split output into a list (one item per line):

```sh
ls *.txt =>@ files
for f in $files
    print "File: $f"
end
```

> **Tip:** Output capture (`=>`) is great for simple assignments. For inline use, prefer bare parens: `var files (ls *.txt)`

---

## Command Substitution

Capture command output inline using `(...)` or `$(...)`:

```sh
print "Today is "(date +%A)
var files (ls *.txt)

# Multi-line output becomes a list
for f in (ls)
    print "- $f"
end
```

### Bare Parens vs Dollar Parens

Both syntaxes work, with a subtle difference:

```sh
# Bare parens - clean, fish-style (preferred for concatenation)
echo (whoami)                    # hello
echo "user: "(whoami)            # user: alice  (concat outside quotes)

# Dollar parens - works inside quotes
echo "user: $(whoami)"           # user: alice  (interpolated inside quotes)
```

Use bare `(...)` for cleaner code. Use `$(...)` when you need interpolation inside double quotes.

---

## Job Control

Run and manage background processes:

### Background Execution

```sh
sleep 30 &                   # Run in background
[1] 12345                    # Job ID and PID
```

### Managing Jobs

```sh
jobs                         # List all jobs
fg 1                         # Bring job 1 to foreground
bg 1                         # Resume stopped job in background
fg                           # Foreground most recent job
```

### Signals

- **Ctrl+C** — Send SIGINT to foreground job
- **Ctrl+Z** — Stop foreground job, return to prompt

---

## Glob Patterns

Oshen expands glob patterns against the filesystem:

```sh
*.txt                        # All .txt files
src/*.zig                    # .zig files in src/
test?.txt                    # test1.txt, testA.txt, etc.
[abc].txt                    # a.txt, b.txt, or c.txt
[a-z].txt                    # Single lowercase letter .txt
[!0-9].txt                   # Not starting with a digit
```

| Pattern | Matches |
|---------|----------|
| `*` | Any sequence of characters |
| `**` | Any directory depth (recursive) |
| `?` | Any single character |
| `[abc]` | Any character in the set |
| `[a-z]` | Any character in the range |
| `[!abc]` or `[^abc]` | Any character NOT in the set |

### Recursive Globbing

Use `**` to match files across any directory depth:

```sh
**/*.zig                     # All .zig files anywhere
src/**/*.zig                 # All .zig files under src/
**/*_test.zig                # All test files anywhere
```

Globs are **suppressed in quotes**:

```sh
echo "*.txt"                 # Literal: *.txt
echo *.txt                   # Expanded: file1.txt file2.txt ...
```

If no files match, the pattern is passed through literally.

## Brace Expansion

Brace expansion provides an explicit syntax for Cartesian products with globs, variables, and literal lists.

### Comma-Separated Lists

```sh
echo {a,b,c}                 # a b c
echo prefix_{x,y,z}          # prefix_x prefix_y prefix_z
echo {hello,goodbye}_world   # hello_world goodbye_world
```

### Globs with Prefixes/Suffixes

Braces enable you to add literal text to each glob match:

```sh
{*.txt}_backup               # file1.txt_backup file2.txt_backup ...
test_{*.zig}                 # test_main.zig test_util.zig ...
```

Without braces, the entire string is treated as one glob pattern:
```sh
*.txt_backup                 # Looks for files matching "*.txt_backup"
{*.txt}_backup               # Globs *.txt first, then appends _backup
```

### Variables with Suffixes

Apply literal text to each item in a list variable:

```sh
var files a.txt b.txt c.txt
echo {$files}_backup         # a.txt_backup b.txt_backup c.txt_backup
```

Oshen also supports this with the multi-part word syntax:
```sh
echo $files"_backup"         # Same result as above
```

### Nested Braces (Cartesian Product)

Multiple braces create a cartesian product:

```sh
{a,b}_{x,y}                  # a_x a_y b_x b_y
test_{1,2}_file_{A,B}.txt    # test_1_file_A.txt test_1_file_B.txt
                             # test_2_file_A.txt test_2_file_B.txt
```

### When to Use Braces

- **Explicit globs with affixes**: `{*.txt}_backup` makes it clear you're appending to glob results
- **Literal lists**: `{dev,staging,prod}_config.json` is more explicit than other approaches
- **Nested cartesians**: `{a,b}_{1,2}` clearly shows the multiplicative expansion
- **Alternative to quotes**: Some prefer `{$var}_suffix` over `$var"_suffix"` for clarity

Both syntaxes work—choose based on readability preference!

---

# Reference

## Builtins

| Command | Description |
|---------|-------------|
| `alias [name [cmd...]]` | Define or list command aliases |
| `bg [n]` | Resume job in background |
| `cd [dir]` | Change directory (`cd` alone goes home, `cd -` returns to previous) |
| `echo [-n] [args...]` | Print arguments (`-n`: no newline, supports `\e` for ESC) |
| `print [-n] [--color]... [text]...` | Print with colors (`--green`, `--red`, `--yellow`, `--blue`, `--magenta`, `--purple`, `--cyan`, `--gray`, `--bold`, `--dim`, `--reset`) |
| `eval [code...]` | Execute arguments as shell code |
| `exit [code]` | Exit shell with optional status |
| `export [name [value]]` \| `[name=value]` | Export to environment |
| `false` | Return failure (exit 1) |
| `fg [n]` | Bring job to foreground |
| `jobs` | List background/stopped jobs |
| `path_prepend VAR path...` | Prepend paths to a variable (deduplicates) |
| `pwd [-t]` | Print working directory (`-t`: replace $HOME with ~) |
| `range [FIRST] LAST [-s STEP]` | Print a sequence of numbers (auto-detects direction) |
| `string [FLAGS...] STRING...` | String manipulation (transform, split, join, match, etc.) |
| `var` / `set `[name [values...]]` | Get/set shell variables |
| `source <file>` | Execute file in current shell |
| `calc <expr>` / `= <expr>` | Evaluate arithmetic expression (+ - x * / %) |
| `increment <var> [--by <n>]` | Increment variable by n (default: 1) |
| `test <expr>` / `[ <expr> ]` | Evaluate conditional expression |
| `true` | Return success (exit 0) |
| `type <name>...` | Show how a name resolves (alias/builtin/function/external) |
| `unalias <name>...` | Remove command aliases |
| `unset <name>...` | Remove shell variables and environment variables |

> Note: All builtins support `-h` or `--help` to display usage information.

### Test Expressions

The `test` builtin (and its `[` alias) evaluates conditional expressions:

| Expression | True if... |
|------------|------------|
| **File tests** | |
| `-e FILE` | File exists |
| `-f FILE` | Regular file exists |
| `-d FILE` | Directory exists |
| `-r FILE` | File is readable |
| `-w FILE` | File is writable |
| `-x FILE` | File is executable |
| `-s FILE` | File has size > 0 |
| `-L FILE` | File is a symbolic link |
| **String tests** | |
| `-z STRING` | String is empty |
| `-n STRING` | String is non-empty |
| `S1 = S2` | Strings are equal |
| `S1 != S2` | Strings are different |
| **Numeric tests** | |
| `N1 -eq N2` | Numbers are equal |
| `N1 -ne N2` | Numbers are not equal |
| `N1 -lt N2` | N1 < N2 |
| `N1 -le N2` | N1 ≤ N2 |
| `N1 -gt N2` | N1 > N2 |
| `N1 -ge N2` | N1 ≥ N2 |
| **Logical** | |
| `! EXPR` | Negate expression |
| `EXPR -a EXPR` | AND |
| `EXPR -o EXPR` | OR |

```sh
# File tests
if test -f config.json; print "config exists"; end
if [ -d ~/.config ]; print "config dir exists"; end

# String tests
if test -n "$name"; print "name is set"; end
if [ "$x" = "yes" ]; print "x is yes"; end

# Numeric tests
var count 5
if [ $count -gt 0 ]; print "count is positive"; end
```

### Numeric Operations

#### Arithmetic with `calc` / `=`

The `calc` builtin (or its expressive `=` alias) evaluates arithmetic expressions:

```sh
= 2 + 3                          # 5
calc 10 - 3                      # 7
= 20 / 4                         # 5 (integer division)
= 17 % 5                         # 2 (modulo)
= 4 x 5                          # 20 (x as multiplication - no quoting needed!)
```

The `=` alias reads naturally: `var sum (= $a + $b)` → "sum equals a plus b"

#### Multiplication with `x` or `*`

For multiplication, use `x` (no quoting needed) or `*` (requires quoting):

```sh
# Use x - clean and simple (recommended)
= 4 x 5                          # 20
= 2 x 3 x 4                      # 24

# Or quote * if you prefer the traditional operator
= "4 * 5"                        # 20
= "(2 + 3) * 4"                  # 20
```

The `x` operator reads naturally: "4 times 5"

**Quoting Parentheses**

Since `(` triggers command substitution, quote complex expressions with parentheses:

```sh
= "(2 + 3) x 4"                  # 20 (parentheses for grouping)
= "((2 + 3) x 2) + 1"            # 11 (nested parentheses)
```

**Operator Precedence**

Multiplication (both `x` and `*`), division, and modulo bind tighter than addition/subtraction:

```sh
= 2 + 3 x 4                      # 14 (not 20)
= "(2 + 3) x 4"                  # 20 (parens override)
```

**With Variables and Loops**

```sh
var x 5
var y (= $x + 3)                 # y = 8

# Countdown loop
var count 5
while test $count -gt 0
    print "Countdown: $count"
    var count (= $count - 1)
end

# Sum a list
var sum 0
for i in 1 2 3 4 5
    var sum (= $sum + $i)
end
print "Sum: $sum"                # Sum: 15
```

**Supported Operators**

| Operator | Description |
|----------|-------------|
| `+` | Addition |
| `-` | Subtraction (also unary minus) |
| `x` | Multiplication (no quoting needed) |
| `*` | Multiplication (requires quoting) |
| `/` | Integer division |
| `%` | Modulo |
| `()` | Grouping (requires quoting) |

#### Incrementing with `increment`

The `increment` builtin provides a clean way to increment (or decrement) numeric variables:

```sh
var count 0
increment count                  # count = 1
increment count                  # count = 2
increment count --by 5           # count = 7
increment count --by -3          # count = 4 (decrement)
```

This is much cleaner than `var count (= $count + 1)` for the common case of incrementing counters, especially in loops and test frameworks.

**Syntax:**
- `increment <var>` — Increment by 1 (default)
- `increment <var> --by <n>` — Increment by n (can be negative for decrement)

**Examples:**
```sh
# Test counter
var PASS 0
increment PASS                   # PASS = 1

# Custom increment
var score 100
increment score --by 50          # score = 150

# Decrement
var lives 3
increment lives --by -1          # lives = 2
```

### Generating Sequences with `range`

The `range` builtin prints sequences of numbers, perfect for loops and data generation:

```sh
range 5                          # 1 2 3 4 5 (newline-separated)
range 3 7                        # 3 4 5 6 7
range 10 1                       # 10 9 8 ... 1 (auto-detects countdown)
range --step 2 1 10              # 1 3 5 7 9
range -s 2 1 10                  # Same as above (short form)
```

**Syntax:**
- `range LAST` — Count from 1 to LAST
- `range FIRST LAST` — Count from FIRST to LAST
- `range -s STEP FIRST LAST` — Count with custom step

**Options:**
| Option | Description |
|--------|-------------|
| `-s`, `--step N` | Increment between numbers (default: 1 or -1) |
| `-S`, `--separator S` | Separator between numbers (default: newline) |

**Examples:**
```sh
# Use in loops
for i in (range 1 5)
    print "Item $i"
end

# Custom separator
range -S ", " 1 5                # 1, 2, 3, 4, 5

# Generate test data
for n in (range 100 1)
    print "Countdown: $n"
end
```

**Smart Direction Detection:**

If FIRST > LAST and no step is given, `range` automatically counts down:
```sh
range 5 1                        # 5 4 3 2 1 (auto-detected)
range --step -1 5 1              # Same, but explicit
```

### String Manipulation with `string`

The `string` builtin provides powerful text processing with zero heap allocations:

```sh
string --upper "hello"           # HELLO
string --lower "HELLO"           # hello
string --trim "  hello  "        # hello
string --reverse "hello"         # olleh
```

**Transform Flags (combinable):**

| Flag | Description |
|------|-------------|
| `--upper` | Convert to uppercase |
| `--lower` | Convert to lowercase |
| `--trim` | Trim whitespace from both ends |
| `--trim-left` | Trim whitespace from start |
| `--trim-right` | Trim whitespace from end |
| `--reverse` | Reverse the string |
| `--length` | Output length instead of content |
| `--escape` | Escape special shell characters |
| `--unescape` | Process escape sequences (`\n`, `\t`, etc.) |

```sh
# Combine transforms
string --trim --upper "  hello  "     # HELLO
string --reverse --upper "hello"      # OLLEH

# Get string length
string --length "hello world"         # 11
```

**Operations:**

| Operation | Description |
|-----------|-------------|
| `--split SEP` | Split by separator (one result per line) |
| `--join SEP` | Join arguments with separator |
| `--replace OLD NEW` | Replace all occurrences |
| `--sub START [LEN]` | Substring (1-indexed, negative from end) |
| `--repeat N` | Repeat N times |
| `--match PATTERN` | Exit 0 if glob matches, 1 otherwise |
| `--contains STR` | Exit 0 if substring found, 1 otherwise |

```sh
# Split and join
string --split : "a:b:c"              # a\nb\nc
string --join , a b c                 # a,b,c

# Replace
string --replace foo bar "foo is foo" # bar is bar

# Substring (1-indexed)
string --sub 2 3 "hello"              # ell (3 chars starting at position 2)
string --sub -3 "hello"               # llo (last 3 characters)

# Repeat
string --repeat 3 "ab"                # ababab

# Pattern matching (no output, just exit code)
if string --match "*.txt" "$file"
    print "It's a text file"
end

# Substring check (no output, just exit code)
if string --contains "error" "$log"
    print "Found an error!"
end
```

**Padding and Truncation:**

| Operation | Description |
|-----------|-------------|
| `--pad N [C]` | Right-pad to width N with char C (default: space) |
| `--pad-left N [C]` | Left-pad to width N |
| `--pad-center N [C]` | Center-pad to width N |
| `--shorten N [ELLIPSIS]` | Truncate to N chars with ellipsis (default: ...) |

```sh
# Padding for alignment
string --pad-left 8 0 "42"            # 00000042
string --pad 10 "hello"               # "hello     "
string --pad-center 10 "hi"           # "    hi    "

# Truncation
string --shorten 10 "Hello, World!"   # Hello, ...
string --shorten 8 "..." "Too long"   # Too l...
```

**Stdin Support:**

When no string arguments are given, `string` reads lines from stdin:

```sh
echo "hello" | string --upper        # HELLO
cat file.txt | string --trim         # Trim each line
```

---

## Command Line Options

| Option | Description |
|--------|-------------|
| `-c <cmd>` | Execute command and exit |
| `-i` | Force interactive mode |
| `-h`, `--help` | Show help |
| `-v`, `--version` | Show version |
