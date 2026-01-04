# 🌊 Oshen

Oshen is a modern terminal shell, with a nice developer experience out-of-the-box:

- **Zero config** — Syntax highlighting, autosuggestions, tab completion, and a git-aware prompt.
- **Expressive syntax** — Compose commands, enumerate lists, define functions, expand globs, and more!
- **Modern builtins** — Colored `print`, `terminal` for TUIs, `string` and `list` for data processing.
- **Fast and efficient** — Written in Zig. Starts instantly (~3ms), runs scripts quickly, and uses minimal memory.

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

## At-a-glance

```sh
# Shell basics
ls -la | grep .txt                # Basic multi-step pipelines
cat file.txt > output.txt         # Write command output to files
sleep 10 && print "Done!"         # Compose conditional chains of commands
var files (ls *.txt)              # Capture command output in a variable

# Variables
var name Jonathan Carter          # Variables are lists by default
print --green Hello --bold $name  # Colored print + list expansion
list 1..50 => numbers             # Write a command's output to a variable

# Loops
each *.txt
    print "Processing $item".     # $item is automatically bound
end

each {1..50}                      # Define loops are ranges
    ...
end

# Globs and brace expansion
rm *.log                          # Delete all .log files
mkdir -p src/{lib,bin,test}       # Create multiple directories


# string/list processing w/two-way binding
var names jon julian alden jon    # Variables are lists by default
list --sort --unique @names       # Sorts/dedupes the list var in place
string --upper --join "-" @names  # Uppercase/join the list in place
print $names                      # Prints: ALDEN-JON-JULIAN

# Functions with defer
fun process
   mkdir /tmp/scratch
   defer rm -rf /tmp/scratch      # Define cleanup at the point of allocation

   ...
end
```

---

### Commands & I/O

- [Pipelines](#pipelines)
- [Redirections](#redirections)
- [Globs](#globs)
- [Command Substitution](#command-substitution)
- [Output Capture](#output-capture)
- [Job Control](#job-control)

### Core Language

- [Variables](#variables)
- [Lists](#lists)
- [Conditionals](#conditionals)
- [Loops](#loops)
- [Functions](#functions)
- [Comments](#comments)

### Interactive Shell

- [Syntax Highlighting](#syntax-highlighting)
- [Autosuggestions](#autosuggestions)
- [Tab Completion](#tab-completion)
- [Line Editing](#line-editing)
- [History](#history)
- [Configuration](#configuration)

### Reference

- [Builtins](#builtins)
- [Command Line Options](#command-line-options)

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

## Globs

Glob patterns expand against the filesystem. Each match becomes a separate value:

```sh
*.txt                        # All .txt files
src/*.zig                    # .zig files in src/
test?.txt                    # test1.txt, testA.txt, etc.
**/*.zig                     # All .zig files anywhere (recursive)
```

| Pattern | Matches |
|---------|----------|
| `*` | Any sequence of characters |
| `**` | Any directory depth (recursive) |
| `?` | Any single character |
| `[abc]` | Any character in the set |
| `[a-z]` | Any character in the range |
| `[!abc]` or `[^abc]` | Any character NOT in the set |

Globs are suppressed in quotes: `echo "*.txt"` prints a literal asterisk.

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

Use bare `(...)` for cleaner code. Use `$(...)` when you need interpolation inside double quotes: `"user: $(whoami)"`.

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

### Environment Variables

Use `export` to make variables available to child processes:

```sh
export PATH "$HOME/bin:$PATH"
export EDITOR vim

# Export an existing shell variable
var MY_VAR value
export MY_VAR
```

### Quoting & Escaping

```sh
echo 'No $expansion here'    # Single quotes: literal, no expansion
echo "Hello, $name"          # Double quotes: variables expand, globs don't
echo \$literal               # Backslash: escape single characters
```

Supported escapes in double quotes: `\n`, `\t`, `\\`, `\"`, `\$`

---

## Lists

Variables in Oshen are **lists by default**. The language provides concise syntax for creating, accessing, and transforming lists.

### Creating Lists

```sh
var colors red green blue        # Literal values
var nums {1..5}                  # Brace expansion: 1 2 3 4 5
var chars {a,b,c}                # Comma list: a b c
var files (ls *.txt)             # Command output
```

Brace expansion supports variables in ranges:

```sh
var n 10
echo {1..$n}                     # 1 2 3 4 5 6 7 8 9 10
echo file{1..$n}.txt             # file1.txt file2.txt ... file10.txt
```

Cartesian products combine multiple expansions:

```sh
mkdir -p {src,test}/{lib,bin}    # Creates 4 directories
echo {a,b}_{1,2}                 # a_1 a_2 b_1 b_2
```

### Accessing Elements

Indices are **1-based**. Negative indices count from the end:

```sh
var colors red green blue yellow

echo $colors[1]                  # red (first)
echo $colors[-1]                 # yellow (last)
echo $colors[2..3]               # green blue (slice)
echo $colors[2..]                # green blue yellow (to end)
```

| Syntax | Description |
|--------|-------------|
| `$var[n]` | Element at index n |
| `$var[-n]` | Element n from end |
| `$var[a..b]` | Slice from a to b (inclusive) |
| `$var[n..]` | From index n to end |
| `$var[..n]` | From start to index n |

### Iterating

Use `each` or `for` to loop over list elements:

```sh
var files (ls *.txt)
for f in $files
    print "Processing $f"
end

# Or use the implicit $item variable
each $files
    print "File: $item"
end
```

### The `list` Builtin

The `list` command creates and transforms lists:

```sh
# Create ranges
list 1..10                       # 1 through 10
list 1..10 -s 2                  # 1 3 5 7 9 (step by 2)
list a..z                        # a through z

# Transform lists
list @items --sort               # Sort alphabetically
list @items --sort -n            # Sort numerically
list @items --reverse            # Reverse order
list @items --unique             # Remove duplicates

# Mutate in place
list @items --sort --inplace     # Sort and update variable

# Store result
list @items --sort --into sorted # Sort into new variable
```

Use `@varname` to pass a variable by reference (efficient for large lists).

### Testing Lists

```sh
# Check membership
if list @items --contains "foo"
    print "Found foo"
end

# Check if empty
if list @items --empty
    print "No items"
end

# Get length
list @items --length             # Prints count
```

---

## Conditionals

```sh
if test -f config.txt
    print "Config found"
else if test -f config.json
    print "JSON config found"
else
    print "No config"
end
```

The condition is any command—exit status 0 means true.

### Chaining

```sh
test -f file.txt && print "exists"
test -f file.txt || print "missing"

# Word forms
test -f file.txt and print "exists"
test -f file.txt or  print "missing"
```

---

## Loops

### Each / For

Iterate over items with automatic `$item` and `$index` variables:

```sh
each *.txt
    print "[$index] $item"
end

# Named variable
each file in *.txt
    print "Processing $file"
end

# Inline form
each a b c; print $item; end
```

`for` is an alias for `each`.

### While

```sh
var count 5
while test $count -gt 0
    print "Countdown: $count"
    var count (= $count - 1)
end
```

### Break and Continue

```sh
for i in 1 2 3 4 5
    if test $i -eq 2; continue; end    # skip 2
    if test $i -eq 4; break; end       # stop at 4
    print $i
end
# Output: 1 3
```

---

## Functions

```sh
fun greet
    print "Hello, $argv[1]!"
end

greet Alice                  # Hello, Alice!
```

Functions receive arguments in `$argv`. Use `return` to exit early:

```sh
fun check_file
    if test ! -f $1
        return 1
    end
    cat $1
end
```

### Defer

Schedule cleanup that runs when a function exits—regardless of how it exits:

```sh
fun with_tempdir
    var tmpdir (mktemp -d)
    defer rm -rf $tmpdir           # Runs when function exits

    cp *.txt $tmpdir
    tar -czf archive.tar.gz $tmpdir
end
```

Multiple defers execute in reverse order (LIFO).

---

## Comments

```sh
# This is a comment
echo hello  # inline comment
```

---

# Interactive Shell

Oshen's interactive mode provides a modern editing experience out of the box—no plugins or configuration required.

## Syntax Highlighting

Commands, strings, operators, and variables are color-coded as you type:

- **Commands** — Valid commands in green, invalid in red
- **Keywords** — Control flow (`if`, `for`, `while`, etc.) in blue
- **Strings** — Quoted strings in yellow
- **Variables** — `$var` expansions in magenta
- **Operators** — Pipes, redirects, etc. in cyan

## Autosuggestions

As you type, Oshen shows suggestions from your command history in dimmed text:

- Press **→** or **End** to accept the full suggestion
- Press **Alt+→** to accept word-by-word

Suggestions are context-aware—commands from your current directory rank highest.

## Tab Completion

Press **Tab** to complete commands and paths:

```shell
$ ec<Tab>        →  $ echo
$ cat ~/Doc<Tab> →  $ cat ~/Documents/
```

## Line Editing

| Key | Action |
|-----|--------|
| **Ctrl+A** / **Home** | Beginning of line |
| **Ctrl+E** / **End** | End of line |
| **Ctrl+W** | Delete word before cursor |
| **Ctrl+U** | Delete to beginning |
| **Ctrl+K** | Delete to end |
| **Ctrl+L** | Clear screen |

## History

Command history is saved to `~/.oshen_history` and persists across sessions.

| Key | Action |
|-----|--------|
| **↑** / **Ctrl+P** | Previous command |
| **↓** / **Ctrl+N** | Next command |

## Configuration

Oshen loads `~/.oshen_floor` on startup:

```sh
# ~/.oshen_floor

# Add to PATH
path_prepend PATH /opt/homebrew/bin $HOME/.local/bin

# Aliases
alias ll 'ls -la'
alias gs 'git status'

# Custom prompt
fun prompt
    print -n --green (pwd -t) --reset " # "
end
```

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
| `list [OPTIONS] [ITEMS...] [@VAR]` | List manipulation (sort, unique, reverse, contains, length, append, prepend) |
| `path_prepend VAR path...` | Prepend paths to a variable (deduplicates) |
| `pwd [-t]` | Print working directory (`-t`: replace $HOME with ~) |
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

### Arithmetic with `calc` / `=`

The `=` builtin (alias: `calc`) evaluates arithmetic expressions:

```sh
= 2 + 3                          # 5
= 4 x 5                          # 20 (use x for multiplication—no quoting needed)
= 20 / 4                         # 5 (integer division)
= 17 % 5                         # 2 (modulo)
var y (= $x + 3)                 # use in assignments
= "(2 + 3) x 4"                  # 20 (quote expressions with parentheses)
```

Operators: `+`, `-`, `x` or `*`, `/`, `%`, `()`. Standard precedence applies.

### Incrementing with `increment`

```sh
var count 0
increment count                  # count = 1
increment count --by 5           # count = 6
increment count --by -2          # count = 4 (decrement)
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
