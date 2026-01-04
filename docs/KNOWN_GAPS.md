# Known Gaps

This document tracks known feature and capability gaps not yet implemented in Oshen:

- [Language](#language)
- [Runtime](#runtime)
- [REPL](#repl)

---

## Language

### Recursion Depth Limit

Recursive functions are limited to 40 calls deep due to stack constraints. Tail-call optimization is not implemented.

```wave
fun countdown
    if test $argv[1] -gt 0
        countdown (= $argv[1] - 1)
    end
end
countdown 50  # Error: countdown: maximum recursion depth (40) exceeded
```

**Workaround:** Use `while` loops for deep iterations.

### Heredocs

Heredoc syntax (`<<EOF`) is not supported.

```wave
# Not supported
cat <<EOF
Hello, $name
EOF
```

**Workaround:** Use `echo` with escaped newlines or multiple `echo` commands.

### Process Substitution

Process substitution (`<(command)`) is not supported.

```wave
# Not supported
diff <(sort file1) <(sort file2)
```

**Workaround:** Use temporary files or pipes.

### Default Value Expansion

Parameter expansion modifiers are not supported.

```wave
# Not supported
echo ${VAR:-default}    # Use default if unset
echo ${VAR:=default}    # Assign default if unset
echo ${VAR:+alternate}  # Use alternate if set
echo ${VAR:?error}      # Error if unset
```

**Workaround:** Use explicit conditionals:
```wave
if test -z "$VAR"
    var VAR default
end
```

### Switch/Case Statement

Pattern-matching switch statements are not supported.

```wave
# Not supported
switch $color
    case red orange yellow
        echo "warm"
    case blue green
        echo "cool"
    case '*'
        echo "unknown"
end
```

**Workaround:** Use chained `if`/`else if` statements.

### Defer in Blocks and Scripts

The `defer` statement only works inside functions. It does not execute deferred commands when exiting loops, blocks, or top-level scripts.

```wave
# Works - defer in function
fun cleanup
    defer echo "cleaning up"
    echo "doing work"
end

# Does not work - defer in script
defer echo "script exit"  # Never executes
echo "script body"
```

**Workaround:** Use explicit cleanup code or wrap logic in a function.

---

## Runtime

### Missing Builtins

| Builtin | Description | Workaround |
|---------|-------------|------------|
| `read` | Read user input into a variable | Redirect from file |
| `pushd`/`popd` | Directory stack navigation | Track manually with variables |
| `wait` | Wait for background jobs | — |

### Missing Special Variables

| Variable | Description |
|----------|-------------|
| `$$` | Current shell PID |
| `$!` | Last background job PID |
| `$0` | Script name or shell path |

### Missing Test Operators

| Operator | Description |
|----------|-------------|
| `-t FD` | Test if file descriptor is a TTY |
| `-nt` | File1 newer than file2 |
| `-ot` | File1 older than file2 |
| `-ef` | Files are same inode (hard link) |

### Function Shadowing

User-defined functions cannot override builtins of the same name.

```wave
fun echo
    print --green "Custom: $*"
end
echo "test"  # Still uses builtin echo
```

---

## REPL

### Job Suspension

Ctrl+Z job suspension is not fully tested. The `jobs`, `fg`, and `bg` commands are implemented but may have edge cases in some environments.
