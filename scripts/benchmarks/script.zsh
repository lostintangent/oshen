#!/bin/zsh
# Shell benchmark: loops, functions, conditionals, globs, pipelines, external commands

process_file() {
    if (( count % 2 == 0 )); then
        (( count += 2 ))
    else
        (( count++ ))
    fi
}

i=0
while (( i < 100 )); do
    count=0

    # Glob iteration with function calls
    for f in /usr/bin/*; do
        process_file
    done

    # External command execution
    /usr/bin/true

    # Pipeline with redirection
    echo "test data" | cat > /dev/null

    # Command substitution (external)
    ts=$(date +%s)

    # String interpolation
    msg="Iteration $i completed at $ts"

    # Logical chaining
    [[ -f /usr/bin/ls ]] && found=1
    [[ -f /nonexistent ]] || missing=1

    # Array indexing
    items=(alpha beta gamma delta epsilon)
    first=${items[1]}
    last=${items[-1]}
    slice=(${items[2,4]})

    # Nested loops
    for outer in 1 2 3; do
        for inner in a b c; do
            pair="$outer$inner"
        done
    done

    (( i++ ))
done
