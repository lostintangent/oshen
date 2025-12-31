#!/usr/bin/env fish
# Shell benchmark: loops, functions, conditionals, globs, pipelines, external commands

function process_file
    if test (math "$count % 2") -eq 0
        set -g count (math $count + 2)
    else
        set -g count (math $count + 1)
    end
end

set -g i 0
while test $i -lt 100
    set -g count 0

    # Glob iteration with function calls
    for f in /usr/bin/*
        process_file
    end

    # External command execution
    /usr/bin/true

    # Pipeline with redirection
    echo "test data" | cat > /dev/null

    # Command substitution (external)
    set -l ts (date +%s)

    # String interpolation
    set -l msg "Iteration $i completed at $ts"

    # Logical chaining
    test -f /usr/bin/ls; and set -l found 1
    test -f /nonexistent; or set -l missing 1

    # Array indexing
    set -l items alpha beta gamma delta epsilon
    set -l first $items[1]
    set -l last $items[-1]
    set -l slice $items[2..4]

    # Nested loops
    for outer in 1 2 3
        for inner in a b c
            set -l pair "$outer$inner"
        end
    end

    set -g i (math $i + 1)
end
