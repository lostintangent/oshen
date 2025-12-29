#!/bin/zsh
# Shell benchmark: loops, functions, conditionals, globs, builtins

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
    for f in /usr/bin/*; do
        process_file
    done
    (( i++ ))
done
