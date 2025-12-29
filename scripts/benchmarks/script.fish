#!/usr/bin/env fish
# Shell benchmark: loops, functions, conditionals, globs, builtins

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
    for f in /usr/bin/*
        process_file
    end
    set -g i (math $i + 1)
end
