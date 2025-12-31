# 👍 Oshen: Contributors Guide

## 🧠 Writing great code

When planning out or implementing a request, make sure to take the following (seemingly obvious) values into consideration:

1. All code should be as concise and elegant as possible (e.g. no duplication, simple abstractions), while still embracing neccesary complexity
2. All code should feel extremely idiomatic, and feel like it was written by a Zig expert (e.g. using defer, union enums + switch, error sets, etc.)
3. All code should be well-tested, including both unit and E2E tests (`scripts/e2e.wave`), since that's how we'll sustain our velocity and ambition
4. All code should be well-organized, and intuitively placed. The folder structure + file names are like a map of the project, and we want it to read beautifully 🤌
5. All code should be written with performance in mind, while also balancing readability and ergonomics.

## 🧪 Writing great tests

Tests are code we live in, not code that gets stale. So keep the following values in consideration whenever you make a change that might need test coverage:

1. Unit tests should verify isolated behavior, and E2E tests should verify integration/user experience. Avoid unhelpful duplication between layers, but respect their distinct roles:
   1. Unit tests help safeguard foundational logic that is relied upon by almost every other part of the system. For example, we don't want hundreds of failing E2E tests when the lexer, parser, or word expansion breaks. Code that resides at "edge" of the architecture doesn't have the same needs (e.g. simple built-ins like `echo`)
   2. Unit tests can be run on a per-file basis, and therefore, they provide a focused means of validating correctness after making a change.
   3. Oshen's E2E tests (`scripts/e2e.wave`) are written in Oshen itself, and therefore, they provide a means of "dogfooding"/performance testing the system on every change. And since they run in <1s, they provide holistic signal without adding friction to the feedback loop.
2. For unit tests specifically (see `src/language/lexer.zig` and `src/language/parser.zig` as canonical examples):
   - Organize tests with section headers (`=` for top-level sections, `-` for subsections) and descriptive test names (e.g., `"Category: behavior being tested"`)
   - Group tests by feature category first, then within each category order: basic → complex → error cases
   - Consolidate related assertions into a single test with inline comments. Don't create a separate test for each minor variation, but also, don't group logically seperate assertions
   - Use table-driven tests with anonymous tuples when testing multiple cases with the same structure
   - If tests need repetitive setup, create a `TestContext` struct to encapsulate allocation, the primary operation, and module-specific assertions. Omit this if setup is minimal (see `src/repl/editor/ui/highlight.zig`)
   - Test edge cases explicitly (wraparound, empty inputs, nonexistent keys, error conditions)
3. Some code is inherently hard to unit test (fork/exec, terminal I/O, signal handling). These are better covered by E2E tests
    - The entire `interpreter/execution` folder is covered by E2E tests
4. For `runtime/builtins`, follow a complexity-based testing strategy:
   - **Simple builtins** (e.g., `echo`, `pwd`, `cd`, `true`, `false`, `unset`, `alias`, `var`): E2E tests only. Since `args.zig` is heavily unit tested, argument parsing is already covered. These builtins have minimal logic beyond args handling.
   - **Complex builtins** (e.g., `calc`, `string`, `test`): Unit tests + E2E tests. These have custom parsers, state machines, or non-trivial logic that benefits from direct unit testing of edge cases, error handling, and internal behavior.
   - **TTY-dependent builtins** (e.g., `terminal`, `jobs`, `fg`, `bg`): Unit tests only (for `terminal`) or no automated tests. These require a real TTY and/or process groups that can't be exercised in piped E2E tests.

## 📋 Post-change/pre-commit review checklist

After you've finished a code change, make sure you perform the following steps, no matter how small or seemingly simple your changes might seem:

1. Run `zig fmt` (format code)
2. Run `zig build` (compile)
3. Run `zig build test` (unit tests) _Note: many tests write errors to stderr, because they're testing actual error handling. So focus on test failures not test output._
4. Run `./zig-out/bin/oshen scripts/e2e.wave` (e2e tests)

Optionally, if your change...

1. Might have had a performance impact, then perform the following additional steps:
   1. Compile a fast release build of Oshen
   2. Run the `scripts/benchmark.wave` file before and after your changes (via git stashing) and ensure it didn't slow down
2. Introduced or changed a user-facing behavior, then make sure to update the `README.md`
3. Made a meaningful change/addition to the system, then review `docs/DESIGN.md` and:
   - Confirm your change adheres to the design philosophy (commands all the way down, high-performance, modern DX, no surprises)
   - Update the doc with new details, decisions, or tradeoffs if your change introduces architectural patterns worth documenting

If any of this is missed, then the code won't be ready for review.