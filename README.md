# MGS Lang

A small programming language compiler implemented in C (language name still to be confirmed).

Note: This language is still in very early development and so a lot of the features mentioned
in this document have either not been implemented at all yet or are still being worked on.

This is a hobby project worked on by a solo developer, so progress may be slow and gradual,
but I do aim to deliver on the ideas I have outlined.

## Language

The MGS Language (to be confirmed) is a low level, statically typed language with a focus
on ease of maintenance and high level of control for the developer.

This is achieved through explictness wherever possible, potentially adding a bit of time
to the initial development process, but making long term maintainance of projects much simpler.

We also have a focus on large support for testing at all levels, aiming to encourage better test
coverage throughout our projects by making the tests simple to write and maintain

### Language Features (Future)

Here are a few of the main features I am aiming to implement in the language (this is not an 
exhaustive list and is subject to change):

- Manual memory management and ability to define custom allocators
- No fixed build system
- Explicit error handling through error function annotations
- Everything is constant by default
- No null values everywhere - only when explicitly defined can null values be used
- Duck-typing interfaces 
- First class support for testing - table testing, benchmarking, fuzzing etc.

## Building the Project

As there are currently no releases of the compiler, the project must be built directly from source.

### Dependencies

This project currently uses LLVM as a compiler backend and uses clang to convert the object file into
a native executable.

As a result, to build the compiler from source both LLVM and Clang must be installed on the system.

### Building

Assuming all the dependencies are installed, simply run:
```
make
```
This will create the compiler executable as `./build/Compiler` which can then be run.

If you instead want to build and run the project in a single command, you can instead run:
```
make run
```

## Running the Tests

The compiler comes with a series of unit tests to ensure that compilation is working as expected.

These tests also serve as documentation for the language, since each test contains a snippet of mgs code.

For more information on these tests and how to run them, see the README in the `tests` directory.

## AI Usage

All the code in this project is handwritten. The main rationale behind this is purely that I enjoy
writing code and since this is a hobby project, I see no reason to get AI to write all the code for me.
I have no plans to make AI start writing any code within this project.
