# MGS-Lang

This is the MGS Programming Language created by Michael Shepherd. This language is still in early development stages and so the majority of the language is likely to change.

## Building From Source

The only current way to use the compiler for this language is to build from source. We have aimed to make this process as simple as possible.

Please note that this language currently only supports Arm64 Linux compilation. Support for other operating systems will be added in the future.

Assuming you are using Arm64 Linux, there are a couple of prerequisties that need to be installed on your system before you can build the compiler. There are:

- Make - This is currently used as the build system for the language
- Ocaml and dune - The compiler is written in Ocaml and we use dune as the package manager

With both of these installed, from the root directory of the project you should just be able to run:

```
make
```

This will handle all the compilation of the code and generate the executable which is to used as the compiler. Assuming this all worked correctly,
you should have a `build` folder which contains the executable called `main`.

NOTE: Even on Linux systems this executable will have the extension `.exe` - This is due to the fact that the dune build system adds this extension
to all executables, even on Linux.

Once built, you can then use compiler to compile a `.mgs` source file by using the following command from the root of the project:

```
./build/main.exe -f <source_file_path>
```

The `-f` argument is what specifies the source file that you would like to compile. Currently MGS Lang only supports compilation of a single file,
but support for multiple files will be coming shortly.

This command will generate another executable in the `build` directory, which will have the same name as the input file you provided. This can be run
the same as any executable file.

## Examples

There are a series of examples provided in the `examples` directory of this repo. These are used to highlight some of the core features of the
language and can be used as a form of documention on some of the language features.

