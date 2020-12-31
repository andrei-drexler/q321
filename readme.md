# Q321 - A tiny unofficial Quake III: Arena demake

## Prerequisites
Visual Studio 2017 (2019 should also work, but I haven't tested it)

## Building
For an uncompressed build using Visual Studio: open `demo.sln`, switch to the `Release|x86` configuration, and build/run the `demo` project.\
For a basic compressed build: run `build.bat` (with no arguments).\
To shrink the executable further (e.g. for release): run `build.bat` *again* with the argument `improve`. Warning: this could take a while!\
To reproduce an already-released build: place the corresponding repro file in the project root directory and run
```
build repro <repro_file_name>
```
