# Q321 - A tiny unofficial Quake III: Arena demake

https://user-images.githubusercontent.com/5121829/121275477-7fa45f00-c8d5-11eb-88f1-2abef2c1bdb5.mp4

[![Latest release](https://img.shields.io/badge/Latest%20release-63.6%20KiB-blue)](https://github.com/andrei-drexler/q321/releases/latest)
- 2 levels ([q3dm1](https://quake.fandom.com/wiki/Q3DM1:_Arena_Gate) and [q3dm17](https://quake.fandom.com/wiki/Q3DM17:_The_Longest_Yard)), ~100 textures/shaders and 27 models
- multithreaded lightmap baker (some details [here](https://twitter.com/andrei_drexler/status/1335215558774706178))
- OpenGL 3.3 renderer (using VBO+suballocation)
- raw mouse input and Quake 3-like physics
- (very) basic UI

# P.A.Q. (Preemptively Answered Questions)

## Q: Is this a virus? That's what my antivirus says when I try to download/run this demo.
A: No, this is not a virus. It is, however, a compressed executable, and the heuristics used by antivirus programs often flag these files as potentially malicious. If this is the case for you, there are a few possible solutions:
- add an exception in your antivirus settings (unsafe!)
- try the 'raw' (uncompressed) version of the executable (still unsafe, because you're still running a random program from the internet, but maybe slightly less so)
- build the code yourself

## Q: Is the demo secretly downloading assets from a server?
A: No.

## Q: Then what's that *download.js* script for?
A: Downloading assets from a server. More specifically, it's a helper script for `data/retrieve_models.bat`, which you can use to download model files from the [GtkRadiant Q3 gamepack](http://svn.icculus.org/gtkradiant-gamepacks/Q3Pack/trunk/install/baseq3/models/) (icculus.org), just in case you want to run the [model compiler](src/tools/model_compiler) and regenerate the [cooked_models.h](src/demo/cooked/cooked_models.h) file yourself.

## Q: Why use *crinkler* for a 64k, and not *kkrunchy* or *squishy*?
A: At the start of the project the goal wasn't '64k', it was 'as small as I can make it' - so [crinkler](https://github.com/runestubbe/Crinkler/) was the sensible choice, and it kept up surprisingly well as the amount of code and data grew. Even at 64k, startup (decompression) time is still reasonable and (when using aggressive settings) the compression ratio is similar to that of [kkrunchy_k7](http://www.farbrausch.de/~fg/kkrunchy/), but somewhat below [squishy](http://logicoma.io/squishy/)'s. It has one notable advantage, though, in that the executables it produces seem a tiny bit less likely to get flagged by antivirus programs, which, in my opinion, outweighs its most significant downside - compression time.

## Q: Why does the file size magically go down right before a release?
A: File sizes logged in [size_history.txt](size_history.txt) are from quick development builds, used to get a general idea of the size impact of each code change. Release builds, on the other hand, are generated with much more aggressive compression settings (e.g.`/ORDERTRIES:100000`), usually taking hours to produce a slightly smaller executable.

# Building [![Build status](https://ci.appveyor.com/api/projects/status/m9bhlscm8gqiev4e?svg=true)](https://ci.appveyor.com/project/andrei-drexler/q321)

## Prerequisites
- Visual Studio 2017 with C++ support.\
Note: Visual Studio 2019 should also work, but it hasn't been tested.

## Uncompressed builds
For an uncompressed build, open `demo.sln` in Visual Studio, select the `Release|Win32` configuration, and build/run the `demo` project.

## Compressed builds
Compressed builds are generated using `build.bat`, which creates a file named `demo.exe` in the `output/crinkler` folder and adds a new entry in [size_history.txt](size_history.txt).\
Simply running `build` (with no arguments) will produce a build with a lower compression ratio (in about 30 seconds).\
To shrink the exe further (e.g. for release): run `build` *again*, this time with the argument `improve`. Warning: this may take a while!\
To reproduce an already-released build: place the corresponding repro file in the project root directory and run
```
build repro <repro_file_name>
```
:warning: Warning: you may have to add an exception for the `output/crinkler` folder in your antivirus settings if the newly-created executable is flagged and removed immediately after being built.

# Credits

This is an *unofficial* recreation of Quake III: Arena (1999).\
QUAKE, id, id Software and related logos are registered trademarks or trademarks of id Software LLC in the U.S. and/or other countries.

## Many thanks to:
- id Software for the original Quake III: Arena game ([Steam](https://store.steampowered.com/app/2200/Quake_III_Arena/), [GOG](https://www.gog.com/game/quake_iii_gold), [free demo](https://games.softpedia.com/get/Games-Demo/Quake-3-Arena.shtml)), map samples, data specifications and [code](https://github.com/id-Software/Quake-III-Arena/)
- Aske Simon Christensen "Blueberry/Loonies" and Rune L. H. Stubbe "Mentor/TBC" for [Crinkler](http://crinkler.net/)
- Beautypi for [Shadertoy](https://www.shadertoy.com)
- Inigo Quilez for [articles and code](https://www.iquilezles.org/) covering noise, signed distance fields, and more
- Shadertoy user [Dave_Hoskins](https://www.shadertoy.com/user/Dave_Hoskins) for the [Hash without Sine](https://www.shadertoy.com/view/4djSRW) functions
- Shadertoy user [Shane](https://www.shadertoy.com/user/Shane) for the [Asymmetric Blocks](https://www.shadertoy.com/view/Ws3GRs) function
- Tom Forsyth for the [Linear-Speed Vertex Cache Optimisation](https://tomforsyth1000.github.io/papers/fast_vert_cache_opt.html) algorithm
- Fabian "ryg" Giesen for the [index buffer compression](https://fgiesen.wordpress.com/2013/12/17/index-compression-follow-up/) algorithm
- .theprodukkt/Farbrausch for [.kkrieger](https://www.pouet.net/prod.php?which=12036)
- Dr. Martin Roberts for the [R2 sequence](http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/)
