
  .g8""8q.                           
.dP'    `YM.                    __,  
dM'      `MM  pd""b.   pd*"*b. `7MM  
MM        MM (O)  `8b (O)   j8   MM  
MM.      ,MP      ,89     ,;j9   MM  
`Mb.    ,dP'    ""Yb.  ,-='      MM  
  `"bmmd"'         88 Ammmmmmm .JMML.
      MMb    (O)  .M'                
       `bood' bmmmd'                 

An unofficial Quake III: Arena demake in 65,111 bytes*
Also, a work in progress.
Andrei Drexler, 2021/03/21


// REQUIREMENTS //

- Windows
- OpenGL 3.3 compatible GPU & drivers
- 1 GB RAM
- mouse and keyboard
- nostalgia


// CONTROLS //

WASD, up/down = move
left/right    = turn left/right
Del/PageDown  = look down/up
End           = center view
Space         = jump
Mouse2        = zoom
Backspace     = respawn
PrintScreen   = save screenshot (screenshots/screenshot_<N>.tga)
L             = toggle lightmap
\             = toggle noclip
Alt-F4        = ragequit


// IS THIS A VIRUS? //

It is not. However, it *is* a packed executable, which will likely get flagged by overzealous
antivirus programs. This is why the file is compressed with Crinkler (meant for 4K intros),
and not a more suitable 64K packer (e.g. squishy): Crinkler-generated EXEs seemed less likely
to get flagged by Windows Defender.


// CREDITS //

This is an *unofficial* recreation of Quake III: Arena (1999).
QUAKE, id, id Software and related logos are registered trademarks or trademarks
of id Software LLC in the U.S. and/or other countries.

Many thanks to:

- id Software for the original Quake III game, map samples, data specifications and code
  https://www.idsoftware.com/
  https://github.com/id-Software/

- Aske Simon Christensen "Blueberry/Loonies" and Rune L. H. Stubbe "Mentor/TBC" for Crinkler
  http://crinkler.net/

- Beautypi for Shadertoy
  https://www.shadertoy.com

- Inigo Quilez for articles and code covering noise, signed distance fields, and more
  https://www.iquilezles.org/

- ShaderToy user Dave_Hoskins for the "Hash without Sine" functions
  https://www.shadertoy.com/view/4djSRW

- ShaderToy user Shane for the "Asymmetric Blocks" function
  https://www.shadertoy.com/view/Ws3GRs

- Tom Forsyth for the "Linear-Speed Vertex Cache Optimisation" algorithm
  https://tomforsyth1000.github.io/papers/fast_vert_cache_opt.html

- Fabian "ryg" Giesen for the index buffer compression algorithm
  https://fgiesen.wordpress.com/2013/12/17/index-compression-follow-up/

- .theprodukkt/Farbrausch for .kkrieger
  http://www.farbrausch.de/prod.py?which=114
  https://www.pouet.net/prod.php?which=12036

- Dr. Martin Roberts for the R2 sequence
  http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/

---
* For now. Might balloon up later.
