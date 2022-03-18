# CHIP-8

https://github.com/soupi/chip-8

<br><br>

cmd.exe as Administrator  
<pre>
pacman -Sy mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2 mingw-w64-x86_64-SDL2_ttf
</pre>

<br><br>

<pre>
stack build --extra-lib-dirs=src
</pre>

<br><br>

--- copy ---  
OpenAL32.dll  


<pre>
stack exec -- chip8 .\roms\pong.chip8
</pre>
