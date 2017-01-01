# Hello #

![Screenshot](screenie.png "Screenshot")

  
This is an asteroids clone developed using `cl-opengl`, complete with particles and everything!

For now, just running `(ql:quickload :cl-game)` should be enough to get going.

Then, to start the game, go to asteroids.lisp and run `(game-loop)`.

# Current TODOs #
+ Create a Menu System
+ Score Keeping

# Less Important TODOs #
+ Change ship color depending on state
+ Move vector math to separate package
+ Move Top Level API (game-loop) to separate package
+ Figure out units of length (to allow easier scaling etc..)
