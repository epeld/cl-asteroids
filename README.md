# Hello #
  
This is an asteroids clone developmed using `cl-opengl`.

For now, just running `(ql:quickload :cl-game)` should be enough to get going.

# Current TODOs #

+ Add some rocks
+ Specialize update-object to do different things
+ Apply 'warping' - forcing ship to stay within screen bounds
+ Figure out how to split rocks
+ Add visual effect (= "exhaust") when thrusting
+ Figure out units of length
+ Change ship color depending on state
+ Move vector math to separate package
+ Move Top Level API (game-loop) to separate package
