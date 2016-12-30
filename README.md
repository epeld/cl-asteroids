# Hello #
  
This is an asteroids clone developmed using `cl-opengl`.

For now, just running `(ql:quickload :cl-game)` should be enough to get going.

# Current TODOs #

1) Add some rocks
2) Apply 'warping' - forcing ship to stay within screen bounds
3) Use error handler to 'save' main thread from crashing
4) Add visual effect (= "exhaust") when thrusting
5) Figure out units of length
6) Change ship color depending on state
7) Move vector math to separate package
8) Move Top Level API (game-loop) to separate package
9) Figure out how to draw pretty rocks
10) Figure out how to split rocks
11) Generalize ships, rocks and projectiles into game objects
12) Specialize update-object to do different things
