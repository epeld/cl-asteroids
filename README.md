# Hello #
  
This was originally an attempt to re-create asteroids in Common Lisp and GLUT.
  
That might still be the end result but instead the current goal is to create a bunch of more or less reusable "game development micro services".

The first one up is Foobar the rendering service.

## Foobar ##

Foobar is a GLUT Window functioning as a 3D Rendering Service.

It will publish input events from the user.

It will accept scene descriptions to render.

### Scene Descriptions ###

Basically a list of objects, their positions, rotations etc to be rendered.


# Current TODOs #

1. Work out how to cancel old glut timers so that we don't get spurious redraws - stabilize frame rate!
2. Change ship color depending on state
3. Allow ship thrusting
4. Add some rocks
