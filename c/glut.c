
#include <glut.h>
#include <GL/freeglut.h>

#include "ui.h"
#include "events.h"



void reshape(GLint width, GLint height) {
  // TODO
}

void display(void) {
  // Clear frame buffer and depth buffer
   glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
   // Set up viewing transformation, looking down -Z axis
   glLoadIdentity();
   /* gluLookAt(0, 0, -g_fViewDistance, 0, 0, -1, 0, 1, 0); */
   /* // Set up the stationary light */
   /* glLightfv(GL_LIGHT0, GL_POSITION, g_lightPos); */
   /* // Render the scene */
   /* RenderObjects(); */
   // Make sure changes appear onscreen
   glutSwapBuffers();
}

void keyboard(unsigned char key, int x, int y) {
  foo_input_event* ev = foo_new_event();
  foo_keyboard_event* kbd = foo_make_keyboard_event(ev);

  kbd.key = key;
  kbd.x = x;
  kbd.y = y;
}


/* void mouseButton(int button, int state, int x, int y) { */
/*   if (button == GLUT_LEFT_BUTTON) { */
/*     // TODO */
/*   } */
/* } */

/* void mouseMotion(int x, int y) { */
/*   // TODO */
/* } */

int foo_ui_init(int argc, char* argv[]) {
  glutInit (&argc, argv);
  glutInitWindowSize (640, 480);
  glutInitDisplayMode ( GLUT_RGB | GLUT_DOUBLE | GLUT_DEPTH);
  glutCreateWindow ("Foo");
  
  // Register callbacks:
  glutDisplayFunc (display);
  glutReshapeFunc (reshape);
  glutKeyboardFunc (keyboard);
  /* glutMouseFunc (mouseButton); */
  /* glutMotionFunc (mouseMotion); */

  // Question: What happens when WE are in control of main loop, as opposed to GLUT?
  glutSetOption(GLUT_ACTION_ON_WINDOW_CLOSE, GLUT_ACTION_GLUTMAINLOOP_RETURNS);
  
  return 0;
}

void foo_ui_uninit() {
  // TODO
}

int foo_ui_step() {
  glutMainLoopEvent();
}