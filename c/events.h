#pragma once
#define _FOO_EVENTS

#define MAX_EVENTS_PER_CYCLE 512


struct foo_s_keyboard_event {
  char key;
  int x;
  int y;
};

typedef struct foo_s_keyboard_event foo_keyboard_event;

// TODO later on we will typedef input_event to be a union
typedef foo_keyboard_event foo_input_event;

/* This module is about maintaining a list of input events. */
/* A single buffer is used to allocate memory for events (to avoid mallocing). */
/* It is expected that this buffer is cleared regularly using foo_clear_events() */
/* once the events have been processed */


// Allocate a new event.
// Return NULL if max event count has been reached already
foo_input_event* foo_new_event(void);

// Get a pointer to the event list. Use foo_get_event_count to determine its length
foo_input_event* foo_get_events();

// Get the current length of the event list
int foo_get_event_count();

// Clear all events in the list
void foo_clear_events();


/* Some event helpers */

foo_keyboard_event* foo_make_keyboard_event(foo_input_event* ev);
