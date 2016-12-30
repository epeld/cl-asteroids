#include <stddef.h>

#include "events.h"


static foo_input_event event_buffer[MAX_EVENTS_PER_CYCLE];
static int current_event_count = 0;

foo_input_event* foo_new_event(void) {
  if(current_event_count == MAX_EVENTS_PER_CYCLE - 1) {
    return NULL;
  }
  
  foo_input_event* ev = event_buffer + current_event_count;
  current_event_count++;

  return ev;
}


int foo_get_event_count() {
  return current_event_count;
}

foo_input_event* foo_get_events() {
  return event_buffer;
}


foo_keyboard_event* foo_make_keyboard_event(foo_input_event* ev) {
  // TODO modify later when foo_input_event != foo_keyboard_event
  return ev;
}
