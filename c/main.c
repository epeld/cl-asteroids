#include <stdio.h>

#include <zmq.h>
#include <czmq.h>

#include "events.h"


// Processes all buffered input events,
// Clearing the event buffer for new events
static void publish_input_events(zsock_t* pub) {
  
  // For now, batch all events up into a single message
  zsock_send(pub,
             "b",
             foo_get_events(),
             sizeof(foo_input_event) * foo_get_event_count());

  foo_clear_events();
}


static void reactor_loop(zsock_t* pub) {
  // Idea:
  // (process requests)
  // Do one iteration of glut event loop.
  // collect events, publish on socket
  // if exit event occured, stop execution, uninit
  // else repeat.

  foo_ui_init();

  while(true) {
    // Process requests..
    // TODO

    foo_ui_step();
    publish_input_events(pub);
    // TODO sleep?
  }

  foo_ui_uninit();
}

static zsock_t* make_pub_sock() {
  /* Setup PUB-socket */
  const char * pubaddr = "tcp://*";

  zsock_t *pub = zsock_new_pub(pubaddr);
  pubaddr = zsock_endpoint(pub);
  
  printf("PUB: %s\n", pubaddr);
  
  return pub;
}

int main(int argc, char* argv[]) {
  zsock_t* pub = make_pub_sock();

  reactor_loop(pub);

  zsock_destroy(&pub);

  return 0;
}
