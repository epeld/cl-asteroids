#include <stdio.h>

#include "ui.h"
#include "events.h"


static void reactor_loop() {
  foo_ui_loop();
}

int main(int argc, char* argv[]) {
  foo_ui_init();
  reactor_loop();
  foo_ui_uninit();

  return 0;
}
