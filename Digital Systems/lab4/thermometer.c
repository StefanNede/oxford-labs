#include "microbian.h"
#include "hardware.h"

static int TEMP_TASK;
// use TEMP_VALUE to access it 

static void temp_task(int dummy) {
  int client; 
  int result;
  message m;

  TEMP_STOP = 1; TEMP_DATARDY = 0; TEMP_INTENSET = 1; TEMP_START = 1;
  connect(TEMP_IRQ);
  enable_irq(TEMP_IRQ);
  while (1) {
    TEMP_DATARDY = 0; TEMP_START = 1; // start the process again
    receive(REQUEST, &m);
    client = m.sender;

    receive(INTERRUPT, NULL);
    result = TEMP_VALUE;
    clear_pending(TEMP_IRQ);
    enable_irq(TEMP_IRQ);

    // reply to client
    send_int(client, REPLY, result);
  }
}

int thermometer_reading(void) {
  message m;
  m.type = REQUEST;
  sendrec(TEMP_TASK, &m);

  return m.int1;
}

void temp_init(void) {
  TEMP_TASK = start("thermometer", temp_task, 0, STACK);
}
