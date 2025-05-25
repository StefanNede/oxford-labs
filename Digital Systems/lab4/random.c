#include "microbian.h"
#include "hardware.h"
#include "lib.h"

#define BUF_SIZE 64

static int RNG_TASK; /* PID of RNG driver process */

/*
// add byte to buffer
void buf_put(char byte) {
    buffer[bufin] = byte;
    bufcnt++; 
    bufin = (bufin + 1) % BUF_SIZE;
}

char buf_get(void) {
    while (bufout == 0) pause();
    intr_disable();
    char byte = buffer[bufout];
    bufcnt--; bufout = (bufout+1) % BUF_SIZE;
    intr_enable();
    return byte;
}

void rng_handler(void) {
    if (RNG_VALRDY) {
        RNG_VALRDY = 0;
        if (bufcnt < BUF_SIZE) { buf_put(RNG_VALUE); };
    }
}*/

static void rng_task(int dummy) {
  int client;
  int result;
  message m;

  /*int bufcnt = 0;
  int bufin = 0;
  int bufout = 0;
  static char buffer[BUF_SIZE];*/

  // Initialise the RNG 
  RNG_STOP = 1; RNG_VALRDY = 0; RNG_INTENSET = 1; RNG_START = 1; 
  connect(RNG_IRQ);
  enable_irq(RNG_IRQ);

  while (1) {
    /*receive(ANY, &m);
    client = m.sender;

    switch (m.type) {
      case RNG_IRQ:
        if (RNG_VALRDY) {
          RNG_VALRDY = 0;
          if (bufcnt < BUF_SIZE) { 
            buffer[bufin] = RNG_VALUE;
            bufcnt++; 
            bufin = (bufin + 1) % BUF_SIZE;
          }
        }
        clear_pending(RNG_IRQ);
        enable_irq(RNG_IRQ);
        break;

      case REQUEST:
        result = buffer[bufout]%6;
        bufcnt--; bufout = (bufout+1) % BUF_SIZE;
        // reply to client
        send_int(client, REPLY, result);
        break;
    }*/

    receive(REQUEST, &m);
    client = m.sender;

    receive(INTERRUPT, NULL);
    result = RNG_VALUE;
    clear_pending(RNG_IRQ);
    enable_irq(RNG_IRQ);

    // reply to client
    send_int(client, REPLY, result);
  }
}

int roll_reading(void) {
  message m;
  m.type = REQUEST;
  sendrec(RNG_TASK, &m);

  return (m.int1%6)+1; // map from 0-5 to 1-6
}


/* rng_init -- start RNG driver */
void rng_init(void) {
  RNG_TASK = start("RNG", rng_task, 0, 256);
}

/*
int randint(int dummy) {
    while (bufcnt < 4) pause();
    char bytes[4];
    for (int i = 0; i < 4; i++) {
        bytes[i] = buf_get();
    }

    return ((bytes[0] << 24) | (bytes[1] << 16) | (bytes[2] << 8) | bytes[3]);
}

int roll(void) {
    const int minValue = 3;
    unsigned res;
    do {
        res = randint(1);
    } while (res > minValue);
    return (res%6) + 1;
}*/
