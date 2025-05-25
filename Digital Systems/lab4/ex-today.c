// lab4-phos/today.c

#include "microbian.h"
#include "lib.h"
//#include <string.h>

#define PERMISSION 42
#define FINISHED 43

static int INTERVIEWER;
static int MAY;
static int FARAGE;

void put_string(char *s) {
    for (char *p = s; *p != '\0'; p++)
        serial_putc(*p);
}

char *slogan[] = {
    "no deal is better than a bad deal\n",
    "BREXIT MEANS BREXIT!\n"
};

void speaker(int n) {
  message m;
  while (1) {
    receive(PERMISSION, &m);
    put_string(slogan[n]);
    send_msg(INTERVIEWER, FINISHED);
  }
}

void interviewer(int n) {
  message m;
  while (1) {
    send_msg(MAY, PERMISSION);
    receive(FINISHED, &m);

    send_msg(FARAGE, PERMISSION);
    receive(FINISHED, &m);
  }
}


void init(void) {
    serial_init();
    MAY = start("May", speaker, 0, STACK);
    FARAGE = start("Farage", speaker, 1, STACK);
    INTERVIEWER = start("Interviewer", interviewer, 0, STACK);
}

/* Initial Solution:
void interviewer(int n) {
    while (1) {
        put_string(slogan[n]);
        put_string(slogan[n+1]);
    }

}*/

