#include "microbian.h"
#include "hardware.h"
#include "lib.h"

const unsigned one[] = 
    IMAGE(0,0,0,0,0,
          0,0,0,0,0,
          0,0,1,0,0,
          0,0,0,0,0,
          0,0,0,0,0);

const unsigned two[] = 
    IMAGE(0,0,0,0,0,
          0,1,0,0,0,
          0,0,0,0,0,
          0,0,0,1,0,
          0,0,0,0,0);

const unsigned three[] = 
    IMAGE(0,0,0,0,0,
          0,1,0,0,0,
          0,0,1,0,0,
          0,0,0,1,0,
          0,0,0,0,0);

const unsigned four[] = 
    IMAGE(0,0,0,0,0,
          0,1,0,1,0,
          0,0,0,0,0,
          0,1,0,1,0,
          0,0,0,0,0);

const unsigned five[] = 
    IMAGE(0,0,0,0,0,
          0,1,0,1,0,
          0,0,1,0,0,
          0,1,0,1,0,
          0,0,0,0,0);

const unsigned six[] = 
    IMAGE(0,0,0,0,0,
          0,1,0,1,0,
          0,1,0,1,0,
          0,1,0,1,0,
          0,0,0,0,0);

void sender_task(int dummy) {
  GPIO_PINCNF[BUTTON_A] = 0;
  GPIO_PINCNF[BUTTON_B] = 0;

  while (1) {
    if (GET_BIT(GPIO_IN, BUTTON_A) == 0 || GET_BIT(GPIO_IN, BUTTON_B) == 0) {
      int value_rolled = roll_reading(); // from random.c
      switch(value_rolled) {
        case 1:
          printf("one rolled\n");
          display_show(one);
          break;
        case 2:
          printf("two rolled\n");
          display_show(two);
          break;
        case 3:
          printf("three rolled\n");
          display_show(three);
          break;
        case 4:
          printf("four rolled\n");
          display_show(four);
          break;
        case 5:
          printf("five rolled\n");
          display_show(five);
          break;
        case 6:
          printf("six rolled\n");
          display_show(six);
          break;
        default:
          printf("%d\n", &value_rolled);
          printf("error occurred\n");
      }
    }

    timer_delay(200);
  }
}

void init(void) {
  timer_init(); // to add a little bit of delay between accepting button clicks to prevent spamming
  serial_init(); // to print
  rng_init(); // to get random number
  display_init(); // to display to microbit
  start("Roll", sender_task, 0, STACK);
}

