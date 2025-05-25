#include "microbian.c"
//#include "hardware.h"
//#include "lib.h"

void temperature_task(int dummy) {
  while (1) {
    int temp_reading = thermometer_reading();
    printf("Temp: %d\n", temp_reading);
    timer_delay(1000);
  }
}

void init(void) {
  serial_init();
  temp_init();
  timer_init();
  start("Temp reading", temperature_task, 0, STACK);
}
