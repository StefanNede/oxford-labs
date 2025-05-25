// lab4-microbian/ex-race.c

// include rng_init() leads to my numbers for r being slightly different between runs as expected

#include "microbian.h"
#include "hardware.h"
#include "lib.h"

static volatile int r = 0;

void proc1(int n) {
    for (int i = 0; i < 10; i++)
        printf("r = %d\n", r);
}

void proc2(int n) {
    while (r < 100000)
        r++;

    for (int i = 0; i < 2000000; i++) {
        nop(); nop(); nop();
    }

    dump();
}

void init(void) {
    serial_init();
    //rng_init();
    start("Proc1", proc1, 0, STACK);
    start("Proc2", proc2, 0, STACK);

    /*
       the process dump will get printed first because in proc2() there are no printf calls until dump()
       
       swapping the 2 start calls leads to the process dump being outputted first 
       and just r = 100000 being outputted multiple times 
       just r = 100000 is being outputted because in the intial run of proc2() r gets incremented to 
       100000 and then the printing happnes in proc1
    */
}
