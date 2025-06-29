/* @DIR x3000@/valentine.c */

/* the display process should have a higher priority than the primes process because
   otherwise the heart animation won't be smooth and will depend on the speed of generating the 
   next prime -> the animation becomes much more 'flickery' */

#include "hardware.h"
#include "microbian.h"
#include "lib.h"

/* heart -- filled-in heart image */
const unsigned heart[] =
    IMAGE(0,1,0,1,0,
          1,1,1,1,1,
          1,1,1,1,1,
          0,1,1,1,0,
          0,0,1,0,0);

/* small -- small heart image */
const unsigned small[] =
    IMAGE(0,0,0,0,0,
          0,1,0,1,0,
          0,1,1,1,0,
          0,0,1,0,0,
          0,0,0,0,0);

/* show -- display three rows of a picture n times */
void show(const unsigned *img, int n)
{
    while (n-- > 0) {
        /* Takes 15msec per iteration */
        for (int p = 0; p < 3; p++) {
            GPIO_OUT = img[p];
            timer_delay(5);
        }
    }
}

/* heart_task -- show beating heart */
void heart_task(int n)
{
    GPIO_DIRSET = 0xfff0;
    SET_FIELD(GPIO_PINCNF[ROW1], GPIO_PINCNF_DRIVE, GPIO_DRIVE_S0H1);
    SET_FIELD(GPIO_PINCNF[ROW2], GPIO_PINCNF_DRIVE, GPIO_DRIVE_S0H1);
    SET_FIELD(GPIO_PINCNF[ROW3], GPIO_PINCNF_DRIVE, GPIO_DRIVE_S0H1);

    // priority(P_HIGH);
    // reinstating the priority via the following line
    priority(P_HIGH);

    while (1) {
        show(heart, 70);
        show(small, 10);
        show(heart, 10);
        show(small, 10);
    }
}

/* This is a bit lighter than the earlier example, because we use GCC's
builtin modulo operation, rather than repeated subtraction.  That
leaves some CPU time over to look after the blinking lights. */

/* prime -- test for primality */
int prime(int n)
{
    for (int k = 2; k * k <= n; k++) {
        if (n % k == 0)
            return 0;
    }

    return 1;
}

/* prime_task -- print primes on the serial port */
void prime_task(int arg)
{
    // priority(P_HIGH); // added this line then removed it
    int n = 2, count = 0; 
    // changed n to 1000000
    n = 1000000;

    while (1) {
        if (prime(n)) {
            count++;
            printf("prime(%d) = %d\n", count, n);
        }
        n++;
    }
}

void init(void)
{
    serial_init();
    timer_init();
    start("Heart", heart_task, 0, STACK);
    start("Prime", prime_task, 0, STACK);
}
