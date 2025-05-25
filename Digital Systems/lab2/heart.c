/* @DIR x2000@/heart.c */

#include "hardware.h"

/* delay -- pause for n microseconds */
void delay(unsigned n)
{
    unsigned t = 2 * n;
    while (t > 0)
    {
        /* 500nsec per iteration at 16MHz */
        nop();
        nop();
        nop();
        t--;
    }
}

/* heart -- GPIO values for heart image */
//const unsigned heart[] = {
//0x28f0, 0x5e00, 0x8060};

const unsigned heart[] =
    IMAGE(0, 1, 0, 1, 0,
          1, 1, 1, 1, 1,
          1, 1, 1, 1, 1,
          0, 1, 1, 1, 0,
          0, 0, 1, 0, 0);

/* small -- GPIO values for small heart */
const unsigned small[] = {
    0x2df0, 0x5fb0, 0x8af0};

/* hollow */
const unsigned hollow[] =
    IMAGE(0, 1, 0, 1, 0,
          1, 0, 1, 0, 1,
          1, 0, 0, 0, 1,
          0, 1, 0, 1, 0,
          0, 0, 1, 0, 0);

// question mark
const unsigned question[] =
    IMAGE(0, 1, 1, 1, 0,
          0, 0, 0, 1, 0,
          0, 0, 1, 0, 0,
          0, 0, 0, 0, 0,
          0, 0, 1, 0, 0);

#define JIFFY 5000    /* Delay in microsecs */
int lastPressed = -1; // 0 - A, 1 - B

/* show -- display three rows of a picture n times */
void show(const unsigned img[], int n)
{
    while (n-- > 0)
    {
        /* Takes 15msec per iteration */
        for (int p = 0; p < 3; p++)
        {
            GPIO_OUT = img[p];
            delay(JIFFY);
        }
    }
}

/* pressed -- test if a button is pressed */
int pressed(int button)
{
    return (GPIO_IN & BIT(button)) == 0;
}

// procedure for the regular heart beat
void heartBeat()
{
    show(heart, 70);
    show(small, 10);
    show(heart, 10);
    show(small, 10);
}

// procedure for hollow -> full heart beat
void hollowBeat()
{
    show(heart, 70);
    show(hollow, 10);
    show(heart, 10);
    show(hollow, 10);
}

/* init -- main program */
void init(void)
{
    GPIO_DIR = 0xfff0;
    GPIO_PINCNF[BUTTON_A] = 0;
    GPIO_PINCNF[BUTTON_B] = 0;

    /* Set row pins to high-drive mode to increase brightness */
    SET_FIELD(GPIO_PINCNF[ROW1], GPIO_PINCNF_DRIVE, GPIO_DRIVE_S0H1);
    SET_FIELD(GPIO_PINCNF[ROW2], GPIO_PINCNF_DRIVE, GPIO_DRIVE_S0H1);
    SET_FIELD(GPIO_PINCNF[ROW3], GPIO_PINCNF_DRIVE, GPIO_DRIVE_S0H1);

    while (1)
    {
        if (pressed(BUTTON_A))
        {
            lastPressed = 0;
        }
        else if (pressed(BUTTON_B))
        {
            lastPressed = 1;
        }
        else
        {
            lastPressed = -1;
        }

        if (lastPressed == 0)
        {
            heartBeat();
            heartBeat();
        }
        else if (lastPressed == 1)
        {
            hollowBeat();
            hollowBeat();
        }
        else
        {
            show(question, 10);
        }

        /*
        show(heart, 70);
        if (pressed(BUTTON_A) || lastPressed == 0)
        {
            lastPressed = 0;
            show(small, 10);
        }
        if (pressed(BUTTON_B) || lastPressed == 1)
        {
            lastPressed = 1;
            show(hollow, 10);
        }

        show(heart, 10);

        if (pressed(BUTTON_A) || lastPressed == 0)
        {
            lastPressed = 0;
            show(small, 10);
        }
        if (pressed(BUTTON_B) || lastPressed == 1)
        {
            lastPressed = 1;
            show(hollow, 10);
        }*/
    }
}
