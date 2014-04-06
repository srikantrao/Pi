#include "msp430f5438a.h"

int count_0=0, count_1=0, i=0;
int main(void) {

    WDTCTL = WDTPW | WDTHOLD;	// Stop watchdog timer
    //Configure switch S1 and S2
    	P2SEL &= 0x3F ;//I/O Function 0011 1111=0x3F
    	P2DIR &= 0x3F; //P2.6 as input
    	P2REN |= 0xC0; //Enable pull resistor
    	P2OUT |= 0xC0; //Enable Pull-Up resistor
    	P2IE |=  0xC0;   // Enable Port 2 interrupt registers

    //Configure LED 1
    	P1SEL &= 0xFC; //I/O Function 11111110 = 0xFE
    	P1DIR |= 0x03; //P1.0 as output
    	P1DS |= 0x03; //Enable Full drive strength
    	P1OUT &= 0xFC;

    	__enable_interrupt();
        __bis_SR_register(GIE);

        TA0CCTL0 = CCIE; // Enabling Interrupts for Timer_0
        TA1CCTL0 = CCIE;  // Enabling Interrupts for Timer_0
        TA0EX0 = TAIDEX_7; // slow down ACLK
        TA1EX0 = TAIDEX_7;

    	TA0CTL = TASSEL__ACLK + MC__UP +  ID__8 + TACLR;
    	TA1CTL = TASSEL__ACLK + MC__UP +  ID__8 + TACLR;
    	TA0CCR0 = 1280; // 5 s
    	TA1CCR0 = 1280; // 5 s

    	while(1) {}  // need to figure out how to fix this
}

#pragma vector=TIMER0_A0_VECTOR
__interrupt void TIMERA0_ISR(void) {
 P1OUT ^= BIT0;
}


#pragma vector=TIMER1_A0_VECTOR
__interrupt void TIMERA1_ISR(void) {
  P1OUT ^= BIT1;
}

#pragma vector=PORT2_VECTOR
__interrupt void PORT2_ISR(void)  {

	for(i=80000;i>0;i--); // makeshift debouncer

	switch (P2IV){
		case (P2IV_P2IFG6) :
		count_0 ++;
		count_1--;
			if (((count_0 >= 0) && (count_0<5))&& count_1<5) {	// This if condition is needed for it to blink at 5s on the 6th press.
						TA1CCR0 += 256;
						TA0CCR0 -= 256;
						TA0R = 0x0000; // reset counter to synchronize
						TA1R = 0x0000;

					}
			else if(count_0==5){
				P1OUT &= 0x0000; // disable LEDs
				TA0CCTL0 &= 0xFFEF;   // disable interrupts
				TA1CCTL0 &= 0xFFEF;   // disable interrupts
				TA0R = 0x0000;        // reset counter to synchronize
				TA1R = 0x0000;
		}
		else if((count_0>5)||(count_1>=4))  {
			count_0 =0;
			count_1=0;
			TA0CCTL0 = CCIE;  // enable interrupt
			TA1CCTL0 = CCIE;  // enable interrupt
			TA0CCR0 = 1280; // 5 s
			TA1CCR0 = 1280; // 5 s
			TA0CTL = TASSEL__ACLK + MC__UP +  ID__8 + TACLR;   // TACLR again
			TA1CTL = TASSEL__ACLK + MC__UP +  ID__8 + TACLR;
			TA0R = 0x0000;        // reset counter to synchronize
			TA1R = 0x0000;
		}

		break;

	case (P2IV_P2IFG7) :
		count_1 ++;
	    count_0--;
	if (((count_1 >= 0) && (count_1<5))&& count_0<5)
	{	// This if condition is needed for it to blink at 5s on the 6th press.
			  		  TA0CCR0 += 256;
			  		  TA1CCR0 -= 256;
			  		TA0R = 0x0000;        // reset counter to synchronize
			  		TA1R = 0x0000;

			}
	else if(count_1==5){
		P1OUT &= 0x0000;
		TA0CCTL0 &= 0xFFEF;
		TA1CCTL0 &= 0xFFEF;
		TA0R = 0x0000;        // reset counter to synchronize
		TA1R = 0x0000;
	}
	 else if ((count_1>5)||(count_0>=4)) {
		 count_1=0;
		 count_0=0;
		  TA0CCTL0 = CCIE;
		  TA1CCTL0 = CCIE;
		  TA0CCR0 = 1280; // 5 s
		  TA1CCR0 = 1280; // 5 s
		  TA0CTL = TASSEL__ACLK + MC__UP +  ID__8 + TACLR;
		  TA1CTL = TASSEL__ACLK + MC__UP +  ID__8 + TACLR;
		  TA0R = 0x0000;        // reset counter to synchronize
		  TA1R = 0x0000;
	  }

	break;

	}
}


