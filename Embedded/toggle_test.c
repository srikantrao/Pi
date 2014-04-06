#include <msp430f5438a.h>

int count = 0;

int main(void)
{
  WDTCTL = WDTPW + WDTHOLD;                 // Stop WDT

  //Configure LED
  	P1SEL &= 0xFC;  //-- changing it to I/O function
  	P1DIR |= 0x03;
  	P1DS |= 0x03;    //   -- Full drive strength                            // P1.0 output

  	//Configure switch S1 & S2
  	P2SEL &= 0x3F; //I/O Function  -- changing it to I/O Function from ACLK   0011 1111 --> 3F
  	P2DIR &= 0x3F;   //P2.6 as input -- changing P2.6 to 0
  	P2REN |= 0xC0; //Enable pull resistor
  	P2OUT |= 0xC0; //Enable Pull-Up resistor

  	//__enable_interrupt();
  	//__bis_SR_register(LPM3_bits + GIE);       // Enter LPM3, enable interrupts
  	__bis_SR_register(GIE);       // Enter LPM3, enable interrupts

  	// Set Timer
  	TA0CCTL0 = CCIE;
  	TA0CCTL1 = CCIE;                          // CCR0 interrupt enabled

  	TA0CCR0 = 2048;
  	TA0CCR1 = 2048;

  TA0CTL = TASSEL_1 + MC_2 + TACLR + ID_3;   // ACLK, contmode, clear TAR
  while(1){

  if (!(P2IN & 0x40) && (P2IN & 0x80)){
	  count = 1;
  }
  else if ((P2IN & 0x40) && !(P2IN & 0x80)){
	 count = 2;
  }
  }
}

// Timer1 A0 interrupt service routine
#pragma vector=TIMER0_A0_VECTOR
__interrupt void TIMER0_A0_ISR(void)
{
	P1OUT ^= BIT0; // Toggle P1.0
	switch(count){
	case 0:	TA0CCR0 +=2048;break;
	case 1: TA0CCR0 +=1024;break;
	case 2: TA0CCR0 +=2048;break;
	}


}

#pragma vector=TIMER0_A1_VECTOR
__interrupt void TIMER0_A1_ISR(void)
{

	 P1OUT ^= BIT1;
		switch(count){
	case 0:	TA0CCR1+=2048;break;
	case 1: TA0CCR1+=2048;break;
	case 2: TA0CCR1+=1024;break;
			;
		}
		TA0IV = 0x0000;
}


