;
; a3part-B.asm
;
;
;
; Student name: Ammar Patel
; Student ID: V01013179 
; Completed: 26/11/2023
;
; **********************************
; Code provided for Assignment #3
;
; Author: Mike Zastre (2022-Nov-05)
;
; This skeleton of an assembly-language program is provided to help you 
; begin with the programming tasks for A#3. As with A#2 and A#1, there are
; "DO NOT TOUCH" sections. You are *not* to modify the lines within these
; sections. The only exceptions are for specific changes announced on
; Brightspace or in written permission from the course instruction.
; *** Unapproved changes could result in incorrect code execution
; during assignment evaluation, along with an assignment grade of zero. ***
;


; =============================================
; ==== BEGINNING OF "DO NOT TOUCH" SECTION ====
; =============================================
;
; In this "DO NOT TOUCH" section are:
; 
; (1) assembler direction setting up the interrupt-vector table
;
; (2) "includes" for the LCD display
;
; (3) some definitions of constants that may be used later in
;     the program
;
; (4) code for initial setup of the Analog-to-Digital Converter
;     (in the same manner in which it was set up for Lab #4)
;
; (5) Code for setting up three timers (timers 1, 3, and 4).
;
; After all this initial code, your own solutions's code may start
;

.cseg
.org 0
	jmp reset

; Actual .org details for this an other interrupt vectors can be
; obtained from main ATmega2560 data sheet
;
.org 0x22
	jmp timer1

; This included for completeness. Because timer3 is used to
; drive updates of the LCD display, and because LCD routines
; *cannot* be called from within an interrupt handler, we
; will need to use a polling loop for timer3.
;
; .org 0x40
;	jmp timer3

.org 0x54
	jmp timer4

.include "m2560def.inc"
.include "lcd.asm"

.cseg
#define CLOCK 16.0e6
#define DELAY1 0.01
#define DELAY3 0.1
#define DELAY4 0.5

#define BUTTON_RIGHT_MASK 0b00000001	
#define BUTTON_UP_MASK    0b00000010
#define BUTTON_DOWN_MASK  0b00000100
#define BUTTON_LEFT_MASK  0b00001000

#define BUTTON_RIGHT_ADC  0x032
#define BUTTON_UP_ADC     0x0b0   ; was 0x0c3
#define BUTTON_DOWN_ADC   0x160   ; was 0x17c
#define BUTTON_LEFT_ADC   0x22b
#define BUTTON_SELECT_ADC 0x316

.equ PRESCALE_DIV=1024   ; w.r.t. clock, CS[2:0] = 0b101

; TIMER1 is a 16-bit timer. If the Output Compare value is
; larger than what can be stored in 16 bits, then either
; the PRESCALE needs to be larger, or the DELAY has to be
; shorter, or both.
.equ TOP1=int(0.5+(CLOCK/PRESCALE_DIV*DELAY1))
.if TOP1>65535
.error "TOP1 is out of range"
.endif

; TIMER3 is a 16-bit timer. If the Output Compare value is
; larger than what can be stored in 16 bits, then either
; the PRESCALE needs to be larger, or the DELAY has to be
; shorter, or both.
.equ TOP3=int(0.5+(CLOCK/PRESCALE_DIV*DELAY3))
.if TOP3>65535
.error "TOP3 is out of range"
.endif

; TIMER4 is a 16-bit timer. If the Output Compare value is
; larger than what can be stored in 16 bits, then either
; the PRESCALE needs to be larger, or the DELAY has to be
; shorter, or both.
.equ TOP4=int(0.5+(CLOCK/PRESCALE_DIV*DELAY4))
.if TOP4>65535
.error "TOP4 is out of range"
.endif

reset:
; ***************************************************
; **** BEGINNING OF FIRST "STUDENT CODE" SECTION ****
; ***************************************************

; Anything that needs initialization before interrupts
; start must be placed here.

	ldi r17, 0xFF ; Sets the stack pointer
	out SPL, r17
	ldi r17, 0x21
	out SPH, r17

	ldi r17, ' ' ; Initializes the character space as a blank in order to 'reset' after building
	sts TOP_LINE_CONTENT, r17
	clr r17
	sts CURRENT_CHARSET_INDEX, r17 ;

	.def DATAH=r25  ;DATAH:DATAL  store 10 bits data from ADC
	.def DATAL=r24
	.def BOUNDARY_H=r1  ;hold high byte value of the threshold for button
	.def BOUNDARY_L=r0  ;contains the low byte of the threshold for button

; ***************************************************
; ******* END OF FIRST "STUDENT CODE" SECTION *******
; ***************************************************

; =============================================
; ====  START OF "DO NOT TOUCH" SECTION    ====
; =============================================

	; initialize the ADC converter (which is needed
	; to read buttons on shield). Note that we'll
	; use the interrupt handler for timer 1 to
	; read the buttons (i.e., every 10 ms)
	;
	ldi temp, (1 << ADEN) | (1 << ADPS2) | (1 << ADPS1) | (1 << ADPS0)
	sts ADCSRA, temp
	ldi temp, (1 << REFS0)
	sts ADMUX, r16

	; Timer 1 is for sampling the buttons at 10 ms intervals.
	; We will use an interrupt handler for this timer.
	ldi r17, high(TOP1)
	ldi r16, low(TOP1)
	sts OCR1AH, r17
	sts OCR1AL, r16
	clr r16
	sts TCCR1A, r16
	ldi r16, (1 << WGM12) | (1 << CS12) | (1 << CS10)
	sts TCCR1B, r16
	ldi r16, (1 << OCIE1A)
	sts TIMSK1, r16

	; Timer 3 is for updating the LCD display. We are
	; *not* able to call LCD routines from within an 
	; interrupt handler, so this timer must be used
	; in a polling loop.
	ldi r17, high(TOP3)
	ldi r16, low(TOP3)
	sts OCR3AH, r17
	sts OCR3AL, r16
	clr r16
	sts TCCR3A, r16
	ldi r16, (1 << WGM32) | (1 << CS32) | (1 << CS30)
	sts TCCR3B, r16
	; Notice that the code for enabling the Timer 3
	; interrupt is missing at this point.

	; Timer 4 is for updating the contents to be displayed
	; on the top line of the LCD.
	ldi r17, high(TOP4)
	ldi r16, low(TOP4)
	sts OCR4AH, r17
	sts OCR4AL, r16
	clr r16
	sts TCCR4A, r16
	ldi r16, (1 << WGM42) | (1 << CS42) | (1 << CS40)
	sts TCCR4B, r16
	ldi r16, (1 << OCIE4A)
	sts TIMSK4, r16

	sei

; =============================================
; ====    END OF "DO NOT TOUCH" SECTION    ====
; =============================================

; ****************************************************
; **** BEGINNING OF SECOND "STUDENT CODE" SECTION ****
; ****************************************************

start:
	rcall lcd_init
loop_polling:
	in temp, TIFR3 
	sbrs temp, OCF3A
	rjmp loop_polling
		
	ldi temp, 1<<OCF3A
	out TIFR3, temp

	lds r22, LAST_BUTTON_PRESSED
	lds r23, BUTTON_IS_PRESSED

	tst r23 ; no button pressed '-' displayed
	breq no_button

	cpi r22, 1 ; if equal, right button pressed and 'R' displayed
	breq right_button

	cpi r22, 2 ; if equal, up button pressed and 'U' displayed
	breq up_button

	cpi r22, 3 ; ifequal, down button pressed and 'D' displayed
	breq down_button

	jmp left_button ;if none of them equal, definitely left is pressed 
		
no_button:
	ldi r16, 1
	ldi r17, 15
	push r16
	push r17
	rcall lcd_gotoxy
	pop r17
	pop r16

	ldi r16, '-'
	push r16
	rcall lcd_putchar
	pop r16

	rjmp loop_polling

right_button:
	ldi r16, 1
	ldi r17, 3
	push r16
	push r17
	rcall lcd_gotoxy
	pop r17
	pop r16

	ldi r16, 'R'
	push r16
	rcall lcd_putchar
	pop r16
			
	call clear_for_right ;
	call star_on ; call the function to display '*' as button is pressed
	rjmp loop_polling ; return to the start

up_button:
	ldi r16, 1
	ldi r17, 2
	push r16
	push r17
	rcall lcd_gotoxy
	pop r17
	pop r16

	ldi r16, 'U'
	push r16
	rcall lcd_putchar
	pop r16
	call left_char		
	call clear_for_up ;
	call star_on ; 
	rjmp loop_polling ; return to start

down_button:
	ldi r16, 1
	ldi r17, 1
	push r16
	push r17
	rcall lcd_gotoxy
	pop r17
	pop r16

	ldi r16, 'D'
	push r16
	rcall lcd_putchar
	pop r16
	call left_char	
	call clear_for_down 
	call star_on ; 
	rjmp loop_polling ; return start

left_button:
	ldi r16, 1
	ldi r17, 0
	push r16
	push r17
	rcall lcd_gotoxy
	pop r17
	pop r16

	ldi r16, 'L'
	push r16
	rcall lcd_putchar
	pop r16
			
	call clear_for_left 
	call star_on ; call fucntion to display * as button is pressed alongisde of 'L' displayed
	rjmp loop_polling ; return to start

timer1:
	push r16
	in r16, SREG
	push r16
	push r17 
	push r23

	; all the code below is taken from Lab4 files	
	lds	r16, ADCSRA 
	ori r16, 0x40 ; 0x40 = 0b01000000
	sts	ADCSRA, r16

	wait:
		lds r16, ADCSRA
		andi r16, 0x40
		brne wait

		clr r23 

		lds r17, ADCSRA ;this reads the button is pressed or not
		lds DATAL, ADCL
		lds DATAH, ADCH
							 
	check_for_none:
		ldi r16, low(900) ;threshold above 900 - no button pressed
		mov BOUNDARY_L, r16
		ldi r16, high(900)
		mov BOUNDARY_H, r16

		cp DATAL, BOUNDARY_L
		cpc DATAH, BOUNDARY_H
		brsh skip 
						
	check_right: ; lowest to highest threshold checking 
		ldi r16, low(BUTTON_RIGHT_ADC);
		mov BOUNDARY_L, r16
		ldi r16, high(BUTTON_RIGHT_ADC)
		mov BOUNDARY_H, r16

		cp DATAL, BOUNDARY_L
		cpc DATAH, BOUNDARY_H
		brsh check_up
		ldi r23, 1
		ldi r17, 1 ; if right button is pressed, 1 is stored in register 17
		sts LAST_BUTTON_PRESSED, r17
		rjmp skip

	check_up: ;this checks if the threshhold matches for the up button
		ldi r16, low(BUTTON_UP_ADC);
		mov BOUNDARY_L, r16
		ldi r16, high(BUTTON_UP_ADC)
		mov BOUNDARY_H, r16

		cp DATAL, BOUNDARY_L
		cpc DATAH, BOUNDARY_H
		brsh check_down
		ldi r23, 1
		ldi r17, 2 ; if up button pressed, 2 is loaded on r17
		sts LAST_BUTTON_PRESSED, r17
		rjmp skip

	check_down:  ;this checks if the threshhold matches for the down button
		ldi r16, low(BUTTON_DOWN_ADC);
		mov BOUNDARY_L, r16
		ldi r16, high(BUTTON_DOWN_ADC)
		mov BOUNDARY_H, r16

		cp DATAL, BOUNDARY_L
		cpc DATAH, BOUNDARY_H
		brsh check_left
		ldi r23, 1
		ldi r17, 3
		sts LAST_BUTTON_PRESSED, r17
		rjmp skip

	check_left:  ;this checks if the threshhold matches for the left button
		ldi r16, low(BUTTON_LEFT_ADC);
		mov BOUNDARY_L, r16
		ldi r16, high(BUTTON_LEFT_ADC)
		mov BOUNDARY_H, r16

		cp DATAL, BOUNDARY_L
		cpc DATAH, BOUNDARY_H
		brsh skip
		ldi r23, 1
		ldi r17, 4 
		sts LAST_BUTTON_PRESSED, r17
		rjmp skip	
skip:	
	sts BUTTON_IS_PRESSED, r23
	pop r23
	pop r17
	pop r16
	out SREG, r16
	pop r16
	reti

star_on: ; this function loop is from part a to display a '*' when a button is pressed
	push r16
	push r17
	
	ldi r16, 1 ;setting up columns and rows to display the star 
	ldi r17, 15
	push r16
	push r17
	rcall lcd_gotoxy
	pop r17
	pop r16

	ldi r16, '*'
	push r16
	rcall lcd_putchar
	pop r16
	pop r17
	pop r16
	ret

clear_for_right: ; clear every things in bottom row from left except the R
	push r16
	push r17
	push r18
	push r19

	clr r18
	ldi r19, 0
loop: ; this loop will clear every things in bottom row from left except the R
	ldi r16, 1
	mov r17, r19
	push r16
	push r17
	rcall lcd_gotoxy
	pop r17
	pop r16

	ldi r16, ' ' ; and empty character meant to clear as stated above
	push r16
	rcall lcd_putchar
	pop r16

	inc r18
	inc r19
	cpi r18, 3
	brne loop
	pop r19
	pop r18
	pop r17
	pop r16
	ret

clear_for_up: ; clear every things in bottom row from left except the U
	push r16
	push r17
	push r18
	push r19

	clr r18
	ldi r19, 0
loop1:
	ldi r16, 1
	mov r17, r19
	push r16
	push r17
	rcall lcd_gotoxy
	pop r17
	pop r16

	ldi r16, ' '
	push r16
	rcall lcd_putchar
	pop r16
	inc r18
	inc r19
		
	cpi r18, 2
	brne loop1_done
	inc r19

loop1_done:
	cpi r18, 3
	brne loop1
	
	pop r19
	pop r18
	pop r17
	pop r16
	ret

clear_for_down: ; clear every things in bottom row from left except the D
	push r16
	push r17
	push r18
	push r19

	clr r18
	ldi r19, 0
	loop3:
	ldi r16, 1
	mov r17, r19
	push r16
	push r17
	rcall lcd_gotoxy
	pop r17
	pop r16

	ldi r16, ' '
	push r16
	rcall lcd_putchar
	pop r16
	inc r18
	inc r19
	cpi r18, 1
	brne loop3_done
	inc r19

	loop3_done:
	cpi r18, 3
	brne loop3
	
	pop r19
	pop r18
	pop r17
	pop r16
	ret
		
clear_for_left: ; clear every things in bottom row from left except the L
	push r16
	push r17
	push r18
	push r19

	clr r18
	ldi r19, 1
	loop2:
	ldi r16, 1
	mov r17, r19
	push r16
	push r17
	rcall lcd_gotoxy
	pop r17
	pop r16
	ldi r16, ' '
	push r16
	rcall lcd_putchar
	pop r16

	inc r18
	inc r19
	cpi r18, 4
	brne loop2

	pop r19
	pop r19
	pop r17
	pop r16
	ret

left_char: ; upper left char on lcd
	push r16
	push r17
	
	ldi r16, 0 ;the row and column where char is set
	ldi r17, 0
	push r16
	push r17
	rcall lcd_gotoxy
	pop r17
	pop r16
	lds r16, TOP_LINE_CONTENT ; this char is blank and only updates if up or down is pressed

	push r16
	rcall lcd_putchar
	pop r16

	pop r17
	pop r16
	ret
stop:
	rjmp stop

; timer3:
;
; Note: There is no "timer3" interrupt handler as you must use
; timer3 in a polling style (i.e. it is used to drive the refreshing
; of the LCD display, but LCD functions cannot be called/used from
; within an interrupt handler).


timer4:
	push ZL
	push ZH
	push r16
	in r16, SREG
	push r16
	push r18
	push r19
	push r20
	push r21

	lds r16, BUTTON_IS_PRESSED
	cpi r16, 1
	brne timer_done 
	ldi ZH, high(AVAILABLE_CHARSET<<1) 
	ldi ZL, low(AVAILABLE_CHARSET<<1)
	clr r20
	clr r19

	check_length: 
		lpm r18, Z+
		cpi r18, 0x00
		breq check_button
		inc r19 
		rjmp check_length

	check_button:
		lds r16, LAST_BUTTON_PRESSED
		cpi r16, 2
		breq go_up

		lds r16, LAST_BUTTON_PRESSED 
		cpi r16, 3
		breq go_down
								
	timer_done:
		pop r21
		pop r20
		pop r19
		pop r18
		pop r16
		out SREG, r16
		pop r16
		pop ZH
		pop ZL
		reti

go_up:
		ldi ZH, high(AVAILABLE_CHARSET<<1) 
		ldi ZL, low(AVAILABLE_CHARSET<<1)
		lds r21, CURRENT_CHARSET_INDEX 
		cp r21, r19  
		breq up_char_done
find_char: 
		lpm r18, Z+
		cp r20, r21
		breq char_found
		inc r20
		rjmp find_char

up_char_done: ; reaches here if up button is pressed
		ldi ZH, high(AVAILABLE_CHARSET<<1)
		ldi ZL, low(AVAILABLE_CHARSET<<1)
		clr r20
		clr r21
		lpm r18, Z+
char_found:
		sts TOP_LINE_CONTENT, r18 
		inc r20
		sts CURRENT_CHARSET_INDEX, r20 
		rjmp timer_done

go_down:
		ldi ZH, high(AVAILABLE_CHARSET<<1) ; initialize r30:r31
		ldi ZL, low(AVAILABLE_CHARSET<<1)
		lds r21, CURRENT_CHARSET_INDEX ; curr index
		tst r21
		breq digit_reached
find_char1: 
		dec ZL
		lpm r18, Z
		cp r20, r21
		breq char_found1
		dec r20
		rjmp find_char1
digit_reached: ; this function is recahed if down button is pressed with first char in string 
		lpm r18, Z
		inc r20 
char_found1:
		sts TOP_LINE_CONTENT, r18 
		dec r20
		sts CURRENT_CHARSET_INDEX, r20 

		rjmp timer_done



; ****************************************************
; ******* END OF SECOND "STUDENT CODE" SECTION *******
; ****************************************************


; =============================================
; ==== BEGINNING OF "DO NOT TOUCH" SECTION ====
; =============================================

; r17:r16 -- word 1
; r19:r18 -- word 2
; word 1 < word 2? return -1 in r25
; word 1 > word 2? return 1 in r25
; word 1 == word 2? return 0 in r25
;
compare_words:
	; if high bytes are different, look at lower bytes
	cp r17, r19
	breq compare_words_lower_byte

	; since high bytes are different, use these to
	; determine result
	;
	; if C is set from previous cp, it means r17 < r19
	; 
	; preload r25 with 1 with the assume r17 > r19
	ldi r25, 1
	brcs compare_words_is_less_than
	rjmp compare_words_exit

compare_words_is_less_than:
	ldi r25, -1
	rjmp compare_words_exit

compare_words_lower_byte:
	clr r25
	cp r16, r18
	breq compare_words_exit

	ldi r25, 1
	brcs compare_words_is_less_than  ; re-use what we already wrote...

compare_words_exit:
	ret

.cseg
AVAILABLE_CHARSET: .db "0123456789abcdef_", 0


.dseg

BUTTON_IS_PRESSED: .byte 1			; updated by timer1 interrupt, used by LCD update loop
LAST_BUTTON_PRESSED: .byte 1        ; updated by timer1 interrupt, used by LCD update loop

TOP_LINE_CONTENT: .byte 16			; updated by timer4 interrupt, used by LCD update loop
CURRENT_CHARSET_INDEX: .byte 16		; updated by timer4 interrupt, used by LCD update loop
CURRENT_CHAR_INDEX: .byte 1			; ; updated by timer4 interrupt, used by LCD update loop


; =============================================
; ======= END OF "DO NOT TOUCH" SECTION =======
; =============================================


; ***************************************************
; **** BEGINNING OF THIRD "STUDENT CODE" SECTION ****
; ***************************************************

.dseg

; If you should need additional memory for storage of state,
; then place it within the section. However, the items here
; must not be simply a way to replace or ignore the memory
; locations provided up above.


; ***************************************************
; ******* END OF THIRD "STUDENT CODE" SECTION *******
; ***************************************************
