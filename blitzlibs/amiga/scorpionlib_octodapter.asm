CIAA_prb equ $BFE101 ;Parallel port
CIAA_ddrb equ $BFE301   ;Direction for port B (BFE101);1=output (can be in or out)
CIAB_ddra equ $BFD200    ;Direction for Port A (BFD000);1 = output (set to 0xFF)    
CIAB_pra equ $BFD000 ;pra     /DTR  /RTS  /CD   /CTS  /DSR   SEL   POUT  BUSY

;No parameters
_octodapter_Init
    or.b #7,CIAB_ddra    ; output for addressing the joystick port 

_dynablaster_Init ;Fall through
    move.b #0,CIAA_ddrb  ; input for $BFE101
    rts

;D0.b = port to read 0-7, does not include main Amiga joystick ports. Returns joystick state in D0.b
_octodapter_Read
    and.b #$F8,CIAB_pra
    or.b  d0,CIAB_pra
    move.b CIAA_prb,d0   ; the input of the adressed joyport
    not.b d0	        ; invert the input so that a logical 1 is pressed
    rts

ciab_read
    dc.b 0

ciaa_read
    dc.b 0

;D0.b = port to read 0-1
_dynablaster_read
    move.b CIAB_pra,d6 ;buttons
    move.b CIAA_prb,d7 ;directions

    ;Create a mask - bits aren't valid until they've had a value of 1 (eg NOT pressed)
    or.b d6,(ciab_read)
    or.b d7,(ciaa_read)
    
    ;Negate bits so 1 is pressed    
    not.b d6 
    not.b d7

    ;Apply the mask 
    and.b (ciab_read),d6
    and.b (ciaa_read),d7

    tst.b d0
    bne _dynablaster4
    
_dynablaster3
    and.b #4,d6 ;Get fire 1 only
    lsl.b #2,d6 ;Move to fire 1 position
    and.b #$f,d7 ;Filter out direction bits
    move.b d7,d0
    or.b d6,d0
    rts

_dynablaster4
    and.b #1,d6 ;Get fire 1 only
    lsl.b #4,d6 ;Move to fire 1 position
    lsr.b #4,d7 ;Filter out direction bits
    move.b d7,d0 
    or.b d6,d0
    rts


    ;We also want to copy bit 6->7 to be compatible with Scorpion's implementation of button 3
;    move.b d0,d1    
;    lsl.b #1,d1
;    and.b #$80,d1
;    or.b d1,d0