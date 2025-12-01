;No parameters
_octodapter_Init
    or.b #7,$bfd200    ; output for addressing the joystick port 
    move.b #0,$BFE301  ; input for $BFE101
    rts

;D0.b = port to read 0-7, does not include main Amiga joystick ports. Returns joystick state in D0.b
_octodapter_Read
    and.b #$F8, $BFD000
    or.b  d0, $BFD000
    move.b $bfe101,d0   ; the input of the adressed joyport
    not.b d0	        ; invert the input so that a logical 1 is pressed

    ;We also want to copy bit 6->7 to be compatible with Scorpion's implementation of button 3
    move.b d0,d1    
    lsl.b #1,d1
    and.b #$80,d1
    or.b d1,d0
    rts
