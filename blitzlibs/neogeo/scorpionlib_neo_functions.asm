FIX_CLEAR equ $C004C2                       ; Clear Fix layer
LSP_FIRST equ $C004C8 ; Clear SCB2-4, first SCB1 tilemap
REG_STATUS_B equ $380000

VRAM_BASE equ $3C0002
VRAM_ADDRESS equ -2
VRAM_WRITE equ 0 ;Since write operations are the most common, we'll use zero here
VRAM_MOD equ 2

REG_VRAMRW equ $3C0000
REG_VRAMMOD equ $3C0004
VRAM_RW equ $3C0002

REG_P1CNT equ $300000
REG_P2CNT equ $340000

BIOS_P1STATUS equ $10FD94
BIOS_P2STATUS equ $10FD9A
BIOS_P3STATUS equ $10FDA0
BIOS_P4STATUS equ $10FDA7

BIOS_STATCURNT equ $10FDAC	

STATUS_CURRENT equ 2

;Fake version of Amiga's "Allocmem"
SE_NEO_FakeAllocMem:
  LEA           $100000,A0 ;Eat memory from the top

FakeAllocMem_Loop
  Tst.l         (A0)
  BEQ           FakeAllocMem_Out ;No memory assigned here
  Add.l         (A0),A0 ;Add the length to the current address
  AddQ.l        #4,A0   ;Including the long containing the reserved memory length
  Bra           FakeAllocMem_Loop ;Try again

FakeAllocMem_Out
  Move.l        D0,(A0) ;Store the amount reserved at this address
  Move.l        A0,D0  ;Get the current address
  AddQ.l        #4,D0 ;Skip the amount stored
  RTS           ;Return memory location in D0

SE_NEO_Tile
  lea.l        VRAM_BASE,A0
  move.w       #1,VRAM_MOD(A0)
  lsl.w        #6,D0 ;multiply by 64 to get the sprite
  lsl.w        #1,D1 ;multiply by two to get the pattern down the line
  add.w        D0,D1
  move.w       D1,VRAM_ADDRESS(A0)
  move.l       D2,A1
  move.w       (A1)+,VRAM_WRITE(A0)
  move.w       (A1)+,VRAM_WRITE(A0)
  RTS   

SE_Neo_SpriteX
  lea.l        VRAM_BASE,A0
  add.w        #$8400,D0
  move.w       D0,VRAM_ADDRESS(A0)
  Lsl.w        #7,D1
  move.w       D1,VRAM_WRITE(A0)
  RTS

SE_Neo_SpriteHeight
  lea.l        VRAM_BASE,A0
  add.w        #$8200,D0
  move.w       D0,VRAM_ADDRESS(A0)
  move.w       D1,VRAM_WRITE(A0)
  RTS

SE_Neo_SpriteStick
  lea.l        VRAM_BASE,A0
  add.w        #$8200,D0
  move.w       D0,VRAM_ADDRESS(A0)
  move.w       #$40,VRAM_WRITE(A0)
  RTS

SE_NEO_Gamepad1
  Move.b        BIOS_STATCURNT,D0
  LSL.w         #8,D0	
  Move.b        BIOS_P1STATUS+STATUS_CURRENT,D0
  And.w         #$3FF,D0
  RTS

SE_NEO_Gamepad2
  Move.b        BIOS_STATCURNT,D0
  LSL.w         #6,D0
  Move.b        BIOS_P2STATUS+STATUS_CURRENT,D0
  And.w         #$3FF,D0
  RTS

SE_NEO_Gamepad3
  Move.b        BIOS_STATCURNT,D0
  LSL.w         #4,D0	
  Move.b        BIOS_P2STATUS+STATUS_CURRENT,D0
  And.w         #$3FF,D0
  RTS

SE_NEO_Gamepad4
  Move.b        BIOS_STATCURNT,D0
  LSL.w         #2,D0
  Move.b        BIOS_P4STATUS+STATUS_CURRENT,D0
  And.w         #$3FF,D0
  RTS  

SE_NEO_ClearVDP

  ;Wipe sprite tiles
  Lea.l         VRAM_BASE,A0
  Move.w        #1,VRAM_MOD(A0)
  Move.w        #0,VRAM_ADDRESS(A0)
  MOVEQ         #0,D0
  Move.w        #$6FFE,D1
TileClearLoop
  Move.w        D0,VRAM_WRITE(A0)
  dbra.w        D1,TileClearLoop

  jmp           LSP_FIRST ;Clear sprites
  ;Still should do fix clear?

SE_Neo_ShrinkSprite
    ;https://wiki.neogeodev.org/index.php?title=Scaling_sprite_groups
    ; d0.w is the group's overall x shrink value ($00~$FF)
    ; d1.b is the group's y shrink value ($00~$FF)
    ; d2.w is the first sprite's SCB2 VRAM address ($8000+)
    ; d3.b is the group's width in sprites
    ; returns in D0 the amount of "adjustment" to center on the x axis

    add.w   #$8000,d2
    move.w  d2,REG_VRAMRW
    move.w  #1,REG_VRAMMOD

    cmp.b   #$ff,D0
    beq NoShrinkX

    moveq   #0,D7

    and.w   #$ff,d0 ;ensure is positive value
    move.w  d0,d2      ; Fast *15
    lsl.w   #4,d0
    sub.w   d2,d0

    move.w  d0,d2
    andi.w  #$0F00,d0  ; d0 = integer part << 8    
    or.b    d1,d0      ; Mix in y shrink
    andi.w  #$00FF,d2  ; d2 = fractional part
    move.w  d2,d1      ; d1 = error accumulator

.dzh:
    move.w  d0,d4
    btst.l  #8,d1      ; Enough error to add one pixel ?
    beq     .novf
    addi.w  #$0100,d4
    andi.w  #$00FF,d1  ; Trim error accumulator

.novf:    
    move.w  d4,d6
    lsr.w   #$8,d6
    sub.w   #$F,D6 ;How many pixels were deleted?
    sub.w   D6,D7

    move.w  d4,VRAM_RW
    add.w   d2,d1      ; Accumulate error
    subq.b  #1,d3      ; Next sprite
    bne     .dzh

    move.w  D7,D0
    lsr.w   #1,D0
    rts

NoShrinkX
    or.w   #$F00,d1 ;Put full X width on the Y width we've already established

NoShrinkXNext
    move.w  d1,VRAM_RW
    subq.b  #1,d3      ; Next sprite
    bne     NoShrinkXNext

    moveq   #0,D0 ;Dont adjust on x
    rts

SE_Neo_ShrinkSprite_YAmount
    ;Use for centering on Y
    ; d0.w is the group's overall y shrink value ($00~$FF)
    ; d1.b is the group's height in sprites
    ; returns in D0 the amount of "adjustment" to center on the y axis
    cmp.b   #$ff,d0
    beq     NoYShrink

    moveq   #0,D7
    move.b  d1,d3

    and.w   #$ff,d0 ;ensure is positive value
    add.w   #$8000,d2
    move.w  d0,d2      ; Fast *15
    lsl.w   #4,d0
    sub.w   d2,d0

    move.w  d0,d2
    andi.w  #$0F00,d0  ; d0 = integer part << 8    
    or.b    d1,d0      ; Mix in y shrink
    andi.w  #$00FF,d2  ; d2 = fractional part
    move.w  d2,d1      ; d1 = error accumulator

.dzh2:
    move.w  d0,d4
    btst.l  #8,d1      ; Enough error to add one pixel ?
    beq     .novf2
    addi.w  #$0100,d4
    andi.w  #$00FF,d1  ; Trim error accumulator

.novf2:    
    move.w  d4,d6
    lsr.w   #$8,d6
    sub.w   #$F,D6 ;How many pixels were deleted?
    sub.w   D6,D7

    add.w   d2,d1      ; Accumulate error
    subq.b  #1,d3      ; Next sprite
    bne     .dzh2

    move.w  D7,D0
    lsr.w   #1,D0
    rts

NoYShrink
    moveq   #0,D0 ;Don't adjust on Y
    rts

;MEZZ ESTATE
MZS_send_user_command
    or.b #$80,D0
    or.b #$80,D1

    Move.b D0,$320000
    EOR.b #$FF,D0
commandloop    
    jsr Z80Wait
    move.b $320000,d7
    cmp.b d7,D0
    bne commandloop

    Move.b D1,$320000
    EOR.b #$FF,D1
paramloop
    jsr Z80Wait
    move.b $320000,d7
    cmp.b d7,d1
    bne paramloop

    rts

; Z80Wait
; Gives the Z80 some time to react. (Taken from smkdan's demo) FREEM
Z80Wait:
	move.w	d0,-(sp)
	move.w	#1000,d0		; arbitrary delay length

.Z80Wait_loop:
	dbra	d0,.Z80Wait_loop

	move.w	(sp)+,d0
	rts