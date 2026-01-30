HBlank equ $100000
NextHBlank equ HBlank+2

;Timer hints for REG_LSPCMODE
Timer_On equ $10
Timer_On_Set equ $30
Timer_On_Set_HBlank equ $70
Timer_On_Set_Zero equ $b0
Timer_On_Set_HBlank_Zero equ $f0

Timer_On_HBlank equ $50
Timer_On_HBlank_Zero equ $d0

Timer_On_Zero equ $90

Var_CustomHBlank equ NeoHBlankHandlerEnd-NeoHBlankHandler+HBlank ;Long, pointer to a custom user code HBlank handler
Var_CustomTimer equ Var_CustomHBlank+4 ;Long, timer to reset to AFTER the first line is rendered
Var_MapSprite equ Var_CustomTimer+4 ;Word, just the pointer to the X position of the map sprite in VRAM
Var_CustomWave equ Var_MapSprite+2 ;Long, pointer to the custom wave data
Var_CustomWavePosition equ Var_CustomWave+4 ;Long, pointer to the current read position of the wave data

Var_EndOfVariables equ Var_CustomWavePosition+4
MemStart equ Var_EndOfVariables

OpCode_RTE equ $4E73
OpCode_JSR equ $4EB9

FIX_CLEAR equ $C004C2                       ; Clear Fix layer
LSP_FIRST equ $C004C8 ; Clear SCB2-4, first SCB1 tilemap
REG_STATUS_B equ $380000

VRAM_BASE equ $3C0002
VRAM_ADDRESS equ -2
VRAM_WRITE equ 0 ;Since write operations are the most common, we'll use zero here
VRAM_MOD equ 2

REG_VRAMADDR equ $3C0000
REG_VRAMMOD equ $3C0004
REG_VRAMRW equ $3C0002

REG_P1CNT equ $300000
REG_P2CNT equ $340000

BIOS_P1STATUS equ $10FD94
BIOS_P2STATUS equ $10FD9A
BIOS_P3STATUS equ $10FDA0
BIOS_P4STATUS equ $10FDA7

BIOS_STATCURNT equ $10FDAC	

STATUS_CURRENT equ 2

TIMER_HIGH equ $3C0008
TIMER_LOW equ $3C000A
REG_LSPCMODE equ $3C0006

NeoHBlankHandler
  dc.w $4EF9;Force no optimisation
  dc.l HBlankHandler_Default
NeoHBlankHandlerEnd

HBlankHandler_Custom
  ;Backup and restore A0
  movem.l A0,-(A7)
  move.l (Var_CustomHBlank),A0
  jsr (A0)
  movem.l (A7)+,A0
  tst.l (Var_CustomWavePosition)
  beq HBlankHandler_EndOfLine ;No custom wave, so turn off the timer
  move.l #HBlankHandler_VScroll,(NextHBlank)
  move.w #Timer_On_Set_Zero,(REG_LSPCMODE)
  move.w #0,(TIMER_HIGH)
  move.w #383,(TIMER_LOW)
  bra HBlankHandler_Default

HBlankHandler_EndOfLine
  ;Turn off the timer
  move.l #0,(Var_CustomWavePosition)
  move.w #0,(REG_LSPCMODE)

HBlankHandler_Default
  move.w	#2,$3C000C			;LSPC_IRQ_ACK - ack. interrupt #2 (HBlank)
  rte

HBlankHandler_VScroll
  movem.l A0/D0,-(A7)
  move.w (Var_MapSprite),(REG_VRAMADDR)  
  move.l (Var_CustomWavePosition),A0 ;Add timing gap between vram use
  add.l #2,(Var_CustomWavePosition) ;Add timing gap between vram use
  move.w (A0),(REG_VRAMRW)

  ;Are we beyond the end of the screen?
  move.w (REG_LSPCMODE),D0
  lsr.w #7,D0
  cmp.w #$1EE,D0
  movem.l (A7)+,A0/D0  ;Does not affect condition codes

  bge HBlankHandler_EndOfLine
  bra HBlankHandler_Default

;Install the HBlank handler
SE_Neo_Setup
  Lea NeoHBlankHandler,A0
  Move.l #HBlank,A1
  Lea NeoHBlankHandlerEnd,A2
neo_setup_loop
  move.w (A0)+,(A1)+
  Cmp.l a2,a0
  bne neo_setup_loop
  rts

;Fake version of Amiga's "Allocmem"
SE_NEO_FakeAllocMem:
  Move.l           #MemStart,A0 ;Eat memory from the top (reserving a long word for Scorpion flags - still needed?)

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

;Configure the HBlank code to run at the start of the first visible line
SE_Neo_RefreshHBlank  
  tst.l (Var_CustomHBlank)
  beq.s SE_Neo_RefreshHBlank_Off

  move.l #HBlankHandler_Custom,(NextHBlank)
  move.w #Timer_On_Set,(REG_LSPCMODE)
  move.w (Var_CustomTimer),(TIMER_HIGH)
  move.w (Var_CustomTimer+2),(TIMER_LOW)  
  rts

SE_Neo_RefreshHBlank_Off
  Move.w #0,(REG_LSPCMODE)
  rts

;D0 = My custom vblank (false to turn off)
;D1 = Wave data (false to turn off)
SE_Neo_Custom_HBlank
  Move.w #0,(REG_LSPCMODE) ;Just turn off the timer since RefreshHBlank will turn it back on
  Move.l D0,(Var_CustomHBlank)
  Move.l D1,(Var_CustomWave)
  Move.l #$7FFFFFFF,(Var_CustomTimer)
  RTS

;D0 = Number of lines to wait
SE_Neo_Custom_HBlank_Lines

  ;Set the line wait
  Move.w D0,D7
  Mulu.w #384,D7
  Add.l #(384*39-1),D7 ;Add the gap from the top of the screen
  Move.l D7,(Var_CustomTimer)

  ;Set the target map sprite
  Add.w #$8400,D1
  Move.w D1,(Var_MapSprite)

  ;Calculate the wave position offset
  move.l (Var_CustomWave),D7
  beq NoCustomWave
  add.w D0,D0
  add.l D0,D7
  move.l D7,(Var_CustomWavePosition)
  rts

NoCustomWave
  move.l #0,(Var_CustomWavePosition)
  rts

;This version applies a custom palette increase
;D0 = Source address (long)
;D1 = VDP address (word) (not left shifted)
;D2 = Count (word)
;D3 = Palette address (offset)
SE_Neo_SpriteBatchTilePalette
  subq.w       #1,D2
  blt          SpriteBatchCancel ;Zero or less to do

  lea.l        VRAM_BASE,A0
  move.l       D0,A1
  lsl.w        #6,D1
  move.w       D1,VRAM_ADDRESS(A0)
  move.w       #1,VRAM_MOD(A0)  

SpriteBatchTilePaletteLoop
  Move.w      (A1)+,(A0) ;Even address
  Move.w      (A1)+,D0 ;Odd address
  Add.w       D3,D0 ;Increase by the palette line
  Move.w      D0,(A0)
  dbra        D2,SpriteBatchTilePaletteLoop

  rts

;D0 = Source address (long)
;D1 = VDP address (word) (not left shifted)
;D2 = Count (word)
SE_Neo_SpriteBatchTile
  subq.w       #1,D2
  blt          SpriteBatchCancel ;Zero or less to do

  lea.l        VRAM_BASE,A0
  move.l       D0,A1
  lsl.w        #6,D1
  move.w       D1,VRAM_ADDRESS(A0)
  move.w       #1,VRAM_MOD(A0)  

SpriteBatchTileLoop
  Move.w      (A1)+,(A0) ;Even address
  Move.w      (A1)+,(A0) ;Odd address
  dbra        D2,SpriteBatchTileLoop

  rts

;D0 = Source address (long)
;D1 = VDP address (word)
;D2 = Count (word)
SE_Neo_SpriteBatch
  subq.w       #1,D2
  blt          SpriteBatchCancel ;Zero or less to do
  lea.l        VRAM_BASE,A0
  move.l       D0,A1
  move.w       D1,VRAM_ADDRESS(A0)
  move.w       #1,VRAM_MOD(A0)

SpriteBatchLoop
  Move.w      (A1)+,(A0)
  dbra        D2,SpriteBatchLoop

SpriteBatchCancel
  rts

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
    move.w  d2,REG_VRAMADDR
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

    move.w  d4,REG_VRAMRW
    add.w   d2,d1      ; Accumulate error
    subq.b  #1,d3      ; Next sprite
    bne     .dzh

    move.w  D7,D0
    lsr.w   #1,D0
    rts

NoShrinkX
    or.w   #$F00,d1 ;Put full X width on the Y width we've already established

NoShrinkXNext
    move.w  d1,REG_VRAMRW
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
