
Scorpion_SpriteData equ 84
Scorpion_ImageData_MaxSprite equ 9

CustomBase equ $DFF000
DMACONR equ $2
DMACON equ $96

BLTCON0 equ $40;($DFF040 - BlitterBase)
BLTCON1 equ $42;($DFF042 - BlitterBase)
BLTAFWM equ $44;($DFF044 - BlitterBase)
BLTALWM equ $46;($DFF044 - BlitterBase)

BLTAMOD equ $64;($DFF064 - BlitterBase)
BLTBMOD equ $62;($DFF062 - BlitterBase)
BLTCMOD equ $60;($DFF060 - BlitterBase)
BLTDMOD equ $66;($DFF066 - BlitterBase)

BLTAPTH equ $50;($DFF050 - BlitterBase)
BLTBPTH equ $4c;($DFF04C - BlitterBase)
BLTCPTH equ $48;($DFF048 - BlitterBase)
BLTDPTH equ $54;($DFF054 - BlitterBase)

BLTSIZE equ $58;($DFF058 - BlitterBase)

Supervisor equ    -30
AFB_68020 equ 1
AttnFlags equ 128

FlushCaches
  	movem.l	      a0-a6,-(a7)
    move.l        4,a6
    btst          #AFB_68020,AttnFlags+1(a6)
    beq           .skipFlushCache
    lea           flushCachesInterrupt(pc),a5
    jsr           Supervisor(a6)
    .skipFlushCache:
    movem.l	      (a7)+,a0-a6
    rts

flushCachesInterrupt:
    mc68020
    movec    cacr,d0
    tst.w    d0
    bmi    .1            ; 68040/68060 I-Cache enabled?

    ; clear 68020/68030 caches
    or.w    #$808,d0
    movec    d0,cacr
    rte

    mc68040
    ; clear 68040/68060 caches
.1:    nop
    cpusha    bc
    rte

GetVBR
	movem.l	a0-a6,-(a7)

	;Load vector base into A0
	sub.l   a0,a0
	move.l  4.w,a6
	btst    #0,297(a6)              ; check for 68010
	beq     EndGetVBR
	lea     final_getvbr(pc),a5
	jsr     -30(a6)

EndGetVBR
  move.l a0,d0
  
	movem.l	(a7)+,a0-a6
  rts

CPUBlankTileBlit
  Move.l D0,A0
  MoveQ #0,D7
  SubQ.l #1,D2
  AddQ #2,D1

ClearBP
  Move.w D7,(A0) ;1
  Add.l D1,A0
  Move.w D7,(A0) ;2
  Add.l D1,A0
  Move.w D7,(A0) ;3
  Add.l D1,A0
  Move.w D7,(A0) ;4
  Add.l D1,A0
  Move.w D7,(A0) ;5
  Add.l D1,A0
  Move.w D7,(A0) ;6
  Add.l D1,A0
  Move.w D7,(A0) ;7
  Add.l D1,A0
  Move.w D7,(A0) ;8
  Add.l D1,A0
  Move.w D7,(A0) ;1
  Add.l D1,A0
  Move.w D7,(A0) ;2
  Add.l D1,A0
  Move.w D7,(A0) ;3
  Add.l D1,A0
  Move.w D7,(A0) ;4
  Add.l D1,A0
  Move.w D7,(A0) ;5
  Add.l D1,A0
  Move.w D7,(A0) ;6
  Add.l D1,A0
  Move.w D7,(A0) ;7
  Add.l D1,A0
  Move.w D7,(A0) ;8
  Add.l D1,A0  

  DBra D2,ClearBP
  RTS


PushSpritePointer
  RTS

;NEWTYPE .CopperListSpriteDataPointer
;  POS_Cop.w ;0
;  POS.w ;2
;  CTL_Cop.w ;4
;  CTL.w ;6
;  PTH_Cop.w ;8
;  PTH.w ;10
;  PTL_Cop.w ;12
;  PTL.w ;14
;End NEWTYPE

SpriteCop_Pos equ 2
SpriteCop_Ctl equ 6
SpriteCop_Ph equ 10
SpriteCop_Pl equ 14

SpriteCop_Pos2 equ 18
SpriteCop_Ctl2 equ 22
SpriteCop_Ph2 equ 26
SpriteCop_Pl2 equ 30

;NEWTYPE .Scorpion_SpriteInfo ;CS
;  _data.l             ;00: NULL if no sprite present, else pointer to sprite data
;  _height.w           ;04: height of sprite, in pixels, plus an extra 1
;  _channels.w         ;06: number of sprite channels required to display sprite
;  _flags.w            ;08: low byte = pix width of sprite, hi bit = 1 if 16 colour sprite
;  _nextoff.w          ;10: difference in bytes between seperate sprites for separate sprite channels
;  AssetBundle.w
;End NEWTYPE

;D0 = Pointer to Copperlist
;D1 = Pointer to Sprite Data
;D2 = X position
;D3 = Y position
;D4 = The sprite channel

SpriteInfo_Height equ 4
SpriteInfo_Channels equ 6
SpriteInfo_16Col equ 8
SpriteInfo_NextOff equ 10

;SPRxPOS register:
;                 BIT#   SYM      FUNCTION
;                 ----   ----     -----------------------------
;                 15-08  SV7-SV0  Start vertical value. High bit(SV8) is
;                                 in SPRxCTL register below.
;                 07-00  SH8-SH1  Start horizontal value. Low bit(SH0) is
;                                 in SPRxCTL register below.

;    SPRxCTL register (writing this address disables sprite
;                      horizontal comparator circuit):

;    BIT#    SYM       FUNCTION
;    ----    --------  -----------------------------
;    15-08   EV7-EV0   End (stop) vertical value low 8 bits
;    07      ATT       Sprite attach control bit (odd sprites)
;    06-04    X        Not used
;    02      SV8       Start vertical value high bit
;    01      EV8       End (stop) vertical value high bit
;    00      SH0       Start horizontal value low bit

;D0 = Copperlist pointer -> SprXPos
;D1 = Sprite data pointer -> SprXCop
;D2 = X 
;D3 = Y
;D4 = Screen top
SDisplaySprite

  Move.l D0,A0 ;Copperlist pointer
  Move.l D1,A1 ;Sprite data pointer

  Move.w SpriteInfo_Height(A1),D5 ;Height temporarily goes into D5
  
  Move.l (A1),D6 ;Data pointer

  ;Fix the vertical position
  Tst.w D3
  BGE SDisplaySprite_BelowScreenTop

  ;If we're below zero, we need to subtract from the height and set the top to zero
  Add.w D3,D5
  BLE SDisplaySprite_Cancel ;If the height is less than or equal to zero, we need to cancel the sprite operation

  ;We also need to increase the data pointer by the negative
  Neg.w D3
  And.l #$FFFF,D3
SDisplaySprite_TopCutOff
  LSL.w #2,D3

  Add.l D3,D6 

  ;Set the y position to zero
  MoveQ #0,D3

SDisplaySprite_BelowScreenTop
  Add.w D4,D3 ;Add the top of the screen on to the start position

SDisplaySprite_SetOverBit
  MoveQ #2,D7 ;D7 used for the over bits

  ;Calculate SPRxPOS Vertical value (START), store in D0
  Move.w D3,D0
  LSL.w #8,D0  
  Addx.w D7,D7 ;Add the upper start bit  

  ;Calculate SPRxCTRL Vertical value (END), store in D1
  Move.w D3,D1 ;Get the Y value

  ;Clamp to 255 tall
  Cmp.w #255,D5
  BLT SDisplay_Add

  Add.w #255,D1
  Bra SDisplay_SkipAdd

SDisplay_Add
  Add.w D5,D1 ;Add the end position to the Y  

SDisplay_SkipAdd
  LSL.w #8,D1 ;Move into the upper 8 bits
  Addx.w D7,D7 ;Add the upper end bit  

  ;Calculate the SPRxPOS horizontal value, store in D0
  LSR.w #1,D2
  Addx.w D7,D7 ;Add the x pos start bit
  Move.b D2,D0 ;SprxPOS should now be complete
  Move.b D7,D1 ;SPRxCtrl should now be complete

  MoveQ #0,D7
  Move.b SpriteInfo_Channels(A1),D7 ;D7 is now used to contain the loop value
  MoveQ #0,D5
  Move.w SpriteInfo_NextOff(A1),D5; D5 stores the offset

  ;Is this a 16 color sprite
  TST.w SpriteInfo_16Col(A1)
  BEQ SDisplaySprite_4Col

  ;Append the attach flag to D1
  Or.w #$80,D1

SDisplaySprite_16Col_Loop

  ;First Sprite
CalculatePosXMask_16col
  ;Convert to byte 255
  And.b #$7F,D0 
  Move.w D0,SpriteCop_Pos(A0)
  Move.w D1,SpriteCop_Ctl(A0)

  Move.w D6,SpriteCop_Pl(A0)
  Swap D6
  Move.w D6,SpriteCop_Ph(A0)
  Swap D6
  Add.l D5,D6

  ;Second sprite
  Move.w D0,SpriteCop_Pos2(A0)
  Move.w D1,SpriteCop_Ctl2(A0)

  Move.w D6,SpriteCop_Pl2(A0)
  Swap D6
  Move.w D6,SpriteCop_Ph2(A0)

  ;Do we have move than two sprites left
  SubQ.b #2,D7
  BEQ SDisplaySpriteFinished

  ;We need to loop!
  Swap D6
  Add.l D5,D6
  Add.l #32,A0

SDisplaySprite_16Col_Next
  Add.b #64,D0
  Bra SDisplaySprite_16Col_Loop


SDisplaySprite_4Col

  ;First Sprite
CalculatePosXMask_4col
  And.b #$7F,D0 ;Convert to byte ;255

  Move.w D0,SpriteCop_Pos(A0)
  Move.w D1,SpriteCop_Ctl(A0)

  Move.w D6,SpriteCop_Pl(A0)
  Swap D6
  Move.w D6,SpriteCop_Ph(A0)

  ;Do we have move than one sprite left
  SubQ.b #1,D7
  BEQ SDisplaySpriteFinished
  Swap D6
  Add.l D5,D6
  Add.l #16,A0
  
SDisplaySprite_4Col_Next
  Add.b #64,D0
  Bra SDisplaySprite_4Col

SDisplaySpriteFinished
  RTS

SDisplaySprite_Cancel
  MoveQ #0,D7
  MoveQ #0,D0
  Move.b SpriteInfo_Channels(A1),D7 ;D7 is now used to contain the loop value

SDisplaySprite_CancelLoop
  Move.w D0,SpriteCop_Pos(A0)
  Move.w D0,SpriteCop_Ctl(A0)  
  SubQ.b #1,D7
  BEQ SDisplaySpriteFinished
  Add.l #16,A0
  BRA SDisplaySprite_CancelLoop

PatchScorpion
	Move.l D0,DoMaskBlit2+2
	Move.l D0,DoMaskBlit2FirstWordMask+2
	Move.l D0,DoBarBlit+2
  Move.w D1,SDisplaySprite_16Col_Next+2
  Move.w D1,SDisplaySprite_4Col_Next+2
  Move.w D2,SDisplaySprite_TopCutOff  
  Move.b D3,SDisplaySprite_SetOverBit+1
	bra FlushCaches

PatchScorpion_MP
	Move.w D0,CalculatePosXMask+2
	Move.w D0,CalculatePosXMask_16col+2
	Move.w D0,CalculatePosXMask_4col+2
  Move.w D0,CalculatePosXMask_PCloud1+2
  Move.w D0,CalculatePosXMask_PCloud2+2
	bra FlushCaches	

;Offset, d0
;Table Pointer d1
;Sprite Pointer d2
;Slice line offset d3
;Amount d4
SetPFrame		  

      ;Part 0 - add the increase amount to the offset
      Move.l D0,A0
      Move.l (A0),D7
      Add.l D4,D7
      Move.l D7,(A0)

		  ;Part 1 - 4x the size of offset
      Swap D7
		  And.w #$1f,D7
		  Add.w D7,D7
		  Add.w D7,D7
		  
		  ;Part 2, load the left table sprite
		  Move.l D1,A0 ;The source
		  Move.l D2,A1 ;The destination
		  Move.l (A0,D7),D6
		  Add.l D3,D6
		  Move.w D6,14(A1) ;Lower word
		  Swap D6
		  Move.w D6,10(A1) ;Upper word
		  
		  ;Part 3, load the right table sprite		 		 
		  Add.l #128,A0 ;The source
		  Add.l #26,A1 ;The destination
		  Move.l (A0,D7),D6
		  Add.l D3,D6
		  Move.w D6,4(A1) ;Lower word
		  Swap D6
		  Move.w D6,(A1) ;Upper word
		  
		  Rts
		  
PosLowerByte equ 3
CtrlWord equ 6

;D0 = X
;D1 = Pointer to SPR6
;D2 = Pointer to SPR7
SetPCloud
      Move.l D1,A1
      Move.l D2,A2

      BTST #0,D0
      BNE SetPCloud_LowerBit

      ;Clear the lower bit of the ctrl words
      And.w #$FFFE,CtrlWord(A1)
      And.w #$FFFE,CtrlWord(A2)
      Bra SetPCloud_Final    

SetPCloud_LowerBit
      ;Set the lower bit of the control words
      Or.w #$1,CtrlWord(A1)
      Or.w #$1,CtrlWord(A2)

SetPCloud_Final
      LSR #1,D0 
      Move.b D0,D1
      AddQ #8,D1

CalculatePosXMask_PCloud1
      And.w #255,D0 ;Convert to byte ;255      
      Move.b D0,PosLowerByte(A1)

CalculatePosXMask_PCloud2
      And.w #255,D1 ;Convert to byte ;255           
      Move.b D1,PosLowerByte(A2)
      Rts
		
;WaitBlit   Macro

;Waitblit\1:
 ;   tst $DFF002			;for compatibility
  ;  btst #6,$DFF002
   ; beq WaitblitFinished\1

    ;If we get to here, we're still blitting
;    move.w #$8400,$DFF096 ;Blit nasty on to get blit done ASAP
;WaitblitOngoing\1
 ;   btst #6,$DFF002
  ;  bne WaitblitOngoing\1 ;We're STILL blitting
   ; move.w #$400,$DFF096 ;We're done, so Blit nasty off again
	
;WaitblitFinished\1
;		        EndM

WaitBlitFast   Macro

WaitBlitFast\1:

    tst DMACONR(A2)			;for compatibility
    btst #6,DMACONR(A2)	
    beq WaitBlitFastFinished\1

    ;If we get to here, we're still blitting
    move.w #$8400,DMACON(A2) ;Blit nasty on to get blit done ASAP
WaitBlitFastOngoing\1
    btst #6,DMACONR(A2)
    bne WaitBlitFastOngoing\1 ;We're STILL blitting
    move.w #$400,DMACON(A2) ;We're done, so Blit nasty off again
	
WaitBlitFastFinished\1
		        EndM

;272 -> 236
DoMaskTileBlit
    lea CustomBase,A2
    WaitBlitFast DoMaskTileBlit

    Move.l #$0fca0000,BLTCON0(A2) ;Masked copy
    Move.w D4,BLTCMOD(A2)

    Move.l D0,BLTBPTH(A2) ;Image - B
    Move.l D1,BLTCPTH(A2)  ;Destination - C
    Move.l D3,BLTAPTH(A2)  ;Mask - A ;d0
    Move.l D1,BLTDPTH(A2)  ;Destination - D
    Move.w D2,BLTSIZE(A2) ;Start Blit
    RTS

;  name "DoBlockScroll","source.l,dest.l,ScreenModulo.w,size.w"  

;DoBlockScroll
;    lea CustomBase,A2
;    WaitBlitFast DoMaskTileBlit
;    Move.l #$09f00000,BLTCON0(A2) ;A->D unmasked
;    Move.l D0,BLTAPTH(A2) ;Source
;    Move.l D1,BLTDPTH(A2) ;Destination
;    Move.w #0,BLTAMOD(A2) ;Source modulus
;    Move.w D2,BLTDMOD(A2) ;Destination modulus
;    Move.w D3,BLTSIZE(A2) ;Start Blit
;    RTS

;D0 should equal B, D1 should equal C, D2 should equal A, D3 should equal D
;#BLTBPTH = $4c;($DFF04C - BlitterBase)
;#BLTCPTH = $48;($DFF048 - BlitterBase)
;#BLTAPTH = $50;($DFF050 - BlitterBase)
;#BLTDPTH = $54;($DFF054 - BlitterBase)

;276 -> 224
DoBlockBlit
    Lea CustomBase,A2
    WaitBlitFast DoBlockBlit
    Move.l #$0fca0000,BLTCON0(A2) ;Masked copy
    Move.w #0,BLTCMOD(A2) ;c modulo=bytes to skip between screen lines

    Move.l D0,BLTBPTH(A2) ;Image - B
    Move.l D4,BLTCPTH(A2)  ;Destination - C
    Move.l D3,BLTAPTH(A2)  ;Mask - A
    Move.l D1,BLTDPTH(A2) ;Destination - D
    Move.w D2,BLTSIZE(A2)  ;Start Blit
    RTS
		

;744/864 - 552/554
;D0 = Position
;D1 = Minterm (FCA)
DoMaskBlit2
  Move.l #12345678,A0
  Lea CustomBase,A2
  And.w #$f,D0 ;And.l

  BEQ FastBlit
  Ror.w #4,D0

  ;Todo - maybe move to the memory registers ahead of the blit wait..?
  WaitBlitFast DoMaskBlit2
  Move.w D0,BLTCON1(A2)
  Or.w D1,D0
  Move.w D0,BLTCON0(A2) ;Masked copy
  Move.l #$ffff0000,BLTAFWM(A2) ;First and last word mask

  Move.w (A0),BLTCMOD(A2) ;Source C 60
  Move.w (A0)+,BLTDMOD(A2) ;Destination D 66
  Move.w (A0),BLTBMOD(A2) ;Source B 4c
  Move.w (A0)+,BLTAMOD(A2) ;Source A 64

  Move.l (A0)+,BLTBPTH(A2) ;Image b ;4C
  Move.l (A0)+,BLTAPTH(A2) ;Mask - A ;50
  Move.l (A0),BLTCPTH(A2) ;Destination - C $48

  Add.w #BLTDPTH,A2
  Move.l (A0)+,(A2)+ ;Destination - D BLTDPTH = 54
  Move.w (A0),(A2) ;StartBlit = 58
  RTS
  
;BLTCON0 equ $40;($DFF040 - BlitterBase)
;BLTCON1 equ $42;($DFF042 - BlitterBase)
;BLTAFWM equ $44;($DFF044 - BlitterBase)

;BLTAMOD equ $64;($DFF064 - BlitterBase)
;BLTBMOD equ $62;($DFF062 - BlitterBase)
;BLTCMOD equ $60;($DFF060 - BlitterBase)
;BLTDMOD equ $66;($DFF066 - BlitterBase)

;BLTAPTH equ $50;($DFF050 - BlitterBase)
;BLTBPTH equ $4c;($DFF04C - BlitterBase)
;BLTCPTH equ $48;($DFF048 - BlitterBase)
;BLTDPTH equ $54;($DFF054 - BlitterBase)

;BLTSIZE equ $58;($DFF058 - BlitterBase)

DoMaskBlit2FirstWordMask
  Move.l #12345678,A0
  Lea CustomBase,A2
  And.w #$f,D0 ;And.l
  Ror.w #4,D0

  ;Todo - maybe move to the memory registers ahead of the blit wait..?
  WaitBlitFast DoMaskBlit2FirstWordMask
  Move.w D0,BLTCON1(A2)
  Or.w D1,D0
  Move.w D0,BLTCON0(A2) ;Masked copy
  Move.l D2,BLTAFWM(A2) ;First and last word mask

  Move.w (A0),BLTCMOD(A2) ;Source C 60
  Move.w (A0)+,BLTDMOD(A2) ;Destination D 66
  Move.w (A0),BLTBMOD(A2) ;Source B 4c
  Move.w (A0)+,BLTAMOD(A2) ;Source A 64

  Move.l (A0)+,BLTBPTH(A2) ;Image b ;4C
  Move.l (A0)+,BLTAPTH(A2) ;Mask - A ;50
  Move.l (A0),BLTCPTH(A2) ;Destination - C $48

  Add.w #BLTDPTH,A2
  Move.l (A0)+,(A2)+ ;Destination - D BLTDPTH = 54
  Move.w (A0),(A2) ;StartBlit = 58
  RTS

;D0 = Position
;D1 = Minterm
;D2 = First Word Mask
;D3 = Last Word Mask
DoBarBlit
  Move.l #12345678,A0
  Lea CustomBase,A2
  And.w #$f,D0 ;And.l
  Ror.w #4,D0

  ;Todo - maybe move to the memory registers ahead of the blit wait..?
  WaitBlitFast DoBarBlit
  Move.w D0,BLTCON1(A2)
  Or.w D1,D0
  Move.w D0,BLTCON0(A2) ;Masked copy
  Move.w D2,BLTAFWM(A2) ;First word mask
  Move.w D3,BLTALWM(A2) ;Last word mask

  Move.w (A0),BLTCMOD(A2) ;Source C 60
  Move.w (A0)+,BLTDMOD(A2) ;Destination D 66
  Move.w (A0),BLTBMOD(A2) ;Source B 4c
  Move.w (A0)+,BLTAMOD(A2) ;Source A 64

  Move.l (A0)+,BLTBPTH(A2) ;Image b ;4C
  Move.l (A0)+,BLTAPTH(A2) ;Mask - A ;50
  Move.l (A0),BLTCPTH(A2) ;Destination - C $48

  Add.w #BLTDPTH,A2
  Move.l (A0)+,(A2)+ ;Destination - D BLTDPTH = 54
  Move.w (A0),(A2) ;StartBlit = 58
  RTS


;412-416
FastBlit

  ;Fix the BLTSIZE
  Move.w 16(A0),D2
  Subq.w #1,D2

  ;Fix the Screen modulo
  Move.w (A0)+,D0
  Addq.w #2,D0

  ;Fix the Bob modulo
  Move.w (A0)+,D4
  Addq.w #2,D4

  WaitBlitFast DoMaskBlit2B
  Swap D1
  Move.l D1,BLTCON0(A2) ;Masked copy
  MoveQ #-1,D1
  Move.l D1,BLTAFWM(A2)

  Move.w D0,BLTCMOD(A2) ;Source C
  Move.w D0,BLTDMOD(A2) ;Destination D
  Move.w D4,BLTBMOD(A2) ;Source b
  Move.w D4,BLTAMOD(A2) ;Source A

  Move.l (A0)+,BLTBPTH(A2) ;Image b
  Move.l (A0)+,BLTAPTH(A2) ;Mask - A
  Move.l (A0),BLTCPTH(A2) ;Destination - C
  Move.l (A0),BLTDPTH(A2) ;Destination - D
  Move.w D2,BLTSIZE(A2) ;StartBlit
  RTS

CalculateCtrl_MP
; D0        D1              D2   D3   D4         D5
; EndWords, IV\HighResNudge,*D\X,*D\Y,BaseHeight,*S\SpriteData\_nextoff - IV\Project\ControlWordSize

  ;Add the start Y to the height
  Add.w D3,D4

  ;We only want the first bit of X
  And.w #1,D2
  Or.w D2,D1

  ;((SEndY LSR 7) & 2)
  ;We don't need to use D2 anymore, used above
  Move.w D4,D2
  LSR.w #7,D2
  And.w #2,D2
  Or.w D2,D1

  ;((*D\Y LSR 6) & 4)
  LSR.w #6,D3
  And.w #4,D3
  Or.w D3,D1

  ;((SEndY) & $ff) lsl 8
  LSL.w #8,D4
  Or.w D4,D1

  Move.l D0,A0
  Move.w D1,(A0)
  Add.l D5,D0
  RTS

;UpperCtrl = ((SEndY) & $ff) lsl 8
;LowerCtrl = ((*D\Y LSR 6) & 4) | ((SEndY LSR 7) & 2) | IV\HighResNudge
;SE_PokeW EndWords, UpperCtrl | LowerCtrl | (*D\X & 1)
CalculatePos_MP

  ;Move X one to the right
  LSR.w #1,D1

CalculatePosXMask
  And.w #255,D1 ;Convert to byte ;255

  ;Move Y to the left, it's the upper byte
  LSL.w #8,D2
  Or.w D2,D1

  Move.l D0,A0
  Move.w D1,(A0)
  Add.l D3,D0
  RTS

;D0 = MapAddress	
;D1 = Number of tiles
;D2 = Address base
ProcessMapTiles
	Move.l D0,A0
	SubQ.l #1,D1

ProcessMapTilesStart

;If it's zero, don't adjust
  Tst.l (A0)
  BEQ TileEmpty

  Add.l D2,(A0)+
	SubQ.l #1,D1
	BNE ProcessMapTilesStart
	RTS

TileEmpty
  AddQ.l #4,A0
	SubQ.l #1,D1
	BNE ProcessMapTilesStart
	RTS


; VBR Hack by phx
; https://eab.abime.net/showpost.php?p=1516508&postcount=7
;----- Get VBR 68010+ ---
        mc68010
final_getvbr:
        movec   vbr,a0
        rte
        mc68000

;Waits for vertical blank by just waiting until the current scanline is previous than the last scanline
WaitVertB
	move.l	#$0001ff00,d2
	move.l	$DFF004,d0
	and.l	d2,d0

waitvertbloop

	move.l	d0,d1
	move.l	$DFF004,d0
	and.l	d2,d0
	cmp.l	d1,d0
	bcc	waitvertbloop
  RTS  

  ;No MegaDrive/NeoGeo game should have this string in it, this is just to make sure this library isn't included
  dc.b ".library",0
  even
  