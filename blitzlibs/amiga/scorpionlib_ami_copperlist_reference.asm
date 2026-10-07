
;Cut down copper list builder, taken from the decompiled BB2 displaylib (displaylib.ab3)
;Only the parts Scorpion uses are kept - InitCopList, CreateDisplay and DisplayAdjust
;Always lores, no HAM, no dual playfield, no SpriteMode, no genlock

;Copper lists are no longer Blitz objects, every function takes a pointer to one of these
;It must start zeroed, a non-zero size means there is chip memory to free

;NEWTYPE .coplist
;  size.l      ;0 = not initialised
;  coppos.l    ;4 location in chipmem
;  colors.l    ;8
;  sprites.l   ;12
;  bpcons.l    ;16
;  bplanes.l   ;20
;  dot.l       ;24
;  customs.l   ;28
;  dob.l       ;32
;  bot.w       ;36 ypos of last custom wait before the bottom of the display
;  ypos.w      ;38
;  height.w    ;40
;  setup.w     ;42 lines taken for setup
;  numcols.w   ;44
;End NEWTYPE

CopList_Size equ 0
CopList_CopPos equ 4
CopList_Colors equ 8
CopList_Sprites equ 12
CopList_BPCons equ 16
CopList_BPlanes equ 20
CopList_Dot equ 24
CopList_Customs equ 28
CopList_Dob equ 32
CopList_Bot equ 36
CopList_YPos equ 38
CopList_Height equ 40
CopList_Setup equ 42
CopList_NumCols equ 44

;Value offsets of the display control moves from the start of the list
CopList_DIWSTRT equ 14
CopList_DIWSTOP equ 18
CopList_DDFSTRT equ 22
CopList_DDFSTOP equ 26

;Longs always allocated on top of bitplanes, sprites, colors and customs. Same as displaylib
CopList_StdCops equ 44

CopNop equ $01fe0000
CopWaitPALWrap equ $ffe1fffe
CopEnd equ $fffffffe
CopDMAOn equ $00968120
CopDMAOff equ $00960120
CopJump2 equ $008a0001

MemChipClear equ $10002

;switchlib (always linked on Amiga) - remembers the display for BLITZ/QAMIGA switching
Blitz_SetCopList equ $0000C205

;D0 = Pointer to coplist
;D1 = YPos
;D2 = Height
;D3 = Type (bitplanes | $10 smooth | $40 EHB | $1000/$3000 fetch mode | $10000 AGA colors)
;D4 = Sprites
;D5 = Colors
;D6 = Customs, negative for -n moves after a wait on every line
SE_InitCopList

  Move.l D0,A3
  Move.w D1,CopList_YPos(A3)
  Move.w D1,CopList_Bot(A3)
  Move.w D2,CopList_Height(A3)
  Move.w D5,CopList_NumCols(A3)

  ;Free the old list if there is one
  Movem.l D1-D6,-(A7)
  Bsr SE_FreeCopList
  Movem.l (A7)+,D1-D6

  ;D7 = bytes per custom line, zero if the customs are one block
  MoveQ #0,D7
  Tst.w D6
  Bpl InitCopList_CustomBlock

  ;A wait plus n moves on every line except the last
  Neg.w D6
  AddQ.w #1,D6
  Move.w D6,D7
  Lsl.w #2,D7
  Move.w D2,D0
  SubQ.w #1,D0
  Mulu D0,D6

InitCopList_CustomBlock

  ;AGA colors need two moves per color plus two bank selects per 32 colors
  Btst #16,D3
  Beq InitCopList_ColorsCounted
  Tst.w D5
  Beq InitCopList_ColorsCounted
  Move.w D5,D0
  SubQ.w #1,D0
  Lsr.w #5,D0
  AddQ.w #1,D0
  Add.w D5,D5
  Add.w D0,D5
  Add.w D0,D5

InitCopList_ColorsCounted

  ;Size in longs = bitplanes*2 + sprites*4 + color moves + customs + standard
  MoveQ #15,D0
  And.w D3,D0
  Add.w D0,D0
  Move.w D4,D1
  Lsl.w #2,D1
  Add.w D1,D0
  Add.w D5,D0
  Add.w D6,D0
  Add.w #CopList_StdCops,D0
  Lsl.l #2,D0
  Move.l D0,CopList_Size(A3)

  Move.w D7,-(A7) ;Custom line size is needed again after the alloc
  Movem.l D3-D6,-(A7)
  Move.l #MemChipClear,D1
  ALibJsr Blitz_AllocMem
  Movem.l (A7)+,D3-D6

  Move.l D0,CopList_CopPos(A3)
  Move.l D0,A1
  Move.l #CopNop,D7

  ;CreateDisplay puts the wait for the top of this list here
  Move.l D7,(A1)+
  Move.l D7,(A1)+

  ;Display control moves, from the table row for this fetch mode
  Move.w D3,D0
  Lsr.w #7,D0
  And.w #$60,D0
  Lea CopList_Table(pc),A0
  Add.w D0,A0
  MoveQ #5,D0

InitCopList_CtrlLoop
  Move.l (A0)+,(A1)+
  Dbra D0,InitCopList_CtrlLoop

  ;Smooth scrolling fetches one more word earlier
  Btst #4,D3
  Beq InitCopList_NotSmooth
  Move.w (A0),D0
  Sub.w D0,-10(A1) ;DDFSTRT

InitCopList_NotSmooth

  ;1-7 bitplanes go into BPLCON0 bits 12-14, 8 is bit 4
  MoveQ #15,D0
  And.w D3,D0
  Ror.w #4,D0
  Bpl InitCopList_BPU
  Move.w #$10,D0

InitCopList_BPU
  Or.w D0,-2(A1) ;BPLCON0

  ;Colors, filled in later by the engine
  Move.l A1,CopList_Colors(A3)
  Bra InitCopList_ColorsNext

InitCopList_ColorsLoop
  Move.l D7,(A1)+

InitCopList_ColorsNext
  Dbra D5,InitCopList_ColorsLoop

  ;Sprites, POS/CTL/PTH/PTL for each channel
  Move.l A1,CopList_Sprites(A3)
  Move.l #$01400000,D0
  Move.l #$01420000,D1
  Move.l #$01200000,D2
  Move.l #$01220000,D5
  Bra InitCopList_SpritesNext

InitCopList_SpritesLoop
  Movem.l D0-D2/D5,(A1)
  Lea 16(A1),A1
  Add.l #$80000,D0
  Add.l #$80000,D1
  Add.l #$40000,D2
  Add.l #$40000,D5

InitCopList_SpritesNext
  Dbra D4,InitCopList_SpritesLoop

  ;BPLCON2/3/4/1 and the modulos
  Move.l A1,CopList_BPCons(A3)
  Lea CopList_BPCon(pc),A0
  MoveQ #5,D0

InitCopList_BPConLoop
  Move.l (A0)+,(A1)+
  Dbra D0,InitCopList_BPConLoop

  ;EHB needs KILLEHB cleared in BPLCON2
  Btst #6,D3
  Beq InitCopList_NotEHB
  And.w #$FDFF,-22(A1)

InitCopList_NotEHB

  ;Bitplane pointers
  Move.l A1,CopList_BPlanes(A3)
  Move.l #$00e00000,D0
  MoveQ #15,D1
  And.w D3,D1
  Add.w D1,D1
  Bra InitCopList_BPlanesNext

InitCopList_BPlanesLoop
  Move.l D0,(A1)+
  Add.l #$20000,D0

InitCopList_BPlanesNext
  Dbra D1,InitCopList_BPlanesLoop

  ;Lines the copper needs for everything so far, 222 bytes per line
  Move.l A1,D0
  Sub.l CopList_CopPos(A3),D0
  Divu #222,D0
  AddQ.w #3,D0
  Move.w D0,CopList_Setup(A3)

  ;CreateDisplay puts the wait for the first display line here
  Move.l A1,CopList_Dot(A3)
  Move.l D7,(A1)+
  Move.l D7,(A1)+
  Move.l #CopDMAOn,(A1)+

  Move.l A1,CopList_Customs(A3)
  Move.w (A7)+,D2
  Bne InitCopList_CustomLines

  Bra InitCopList_CustomsNext

InitCopList_CustomsLoop
  Move.l D7,(A1)+

InitCopList_CustomsNext
  Dbra D6,InitCopList_CustomsLoop
  Bra InitCopList_Dob

InitCopList_CustomLines

  ;D2 = moves per line - 1
  Lsr.w #2,D2
  SubQ.w #2,D2
  Move.w CopList_YPos(A3),D0
  SubQ.w #1,D0
  Move.w CopList_Height(A3),D1
  SubQ.w #2,D1
  Bmi InitCopList_Dob

InitCopList_LineLoop
  Move.b D0,(A1)+
  Move.b #$e1,(A1)+
  Move.w #$fffe,(A1)+
  Move.w D2,D3

InitCopList_LineMoves
  Move.l D7,(A1)+
  Dbra D3,InitCopList_LineMoves

  AddQ.w #1,D0
  Dbra D1,InitCopList_LineLoop

  SubQ.w #1,D0
  Move.w D0,CopList_Bot(A3)

InitCopList_Dob

  ;CreateDisplay puts the wait for the bottom here, then the end or the jump to the next list
  Move.l A1,CopList_Dob(A3)
  Move.l D7,(A1)+
  Move.l D7,(A1)+
  Move.l #CopDMAOff,(A1)
  RTS

;A3 = Pointer to coplist
SE_FreeCopList
  Move.l CopList_Size(A3),D0
  Beq SE_FreeCopList_Done
  Move.l CopList_CopPos(A3),A1
  ALibJsr Blitz_FreeMem
  Clr.l CopList_Size(A3)

SE_FreeCopList_Done
  RTS

;FMODE, DIWSTRT, DIWSTOP, DDFSTRT, DDFSTOP, BPLCON0 then the DDFSTRT adjust for smooth scrolling
;One row per fetch mode, lores only
CopList_Table
  Dc.w $1fc,0,$8e,$0081,$90,$70c1,$92,$38,$94,$d0,$100,$201,8,0,0,0
  Dc.w $1fc,1,$8e,$0081,$90,$70c1,$92,$38,$94,$c0,$100,$201,16,0,0,0
  Dc.w $1fc,2,$8e,$0081,$90,$70c1,$92,$38,$94,$c0,$100,$201,16,0,0,0
  Dc.w $1fc,3,$8e,$0081,$90,$70c1,$92,$38,$94,$b8,$100,$201,32,0,0,0

CopList_BPCon
  Dc.w $104,$224,$106,$c00,$10c,$11,$102,0,$108,0,$10a,0

;D0 = First coplist
;D1 = Second coplist, or zero
;D2 = Third coplist, or zero
SE_CreateDisplay

  Movem.l D1-D2,-(A7)
  Sub.l A2,A2 ;A2 = where the previous list jumps from, zero for none yet
  MoveQ #0,D7 ;D7 = line of the last wait

  Move.l D0,A3
  Bsr CreateDisplay_Add
  Move.l (A7)+,A3
  Bsr CreateDisplay_Add
  Move.l (A7)+,A3
  Bsr CreateDisplay_Add

  Move.l A2,D0
  Beq CreateDisplay_Done
  Move.l #CopEnd,(A2)

  Move.l D6,A0
  Lea CustomBase,A2
  WaitBlitFast CreateDisplay
  Move.l A0,$80(A2) ;COP1LC
  ALibJsr Blitz_SetCopList

CreateDisplay_Done
  RTS

;A3 = coplist, or zero to skip
CreateDisplay_Add
  Move.l A3,D0
  Beq CreateDisplay_Skip

  Move.l CopList_CopPos(A3),D0
  Move.l A2,D1
  Bne CreateDisplay_Link

  ;First list goes in COP1LC
  Move.l D0,D6
  Bra CreateDisplay_Waits

CreateDisplay_Link
  ;Jump from the end of the previous list to this one through COP2LC
  Move.w #$86,(A2)+
  Move.w D0,(A2)+
  Move.w #$84,(A2)+
  Swap D0
  Move.w D0,(A2)+
  Move.l #CopJump2,(A2)

CreateDisplay_Waits

  ;Top of the list
  Move.w CopList_YPos(A3),D1
  Sub.w CopList_Setup(A3),D1
  Move.l CopList_CopPos(A3),A1
  Move.b #$e1,D2
  Move.w D7,D3
  Bsr CreateDisplay_Wait
  Move.w D1,D7

  ;First display line
  Add.w CopList_Setup(A3),D1
  Move.l CopList_Dot(A3),A1
  Move.b #$01,D2
  Move.w D7,D3
  Bsr CreateDisplay_Wait

  ;Bottom of the display
  Add.w CopList_Height(A3),D1
  Move.l CopList_Dob(A3),A1
  Move.w CopList_Bot(A3),D3
  Bsr CreateDisplay_Wait
  Move.w D1,D7

  ;Skip DMA off, the next list links from here
  Lea 4(A1),A2

CreateDisplay_Skip
  RTS

;A1 = Destination, D1 = Line, D2 = Horizontal position, D3 = Line of the previous wait
;Past line 255 the PAL wrap wait goes in first, unless the previous wait was past it already
CreateDisplay_Wait
  Move.l #CopNop,D0
  Btst #8,D1
  Beq CreateDisplay_NoWrap
  Btst #8,D3
  Bne CreateDisplay_NoWrap
  Move.l #CopWaitPALWrap,D0

CreateDisplay_NoWrap
  Move.l D0,(A1)+
  Move.b D1,(A1)+
  Move.b D2,(A1)+
  Move.w #$fffe,(A1)+
  RTS

;D0 = Pointer to coplist
;D1 = DDFSTRT adjust
;D2 = DDFSTOP adjust
;D3 = DIWSTRT adjust
;D4 = DIWSTOP adjust
SE_DisplayAdjust
  Move.l D0,A0
  Move.l CopList_CopPos(A0),A0
  Add.w D1,CopList_DDFSTRT(A0)
  Add.w D2,CopList_DDFSTOP(A0)
  Add.w D3,CopList_DIWSTRT(A0)
  Add.w D4,CopList_DIWSTOP(A0)
  RTS
