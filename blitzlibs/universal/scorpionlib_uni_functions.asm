BlockDataSize equ 68 ;Need to round this down to 64
BlockSlope equ 3

DoIllegal
  ILLEGAL

Finish
	RTS

Call
  Move.l D0,A0
  Jmp (A0) ;This will RTS back to the original place

QMin
    Cmp.l d0,d1
    BGE Finish
    Move.l d1,d0
	RTS

QMax
    Cmp.l d0,d1
    BLE Finish
    Move.l d1,d0
	RTS

SE_PeekL
  Move.l d0,a0
  Move.l (a0),d0
  RTS

SE_PeekW
  Move.l d0,a0
  MoveQ #0,d0
  Move.w (a0),d0
  RTS  

SE_PeekB
  Move.l d0,a0
  MoveQ #0,d0
  Move.b (a0),d0
  RTS    

SE_PokeL
  Move.l d0,a0
  Move.l d1,(a0)
  RTS

SE_PokeW
  Move.l d0,a0
  Move.w d1,(a0)
  RTS

SE_PokeB
  Move.l d0,a0
  Move.b d1,(a0)
  RTS  

;24 Bit peek
;Should be compatible with 68000
PeekBBB
  BCLR.l #0,D0 ;Clear the first bit to force to even
  BNE Peek24Odd

  ;This is an even address
  Move.l D0,A0
  Move.l (A0),D0
  LSR.l #8,D0
  RTS

  ;This is an odd address
Peek24Odd
  Move.l D0,A0
  Move.l (A0),D0
  And.l #$00FFFFFF,D0
  RTS
		
GetTileASMAddress
		;Part 1 - Get the Y tile lookup address		
  	;Part 1 - Get the Y tile lookup address		
    Add.w D1,D1
    Add.w D1,D1 ;Convert to LONG address
    Move.l D3,A0
    Add.l 0(A0,D1),D0 ;Add the Y address to the X Address

		;Part 2 - Add the X and Y coordinates together
		Asl.l #2,D0 ;Multiply by 4	
		Add.l D2,D0 ;Add the absolute map tile data address
		rts
		
GetTileASMAddressLine
		;Part 1 - Get the Y tile lookup address		
		Add.w D0,D0
		Add.w D0,D0 ;Convert to LONG address
		Move.l D2,A0
		Move.l (A0,D0),D0

		;Part 2 - Add the X and Y coordinates together
		Asl.l #2,D0;Multiply by 4		
		Add.l D1,D0 ;Add the absolute map tile data address
		rts		
		
GetTileASM
		Add.w D1,D1
		Add.w D1,D1 ;Convert to LONG address
		
		Move.l D3,A0
		Move.l (A0,D1),D1
		
		;Part 2 - Add the X and Y coordinates together
		Add.l D1,D0 ;Add the Y address to the X Address
		Asl.l #2,D0;Multiply by 4
		
		Add.l D0,D2
		Move.l D2,A0
		Move.l (A0),D0		
		rts


GetTileASM_B
		Add.w D1,D1
		Add.w D1,D1 ;Convert to LONG address
		
		Move.l D3,A0
		Move.l (A0,D1),D1
	
		;Part 2 - Add the X and Y coordinates together
		Add.l D1,D0 ;Add the Y address to the X Address
		Asl.l #2,D0;Multiply by 4
		
		Add.l D0,D2
		Move.l D2,A0
		Move.b (A0),D0		
		rts


Copy_VarData
  Move.l D0,A0
  Move.l D1,A1
  Move.l (A0)+,(A1)+ ;Var1-2
  Move.l (A0)+,(A1)+ ;Var3-4
  Move.l (A0)+,(A1)+ ;Var5-6
  Move.l (A0)+,(A1)+ ;Controller/OriginX
  Move.w (A0)+,(A1)+ ;OriginY
  Rts

Copy_VarData_NoOrigin
  Move.l D0,A0
  Move.l D1,A1
  Move.l (A0)+,(A1)+ ;Var1-2
  Move.l (A0)+,(A1)+ ;Var3-4
  Move.l (A0)+,(A1)+ ;Var5-6
  Move.w (A0)+,(A1)+ ;Controller
  Rts

DeleteNewType
  Move.l D1,A0
  ASR #2,D0
  Subq #1,D0
  MoveQ #0,D3
DeleteNewTypeLoop
  Move.l D3,(A0)+
  Dbra D0,DeleteNewTypeLoop
  RTS

CopyNewType
  Move.l D1,A0
  Move.l D2,A1
  ASR #2,D0
  Subq #1,D0
CopyNewTypeLoop
  Move.l (A0)+,(A1)+
  Dbra D0,CopyNewTypeLoop
  RTS  

GetLookDir
    TST.l D0
    BEQ LookingCenter
    BGT LookingRight

LookingLeft
    TST.l D1
    BEQ ExitLeft
    BLT ExitUpLeft
    BRA ExitDownLeft

LookingCenter
    TST.l D1
    BEQ ExitCenter
    BLT ExitUp
    BRA ExitDown

LookingRight
    TST.l D1
    BEQ ExitRight
    BLT ExitUpRight
    BRA ExitDownRight

ExitUpLeft
    MoveQ #0,D0
    RTS

ExitUp
    MoveQ #1,D0
    RTS

ExitUpRight
    MoveQ #2,D0
    RTS

ExitLeft
    MoveQ #3,D0
    RTS

ExitCenter
    MoveQ #4,D0
    RTS

ExitRight
    MoveQ #5,D0
    RTS

ExitDownLeft
    MoveQ #6,D0
    RTS

ExitDown
    MoveQ #7,D0
    RTS

ExitDownRight
    MoveQ #8,D0
    RTS

WMin
  Cmp.l d0,d1
  BGE MinExit
  Move.l d1,d0
MinExit
  RTS

WMax
    Cmp.l d0,d1
    BLE MaxExit
    Move.l d1,d0
MaxExit
	RTS

GetTileB_Address
  	;Part 1 - Get the Y tile lookup address		
    Add.w D1,D1
    Add.w D1,D1 ;Convert to LONG address
    Move.l D3,A0
    Add.l 0(A0,D1),D0 ;Add the Y address to the X Address
    Add.l D2,D0 ;Add the absolute map tile data address
    RTS

GetTileB
  	;Part 1 - Get the Y tile lookup address		
    Add.w D1,D1
    Add.w D1,D1 ;Convert to LONG address
    Move.l D3,A0
    Add.l 0(A0,D1),D0 ;Add the Y address to the X Address
    
    Add.l D2,D0 ;Add the absolute map tile data address
    Move.l D0,A0 ;Put it into
  	MoveQ #0,D0 ;Clear out D0
    Move.b (A0),D0 ;Move that back into D0
    RTS

GetMapBlock ;Same as TileB, but returns the actual block data
  	;Part 1 - Get the Y tile lookup address		
    Add.w D1,D1
    Add.w D1,D1 ;Convert to LONG address
    Move.l D3,A0
    Add.l 0(A0,D1),D0 ;Add the Y address to the X Address

    Add.l D2,D0 ;Add the absolute map tile data address
    Move.l D0,A0 ;Put it into
  	MoveQ #0,D0 ;Clear out D0
    Move.b (A0),D0 ;Move that back into D0
    Mulu #BlockDataSize,D0 ;Get the block offset
    Add.l D4,D0 ;Add the address of the block table
    RTS

GetMapSlope ;Same as GetMapBlock, but only returns the block data if it's an actual slope
  	;Part 1 - Get the Y tile lookup address		
    Add.w D1,D1
    Add.w D1,D1 ;Convert to LONG address
    Move.l D3,A0
    Add.l 0(A0,D1),D0 ;Add the Y address to the X Address

    Add.l D2,D0 ;Add the absolute map tile data address
    Move.l D0,A0 ;Put it into
  	MoveQ #0,D0 ;Clear out D0
    Move.b (A0),D0 ;Move that back into D0
    Mulu #BlockDataSize,D0 ;Get the block offset
    Add.l D4,D0 ;Add the address of the block table

    ;Final check - if it's not a slope, just return the 0th entry in the table
    Move.l D0,A0
    Tst.b BlockSlope(A0)
    BEQ GetMapSlopeFailed
    RTS

GetMapSlopeFailed
    Move.l D4,D0
    RTS

SetTileB
  	;Part 1 - Get the Y tile lookup address		
    Add.w D1,D1
    Add.w D1,D1 ;Convert to LONG address
    Move.l D3,A0
    Add.l 0(A0,D1),D0 ;Add the Y address to the X Address

    Add.l D2,D0 ;Add the absolute map tile data address
    Move.l D0,A0

    Move.b (A0),D0 ;Move the old tile to D0
    Move.b D4,(A0) ;Move new data to memory
    RTS

PushTileToQueue
	Move.l D0,A0 ;Address of counter
	Move.l D0,A1 ;Use this for the entry address

	Move.w (A0),D3 ;Get the current count
	Add.w D3,D3 ;mUltiply by 4
  	Add.w D3,D3

	Addq.w #2,D3 ;Add two
	Add.w D3,A1 ;This should now be at the pointer address

	Move.w D1,(A1)+ ;Set X
	Move.w D2,(A1) ;Set Y

	Addq.w #1,(A0) 	;Bump the counter count
	RTS

;+0 = X
;+2 = Y
;+4 = Width
;+6 = Height
RectOverlap
  Move.l D0,A0
  Move.l D1,A1

;  rect0.x < rect1.x + rect1.w &&
  Move.w (A0),D0
  Move.w (A1),d1
  Add.w 4(A1),d1
  Cmp.w D1,D0
  BGT RectOverlapFalse

; rect0.x + rect0.w > rect1.x &&
  Add.w 4(A0),D0
  Move.w (A1),D1
  Cmp.w D1,D0
  BLT RectOverlapFalse

;    rect0.y < rect1.y + rect1.h &&
  Move.w 2(A0),D0
  Move.w 2(A1),d1
  Add.w 6(A1),d1
  Cmp.w D1,D0
  BGT RectOverlapFalse

;    rect1.h + rect1.y > rect2.y
  Add.w 6(A0),D0
  Move.w 2(A1),D1
  Cmp.w D1,D0
  BLT RectOverlapFalse

  MoveQ #-1,D0
  RTS

RectOverlapFalse
  MoveQ #0,D0
  RTS


;+0 = Left
;+2 = Top
;+4 = Right
;+6 = Bottom
RectOverlap2
  Move.l D0,A0
  Move.l D1,A1

;  rect0.l < rect1.r &&
  Move.w (A0),D0
  Move.w 4(A1),d1
  Cmp.w D0,D1
  BGT RectOverlapFalse

; rect0.r > rect1.l &&
  Move.w 4(A0),D0
  Move.w (A1),d1
  Cmp.w D1,D0
  BLT RectOverlapFalse

;    rect0.t < rect1.b &&
  Move.w 2(A0),D0
  Move.w 6(A1),d1
  Cmp.w D1,D0
  BGT RectOverlapFalse

;    rect0.b > rect1.t
  Move.w 6(A0),D0
  Move.w 2(A1),d1
  Cmp.w D1,D0
  BLT RectOverlapFalse

  MoveQ #-1,D0
  RTS


SE_Int
  Swap.w D0 ;Put the upper 16 bits into the lower 16 bits
  RTS

;Copied from Amiblitz 3 reverse engineer
SE_QWrap
  CMP.l d1,d0
  BGE SE_QWrap_Skip1
    SUB.l d1,d0
    ADD.l d2,d0
    BRA SE_QWrap ;Repeat until done

SE_QWrap_Skip1:
  CMP.l d2,d0
  BLT SE_QWrap_Skip2
    SUB.l d2,d0
    ADD.l d1,d0
    BRA SE_QWrap ;Repeat until done

SE_QWrap_Skip2:
  RTS

SE_QAbs:
  NEG.l d0
  BPL SE_QAbs_Skip
  NEG.l d0
SE_QAbs_Skip:
  RTS

SE_QSgn
  TST.l D0
  BEQ SE_QSgn_Skip ;If it equals zero, just return the value
  BLT SE_QSgn_Neg ;Negative value
  MoveQ #1,D0
  RTS

SE_QSgn_Neg
  MoveQ #-1,D0

SE_QSgn_Skip
  RTS

SE_QLimit:
  CMP.l d1,d0
  BGE SE_QLimit_Skip
    MOVE.l d1,d0
    RTS
SE_QLimit_Skip:
  CMP.l d2,d0
  BLT SE_QLimit_Skip2
  MOVE.l d2,d0
SE_QLimit_Skip2
 RTS

SE_Limit:
  CMP.w d1,d0
  BGE SE_Limit_Skip
    MOVE.w d1,d0
    RTS
SE_Limit_Skip:
  CMP.w d2,d0
  BLT SE_Limit_Skip2
  MOVE.w d2,d0
SE_Limit_Skip2
 RTS

;D0 = Address in memory of the seed
;D1 = The number to multiply by (eg 6 for a 6 sided dice)
SE_XORSHIFT2
  Move.w D1,D7
  JSR SE_XORSHIFT
  Mulu.w D7,D0
  RTS

;https://bumbershootsoft.wordpress.com/2019/02/09/the-xorshift-star-prng-for-three-32-bit-chips/
SE_XORSHIFT
  move.l d0,a0
  movem.l (a0),d0-d1 ;Need a pointer to 8 bytes / 4 words / 2  longwords
  moveq   #12,d3
  moveq   #20,d4
  move.l  d1,d2
  lsr.l   d3,d2
  eor.l   d2,d1
  move.l  d0,d2
  lsl.l   d4,d2
  eor.l   d2,d1
  move.l  d0,d2
  lsr.l   d3,d2
  eor.l   d2,d0
  moveq   #25,d3
  move.l  d0,d2
  lsl.l   d3,d2
  eor.l   d2,d0
  move.l  d1,d2
  lsr.l   #7,d2
  eor.l   d2,d0
  move.l  d1,d2
  lsl.l   d3,d2
  eor.l   d2,d1
  moveq   #27,d3
  move.l  d1,d2
  lsr.l   d3,d2
  eor.l   d2,d1
  move.l  d0,d2
  lsl.l   #5,d2
  eor.l   d2,d1
  move.l  d0,d2
  lsr.l   d3,d2
  eor.l   d2,d0  
;Storing the value back is as easy as it was on ARM.  
  movem.l d0-d1,(a0)
;Using the actual multiply instruction is, as we saw earlier, kind of more trouble than it’s worth when we can only do it 16 bits at a time. So we do a more primitive shift-and-add 64-bit multiplication routine instead.
  move.l  #$2545F491,d2
  move.l  #$4F6CDD1D,d3
  clr.l   d4
  clr.l   d5
  moveq   #63,d6
m64:    
  lsr.l   d0
  roxr.l  d1
  bcc.s   m64_next
  add.l   d3,d5
  addx.l  d2,d4
m64_next:
  lsl.l   d3
  roxl.l  d2
  dbra    d6,m64  
;The return value goes in d0. I suppose I could have made this instruction unnecessary by juggling all the rest of the registers and having this result start out in d0.
  move.l  d4,d0
;Computation accomplished, we restore our non-scratch registers and return.
;Erik - we want to turn this into a fraction, so we swap the words and clear the top bits
  Clr.w d0
  SWAP d0  
  rts

SE_ResetList
  MOVE.l d0,a0
  CLR.l -32(a0)
  RTS

SE_PushItem
  MOVE.l d0,a0
  MOVE.l d1,a1
  MOVE.l (a1),a1
  MOVE.l d1,a2

  MOVE.l a0,(a1)+
  MOVE.l -32(a0),(a1)+
  MOVE.l a1,(a2)
  RTS

SE_PopItem
  MOVE.l d0,a0
  MOVE.l d1,a1
  MOVE.l (a1),a1
  MOVE.l d1,a2

  MOVE.l -(a1),-32(a0)
  SUBQ #4,a1
  MOVE.l a1,(a2)
  RTS

SE_ClearList
  MOVE.l d0,a0
SE_ClearList_Loop
    MOVE.l -28(a0),a1
    MOVE.l (a1),d0
    BEQ SE_ClearList_Done
      BSR unlinkitem
      LEA -16(a0),a2
      BSR linkitem
  BRA SE_ClearList_Loop
SE_ClearList_Done
  CLR.l -32(a0)
  RTS

SE_SortList:
  MOVE d1,d6
  MOVEM.l a4-a6,-(a7)
  MOVE.l d0,a0
  SUB.l #24,d0
  LEA -28(a0),a0
  CMP.l (a0),d0
  BEQ dun
    dopass:
      MOVE.l (a0),a1
      MOVEM.l (a1),a2-a3
      CMP.l a2,d0
      BEQ dun
        MOVE.l (a2),a4
        MOVE.l 8(a1,d6),d1
        MOVEQ #0,d3      ;d3=flag
;        CMP.l d3,a3
        ;NEVER TRAP
;        BNE nxpass
;          TRAP #7

        nxpass:
          MOVE.l 8(a2,d6),d2
          CMP.l d1,d2
          BMI doswap

          MOVE.l d2,d1
          nxitem:
          CMP.l d0,a4
          BEQ dunpass
            MOVE.l a1,a3
            MOVE.l a2,a1
            MOVE.l a4,a2
            MOVE.l (a2),a4
        BRA nxpass
        dunpass:
      MOVE.l a2,d0
    TST d3
    BNE dopass
  dun:
  MOVEM.l (a7)+,a4-a6
  RTS

doswap:
  MOVE.l a2,(a3)
  MOVEM.l a1/a3,(a2)
  MOVE.l a2,4(a1)
  MOVE.l a4,(a1)
  MOVE.l a1,4(a4)
  EXG a1,a2
  MOVEQ #1,d3
  BRA nxitem

SE_KillItem:
  MOVE.l d0,a0
  MOVE.l -32(a0),a1;current item
  MOVE.l 4(a1),-(a7);old previous of current

  MOVE.l a3,-(a7)
  BSR unlinkitem;remove from used...
  LEA -16(a0),a2
  BSR linkitem;add to free

  MOVE.l (a7)+,a3
  MOVE.l (a7)+,a1
  MOVE.l 4(a1),d0
  BNE SE_KillItem_Skip
    MOVE.l d0,a1
SE_KillItem_Skip
  MOVE.l a1,-32(a0)
  RTS

SE_AddItem:                    ; if no current, add at beginning
  MOVE.l d0,a0
  MOVE.l -16(a0),a1 ;next of head node
  MOVE.l (a1),d0
  BNE SE_AddItem_l1

  TST.w -34(a0)
  BEQ SE_AddItem_Done
    BSR increase
    MOVE.l -16(a0),a1
    MOVE.l (a1),d0
SE_AddItem_l1
    MOVE.l a3,-(a7)
    BSR unlinkitem
    MOVE.l -32(a0),d0
    BNE SE_AddItem_Skip
      LEA -28(a0),a2
      MOVE.l a2,d0
SE_AddItem_Skip
    MOVE.l d0,a2
    BSR linkitem
    MOVE.l (a7)+,a3
    MOVE.l a1,-32(a0);now current
    MOVEQ #-1,d0
SE_AddItem_Done
  RTS  

SE_NextItem:
  MOVE.l d0,a0
  MOVE.l -32(a0),d0
  BEQ do_FirstItem2

  MOVE.l d0,a1
  MOVE.l (a1),a1;next of current
  MOVE.l (a1),d0
  BEQ SE_NextItem_Done
    MOVE.l a1,-32(a0)
;    MOVEQ #-1,d0
SE_NextItem_Done
  RTS

SE_PrevItem ; if no current, do last item.
  MOVE.l d0,a0
  MOVE.l -32(a0),d0
  BEQ do_LastItem2

  MOVE.l d0,a1
  MOVE.l 4(a1),a1;previous of current
  MOVE.l 4(a1),d0
  BEQ SE_PrevItem_Done
    MOVE.l a1,-32(a0)
;    MOVEQ #-1,d0
SE_PrevItem_Done
  RTS  

do_FirstItem        ;ok, d0=array base. make current=first
  MOVE.l d0,a0

do_FirstItem2
  MOVE.l -28(a0),a1 ;a1=next of head node
  MOVE.l (a1),d0
  BNE do_FirstItem_skip
    MOVE.l d0,a1
do_FirstItem_skip
  MOVE.l a1,-32(a0)
;  TST.l d0
;  BEQ do_FirstItem_knob
;  MOVEQ #-1,d0
;do_FirstItem_knob
  RTS

do_LastItem:   ; d0=array base. make current=last
  MOVE.l d0,a0

do_LastItem2
  MOVE.l -20(a0),a1 ;a1=prev of tail node
  MOVE.l 4(a1),d0
  BNE do_LastItem2_skip
    MOVE.l d0,a1
do_LastItem2_skip
  MOVE.l a1,-32(a0)
;  TST.l d0
;  BEQ do_LastItem2_knob
;    MOVEQ #-1,d0
;do_LastItem2_knob
  RTS

increase:
;  MOVEM.l a0-a2/a6/d2,-(a7)
;  MOVEM.l (a7)+,a0-a2/a6/d2
  ADDQ.l #4,a7
  RTS

unlinkitem:                    ;item to unlink in a1
  MOVE.l 4(a1),a2
  MOVE.l (a1),a3
  MOVE.l a2,4(a3)
  MOVE.l a3,(a2)
  RTS

linkitem:                      ;link item in a1 after a2
  MOVE.l (a2),a3
  MOVE.l a1,(a2)
  MOVE.l a1,4(a3)
  MOVE.l a2,4(a1)
  MOVE.l a3,(a1)
  RTS

;Throw out the top 16 bits
SE_QFrac
  And.l #$0000FFFF,D0
  RTS

SqrRoot
lsqrt   tst.l d0        (4)     ; skip doing zero.
        beq.s done      (10/8)
        cmp.l #$10000,d0 (14)   ; If it is a longword, use the long routine.
        bhs.s glsqrt    (10/8)
        cmp.w #625,d0   (8)     ; Would the short word routine be quicker?
        bhi.s gsqrt     (10/8)  ; No, use general purpose word routine.
*                               ; Otherwise fall into special routine.
*
*  For speed, we use three exit points.
*  This is cheesy, but this is a speed-optimized subroutine!

*************************************************************************
*                                                                       *
*       Faster Integer Square Root (16 to 8 bit).  For small arguments. *
*                                                                       *
*       (Exact method, not approximate).                                *
*                                                                       *
*       Call with:                                                      *
*               D0.W = Unsigned number.                                 *
*                                                                       *
*       Returns:                                                        *
*               D0.W - SQRT(D0.W)                                       *
*                                                                       *
*       Notes:  Result fits in D0.B, but is valid in word.              *
*               Takes from 72 (d0=1) to 504 cycles (d0=625) cycles      *
*               (including rts).                                        *
*                                                                       *
*       Algorithm supplied by Motorola.                                 *
*                                                                       *
*************************************************************************

* Use the theorem that a perfect square is the sum of the first
* sqrt(arg) number of odd integers.
 
*                       Cycles
        move.w #-1,d1   (8)
qsqrt1  addq.w #2,d1    (4)
        sub.w d1,d0     (4)
        bpl  qsqrt1     (10/8)
        asr.w #1,d1     (8)
        move.w d1,d0    (4)
done    rts             (16)

*************************************************************************
*                                                                       *
*       Integer Square Root (16 to 8 bit).                              *
*                                                                       *
*       (Exact method, not approximate).                                *
*                                                                       *
*       Call with:                                                      *
*               D0.W = Unsigned number.                                 *
*                                                                       *
*       Returns:                                                        *
*               D0.L - SQRT(D0.W)                                       *
*                                                                       *
*       Uses:   D1-D4 as temporaries --                                 *
*               D1 = Error term;                                        *
*               D2 = Running estimate;                                  *
*               D3 = High bracket;                                      *
*               D4 = Loop counter.                                      *
*                                                                       *
*       Notes:  Result fits in D0.B, but is valid in word.              *    
*                                                                       *
*               Takes from 512 to 592 cycles (including rts).           *
*                                                                       *
*               Instruction times for branch-type instructions          *
*               listed as (X/Y) are for (taken/not taken).              *
*                                                                       *
*************************************************************************

*                       Cycles
gsqrt 
        move.w #7,d4    (8)     ; Loop count (bits-1 of result).
        clr.w d1        (4)     ; Error term in D1.
        clr.w d2        (4)
sqrt1   add.w d0,d0     (4)     ; Get 2 leading bits a time and add
        addx.w d1,d1    (4)     ; into Error term for interpolation.
        add.w d0,d0     (4)     ; (Classical method, easy in binary).
        addx.w d1,d1    (4)
        add.w d2,d2     (4)     ; Running estimate *2.
        move.w d2,d3    (4)
        add.w d3,d3     (4)
        cmp.w d3,d1     (4)
        bls.s sqrt2     (10/8)  ; New Error term > 2* Running estimate?
        addq.w #1,d2    (4)     ; Yes, we want a '1' bit then.
        addq.w #1,d3    (4)     ; Fix up new Error term.
        sub.w d3,d1     (4)
sqrt2   dbra d4,sqrt1   (10/14) ; Do all 8 bit-pairs.
        move.w d2,d0    (4)
        rts             (16)

*************************************************************************
*                                                                       *
*       Integer Square Root (32 to 16 bit).                             *
*                                                                       *
*       (Exact method, not approximate).                                *
*                                                                       *
*       Call with:                                                      *
*               D0.L = Unsigned number.                                 *
*                                                                       *
*       Returns:                                                        *
*               D0.L - SQRT(D0.L)                                       *
*                                                                       *
*       Uses:   D1-D4 as temporaries --                                 *
*               D1 = Error term;                                        *
*               D2 = Running estimate;                                  *
*               D3 = High bracket;                                      *
*               D4 = Loop counter.                                      *
*                                                                       *
*       Notes:  Result fits in D0.W, but is valid in longword.          *    
*                                                                       *
*               Takes from 1080 to 1236 cycles (including rts).         *
*                                                                       *
*               Two of the 16 passes are unrolled from the loop so that *
*               quicker instructions may be used where there is no      *
*               danger of overflow (in the early passes).               *
*                                                                       *
*               Instruction times for branch-type instructions          *
*               listed as (X/Y) are for (taken/not taken).              *
*                                                                       *
*************************************************************************

*                       Cycles
glsqrt
        moveq #13,d4    (4)     ; Loop count (bits-1 of result).
        moveq #0,d1     (4)     ; Error term in D1.
        moveq #0,d2     (4)
lsqrt1  add.l d0,d0     (8)     ; Get 2 leading bits a time and add
        addx.w d1,d1    (4)     ; into Error term for interpolation.
        add.l d0,d0     (8)     ; (Classical method, easy in binary).
        addx.w d1,d1    (4)
        add.w d2,d2     (4)     ; Running estimate * 2.
        move.w d2,d3    (4)
        add.w d3,d3     (4)
        cmp.w d3,d1     (4)
        bls.s lsqrt2    (10/8)  ; New Error term > 2* Running estimate?
        addq.w #1,d2    (4)     ; Yes, we want a '1' bit then.
        addq.w #1,d3    (4)     ; Fix up new Error term.
        sub.w d3,d1     (4)
lsqrt2  dbra d4,lsqrt1  (10/14) ; Do first 14 bit-pairs.
        add.l d0,d0     (8)     ; Do 15-th bit-pair.
        addx.w d1,d1    (4)
        add.l d0,d0     (8)
        addx.l d1,d1    (8)
        add.w d2,d2     (4)
        move.l d2,d3    (4)
        add.w d3,d3     (4)
        cmp.l d3,d1     (6)
        bls.s lsqrt3    (10/8)
        addq.w #1,d2    (4)
        addq.w #1,d3    (3)
        sub.l d3,d1     (8)

lsqrt3  add.l d0,d0     (8)     ; Do 16-th bit-pair.
        addx.l d1,d1    (8)
        add.l d0,d0     (8)
        addx.l d1,d1    (8)
        add.w d2,d2     (4)
        move.l d2,d3    (4)
        add.l d3,d3     (8)
        cmp.l d3,d1     (6)
        bls.s lsqrt4    (10/8)
        addq.w #1,d2    (4)
lsqrt4  move.w d2,d0    (4)
        rts             (16)


;Speed.q,Acceleration.q,MaxSpeed.q
FixUpLeftAcceleration

  ;Result.q = Speed - Acceleration
  Move.l D0,D3
  Sub.l D1,D3

  ;This is a negative downwards acceleration
  Tst.l D1
  BGE FixUpLeftAcceleration_NotNegative

  Tst.l D3
  BGT FixUpLeftAcceleration_AlreadyBelowZero
  Move.l D3,D0 ;Return the result
  RTS

FixUpLeftAcceleration_AlreadyBelowZero
  MoveQ #0,D0
  RTS

FixUpLeftAcceleration_NotNegative
  ;Negative max speed
  Neg.l D2

;  if speed > -MaxSpeed
  Cmp.l D0,D2  ;MaxSpeed - Speed
  BLT FixUpLeftAcceleration_SpeedNotAtLimit
  RTS

FixUpLeftAcceleration_SpeedNotAtLimit

  ;Negative acceleration
  Cmp.l D3,D2  
  BLT FixUpLeftAcceleration_ResultNotAtLimit
  Move.l D2,D0
  RTS

FixUpLeftAcceleration_ResultNotAtLimit
  Move.l D3,D0
  RTS

;--------------------------------------
;Speed.q,Acceleration.q,MaxSpeed.q
FixDownRightAcceleration

  Move.l D0,D3 ;Result.q = Speed
  Add.l D1,D3 ;+ Acceleration

  Tst.l D1
  BGE FixDownRightAcceleration_NotNegative 
  ;This is a negative upwards acceleration
  Tst.l D3 
  BLT FixDownRightAcceleration_AlreadyBelowZero
  Move.l D3,D0 ;Return the result
  RTS

FixDownRightAcceleration_AlreadyBelowZero
  MoveQ #0,D0
  RTS

FixDownRightAcceleration_NotNegative

  ;Are we already travelling at, or even beyond, the speed limit?
  Cmp.l D0,D2 ;MaxSpeed - Speed
  BGE FixDownRightAcceleration_SpeedNotAtLimit
  RTS

FixDownRightAcceleration_SpeedNotAtLimit
  ;We've accelerated to the maximum speed
  Cmp.l D3,D2 ;Maxspeed - Result
  BGE FixDownRightAcceleration_ResultNotAtLimit
  Move.l D2,D0
  RTS

FixDownRightAcceleration_ResultNotAtLimit
  Move.l D3,D0
  RTS


;  unzx0_68000.s - ZX0 decompressor for 68000 - 88 bytes
;
;  in:  a0 = start of compressed data
;       a1 = start of decompression buffer
;
;  Copyright (C) 2021 Emmanuel Marty
;  ZX0 compression (c) 2021 Einar Saukas, https://github.com/einar-saukas/ZX0
;
;  This software is provided 'as-is', without any express or implied
;  warranty.  In no event will the authors be held liable for any damages
;  arising from the use of this software.
;
;  Permission is granted to anyone to use this software for any purpose,
;  including commercial applications, and to alter it and redistribute it
;  freely, subject to the following restrictions:
;
;  1. The origin of this software must not be misrepresented; you must not
;     claim that you wrote the original software. If you use this software
;     in a product, an acknowledgment in the product documentation would be
;     appreciated but is not required.
;  2. Altered source versions must be plainly marked as such, and must not be
;     misrepresented as being the original software.
;  3. This notice may not be removed or altered from any source distribution.

zx0_decompress:
               move.l D0,A0
               move.l D1,A1
               movem.l a2/d2,-(sp)  ; preserve registers
               moveq #-128,d1       ; initialize empty bit queue
                                    ; plus bit to roll into carry
               moveq #-1,d2         ; initialize rep-offset to 1

.literals:     bsr.s .get_elias     ; read number of literals to copy
               subq.l #1,d0         ; dbf will loop until d0 is -1, not 0
.copy_lits:    move.b (a0)+,(a1)+   ; copy literal byte
               dbf d0,.copy_lits    ; loop for all literal bytes
               
               add.b d1,d1          ; read 'match or rep-match' bit
               bcs.s .get_offset    ; if 1: read offset, if 0: rep-match

.rep_match:    bsr.s .get_elias     ; read match length (starts at 1)
.do_copy:      subq.l #1,d0         ; dbf will loop until d0 is -1, not 0
.do_copy_offs: move.l a1,a2         ; calculate backreference address
               add.l d2,a2          ; (dest + negative match offset)               
.copy_match:   move.b (a2)+,(a1)+   ; copy matched byte
               dbf d0,.copy_match   ; loop for all matched bytes

               add.b d1,d1          ; read 'literal or match' bit
               bcc.s .literals      ; if 0: go copy literals

.get_offset:   moveq #-2,d0         ; initialize value to $fe
               bsr.s .elias_loop    ; read high byte of match offset
               addq.b #1,d0         ; obtain negative offset high byte
               beq.s .done          ; exit if EOD marker
               move.w d0,d2         ; transfer negative high byte into d2
               lsl.w #8,d2          ; shift it to make room for low byte

               moveq #1,d0          ; initialize length value to 1
               move.b (a0)+,d2      ; read low byte of offset + 1 bit of len
               asr.l #1,d2          ; shift len bit into carry/offset in place
               bcs.s .do_copy_offs  ; if len bit is set, no need for more
               bsr.s .elias_bt      ; read rest of elias-encoded match length
               bra.s .do_copy_offs  ; go copy match

.get_elias:    moveq #1,d0          ; initialize value to 1
.elias_loop:   add.b d1,d1          ; shift bit queue, high bit into carry
               bne.s .got_bit       ; queue not empty, bits remain
               move.b (a0)+,d1      ; read 8 new bits
               addx.b d1,d1         ; shift bit queue, high bit into carry
                                    ; and shift 1 from carry into bit queue

.got_bit:      bcs.s .got_elias     ; done if control bit is 1
.elias_bt:     add.b d1,d1          ; read data bit
               addx.l d0,d0         ; shift data bit into value in d0
               bra.s .elias_loop    ; keep reading

.done:         movem.l (sp)+,a2/d2  ; restore preserved registers
.got_elias:    rts