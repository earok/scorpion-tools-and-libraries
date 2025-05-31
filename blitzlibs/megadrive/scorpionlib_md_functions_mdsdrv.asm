;D0 = Work area of at least 1062 bytes
;D1 = Sequence Data
;D2 = PCM data
;D3 = DMA protection size in bytes (0 if none, else 40-220 with 100 recommended)
;D4 = MDSDRV blob
MDSDRV_Init:

	Move.l D0,A0
	Move.l D1,A1
	Move.l D2,A2

	Move.l D4,A3

	Move.w D3,-(SP) ;Store DMA protection amount
    movem.l a0-a6,-(SP) ;Store address registers
	JSR (A3) ;MDSDRV
    movem.l (SP)+,a0-a6	;Restore address registers
	Move.w (SP)+,D3 ;Restore DMA protection amount

	;Wait until the Z80 is ready
MDSDRV_WaitForZ80
	moveq	#$12,d0				; get Z80 pcm mode
	Move.w D3,-(SP) ;Store DMA protection amount
    movem.l a0-a6,-(SP) ;Store address registers	
	jsr $0c(A3) ;bsr.w	MDSDRV+$0c
    movem.l (SP)+,a0-a6	;Restore address registers
	Move.w (SP)+,D3 ;Restore DMA protection amount	
	tst.b	d0
	beq.s	MDSDRV_WaitForZ80	
	
	moveq	#$11,d0			; set_pcmmode
	moveq	#$3,d1			; mixing = 3 channel
	moveq   #0,D2			; Clear the whole longword
	move.w	D3,D2			; DMA protection on

    movem.l a0-a6,-(SP) ;Store address registers

	jsr $0c(A3) ;bsr.w	MDSDRV+$0c
    movem.l (SP)+,a0-a6	;Restore address registers

MDSDRV_Init_Finished
	RTS
	
;D0 = Work area of at least 1062 bytes
;D1 = MDSDRV blob
MDSDRV_Update:
	Move.l D1,A3

    movem.l a0-a6,-(SP)	
	Move.l D0,A0
	JSR 4(A3) ;MDSDRV+4
    movem.l (SP)+,a0-a6		
	RTS

;Clamps D7 to range where 127 is minimum, 0 is maximum. Trashes D6-D7. D6 is in, D7 is out
ClampSound

	;The new volume level goes into D2
	;Clamp to 255
	CMP.w #255,D6
	BLT NoClamp
	Move.w #255,D6
NoClamp
	LSR.w #1,D6 ;Divide by 2. 255 -> 127
	MoveQ #127,D7
	Sub.w D6,D7 ;Reverse the volume level	
	RTS


;D0 = Volume
;D1 = Priority level
;D2 = Work area of at least 1062 bytes
;D3 = MDSDRV mapper
MDSDRV_Volume:
   	Move.l D3,A3

	movem.l a0-a6,-(SP)	
	Move.l D2,A0

	Exg D0,D6
	BSR ClampSound
	Exg D7,D2

	MoveQ #$D,D0

	JSR 12(A3) ;MDSDRV+12

    movem.l (SP)+,a0-a6	
	RTS
	

;D0 = Volume for Music
;D1 = Volume for SFX
;D2 = Work area of at least 1062 bytes
;D3 = MDSDRV mapper
MDSDRV_GVolume:
    Move.l D3,A3

	movem.l a0-a6,-(SP)	
	Move.l D2,A0

	;Upper eight bits is music
	Move.w D0,D6
	BSR ClampSound
	LSL.w #8,D7
	Move.w D7,D0

	;Lower eight bits is SFX
	Move.w D1,D6
	BSR ClampSound
	Move.b D7,D0

	Move.w D0,D1
	MoveQ #$7,D0

	JSR 12(A3);MDSDRV+12

    movem.l (SP)+,a0-a6	
	RTS


;D0 = Sound number
;D1 = Priority level
;D2 = Work area of at least 1062 bytes
;D3 = MDSDRV blob
MDSDRV_Request:
    Move.l D3,A3
    movem.l a0-a6,-(SP)	
	Move.l D2,A0
	JSR 8(A3) ; MDSDRV+8
    movem.l (SP)+,a0-a6	
	RTS
	
JustReturn
	RTS