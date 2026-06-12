    include "audio_api_header.asm"

    ;1066 bytes of memory are currently allocated here
    Macro PrepareWorkArea
    move.l megadrive_workarea_pointer,A0
    EndM

    Macro SafeCommand
    movem.l A0-A6,-(SP)
    jsr mds_command(A3)
    movem.l (SP)+,A0-A6
    EndM

    Macro SafeRequest
    movem.l A0-A6,-(SP)
    jsr mds_request(A3)
    movem.l (SP)+,A0-A6
    EndM

WorkAreaMemory equ 1064

_ScorpionAPI_ConstMaxVolume equ 256
_ScorpionAPI_ConstWorkAreaMemory equ WorkAreaMemory+2 ;Additional memory for internal plugin use

;Remember the song ID to play
SongId equ WorkAreaMemory

;======================================================================
; initialize sound driver
;----------------------------------------------------------------------
; INPUT
;	a0 - pointer to work area
;   a1 - pointer to request base table (sdtop)
;   a2 - pointer to PCM data
; OUTPUT
;   d0 - zero if successful, non-zero if not
; TRASHES
;   a0-a1, d1

_ScorpionAPI_Install
    PrepareWorkArea

    lea mdsdrv,A3
    jsr mds_init(A3)

    ;Wait until the Z80 is ready
MDSDRV_WaitForZ80
	moveq	#command_getpcmmode,d0
	SafeCommand
	tst.b	d0
	beq.s	MDSDRV_WaitForZ80

    ;Do our setup with default settings
	moveq	#command_setpcmmode,d0
	moveq	#$3,d1 ; mixing = 3 channel
	moveq   #0,D2 ; Clear the whole longword
	move.w	#220,D2 ; DMA protection on (220 as defaullt value)
	SafeCommand

    moveq #1,D0 ;Force success
    rts

_ScorpionAPI_Uninstall
    ;Do nothing
    rts

_ScorpionAPI_Play
    move.b #3,D1 ;Set the priority slot
    PrepareWorkArea
    move.w SongId(A0),D0
    lea mdsdrv,A3
    SafeRequest
    rts

_ScorpionAPI_Pause
    moveq.l #0,D0 ;Set the ID to zero
    move.b #3,D1 ;Set the priority slot
    PrepareWorkArea
    lea mdsdrv,A3
    SafeRequest
    rts

_ScorpionAPI_Stop
    ;Same as pause ebut we want to forget the song ID
    PrepareWorkArea
    move.w #0,SongId(A0)
    bra _ScorpionAPI_Pause

_ScorpionAPI_InitSong
    ;Remember the song to play
    PrepareWorkArea
    move.w D0,SongId(A0)    
    rts

_ScorpionAPI_SFX
    move.l sound_pointer(A0),D0 ;Set the sound ID
    move.b sound_priority(A0),D1 ;Set the priority slot
    PrepareWorkArea
    lea mdsdrv,A3
    SafeRequest
    rts

_ScorpionAPI_SFX_Stop
    move.l D0,D1 ;Set the priority slot
    moveq #0,D0 ;Set the sound ID to zero
    PrepareWorkArea
    lea mdsdrv,A3
    SafeRequest
    rts

_ScorpionAPI_MasterVolume
	Exg D0,D6
	BSR ClampSound
	Exg D7,D2

	MoveQ.l #command_setsongvolume,D0
    PrepareWorkArea
    lea mdsdrv,A3
	SafeCommand
    rts

_ScorpionAPI_MusicChannels
    rts

_ScorpionAPI_MusicMask
    rts

_ScorpionAPI_VBlank

    move.w  #$100,$a11100 ;Fast pause Z80
dmawait
    btst	#0,$a11100
    bne	    dmawait
    st.b	$a00e06					; Set z_vbl_ack to $ff
    move.w  #$000,$a11100 ;Resume Z80

    movem.l A4-A6,-(SP)
    PrepareWorkArea
    lea mdsdrv,A3
    jsr mds_update(A3)
    movem.l (SP)+,A4-A6
    MoveQ #0,D0 ;No events
    rts

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
    
;Probably should be compiling that from scratch...
mdsdrv
	incbin "mdsdrv.bin"
	

mds_init equ 0
mds_update equ 4
mds_request equ 8
mds_command equ 12

command_setsongvolume equ $d
command_setpcmmode equ $11
command_getpcmmode equ $12

;======================================================================
; MDSDRV - Mega Drive 68K Sound Driver
;======================================================================
; Copyright (c) 2019-2022 Ian Karlsson
;
; This software is provided 'as-is', without any express or implied
; warranty. In no event will the authors be held liable for any damages
; arising from the use of this software.
;
; Permission is granted to anyone to use this software for any purpose,
; including commercial applications, and to alter it and redistribute
; it freely, subject to the following restrictions:
;
; 1. The origin of this software must not be misrepresented; you must
;    not claim that you wrote the original software. If you use this
;    software in a product, an acknowledgment in the product
;    documentation would be appreciated but is not required.
; 2. Altered source versions must be plainly marked as such, and must
;    not be misrepresented as being the original software.
; 3. This notice may not be removed or altered from any source
;    distribution.
;======================================================================

;======================================================================
; Sound request
;----------------------------------------------------------------------
; INPUT
;	a0 - pointer to work area
;	d0 - request sound number
;	d1 - request priority, range 0-3.
; TRASHES
;	d0,d1

;======================================================================
; update sound driver
;----------------------------------------------------------------------
; INPUT
;	a0 - pointer to work area
; TRASHES
;   a1-a6, d0-d7
