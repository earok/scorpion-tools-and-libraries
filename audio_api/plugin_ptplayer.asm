    include "audio_api_header.asm"

_ScorpionAPI_ConstWorkAreaMemory equ 0
_ScorpionAPI_ConstMaxVolume equ 64

storeAddressRegisters	macro
	movem.l a4-a6,-(sp) ; Save registers for Blitz 2
	lea     CUSTOM,a6 ;Store the custom register in A6
	endm
	
restoreAddressRegisters	macro
	movem.l (sp)+,a4-a6	; Restore registers for Blitz
	rts ;Return to Blitz
	endm

_ScorpionAPI_Install
    storeAddressRegisters
    bsr _mt_install_cia
    restoreAddressRegisters

_ScorpionAPI_Uninstall
    storeAddressRegisters
    bsr _mt_remove_cia
    restoreAddressRegisters

_ScorpionAPI_Play
    lea _mt_Enable,A0
    move.b #-1,(A0)
    rts

_ScorpionAPI_Pause
    lea _mt_Enable,A0
    move.b #0,(A0)
    rts

_ScorpionAPI_Stop
    storeAddressRegisters
    bsr _mt_end
    restoreAddressRegisters

_ScorpionAPI_InitSong
    storeAddressRegisters
    move.l D1,D0 ;Move the position into the position slot
    bsr _mt_init
    restoreAddressRegisters

_ScorpionAPI_SFX
    storeAddressRegisters
    bsr _mt_playfx
    restoreAddressRegisters

_ScorpionAPI_SFX_Stop
    storeAddressRegisters
    bsr _mt_stopfx
    restoreAddressRegisters

_ScorpionAPI_MasterVolume
    storeAddressRegisters
    bsr _mt_mastervol
    restoreAddressRegisters

_ScorpionAPI_MusicChannels
    lea _mt_MusicChannels,A0
    moveq #0,D0
    move.b D0,(A0)
    rts

_ScorpionAPI_MusicMask
    storeAddressRegisters
    bsr _mt_musicmask
    restoreAddressRegisters

_ScorpionAPI_VBlank
    lea _mt_E8Trigger,A0
    moveq.l #0,D0
    move.b (A0),D0
    rts
 
    include "ptplayer.asm"

