; Sega Pico ADPCM audio driver

    include "audio_api_header.asm"

PICO_Data    equ $800010  ; ADPCM data port (write: stream data; read: FIFO status)
PICO_Control equ $800012  ; ADPCM control port
PICO_Reset   equ $8000
PICO_Defaults equ $0880   ; max volume + max filter + interrupt enabled

; Work area offsets (relative to megadrive_workarea_pointer)
pico_Address equ 0  ; long: pointer to current position in sample data
pico_Words   equ 4  ; word: remaining words to stream to FIFO

WorkAreaMemory equ 6

_ScorpionAPI_ConstWorkAreaMemory equ WorkAreaMemory
_ScorpionAPI_ConstMaxVolume equ 0

; Install the Pico audio driver
; D0 = PAL flag (unused)
_ScorpionAPI_Install
    movem.l A0,-(SP)
    move.l megadrive_workarea_pointer,A0
    clr.l pico_Address(A0)
    clr.w pico_Words(A0)
    movem.l (SP)+,A0
    move.w #PICO_Reset,(PICO_Control).l
    move.w #PICO_Defaults,(PICO_Control).l
    moveq #1,D0
    rts

_ScorpionAPI_Uninstall
    rts

_ScorpionAPI_Play
    rts

_ScorpionAPI_Pause
    rts

_ScorpionAPI_Stop
    rts

_ScorpionAPI_InitSong
    rts

; Trigger a PCM sound effect
; D0 = Sound ID (unused by Pico)
; A0 = SFX structure
_ScorpionAPI_SFX
    movem.l A1,-(SP)
    move.l megadrive_workarea_pointer,A1
    move.l sound_pointer(A0),pico_Address(A1)
    move.w sound_length(A0),pico_Words(A1)
    movem.l (SP)+,A1
    rts

; Stop current sound by clearing remaining word count
_ScorpionAPI_SFX_Stop
    movem.l A0,-(SP)
    move.l megadrive_workarea_pointer,A0
    clr.w pico_Words(A0)
    movem.l (SP)+,A0
    rts

_ScorpionAPI_MasterVolume
    rts

_ScorpionAPI_MusicChannels
    rts

_ScorpionAPI_MusicMask
    rts

; VBlank update — stream pending sample words into the PICO FIFO
_ScorpionAPI_VBlank
    movem.l A0-A2,-(SP)
    move.l megadrive_workarea_pointer,A0

    move.w pico_Words(A0),D1
    beq.s @pico_vblank_done

    ; Read FIFO free space: lower 6 bits = free bytes, /2 = free words
    move.w (PICO_Data).l,D0
    and.w #$3F,D0
    lsr.w #1,D0
    beq.s @pico_vblank_done

    move.l pico_Address(A0),A1
    movea.l #PICO_Data,A2      ; -pic: constant hardware address, not a relocation

@pico_stream_loop:
    move.w (A1)+,(A2)
    subq.w #1,D1
    beq.s @pico_stream_done
    subq.w #1,D0
    bne.s @pico_stream_loop

@pico_stream_done:
    move.l A1,pico_Address(A0)
    move.w D1,pico_Words(A0)

@pico_vblank_done:
    movem.l (SP)+,A0-A2
    moveq #0,D0
    rts

_ScorpionAPI_EnableDMAProtection
    rts
