    include "audio_api_header.asm"

; TEMPORARY: ADPCM-B extensions. These will move to a dedicated CD/music plugin
; once that plugin API is defined. Appended after the standard header so other
; plugins and the header itself are unaffected.
    bra.w _ScorpionAPI_ADPCMB_Play
    bra.w _ScorpionAPI_ADPCMB_Stop

_ScorpionAPI_ConstWorkAreaMemory equ 0
_ScorpionAPI_ConstMaxVolume equ 64

; M1 ROM builder must assign command 4 to the ADPCM-B stop handler.
; All other user sounds start at command 5.
NULLSOUND_CMD_RESET equ 3
NULLSOUND_CMD_ADPCMB_STOP equ 4

_ScorpionAPI_Install
    moveq #NULLSOUND_CMD_RESET,D0
    bsr.w ns_send
    moveq #1,D0
    rts

_ScorpionAPI_Uninstall
    rts

; Music play/pause/stop are no-ops until NSS music is implemented
_ScorpionAPI_Play
    rts

_ScorpionAPI_Pause
    rts

_ScorpionAPI_Stop
    rts

; D0 = NullSound command ID for this music track. No-op until NSS is implemented;
; SP_InitSong calls SP_Play immediately after, so both are stubs for now.
_ScorpionAPI_InitSong
    rts

; D0 = NullSound command ID stored in sfx_cha of the project sound entry
_ScorpionAPI_SFX
    bsr.w ns_send
    rts

; ADPCM-A has no per-channel stop in NullSound without dedicated commands.
; Samples are short; natural decay is acceptable.
_ScorpionAPI_SFX_Stop
    rts

; NullSound has no global master volume command
_ScorpionAPI_MasterVolume
    rts

_ScorpionAPI_MusicChannels
    rts

_ScorpionAPI_MusicMask
    rts

_ScorpionAPI_VBlank
    moveq #0,D0
    rts

_ScorpionAPI_EnableDMAProtection
    rts

; TEMPORARY: ADPCM-B extension implementations
_ScorpionAPI_ADPCMB_Play
    ; D0 = NullSound command ID for this ADPCM-B track (assigned by M1 ROM builder)
    bsr.w ns_send
    rts

_ScorpionAPI_ADPCMB_Stop
    moveq #NULLSOUND_CMD_ADPCMB_STOP,D0
    bsr.w ns_send
    rts

; NullSound command send routine
; Writes command byte to the Z80 sound port and waits for acknowledgment.
; NullSound NMI handler responds with (command | $80).
; D0.b = command ID (range $03..$7F)
; Trashes D0, D7
ns_send
    move.b D0,$320000
    or.b #$80,D0
.wait
    move.w #999,D7
.delay
    dbra D7,.delay
    move.b $320000,D7
    cmp.b D7,D0
    bne.s .wait
    rts
