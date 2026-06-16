    include "audio_api_header.asm"

_ScorpionAPI_ConstWorkAreaMemory equ 0
_ScorpionAPI_ConstMaxVolume equ 64

; Command ID conventions (must match M1 ROM builder):
;   0x00        unused
;   0x01        ROM switch (NullSound internal, NMI handler)
;   0x02        eye catcher (NullSound internal, NMI handler)
;   0x03        reset/init (NullSound internal, NMI handler)
;   0x04        ADPCM-B stop
;   0x05        NSS stream stop
;   0x06+       samples  (5 + SampleID)
;   0x7F down   music    (128 - MusicID)
NULLSOUND_CMD_RESET equ 3
NULLSOUND_CMD_ADPCMB_STOP equ 4
NULLSOUND_CMD_STREAM_STOP equ 5

_ScorpionAPI_Install
    moveq #NULLSOUND_CMD_RESET,D0
    bsr.w ns_send
    moveq #1,D0
    rts

_ScorpionAPI_Uninstall
    rts

; No pause concept in NullSound streams
_ScorpionAPI_Play
    rts

_ScorpionAPI_Pause
    rts

_ScorpionAPI_Stop
; D0 = channel (for ADPCM B)
    tst.w D0
    bne _ScorpionAPI_ADPCMB_Stop
    moveq #NULLSOUND_CMD_STREAM_STOP,D0
    bsr.w ns_send
    rts

; D0 = MusicID (1, 2, 3...) -> command 128 - MusicID
; D1 = position (not used here)
; D2 = channel (for ADPCM B)
; SP_InitSong calls SP_Play immediately after, so send here and let Play no-op
_ScorpionAPI_InitSong
    tst.w D2
    bne _ScorpionAPI_ADPCMB_Play    
    neg.b D0
    add.b #$80,D0
    bsr.w ns_send
    rts

; D0 = SampleID (1, 2, 3...) -> command 5 + SampleID
_ScorpionAPI_SFX
    add.b #5,D0
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
; D0 = MusicID (1, 2, 3...) -> command 128 - MusicID
_ScorpionAPI_ADPCMB_Play
    neg.b D0
    add.b #$80,D0
    bsr.w ns_send
    rts

_ScorpionAPI_ADPCMB_Stop
    moveq #NULLSOUND_CMD_ADPCMB_STOP,D0
    bsr.w ns_send
    rts

; NullSound command send routine
; D0.b = command ID
ns_send
    move.b D0,$320000
    rts
