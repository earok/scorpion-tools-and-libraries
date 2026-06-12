    include "audio_api_header.asm"

;Empty "Null/NA" audio driver. Ideal on new/niche platforms that don't yet have audio support
_ScorpionAPI_ConstWorkAreaMemory equ 0
_ScorpionAPI_ConstMaxVolume equ 0

_ScorpionAPI_Install
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

_ScorpionAPI_SFX
    rts

_ScorpionAPI_SFX_Stop
    rts

_ScorpionAPI_MasterVolume
    rts

_ScorpionAPI_MusicChannels
    rts

_ScorpionAPI_MusicMask
    rts

_ScorpionAPI_VBlank
    MoveQ #0,D0
    rts

_ScorpionAPI_EnableDMAProtection
    rts
