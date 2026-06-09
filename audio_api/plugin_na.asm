    include "audio_api_header.asm"

;Empty "Null/NA" audio driver

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

_ScorpionAPI_Event
    MoveQ #0,D0
    rts
    