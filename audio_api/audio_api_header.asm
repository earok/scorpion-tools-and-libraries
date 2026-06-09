;The audio API is intended to provide a common framework for Scorpion audio plugins
;Data registers never need to be preserved, but address registers should be

;     void *sfx_ptr  (pointer to sample start in Chip RAM, even address)
;     WORD  sfx_len  (sample length in words)
;     WORD  sfx_per  (hardware replay period for sample)
;     WORD  sfx_vol  (volume 0..64, is unaffected by the song's master volume)
;     BYTE  sfx_cha  (0..3 selected replay channel, -1 selects best channel)
;     BYTE  sfx_pri  (priority, must be in the range 1..127)

sound_pointer equ 0
sound_length equ sound_pointer + 4
sound_period equ sound_length + 2
sound_volume equ sound_period + 2
sound_channel equ sound_volume + 2
sound_priority equ sound_channel + 1

;Definition of the maximum supported volume eg 64 for Amiga
    dc.w _ScorpionAPI_ConstMaxVolume

;Install the audio library
;D0 = Zero equals running on NTSC, otherwise PAL
;A0 = VBL Address (AMIGA)
;A1 = Data table (Mega Drive)
;A2 = Sample table (Mega Drive)
;D0 Return = Install successful if true
    bra.w _ScorpionAPI_Install

;Uninstall the audio library
    bra.w _ScorpionAPI_Uninstall

;Play/Resume
;D0 = Song number (only needed for MDSDRV?)
    bra.w _ScorpionAPI_Play

;Pause
    bra.w _ScorpionAPI_Pause

;Stop music
    bra.w _ScorpionAPI_Stop

;Initialise song. May or may not imply play immediately afterwards
;D0 = Song number
;A0 = Song memory position 
;A1 = Song sample position
    bra.w _ScorpionAPI_InitSong

;Trigger a sound effect
;A0 = Pointer to structure as below. This replicates PTPlayer's expected form, not all of these need to be handled per plugin
    bra.w _ScorpionAPI_SFX
;   Structure layout of SfxStructure:
;     void *sfx_ptr  (pointer to sample start in Chip RAM, even address)
;     WORD  sfx_len  (sample length in words)
;     WORD  sfx_per  (hardware replay period for sample)
;     WORD  sfx_vol  (volume 0..64, is unaffected by the song's master volume)
;     BYTE  sfx_cha  (0..3 selected replay channel, -1 selects best channel)
;     BYTE  sfx_pri  (priority, must be in the range 1..127)

;Stop a currently playing sound effect
;D0 = Relevant channel
    bra.w _ScorpionAPI_SFX_Stop

;Master volume
;D0 = Master volume already premultiplied to be no more than _ScorpionAPI_ConstMaxVolume
    bra.w _ScorpionAPI_MasterVolume

;Music channels
;D0 = Maximum number of channels dedicated to music
    bra.w _ScorpionAPI_MusicChannels

;Music mask
;D0 = Mask of which channels are turned on or off for music
    bra.w _ScorpionAPI_MusicMask
    
;Get event value (E8 etc)
;Returns event value in D0
    bra.w _ScorpionAPI_Event

;Vertical blank update (if needed)
    bra.w _ScorpionAPI_Update