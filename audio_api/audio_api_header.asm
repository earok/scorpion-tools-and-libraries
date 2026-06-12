;The audio API is intended to provide a common framework for Scorpion audio plugins
;Data registers never need to be preserved, but address registers should be
;On Mega Drive is guaranteed to be aligned with 256 bytes

;     This is based on the shape of the PTPlayer plugin but is extended where needed
;     void *sfx_ptr  (pointer to sample start in Chip RAM, even address)
;     WORD  sfx_len  (sample length in words)
;     WORD  sfx_per  (hardware replay period for sample)
;     WORD  sfx_vol  (volume 0..64, is unaffected by the song's master volume)
;     BYTE  sfx_cha  (0..3 selected replay channel, -1 selects best channel)
;     BYTE  sfx_pri  (priority, must be in the range 1..127)

sound_pointer equ 0
sound_length equ sound_pointer+4
sound_period equ sound_length+2
sound_volume equ sound_period+2
sound_channel equ sound_volume+2
sound_priority equ sound_channel+1

;Pointers to work areas on different systems. Not needed on Amiga since variables can be local, not implemented on NeoGeo yet
megadrive_workarea_pointer equ $FF0008

;How much memory is required to be allocated for this plugin. Only supported on Mega Drive
    dc.l _ScorpionAPI_ConstWorkAreaMemory

;Definition of the maximum supported volume eg 64 for Amiga
    dc.w _ScorpionAPI_ConstMaxVolume

;Install the audio library
;D0 = Zero equals running on NTSC, otherwise PAL
;A0 = Additional Data 0 (VBL on Amiga)
;A1 = Additional Data 1
;A2 = Additioanl Data 2
;D0 Return = Install successful if true
    bra.w _ScorpionAPI_Install

;Uninstall the audio library
    bra.w _ScorpionAPI_Uninstall

;Play/Resume
    bra.w _ScorpionAPI_Play

;Pause
    bra.w _ScorpionAPI_Pause

;Stop music
    bra.w _ScorpionAPI_Stop

;Initialise song. Scorpion internally will call _ScorpionAPI_Play immediately after so this should only handle the song setup, not the song triggering
;D0 = Song id
;D1 = Song position
;A0 = Song data position 
;A1 = Song sample position
    bra.w _ScorpionAPI_InitSong

;Trigger a sound effect
;D0 = Sound ID (for ID-based drivers such as MDSDRV; pointer-based drivers such as XGM use sfx_ptr from the structure instead)
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

;Vertical blank update
;Returns event value in D0.l (eg Protracker E8s), just set to zero if there are no events this driver does
    bra.w _ScorpionAPI_VBlank

;Enable DMA protection - call at the top of VBlank before any DMA transfers
;On most drivers this does nothing. On XGM it signals the Z80 not to touch the DMA bus.
;The VBlank call automatically clears this at the bottom of VBlank.
    bra.w _ScorpionAPI_EnableDMAProtection