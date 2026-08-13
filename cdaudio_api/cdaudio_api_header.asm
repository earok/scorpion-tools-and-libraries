version equ 1

;The CD audio API is intended to provide a common framework for Scorpion redbook/streamed
;music plugins, in the same shape as the audio API but far smaller - the engine only ever
;asks for a track to start, a track to stop, and how many tracks are visible.
;
;Data registers never need to be preserved, but address registers should be.
;On Mega Drive/NeoGeo the plugin is guaranteed to be aligned to 256 bytes.
;
;"CD" here is loose - the AES/MVS plugin drives ADPCM-B streams rather than a disc. Any
;driver that plays one long numbered piece of music at a time fits behind this API.

;Feature flags, reported back to the engine so it does not promise things the hardware
;cannot do. Test with the #CDFlag_ constants in plugin_cdaudio.bb.
CDFLAG_REPEAT equ 1     ;Bit 0 - the repeat argument to Play is honoured
CDFLAG_TRACKS equ 2     ;Bit 1 - Tracks counts the disc, rather than returning -1
;Bit 2 - Play must be called with the OS restored, not from Blitz mode. The engine pays
;for a SafeQAMIGA/SAFEBLITZ round trip around every Play for plugins that set this, so
;only set it where the device genuinely needs it (cdtv.device does, cd.device does not).
CDFLAG_NEEDSOS equ 4

;Can be used to ensure that a compiled plugin is compatible with scorpion itself
    dc.l version

;Feature flags as above
    dc.l _ScorpionAPI_ConstFlags

;Install the CD library (open Amiga devices, read the TOC, etc)
;A0 = Additional Data 0
;A1 = Additional Data 1
;A2 = Additional Data 2
;D0 Return = Install successful if true. A plugin that cannot find a drive/disc should
;            return zero and leave nothing open - the engine then behaves as if there is
;            no CD plugin at all.
    bra.w _ScorpionAPI_Install

;Uninstall the CD library. Must stop any playback and close anything Install opened.
;Only ever called after a successful Install.
    bra.w _ScorpionAPI_Uninstall

;Play a track
;D0 = Track number. Zero or below means "stop playback".
;D1 = Repeat flag, non-zero to loop the track. Plugins that do not set CDFLAG_REPEAT
;     ignore this and play the track once.
;Out of range tracks are the plugin's problem, not the engine's - drop them silently.
    bra.w _ScorpionAPI_Play

;Number of tracks visible to the plugin
;D0 Return = Track count, or -1 if this driver cannot enumerate tracks (CDFLAG_TRACKS
;            clear). -1 is not "no tracks" - it means the engine should keep the count the
;            project was compiled with, since only the plugin knows whether it can do
;            better than the author's assumption. Zero is a real answer: an empty disc.
    bra.w _ScorpionAPI_Tracks
