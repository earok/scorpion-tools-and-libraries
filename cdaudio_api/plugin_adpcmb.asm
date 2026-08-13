    include "cdaudio_api_header.asm"

;NeoGeo AES/MVS "CD music" through NullSound's ADPCM-B channel. Cartridge hardware has no
;disc, so the second streaming channel stands in for one - a project's CD tracks become
;ADPCM-B streams and the game code does not have to know the difference.
;
;This replaces the ADPCM-B extension that used to be bolted onto the audio plugin
;(plugin_nullsound.asm), where it was reached by passing a non-zero song channel through
;SP_InitSong/SP_Stop. That channel argument no longer has a second meaning.
;
;NSS streams loop on their own, so the repeat argument cannot change anything and
;CDFLAG_REPEAT stays clear. There is no track list either, hence no CDFLAG_TRACKS.
_ScorpionAPI_ConstFlags equ 0

;Command ID conventions (must match the M1 ROM builder and plugin_nullsound.asm):
;   0x00        unused
;   0x01        ROM switch (NullSound internal, NMI handler)
;   0x02        eye catcher (NullSound internal, NMI handler)
;   0x03        reset/init (NullSound internal, NMI handler)
;   0x04        ADPCM-B stop
;   0x05        NSS stream stop
;   0x06+       samples  (5 + SampleID)
;   0x7F down   music    (128 - MusicID)
NULLSOUND_PORT       equ $320000
NULLSOUND_CMD_ADPCMB_STOP equ 4

;The audio plugin has already reset NullSound by the time we get here - sending another
;reset would silence the music that is playing
_ScorpionAPI_Install
    moveq #-1,d0
    rts

_ScorpionAPI_Uninstall
    moveq #NULLSOUND_CMD_ADPCMB_STOP,d0
    bra.s ns_send

;D0 = track (<=0 stops), D1 = repeat (ignored, see CDFLAG_REPEAT above)
;Track IDs are clamped by the compiler, not checked here
_ScorpionAPI_Play
    tst.l d0
    ble.s .stop
    neg.b d0
    add.b #$80,d0               ;128 - MusicID
    bra.s ns_send
.stop
    moveq #NULLSOUND_CMD_ADPCMB_STOP,d0
    bra.s ns_send

;ADPCM-B streams are numbered by the compiler, there is nothing to enumerate at runtime,
;so -1 leaves the compiled in track count standing
_ScorpionAPI_Tracks
    moveq #-1,d0
    rts

;NullSound command send routine
;D0.b = command ID
ns_send
    move.b d0,NULLSOUND_PORT
    rts
