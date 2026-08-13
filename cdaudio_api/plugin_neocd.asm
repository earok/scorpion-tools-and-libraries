    include "cdaudio_api_header.asm"

;NeoGeo CD redbook audio through the CDDA BIOS call. This replaces the SystemVersion
;branch that used to sit in neogeo/functions_neogeo.bb.
;
;The BIOS gives us looping for free, so unlike the Amiga drives the repeat argument is
;real. It cannot enumerate tracks though, so CDFLAG_TRACKS stays clear and Tracks
;reports zero ("unknown") rather than a made up count.
_ScorpionAPI_ConstFlags equ CDFLAG_REPEAT

;https://wiki.neogeodev.org/index.php?title=CDDA
;D0 is one word: high byte = command, low byte = track number in BCD
BIOS_CDDA equ $C0056A

CDDA_PLAY_LOOP equ $0000        ;Keeps the track going, which is what the engine has
CDDA_PLAY_ONCE equ $0100        ;always done for CD music
CDDA_STOP      equ $0200

storeAddressRegisters macro
    movem.l d2/a4-a6,-(sp)      ;A4-A6 belong to Blitz 2, the BIOS does not respect them
    endm

restoreAddressRegisters macro
    movem.l (sp)+,d2/a4-a6
    rts
    endm

_ScorpionAPI_Install
    moveq #-1,d0                ;Nothing to open, the BIOS is always there
    rts

_ScorpionAPI_Uninstall
    storeAddressRegisters
    bsr.s neocd_stop
    restoreAddressRegisters

;D0 = track (<=0 stops), D1 = repeat
_ScorpionAPI_Play
    storeAddressRegisters
    tst.l d0
    ble.s .stop

    ;BCD encode the track number - track 42 has to be sent as $42, not $2A
    move.l d0,d2
    moveq #0,d0
.bcdloop
    cmp.w #10,d2
    blt.s .bcddone
    sub.w #10,d2
    add.w #$10,d0
    bra.s .bcdloop
.bcddone
    or.w d2,d0

    tst.l d1
    bne.s .send                 ;CDDA_PLAY_LOOP is zero, nothing to or in
    or.w #CDDA_PLAY_ONCE,d0
.send
    jsr BIOS_CDDA
    restoreAddressRegisters

.stop
    bsr.s neocd_stop
    restoreAddressRegisters

;The CDDA BIOS has no TOC query. -1 tells the engine to keep whatever track count the
;project was compiled with rather than replacing it with a guess.
_ScorpionAPI_Tracks
    moveq #-1,d0
    rts

neocd_stop
    move.w #CDDA_STOP,d0
    jsr BIOS_CDDA
    rts
