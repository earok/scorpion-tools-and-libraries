    include "cdaudio_api_header.asm"
    include "amiga_cd_common.asm"

;CD32 redbook audio through cd.device. This replaces the Blitz CD32 library calls
;(InitCD32/ExamineCD32/TocCD32/PlayCD32/StopCD32) that used to sit in the engine proper.
;
;cd.device has no repeat mode - CD_PLAYTRACK simply completes when the track ends - and
;nothing calls the plugin per frame, so there is nowhere to re-trigger from. CDFLAG_REPEAT
;is therefore clear and the repeat argument is ignored.
_ScorpionAPI_ConstFlags equ CDFLAG_TRACKS

;--- devices/cd.i
CD_INFO      equ 32
CD_TOCLSN    equ 35
CD_PLAYTRACK equ 37
CD_PAUSE     equ 40

;CD_TOCLSN entry zero is the summary: FirstTrack, LastTrack, then the lead-out position
CDTOC_LASTTRACK equ 1

storeAddressRegisters macro
    movem.l d2-d3/a2-a6,-(sp)   ;A4-A6 belong to Blitz 2
    endm

restoreAddressRegisters macro
    movem.l (sp)+,d2-d3/a2-a6
    rts
    endm

;A failed TOC read means no disc, or a disc we cannot enumerate. Report failure rather
;than installing with a zero track count - the engine then treats the machine as having
;no CD at all, which is what the old InitCD32/ExamineCD32 pair did.
_ScorpionAPI_Install
    storeAddressRegisters
    lea cd32_devname(pc),a0
    bsr cdcom_open
    tst.l d0
    beq.s .fail

    lea cdv_io(a3),a1
    move.w #CD_TOCLSN,IO_COMMAND(a1)
    clr.l IO_OFFSET(a1)             ;Entry 0 = summary
    moveq #1,d0
    move.l d0,IO_LENGTH(a1)         ;One entry
    lea cdv_toc(a3),a0
    move.l a0,IO_DATA(a1)
    clr.b IO_FLAGS(a1)
    move.l 4.w,a6
    jsr _LVODoIO(a6)
    tst.l d0
    bne.s .failclose

    moveq #0,d0
    move.b cdv_toc+CDTOC_LASTTRACK(a3),d0
    move.w d0,cdv_tracks(a3)
    beq.s .failclose                ;An empty TOC is no more use to us than no disc

    moveq #-1,d0
    restoreAddressRegisters

.failclose
    bsr cdcom_close
.fail
    moveq #0,d0
    restoreAddressRegisters

_ScorpionAPI_Uninstall
    storeAddressRegisters
    lea cdcom_vars(pc),a3
    bsr cdcom_abort
    bsr cdcom_close
    restoreAddressRegisters

;D0 = track (<=0 stops), D1 = repeat (ignored, see CDFLAG_REPEAT above)
_ScorpionAPI_Play
    storeAddressRegisters
    move.l d0,d2                    ;Requested track
    lea cdcom_vars(pc),a3
    move.w cdv_open(a3),d0
    beq.s .exit

    ;Whatever we do next, the current track has to stop first
    bsr cdcom_abort
    tst.l d0
    beq.s .exit                     ;Device still owns the request, do not touch it

    tst.l d2
    ble.s .exit                     ;Stop only
    moveq #0,d0
    move.w cdv_tracks(a3),d0
    cmp.l d0,d2
    bgt.s .exit                     ;Past the end of the disc

    lea cdv_io(a3),a1
    move.w #CD_PLAYTRACK,IO_COMMAND(a1)
    move.l d2,IO_OFFSET(a1)         ;Track number, counting from 1
    moveq #1,d0
    move.l d0,IO_LENGTH(a1)         ;Play exactly one track
    clr.l IO_DATA(a1)
    clr.b IO_FLAGS(a1)
    move.l 4.w,a6
    jsr _LVOSendIO(a6)              ;Async - CD_PLAYTRACK does not return until the end
    move.w #1,cdv_playing(a3)

.exit
    restoreAddressRegisters

_ScorpionAPI_Tracks
    lea cdcom_vars(pc),a0
    moveq #0,d0
    move.w cdv_tracks(a0),d0
    rts

cd32_devname
    dc.b "cd.device",0
    even
