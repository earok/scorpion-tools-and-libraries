    include "cdaudio_api_header.asm"
    include "amiga_cd_common.asm"

;CDTV redbook audio through cdtv.device. This is a reimplementation of the old
;amiga/cdtv.old.bb that was pulled out of the engine proper.
;
;The CDTV drive is slow and unforgiving: commands issued back to back get dropped, and
;a play request is not actually playing until QUICKSTATUS says so. The generous waits
;below are carried over verbatim from the Blitz version because they were tuned against
;real hardware - shortening them is a hardware experiment, not a cleanup.
;
;cdtv.device has no repeat mode and nothing calls the plugin per frame, so CDFLAG_REPEAT
;is clear and the repeat argument is ignored.
_ScorpionAPI_ConstFlags equ CDFLAG_TRACKS

;--- cdtv.device, see the CDTV Developer Reference Manual
CDTV_PLAYTRACK equ 43
CDTV_TOCLSN    equ 48
CDTV_STOPPLAY  equ 53

;A cdtv.device TOC entry is NOT laid out like a cd.device one - the last track number
;lives at offset 3, behind rsvd/AddrCtrl/Track
CDTVTOC_LASTTRACK equ 3

;Frame counts, matching the VWaits of the original Blitz implementation
CDTV_WAIT_LONG  equ 50          ;A full second
CDTV_WAIT_SHORT equ 10

storeAddressRegisters macro
    movem.l d2/a2-a6,-(sp)      ;A4-A6 belong to Blitz 2
    endm

restoreAddressRegisters macro
    movem.l (sp)+,d2/a2-a6
    rts
    endm

_ScorpionAPI_Install
    storeAddressRegisters
    lea cdtv_devname(pc),a0
    bsr cdcom_open
    tst.l d0
    beq.s .fail

    lea cdv_io(a3),a1
    move.w #CDTV_TOCLSN,IO_COMMAND(a1)
    clr.l IO_OFFSET(a1)             ;Entry 0 = summary
    moveq #1,d0
    move.l d0,IO_LENGTH(a1)
    lea cdv_toc(a3),a0
    move.l a0,IO_DATA(a1)
    clr.b IO_FLAGS(a1)
    move.l 4.w,a6
    jsr _LVODoIO(a6)
    tst.l d0
    bne.s .failclose

    moveq #0,d0
    move.b cdv_toc+CDTVTOC_LASTTRACK(a3),d0
    move.w d0,cdv_tracks(a3)
    beq.s .failclose                ;No tracks is no more use to us than no disc

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
    bsr cdtv_stop
    bsr cdcom_close
    restoreAddressRegisters

;D0 = track (<=0 stops), D1 = repeat (ignored, see CDFLAG_REPEAT above)
_ScorpionAPI_Play
    storeAddressRegisters
    move.l d0,d2                    ;Requested track
    lea cdcom_vars(pc),a3
    move.w cdv_open(a3),d0
    beq .exit

    bsr cdtv_stop
    tst.l d0
    beq .exit                       ;Device still owns the request, do not touch it

    tst.l d2
    ble .exit                       ;Stop only
    moveq #0,d0
    move.w cdv_tracks(a3),d0
    cmp.l d0,d2
    bgt .exit                       ;Past the end of the disc

    moveq #CDTV_WAIT_LONG,d0
    bsr cdcom_waitframes

    lea cdv_io(a3),a1
    move.w #CDTV_PLAYTRACK,IO_COMMAND(a1)
    move.l d2,IO_OFFSET(a1)         ;Track number, counting from 1
    clr.l IO_LENGTH(a1)             ;Stop at the next track
    clr.l IO_DATA(a1)
    clr.b IO_FLAGS(a1)

    moveq #CDTV_WAIT_SHORT,d0
    bsr cdcom_waitframes
    move.l 4.w,a6
    lea cdv_io(a3),a1
    jsr _LVOSendIO(a6)

    ;Mark it as playing the moment the request is queued, not once the drive confirms
    ;audio. The old Blitz version only set its flag on success, which meant a track that
    ;never started left a request outstanding that nothing would ever abort - and the
    ;next SendIO on that same IORequest would corrupt exec's message list.
    move.w #1,cdv_playing(a3)

    ;The Blitz version followed this with a QUICKSTATUS poll to confirm audio was really
    ;coming out. That poll is gone: it used blocking DoIO, and Play is reachable from the
    ;VM in Blitz mode where the OS interrupts needed to complete it are dead, so it could
    ;hang the machine on the ordinary case of starting a track. The settling waits either
    ;side are the part that was actually tuned against hardware, and they stay.
    moveq #CDTV_WAIT_LONG,d0
    bsr cdcom_waitframes

.exit
    restoreAddressRegisters

_ScorpionAPI_Tracks
    lea cdcom_vars(pc),a0
    moveq #0,d0
    move.w cdv_tracks(a0),d0
    rts

;Bring the drive to a halt, with the settling waits the hardware needs.
;A3 = variable block. Does nothing if we never started anything.
;D0 Return = non-zero if cdv_io is free to reuse, as for cdcom_abort
cdtv_stop
    move.w cdv_playing(a3),d0
    beq.s .free

    moveq #CDTV_WAIT_LONG,d0
    bsr cdcom_waitframes
    moveq #CDTV_WAIT_SHORT,d0
    bsr cdcom_waitframes

    bsr cdcom_abort

    lea cdv_io(a3),a1
    move.w #CDTV_STOPPLAY,IO_COMMAND(a1)
    clr.l IO_OFFSET(a1)
    clr.l IO_DATA(a1)
    clr.l IO_LENGTH(a1)
    clr.b IO_FLAGS(a1)

    moveq #CDTV_WAIT_SHORT,d0
    bsr cdcom_waitframes
    move.l 4.w,a6
    lea cdv_io(a3),a1
    jsr _LVOSendIO(a6)

    ;SendIO rather than DoIO, because a track that ended on its own leaves cdv_playing set
    ;and lets us reach here in Blitz mode, where a blocking DoIO would never return.
    ;Reclaim rather than abort - aborting the stop would defeat the point of sending it.
    move.w #1,cdv_playing(a3)
    bsr cdcom_reclaim
    tst.l d0
    beq.s .busy

    moveq #CDTV_WAIT_LONG,d0
    bsr cdcom_waitframes
.free
    moveq #-1,d0
    rts
.busy
    moveq #0,d0
    rts

cdtv_devname
    dc.b "cdtv.device",0
    even
