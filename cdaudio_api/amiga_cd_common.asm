;Shared exec/device plumbing for the Amiga CD audio plugins (CD32 and CDTV).
;
;Both drives are talked to the same way - open a device on a hand built reply port,
;send an async play request, AbortIO it to stop - only the device name, the command
;numbers and the TOC layout differ. Everything that is identical lives here.
;
;A4-A6 belong to Blitz 2 and are saved/restored by every API entry point before this
;code is reached, so exec calls trashing A6 are safe.

;--- exec library vectors
_LVOFindTask    equ -294
_LVOAllocSignal equ -330
_LVOFreeSignal  equ -336
_LVOOpenDevice  equ -444
_LVOCloseDevice equ -450
_LVODoIO        equ -456
_LVOSendIO      equ -462
_LVOWaitIO      equ -474
_LVOAbortIO     equ -480

;--- exec/nodes.i
NT_MSGPORT equ 4
NT_MESSAGE equ 5

;--- exec/ports.i (MsgPort)
MP_LNTYPE  equ 8
MP_FLAGS   equ 14
MP_SIGBIT  equ 15
MP_SIGTASK equ 16
MP_MSGLIST equ 20
MP_SIZE    equ 34

;--- exec/io.i (IOStdReq)
MN_LNTYPE    equ 8
MN_REPLYPORT equ 14
MN_LENGTH    equ 18
IO_DEVICE    equ 20
IO_UNIT      equ 24
IO_COMMAND   equ 28
IO_FLAGS     equ 30
IO_ERROR     equ 31
IO_ACTUAL    equ 32
IO_LENGTH    equ 36
IO_DATA      equ 40
IO_OFFSET    equ 44
IOSTD_SIZE   equ 48

;--- Plugin state. All of it is reached through A3 so that nothing here needs a
;    non-PC-relative reference (the plugin is assembled -pic and loaded anywhere).
cdv_open     equ 0                  ;Word, non-zero once the device is open
cdv_playing  equ 2                  ;Word, non-zero while a play request is outstanding
cdv_tracks   equ 4                  ;Word, last track number reported by the TOC
cdv_sigbit   equ 6                  ;Byte, signal allocated for the reply port
cdv_pad      equ 7
cdv_port     equ 8                  ;MsgPort
cdv_io       equ cdv_port+MP_SIZE   ;IOStdReq used for both queries and playback
cdv_toc      equ cdv_io+IOSTD_SIZE  ;One TOC entry - 6 bytes on cd.device, 8 on cdtv.device
cdv_size     equ cdv_toc+8

;Open a CD device and make it ready for play requests
;A0 = device name (null terminated)
;D0 Return = non-zero on success
;A3 Return = variable block (valid whether or not the open succeeded)
;Exec preserves A2-A6 across library calls, so A3 stays pointed at the variable block
;for the whole of this routine.
cdcom_open
    move.l a2,-(sp)
    move.l a0,a2                    ;Device name for later
    lea cdcom_vars(pc),a3

    ;Wipe the whole block - Install can legitimately be reached twice (a failed first
    ;attempt, or a plugin left resident across a restart) and stale port/IO fields are
    ;far more dangerous than the handful of cycles this costs
    move.l a3,a0
    move.w #cdv_size-1,d0
.wipe
    clr.b (a0)+
    dbra d0,.wipe

    move.l 4.w,a6

    ;A reply port needs a signal of our own. Without this DoIO/WaitIO would signal a
    ;null task, which is the classic way to hang a Blitz program on a slow drive.
    moveq #-1,d0
    jsr _LVOAllocSignal(a6)
    cmp.b #-1,d0
    beq .fail
    move.b d0,cdv_sigbit(a3)

    sub.l a1,a1                     ;FindTask(NULL) = ourselves
    jsr _LVOFindTask(a6)
    move.l d0,cdv_port+MP_SIGTASK(a3)

    lea cdv_port(a3),a1
    move.b #NT_MSGPORT,MP_LNTYPE(a1)
    move.b cdv_sigbit(a3),MP_SIGBIT(a1)
    ;MP_FLAGS stays zero (PA_SIGNAL)

    ;NewList(&port->mp_MsgList)
    lea cdv_port+MP_MSGLIST(a3),a0
    move.l a0,d0
    addq.l #4,d0
    move.l d0,(a0)                  ;lh_Head = &lh_Tail
    move.l a0,8(a0)                 ;lh_TailPred = &lh_Head

    lea cdv_io(a3),a1
    move.b #NT_MESSAGE,MN_LNTYPE(a1)
    lea cdv_port(a3),a0
    move.l a0,MN_REPLYPORT(a1)
    move.w #IOSTD_SIZE,MN_LENGTH(a1)

    move.l a2,a0                    ;Device name
    moveq #0,d0                     ;Unit 0
    lea cdv_io(a3),a1
    moveq #0,d1                     ;No flags
    jsr _LVOOpenDevice(a6)
    tst.l d0
    bne.s .failsignal

    move.w #1,cdv_open(a3)
    moveq #-1,d0
    bra.s .exit

.failsignal
    ;Hand the signal back - leaking one per failed Install would eventually exhaust
    ;the task's 32 signals
    moveq #0,d0
    move.b cdv_sigbit(a3),d0
    jsr _LVOFreeSignal(a6)
.fail
    clr.w cdv_open(a3)
    moveq #0,d0
.exit
    move.l (sp)+,a2
    rts

;Close the device and release the signal. Safe to call when nothing was ever opened.
cdcom_close
    lea cdcom_vars(pc),a3
    move.w cdv_open(a3),d0
    beq.s .done
    move.l 4.w,a6
    lea cdv_io(a3),a1
    jsr _LVOCloseDevice(a6)
    moveq #0,d0
    move.b cdv_sigbit(a3),d0
    jsr _LVOFreeSignal(a6)
    clr.w cdv_open(a3)
    clr.w cdv_playing(a3)
.done
    rts

;Cancel any outstanding play request and reclaim the IORequest.
;
;AbortIO on its own only marks the request; the drive can take a good fraction of a second
;to actually stop, and it is WaitIO that sees the abort through. An earlier version polled
;CheckIO a bounded number of times instead, on the theory that Blitz mode has killed the
;interrupts a blocking Wait() depends on. That theory was wrong - the Blitz CD32 library's
;own StopCD32 is AbortIO followed by WaitIO and was called from Blitz mode for years - and
;the poll simply gave up before the drive had stopped, so music could never be stopped.
;
;On return cdv_io is always free to reuse.
;A3 = variable block. Trashes D0-D1/A0-A1/A6.
cdcom_abort
    move.w cdv_playing(a3),d0
    beq.s .done                     ;Nothing outstanding, the request is already ours
    move.l 4.w,a6
    lea cdv_io(a3),a1
    jsr _LVOAbortIO(a6)
    lea cdv_io(a3),a1
    jsr _LVOWaitIO(a6)
    clr.w cdv_playing(a3)
.done
    rts

;Busy wait for a number of vertical blanks. Used by the CDTV plugin, whose drive needs
;real settling time between commands and which may be called with interrupts disabled
;(Blitz mode), so a copper/interrupt based wait is not available.
;D0.l = frames to wait
cdcom_waitframes
    movem.l d0-d1/a0,-(sp)
    tst.l d0
    ble.s .exit
    lea $DFF004,a0
.frameloop
    ;Down into the visible area...
.waitlow
    bsr.s cdcom_vpos
    cmp.w #100,d1
    bcc.s .waitlow
    ;...then back out of it. One crossing per frame on both PAL and NTSC.
.waithigh
    bsr.s cdcom_vpos
    cmp.w #200,d1
    bcs.s .waithigh
    subq.l #1,d0
    bgt.s .frameloop
.exit
    movem.l (sp)+,d0-d1/a0
    rts

;Full 9 bit beam line into D1.w. A0 must point at VPOSR ($DFF004) so that the long read
;picks up VPOSR:VHPOSR together and V8 is not lost.
cdcom_vpos
    move.l (a0),d1
    lsr.l #8,d1
    and.w #$01FF,d1
    rts

    even
cdcom_vars
    ds.b cdv_size
