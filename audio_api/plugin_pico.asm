; Sega Pico ADPCM audio driver
;
; The Pico has no Z80 and no YM2612. In their place sits a Sega 315-5641 (a
; uPD7759 with a 64 byte FIFO bolted on) at $800010. It plays a single stream of
; uPD7759 ADPCM, fed a byte at a time from that FIFO - there is no ROM address
; register, so the 68000 has to hand it every byte of the sample.
;
; The sample data itself is a uPD7759 slave stream (block headers + 4 bit
; nibbles), produced editor side by PicoAdpManager.Encode. The header byte of
; each block carries the replay rate, so there is no period to set here.
;
; Feeding is IRQ driven: the chip raises the level 3 autovector once the FIFO
; drops below its low water mark, and megadrive_header.bb points that vector at
; PicoInterrupt, which calls straight back into _ScorpionAPI_VBlank. A 64 byte
; FIFO only carries about 32 words, so a VBlank-only refill (63 bytes per frame
; = 3.7KB/s) cannot keep up with anything above ~7.5KHz - the interrupt is what
; makes the higher divider rates playable, not an optimisation.

    include "audio_api_header.asm"

PICO_Data    equ $800010  ; W: pushes both bytes of the word into the FIFO
                          ; R: bytes free in the FIFO, 0..63 (63 = empty)
PICO_Control equ $800012  ; W: control bits below. R: bit 15 = idle and drained

; Control register. Bit meanings as implemented by PicoDrive (hardware tested)
; and MAME; where the two disagree the difference does not change what we write:
;   bit 15  write 1 to reset the chip and flush the FIFO
;   bit 14  PicoDrive: level 3 FIFO interrupt enable. MAME: the uPD7759 START
;           line, whose 0->1 edge queues the stream preamble. Either way nothing
;           is heard until it is set, so it doubles as our "sound is playing" bit
;   bit 11  /RESET held high - chip out of reset and in slave (FIFO fed) mode.
;           Must stay set for as long as we want sound
;   bits 7-6  low pass filter: 0 = off, 1 = 6KHz, 2 = 9KHz, 3 = 15KHz cutoff
;   bits 2-0  attenuation, 0 = loudest
PICO_Reset   equ $8000  ; reset chip + flush FIFO
PICO_Idle    equ $0880  ; running, slave mode, 9KHz filter, full volume, silent
PICO_Play    equ PICO_Idle|$4000

; Work area offsets (relative to megadrive_workarea_pointer)
pico_Address equ 0  ; long: pointer to current position in sample data
pico_Words   equ 4  ; word: remaining words to stream to FIFO

WorkAreaMemory equ 6

_ScorpionAPI_ConstWorkAreaMemory equ WorkAreaMemory
_ScorpionAPI_ConstMaxVolume equ 0

; Install the Pico audio driver
; D0 = PAL flag (unused)
_ScorpionAPI_Install
    bsr.s _pico_Silence            ; clears pico_Address and pico_Words
    moveq #1,D0
    rts

_ScorpionAPI_Uninstall
_ScorpionAPI_SFX_Stop

; Reset the chip and leave it idle. Clearing the play bit matters as much as the
; reset does: an empty FIFO holds the level 3 interrupt asserted, so leaving it
; enabled with nothing left to send would have the 68000 servicing IRQs forever.
_pico_Silence
    move.l A0,-(SP)
    move.l megadrive_workarea_pointer,A0
    ; Order matters - level 3 can cut in between these two. Words first leaves
    ; the refill looking at a still-armed drain (harmless, the reset follows);
    ; address first would leave it streaming a live word count from address 0.
    clr.w pico_Words(A0)
    clr.l pico_Address(A0)
    move.l (SP)+,A0
    move.w #PICO_Reset,(PICO_Control).l
    move.w #PICO_Idle,(PICO_Control).l
    rts

; No music driver on Pico - the chip is one PCM stream and nothing else
_ScorpionAPI_Play
_ScorpionAPI_Pause
_ScorpionAPI_Stop
_ScorpionAPI_InitSong
_ScorpionAPI_MasterVolume
_ScorpionAPI_MusicChannels
_ScorpionAPI_MusicMask
_ScorpionAPI_EnableDMAProtection
_ScorpionAPI_DisableDMAProtection
    rts

; Trigger a PCM sound effect
; D0 = Sound ID (unused by Pico)
; A0 = SFX structure
; A1 = plugin base (must be preserved)
_ScorpionAPI_SFX
    move.w sound_length(A0),D0
    beq.s _pico_Silence            ; empty sample - do not arm the interrupt

    ; Main code runs at level 2 (see StartVBlank), so the level 3 refill can cut
    ; in part way through the restart. It must never see the new pico_Address
    ; paired with the old pico_Words: it would stream the new sample under the
    ; outgoing sound's count and leave the pointer advanced past bytes we have
    ; not sent yet, so the driver would then read off the end of the sample.
    move.w SR,-(SP)
    move.w #$2700,SR

    ; Reset first. It flushes the FIFO so the preamble lands at the front of an
    ; empty one rather than behind whatever the previous sound left there, and
    ; it drops the play bit, so the outgoing sound stops asking for data while
    ; the pointers are swapped over.
    move.w #PICO_Reset,(PICO_Control).l
    move.w #PICO_Idle,(PICO_Control).l

    move.l A1,-(SP)
    move.l megadrive_workarea_pointer,A1
    move.l sound_pointer(A0),pico_Address(A1)
    move.w D0,pico_Words(A1)
    move.l (SP)+,A1

    ; Prime the FIFO before the play edge, not after it, so the chip never sees
    ; its start with nothing to read. Worth the ~80us at level 7 that the fill
    ; costs - an SFX trigger already resets the chip.
    bsr.s _pico_Fill
    move.w #PICO_Play,(PICO_Control).l

    move.w (SP)+,SR
    moveq #0,D0
    rts

; VBlank update - stream pending sample words into the PICO FIFO.
; Also the body of the level 3 ADPCM interrupt, which is where most of the
; refills actually happen. PicoInterrupt masks to level 7 around this call, so
; the two callers can never overlap on pico_Address/pico_Words.
_ScorpionAPI_VBlank
_pico_Fill
    movem.l A0-A2,-(SP)
    move.l megadrive_workarea_pointer,A0
    movea.l #PICO_Data,A2      ; -pic: constant hardware address, not a relocation

    move.w pico_Words(A0),D1
    beq.s @pico_drain

    ; Read FIFO free space: lower 6 bits = free bytes, /2 = free words
    move.w (A2),D0
    and.w #$3F,D0
    lsr.w #1,D0
    beq.s @pico_vblank_done

    move.l pico_Address(A0),A1

@pico_stream_loop:
    move.w (A1)+,(A2)
    subq.w #1,D1
    beq.s @pico_stream_end
    subq.w #1,D0
    bne.s @pico_stream_loop

    move.l A1,pico_Address(A0)
    move.w D1,pico_Words(A0)

@pico_vblank_done:
    movem.l (SP)+,A0-A2
    moveq #0,D0
    rts

; Whole sample handed over - but up to 62 bytes of it are still queued in the
; FIFO, including the stream's own 0x00 terminator, so the play bit has to stay
; set here or the tail is cut off. pico_Address is left non-zero as the "still
; armed" flag that @pico_drain below looks at.
@pico_stream_end:
    move.l A1,pico_Address(A0)
    clr.w pico_Words(A0)
    bra.s @pico_vblank_done

; Nothing left to send. Wait for the chip to report drained before dropping the
; play bit, and pad the FIFO with 0x00 terminators until it does: that keeps the
; FIFO above its low water mark, so level 3 stays quiet through the tail instead
; of retriggering hundreds of times on a FIFO we have nothing left to fill. The
; chip stops at the encoder's own terminator, so the padding is never decoded.
@pico_drain:
    tst.l pico_Address(A0)
    beq.s @pico_vblank_done        ; already disarmed, nothing is playing

    tst.w (PICO_Control).l
    bmi.s @pico_drained            ; bit 15 = idle and drained

    move.w (A2),D0
    and.w #$3F,D0
    lsr.w #1,D0
    beq.s @pico_vblank_done
@pico_pad_loop:
    clr.w (A2)
    subq.w #1,D0
    bne.s @pico_pad_loop
    bra.s @pico_vblank_done

@pico_drained:
    clr.l pico_Address(A0)
    move.w #PICO_Idle,(PICO_Control).l
    bra.s @pico_vblank_done
