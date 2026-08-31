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
    move.l A0,-(SP)
    move.l megadrive_workarea_pointer,A0
    clr.l pico_Address(A0)
    move.l (SP)+,A0
    bsr.s _pico_Silence            ; also clears pico_Words
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
    clr.w pico_Words(A0)
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

    move.l A1,-(SP)
    move.l megadrive_workarea_pointer,A1
    move.l sound_pointer(A0),pico_Address(A1)
    move.w D0,pico_Words(A1)
    move.l (SP)+,A1

    ; Restart the stream. The FIFO reset has to happen before the play edge so
    ; that the preamble lands at the front of an empty FIFO rather than behind
    ; whatever the previous sound left there.
    move.w #PICO_Reset,(PICO_Control).l
    move.w #PICO_Idle,(PICO_Control).l
    move.w #PICO_Play,(PICO_Control).l

    ; Falls through to prime the FIFO now rather than waiting for the first interrupt

; VBlank update - stream pending sample words into the PICO FIFO.
; Also the body of the level 3 ADPCM interrupt, which is where most of the
; refills actually happen. PicoInterrupt masks to level 7 around this call, so
; the two callers can never overlap on pico_Address/pico_Words.
_ScorpionAPI_VBlank
    movem.l A0-A2,-(SP)
    move.l megadrive_workarea_pointer,A0

    move.w pico_Words(A0),D1
    beq.s @pico_vblank_done

    ; Read FIFO free space: lower 6 bits = free bytes, /2 = free words
    move.w (PICO_Data).l,D0
    and.w #$3F,D0
    lsr.w #1,D0
    beq.s @pico_vblank_done

    move.l pico_Address(A0),A1
    movea.l #PICO_Data,A2      ; -pic: constant hardware address, not a relocation

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

; Whole sample handed over. Drop the play bit so the FIFO going empty stops
; re-triggering level 3 - the bytes still sitting in the FIFO play out either way.
@pico_stream_end:
    move.l A1,pico_Address(A0)
    clr.w pico_Words(A0)
    move.w #PICO_Idle,(PICO_Control).l
    bra.s @pico_vblank_done
