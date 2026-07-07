; Mega Drive XGM sound driver (part of SGDK by Stef), based on the NextBasic
; implementation of XGM, thanks to Luiz Mendoza and Matheus Castellar.

    include "audio_api_header.asm"

Z80_DRV_COMMAND equ $A00100
Z80_DRV_PARAMS  equ $A00104


; Work area offsets (relative to megadrive_workarea_pointer)
xgm_SongPtr     equ 0   ; long: current XGM song data pointer
xgm_PCMId       equ 4   ; long: PCM id counter (initialized to $40)
xgm_PALFlag     equ 8   ; byte: non-zero = PAL
xgm_PALCounter  equ 9   ; byte: PAL vblank timing counter (cycles 0-4)
xgm_State       equ 10  ; byte: 0=stopped, 1=paused, 2=playing
; equ 11: padding for word alignment
xgm_SampleTable equ 12  ; 256 bytes: sample ID table built by XGM_StartPlayMusic
;                          (64 entries x 4 bytes: addr_hi, addr_lo, len_hi, len_lo)

WorkAreaMemory equ xgm_SampleTable+(256*4)

_ScorpionAPI_ConstWorkAreaMemory equ WorkAreaMemory
_ScorpionAPI_ConstMaxVolume equ 255

; Install the XGM driver
; D0 = PAL flag (non-zero = PAL)
_ScorpionAPI_Install
    movem.l A2,-(SP)
    move.l megadrive_workarea_pointer,A2

    move.l #$40,xgm_PCMId(A2)
    tst.l D0
    sne.b xgm_PALFlag(A2)
    clr.b xgm_PALCounter(A2)
    clr.b xgm_State(A2)
    clr.l xgm_SongPtr(A2)

    movem.l (SP)+,A2

    ; XGM_init expects: a0=driver, d0=size-1, d1=null_sample_addr
    lea xgm_z80_driver,A0
    move.l #xgm_z80_driver_end-xgm_z80_driver-1,D0
    lea xgm_null_sample,A1      ; -pic: must use lea, not move.l #label,Dn (absolute relocation not allowed)
    move.l A1,D1
    bsr XGM_init

    moveq #1,D0
    rts

_ScorpionAPI_Uninstall
    rts

; Play or resume depending on prior state
_ScorpionAPI_Play
    movem.l A0-A2,-(SP)
    move.l megadrive_workarea_pointer,A1

    cmp.b #1,xgm_State(A1)
    beq.s @xgm_play_resume

    tst.l xgm_SongPtr(A1)
    move.b #2,xgm_State(A1)
    lea xgm_SampleTable(A1),A0
    lea xgm_null_sample,A2
    move.l xgm_SongPtr(A1),A1    ; last: overwrites work area ptr
    bsr XGM_StartPlayMusic
    bra.s @xgm_play_done

@xgm_play_resume:
    move.b #2,xgm_State(A1)
    bsr XGM_ResumePlayMusic

@xgm_play_done:
    movem.l (SP)+,A0-A2
    rts

_ScorpionAPI_Pause
    movem.l A0,-(SP)
    move.l megadrive_workarea_pointer,A0
    move.b #1,xgm_State(A0)
    movem.l (SP)+,A0
    bra XGM_PausePlayMusic

_ScorpionAPI_Stop
    movem.l A0,-(SP)
    move.l megadrive_workarea_pointer,A0
    clr.b xgm_State(A0)
    clr.l xgm_SongPtr(A0)
    movem.l (SP)+,A0
    bra XGM_PausePlayMusic

; Store song for Play to use
; D0 = Song id (unused by XGM)
; D1 = Song position (unused by XGM)
; A0 = Song data (XGM format)
; A1 = Sample table buffer (unused by XGM; table is embedded in the work area)
_ScorpionAPI_InitSong
    movem.l A1,-(SP)
    move.l megadrive_workarea_pointer,A1
    move.l A0,xgm_SongPtr(A1)
    movem.l (SP)+,A1
    rts

; Trigger a PCM sound effect
; A0 = SFX structure (see audio_api_header.asm)
; Note: sound_length should be pre-divided by 256 per XGM convention
_ScorpionAPI_SFX
    movem.l A1-A2,-(SP)
    move.w sound_length(A0),D1
    move.l sound_pointer(A0),A1

    ; D2 = channel[1:0] | priority[5:2]
    moveq #0,D2
    move.b sound_channel(A0),D2
    bpl.s @xgm_sfx_ch
    clr.b D2                       ; -1 (auto) → channel 0
@xgm_sfx_ch:
    and.b #3,D2
    moveq #0,D0
    move.b sound_priority(A0),D0
    cmp.b #15,D0
    ble.s @xgm_sfx_pri
    moveq #15,D0                   ; clamp to XGM max priority
@xgm_sfx_pri:
    lsl.b #2,D0
    or.b D0,D2

    move.l megadrive_workarea_pointer,A2
    lea xgm_PCMId(A2),A2
    bsr XGM_PlayPCM

    movem.l (SP)+,A1-A2
    rts

; Stop a PCM channel by playing the null sample on it at max priority
; D0 = channel (0-3)
_ScorpionAPI_SFX_Stop
    movem.l A1-A2,-(SP)
    lea xgm_null_sample,A1
    moveq #1,D1                    ; null sample is 256 bytes, length pre-divided by 256 = 1
    move.l D0,D2
    and.l #3,D2                    ; channel in bits 0-1
    or.l #(15<<2),D2               ; max priority (15) in bits 2-5 to override current sound
    move.l megadrive_workarea_pointer,A2
    lea xgm_PCMId(A2),A2
    bsr XGM_PlayPCM
    movem.l (SP)+,A1-A2
    rts

_ScorpionAPI_MasterVolume
    ; XGM has no simple master volume command
    rts

_ScorpionAPI_MusicChannels
    rts

_ScorpionAPI_MusicMask
    rts

; VBlank update — advances XGM internal clock
; Sends 2 ticks every 5th vblank on PAL to maintain 60Hz equivalent
_ScorpionAPI_VBlank
    movem.l A0,-(SP)
    move.l megadrive_workarea_pointer,A0

    moveq #1,D0
    tst.b xgm_PALFlag(A0)
    beq.s @xgm_vblank_send

    move.b xgm_PALCounter(A0),D0
    addq.b #1,D0
    cmp.b #5,D0
    blt.s @xgm_pal_store
    clr.b D0
@xgm_pal_store:
    move.b D0,xgm_PALCounter(A0)
    moveq #1,D0
    tst.b xgm_PALCounter(A0)       ; counter just rolled to 0 = 5th vblank
    bne.s @xgm_vblank_send
    moveq #2,D0

@xgm_vblank_send:
    bsr XGM_vint
    movem.l (SP)+,A0
    moveq #0,D0
    rts

_ScorpionAPI_EnableDMAProtection
    bra XGM_EnableProtection


; ============================================================
; Internal XGM driver routines
; ============================================================

; Initialize: load Z80 driver and set up null sample
; a0 = Z80 driver location
; d0 = Z80 driver size in bytes
; d1 = Null sample address (256-byte aligned)
XGM_init
    move.w  #$100,($A11100)
    move.w  #$100,($A11200)

@z80_wait1:
    move.w  ($A11100),d2
    btst    #8,d2
    bne     @z80_wait1

    MOVE.L  #$A00000,a1

@loop:
    MOVE.B  (a0)+,(a1)+
    DBRA    d0,@loop

    move.l  #$A01C00,a0

    lsr.l   #8,d1
    move.b  d1,(a0)+
    lsr.l   #8,d1
    move.b  d1,(a0)+
    move.b  #$01,(a0)+
    move.b  #$00,(a0)

    move.w  #$000,($A11200)
    move.w  #$000,($A11100)

    move.l  #$A00102,a0

@test_ready:
    move.w  #100,d0

@wait:
    DBRA    d0,@wait

    move.w  #$100,($A11100)
    move.w  #$100,($A11200)

@z80_wait2:
    move.w  ($A11100),d0
    btst    #8,d0
    bne     @z80_wait2

    move.b (a0),d0
    move.w  #$000,($A11100)

    btst   #7,d0
    beq    @test_ready
    rts


; VBlank tick
; d0.b = number of frames to advance
XGM_vint
@z80_wait1_XGM_vint:
    move.w  #$100,($A11100)
    move.w  ($A11100),d1
    btst    #8,d1
    bne     @z80_wait1_XGM_vint

    move.b  #0,(Z80_DRV_PARAMS+$D)

    tst.b ($A00112)
    beq XGM_vint_ready

    move.w  #$000,($A11100)
    movem.l d0-d3,-(sp)
    movem.l (sp)+,d0-d3
    bra XGM_vint

XGM_vint_ready:
    move.b d0,($A00113)
    move.w  #$000,($A11100)
    rts


; Returns non-zero in D0 if music is currently playing
XGM_IsPlayingMusic
    move.w  #$100,($A11100)
    move.w  #$100,($A11200)

@XGM_IsPlayingMusic_z80_wait1:
    move.w  ($A11100),d0
    btst    #8,d0
    bne     @XGM_IsPlayingMusic_z80_wait1

    move.b  ($A00102),d0
    andi.l  #$40,d0
    move.w  #$000,($A11100)
    rts


; Start playing a song
; a0 = sample table buffer
; a1 = XGM song data
; a2 = null sample pointer
XGM_StartPlayMusic
    move.w  #$100,($A11100)
    move.w  #$100,($A11200)

@XGM_StartPlayMusic_z80_wait1:
    move.w  ($A11100),d0
    btst    #8,d0
    bne     @XGM_StartPlayMusic_z80_wait1
    moveq   #0,d0

@XGM_Playmusic_Loop:
    move.w  d0,d1
    add.w   d1,d1
    add.w   d1,d1
    moveq   #0,d2
    move.w  0(a1,d1.w),d2
    rol.w   #8,d2

    cmp.w   #$FFFF,d2
    bne     @not_null

    move.l  a2,d2
    jmp     @addr_done

@not_null:
    addq.w  #1,d2
    lsl.l   #8,d2
    add.l   a1,d2

@addr_done
    lsr.l   #8,d2
    move.b  d2,0(a0,d1.w)
    lsr.w   #8,d2
    move.b  d2,1(a0,d1.w)
    move.w  2(a1,d1.w),2(a0,d1.w)

    addq.w  #1,d0
    cmp.w   #$3F,d0
    bne     @XGM_Playmusic_Loop

    move.l  #$A01C04,a2
    lsl.w   #2,d0
    subq.w  #1,d0

@sampleIdLoop:
    move.b (a0)+,(a2)+
    dbra   d0,@sampleIdLoop

    move.l  a1,d0
    add.l   #$100,d0

    moveq   #0,d2
    move.w  $FC(a1),d2
    rol.w   #8,d2
    lsl.l   #8,d2

    add.l   d2,d0
    addq.l  #4,d0

    move.l  #Z80_DRV_PARAMS,a2

    move.b  d0,0(a2)
    lsr.l   #8,d0
    move.b  d0,1(a2)
    lsr.l   #8,d0
    move.b  d0,2(a2)
    lsr.l   #8,d0
    move.b  d0,3(a2)

    or.b    #$40,(Z80_DRV_COMMAND)
    move.w  #$000,($A11100)
    rts


XGM_ResumePlayMusic
    move.w  #$100,($A11100)
    move.w  #$100,($A11200)

@XGM_ResumePlayMusic_z80_wait1:
    move.w  ($A11100),d0
    btst    #8,d0
    bne     @XGM_ResumePlayMusic_z80_wait1

    or.b    #$20,(Z80_DRV_COMMAND)
    move.w  #$000,($A11100)
    rts


XGM_PausePlayMusic
    move.w  #$100,($A11100)
    move.w  #$100,($A11200)

@XGM_PausePlayMusic_z80_wait1:
    move.w  ($A11100),d0
    btst    #8,d0
    bne     @XGM_PausePlayMusic_z80_wait1

    or.b    #$10,(Z80_DRV_COMMAND)
    move.w  #$000,($A11100)
    rts


; Play a silent track (effectively stops music)
; d0 = address of empty/silent XGM file
XGM_StopPlayMusic
    move.w  #$100,($A11100)
    move.w  #$100,($A11200)

@XGM_StopPlayMusic_z80_wait1:
    move.w  ($A11100),d1
    btst    #8,d1
    bne     @XGM_StopPlayMusic_z80_wait1

    move.l  #Z80_DRV_PARAMS,a2

    move.b  d0,0(a2)
    lsr.l   #8,d0
    move.b  d0,1(a2)
    lsr.l   #8,d0
    move.b  d0,2(a2)
    lsr.l   #8,d0
    move.b  d0,3(a2)

    and.b   #$f,(Z80_DRV_COMMAND)
    or.b    #$40,(Z80_DRV_COMMAND)
    move.w  #$000,($A11100)
    rts


XGM_IsPlayingPCM
    move.w  #$100,($A11100)
    move.w  #$100,($A11200)

@XGM_IsPlayingPCM_z80_wait1:
    move.w  ($A11100),d1
    btst    #8,d1
    bne     @XGM_IsPlayingPCM_z80_wait1

    and.b   ($A00102),d2
    moveq   #0,d7
    move.b  d2,d0
    move.w  #$000,($A11100)
    rts


; Play a PCM sound effect
; a1 = sample pointer (256-byte aligned)
; a2 = pointer to PCM id counter longword (initial value $40)
; d1 = sample length (pre-divided by 256)
; d2 = channel[1:0] | priority[5:2]
XGM_PlayPCM
    move.w  #$100,($A11100)
    move.w  #$100,($A11200)

@XGM_PlayPCM_z80_wait1:
    move.w  ($A11100),d0
    btst    #8,d0
    bne     @XGM_PlayPCM_z80_wait1

    move.l  (a2),d0
    lsl.l   #2,d0
    lea     $A01C00,a0
    adda.l  d0,a0

    move.l  a1,d0

    lsr.l   #8,d0
    move.b  d0,(a0)+
    lsr.w   #8,d0
    move.b  d0,(a0)+
;    lsr.l   #8,d1   ; length already divided by 256 at compile time
    move.b  d1,(a0)+
    lsr.w   #8,d1
    move.b  d1,(a0)+

    move.l  d2,d0
    and.l   #3,d0

    lea     Z80_DRV_COMMAND,a0
    moveq   #1,d1
    lsl.l   d0,d1
    or.b    d1,(a0)

    lea     $A00108,a0
    add.l   d0,d0
    adda.l  d0,a0

    move.l  d2,d0
    lsr.l   #2,d0
    and.l   #$F,d0

    move.b  d0,(a0)+

    move.l  (a2),d0
    move.b  d0,(a0)

    addq.l  #1,d0
    and.l   #$FF,d0
    or.l    #$40,d0
    move.l  d0,(a2)

    move.w  #$000,($A11100)
    rts


XGM_EnableProtection
    move.w  #$100,($A11100)
    move.w  ($A11100),d1
    btst    #8,d1
    bne     XGM_EnableProtection
    move.b  #1,(Z80_DRV_PARAMS+$D)
    move.w  #$000,($A11100)
    rts


; ============================================================
; Embedded null/silent PCM sample (256 bytes of silence)
; cnop ensures 256-byte alignment (requires plugin to be ROM-aligned to 256 bytes)
; ============================================================
    cnop 0,256
xgm_null_sample:
    dcb.b 256,0

; ============================================================
; Embedded Z80 XGM driver binary
; ============================================================
xgm_z80_driver:
    incbin "./audio_plugins/plugin_xgm_z80.bin"
xgm_z80_driver_end:
