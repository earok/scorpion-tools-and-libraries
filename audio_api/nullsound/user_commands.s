;;;
;;; Scorpion Engine - NullSound template command jump table
;;;
;;; All user commands (0x04-0x7F) point to snd_command_unused (safe no-op).
;;; The Scorpion C# compiler patches these at build time with per-game handlers.
;;;
;;; Commands 0x00-0x03 are reserved by NullSound:
;;;   0x00 - unused
;;;   0x01 - prepare for ROM switch (handled in NMI, stub here is never reached via FIFO)
;;;   0x02 - eye catcher music hook (no music in template; startup skips gracefully)
;;;   0x03 - reset driver (handled in NMI, stub here is never reached via FIFO)
;;;

        .module nullsound

        .include "helpers.inc"

        .area CODE

cmd_jmptable::
        jp      snd_command_unused      ; 0x00 - unused
        jp      snd_command_unused      ; 0x01 - ROM switch stub
        jp      snd_command_unused      ; 0x02 - eye catcher stub (no music in template)
        jp      snd_command_unused      ; 0x03 - reset stub
        ;; 0x04..0x7F: safe stubs - patched per-game by the C# compiler
        init_unused_cmd_jmptable
