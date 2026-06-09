::Build the universal Scorpion library (Amiga/MD/NeoGeo)
vasmm68k_mot -kick1hunks -Fhunkexe -nosym -o .\blitzlibs\bin\scorpionlib.obj .\blitzlibs\universal\scorpionlib_uni.asm

::Build the Amiga specific functions
vasmm68k_mot -kick1hunks -Fhunkexe -nosym -o .\blitzlibs\bin\scorpionlib_ami.obj .\blitzlibs\amiga\scorpionlib_ami.asm

::Build the NeoGeo specific functions
vasmm68k_mot -kick1hunks -Fhunkexe -nosym -o .\blitzlibs\bin\scorpionlib_neo.obj .\blitzlibs\neogeo\scorpionlib_neo.asm

::Build the Mega Drive specific functions
vasmm68k_mot -kick1hunks -Fhunkexe -nosym -o .\blitzlibs\bin\scorpionlib_md.obj .\blitzlibs\megadrive\scorpionlib_md.asm

::Build all of the audio plugins
vasmm68k_mot -pic -Fbin -nosym -o .\audio_api\audio_plugins\pt_audio.bin .\audio_api\plugin_ptplayer.asm
vasmm68k_mot -pic -Fbin -nosym -o .\audio_api\audio_plugins\na_audio.bin .\audio_api\plugin_na.asm
vasmm68k_mot -pic -Fbin -nosym -o .\audio_api\audio_plugins\mdsdrv_audio.bin .\audio_api\plugin_mdsdrv.asm
