::Build the universal Scorpion library (Amiga/MD/NeoGeo)
vasmm68k_mot -kick1hunks -Fhunkexe -nosym -o .\blitzlibs\bin\scorpionlib.obj .\blitzlibs\universal\scorpionlib_uni.asm
vasmm68k_mot -kick1hunks -Fhunkexe -nosym -o .\blitzlibs\bin\scorpionlib_neo.obj .\blitzlibs\neogeo\scorpionlib_neo.asm

