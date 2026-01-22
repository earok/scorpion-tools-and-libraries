  include "..\blitz.i"

	libheader $12,0,0,blitz_finit,0
  ; BB2 library header
	
  astatement
    args
    libs
    subs SE_MD_Fake_AllocMem,0,0		
  name "SE_MD_Fake_AllocMem","A fake version of Exec.Library's AvailMem function"

  afunction long
    args long
    libs
    subs SE_MD_Fake_AllocMem,0,0		
  name "SE_MD_Fake_AllocMem_Func","As above, but returns a value"
	
  astatement
    args
    libs
    subs SE_MD_HBlank_On,0,0		
  name "SE_MD_HBlank_On",""

  astatement
    args
    libs
    subs SE_MD_HBlank_Off,0,0		
  name "SE_MD_HBlank_Off",""

  astatement
    args
    libs
    subs SE_MD_ClearVDP,0,0	
  name "SE_MD_ClearVDP","Clears the VDP (tiles,map,palette etc)"	

  astatement
    args
    libs
    subs SE_MD_Setup,0,0	
  name "SE_MD_Setup","Initialisation process for Mega Drive"

  astatement
    args
    libs
    subs SE_MD_Stop,0,0	
  name "SE_MD_Stop","Stop the MegaDrive CPU"

  astatement
    args byte,byte
    libs
    subs SE_MD_SetPlaneSize,0,0	
  name "SE_MD_SetPlaneSize","Width,Height. 0=256 pixels 1=512pixels 3=1024pixels"

  astatement
	args word,word,word,word
	libs
	subs SE_MD_SetWindowPosition,0,0
  name "SE_MD_SetWindowPosition","X Offset,Y Offset,From Right,From Bottom"

  afunction long
    args word
    libs
    subs SE_MD_SetHorizontalScrollTable,0,0
  name "SE_MD_SetHorizontalScrollTable","HScroll Table Address. Returns value to feed into scroll function"	

  astatement
    args
    libs
    subs SE_MD_VWait,0,0
    args word
    libs
    subs SE_MD_VWait_Frames,0,0		
  name "SE_MD_VWait","MD_VWait [frames] Waits for the next vertical blank"	
	
  afunction word
    args long,quick,word,word,long,long
    libs
    subs SE_MD_FadePalette,0,0    
	name "SE_MD_FadePalette","*SPalette.Scorpion_Interface,Amount.q,FirstEntry.w,NumberOfEntries.w,Destination.l,LUT.l"        

  astatement
    args word,word ;,word,word
    libs
    subs SE_MD_Scroll,0,0
  name "SE_MD_Scroll","FG X,BG X" ;,BG X,BG Y"	

  astatement
    args word,long
    libs
    subs SE_MD_Scroll_Line,0,0
  name "SE_MD_Scroll_Line","FG X,BG X Data"	

  astatement
    args long, long, long
    libs
    subs SE_MD_WriteSRAM,0,0
  name "SE_MD_WriteSRAM","RAM Address,SRAM Address,Length, returns success"

  astatement
    args long, long, long
    libs
    subs SE_MD_ReadSRAM,0,0
  name "SE_MD_ReadSRAM","RAM Address,SRAM Address,Length, returns success"

  astatement
    args long,long,long
    libs
    subs SE_MD_LoadPatterns,0,0	
  name "SE_MD_LoadPatterns","Pattern address, first pattern index, num of patterns"
		
  astatement
    args long,long,long
    libs
    subs SE_MD_LoadPatterns_DMA,0,0	
  name "SE_MD_LoadPatterns_DMA","Pattern address, first pattern index, num of patterns"

  astatement
    args word,long
    libs
    subs SE_MD_CopyTo_VDP_W,0,0
  name "SE_MD_CopyTo_VDP_W","Word Data,Dest Address"	

  astatement
    args long,long
    libs
    subs SE_MD_CopyTo_VDP_L,0,0
  name "SE_MD_CopyTo_VDP_L","Word Data,Dest Address"	

  astatement
    args long,long,long,long
    libs
    subs SE_MD_CopyTo_VDP,0,0
  name "SE_MD_CopyTo_VDP","Source Address,Length,Dest Address,Auto Increment"	

  astatement
    args long,long,long
    libs
    subs SE_MD_DMA_Transfer,0,0
  name "SE_MD_DMA_Transfer","Source Address,Dest Address,Length"	

  afunction word
	args
	libs
	subs SE_MD_IsEAMultitap,0,0
  name "SE_MD_IsEAMultitap","Is an EA multitap present?"

  afunction word
	args
	libs
	subs SE_MD_IsSegaMultitap,0,0
  name "SE_MD_IsSegaMultitap","Is a Sega multitap present?"

  astatement
	args long,long
	libs
	subs SE_MD_ReadEAMultitap,0,0
  name "SE_MD_ReadEAMultitap","Buffer of 28 bytes,number of loops (2/7)"

  astatement
	args long
	libs
	subs SE_MD_ReadSegaMultitap,0,0
  name "SE_MD_ReadSegaMultitap","Buffer of 24 bytes"

  astatement
	args long
	libs
	subs SE_MD_SetPlaneANameTable,0,0
  name "SE_MD_SetPlaneANameTable","Set the address of Plane A Name Table"

  astatement
	args long
	libs
	subs SE_MD_SetPlaneBNameTable,0,0
  name "SE_MD_SetPlaneBNameTable","Set the address of Plane B Name Table"

  astatement
    args word
    libs
    subs SE_MD_SetSpriteTable,0,0
  name "SE_MD_SetSpriteTable","Sprite Table Address"	

  astatement
	args long
	libs
	subs SE_MD_SetWindowNameTable,0,0
  name "SE_MD_SetWindowNameTable","Set the address of Window Name Table"

  astatement
    args word, word
    libs
    subs SE_MD_ModeRegister2,0,0	
  name "SE_MD_ModeRegister2","EnableDisplay,Height (-1=240,0=224)"

  astatement
    args word, word
    libs
    subs SE_MD_ModeRegister3,0,0	
  name "SE_MD_ModeRegister3","Horizontal Scroll Mode, Vertical Scroll Mode"

  astatement
    args word,word
    libs
    subs SE_MD_ModeRegister4,0,0	
  name "SE_MD_ModeRegister4","Width (-1=320,0=256),Highlight/Shadow mode"

  afunction word
	args
	libs
	subs SE_MD_GamePad1_3Button,0,0
  name "SE_MD_GamePad1_3Button","Button State for GamePad 1 - SACBRLDU"

  afunction word
	args
	libs
	subs SE_MD_GamePad2_3Button,0,0
  name "SE_MD_GamePad2_3Button","Button State for GamePad 2 - SACBRLDU"

  afunction word
	args
	libs
	subs SE_MD_GamePad1_6Button,0,0
  name "SE_MD_GamePad1_6Button","Extended Button State for GamePad 2 - SACBRLDU"

  afunction word
	args
	libs
	subs SE_MD_GamePad2_6Button,0,0
  name "SE_MD_GamePad2_6Button","Extended Button State for GamePad 2 - MXYZSACBRLDU"


;MDSDRV calls
  astatement
	args long,long,long,long,long
	libs
	subs MDSDRV_Init,0,0
  name "MDSDRV_Init","Work area of >= 1024 bytes,Sequence pointer,PCM data pointer,DMA protection bytes,MDSDRV blob"

  astatement
	args long, long
	libs
	subs MDSDRV_Update,0,0
  name "MDSDRV_Update","Work area of >= 1024 bytes,MDSDRV blob"

  astatement
	args long,long,long,long
	libs
	subs MDSDRV_Volume,0,0
  name "MDSDRV_Volume","Volume (0-256), Priority, Work area of >= 1024 bytes,MDSDRV blob"

  astatement
	args long,long,long,long
	libs
	subs MDSDRV_GVolume,0,0
  name "MDSDRV_GVolume","Music Volume (0-256), Sound Volume (0-256), Work area of >= 1024 bytes,MDSDRV blob"

  astatement
	args long,long,long,long
	libs
	subs MDSDRV_Request,0,0
  name "MDSDRV_Request","Sound number,priority level,Work area of >= 1024 bytes,MDSDRV blob"

blitz_finit:
	nullsub _blitz_ahx_lib_finit,0,0
	libfin

_blitz_ahx_lib_finit:
 	rts	  

  include ".\scorpionlib_md_functions.asm"
  include ".\scorpionlib_md_functions_mdsdrv.asm"
