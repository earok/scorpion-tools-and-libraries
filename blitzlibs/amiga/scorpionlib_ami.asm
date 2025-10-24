  include "..\blitz.i"

	libheader $11,0,0,blitz_finit,0
	; BB2 library header

  afunction long
    args
    libs
    subs GetVBR,0,0
  name "GetVBR",""

  astatement
    args
    libs
    subs FlushCaches,0,0
  name "FlushCaches",""

	afunction long
    args long,word,word,word,word,long
    libs
    subs CalculateCtrl_MP,0,0
  name "CalculateCtrl_MP","EndWords.l,Nudge.w,X.w,StartY.w,EndY.w,Offset.l"  

	afunction long
    args long,word,word,long
    libs
    subs CalculatePos_MP,0,0
  name "CalculatePos_MP","Destination.l,X.w,Y.w,Offset.l"  

	astatement
		args long,long,long,long,quick
		libs
		subs SetPFrame,0,0
	name "SetPFrame","Offset Pointer,Table Pointer,Sprite Pointer,Line Pointer,Amount"  
	
	astatement
		args word,long,long
		libs
		subs SetPCloud,0,0
	name "SetPCloud","Offset,Pointer to SPR6,Pointer to SPR7"   

	astatement
		args word, word
		libs
		subs DoMaskBlit2,0,0
	name "DoMaskBlit2","DrawX, MinTerm"  

	astatement
		args word, word, long
		libs
		subs DoMaskBlit2FirstWordMask,0,0
	name "DoMaskBlit2FirstWordMask","DrawX, MinTerm"  

	astatement
		args word, word, word, word
		libs
		subs DoBarBlit,0,0
	name "DoBarBlit","DrawX, MinTerm, FirstWordMask, LastWordMask"  

	astatement
		args word,long,long,long,long,word
		libs
		subs 0,0,0
	name "DoSlicedBlit","DrawX, MinTerm, ImagePointer, HeightAdjust, DestDelta, Bitplanes"  

	astatement
    args long,long,long,long,long
    libs
    subs DoBlockBlit,0,0
  name "DoBlockBlit","source.l,dest.l,tilesize.l,maskaddress.l,tilesource.l"  

	astatement
    args long,long,word,long,word
    libs
    subs DoMaskTileBlit,0,0
  name "DoMaskTileBlit","source.l,dest.l,tilesize.w,mask.l,ScreenModulo.w"  

;	astatement
   ; args long,long,word,word,word
  ;  libs
 ;   subs DoBlockScroll,0,0
;  name "DoBlockScroll","source.l,dest.l,Width.w,ScreenModulo.w,size.w"  

	astatement
		args long, word, word, byte
		libs
		subs PatchScorpion,0,0
	name "PatchScorpion","BConfig Address,Spr size / 2,Cut Off instruction,Sprite Subpixel adjust"

	astatement
		args word
		libs
		subs PatchScorpion_MP,0,0
	name "PatchScorpion_MP","Multiplex AGA Scan2 Mask"

	afunction byte
		args byte
		libs
		subs IsCD32Pad,0,0
	name "IsCD32Pad","Port"  

	afunction long
		args long,long
		libs
		subs ReadCD32Pad,0,0
	name "ReadCD32Pad","Port, CD32Pad Buttons"

	astatement
		args long,long,word,word,word
		libs
		subs SDisplaySprite,0,0
		name "SDisplaySprite","Pointer to copper list,pointer to sprite data,x,y,screentop"      
	
	astatement
		args long,long,long
		libs
		subs ProcessMapTiles,0,0
	name "ProcessMapTiles","First Tile Address,Num of Tiles,GFX base"  

	astatement
		args long,word,word,byte,long,long,long
		libs
		subs 0,0,0
	name "PushSpritePointer","Sprite Buffer,X,Y,TargetSprite,*ImageData,*SpriteData,*ActorData"  

	astatement
		args long,long,word
		libs
		subs 0,0,0
	name "SetupTileBlit",""

	astatement
		args long,long,long
		libs
		subs CPUBlankTileBlit,0,0
	name "CPUBlankTileBlit",""

	astatement quick
		args long,long,word
		libs
		subs 0,0,0
	name "PrepareChunkBufferDraw",""

  astatement
    args
    libs
    subs WaitVertB,0,0
  name "WaitVertB",""

;Trackdisk related functions
  astatement
    args
    libs
    subs _td_init,0,0
  name "TD_Init","Make sure to also run TD_InitMem to set the memory locations"
  ;, OPTIONAL writebuffer of 11*512 bytes

  astatement
    args long,long
    libs
    subs _td_initmem,0,0
  name "TD_InitMem","Chipram buffer of $1a00*2 bytes, Directory buffer of 512 bytes"	

 afunction long
    args long
    libs
    subs _td_selectdisk,0,0
  name "TD_SelectDisk","Disk ID (Returns Error)"

 afunction long
    args word,word,long
    libs
    subs _td_read,0,0
  name "TD_Read","FirstBlock NumBlock Destination (Returns Error). Block=512 bytes"

 afunction long
    args word,long
    libs
    subs _td_format,0,0
  name "TD_Format","TrackNum Source (Returns Error)"

 astatement
    args
    libs
    subs _td_motoroff,0,0
  name "TD_MotorOff",""

blitz_finit:
	nullsub _blitz_ahx_lib_finit,0,0
	libfin

_blitz_ahx_lib_finit:
 	rts	  

  include "scorpionlib_ami_functions.asm"
  include "scorpionlib_ami_functions_trackdisk.asm"
