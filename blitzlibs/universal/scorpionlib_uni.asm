; The Scorpion Universal library is designed for all Scorpion platforms (Amiga, MD, NeoGeo etc). A lot of commands here are replacements for commands that are 'built in' to Blitz Basic but call external math libraries (which obviosuly don't exist on MD or NeoGeo).
; List is faster here as it doesn't stop interrupts like Blitz Basic lists do

  include "../blitz.i"

	libheader $10,0,0,blitz_finit,0
  ; BB2 library header


;Peek a long address
  afunction long
    args long
    libs
    subs SE_PeekL,0,0
  name "SE_PeekL","Address"

;Peek a quick address
  afunction quick
    args quick
    libs
    subs SE_PeekL,0,0
  name "SE_PeekQ","Address"

;Peek a long address
  afunction word
    args long
    libs
    subs SE_PeekW,0,0
  name "SE_PeekW","Address"

;Peek a byte
  afunction byte
    args long
    libs
    subs SE_PeekB,0,0
  name "SE_PeekB","Address"

;Poke a long address
  astatement
    args long,long
    libs
    subs SE_PokeL,0,0
  name "SE_PokeL","Address"

;Poke a word
  astatement
    args long,word
    libs
    subs SE_PokeW,0,0
  name "SE_PokeW","Address"

;Poke a byte
  astatement
    args long,byte
    libs
    subs SE_PokeB,0,0
  name "SE_PokeB","Address,Byte"

;GetRegsFast is much faster than built in Blitz Basic GetRegs command as it doesn't touch the stack
  astatement
    args long
    libs
    subs 0,0,0
    args long,long
    libs
    subs 0,0,0
    args long,long,long
    libs
    subs 0,0,0
    args long,long,long,long
    libs
    subs 0,0,0	
    args long,long,long,long,long
    libs
    subs 0,0,0		
    args long,long,long,long,long,long
    libs
    subs 0,0,0		
  name "GetRegsFast","Variable,..;Put Variables to d0-d6 a2 =Newtypeaddr ???"

;Used for inline ASM on lists
  astatement
    args $20
    libs
    subs 0,0,0
  name "GetListFast","Array"

;GetRegsFast but only for word variables
  astatement
    args word
    libs
    subs 0,0,0
    args word,word
    libs
    subs 0,0,0
    args word,word,word
    libs
    subs 0,0,0
    args word,word,word,word
    libs
    subs 0,0,0	
    args word,word,word,word,word
    libs
    subs 0,0,0		
    args word,word,word,word,word,word
    libs
    subs 0,0,0		
  name "GetRegsFastW","Variable,..;Put Variables to d0-d6 a2 =Newtypeaddr ???"

;GetRegsFast but only for byte variables
  astatement
    args byte
    libs
    subs 0,0,0
    args byte,byte
    libs
    subs 0,0,0
    args byte,byte,byte
    libs
    subs 0,0,0
    args byte,byte,byte,byte
    libs
    subs 0,0,0	
    args byte,byte,byte,byte,byte
    libs
    subs 0,0,0		
    args byte,byte,byte,byte,byte,byte
    libs
    subs 0,0,0		
  name "GetRegsFastB","Variable,..;Put Variables to d0-d6 a2 =Newtypeaddr ???"

;GetRegsFast but for quick variables
  astatement
    args quick
    libs
    subs 0,0,0
    args quick,quick
    libs
    subs 0,0,0
    args quick,quick,quick
    libs
    subs 0,0,0
    args quick,quick,quick,quick
    libs
    subs 0,0,0	
    args quick,quick,quick,quick,quick
    libs
    subs 0,0,0		
    args quick,quick,quick,quick,quick,quick
    libs
    subs 0,0,0		
  name "GetRegsFastQ","Variable,..;Put Variables to d0-d6 a2 =Newtypeaddr ???"

;Scorpion internal use
  astatement
    args word,long
    libs
    subs 0,0,0
  name "AreNotOnSameTileSetup","Tile1 Tile2 Distance"

;Gets a look direction, converting X and Y values into a single direction
  afunction byte
	args quick,quick
	libs
	subs GetLookDir,0,0
  name "GetLookDir","X.q,Y.q"

;Used for encoding data into 24 bit variables
  afunction long
	args long
	libs
	subs PeekBBB,0,0
  name "PeekBBB","Address.l"

;AKA PushItem
  astatement
	args $20,long
	libs
	subs SE_PushItem,0,0
  name "SE_PushItem","ArrayName(),StackPointer"

;AKA PopItem
  astatement
	args $20,long
	libs
	subs SE_PopItem,0,0
  name "SE_PopItem","ArrayName(),StackPointer"

;AKA ClearList
  astatement
	args $20
	libs
	subs SE_ClearList,0,0
  name "SE_ClearList","ArrayName()"

;AKA ResetList
  astatement
	args $20
	libs
	subs SE_ResetList,0,0
  name "SE_ResetList","ArrayName()"

;AKA FirstItem
  afunction long
	args $20
	libs
	subs do_FirstItem,0,0
  name "SE_FirstItem","ArrayName()"

;AKA LastItem
  afunction long
	args $20
	libs
	subs do_LastItem,0,0
  name "SE_LastItem","ArrayName()"

;AKA AddItem
  afunction long
	args $20
	libs
	subs SE_AddItem,0,0
  name "SE_AddItem","ArrayName()"

;AKA KillItem
  astatement
	args $20
	libs
	subs SE_KillItem,0,0
  name "SE_KillItem","ArrayName()"

;AKA SortList
  astatement
	args $20,word
	libs
	subs SE_SortList,0,0
  name "SE_SortList","ArrayName()"

;AKA NextItem
  afunction long
	args $20
	libs
	subs SE_NextItem,0,0
  name "SE_NextItem","ArrayName()"

;AKA LastItem
  afunction long
	args $20
	libs
	subs SE_PrevItem,0,0
  name "SE_PrevItem","ArrayName()"

;AKA QFrac
  afunction quick
	args quick
	libs
	subs SE_QFrac,0,0
  name "SE_QFrac","Quick"

;Word minimum
  afunction word
	args long,long
	libs
	subs WMin,0,0
  name "WMin","A,B"

;Convert quick to int
  afunction word
	args quick
	libs
	subs SE_Int,0,0
  name "SE_Int","Quick"

;Wraps a value around if it exceeds min or max
  afunction quick
	args quick, quick, quick
	libs
	subs SE_QWrap,0,0
  name "SE_QWrap","(Quick,Low,High)"

;Limits a quick between low and high values
  afunction quick
	args quick, quick, quick
	libs
	subs SE_QLimit,0,0
  name "SE_QLimit","(Quick,Low,High)"

;Gets the sign of a quick
  afunction word
	args quick
	libs
	subs SE_QSgn,0,0
  name "SE_QSgn","(Quick)"

;Limits a word between low and high values
  afunction word
	args word, word, word
	libs
	subs SE_Limit,0,0
  name "SE_Limit","(Word,Low,High)"

;Used for random number handling
  afunction quick
	args long,word
	libs
	subs SE_XORSHIFT2,0,0
	args long
	libs
	subs SE_XORSHIFT,0,0
  name "SE_XORSHIFT","&TwoLongWords,[Quick]"

;Get an absolute value of a quick
  afunction quick
  args quick
  libs
  subs SE_QAbs,0,0
  name "SE_QAbs","Quick"

;Get an absolute value of a long
  afunction long
  args long
  libs
  subs SE_QAbs,0,0
  name "SE_Abs","Long"

;Get the maximum of two words
  afunction word
	args long,long
	libs
	subs WMax,0,0
  name "WMax","A,B"

;Faster version of putreg (long)
  afunction long
    args
    libs
    subs 0,0,0
  name "PutRegFast","Put d0 to Variable var=PutD0"

;Faster version of putreg (word)
  afunction word
    args
    libs
    subs 0,0,0
  name "PutRegFastW","Put d0 to Variable var=PutD0"

;Faster version of putreg (byte)
  afunction byte
    args
    libs
    subs 0,0,0
  name "PutRegFastB","Put d0 to Variable var=PutD0"

;Faster versio of putreg (quick)
  afunction quick
    args
    libs
    subs 0,0,0
  name "PutRegFastQ","Put d0 to Variable var=PutD0"

;Finish documentation from here (some of these should be in Amiga library)

  afunction long
    args long,long,long,long
    libs
    subs GetTileASMAddress,0,0
  name "GetTileASMAddress","X,Y,MapTileDataAddress,YTileLookupAddress"  
  
  afunction long
    args long,long,long
    libs
    subs GetTileASMAddressLine,0,0
  name "GetTileASMAddressLine","Y,MapTileDataAddress,YTileLookupAddress"    
  
   afunction long
    args long,long,long,long
    libs
    subs GetTileASM,0,0
  name "GetTileASM","X,Y,MapTileDataAddress,YTileLookupAddress"  
  
   afunction byte
    args long,long,long,long
    libs
    subs GetTileASM_B,0,0
  name "GetTileASM_B","X,Y,MapTileDataAddress,YTileLookupAddress"  
  
   afunction quick
    args quick,quick
    libs
    subs QMin,0,0
  name "QMin","V1,V2"  

   afunction quick
    args quick,quick
    libs
    subs QMax,0,0
  name "QMax","V1,V2"    

	afunction word ;Returns word in order to be positive
		args long,long,long,long
		libs
		subs GetTileB,0,0
	name "GetTileB","X.l,Y.l,ArrayAddress.l,YTileLookupAddress.l"

	afunction long
		args long,long,long,long
		libs
		subs GetTileB_Address,0,0
	name "GetTileB_Address","X.l,Y.l,ArrayAddress.l,YTileLookupAddress.l"

	afunction long 
		args long,long,long,long,long
		libs
		subs GetMapBlock,0,0
	name "GetMapBlock","X.l,Y.l,ArrayAddress.l,YTileLookupAddress.l,ProjectBlockAddress.l"

	afunction long 
		args long,long,long,long,long
		libs
		subs GetMapSlope,0,0
	name "GetMapSlope","X.l,Y.l,ArrayAddress.l,YTileLookupAddress.l,ProjectBlockAddress.l"

	afunction word ;Returns word in order to be positive
		args long,long,long,long,byte
		libs
		subs SetTileB,0,0
	name "SetTileB","X.l,Y.l,ArrayAddress.l,YTileLookupAddress.l,D.b"

	astatement
		args long,word,word
		libs
		subs PushTileToQueue,0,0
	name "PushTileToQueue","*BQ.BufferQueue,X,Y"

	afunction word
		args long,long
		libs
		subs RectOverlap,0,0
	name "RectOverlap","*Rect1,*Rect2"

	astatement
		args long,long
		libs
		subs Copy_VarData,0,0
	name "Copy_VarData","Source, Destination"  

	astatement
		args long,long
		libs
		subs Copy_VarData_NoOrigin,0,0
	name "Copy_VarData_NoOrigin","Source, Destination"  

	astatement
		args word,long
		libs
		subs DeleteNewType,0,0
	name "DeleteNewType","Size.w, Address.l"  

	astatement
		args word,long,long
		libs
		subs CopyNewType,0,0
	name "CopyNewType","Size.w, Address1.l, Address2.l"    

	astatement
		args
		libs
		subs DoIllegal,0,0
	name "DoIllegal",""    

  afunction word
    args long
    libs
    subs SqrRoot,0,0
    name "SqrRoot",""


	afunction quick
		args quick,quick,quick
		libs
		subs FixDownRightAcceleration,0,0
	name "FixDownRightAcceleration","Speed.q,Acceleration.q,MaxSpeed.q"

	afunction quick
		args quick,quick,quick
		libs
		subs FixUpLeftAcceleration,0,0
	name "FixUpLeftAcceleration","Speed.q,Acceleration.q,MaxSpeed.q"

	astatement
		args long,long
		libs
		subs zx0_decompress,0,0
	name "ZX0_Decompress","Source,Destination"

	astatement
		args long
		libs
		subs Call,0,0
	name "Call","Destination"

blitz_finit:
	nullsub _blitz_ahx_lib_finit,0,0
	libfin

_blitz_ahx_lib_finit:
 	rts	  

  include "scorpionlib_uni_functions.asm" 
