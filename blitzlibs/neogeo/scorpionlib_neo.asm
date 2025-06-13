  include       "..\blitz.i"

  libheader     $13,0,0,blitz_finit,0
  ; BB2 library header
	
  astatement
  args          
  libs
  subs          SE_NEO_FakeAllocMem,0,0    
  name          "SE_NEO_FakeAllocMem",""        

  astatement
  args          word,word,long
  libs
  subs          SE_NEO_Tile,0,0    
  name          "SE_NEO_Tile","Sprite,Position,Source Address"    

  astatement
  args          word,word
  libs
  subs          SE_Neo_SpriteX,0,0    
  name          "SE_Neo_SpriteX","Sprite,X"   

  astatement
  args          word,word
  libs
  subs          SE_Neo_SpriteHeight,0,0    
  name          "SE_Neo_SpriteHeight","Sprite,Height"

  astatement     
  args          word
  libs
  subs          SE_Neo_SpriteStick,0,0   
  name          "SE_Neo_SpriteStick","Sprite ID"

  astatement
  args          
  libs
  subs          SE_NEO_ClearVDP,0,0    
  name          "SE_NEO_ClearVDP",""      

  afunction     word
  args 
  libs
  subs          SE_NEO_Gamepad1,0,0    
  name          "SE_NEO_Gamepad1",""        

  afunction     word
  args 
  libs
  subs          SE_NEO_Gamepad2,0,0    
  name          "SE_NEO_Gamepad2","" 

  afunction     word
  args
  libs
  subs          SE_NEO_Gamepad3,0,0
  name          "SE_NEO_Gamepad3",""

  afunction     word
  args
  libs
  subs          SE_NEO_Gamepad4,0,0   
  name          "SE_NEO_Gamepad4",""

  afunction word     
  args          word,byte,word,byte
  libs
  subs          SE_Neo_ShrinkSprite,0,0   
  name          "SE_Neo_ShrinkSprite","Shrink X,Shrink Y,Sprite ID,Sprite Width"

  afunction word     
  args          word,byte
  libs
  subs          SE_Neo_ShrinkSprite_YAmount,0,0   
  name          "SE_Neo_ShrinkSprite_YAmount","Shrink Y,Sprite Height"

;MEZZ ESTATE INTERFACE
  astatement     
  args          byte,byte
  libs
  subs          MZS_send_user_command,0,0   
  name          "MZS_send_user_command","command, parameter"

    ; d0.w is the group's overall x shrink value ($00~$FF)
    ; d1.b is the group's y shrink value ($00~$FF)
    ; d2.w is the first sprite's SCB2 VRAM address ($8000+)
    ; d3.b is the group's width in sprites

blitz_finit:
  nullsub       _blitz_ahx_lib_finit,0,0
  libfin

_blitz_ahx_lib_finit:
  rts	  

  include "scorpionlib_neo_functions.asm"