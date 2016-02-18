;EnableExplicit
; yes this is the legit clientD source code please report bugfixes/modifications/feature requests to sD/trtukz on skype
CompilerIf #PB_Compiler_OS <> #PB_OS_Windows
  #MB_ICONERROR=0
CompilerEndIf
;- Defining Structure
Structure CharacterArray
  StructureUnion
    c.c[0]
    s.s{1}[0]
  EndStructureUnion
EndStructure

Structure Evidence
  type.w
  name.s
  desc.s
  image.s
EndStructure

Structure sstruct
  name.s
  desc.s
  ip.s
  port.i
EndStructure

Structure ACharacter
  name.s
  desc.s
  taken.b
  dj.b
  evinumber.w
  evidence.s
  pw.s
EndStructure

#C1 = 53761
#C2 = 32618
#BufferSize = 2048
#server=3
#master=5
Global ClientID
Global MasterID
Global chatmode=#master
Global version$="1.7.5"
Global decryptor$="33"
Global key=2
Global oBG.s="gs4"
Global EviNumber=0
Global masterip$="54.93.210.149"
Global masterport=27016
Global ChatMutex = CreateMutex()
Global CharCount
Global MusicCount
Global EvidenceCount
Global DemoPage
Global bg$
Global char$
Global text$
Global NewList Music.s()
Global NewList Servers.sstruct()
Global NewList Favorites.sstruct()
Global Dim Characters.ACharacter(100)
Global Dim Evidences.Evidence(100)

;- Initialize
CompilerIf #PB_Compiler_Debugger=0
  OnErrorGoto(?start)
CompilerEndIf
UsePNGImageDecoder()
LoadFont(0,"Tahoma",10)
LoadFont(1,"Tahoma",8)
CreateImage(42,256,192,32,#PB_Image_Transparent )
XIncludeFile "GIFanimation.pbi"
Define.w a
Global Dim Frames.GIF_Frame(0)
#CONSOLE=0
If InitNetwork() = 0
  CompilerIf #CONSOLE=0
    MessageRequester("clientD", "Can't initialize the network!",#MB_ICONERROR)
  CompilerEndIf
  End
EndIf

If InitMovie() = 0
  CompilerIf #CONSOLE=0
    MessageRequester("clientD", "Can't initialize sound!",#MB_ICONERROR)
  CompilerEndIf
EndIf

;- Include files
CompilerIf #CONSOLE=0
  IncludeFile "Form3.pbf"
CompilerEndIf

Procedure WriteReplay(string$)
  If Replays
    If ReplayOpen
      WriteStringN(3,string$) 
      WriteStringN(3,"wait")
      rline+1
      If rline>replayline
        CloseFile(3)
        ReplayOpen=0
      EndIf
    Else
      OpenFile(3,"base/replays/AAO replay "+FormatDate("%dd-%mm-%yy %hh-%ii-%ss",Date())+".txt",#PB_File_SharedRead | #PB_File_NoBuffering)
      WriteStringN(3,"decryptor#"+decryptor$+"#%")
      ReplayOpen=1
    EndIf
  EndIf
EndProcedure

ProcedureDLL.s HexToString(hex.s)
  Define str.s="",i
  For i = 1 To Len(hex.s) Step 2
    str.s = str.s + Chr(Val("$"+Mid(hex.s, i, 2)))
  Next i
  ProcedureReturn str.s
EndProcedure

ProcedureDLL.s StringToHex(str.s)
  Define StringToHexR.s = ""
  Define hexchar.s = ""
  Define x
  For x = 1 To Len(str)
    hexchar.s = Hex(Asc(Mid(str, x, 1)))
    If Len(hexchar) = 1
      hexchar = "0" + hexchar
    EndIf
    StringToHexR.s = StringToHexR + hexchar
  Next x
  ProcedureReturn StringToHexR.s
EndProcedure

ProcedureDLL.s EncryptStr(S.s, Key.u)
  Define Result.s = S.s
  Define I
  Define *S.CharacterArray = @S
  Define *Result.CharacterArray = @Result
  
  For I = 0 To Len(S.s)-1
    *Result\c[I] = (*S\c[I] ! (Key >> 8))
    Key = ((*Result\c[I] + Key) * #C1) + #C2
  Next
  
  ProcedureReturn Result.s
EndProcedure

ProcedureDLL.s DecryptStr(S.s, Key.u)
  Define Result.s = S.s
  Define I
  Define *S.CharacterArray = @S
  Define *Result.CharacterArray = @Result
  
  For I = 0 To Len(S.s)-1
    *Result\c[I] = (*S\c[I] ! (Key >> 8))
    Key = ((*S\c[I] + Key) * #C1) + #C2
  Next
  
  ProcedureReturn Result.s
EndProcedure

Procedure.s EncodeCommand(com$)
  estr$=EncryptStr(com$,key)
  encom$=StringToHex(estr$)
  ProcedureReturn "#"+encom$+"#"
EndProcedure

ProcedureDLL.s RecEscape(smes$)
  smes$=ReplaceString(smes$,"<num>","#")
  smes$=ReplaceString(smes$,"<pound>","#")
  smes$=ReplaceString(smes$,"<and>","&")
  smes$=ReplaceString(smes$,"<percent>","%")
  smes$=ReplaceString(smes$,"<dollar>","$")
  ProcedureReturn smes$
EndProcedure

ProcedureDLL.s SendEscape(smes$)
  smes$=ReplaceString(smes$,"#","<num>")
  smes$=ReplaceString(smes$,"&","<and>")
  smes$=ReplaceString(smes$,"%","<percent>")
  smes$=ReplaceString(smes$,"$","<dollar>")
  ProcedureReturn smes$
EndProcedure


Procedure Redraw(anim)
  If anim > ArraySize(Frames()) : anim = 0 : EndIf
  
  SetGadgetState(RxGifAnimator1,ImageID(0))  
  
  SetGadgetState(RxGifAnimator2,ImageID(Frames(anim)\Image))
  
  If bank
    If wit
      SetGadgetState(RxGifAnimator3,ImageID(2))
    Else
      SetGadgetState(RxGifAnimator3,ImageID(2))
    EndIf
  Else
    SetGadgetState(RxGifAnimator3,0)
  EndIf
  FreeImage(42)  
  CreateImage(42,256,192,32,#PB_Image_Transparent)
  StartDrawing(ImageOutput(42))
  DrawingMode(#PB_2DDrawing_AlphaBlend)  
  DrawingFont(FontID(0))
  If TextWidth(char$)<48
    chat=LoadImage(3, "base\misc\chat.png")
  ElseIf TextWidth(char$)<64
    chat=LoadImage(3, "base\misc\chatmed.png")
  Else
    chat=LoadImage(3, "base\misc\chatbig.png")
  EndIf
  
  DrawAlphaImage(chat,0,114)
  DrawText(4, 132,text$,RGBA(255,255,255,255),RGBA(0,0,0,0))
  DrawingFont(FontID(1))
  DrawText(6, 114,char$,RGBA(255,255,255,255),RGBA(0,0,0,0))  
  
  ;   If TextWidth(text$)>250
  ;     For i=1 To 255
  ;       stext$=Left(text$,Len(text$)-i)
  ;       If TextWidth(stext$)<250
  ;         ntext$=Right(text$,i)
  ;         Break
  ;       EndIf
  ;     Next
  ;     
  ;     DrawText(5, 132,stext$,RGBA(255,255,255,255),RGBA(0,0,0,0))
  ;     DrawText(5, 145,ntext$,RGBA(255,255,255,255),RGBA(0,0,0,0))
  ;   Else
  
  Debug text$
  ;   EndIf          
  StopDrawing()
  SetGadgetState(RxGifAnimator4,ImageID(42))
EndProcedure


Procedure DemoThings(page)
  charid=page*20
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char0,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char1,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char2,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char3,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char4,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char5,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char6,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char7,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char8,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char9,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char10,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char11,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char12,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char13,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char14,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char15,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char16,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char17,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char18,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
  charid+1
  If charid<=CharCount
    If Characters(charid)\taken
      taken$="_chosen.png"
    Else
      taken$=".png"
    EndIf
    SetGadgetState(char19,ImageID(LoadImage(#PB_Any,"base\misc\DemoThings\"+Characters(charid)\name+"_char_icon"+taken$)))
  EndIf
EndProcedure

Procedure HandleAOCommand(rawreceive$)
  Debug rawreceive$
  command$=StringField(rawreceive$,1,"#")
  Select command$
    Case "decryptor"
      decryptor$=StringField(rawreceive$,2,"#")
      key = Val(DecryptStr(HexToString(decryptor$), 322))
      Img_Form_3_2 = LoadImage(#PB_Any,"base\misc\b3_on.png")
      SetGadgetState(img21,ImageID(Img_Form_3_2))
      SendNetworkString(ClientID,EncodeCommand("HI")+"clientD#%")
    Case "PN"
      SetGadgetText(dbtxt2,"Online: "+StringField(rawreceive$,2,"#")+"/"+StringField(rawreceive$,3,"#"))
    Case "SI"
      CharCount=Val(StringField(rawreceive$,2,"#"))
      EvidenceCount=Val(StringField(rawreceive$,3,"#"))
      MusicCount=Val(StringField(rawreceive$,4,"#"))
      SetGadgetAttribute(ProgressBar1,#PB_ProgressBar_Maximum,CharCount+EvidenceCount+MusicCount)
      HideGadget(img25,1)
      HideGadget(img24,1)
      HideGadget(img23,1)
      HideGadget(img22,1)
      HideGadget(img21,1)
      HideGadget(dbtxt2,1)
      HideGadget(mmo1,1)
      HideGadget(mmo2,1)
      HideGadget(edt3,1)
      HideGadget(edt4,1)
      HideGadget(img12,1)
      HideGadget(lst2,1)
      HideGadget(imgConnectmenu,0)
      HideGadget(Label3,0)
      HideGadget(ProgressBar1,0)
      HideGadget(Button6,0)
      SetGadgetText(Label3,"Chars: 0/"+Str(CharCount))
      SendNetworkString(ClientID,EncodeCommand("askchar2")+"%")
      
    Case "DONE"
      ResizeWindow(Form_3,#PB_Ignore,#PB_Ignore,500,669)
      ResizeGadget(mmo2,500,0,200,280)
      ResizeGadget(edt3,500,280,200,21)
      ResizeGadget(edt4,500,301,90,21)
      HideGadget(mmo2,1)
      HideGadget(img25,1)
      HideGadget(img24,1)
      HideGadget(img23,1)
      HideGadget(img22,1)
      HideGadget(img21,1)
      HideGadget(dbtxt2,1)
      HideGadget(mmo1,1)
      HideGadget(edt3,1)
      HideGadget(edt4,1)
      HideGadget(img12,1)
      HideGadget(lst2,1)
      HideGadget(imgConnectmenu,1)
      HideGadget(Label3,1)
      HideGadget(ProgressBar1,1)
      HideGadget(Button6,1)
      HideGadget(Button7,0)
      HideGadget(img11,0)
      HideGadget(edt5,0)
      HideGadget(img30,0)
      HideGadget(img31,0)
      HideGadget(char0,0)
      HideGadget(char1,0)
      HideGadget(char2,0)
      HideGadget(char3,0)
      HideGadget(char4,0)
      HideGadget(char5,0)
      HideGadget(char6,0)
      HideGadget(char7,0)
      HideGadget(char8,0)
      HideGadget(char9,0)
      HideGadget(char10,0)
      HideGadget(char11,0)
      HideGadget(char12,0)
      HideGadget(char13,0)
      HideGadget(char14,0)
      HideGadget(char15,0)
      HideGadget(char16,0)
      HideGadget(char17,0)
      HideGadget(char18,0)
      HideGadget(char19,0)
      HideGadget(img17,0)
      DemoThings(0)
      
    Case "PV"      
      ResizeWindow(Form_3,#PB_Ignore,#PB_Ignore,700,669)
      HideGadget(RxGIFAnimator1,0)
      HideGadget(RxGIFAnimator2,0)
      HideGadget(RxGIFAnimator3,0)
      HideGadget(RxGIFAnimator4,0)
      HideGadget(imgBG,0)
      HideGadget(OBJbutton,0)
      HideGadget(imgHoldit,0)
      HideGadget(imgTakeThat,0)      
      HideGadget(img14,0)
      HideGadget(Edit1,0)
      HideGadget(Edit3,0)
      HideGadget(ListBox1,0)
      HideGadget(img30,1)
      HideGadget(img31,1)
      HideGadget(char0,1)
      HideGadget(char1,1)
      HideGadget(char2,1)
      HideGadget(char3,1)
      HideGadget(char4,1)
      HideGadget(char5,1)
      HideGadget(char6,1)
      HideGadget(char7,1)
      HideGadget(char8,1)
      HideGadget(char9,1)
      HideGadget(char10,1)
      HideGadget(char11,1)
      HideGadget(char12,1)
      HideGadget(char13,1)
      HideGadget(char14,1)
      HideGadget(char15,1)
      HideGadget(char16,1)
      HideGadget(char17,1)
      HideGadget(char18,1)
      HideGadget(char19,1)
      HideGadget(img17,1)
      HideGadget(edt5,1)
      HideGadget(mmo2,0)
      HideGadget(edt3,0)
      HideGadget(edt4,0)
      HideGadget(TrackBar1,0)
      
    Case "BN"
      bg$=StringField(rawreceive$,2,"#")
      
    Case "CI"
      SendNetworkString(ClientID,EncodeCommand("AN")+Str((Val(StringField(rawreceive$,2,"#"))+10)/10)+"#%")
      For c=0 To 9
        charinfo$=StringField(rawreceive$,3+c*2,"#")
        Debug charinfo$
        ca=Val(StringField(rawreceive$,2,"#"))+c
        SetGadgetState(ProgressBar1,ca)
        SetGadgetText(Label3,"Chars: "+Str(ca)+"/"+Str(CharCount))
        Debug Str(ca)+"%"
        Characters(ca)\name=StringField(charinfo$,1,"&")
        Characters(ca)\desc=StringField(charinfo$,2,"&")
        Characters(ca)\evinumber=Val(StringField(charinfo$,3,"&"))
        Characters(ca)\evidence=StringField(charinfo$,4,"&")
        Characters(ca)\pw=StringField(charinfo$,5,"&")
        Characters(ca)\dj=Val(StringField(charinfo$,6,"&"))
        Characters(ca)\taken=0
      Next
      
    Case "EI"
      SendNetworkString(ClientID,EncodeCommand("AE")+Str(Val(StringField(rawreceive$,2,"#"))+1)+"#%")
      SetGadgetState(ProgressBar1,CharCount+Val(StringField(rawreceive$,2,"#")))
      SetGadgetText(Label3,"Evidence: 0/"+Str(EvidenceCount))
      Debug Str(CharCount+Val(StringField(rawreceive$,2,"#")))+"%"
      
    Case "EM"
      SendNetworkString(ClientID,EncodeCommand("AM")+Str((Val(StringField(rawreceive$,2,"#"))+10)/10)+"#%")
      Debug Str(CharCount+EvidenceCount+(Val(StringField(rawreceive$,2,"#"))+m))+"%"
      For m=0 To 9
        AddElement(Music())
        Music()=StringField(rawreceive$,3+m*2,"#")
        AddGadgetItem(ListBox1,Val(StringField(rawreceive$,2,"#"))+m,StringField(rawreceive$,3+m*2,"#"))
        SetGadgetState(ProgressBar1,CharCount+EvidenceCount+(Val(StringField(rawreceive$,2,"#"))+m))
        SetGadgetText(Label3,"Music: "+Str(m)+"/"+Str(MusicCount))
      Next
      
    Case "CharsCheck"
      For c=0 To CharCount
        check$=StringField(rawreceive$,2+c,"#")
        Debug check$
        If check$="0"
          took=0
        Else
          took=1
        EndIf
        Characters(c)\taken=took
      Next
      
    Case "MS"
      StartProfiler()
      emote$=StringField(rawreceive$,3,"#")
      char$=StringField(rawreceive$,4,"#")
      emote2$=StringField(rawreceive$,5,"#")
      text$=StringField(rawreceive$,6,"#")
      bank$=StringField(rawreceive$,7,"#")
      If LoadImage(0,"base\background\"+bg$+"\defenseempty.png")=0
        bg$="default"
      EndIf
      wit=0
      bank=0
      Select bank$
        Case "def"
          LoadImage(0,"base\background\"+bg$+"\defenseempty.png")
          LoadImage(2,"base\background\"+bg$+"\bancodefensa.png")
          bank=1
        Case "pro"
          LoadImage(0,"base\background\"+bg$+"\prosecutorempty.png")
          LoadImage(2,"base\background\"+bg$+"\bancoacusacion.png")
          bank=1
        Case "hld"
          LoadImage(0,"base\background\"+bg$+"\helperstand.png")
        Case "hlp"
          LoadImage(0,"base\background\"+bg$+"\prohelperstand.png")
        Case "jud"
          LoadImage(0,"base\background\"+bg$+"\judgestand.png")
        Case "wit"
          LoadImage(0,"base\background\"+bg$+"\witnessempty.png")
          LoadImage(2,"base\background\"+bg$+"\estrado.png")
          bank=1
          wit=1
      EndSelect
      GIF_LoadFrames(Frames(),"base\characters\"+char$+"\(a)"+emote$+".gif")  
      Redraw(0)
      StopProfiler()
      
    Case "MC"
      Debug "base\sounds\music\"+StringField(rawreceive$,2,"#")
      If LoadMovie(1,"base\sounds\music\"+StringField(rawreceive$,2,"#"))
        If IsMovie(1)
          PlayMovie(1,0)
        EndIf
      EndIf
  EndSelect
  
EndProcedure

Procedure HandleMasterCommand(rawreceive$)
  Debug rawreceive$
  StartProfiler()
  command$=StringField(rawreceive$,1,"#")
  Select command$
    Case "CT"
      AddGadgetItem(mmo2,-1,StringField(rawreceive$,2,"#")+": "+StringField(rawreceive$,3,"#"))
    Case "servercheok"
      ClearList(Servers())
      ClearGadgetItems(lst2)
      SendNetworkString(MasterID,"askforservers#%")
    Case "SN"
      ;SN#0#74.138.163.255##27017#NotMiles' Courtroom Catastrophe#https://docs.google.com/document/d/1XhjgRc10aDbevfCYM2u_l43LjM9iyFp6Mj9mXMYgTXY/edit?usp=sharing#%
      AddElement(Servers())
      Servers()\name = StringField(rawreceive$,6,"#")
      Servers()\desc = StringField(rawreceive$,7,"#")
      Servers()\ip   = StringField(rawreceive$,3,"#")
      Servers()\port = Val(StringField(rawreceive$,5,"#"))
      AddGadgetItem(lst2,-1,Servers()\name)
      SetGadgetItemData(lst2,CountGadgetItems(lst2)-1,@Servers())
      SendNetworkString(MasterID,"SR#"+StringField(rawreceive$,2,"#")+"#%")
  EndSelect
  StopProfiler()
EndProcedure

Procedure MasterNetwork(none)
  MasterID=OpenNetworkConnection(masterip$,masterport)
  If MasterID
    Img_Form_3_10 = LoadImage(#PB_Any,"base\misc\b5_on.png")
    SetGadgetState(img23,ImageID(Img_Form_3_10))
    MasterClose=0
    *Buffer = AllocateMemory(#BufferSize)
    
    Repeat
      
      CEvent = NetworkClientEvent(MasterID)
      
      Select CEvent
        Case 0
          Delay(1)
          
        Case #PB_NetworkEvent_Disconnect 
          MasterClose = 1          
          
        Case #PB_NetworkEvent_Data ;//////////////////////////Data
          length=ReceiveNetworkData(MasterID, *Buffer, #BufferSize)
          If length
            rawreceive$=PeekS(*Buffer,length)
            Debug rawreceive$
            sc=1
            While StringField(rawreceive$,sc,"%")<>""
              subcommand$=StringField(rawreceive$,sc,"%")+"%"
              HandleMasterCommand(subcommand$)
              sc+1
            Wend
          EndIf
          
        Default
          Delay(1)
          
      EndSelect
      
    Until MasterClose = 1
    CloseNetworkConnection(MasterID)
    FreeMemory(*Buffer)
  EndIf  
EndProcedure

Procedure Network(*networkserver.sstruct)
  Debug "connecting"
  ClientID=OpenNetworkConnection(*networkserver\ip,*networkserver\port)
  If ClientID
    Debug "connected"
    NetworkClose=0
    *Buffer = AllocateMemory(#BufferSize)
    
    Repeat
      
      CEvent = NetworkClientEvent(ClientID)
      
      Select CEvent
        Case 0
          Delay(1)
          
        Case #PB_NetworkEvent_Disconnect 
          ;SwitchToMaster()
          
        Case #PB_NetworkEvent_Data ;//////////////////////////Data
          length=ReceiveNetworkData(ClientID, *Buffer, #BufferSize)
          If length
            rawreceive$=PeekS(*Buffer,length)
            Debug rawreceive$
            sc=1
            While StringField(rawreceive$,sc,"%")<>""
              subcommand$=StringField(rawreceive$,sc,"%")+"%"
              HandleAOCommand(subcommand$)
              sc+1
            Wend
          EndIf
          
        Default
          Delay(1)
          
      EndSelect
      
    Until NetworkClose = 1
    CloseNetworkConnection(ClientID)
    FreeMemory(*Buffer)
  EndIf  
EndProcedure
start:
OpenForm_3()
AddKeyboardShortcut(Form_3, #PB_Shortcut_Return, 15)
CreateThread(@MasterNetwork(),0)
*clickedServer.sstruct
Repeat
  WEvent=WaitWindowEvent()
  GadgetID = EventGadget()
  Select WEvent
    Case #PB_Event_Gadget
      Debug GadgetID
      Debug char0
      Select GadgetID
        Case lst2
          If GetGadgetState(lst2)>=0
            *clickedServer=GetGadgetItemData(lst2, GetGadgetState(lst2))
            SetGadgetText(mmo1,*clickedServer\desc)
            CreateThread(@Network(),*clickedServer)
            Img_Form_3_10 = LoadImage(#PB_Any,"base\misc\b5_on.png")
            SetGadgetState(img23,ImageID(Img_Form_3_10))
          EndIf
          
        Case img23
          If MasterID
            HandleMasterCommand("servercheok#%")
          EndIf
          
        Case img24
          ;favs
          Img_Form_3_3 = LoadImage(#PB_Any,"base\misc\b1_off.png")
          SetGadgetState(img25,ImageID(Img_Form_3_3))
          Img_Form_3_4 = LoadImage(#PB_Any,"base\misc\b2_on.png")
          SetGadgetState(img24,ImageID(Img_Form_3_4))
          ClearGadgetItems(lst2)
          ReadFile(1,"base\serverlist.txt")
          While Eof(1) = 0
            serverlist$=ReadString(1)
            Debug serverlist$
            AddElement(Favorites())
            Favorites()\name = StringField(serverlist$,3,":")
            Favorites()\desc = StringField(serverlist$,4,":")
            Favorites()\ip   = StringField(serverlist$,1,":")
            Debug StringField(serverlist$,1,":")
            Favorites()\port = Val(StringField(serverlist$,2,":"))
            Debug Val(StringField(serverlist$,2,":"))
            AddGadgetItem(lst2,-1,Favorites()\name)
            SetGadgetItemData(lst2,CountGadgetItems(lst2)-1,@Favorites())
          Wend
          CloseFile(1)
        Case img25
          ;masterlist
          Img_Form_3_3 = LoadImage(#PB_Any,"base\misc\b1_on.png")
          SetGadgetState(img25,ImageID(Img_Form_3_3))
          Img_Form_3_4 = LoadImage(#PB_Any,"base\misc\b2_off.png")
          SetGadgetState(img24,ImageID(Img_Form_3_4))
          If MasterID
            HandleMasterCommand("servercheok#%")
          EndIf
          
        Case img21
          If ClientID
            SendNetworkString(ClientID,EncodeCommand("askchaa")+"%")
          EndIf
          
        Case char0
          If ClientID
            SendNetworkString(ClientID,EncodeCommand("CC")+"#"+Str(DemoPage*20)+"#%")
          EndIf
          
        Case char1
          If ClientID
            SendNetworkString(ClientID,EncodeCommand("CC")+"#"+Str(DemoPage*20+1)+"#%")
          EndIf
          
        Case char2
          If ClientID
            SendNetworkString(ClientID,EncodeCommand("CC")+"#"+Str(DemoPage*20+2)+"#%")
          EndIf        
          
          
        Case img30
          DemoPage+1
          Debug DemoPage
          DemoThings(DemoPage)
          
        Case img31
          DemoPage-1
          DemoThings(DemoPage)
          
      EndSelect
      
    Case #PB_Event_Menu
      If EventMenu() = 15
        Select GetActiveGadget()
          Case edt3
            If chatmode=#master
              If MasterID
                SendNetworkString(MasterID,"CT#"+SendEscape(GetGadgetText(edt4))+"#"+SendEscape(GetGadgetText(edt3))+"#%")
              EndIf
            ElseIf chatmode=#server
              If ClientID
                SendNetworkString(ClientID,"CT#"+SendEscape(GetGadgetText(edt4))+"#"+SendEscape(GetGadgetText(edt3))+"#%")
              EndIf
            EndIf
            SetGadgetText(edt3,"")
        EndSelect
      EndIf
    Case #PB_Event_Timer
      RemoveWindowTimer(Form_3, 0)
      If a > ArraySize(Frames()) : a = 0 : EndIf
      If IsImage(Frames(a)\Image)
        Redraw(a)
        AddWindowTimer(Form_3, 0, Frames(a)\DelayTime * 10)
      Else
        AddWindowTimer(Form_3, 0, 100)
      EndIf
      a + 1   
  EndSelect
Until WEvent=#PB_Event_CloseWindow 


; IDE Options = PureBasic 5.31 (Windows - x86)
; CursorPosition = 551
; FirstLine = 516
; Folding = ---
; EnableXP
; EnableCompileCount = 0
; EnableBuildCount = 0