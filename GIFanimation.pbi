; Original PB code by "hagibaba" (Purebasic.fr forums) based on "loadgif.c" for ImageShop32 by John Findlay
; Gif Anim support by localmotion34 (Purebasic.fr forums)
; Updated & Extended by Dean Williams - resplace.net
; APIs free, alpha channel support and bug fixes by Niffo (Purebasic.fr forums)

; #######################################
; enhanced and optimized by Thomas <ts-soft> Schulz (www.realsource.de)
; + added Catch-Support
; + added reading of complete giffile into memory, this should be faster ;-)
; + optimized Variabletypes for better support 64-Bit Programs
; + changed Syntax of GIF_LoadFrames() to:
; GIF_LoadFrames(Array GIF_Frames.GIF_Frame(1), filename.s = "", *memory = 0, memsize = 0)
; changed MessageRequester to Debug.
; #######################################
; Please help development and report any problems or improvements/bug fixes you may have on
; if we all work together we can have some really nice GIF support in PureBasic!
; http://www.purebasic.fr/english/viewtopic.php?f=12&t=27575

;{ - Structures

CompilerIf Defined(BITMAPINFOHEADER, #PB_Structure)
CompilerElse
Structure BITMAPINFOHEADER
  biSize.l
  biWidth.l
  biHeight.l
  biPlanes.w
  biBitCount.w
  biCompression.l
  biSizeImage.l
  biXPelsPerMeter.l
  biYPelsPerMeter.l
  biClrUsed.l
  biClrImportant.l
EndStructure

#BI_RGB = 0

Structure RGBQUAD
  rgbBlue.a
  rgbGreen.a
  rgbRed.a
  rgbReserved.a
EndStructure
CompilerEndIf

Structure GIF_Frame
  Image.i
  DelayTime.w
EndStructure

Structure GIFHEADER ;Header
  ghSig.b[6] ;Signature & Version
  ghWidth.w ;Logical Screen Width
  ghHeight.w ;Logical Screen Height
  ghPkFields.b ;Global Color Table Flag
  ghBkColIndex.b ;Background Color Index
  ghAspRatio.b ;Pixel Aspect Ratio
EndStructure

Structure GIFIMAGE ;Image Descriptor
  imSep.b ;Image Separator
  imLeft.w ;Image Left Position
  imTop.w ;Image Top Position
  imWidth.w ;Image Width
  imHeight.w ;Image Height
  impkFields.b ;Local Color Table Flag
EndStructure

Structure GIFCLASS ;This is instead of using globals
  *lpBytes.Byte ;Pointer to next byte in block
  Pass.l ;First pass for interlaced images in OutLineGIF()
  Line.l ;Offset for addressing the bits in OutLineGIF()
  lpBits.l ;Scanline for bits
  pitch.l ;Bytes are rounded up for image lines
  CurrCodeSize.l ;The current code size
  BitsLeft.l ;Used in NextCodeGIF()
  BytesLeft.l ;Used in NextCodeGIF()
  CurrByte.l ;Current byte
  bUseGlobalColMap.b ;Is the color table global
  GlobColRes.l ;Color Resolution, bits '6' '5' '4'
  bImInterLace.b ;Is the image interlaced
  ImgColRes.l ;Color Resolution
EndStructure

;}

Procedure GIF_OutLine(lpPixels.l,LineLen.l,height.l,*cl.GIFCLASS)
  ;Outputs the pixel color index data to the DIB
  ;lpPixels -> Memory block that holds the color index value
  ;LineLen -> Length of the line of pixels
  ;Height -> im\imHeight
  ;Gif images are 2, 16 or 256 colors, poking the values into memory
  ;requires a different method for each case. If gif is interlaced,
  ;that is dealt with here.
  
  Protected ib.l,pixel.l,byte.l,BitCnt.l,CntBk.l,ColRes.l,Bits.l
  
  Bits=*cl\lpBits-(*cl\Line * *cl\pitch) ;Pointer to bits
  
  If *cl\bUseGlobalColMap
    ColRes=*cl\GlobColRes
  Else
    ColRes=*cl\ImgColRes
  EndIf
  
  Select ColRes
      
    Case 1
      byte=0
      For pixel=0 To LineLen-1 Step 8
        ib=0
        CntBk=7
        For BitCnt=0 To 8-1
          If PeekB(lpPixels+BitCnt+pixel)
            ib=ib | (1 << CntBk)
          EndIf
          CntBk-1
        Next
        PokeB(Bits+byte,ib)
        byte+1
      Next
      
    Case 4
      byte=0
      For pixel=0 To LineLen-1 Step 2
        ib=((PeekB(lpPixels+pixel) & 255) << 4)
        ib | (PeekB(lpPixels+pixel+1) & 255)
        PokeB(Bits+byte,ib)
        byte+1
      Next
      
    Case 8
      For pixel=0 To LineLen-1
        ib=PeekB(lpPixels+pixel) & 255
        PokeB(Bits+pixel,ib)
      Next
      
  EndSelect
  
  If *cl\bImInterLace ;Set Line for different passes when Interlaced
    
    Select *cl\Pass
        
      Case 0 ;Pass 1
        If *cl\Line<height-8
          *cl\Line+8
        Else
          *cl\Line=4 : *cl\Pass+1 ;Set Line for second pass
        EndIf
        
      Case 1 ;Pass 2
        If *cl\Line<height-8
          *cl\Line+8
        Else
          *cl\Line=2 : *cl\Pass+1 ;Set Line for third pass
        EndIf
        
      Case 2 ;Pass 3
        If *cl\Line<height-4
          *cl\Line+4
        Else
          *cl\Line=1 : *cl\Pass+1 ;Set Line for fourth pass
        EndIf
        
      Case 3 ;Pass 4
        If *cl\Line<height-2
          *cl\Line+2
        EndIf
        
    EndSelect
    
  Else ;When not Interlaced increment Line
    
    *cl\Line+1
    
  EndIf
  
EndProcedure

Procedure.l GIF_NextCode(*mem, mempos, *pos.long, Array CharBuff.b(1),Array CodeMask.l(1),*cl.GIFCLASS)
  ;Reads the next code from the data stream
  ;Returns the LZW CODE or ERROR
  
  Protected count.l,Char.l,ret.l
  
  If *cl\BitsLeft=0 ;Any bits left in byte?
    
    If *cl\BytesLeft<=0 ;If not get another block
      
      *cl\lpBytes=@CharBuff(0) ;Set byte pointer
      *cl\BytesLeft = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
      
      If *cl\BytesLeft<0
        *pos\l = mempos
        ProcedureReturn *cl\BytesLeft ;Return if error
      ElseIf *cl\BytesLeft
        For count=0 To *cl\BytesLeft-1
          Char = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
          *pos\l = mempos
          If Char<0 : ProcedureReturn Char : EndIf
          CharBuff(count)=Char ;Fill the char buffer with the new block
        Next
      EndIf
      
    EndIf
    
    *cl\CurrByte=*cl\lpBytes\b & 255 ;Get a byte
    *cl\lpBytes+1 ;Increment index pointer
    *cl\BitsLeft=8 ;Set bits left in the byte
    *cl\BytesLeft-1 ;Decrement the bytes left counter
    
  EndIf
  
  ;Shift off any previously used bits
  ret=*cl\CurrByte >> (8-*cl\BitsLeft)
  
  While *cl\CurrCodeSize>*cl\BitsLeft
    
    If *cl\BytesLeft<=0
      
      ;Out of bytes in current block
      *cl\lpBytes=@CharBuff(0) ;Set byte pointer
      *cl\BytesLeft = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
      
      If *cl\BytesLeft<0
        *pos\l = mempos
        ProcedureReturn *cl\BytesLeft ;Return if error
      ElseIf *cl\BytesLeft
        For count=0 To *cl\BytesLeft-1
          Char = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
          *pos\l = mempos
          If Char<0 : ProcedureReturn Char : EndIf
          CharBuff(count)=Char ;Fill the char buffer with the current block
        Next
      EndIf
      
    EndIf
    
    *cl\CurrByte=*cl\lpBytes\b & 255 ;Get a byte
    *cl\lpBytes+1 ;Increment index pointer
    ret | (*cl\CurrByte << *cl\BitsLeft) ;Add remaining bits to return
    *cl\BitsLeft+8 ;Set bit counter
    *cl\BytesLeft-1 ;Decrement bytesleft counter
    
  Wend
  
  *cl\BitsLeft-*cl\CurrCodeSize ;Subtract the code size from bitsleft
  ret & CodeMask(*cl\CurrCodeSize) ;Mask off the right number of bits
  *pos\l = mempos
  ProcedureReturn ret
  
EndProcedure

Procedure GIF_CreateDIImage(*dib.BITMAPINFOHEADER, TransColIndex.w)
  Protected *Bits = *dib+*dib\biSize+(*dib\biClrUsed*4)
  Protected Image = CreateImage(#PB_Any, *dib\biWidth, *dib\biHeight, 32)
  Protected.l X, Y, RGBA, Col
  Protected *Pal = *dib + *dib\biSize
  Protected.a ColInd, Alpha
  Protected.l dibPitch = *dib\biSizeImage/*dib\biHeight
  
  StartDrawing(ImageOutput(Image))
  DrawingMode(#PB_2DDrawing_AllChannels)
  
  ;   Define *BufAddr = DrawingBuffer()
  ;   Define.l BufPitch = DrawingBufferPitch()
  For Y = 0 To *dib\biHeight - 1
    For X = 0 To *dib\biWidth - 1
      
      Select *dib\biBitCount
        Case 8
          ColInd = PeekA(*Bits + Y*dibPitch+X)
        Case 4
          ;If X % 2
          ;   ColInd = PeekA(*Bits + Y*dibPitch+X) & $F
          ;Else
          ;   ColInd = PeekA(*Bits + Y*dibPitch+X) >> 4
          ;EndIf
          ColInd = PeekA(*Bits + Y*dibPitch+X/2) >> (4*(1-X%2)) & $F
      EndSelect         
      
      If ColInd = TransColIndex : Alpha = 0 : Else : Alpha = 255 : EndIf
      Col = PeekL(*Pal+ColInd*4)
      
      ; Plot
      RGBA = RGBA(Blue(Col), Green(Col), Red(Col), Alpha)
      Plot(X, *dib\biHeight-1-Y, RGBA)
      
      ;          ; Direct Buffer Write (not faster !?)
      ;          If DrawingBufferPixelFormat() & #PB_PixelFormat_32Bits_RGB
      ;             RGBA = RGBA(Blue(Col), Green(Col), Red(Col), Alpha)
      ;          ElseIf DrawingBufferPixelFormat() & #PB_PixelFormat_32Bits_BGR
      ;             RGBA = RGBA(Red(Col), Green(Col), Blue(Col), Alpha)
      ;          EndIf
      ;          If DrawingBufferPixelFormat() & #PB_PixelFormat_ReversedY
      ;             PokeL(*BufAddr + (Y*BufPitch+X*4), RGBA)
      ;          Else
      ;             PokeL(*BufAddr + ((*dib\biHeight-1-Y)*BufPitch+X*4), RGBA)
      ;          EndIf
      
    Next X
  Next Y
  
  StopDrawing()
  
  ProcedureReturn Image
  
EndProcedure

Procedure.i GIF_LoadFrames(Array GIF_Frames.GIF_Frame(1), filename.s = "", *memory = 0, memsize = 0)
  ;From "loadgif.c" for ImageShop32 by John Findlay
  ;Loads LZW Graphics Interchange Format files
  ;Uses NextCodeGIF() and OutLineGIF()
  
  Protected Dim stack.b(4096) ;Stack for storing pixels
  Protected Dim suffix.b(4096) ;Suffix table, max number of LZW codes
  Protected Dim prefix.l(4096) ;Prefix linked list (these are longs)
  Protected Dim CharBuff.b(279) ;Current block
  Protected Dim GlobalCols.l(256) ;Global colors of gif
  Protected Dim localCols.l(256) ;Local image colors of gif
  Protected Dim CodeMask.l(16) ;Masks for LZW compression algorithm
  Protected gh.GIFHEADER
  Protected im.GIFIMAGE
  Protected cl.GIFCLASS
  Protected bi.BITMAPINFOHEADER
  Protected *pal.RGBQUAD
  Protected *lpSP.Byte ;Pointer to stack
  Protected *lpBuffPtr.Byte ;Pointer to buffer
  Protected bGlobColsSorted.b ;Sort Flag  bit '3' (this is unused)
  Protected file.l,sig.s,PkFields.l,bGlobColTable.b,GlobColBytes.l
  Protected GlobColors.l,count.l,Red.l,Green.l,Blue.l
  Protected width.l,height.l,impkFields.l,bImColsSorted.b
  Protected bImColTable.b,ImgColBytes.l,LZWCodeSize.l,TopSlot.l
  Protected ClearCode.l,ImgColors.l,EndingCode.l,NewCodes.l,Slot.l
  Protected lpBUFF.l,TempOldCode.l,OldCode.l,BufCnt.l,bitcount.l
  Protected ncolors.l,Len.l,hDIB.l,cc.l,code.l
  Protected *dib.BITMAPINFOHEADER
  
  CodeMask( 0)=$0000 : CodeMask( 1)=$0001
  CodeMask( 2)=$0003 : CodeMask( 3)=$0007
  CodeMask( 4)=$000F : CodeMask( 5)=$001F
  CodeMask( 6)=$003F : CodeMask( 7)=$007F
  CodeMask( 8)=$00FF : CodeMask( 9)=$01FF
  CodeMask(10)=$03FF : CodeMask(11)=$07FF
  CodeMask(12)=$0FFF : CodeMask(13)=$1FFF
  CodeMask(14)=$3FFF : CodeMask(15)=$7FFF
  CodeMask(16)=$FFFF
  
  Protected mempos, newpos, *mem
  If filename <> ""
    ;Open the file
    file=ReadFile(#PB_Any,filename)
    If file=0
      Debug "GIF Load Error! Could not open the GIF image file for reading."
      ProcedureReturn #False
    EndIf
    ;Read the file header and logical screen descriptor
    memsize = Lof(file)
    *mem = AllocateMemory(memsize)
    If *mem = 0
      Debug "GIF Load Error! Could not allocate Memory for GIF image."
      ProcedureReturn #False
    EndIf
    ReadData(file, *mem, memsize)
    CloseFile(file)
  ElseIf *memory
    *mem = AllocateMemory(memsize)
    If *mem
      CopyMemory(*memory, *mem, memsize)
    EndIf
  EndIf
  If *mem = 0
    Debug "GIF Load Error! Could not find Memory for GIF image."
    ProcedureReturn #False    
  EndIf
  CopyMemory(*mem, gh, SizeOf(gh))
  mempos + SizeOf(gh)
  sig=PeekS(@gh\ghSig,6,#PB_Ascii) ;Get the header version string
  
  If sig<>"GIF89a" And sig<>"GIF87a"
    FreeMemory(*mem)
    Debug "GIF Load Error! File was not a valid GIF image file"
    ProcedureReturn #False ;NOT_VALID
  EndIf
  Protected.l realwidth=gh\ghWidth
  Protected.l realheight=gh\ghHeight
  
  ;Store gh\ghPkFields for bit manipulation
  PkFields=gh\ghPkFields & 255
  
  ;Global Color Table Flag bit '7'
  bGlobColTable=(PkFields & (1 << 7)) >> 7
  
  If bGlobColTable
    cl\bUseGlobalColMap=#True
    
    GlobColBytes=3*(1 << ((PkFields & $07)+1)) ;Table size in bytes
    GlobColors=GlobColBytes/3 ;Number of colors
    
    ;Some gif encoders do not follow the gif spec very well,
    ;so make cl\GlobColRes from GlobColors.
    ;Also gif's are used on different platforms, which do
    ;have different bits per pixel. i.e. 32 colors is 5 bits/pixel.
    If GlobColors<=2
      cl\GlobColRes=1
    ElseIf GlobColors<=16
      cl\GlobColRes=4
    Else
      cl\GlobColRes=8
    EndIf
    
    For count=0 To GlobColors-1 ;Get the global screen colors
      Red = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
      Green = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
      Blue = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
      GlobalCols(count)=RGB(Red,Green,Blue)
    Next
  EndIf
  
  Protected.w TransColorIndex = -1
  count=0
  While count<>$2C ;Search for im\imSep
    count = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
    If count = $F9
      mempos + SizeOf(Byte)
      Protected.b tflag = PeekB(*mem + mempos) & 1 : mempos + SizeOf(Byte)
      Protected.w delaytime = PeekW(*mem + mempos) : mempos + SizeOf(Word)
      Protected.a transparent = PeekB(*mem + mempos) : mempos + SizeOf(Byte)
      If tflag : TransColorIndex = transparent : EndIf
    EndIf
  Wend
  mempos - SizeOf(byte);Seek to im\imSep
  CopyMemory(*mem + mempos, im, SizeOf(im)) ;Read the image descriptor
  mempos + SizeOf(im)
  ;Store im\imPkFields for bit manipulation
  impkFields=im\impkFields & 255
  
  ;Is the image interlaced
  cl\bImInterLace=(impkFields & (1 << 6)) >> 6
  
  ;Is the local color table sorted
  bImColsSorted=(impkFields & (1 << 5)) >> 5
  
  ;Is there a local color table
  bImColTable=(impkFields & (1 << 7)) >> 7
  
  If bImColTable
    cl\bUseGlobalColMap=#False
    
    ImgColBytes=3*(1 << ((impkFields & $07)+1)) ;Table size in bytes
    ImgColors=ImgColBytes/3 ;Number of colors
    
    If ImgColors<=2 ;Make sure image bit depth is 1, 4 or 8
      cl\ImgColRes=1
    ElseIf ImgColors<=16
      cl\ImgColRes=4
    Else
      cl\ImgColRes=8
    EndIf
    
    For count=0 To ImgColors-1 ;Get the local image colors
      Red = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
      Green = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
      Blue = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
      localCols(count)=RGB(Red,Green,Blue)
    Next
    ;transcolor=LocalCols(TByte)
  Else ;No local color table
    If cl\bUseGlobalColMap=#False ;No global color table
      FreeMemory(*mem)
      Debug "GIF Load Error! The GIF image does not contain a valid color table."
      ProcedureReturn #False ;NO_COLORTABLE
    EndIf
  EndIf
  
  width=im\imWidth & $FFFF ;Image width
  height=im\imHeight & $FFFF ;Image height
  
  ;Get the first byte of the new block of image data.
  ;Should be the bit size
  LZWCodeSize = PeekA(*mem + mempos) : mempos + SizeOf(Byte)

  Debug LZWCodeSize
  ;Bit size is normally the same as the color resolution.
  ;i.e. 8 for 256 colors
  If LZWCodeSize<2 Or LZWCodeSize>12    
    FreeMemory(*mem)
    Debug "GIF Load Error! LZW code size is not valid!"
    ProcedureReturn #False ;BAD_CODE_SIZE
  EndIf
  
  ;Initialise the variables for the decoder for reading a new image.
  cl\CurrCodeSize=LZWCodeSize+1
  TopSlot=1 << cl\CurrCodeSize ;Highest code for current size
  ClearCode=1 << LZWCodeSize ;Value for a clear code
  EndingCode=ClearCode+1 ;Value for an ending code
  NewCodes=ClearCode+2 ;First available code
  Slot=NewCodes ;Last read code
  cl\BitsLeft=0
  cl\BytesLeft=0
  
  ;Just in case...
  TempOldCode=0 : OldCode=0
  
  ;Allocate space for the decode buffer
  lpBUFF=AllocateMemory(width+8) ;+8 just in case
  
  ;Set up the stack pointer, decode buffer pointer and line counter
  *lpSP=@stack(0)
  *lpBuffPtr=lpBUFF
  BufCnt=width ;Count for pixel line length
  
  ;Start creating the DIB
  If cl\bUseGlobalColMap ;Global color table
    bitcount=cl\GlobColRes
  Else ;Local color table
    bitcount=cl\ImgColRes
  EndIf
  
  bi\biSize=SizeOf(bi)
  bi\biWidth=width
  bi\biHeight=height
  bi\biPlanes=1
  bi\biBitCount=bitcount ;BitCount will be 1, 4 or 8
  bi\biCompression=#BI_RGB
  bi\biSizeImage=0
  bi\biXPelsPerMeter=0
  bi\biYPelsPerMeter=0
  If cl\bUseGlobalColMap ;Global color table
    bi\biClrUsed=GlobColors
  Else ;Local color table
    bi\biClrUsed=ImgColors
  EndIf
  bi\biClrImportant=0
  
  ;With the BITMAPINFO format headers, the size of the palette is
  ;in biClrUsed, whereas in the BITMAPCORE - style headers, it is
  ;dependent on the Bits per pixel (2 to the power of bitsperpixel).
  If bi\biClrUsed<>0
    ncolors=bi\biClrUsed
  Else ;We don't have an optimal palette
    ncolors=1 << bi\biBitCount
  EndIf
  
  cl\pitch=(((bitcount*width)+31) >> 5) << 2 ;Bytes per line
  Len=bi\biSize+(ncolors*4)+(cl\pitch*height) ;Size of DIB
  
  bi\biSizeImage=cl\pitch*height ;Fill in biSizeImage
  
  ;Allocate memory block to store our DIB
  hDIB=AllocateMemory(Len)
  If hDIB=0
    FreeMemory(lpBUFF)
    FreeMemory(*mem)
    Debug "GIF Load Error! Memory allocation failed!"
    ProcedureReturn #False ;NO_DIB
  EndIf
  
  ;Fill first part of DIB with the BITMAPINFOHEADER
  CopyMemory(bi,hDIB,SizeOf(bi))

  ;Set the colors in the DIB (or masks for the new DIB formats)
  *pal=hDIB+SizeOf(bi)
  If cl\bUseGlobalColMap
    For count=0 To bi\biClrUsed-1
      *pal\rgbBlue=Blue(GlobalCols(count))
      *pal\rgbGreen=Green(GlobalCols(count))
      *pal\rgbRed=Red(GlobalCols(count))
      *pal+4
    Next
  Else
    For count=0 To bi\biClrUsed-1
      *pal\rgbBlue=Blue(localCols(count))
      *pal\rgbGreen=Green(localCols(count))
      *pal\rgbRed=Red(localCols(count))
      *pal+4
    Next
  EndIf
  
  cl\Line=0 ;Set address offset for OutLineGIF()
  cl\Pass=0 ;For interlaced images in OutLineGIF()
  
  ;Image data bits of DIB
  cl\lpBits=hDIB+bi\biSize+(ncolors*4)+(cl\pitch*(height-1))
  
  ;This is the main loop. For each code we get we pass through the
  ;linked list of prefix codes, pushing the corresponding "character"
  ;for each code onto the stack. When the list reaches a single
  ;"character" we push that on the stack too, and then start
  ;unstacking each character for output in the correct order.
  ;Special handling is included for the clear code, and the whole
  ;thing ends when we get an ending code.
  While cc<>EndingCode
    
    cc=GIF_NextCode(*mem, mempos, @newpos, CharBuff(),CodeMask(),cl)
    mempos = newpos
    If cc<0 ;If a file error, return without completing the decode
      FreeMemory(lpBUFF)
      FreeMemory(*mem)
      Debug "GIF Load Error! LZW code size is Not valid!"
      ProcedureReturn #False ;FILE_ERROR
    EndIf
    
    ;If the code is a clear code, re-initialise all necessary items.
    If cc=ClearCode
      
      cl\CurrCodeSize=LZWCodeSize+1
      Slot=NewCodes
      TopSlot=1 << cl\CurrCodeSize
      
      ;Continue reading codes until we get a non-clear code
      ;(another unlikely, but possible case...)
      While cc=ClearCode
        cc=GIF_NextCode(*mem, mempos, @newpos,CharBuff(),CodeMask(),cl)
         mempos = newpos
      Wend
      
      ;If we get an ending code immediately after a clear code
      ;(yet another unlikely case), then break out of the loop.
      If cc=EndingCode
        Break ;end loop
      EndIf
      
      ;Finally, if the code is beyond the range of already set codes,
      ;(This one had better not happen, I have no idea what will
      ;result from this, but I doubt it will look good)
      ;then set it to color zero.
      If cc>=Slot
        cc=0
      EndIf
      
      OldCode=cc
      TempOldCode=OldCode
      
      ;And let us not forget to put the char into the buffer, and if,
      ;on the off chance, we were exactly one pixel from the end of
      ;the line, we have to send the buffer to the OutLineGIF() routine
      *lpBuffPtr\b=cc
      *lpBuffPtr+1
      BufCnt-1
      
      If BufCnt=0
        GIF_OutLine(lpBUFF,width,height,cl)
        *lpBuffPtr=lpBUFF
        BufCnt=width
      EndIf
      
    Else
      
      ;In this case, it's not a clear code or an ending code, so it
      ;must be a code code. So we can now decode the code into a
      ;stack of character codes (Clear as mud, right?).
      code=cc
      
      If code=Slot
        code=TempOldCode
        *lpSP\b=OldCode
        *lpSP+1
      EndIf
      
      ;Here we scan back along the linked list of prefixes, pushing
      ;helpless characters (i.e. suffixes) onto the stack as we do so.
      While code>=NewCodes
        *lpSP\b=suffix(code)
        *lpSP+1
        code=prefix(code)
      Wend
      
      ;Push the last character on the stack, and set up the new
      ;prefix and suffix, and if the required slot number is greater
      ;than that allowed by the current bit size, increase the bit
      ;size. (Note - if we are all full, we *don't* save the new
      ;suffix and prefix. I'm not certain if this is correct,
      ;it might be more proper to overwrite the last code.
      *lpSP\b=code
      *lpSP+1
      
      If Slot<TopSlot
        OldCode=code
        suffix(Slot)=OldCode
        prefix(Slot)=TempOldCode
        Slot+1
        TempOldCode=cc
      EndIf
      
      If Slot>=TopSlot
        If cl\CurrCodeSize<12
          TopSlot=TopSlot << 1
          cl\CurrCodeSize+1
        EndIf
      EndIf
      
      ;Now that we've pushed the decoded string (in reverse order)
      ;onto the stack, lets pop it off and put it into our decode
      ;buffer, and when the decode buffer is full, write another line.
      While *lpSP>@stack(0)
        *lpSP-1
        *lpBuffPtr\b=*lpSP\b
        *lpBuffPtr+1
        BufCnt-1
        
        If BufCnt=0
          GIF_OutLine(lpBUFF,width,height,cl)
          *lpBuffPtr=lpBUFF
          BufCnt=width
        EndIf
      Wend
      
    EndIf
  Wend
  
  If BufCnt<>width ;If there are any left, output the bytes
    GIF_OutLine(lpBUFF,width-BufCnt-1,height,cl)
  EndIf
  *dib=hDIB
  If *dib=0 ;Avoid errors
    ProcedureReturn #False
  EndIf
  
  Protected.i Bits=*dib+*dib\biSize+(*dib\biClrUsed*4) ;Pointer to bits
  
  ;Create the DDB bitmap
  Protected.i hImage=GIF_CreateDIImage(*dib, TransColorIndex)
  Protected.i pbimage=CreateImage(#PB_Any,realwidth,realheight, 32, #PB_Image_Transparent) ; Create initial "screen"
  StartDrawing(ImageOutput(pbimage))
  DrawingMode(#PB_2DDrawing_AlphaBlend)
  ;Box(0,0,realwidth,realheight,$FFFFFF)
  DrawImage(ImageID(hImage),im\imLeft,im\imTop,im\imWidth,im\imHeight)
  StopDrawing()
  FreeImage(hImage)
  FreeMemory(hDIB)
  ;imageArray(0)=ImageID(pbimage)
  GIF_Frames(0)\Image = pbimage
  GIF_Frames(0)\DelayTime = delaytime
  Protected.l numberimages=1
  
  ;===========================
  ;- continue to other frames
  ;===========================
  
  ; Read through the various image blocks
  Protected NotatEnd=1
  While NotatEnd=1
    TransColorIndex = -1
    Protected.i n
    While n<>$2C
      n=PeekA(*mem + mempos) : mempos + SizeOf(Byte)
      If n=$3B
        NotatEnd=0
        FreeMemory(*mem)
        FreeMemory(lpBUFF)
        ProcedureReturn numberimages
      ElseIf n=$F9
        ;Graphics control extension
        n=PeekA(*mem + mempos) : mempos + SizeOf(Byte)
        Protected.l Size = n
        n=PeekA(*mem + mempos) : mempos + SizeOf(Byte)
        ;Define.b packedfields = n &$FF
        Protected.l disposalmethod = (n & %00011100) >>2
        tflag = n & %00000001
        delaytime = PeekW(*mem + mempos) : mempos + SizeOf(Word)
        transparent = PeekB(*mem + mempos) : mempos + SizeOf(Byte)
        If tflag : TransColorIndex = transparent : EndIf
      ElseIf n=$FF
        ;application extension
      ElseIf n=$FE
        ;comment extention
        n=PeekA(*mem + mempos) : mempos + SizeOf(Byte)
        mempos + n
      ElseIf n= $01
        ;"plain text extention"
        ;Debug "text"
        ; n=ReadByte(file) & 255
        ;FileSeek(file,Loc(file)+n& $FF)
      ElseIf n =$21
        ;"A Extension_block
      EndIf
    Wend
    n=0
    
    ; done with reading the image blocks for this frame
    mempos - SizeOf(Byte)
    count=0
    While count<>$2C ;Search for im\imSep
      count = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
    Wend
    mempos - SizeOf(Byte) ;Seek to im\imSep
    CopyMemory(*mem + mempos, im, SizeOf(im)) ;Read the image descriptor
    mempos + SizeOf(im)
    ;Store im\imPkFields for bit manipulation
    impkFields=im\impkFields & 255
    
    ;Is the image interlaced
    cl\bImInterLace=(impkFields & (1 << 6)) >> 6
    
    ;Is the local color table sorted
    bImColsSorted=(impkFields & (1 << 5)) >> 5
    
    ;Is there a local color table
    bImColTable=(impkFields & (1 << 7)) >> 7
    
    If bImColTable
      cl\bUseGlobalColMap=#False
      
      ImgColBytes=3*(1 << ((impkFields & $07)+1)) ;Table size in bytes
      ImgColors=ImgColBytes/3 ;Number of colors
      
      If ImgColors<=2 ;Make sure image bit depth is 1, 4 or 8
        cl\ImgColRes=1
      ElseIf ImgColors<=16
        cl\ImgColRes=4
      Else
        cl\ImgColRes=8
      EndIf
      
      For count=0 To ImgColors-1 ;Get the local image colors
        Red = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
        Green = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
        Blue = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
        localCols(count)=RGB(Red,Green,Blue)
      Next
      ;loctranscolor=localCols(transparent& $FF)
      ;transcolor=localCols(transparent& $FF)
    Else ;No local color table
      If cl\bUseGlobalColMap=#False ;No global color table
        FreeMemory(*mem)
        Debug "GIF Load Error! The GIF image does not contain a valid color table."
        ProcedureReturn #False ;NO_COLORTABLE
      EndIf
      ;transcolor=GlobalCols(transparent& $FF)
    EndIf
    
    width=im\imWidth & $FFFF ;Image width
    height=im\imHeight & $FFFF ;Image height
    
    ;Get the first byte of the new block of image data.
    ;Should be the bit size
    LZWCodeSize = PeekA(*mem + mempos) : mempos + SizeOf(Byte)
    
    ;Bit size is normally the same as the color resolution.
    ;i.e. 8 for 256 colors
    If LZWCodeSize<2 Or LZWCodeSize>8
      FreeMemory(*mem)
      Debug "GIF Load Error! LZW code size is Not valid!"
      ProcedureReturn #False ;BAD_CODE_SIZE
    EndIf
    
    ;Initialise the variables for the decoder for reading a new image.
    cl\CurrCodeSize=LZWCodeSize+1
    TopSlot=1 << cl\CurrCodeSize ;Highest code for current size
    ClearCode=1 << LZWCodeSize ;Value for a clear code
    EndingCode=ClearCode+1 ;Value for an ending code
    NewCodes=ClearCode+2 ;First available code
    Slot=NewCodes ;Last read code
    cl\BitsLeft=0
    cl\BytesLeft=0
    
    ;Just in case...
    TempOldCode=0 : OldCode=0
    
    ;Allocate space for the decode buffer
    lpBUFF=AllocateMemory(width+8) ;+8 just in case
    
    ;Set up the stack pointer, decode buffer pointer and line counter
    *lpSP=@stack(0)
    *lpBuffPtr=lpBUFF
    BufCnt=width ;Count for pixel line length
    
    ;Start creating the DIB
    If cl\bUseGlobalColMap ;Global color table
      bitcount=cl\GlobColRes
    Else ;Local color table
      bitcount=cl\ImgColRes
    EndIf
    
    bi\biSize=SizeOf(bi)
    bi\biWidth=width
    bi\biHeight=height
    bi\biPlanes=1
    bi\biBitCount=bitcount ;BitCount will be 1, 4 or 8
    bi\biCompression=#BI_RGB
    bi\biSizeImage=0
    bi\biXPelsPerMeter=0
    bi\biYPelsPerMeter=0
    If cl\bUseGlobalColMap ;Global color table
      bi\biClrUsed=GlobColors
    Else ;Local color table
      bi\biClrUsed=ImgColors
    EndIf
    bi\biClrImportant=0
    
    ;With the BITMAPINFO format headers, the size of the palette is
    ;in biClrUsed, whereas in the BITMAPCORE - style headers, it is
    ;dependent on the Bits per pixel (2 to the power of bitsperpixel).
    If bi\biClrUsed<>0
      ncolors=bi\biClrUsed
    Else ;We don't have an optimal palette
      ncolors=1 << bi\biBitCount
    EndIf
    
    cl\pitch=(((bitcount*width)+31) >> 5) << 2 ;Bytes per line
    Len=bi\biSize+(ncolors*4)+(cl\pitch*height) ;Size of DIB
    
    bi\biSizeImage=cl\pitch*height ;Fill in biSizeImage
    
    ;Allocate memory block to store our DIB
    hDIB=AllocateMemory(Len)
    If hDIB=0
      FreeMemory(lpBUFF)
      FreeMemory(*mem)
      Debug "GIF Load Error! Memory allocation failed!"
      ProcedureReturn #False ;NO_DIB
    EndIf
    
    ;Fill first part of DIB with the BITMAPINFOHEADER
    CopyMemory(bi,hDIB,SizeOf(bi))
    ;Set the colors in the DIB (or masks for the new DIB formats)
    *pal=hDIB+SizeOf(bi)
    If cl\bUseGlobalColMap
      For count=0 To bi\biClrUsed-1
        *pal\rgbBlue=Blue(GlobalCols(count))
        *pal\rgbGreen=Green(GlobalCols(count))
        *pal\rgbRed=Red(GlobalCols(count))
        *pal+4
      Next
    Else
      For count=0 To bi\biClrUsed-1
        *pal\rgbBlue=Blue(localCols(count))
        *pal\rgbGreen=Green(localCols(count))
        *pal\rgbRed=Red(localCols(count))
        *pal+4
      Next
    EndIf
    
    cl\Line=0 ;Set address offset for OutLineGIF()
    cl\Pass=0 ;For interlaced images in OutLineGIF()
    
    ;Image data bits of DIB
    cl\lpBits=hDIB+bi\biSize+(ncolors*4)+(cl\pitch*(height-1))
    
    ;This is the main loop. For each code we get we pass through the
    ;linked list of prefix codes, pushing the corresponding "character"
    ;for each code onto the stack. When the list reaches a single
    ;"character" we push that on the stack too, and then start
    ;unstacking each character for output in the correct order.
    ;Special handling is included for the clear code, and the whole
    ;thing ends when we get an ending code.
    cc=0
    
    While cc<>EndingCode
      
      cc=GIF_NextCode(*mem, mempos, @newpos,CharBuff(),CodeMask(),cl)
      mempos = newpos
      If cc<0 ;If a file error, return without completing the decode
        FreeMemory(lpBUFF)
        FreeMemory(*mem)
        Debug "GIF Load Error!GIF image contained an in-valid LZW code."
        ProcedureReturn #False ;FILE_ERROR
      EndIf
      
      ;If the code is a clear code, re-initialise all necessary items.
      If cc=ClearCode
        
        cl\CurrCodeSize=LZWCodeSize+1
        Slot=NewCodes
        TopSlot=1 << cl\CurrCodeSize
        
        ;Continue reading codes until we get a non-clear code
        ;(another unlikely, but possible case...)
        While cc=ClearCode
          cc=GIF_NextCode(*mem, mempos, @newpos, CharBuff(), CodeMask(),cl)
          mempos = newpos
        Wend
        
        ;If we get an ending code immediately after a clear code
        ;(yet another unlikely case), then break out of the loop.
        If cc=EndingCode
          Break ;end loop
        EndIf
        
        ;Finally, if the code is beyond the range of already set codes,
        ;(This one had better not happen, I have no idea what will
        ;result from this, but I doubt it will look good)
        ;then set it to color zero.
        If cc>=Slot
          cc=0
        EndIf
        
        OldCode=cc
        TempOldCode=OldCode
        
        ;And let us not forget to put the char into the buffer, and if,
        ;on the off chance, we were exactly one pixel from the end of
        ;the line, we have to send the buffer to the OutLineGIF() routine
        *lpBuffPtr\b=cc
        *lpBuffPtr+1
        BufCnt-1
        
        If BufCnt=0
          GIF_OutLine(lpBUFF,width,height,cl)
          *lpBuffPtr=lpBUFF
          BufCnt=width
        EndIf
        
      Else
        
        ;In this case, it's not a clear code or an ending code, so it
        ;must be a code code. So we can now decode the code into a
        ;stack of character codes (Clear as mud, right?).
        code=cc
        
        If code=Slot
          code=TempOldCode
          *lpSP\b=OldCode
          *lpSP+1
        EndIf
        
        ;Here we scan back along the linked list of prefixes, pushing
        ;helpless characters (i.e. suffixes) onto the stack as we do so.
        While code>=NewCodes
          *lpSP\b=suffix(code)
          *lpSP+1
          code=prefix(code)
        Wend
        
        ;Push the last character on the stack, and set up the new
        ;prefix and suffix, and if the required slot number is greater
        ;than that allowed by the current bit size, increase the bit
        ;size. (Note - if we are all full, we *don't* save the new
        ;suffix and prefix. I'm not certain if this is correct,
        ;it might be more proper to overwrite the last code.
        *lpSP\b=code
        *lpSP+1
        
        If Slot<TopSlot
          OldCode=code
          suffix(Slot)=OldCode
          prefix(Slot)=TempOldCode
          Slot+1
          TempOldCode=cc
        EndIf
        
        If Slot>=TopSlot
          If cl\CurrCodeSize<12
            TopSlot=TopSlot << 1
            cl\CurrCodeSize+1
          EndIf
        EndIf
        
        ;Now that we've pushed the decoded string (in reverse order)
        ;onto the stack, lets pop it off and put it into our decode
        ;buffer, and when the decode buffer is full, write another line.
        While *lpSP>@stack(0)
          *lpSP-1
          *lpBuffPtr\b=*lpSP\b
          *lpBuffPtr+1
          BufCnt-1
          
          If BufCnt=0
            GIF_OutLine(lpBUFF,width,height,cl)
            *lpBuffPtr=lpBUFF
            BufCnt=width
          EndIf
        Wend
        
      EndIf
      
    Wend
    
    If BufCnt<>width ;If there are any left, output the bytes
      GIF_OutLine(lpBUFF,width-BufCnt-1,height,cl)
    EndIf
    
    ;Create the DDB bitmap
    *dib=hDIB
    If *dib=0 ;Avoid errors
      ProcedureReturn #False
    EndIf
    
    Bits=*dib+*dib\biSize+(*dib\biClrUsed*4) ;Pointer to bits
    
    ;- create the bitmap
    ;Create the DDB bitmap
    hImage = GIF_CreateDIImage(*dib, TransColorIndex)
    pbimage=CreateImage(#PB_Any,realwidth,realheight, 32, #PB_Image_Transparent)
    StartDrawing(ImageOutput(pbimage))
    DrawingMode(#PB_2DDrawing_AlphaBlend)
    ; For some retarded reason, we have to draw and redraw the GIF frames over the previous image imagenumber-1
    
    ;   If bUseGlobalColMap ; if a local color table, then draw previous image in array, and then dray new hbitmap with transparency
    ;     Box(0,0,realwidth,realheight,$FFFFFF)
    ;     DrawImage(imageArray(numberimages-1),0,0)
    ;     If tflag=1
    ;       ;loc
    ;       DrawTransparentImage(drawdc,hBitmap,im\imLeft,im\imTop,im\imWidth,im\imHeight,transcolor)
    ;     Else
    ;       DrawImage(hBitmap,im\imLeft,im\imTop,im\imWidth,im\imHeight)
    ;     EndIf
    ;   Else
    
    If disposalmethod = 1
      ;Box(0,0,realwidth,realheight,$FFFFFF)
      DrawImage(ImageID(GIF_Frames(numberimages-1)\Image),0,0)
      DrawImage(ImageID(hImage),im\imLeft,im\imTop,im\imWidth,im\imHeight)
    ElseIf disposalmethod = 2
      ;Box(0,0,realwidth,realheight,$FFFFFF)
      ;DrawImage(ImageID(GIF_Frames(1)),0,0)
      DrawImage(ImageID(hImage),im\imLeft,im\imTop,im\imWidth,im\imHeight)
    Else
      ;Box(0,0,realwidth,realheight,$FFFFFF)
      DrawImage(ImageID(GIF_Frames(numberimages-1)\Image),0,0)
      ;DrawImage(hBitmap,im\imLeft,im\imTop)
      DrawImage(ImageID(hImage),im\imLeft,im\imTop,im\imWidth,im\imHeight)
    EndIf
    ;   EndIf
    StopDrawing()
    FreeImage(hImage)
    FreeMemory(hDIB) ;Free the DIB
    ReDim GIF_Frames(numberimages)
    GIF_Frames(numberimages)\Image = pbimage
    GIF_Frames(numberimages)\DelayTime = delaytime
    numberimages + 1
  Wend
  FreeMemory(*mem)
  ProcedureReturn numberimages
EndProcedure
; IDE Options = PureBasic 5.31 (Windows - x86)
; CursorPosition = 1126
; FirstLine = 1074
; Folding = -
; EnableXP