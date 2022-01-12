unit BConsole;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

  { version 2.0   16.08.2019  Boby :) aka Sdex32  }
  { version 2.2   10.01.2022  Boby :) aka Sdex32  add 64 bit supprt }


interface

uses SysUtils;

    //todo asnistring put  out
    //TODO LEFT IN EDIT    synch by 24 fps




const                         //  resolution    font
      BTC_Mode_EGA80x25 = 0;  //  640 x 350     8x14
      BTC_Mode_EGA40x25 = 1;  //  320 x 200     8x8
      BTC_Mode_VGA80x25 = 2;  //  640 x 400     8x16
      BTC_Mode_VGA80x50 = 3;  //  640 x 400     8x8

type

      BTconsole = class
         private
            aCPU_thread_H   : longword;
            aCPU_thread_ID  : longword;
            aCPU_thread_RUN : boolean;
            aVGA_thread_H   : longword;
            aVGA_thread_ID  : longword;
            aVGA_thread_RUN : boolean;

            aMode      : longword;
            aHost_WND  : longword;
            aHost_Xpos : longword;
            aHost_Ypos : longword;
            aHost_Xlng : longword;
            aHost_Ylng : longword;
            aKeyTail   : array [0..31] of word;
            aKeyBegin  : longword;
            aKeyEnd    : longword;
            aKeyState  : longword;
            aKeyLast   : longword;
            aKeyAutorepeatOff : longword;
            aMouseXpos : longword;
            aMouseYpos : longword;
            aMouseKey  : longword;
            aColors    : array [0..255] of longword; // pal
            // video memory  max 640x480 (*4 byte longword)
            //  640*480=  307200*4 = 1 228 800 bytes (true color 4 bytes xrgb)
            // for vga pal mode
            // 320*240 =  76800 (bytes) /4 = 19200 longword  $AABBCCDD  AA-pixel1 BB-pixel2 CC-pixel3 ..
            // for svga pal mode
            // 640*480 = 307200 (bytes) /4 = 76800 longword  !! max longwords to add
            // memory organisation A0000 = pal mode screnn A0000+76800 = real bitmap screnn
            // for ega pal mode
            // 640*480 = 307200 (bytes) /8 = 38400   $ABCDEFGH  8 pixels in one logword
            xA0000     : array [0..(640*480)-1+76800] of longword; //rgb  307200*4  1228800 bytes
            xB8000     : array [0..3999] of word; // max 80x50   char   color shl 8
            aFont      : array [0..4095] of byte;
            aCurPos    : longword;
            aEditPos   : longword;
            aScrMax    : longword;
            aClearCol  : longword;
            aTColor    : longword;
            aBColor    : longword;
            aTxtXlng   : longword;
            aTxtYlng   : longword;
            aYskip     : longword;
//            aScrXlng   : longword;
            aScrYlng   : longword;
            aCursorMask: longword;
            aCharHeight: longword;
            aLastPos   : longword;
            aSetFntCnt : longword;
            aSetColorCnt : longword;
            aPrompt    : ansistring;
            aCmd       : ansistring;
            aLastCmd   : ansistring;
            aCmdDone   : longword;
            aRunner    : pointer;
            aInsm      : longword;
            aEnableNL  : longword;
            aReaderON  : longword;
            aTempPos   : longword;
            aBlinkingON : boolean;
            aGraphicON : boolean;
            aGraphic256 : longword;
            aBorder16  : boolean;
            aCursorON  : boolean;
            aExtRender : pointer;
            aExtRndPar : pointer;
            aExtEvent  : pointer;
            aExtEvnPar : pointer;
            aRendTm    : longword;
            a480m      : boolean;
            procedure  _SetSystemFont(id:longword);
            procedure  _MoveEditCursor(dx,dy:longint);
            procedure  _DelChar(m:longword);
            procedure  _MoveToEnd(var p:longword);
            procedure  _MoveRight;
            procedure  _PutPrompt;
            procedure  _AddKey(a:longword);
         public
            constructor Create;
            destructor  Destroy; override;
            procedure   SetMode(mode:longword; target_HWND,Xpos,Ypos,ScrXlng,ScrYlng,Flag:longword; var ClientXlng,ClientYlng :longword);
            procedure   Reset;
            function    GetProp(indx :longword) :longword;
            procedure   SetProp(indx, value :longword);
            procedure   SetGetPtr(indx:longword; var p:pointer);
            procedure   SetInterpreter(p:pointer);
            procedure   Setup(Prompt :string; ClearTxtColor, ClearBgTxtColor :longword);
            procedure   Interupt(m,a,b:longword);  // Use KeyPress event
            procedure   Execute(cmd:ansistring);
            //CRT
            function    KeyPressed :boolean;
            function    GetKey :longword;
            procedure   FlushKey;
            function    GetKeyState :longword;
            function    GetMouseXpos :longword;
            function    GetMouseYpos :longword;
            function    GetMouseKey :longword;
            procedure   ClrScr;
            procedure   GotoXY(X,Y :longword);
            function    WhereX :longword;
            function    WhereY :longword;
            procedure   TextColor(c :longword); //0-15 color index
            procedure   TextBackGround(c :longword);  //0-15;
            procedure   Scroll;
            procedure   WriteLn(S :ansistring); overload;
            procedure   WriteLn; overload;
            procedure   WriteCh(C:ansichar);
            procedure   Write(S :ansistring);
            procedure   Print(X,Y :longword; S :ansistring; TC,BC :longword);
            function    ReadLn :ansistring;
            procedure   GClear(C:longword);
            procedure   GBox(X,Y,Xl,YL:longint; C:longword);
            procedure   GLine(x1,y1,x2,y2:longint; C:longword);
            function    GRbios(a,b,c,d,e,f:longint):longint; // nostalgia
      end;

BTConsole_Runner = function(con:BTconsole; cmd:ansistring):ansistring; stdcall;


implementation

uses windows;


(*
const BM_INFO :array[0..12] of longword =
      (  40,  //bmiHeader.biSize cardinal = 40
         0,  //bmiHeader.biWidth integer = Xlng
         0,  //bmiHeader.biHeight integer = -Ylng >> start from 0,0 left,top
         $00200001,  //bmiHeader.biPlanes = 1 word   , biBitCount = 32 word
         3,  //bmiHeader.biCompression cardinal = 3  BI_BITFIELDS
         0,  //bmiHeader.biSizeImage  cardinal
         0,  //bmiHeader.biXpelsPerMeter integer
         0,  //bmiHeader.biYpelsPerMeter integer
         0,  //bmiHeader.biClrUsed cardinal
         0,  //bmiHeader.biClrImportant cardinal
         $0000FF,  //bmiColors[0]
         $00FF00,  //bmiColors[1]
         $FF0000   //bmiColors[2]
// How to use it
//  longint((@BM_INFO[1])^):= Xlng; //Write to const Dont do this at home :)
//  longint((@BM_INFO[2])^):= -Ylng;
//    SetStretchBltMode(dc, HALFTONE);
//    StretchDibits(dc, 10, 10, 50, 50, 0,0,100, 100,
//                  p , bitmapinfo((@BM_INFO)^), DIB_RGB_COLORS,SRCCOPY);
);
*)


//------------------------------------------------------------------------------
type charar = array[0..16] of byte;
//     btarr = array[0..0] of byte;
     ExtRender = function(a:pointer):pointer; stdcall;

const fmask :array[0..7] of byte = ($80,$40,$20,$10,$8,$4,$2,$1);

const BM_INFO :array[0..12] of longword =
      (  40,  //bmiHeader.biSize cardinal = 40
         0,  //bmiHeader.biWidth integer = Xlng
         0,  //bmiHeader.biHeight integer = -Ylng >> start from 0,0 left,top
         $00200001,  //bmiHeader.biPlanes = 1 word   , biBitCount = 32 word
         3,  //bmiHeader.biCompression cardinal = 3  BI_BITFIELDS
         0,  //bmiHeader.biSizeImage  cardinal
         0,  //bmiHeader.biXpelsPerMeter integer
         0,  //bmiHeader.biYpelsPerMeter integer
         0,  //bmiHeader.biClrUsed cardinal
         0,  //bmiHeader.biClrImportant cardinal
         $0000FF,  //bmiColors[0]
         $00FF00,  //bmiColors[1]
         $FF0000   //bmiColors[2]
       );

procedure VGA_thread(pcon:pointer); stdcall;
var con :BTconsole;
    bisize : longword;
//    cmask : array[0..2] of longint;
//    pbitmapinfo : array[0..2048] of byte; // sizeof(BITMAPINFOHEADER)+512  { 512 for 256 di colors word }
    dc,x,xb,tc,bc,y,yb,ptc,ch,yr,i,x_x,dbl:longword;
    blink,blnk,cursor,cx,cy,mc:longword;
    w,ren_t:longword;
    cp:^charar;
    m:byte;
    rect:TRect;
    brush:HBRUSH;
    ScrBitmapPtr :pointer;
    Ext_Render:ExtRender;

begin
   con := pcon;


 (*
   // prepare once
   bisize:=sizeof(BITMAPINFOHEADER);
   fillchar(pbitmapinfo, bisize+512, 0);

   with BITMAPINFO((@pbitmapinfo)^) do
   begin {BitmapInfoHeader 16Bit}
      bmiHeader.biSize        := bisize;
//      bmiHeader.biWidth       := con.aScrXlng;
//      bmiHeader.biHeight      := -con.aScrYlng;
      bmiHeader.biPlanes      := 1;
      bmiHeader.biBitCount    := 32; //bpp
      bmiHeader.biCompression := BI_BITFIELDS;
      bmiHeader.biWidth       := 640;
      bmiHeader.biHeight      := -con.aScrYlng;

//      cmask[0]:=$FF0000;                       {Bit-Positions R G B 24/32Bit }
//      cmask[1]:=$00FF00;     // work faster
//      cmask[2]:=$0000FF;

      cmask[0]:=$0000FF;                       {Bit-Positions R G B 24/32Bit }
      cmask[1]:=$00FF00;     // work faster
      cmask[2]:=$FF0000;

      move(cmask,pointer(longword(@pbitmapinfo)+ bisize)^,sizeof(cmask));
   end;
   *)

   blink := 0;
   blnk := 0;

   while con.aVGA_thread_RUN do
   begin
       ScrBitmapPtr := @con.xA0000[0];

       //update
       cursor := con.aCursorMask shl (32-8);
       if con.aCharHeight = 14 then cursor := con.aCursorMask shl (32-14);
       if con.aCharHeight = 16 then cursor := con.aCursorMask shl (32-16);


      // render
      ren_t := GetTickCount;
      if con.aHost_WND <> 0 then
      begin
         if not con.aGraphicON then
         begin  // Text mode rasterizer

            cx := con.WhereX -1 ; // con.aCurXpos -1;
            cy := con.WhereY -1 ; // con.aCurYpos -1;
            //m := $80;
            yb := 0;
            tc := 0;
            bc := 0;
            cp := pointer(@con.aFont[0]);
            dbl := 0;
            if con.aMode = 1 then dbl := 1; // 8x8 font 40x25

            ch := con.aCharHeight;
            ptc := 0;
            for yr := 0 to (con.aScrYlng) - 1 do
            begin
               if (yr >= con.aYskip) and (yr <= (con.aScrYlng - 1 - con.aYskip)) then
               begin
                  y := (yr - con.aYskip) shr dbl;
                  xb := $FFFFFFF;
                  for x_x := 0  to 639 do //con.aScrXlng - 1 do
                  begin //row by row of
                     x := x_x shr dbl;
                     m := x shr 3;

                     if xb <> m then
                     begin
//                        m := $80;
                        xb := m;
                        yb := y div ch;

                        w := con.xB8000[xb + yb*con.aTxtXlng];
                        bc := con.aColors[w shr 12];
                        tc := con.aColors[(w shr 8) and $F];
                        if con.aBlinkingON then
                        begin
                           bc := con.aColors[(w shr 12) and $7];
                           if (w  and $8000) <> 0 then
                           begin
                              if blink <> 0 then tc := bc;
                           end;
                        end;
{$IFDEF CPUX64}
                        cp := pointer(nativeUint(@con.aFont[0]) + (w and $FF)*ch);
{$ELSE}
                        cp := pointer(longword(@con.aFont[0]) + (w and $FF)*ch);
{$ENDIF}
                     end;


                     if (cp[y  mod ch] and fmask[x and 7]) <> 0
                     then con.xA0000[x_x + ptc] := tc
                     else con.xA0000[x_x + ptc] := bc;
//                     m := m shr 1;

                     if blink = 0 then
                     begin
                        if con.aCursorON  then
                        begin
                           if (cx = xb) and (cy = yb) then
                           begin
                              mc := $80000000  shr (y  mod ch);
                              if (mc and cursor) <> 0 then  con.xA0000[x_x + ptc] := tc;
                           end;
                        end;
                     end;
                  end;
               end else begin
                  for x := 0  to 639 {con.aScrXlng - 1} do con.xA0000[x + ptc] := 0;
               end;
               ptc := ptc + 640; //con.aScrXlng;
            end;

         end else begin
            //Graphic mode

            if con.aExtRender <> nil then
            begin
               Ext_Render := con.aExtRender;
               ScrBitmapPtr := Ext_Render(con.aExtRndPar);
               if ScrBitmapPtr = nil then ScrBitmapPtr := @con.xA0000[0];
            end;

            if con.aGraphic256 <> 0 then
            begin
{$IFDEF CPUX64}
               ScrBitmapPtr := pointer(nativeUint(@con.xA0000[0])+ 76800*4);  //640*480/4=76800
{$ELSE}
               ScrBitmapPtr := pointer(longword(@con.xA0000[0])+ 76800*4);  //640*480/4=76800
{$ENDIF}
               ptc := 76800;
               ch := 0;
               bc := 480; //con.aScrYlng ;//div 4;
               if con.aGraphic256 = 1 then bc := 240;// con.aScrYlng div 2;

               for y := 1 to bc do
               begin
                  case con.aGraphic256 of
                     1: begin
                        for x := 1 to 80 do  //320 div 4 = 80
                        begin
                           w := con.xA0000[ch]; // Hi Lo
                           for i:= 0 to 3 do
                           begin
                              tc := con.aColors[ w and $FF];
                              con.xA0000[ptc] := tc;
                              con.xA0000[ptc+1] := tc;
                              con.xA0000[ptc+640] := tc; // second row
                              con.xA0000[ptc+641] := tc;
                              inc(ptc,2);
                              w := w shr 8;
                           end;
                           inc(ch);
                        end;
                        inc(ptc,640);
                     end;
                     2: begin
                        for x := 1 to 160 do  //640 div 4 = 160
                        begin
                           w := con.xA0000[ch]; // Hi Lo
                           for i:= 0 to 3 do
                           begin
                              con.xA0000[ptc] := con.aColors[ w and $FF];
                              inc(ptc,1);
                              w := w shr 8;
                           end;
                           inc(ch);
                        end;
                     end;
                     3: begin
                        for x := 1 to 80 do  //640 div 8 = 80
                        begin
                           w := con.xA0000[ch]; // Hi Lo
                           for i:= 0 to 7 do
                           begin
                              con.xA0000[ptc] := con.aColors[w and $F];
                              inc(ptc,1);
                              w := w shr 4;
                           end;
                           inc(ch);
                        end;
                     end;
                  end;
               end;
            end;
         end;

      // display it

            dc:= GetDC( con.aHost_WND);
            if con.aBorder16 then
            begin
               brush :=  CreateSolidBrush(0);

               x := con.aHost_Xpos-16;
               xb := 640; //con.aScrXlng;
               if con.aHost_Xlng <> 0 then xb := con.aHost_Xlng;
               xb := xb + x + 32;

               y := con.aHost_Ypos-16;
               yb := con.aScrYlng;
               if con.aHost_Ylng <> 0 then yb := con.aHost_Ylng;
               yb := yb + y + 32;

               rect.left := x;
               rect.top := y;
               rect.right := xb;
               rect.bottom := y+16;
               FillRect(dc, Rect, brush);

               rect.left := x;
               rect.top := y + 16;
               rect.right := x + 16;
               rect.bottom :=  yb;
               FillRect(dc, Rect, brush);

               rect.left := xb-16;
               rect.top := y;
               rect.right := xb;
               rect.bottom :=  yb;
               FillRect(dc, Rect, brush);

               rect.left := x;
               rect.top := yb - 16;
               rect.right := xb;
               rect.bottom :=  yb;
               FillRect(dc, Rect, brush);

               DeleteObject(brush);
            end;
                                {
            with BITMAPINFO((@pbitmapinfo)^) do
            begin
               bmiHeader.biWidth       := 640;
               bmiHeader.biHeight      := -con.aScrYlng;
            end;
                                 }
            longint((@BM_INFO[1])^):= 640; //Write to const Dont do this at home :)
            longint((@BM_INFO[2])^):= -con.aScrYlng;


            if (con.aHost_Xlng + con.aHost_Ylng) = 0 then
            begin
                SetDIBitsToDevice(dc, con.aHost_Xpos, con.aHost_Ypos,
                              640, con.aScrYlng, 0, 0, 0,
                              con.aScrYlng, ScrBitmapPtr, bitmapinfo((@BM_INFO)^),
                              // bitmapinfo((@pbitmapinfo)^),
                              DIB_RGB_COLORS);
            end else begin
                x := 640;
                if con.aHost_Xlng <> 0 then  x := con.aHost_Xlng;
                y := con.aScrYlng;
                if con.aHost_Ylng <> 0 then  y := con.aHost_Ylng;
         //??       if con.aGraphic256 = 0 then
         SetStretchBltMode(dc, HALFTONE);
                StretchDibits(dc, con.aHost_Xpos, con.aHost_Ypos, x, y,
                              0,0,640,con.aScrYlng, ScrBitmapPtr , bitmapinfo((@BM_INFO)^),
                            //  bitmapinfo((@pbitmapinfo)^),
                              DIB_RGB_COLORS,SRCCOPY);
            end;

            ReleaseDc( con.aHost_WND, dc);


      end; // have dc
      con.aRendTm := GetTickCount - ren_t;
      if con.aRendTm < 42 then
      begin
         sleep(42 - con.aRendTm);   //24 frames per seconds
      end;
      inc(blnk);
      if blnk = 3 then
      begin
         blnk := 0;
         blink := blink xor 1;
      end;
   end; // loop
end;

//------------------------------------------------------------------------------
procedure CPU_thread(pcon:pointer); stdcall;
var con :BTconsole;
    ch,i,j:longword;
    ps:ansistring;
    done:boolean;
    RunnerOn:longword;
begin
   con := pcon;
   ps := 'A';
   RunnerOn := 0;

   while con.aCPU_thread_RUN do
   begin
      done := false;
      if con.KeyPressed then
      begin
        con._MoveToEnd(j);
         ch := con.GetKey;
         if ch < 256 then
         begin
            case ch of
               8: con._DelChar(1); // back space
               13: begin
                      con.aCmd := '';
                      dec(j);
                      if j >= con.aEditPos then
                         for i := con.aEditPos to j do con.aCmd := con.aCmd + ansichar(con.xB8000[i] and $FF);
                      con.aCmdDone := 1;
                      con.aLastCmd := con.aCmd;
                      RunnerON := 1;
                   end;
               else begin
                  ps[1]:=ansichar(ch);
                  if con.aInsm = 0 then
                  begin
                     if con.aCurPos < j then con._MoveRight;
                     con.Write(ps); // put char
                  end else con.Write(ps); // overwrite
               end;
            end;
         end else begin
            case ch of
              256: {VK_LEFT}  con._MoveEditCursor(-1,0);
              257: {VK_RIGHT} con._MoveEditCursor(1,0);
              258: {VK_UP} begin
                      con._MoveToEnd(i);
                      if i = con.aEditPos then con.Write(con.aLastCmd);
                      con._MoveEditCursor(0,-1);
                   end;
              259: {VK_DOWN}  con._MoveEditCursor(0,1);
              260: {VK_INSERT} begin
                      con.aInsm := con.aInsm xor 1;
                      if con.aInsm = 0 then con.aCursorMask := 7
                      else con.aCursorMask := $FFFF; //todo custom
                   end;
              261: {VK_HOME}  con.aCurPos := con.aEditPos;
              262: {VK_END}   con._MoveToEnd(con.aCurPos);
              263: {VK_DELETE}con._DelChar(0);
            end;
         end;
         done := true;
      end; //key event

      if RunnerON = 1 then
      begin
         con.Execute(con.aCmd);
         RunnerON := 0;
         con.aCmd := '';
         done := true;
      end;

      if not done then sleep(20);
      //todo stepper
   end;
end;


//------------------------------------------------------------------------------
type cb_callback = function(obj:pointer; Key,Pres,KeyStatus:longword):longint; stdcall;

procedure   BTconsole.Interupt(m,a,b:longword);
var cbExt:cb_callback;
    dc:longword;
    s:ansistring;

   procedure Key_up_do;
   begin
//      if aExtEvent <> nil then
//      begin
//         cbExt := aExtEvent;
//         cbExt(aExtEvnPar,a,1,aKeyState);
//      end;
      if a = VK_LEFT   then _AddKey(256);
      if a = VK_RIGHT  then _AddKey(257);
      if a = VK_UP     then _AddKey(258);
      if a = VK_DOWN   then _AddKey(259);
      if a = VK_INSERT then _AddKey(260);
      if a = VK_HOME   then _AddKey(261);
      if a = VK_END    then _AddKey(262);
      if a = VK_DELETE then _AddKey(263);
      if a = VK_PRIOR  then _AddKey(264); {PAGE UP }
      if a = VK_NEXT   then _AddKey(265); {PAGE DOWN }
      { F1=266 .. F12=277 }
      if (a >= VK_F1) and ( a <= VK_F12) then _AddKey(266+(a-VK_F1));
//      if a = VK_SHIFT  then aKeyState := aKeyState and (not $1);
//      if a = VK_CONTROL then aKeyState := aKeyState and (not $2);
//      if a = VK_MENU   then aKeyState := aKeyState and (not $4); {ALT}
   end;

begin
   if (m = 1) or (m = 2)then
   begin
   dc:=GetDc(self.aHost_WND);
   str(a,s);
   s :=s + #0;
   TextOutA(dc,700,10,@s[1],length(s));
   end;

   if m = 0 then  // keyboard OkKeyPress   WM_CHAR translated
   begin
      if aExtEvent <> nil then
      begin
         cbExt := aExtEvent;
         cbExt(aExtEvnPar,a,2,aKeyState);
      end;
      _AddKey(a);
   end;
   if m = 1 then  // keyboard OkKeyUp  WM_KEYUP
   begin
//      aKeyLast := $FFFFFFFF;  //???????????????????????
//      Key_Up_Do;
      if aExtEvent <> nil then
      begin
         cbExt := aExtEvent;
         cbExt(aExtEvnPar,a,1,aKeyState);
      end;
      if a = VK_SHIFT  then aKeyState := aKeyState and (not $1);
      if a = VK_CONTROL then aKeyState := aKeyState and (not $2);
      if a = VK_MENU   then aKeyState := aKeyState and (not $4); {ALT}
   end;
   if m = 2 then  // keyboard OkKeyDown  WM_KEYDOWN
   begin
      aKeyLast := $FFFFFFFF;
      Key_Up_Do;

      if a = aKeyLast then // repeat
      begin
         if aKeyAutorepeatOff = 0 then Key_Up_Do;
      end;

      if aExtEvent <> nil then
      begin
         cbExt := aExtEvent;
         cbExt(aExtEvnPar,a,0,aKeyState);
      end;
      if a = VK_SHIFT  then aKeyState := aKeyState or $1;
      if a = VK_CONTROL then aKeyState := aKeyState or $2;
      if a = VK_MENU   then aKeyState := aKeyState or $4; {ALT}
      aKeyLast := a;
   end;
   if m = 3 then // mouse MoseMove
   begin
      aMouseXpos := a;
      aMouseYpos := b;
   end;
   if m = 4 then // mouse MoseDown
   begin
      if a = 1 then  aMouseKey := aMouseKey or $1;
      if a = 2 then  aMouseKey := aMouseKey or $2;
      if a = 3 then  aMouseKey := aMouseKey or $4;
   end;
   if m = 5 then // mouse MoseUp
   begin
      if a = 1 then  aMouseKey := aMouseKey and (not $1);
      if a = 2 then  aMouseKey := aMouseKey and (not $2);
      if a = 3 then  aMouseKey := aMouseKey and (not $4);
   end;
end;

//------------------------------------------------------------------------------
procedure   BTconsole.Execute(cmd:ansistring);
var run :BTConsole_Runner;
    s:ansistring;
begin
   WriteLn;
   s := '';
   if aRunner <> nil then
   begin
      run := aRunner;
      s := Run(self, Cmd);  //todo result output
   end;
   if length(s) > 0 then WriteLn(s);
   _PutPrompt;
end;

//------------------------------------------------------------------------------
constructor BTconsole.Create;
var i:longword;
begin
   for i:=  0 to (640*480)-1 do xA0000[i] := 0;
//   aScrXlng := 640;
   aScrYlng := 400;
   aTxtXlng := 80;
   aTxtYlng := 25;
   aYskip := 0;
   aCharHeight := 8;
   aMode := 2;
   aHost_WND := 0;
   aScrMax := 80*25-1;
   aCurPos := 0;
   aSetFntCnt := 0;
   aSetColorCnt := 0;
   aRunner := nil;
   aInsm := 0;
   aBlinkingOn := true;
   aGraphicON := false;
   a480m := false;
   aGraphic256 :=0;
   aBorder16 := false;
   aCursorON := true;
   aExtRender := nil;
   aExtRndPar := nil;
   aExtEvent := nil;
   aExtEvnPar := nil;
   aEnableNL := 0;
   aCmd := '';
   aLastCmd := '';
   aReaderON := 0;
   aKeyBegin := 0;
   aKeyEnd   := 0;
   aKeyState := 0;
   aKeyAutorepeatOff := 0;
   aKeyLast := $FFFFFFFF;
   aMouseXPos := 0;
   aMouseYpos := 0;
   aMouseKey := 0;
   aVGA_thread_RUN := true;
   aVGA_thread_H := CreateThread(nil,0,@VGA_thread,pointer(self),0,aVGA_thread_ID);
   aCPU_thread_RUN := true;
   aCPU_thread_H := CreateThread(nil,0,@CPU_thread,pointer(self),0,aCPU_thread_ID);
   Reset;
end;

//------------------------------------------------------------------------------
destructor  BTconsole.Destroy;
begin
   aVGA_thread_RUN := false;
   aCPU_thread_RUN := false;
   sleep(200);
   TerminateThread(aVGA_thread_H,0);
   TerminateThread(aCPU_thread_H,0);
   FileClose(aVGA_thread_H); { *Converted from CloseHandle* }
   FileClose(aCPU_thread_H); { *Converted from CloseHandle* }
   inherited;
end;

//------------------------------------------------------------------------------
//      BTC_Mode_EGA80x25 = 0;  //  640 x 350     8x14
//      BTC_Mode_EGA40x25 = 1;  //  320 x 200     8x8
//      BTC_Mode_VGA80x25 = 2;  //  640 x 400     8x16
//      BTC_Mode_VGA80x50 = 3;  //  640 x 400     8x8

const Mode_table : array [0..3,1..6] of longword =
//  font  resol    tx res
    ((1, 640, 350, 80, 25, 14),
     (0, 640, 400, 40, 25, 8 ),
     (2, 640, 400, 80, 25, 16),
     (0, 640, 400, 80, 50, 8 ));

procedure   BTconsole.SetMode(mode:longword; target_HWND,Xpos,Ypos,ScrXlng,ScrYlng,Flag:longword; var ClientXlng,ClientYlng :longword);
//var Yadd:longword;
begin


   if mode > 3 then mode := 3;
   aYskip := 0;
//   Yadd := 0;
   aScrYlng := mode_table[mode,3];
   a480m := false;
   if ((Flag and $2) <> 0 ) then //and (mode >= 2) then
   begin // force 480 to enable graphic mode

      aYskip := (480 - aScrYlng) div 2;
      aScrYlng := 480; //force
      a480m := true;
 //     Yadd := 80;
 //     aYskip := 40;
 //     if mode = 1 then //320
 //     begin
 //        Yadd := 40;
 //     end;
   end;


   aMode := mode;
   aHost_WND := target_HWND;
   aHost_Xpos := Xpos;
   aHost_Ypos := Ypos;
   aHost_Xlng := ScrXlng; // To be displayed
   aHost_Ylng := ScrYlng;
//   aScrXlng := mode_table[mode,2]; // Size of VGA buffer size
//   aScrYlng := mode_table[mode,3] + Yadd;
   if aHost_Xlng = 0 then ClientXlng := 640 {aScrXlng} + Xpos
                     else ClientXlng := aHost_Xlng + Xpos;
   if aHost_Ylng = 0 then ClientYlng := aScrYlng + Ypos
                     else ClientYlng := aHost_Ylng + Ypos;
   aTxtXlng := mode_table[mode,4];
   aTxtYlng := mode_table[mode,5];
   aCharHeight := mode_table[mode,6];
   aScrMax := aTxtXlng * aTxtYlng - 1;

   if (Flag and $1) <> 0 then
   begin  // add border
      aBorder16 := true;
      inc(ClientXlng,32);
      inc(ClientYlng,32);
      inc(aHost_Xpos,16);
      inc(aHost_Ypos,16);
   end;


   Reset;
end;




//------------------------------------------------------------------------------
procedure   BTconsole.Reset;
begin
{
   aColors[0 ] := rgb(   0,  0,  0);
   aColors[1 ] := rgb(   0,  0,128);
   aColors[2 ] := rgb(   0,128,  0);
   aColors[3 ] := rgb(   0,128,128);
   aColors[4 ] := rgb( 128,  0,  0);
   aColors[5 ] := rgb( 128,  0,128);
   aColors[6 ] := rgb( 128,128,  0);
   aColors[7 ] := rgb( 192,192,192);
   aColors[8 ] := rgb( 160,160,164);
   aColors[9 ] := rgb(   0,  0,255);
   aColors[10] := rgb(   0,255,  0);
   aColors[11] := rgb(   0,255,255);
   aColors[12] := rgb( 255,  0,  0);
   aColors[13] := rgb( 255,  0,255);
   aColors[14] := rgb( 255,255,  0);
   aColors[15] := rgb( 255,255,255);
}
   aColors[0 ] := rgb(   0,  0,  0);   {more pastel}
   aColors[1 ] := rgb(   0,  0,$AA);
   aColors[2 ] := rgb(   0,$AA,  0);
   aColors[3 ] := rgb(   0,$AA,$AA);
   aColors[4 ] := rgb( $AA,  0,  0);
   aColors[5 ] := rgb( $AA,  0,$AA);
   aColors[6 ] := rgb( $AA,$55,  0);
   aColors[7 ] := rgb( $AA,$AA,$AA);
   aColors[8 ] := rgb( $55,$55,$55);
   aColors[9 ] := rgb( $55,$55,$FF);
   aColors[10] := rgb( $55,$FF,$55);
   aColors[11] := rgb( $55,$FF,$FF);
   aColors[12] := rgb( $FF,$55,$55);
   aColors[13] := rgb( $FF,$55,$FF);
   aColors[14] := rgb( $FF,$FF,$55);
   aColors[15] := rgb( $FF,$FF,$FF);

   _SetSystemFont(mode_table[aMode,1]);
   Setup('>',7,0);
   aCursorMask := 7;
   ClrScr;

end;

//------------------------------------------------------------------------------
procedure   BTconsole.ClrScr;
var i:longword;
begin
   for i:= 0 to 3999 do xB8000[i]:= word(aClearCol);
   GotoXY(1,1);
   aTColor := (aClearCol shr 8) and $f;
   aBColor := (aClearCol shr 12) and $f
  // _PutPrompt;
end;

//------------------------------------------------------------------------------
procedure   BTconsole.GotoXY(X,Y:longword);
begin
   if X = 0 then X := 1;
   if Y = 0 then Y := 1;
   if X > aTxtXlng then X := aTxtXlng;
   if Y > aTxtYlng then Y := aTxtYlng;
   aCurPos := (X-1) + (Y-1)*aTxtXlng;
end;

//------------------------------------------------------------------------------
function    BTconsole.WhereX :longword;
begin
   Result := aCurPos mod (aTxtXlng) + 1;
end;

//------------------------------------------------------------------------------
function    BTconsole.WhereY :longword;
begin
   Result := (aCurPos div aTxtXlng) + 1;
end;

//------------------------------------------------------------------------------
procedure   BTconsole.TextColor(c :longword); //0-15 color index
begin
   aTColor := c and $F;
end;

//------------------------------------------------------------------------------
procedure   BTconsole.TextBackGround(c :longword);  //0-15;
begin
   aBColor := c and $F;
end;

//------------------------------------------------------------------------------
procedure   BTconsole.Scroll;
var i,j,c:longword;
begin
   c := aTxtXlng * (aTxtYlng-1) - 1;
   j := aTxtXlng ; // next row
   for i := 0 to c do  xB8000[i] := xB8000[i+j];
   inc(c); // to last row
   for i := c to 3999 do xB8000[i] := word(aClearCol); //word(j);
   if (longint(aEditPos) - longint(aTxtXlng)) > 0 then aEditPos := aEditPos - aTxtXlng;
   if (longint(aTempPos) - longint(aTxtXlng)) > 0 then aTempPos := aTempPos - aTxtXlng;
end;

//------------------------------------------------------------------------------
procedure   BTconsole.WriteLn(S :ansistring);
begin
   Write(S);
   WriteLn;
end;

//------------------------------------------------------------------------------
procedure   BTconsole.WriteLn;
var y:longword;
begin
   y := whereY + 1;
   if y > aTxtYlng then
   begin
      dec(y);
      Scroll;
   end;
   GotoXY(1,y);
end;

//------------------------------------------------------------------------------
procedure   BTconsole.WriteCh(C:ansichar);
var p:ansistring;
begin
   p:=C;
   Write(p);
end;

//------------------------------------------------------------------------------
procedure   BTconsole.Write(S :ansistring);
begin
   if s = #13 then
   begin
      WriteLn;
      Exit;
   end;
   aEnableNL := 1;
   Print(WhereX,WhereY,S,aTcolor,aBcolor);
   aEnableNL := 0;
   aCurPos := aLastPos;
end;


//------------------------------------------------------------------------------
procedure   BTconsole.Print(X,Y:longword; S: ansistring; TC,BC:longword);
var i,c,lp,ch :longword;
begin
   if aTxtYlng = 0 then Exit; // still not init
   c := $FF;
   if Tc = $FF then c := (BC and $FF) shl 8;  // bypass direct color in BC
   lp := aCurPos;
   Tc := Tc and $F;
   Bc := Bc and $F;
   GotoXY(X,Y);
   if c = $FF then c := ((Tc or (Bc shl 4)) shl 8);
   for i := 1 to length(S) do
   begin
      ch := longword(S[i]);
      xB8000[aCurPos] := word( c or ch );
      if (aCurPos+1) > aScrMax then
      begin
         if aEnableNL = 1 then
         begin
            Scroll;
            aCurPos := aScrMax - aTxtXlng;
         end else break;
      end;
      inc(aCurPos);
   end;
   aLastPos := aCurPos;
   aCurPos := lp;
end;

//------------------------------------------------------------------------------
function    BTconsole.ReadLn :ansistring;    // todo
var t:longword;
begin
   Result := '';
   t := 0;
   repeat
      if KeyPressed then
      begin
         t := GetKey;
         if t <> 13 then
         begin
            WriteCh(Ansichar(t));
            Result := Result + ansichar(t);
         end;
      end;
   until (t = 13);
end;

//------------------------------------------------------------------------------
function    BTconsole.KeyPressed :boolean;
begin
   Result := aKeyBegin <> aKeyEnd;
end;

//------------------------------------------------------------------------------
procedure   BTconsole._AddKey(a:longword);
var i:longword;
begin
   i := aKeyEnd;
   inc(i);
   if i > 31 then i := 0;
   if i <> aKeyBegin then
   begin
      aKeyTail[aKeyEnd]:= word(a);
      aKeyEnd := i;
   end;
end;

//------------------------------------------------------------------------------
function    BTconsole.GetKey :longword;
begin
   Result := 0;
   if aKeyBegin <> aKeyEnd then
   begin
      Result := longword( aKeyTail[aKeyBegin] );
      inc(aKeyBegin);
      if aKeyBegin > 31 then aKeyBegin := 0;
   end;
end;

//------------------------------------------------------------------------------
procedure   BTconsole.FlushKey;
begin
   aKeyBegin := 0;
   aKeyEnd := 0;
end;

//------------------------------------------------------------------------------
function    BTconsole.GetKeyState :longword;
begin
   Result := aKeyState;
end;

//------------------------------------------------------------------------------
function    BTconsole.GetMouseXpos :longword;
begin
   Result := aMouseXpos;
   if aBorder16 then  Result := Result - 16;
   if aHost_Xlng <> 0 then Result := trunc((640/aHost_Xlng)*Result);
end;

//------------------------------------------------------------------------------
function    BTconsole.GetMouseYpos :longword;
begin
   Result := aMouseYpos;
   if aBorder16 then  Result := Result - 16;
   if aHost_Ylng <> 0 then Result := trunc((aScrYlng/aHost_Ylng)*Result);
end;

//------------------------------------------------------------------------------
function    BTconsole.GetMouseKey :longword;
begin
   Result := aMouseKey;
end;


//------------------------------------------------------------------------------
procedure   BTconsole.SetGetPtr(indx:longword; var p:pointer);
var o:pointer;
begin
   case indx of
      0 : p := @aFont[0]; // no set
      1 : p := @xB8000[0]; //no set
      2 : p := @xA0000[0]; // no set
      3 : begin o := aExtRender; aExtRender := p; p := o; end;
      4 : begin o := aExtRndPar; aExtRndPar := p; p := o; end;
      5 : begin o := aExtEvent; aExtEvent := p; p := o; end;
      6 : begin o := aExtEvnPar; aExtEvnPar := p; p := o; end;
   end;
end;

//------------------------------------------------------------------------------
function    BTconsole.GetProp(indx :longword) :longword;
begin
   Result := 0;
   if aSetColorCnt <> 0 then
   begin
      Result := aColors[indx and $FF]; //TODO  := rgb((value and $FF0000)shr 16,(value and $FF00)shr 8,(value and $FF));
      dec(aSetColorCnt);
      Exit;
   end;


   case indx of
      0..15: Result := aColors[indx];
      16: Result := aCursorMask;
      17: Result := aCharHeight;
      18: Result := aHost_Xpos;
      19: Result := aHost_Ypos;
      20: Result := aHost_Xlng;
      21: Result := aHost_Ylng;
      22: if aBlinkingON then Result := 1 else Result := 0;
      23: if aGraphicON then Result := 1 else Result := 0;
      24: if aCursorON then Result := 1 else Result := 0;
      25: if aBorder16 then Result := 1 else Result := 0;
      26: Result := aTColor;
      27: Result := aBColor;




      40: Result := aHost_WND;

      50: Result := aTxtXlng;
      51: Result := aTxtYlng;
      52: begin
             Result := 640; //aScrXlng;
             if aGraphicOn and (aGraphic256 = 1) then Result := 320;
          end;
      53: begin
             Result := 480; //aScrYlng;
             if aGraphicOn and (aGraphic256 = 1) then Result := 240;
          end;
      54: Result := aTColor;
      55: Result := aBColor;

//      80: Result := longword(@aFont[0]);
//      81: Result := longword(@xB8000[0]);
//      82: Result := longword(@xA0000[0]);
   end;
end;

//------------------------------------------------------------------------------
procedure   BTconsole.SetProp(indx, value :longword);
begin
   // sequentional calls
   if aSetColorCnt <> 0 then
   begin
      aColors[indx and $FF] := rgb((value and $FF0000)shr 16,(value and $FF00)shr 8,(value and $FF));
      dec(aSetColorCnt);
      Exit;
   end;
   if aSetFntCnt <> 0 then // have sequence of char bitmap
   begin
      aFont[(indx and $FF)*aCharHeight + (aCharHeight-aSetFntCnt)] := byte(value);
      dec(aSetFntCnt);
      Exit;
   end;


   if indx <= 15 then aColors[indx] := value; // setpalette
   if indx = 16  then aCursorMask := value;   // set cursor mask
   if indx = 17  then aSetFntCnt := aCharHeight; // next call to setProp is Font bitmap
   if indx = 18  then aHost_Xpos := value;
   if indx = 19  then aHost_Ypos := value;
   if indx = 20  then aHost_Xlng := value;
   if indx = 21  then aHost_Ylng := value;
   if indx = 22  then if value = 0 then aBlinkingON := false else aBlinkingON := true;
   if indx = 23  then if value = 0 then begin aGraphicON := false; Reset; end else if a480m then aGraphicON := true;
   if indx = 24  then if value = 0 then aCursorON := false else aCursorON := true;
   if indx = 25  then if value = 0 then aBorder16 := false else aBorder16 := true;
   if indx = 26  then aTColor := value and $F;
   if indx = 27  then aBColor := value and $F;
//   if indx = 28  then aExtRender := pointer(value);
//   if indx = 29  then aExtRndPar := value;
//   if indx = 30  then aExtEvent := pointer(value);
//   if indx = 31  then aExtEvnPar := value;
   if indx = 32  then aKeyAutoRepeatOff := value and 1; //1-auto repeat stop
   if indx = 33  then aSetColorCnt := value;
   if indx = 34  then if value = 0 then begin aGraphic256 := 0; Reset; end else aGraphic256 := value and 3;





end;

//------------------------------------------------------------------------------
procedure   BTconsole.Setup(Prompt :string; ClearTxtColor, ClearBgTxtColor :longword);
begin
   aPrompt := ansistring(Prompt);
   aTColor := ClearTxtColor and $F;
   aBColor := ClearBgTxtColor and $F;
   aClearCol := ((ClearTxtColor or (ClearBgTxtColor shl 4)) shl 8);
end;

//------------------------------------------------------------------------------
procedure   BTconsole.SetInterpreter(p:pointer);
begin
   aRunner := p;
end;

//------------------------------------------------------------------------------
procedure   BTconsole._MoveEditCursor(dx,dy:longint);
var p:longint;
    l:longword;
begin
//   x := WhereX;
   _MoveToEnd(l);
   p := longint(aCurPos);
   p := p + dx + dy * longint(aTxtXlng);
   if p < longint(aEditPos) then p := aEditPos; //+x;
   if p > longint(aScrMax) then p := aScrMax;
   if p > longint(l) then p := l;
   aCurPos := longword(p);
end;

//------------------------------------------------------------------------------
procedure   BTconsole._DelChar(m:longword);
var l,p:longword;
begin
   _MoveToEnd(l);
//   if m = 0 then del
   p := aCurPos;
   if m = 1 then p := p - 1;
   if p < aEditPos then p := aEditPos;
   aCurPos := p;
   while (p<l) do
   begin
      xB8000[p] := xB8000[p+1];
      inc(p);
   end;
   xB8000[l] := aClearCol;
end;

//------------------------------------------------------------------------------
procedure   BTconsole._MoveRight;
var p,l,i:longword;
begin
   aTempPos := aCurPos;
   _MoveToEnd(l);
   aCurPos := l;
   Write(#0); // to make need scrolls
   _MoveToEnd(l);
   p := aTempPos;
   if (l > p) and (l<>0) then
   for i := l downto p+1 do  xB8000[i] := xB8000[i-1];
   aCurPos := aTempPos;
end;

//------------------------------------------------------------------------------
procedure   BTconsole._PutPrompt;
var i:longword;
begin
   i := length(aPrompt);
   if i > 0 then Write(aPrompt);
   aEditPos := aCurPos;
end;

//------------------------------------------------------------------------------
procedure   BTconsole._MoveToEnd(var P:longword);
var pp :longword;
begin
   pp := aCurPos;
   while ((xB8000[pp] and $FF) <> 0) and ((pp+1)<=aScrMax)  do inc(pp);
   p := pp;
end;



//  Old School Fonts

const VGA_Font_8x8 : array [0..2047] of byte =
  (0,0,0,0,0,0,0,0,
   126,129,165,129,189,153,129,126,
   126,255,219,255,195,231,255,126,
   108,254,254,254,124,56,16,0,
   16,56,124,254,124,56,16,0,
   56,124,56,254,254,124,56,124,
   16,16,56,124,254,124,56,124,
   0,0,24,60,60,24,0,0,
   255,255,231,195,195,231,255,255,
   0,60,102,66,66,102,60,0,
   255,195,153,189,189,153,195,255,
   15,7,15,125,204,204,204,120,
   60,102,102,102,60,24,126,24,
   63,51,63,48,48,112,240,224,
   127,99,127,99,99,103,230,192,
   153,90,60,231,231,60,90,153,
   128,224,248,254,248,224,128,0,
   2,14,62,254,62,14,2,0,
   24,60,126,24,24,126,60,24,
   102,102,102,102,102,0,102,0,
   127,219,219,123,27,27,27,0,
   62,99,56,108,108,56,204,120,
   0,0,0,0,126,126,126,0,
   24,60,126,24,126,60,24,255,
   24,60,126,24,24,24,24,0,
   24,24,24,24,126,60,24,0,
   0,24,12,254,12,24,0,0,
   0,48,96,254,96,48,0,0,
   0,0,192,192,192,254,0,0,
   0,36,102,255,102,36,0,0,
   0,24,60,126,255,255,0,0,
   0,255,255,126,60,24,0,0,
   0,0,0,0,0,0,0,0,
   48,120,120,48,48,0,48,0,
   108,108,108,0,0,0,0,0,
   108,108,254,108,254,108,108,0,
   48,124,192,120,12,248,48,0,
   0,198,204,24,48,102,198,0,
   56,108,56,118,220,204,118,0,
   96,96,192,0,0,0,0,0,
   24,48,96,96,96,48,24,0,
   96,48,24,24,24,48,96,0,
   0,102,60,255,60,102,0,0,
   0,48,48,252,48,48,0,0,
   0,0,0,0,0,48,48,96,
   0,0,0,252,0,0,0,0,
   0,0,0,0,0,48,48,0,
   6,12,24,48,96,192,128,0,
   124,198,206,222,246,230,124,0,
   48,112,48,48,48,48,252,0,
   120,204,12,56,96,204,252,0,
   120,204,12,56,12,204,120,0,
   28,60,108,204,254,12,30,0,
   252,192,248,12,12,204,120,0,
   56,96,192,248,204,204,120,0,
   252,204,12,24,48,48,48,0,
   120,204,204,120,204,204,120,0,
   120,204,204,124,12,24,112,0,
   0,48,48,0,0,48,48,0,
   0,48,48,0,0,48,48,96,
   24,48,96,192,96,48,24,0,
   0,0,252,0,0,252,0,0,
   96,48,24,12,24,48,96,0,
   120,204,12,24,48,0,48,0,
   124,198,222,222,222,192,120,0,
   48,120,204,204,252,204,204,0,
   252,102,102,124,102,102,252,0,
   60,102,192,192,192,102,60,0,
   248,108,102,102,102,108,248,0,
   254,98,104,120,104,98,254,0,
   254,98,104,120,104,96,240,0,
   60,102,192,192,206,102,62,0,
   204,204,204,252,204,204,204,0,
   120,48,48,48,48,48,120,0,
   30,12,12,12,204,204,120,0,
   230,102,108,120,108,102,230,0,
   240,96,96,96,98,102,254,0,
   198,238,254,254,214,198,198,0,
   198,230,246,222,206,198,198,0,
   56,108,198,198,198,108,56,0,
   252,102,102,124,96,96,240,0,
   120,204,204,204,220,120,28,0,
   252,102,102,124,108,102,230,0,
   120,204,224,112,28,204,120,0,
   252,180,48,48,48,48,120,0,
   204,204,204,204,204,204,252,0,
   204,204,204,204,204,120,48,0,
   198,198,198,214,254,238,198,0,
   198,198,108,56,56,108,198,0,
   204,204,204,120,48,48,120,0,
   254,198,140,24,50,102,254,0,
   120,96,96,96,96,96,120,0,
   192,96,48,24,12,6,2,0,
   120,24,24,24,24,24,120,0,
   16,56,108,198,0,0,0,0,
   0,0,0,0,0,0,0,255,
   48,48,24,0,0,0,0,0,
   0,0,120,12,124,204,118,0,
   224,96,96,124,102,102,220,0,
   0,0,120,204,192,204,120,0,
   28,12,12,124,204,204,118,0,
   0,0,120,204,252,192,120,0,
   56,108,96,240,96,96,240,0,
   0,0,118,204,204,124,12,248,
   224,96,108,118,102,102,230,0,
   48,0,112,48,48,48,120,0,
   12,0,12,12,12,204,204,120,
   224,96,102,108,120,108,230,0,
   112,48,48,48,48,48,120,0,
   0,0,204,254,254,214,198,0,
   0,0,248,204,204,204,204,0,
   0,0,120,204,204,204,120,0,
   0,0,220,102,102,124,96,240,
   0,0,118,204,204,124,12,30,
   0,0,220,118,102,96,240,0,
   0,0,124,192,120,12,248,0,
   16,48,124,48,48,52,24,0,
   0,0,204,204,204,204,118,0,
   0,0,204,204,204,120,48,0,
   0,0,198,214,254,254,108,0,
   0,0,198,108,56,108,198,0,
   0,0,204,204,204,124,12,248,
   0,0,252,152,48,100,252,0,
   28,48,48,224,48,48,28,0,
   24,24,24,0,24,24,24,0,
   224,48,48,28,48,48,224,0,
   118,220,0,0,0,0,0,0,
   0,16,56,108,198,198,254,0,
   120,204,192,204,120,24,12,120,
   0,204,0,204,204,204,126,0,
   28,0,120,204,252,192,120,0,
   126,195,60,6,62,102,63,0,
   204,0,120,12,124,204,126,0,
   224,0,120,12,124,204,126,0,
   48,48,120,12,124,204,126,0,
   0,0,120,192,192,120,12,56,
   126,195,60,102,126,96,60,0,
   204,0,120,204,252,192,120,0,
   224,0,120,204,252,192,120,0,
   204,0,112,48,48,48,120,0,
   124,198,56,24,24,24,60,0,
   224,0,112,48,48,48,120,0,
   198,56,108,198,254,198,198,0,
   48,48,0,120,204,252,204,0,
   28,0,252,96,120,96,252,0,
   0,0,127,12,127,204,127,0,
   62,108,204,254,204,204,206,0,
   120,204,0,120,204,204,120,0,
   0,204,0,120,204,204,120,0,
   0,224,0,120,204,204,120,0,
   120,204,0,204,204,204,126,0,
   0,224,0,204,204,204,126,0,
   0,204,0,204,204,124,12,248,
   195,24,60,102,102,60,24,0,
   204,0,204,204,204,204,120,0,
   24,24,126,192,192,126,24,24,
   56,108,100,240,96,230,252,0,
   204,204,120,252,48,252,48,48,
   248,204,204,250,198,207,198,199,
   14,27,24,60,24,24,216,112,
   28,0,120,12,124,204,126,0,
   56,0,112,48,48,48,120,0,
   0,28,0,120,204,204,120,0,
   0,28,0,204,204,204,126,0,
   0,248,0,248,204,204,204,0,
   252,0,204,236,252,220,204,0,
   60,108,108,62,0,126,0,0,
   56,108,108,56,0,124,0,0,
   48,0,48,96,192,204,120,0,
   0,0,0,252,192,192,0,0,
   0,0,0,252,12,12,0,0,
   195,198,204,222,51,102,204,15,
   195,198,204,219,55,111,207,3,
   24,24,0,24,24,24,24,0,
   0,51,102,204,102,51,0,0,
   0,204,102,51,102,204,0,0,
   34,136,34,136,34,136,34,136,
   85,170,85,170,85,170,85,170,
   219,119,219,238,219,119,219,238,
   24,24,24,24,24,24,24,24,
   24,24,24,24,248,24,24,24,
   24,24,248,24,248,24,24,24,
   54,54,54,54,246,54,54,54,
   0,0,0,0,254,54,54,54,
   0,0,248,24,248,24,24,24,
   54,54,246,6,246,54,54,54,
   54,54,54,54,54,54,54,54,
   0,0,254,6,246,54,54,54,
   54,54,246,6,254,0,0,0,
   54,54,54,54,254,0,0,0,
   24,24,248,24,248,0,0,0,
   0,0,0,0,248,24,24,24,
   24,24,24,24,31,0,0,0,
   24,24,24,24,255,0,0,0,
   0,0,0,0,255,24,24,24,
   24,24,24,24,31,24,24,24,
   0,0,0,0,255,0,0,0,
   24,24,24,24,255,24,24,24,
   24,24,31,24,31,24,24,24,
   54,54,54,54,55,54,54,54,
   54,54,55,48,63,0,0,0,
   0,0,63,48,55,54,54,54,
   54,54,247,0,255,0,0,0,
   0,0,255,0,247,54,54,54,
   54,54,55,48,55,54,54,54,
   0,0,255,0,255,0,0,0,
   54,54,247,0,247,54,54,54,
   24,24,255,0,255,0,0,0,
   54,54,54,54,255,0,0,0,
   0,0,255,0,255,24,24,24,
   0,0,0,0,255,54,54,54,
   54,54,54,54,63,0,0,0,
   24,24,31,24,31,0,0,0,
   0,0,31,24,31,24,24,24,
   0,0,0,0,63,54,54,54,
   54,54,54,54,255,54,54,54,
   24,24,255,24,255,24,24,24,
   24,24,24,24,248,0,0,0,
   0,0,0,0,31,24,24,24,
   255,255,255,255,255,255,255,255,
   0,0,0,0,255,255,255,255,
   240,240,240,240,240,240,240,240,
   15,15,15,15,15,15,15,15,
   255,255,255,255,0,0,0,0,
   0,0,118,220,200,220,118,0,
   0,120,204,248,204,248,192,192,
   0,252,204,192,192,192,192,0,
   0,254,108,108,108,108,108,0,
   252,204,96,48,96,204,252,0,
   0,0,126,216,216,216,112,0,
   0,102,102,102,102,124,96,192,
   0,118,220,24,24,24,24,0,
   252,48,120,204,204,120,48,252,
   56,108,198,254,198,108,56,0,
   56,108,198,198,108,108,238,0,
   28,48,24,124,204,204,120,0,
   0,0,126,219,219,126,0,0,
   6,12,126,219,219,126,96,192,
   56,96,192,248,192,96,56,0,
   120,204,204,204,204,204,204,0,
   0,252,0,252,0,252,0,0,
   48,48,252,48,48,0,252,0,
   96,48,24,48,96,0,252,0,
   24,48,96,48,24,0,252,0,
   14,27,27,24,24,24,24,24,
   24,24,24,24,24,216,216,112,
   48,48,0,252,0,48,48,0,
   0,118,220,0,118,220,0,0,
   56,108,108,56,0,0,0,0,
   0,0,0,24,24,0,0,0,
   0,0,0,0,24,0,0,0,
   15,12,12,12,236,108,60,28,
   120,108,108,108,108,0,0,0,
   112,24,48,96,120,0,0,0,
   0,0,60,60,60,60,0,0,
   0,0,0,0,0,0,0,0);

const VGA_Font_8x14 : array [0..3583] of byte =
  (0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,126,129,165,129,129,189,153,129,126,0,0,0,
   0,0,126,255,219,255,255,195,231,255,126,0,0,0,
   0,0,0,108,254,254,254,254,124,56,16,0,0,0,
   0,0,0,16,56,124,254,124,56,16,0,0,0,0,
   0,0,24,60,60,231,231,231,24,24,60,0,0,0,
   0,0,24,60,126,255,255,126,24,24,60,0,0,0,
   0,0,0,0,0,24,60,60,24,0,0,0,0,0,
   255,255,255,255,255,231,195,195,231,255,255,255,255,255,
   0,0,0,0,60,102,66,66,102,60,0,0,0,0,
   255,255,255,255,195,153,189,189,153,195,255,255,255,255,
   0,0,30,14,26,50,120,204,204,204,120,0,0,0,
   0,0,60,102,102,102,60,24,126,24,24,0,0,0,
   0,0,63,51,63,48,48,48,112,240,224,0,0,0,
   0,0,127,99,127,99,99,99,103,231,230,192,0,0,
   0,0,24,24,219,60,231,60,219,24,24,0,0,0,
   0,0,128,192,224,248,254,248,224,192,128,0,0,0,
   0,0,2,6,14,62,254,62,14,6,2,0,0,0,
   0,0,24,60,126,24,24,24,126,60,24,0,0,0,
   0,0,102,102,102,102,102,102,0,102,102,0,0,0,
   0,0,127,219,219,219,123,27,27,27,27,0,0,0,
   0,124,198,96,56,108,198,198,108,56,12,198,124,0,
   0,0,0,0,0,0,0,0,254,254,254,0,0,0,
   0,0,24,60,126,24,24,24,126,60,24,126,0,0,
   0,0,24,60,126,24,24,24,24,24,24,0,0,0,
   0,0,24,24,24,24,24,24,126,60,24,0,0,0,
   0,0,0,0,24,12,254,12,24,0,0,0,0,0,
   0,0,0,0,48,96,254,96,48,0,0,0,0,0,
   0,0,0,0,0,192,192,192,254,0,0,0,0,0,
   0,0,0,0,40,108,254,108,40,0,0,0,0,0,
   0,0,0,16,56,56,124,124,254,254,0,0,0,0,
   0,0,0,254,254,124,124,56,56,16,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,24,60,60,60,24,24,0,24,24,0,0,0,
   0,102,102,102,36,0,0,0,0,0,0,0,0,0,
   0,0,108,108,254,108,108,108,254,108,108,0,0,0,
   24,24,124,198,194,192,124,6,134,198,124,24,24,0,
   0,0,0,0,194,198,12,24,48,102,198,0,0,0,
   0,0,56,108,108,56,118,220,204,204,118,0,0,0,
   0,48,48,48,96,0,0,0,0,0,0,0,0,0,
   0,0,12,24,48,48,48,48,48,24,12,0,0,0,
   0,0,48,24,12,12,12,12,12,24,48,0,0,0,
   0,0,0,0,102,60,255,60,102,0,0,0,0,0,
   0,0,0,0,24,24,126,24,24,0,0,0,0,0,
   0,0,0,0,0,0,0,0,24,24,24,48,0,0,
   0,0,0,0,0,0,254,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,24,24,0,0,0,
   0,0,2,6,12,24,48,96,192,128,0,0,0,0,
   0,0,124,198,206,222,246,230,198,198,124,0,0,0,
   0,0,24,56,120,24,24,24,24,24,126,0,0,0,
   0,0,124,198,6,12,24,48,96,198,254,0,0,0,
   0,0,124,198,6,6,60,6,6,198,124,0,0,0,
   0,0,12,28,60,108,204,254,12,12,30,0,0,0,
   0,0,254,192,192,192,252,6,6,198,124,0,0,0,
   0,0,56,96,192,192,252,198,198,198,124,0,0,0,
   0,0,254,198,6,12,24,48,48,48,48,0,0,0,
   0,0,124,198,198,198,124,198,198,198,124,0,0,0,
   0,0,124,198,198,198,126,6,6,12,120,0,0,0,
   0,0,0,24,24,0,0,0,24,24,0,0,0,0,
   0,0,0,24,24,0,0,0,24,24,48,0,0,0,
   0,0,6,12,24,48,96,48,24,12,6,0,0,0,
   0,0,0,0,0,126,0,0,126,0,0,0,0,0,
   0,0,96,48,24,12,6,12,24,48,96,0,0,0,
   0,0,124,198,198,12,24,24,0,24,24,0,0,0,
   0,0,124,198,198,222,222,222,220,192,124,0,0,0,
   0,0,16,56,108,198,198,254,198,198,198,0,0,0,
   0,0,252,102,102,102,124,102,102,102,252,0,0,0,
   0,0,60,102,194,192,192,192,194,102,60,0,0,0,
   0,0,248,108,102,102,102,102,102,108,248,0,0,0,
   0,0,254,102,98,104,120,104,98,102,254,0,0,0,
   0,0,254,102,98,104,120,104,96,96,240,0,0,0,
   0,0,60,102,194,192,192,222,198,102,58,0,0,0,
   0,0,198,198,198,198,254,198,198,198,198,0,0,0,
   0,0,60,24,24,24,24,24,24,24,60,0,0,0,
   0,0,30,12,12,12,12,12,204,204,120,0,0,0,
   0,0,230,102,108,108,120,108,108,102,230,0,0,0,
   0,0,240,96,96,96,96,96,98,102,254,0,0,0,
   0,0,198,238,254,254,214,198,198,198,198,0,0,0,
   0,0,198,230,246,254,222,206,198,198,198,0,0,0,
   0,0,56,108,198,198,198,198,198,108,56,0,0,0,
   0,0,252,102,102,102,124,96,96,96,240,0,0,0,
   0,0,124,198,198,198,198,214,222,124,12,14,0,0,
   0,0,252,102,102,102,124,108,102,102,230,0,0,0,
   0,0,124,198,198,96,56,12,198,198,124,0,0,0,
   0,0,126,126,90,24,24,24,24,24,60,0,0,0,
   0,0,198,198,198,198,198,198,198,198,124,0,0,0,
   0,0,198,198,198,198,198,198,108,56,16,0,0,0,
   0,0,198,198,198,198,214,214,254,124,108,0,0,0,
   0,0,198,198,108,56,56,56,108,198,198,0,0,0,
   0,0,102,102,102,102,60,24,24,24,60,0,0,0,
   0,0,254,198,140,24,48,96,194,198,254,0,0,0,
   0,0,60,48,48,48,48,48,48,48,60,0,0,0,
   0,0,128,192,224,112,56,28,14,6,2,0,0,0,
   0,0,60,12,12,12,12,12,12,12,60,0,0,0,
   16,56,108,198,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,255,0,
   48,48,24,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,120,12,124,204,204,118,0,0,0,
   0,0,224,96,96,120,108,102,102,102,124,0,0,0,
   0,0,0,0,0,124,198,192,192,198,124,0,0,0,
   0,0,28,12,12,60,108,204,204,204,118,0,0,0,
   0,0,0,0,0,124,198,254,192,198,124,0,0,0,
   0,0,56,108,100,96,240,96,96,96,240,0,0,0,
   0,0,0,0,0,118,204,204,204,124,12,204,120,0,
   0,0,224,96,96,108,118,102,102,102,230,0,0,0,
   0,0,24,24,0,56,24,24,24,24,60,0,0,0,
   0,0,6,6,0,14,6,6,6,6,102,102,60,0,
   0,0,224,96,96,102,108,120,108,102,230,0,0,0,
   0,0,56,24,24,24,24,24,24,24,60,0,0,0,
   0,0,0,0,0,236,254,214,214,214,198,0,0,0,
   0,0,0,0,0,220,102,102,102,102,102,0,0,0,
   0,0,0,0,0,124,198,198,198,198,124,0,0,0,
   0,0,0,0,0,220,102,102,102,124,96,96,240,0,
   0,0,0,0,0,118,204,204,204,124,12,12,30,0,
   0,0,0,0,0,220,118,102,96,96,240,0,0,0,
   0,0,0,0,0,124,198,112,28,198,124,0,0,0,
   0,0,16,48,48,252,48,48,48,54,28,0,0,0,
   0,0,0,0,0,204,204,204,204,204,118,0,0,0,
   0,0,0,0,0,102,102,102,102,60,24,0,0,0,
   0,0,0,0,0,198,198,214,214,254,108,0,0,0,
   0,0,0,0,0,198,108,56,56,108,198,0,0,0,
   0,0,0,0,0,198,198,198,198,126,6,12,248,0,
   0,0,0,0,0,254,204,24,48,102,254,0,0,0,
   0,0,14,24,24,24,112,24,24,24,14,0,0,0,
   0,0,24,24,24,24,0,24,24,24,24,0,0,0,
   0,0,112,24,24,24,14,24,24,24,112,0,0,0,
   0,0,118,220,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,16,56,108,198,198,254,0,0,0,0,
   0,0,60,102,194,192,192,194,102,60,12,6,124,0,
   0,0,204,204,0,204,204,204,204,204,118,0,0,0,
   0,12,24,48,0,124,198,254,192,198,124,0,0,0,
   0,16,56,108,0,120,12,124,204,204,118,0,0,0,
   0,0,204,204,0,120,12,124,204,204,118,0,0,0,
   0,96,48,24,0,120,12,124,204,204,118,0,0,0,
   0,56,108,56,0,120,12,124,204,204,118,0,0,0,
   0,0,0,0,60,102,96,102,60,12,6,60,0,0,
   0,16,56,108,0,124,198,254,192,198,124,0,0,0,
   0,0,204,204,0,124,198,254,192,198,124,0,0,0,
   0,96,48,24,0,124,198,254,192,198,124,0,0,0,
   0,0,102,102,0,56,24,24,24,24,60,0,0,0,
   0,24,60,102,0,56,24,24,24,24,60,0,0,0,
   0,96,48,24,0,56,24,24,24,24,60,0,0,0,
   0,198,198,16,56,108,198,198,254,198,198,0,0,0,
   56,108,56,0,56,108,198,198,254,198,198,0,0,0,
   24,48,96,0,254,102,96,124,96,102,254,0,0,0,
   0,0,0,0,204,118,54,126,216,216,110,0,0,0,
   0,0,62,108,204,204,254,204,204,204,206,0,0,0,
   0,16,56,108,0,124,198,198,198,198,124,0,0,0,
   0,0,198,198,0,124,198,198,198,198,124,0,0,0,
   0,96,48,24,0,124,198,198,198,198,124,0,0,0,
   0,48,120,204,0,204,204,204,204,204,118,0,0,0,
   0,96,48,24,0,204,204,204,204,204,118,0,0,0,
   0,0,198,198,0,198,198,198,198,126,6,12,120,0,
   0,198,198,56,108,198,198,198,198,108,56,0,0,0,
   0,198,198,0,198,198,198,198,198,198,124,0,0,0,
   0,24,24,60,102,96,96,102,60,24,24,0,0,0,
   0,56,108,100,96,240,96,96,96,230,252,0,0,0,
   0,0,102,102,60,24,126,24,126,24,24,0,0,0,
   0,248,204,204,248,196,204,222,204,204,198,0,0,0,
   0,14,27,24,24,24,126,24,24,24,24,216,112,0,
   0,24,48,96,0,120,12,124,204,204,118,0,0,0,
   0,12,24,48,0,56,24,24,24,24,60,0,0,0,
   0,24,48,96,0,124,198,198,198,198,124,0,0,0,
   0,24,48,96,0,204,204,204,204,204,118,0,0,0,
   0,0,118,220,0,220,102,102,102,102,102,0,0,0,
   118,220,0,198,230,246,254,222,206,198,198,0,0,0,
   0,60,108,108,62,0,126,0,0,0,0,0,0,0,
   0,56,108,108,56,0,124,0,0,0,0,0,0,0,
   0,0,48,48,0,48,48,96,198,198,124,0,0,0,
   0,0,0,0,0,0,254,192,192,192,0,0,0,0,
   0,0,0,0,0,0,254,6,6,6,0,0,0,0,
   0,192,192,198,204,216,48,96,220,134,12,24,62,0,
   0,192,192,198,204,216,48,102,206,158,62,6,6,0,
   0,0,24,24,0,24,24,60,60,60,24,0,0,0,
   0,0,0,0,54,108,216,108,54,0,0,0,0,0,
   0,0,0,0,216,108,54,108,216,0,0,0,0,0,
   17,68,17,68,17,68,17,68,17,68,17,68,17,68,
   85,170,85,170,85,170,85,170,85,170,85,170,85,170,
   221,119,221,119,221,119,221,119,221,119,221,119,221,119,
   24,24,24,24,24,24,24,24,24,24,24,24,24,24,
   24,24,24,24,24,24,24,248,24,24,24,24,24,24,
   24,24,24,24,24,248,24,248,24,24,24,24,24,24,
   54,54,54,54,54,54,54,246,54,54,54,54,54,54,
   0,0,0,0,0,0,0,254,54,54,54,54,54,54,
   0,0,0,0,0,248,24,248,24,24,24,24,24,24,
   54,54,54,54,54,246,6,246,54,54,54,54,54,54,
   54,54,54,54,54,54,54,54,54,54,54,54,54,54,
   0,0,0,0,0,254,6,246,54,54,54,54,54,54,
   54,54,54,54,54,246,6,254,0,0,0,0,0,0,
   54,54,54,54,54,54,54,254,0,0,0,0,0,0,
   24,24,24,24,24,248,24,248,0,0,0,0,0,0,
   0,0,0,0,0,0,0,248,24,24,24,24,24,24,
   24,24,24,24,24,24,24,31,0,0,0,0,0,0,
   24,24,24,24,24,24,24,255,0,0,0,0,0,0,
   0,0,0,0,0,0,0,255,24,24,24,24,24,24,
   24,24,24,24,24,24,24,31,24,24,24,24,24,24,
   0,0,0,0,0,0,0,255,0,0,0,0,0,0,
   24,24,24,24,24,24,24,255,24,24,24,24,24,24,
   24,24,24,24,24,31,24,31,24,24,24,24,24,24,
   54,54,54,54,54,54,54,55,54,54,54,54,54,54,
   54,54,54,54,54,55,48,63,0,0,0,0,0,0,
   0,0,0,0,0,63,48,55,54,54,54,54,54,54,
   54,54,54,54,54,247,0,255,0,0,0,0,0,0,
   0,0,0,0,0,255,0,247,54,54,54,54,54,54,
   54,54,54,54,54,55,48,55,54,54,54,54,54,54,
   0,0,0,0,0,255,0,255,0,0,0,0,0,0,
   54,54,54,54,54,247,0,247,54,54,54,54,54,54,
   24,24,24,24,24,255,0,255,0,0,0,0,0,0,
   54,54,54,54,54,54,54,255,0,0,0,0,0,0,
   0,0,0,0,0,255,0,255,24,24,24,24,24,24,
   0,0,0,0,0,0,0,255,54,54,54,54,54,54,
   54,54,54,54,54,54,54,63,0,0,0,0,0,0,
   24,24,24,24,24,31,24,31,0,0,0,0,0,0,
   0,0,0,0,0,31,24,31,24,24,24,24,24,24,
   0,0,0,0,0,0,0,63,54,54,54,54,54,54,
   54,54,54,54,54,54,54,255,54,54,54,54,54,54,
   24,24,24,24,24,255,24,255,24,24,24,24,24,24,
   24,24,24,24,24,24,24,248,0,0,0,0,0,0,
   0,0,0,0,0,0,0,31,24,24,24,24,24,24,
   255,255,255,255,255,255,255,255,255,255,255,255,255,255,
   0,0,0,0,0,0,0,255,255,255,255,255,255,255,
   240,240,240,240,240,240,240,240,240,240,240,240,240,240,
   15,15,15,15,15,15,15,15,15,15,15,15,15,15,
   255,255,255,255,255,255,255,0,0,0,0,0,0,0,
   0,0,0,0,0,118,220,216,216,220,118,0,0,0,
   0,0,0,0,124,198,252,198,198,252,192,192,64,0,
   0,0,254,198,198,192,192,192,192,192,192,0,0,0,
   0,0,0,0,254,108,108,108,108,108,108,0,0,0,
   0,0,254,198,96,48,24,48,96,198,254,0,0,0,
   0,0,0,0,0,126,216,216,216,216,112,0,0,0,
   0,0,0,0,102,102,102,102,124,96,96,192,0,0,
   0,0,0,0,118,220,24,24,24,24,24,0,0,0,
   0,0,126,24,60,102,102,102,60,24,126,0,0,0,
   0,0,56,108,198,198,254,198,198,108,56,0,0,0,
   0,0,56,108,198,198,198,108,108,108,238,0,0,0,
   0,0,30,48,24,12,62,102,102,102,60,0,0,0,
   0,0,0,0,0,126,219,219,126,0,0,0,0,0,
   0,0,3,6,126,219,219,243,126,96,192,0,0,0,
   0,0,28,48,96,96,124,96,96,48,28,0,0,0,
   0,0,0,124,198,198,198,198,198,198,198,0,0,0,
   0,0,0,254,0,0,254,0,0,254,0,0,0,0,
   0,0,0,24,24,126,24,24,0,0,255,0,0,0,
   0,0,48,24,12,6,12,24,48,0,126,0,0,0,
   0,0,12,24,48,96,48,24,12,0,126,0,0,0,
   0,0,14,27,27,24,24,24,24,24,24,24,24,24,
   24,24,24,24,24,24,24,24,216,216,112,0,0,0,
   0,0,0,24,24,0,126,0,24,24,0,0,0,0,
   0,0,0,0,118,220,0,118,220,0,0,0,0,0,
   0,56,108,108,56,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,24,24,0,0,0,0,0,0,
   0,0,0,0,0,0,0,24,0,0,0,0,0,0,
   0,15,12,12,12,12,12,236,108,60,28,0,0,0,
   0,216,108,108,108,108,108,0,0,0,0,0,0,0,
   0,112,216,48,96,200,248,0,0,0,0,0,0,0,
   0,0,0,0,124,124,124,124,124,124,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0);

const VGA_Font_8x16 : array [0..4095] of byte =
  (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,126,129,165,129,129,189,153,129,129,126,0,0,0,0,
   0,0,126,255,219,255,255,195,231,255,255,126,0,0,0,0,
   0,0,0,0,108,254,254,254,254,124,56,16,0,0,0,0,
   0,0,0,0,16,56,124,254,124,56,16,0,0,0,0,0,
   0,0,0,24,60,60,231,231,231,153,24,60,0,0,0,0,
   0,0,0,24,60,126,255,255,126,24,24,60,0,0,0,0,
   0,0,0,0,0,0,24,60,60,24,0,0,0,0,0,0,
   255,255,255,255,255,255,231,195,195,231,255,255,255,255,255,255,
   0,0,0,0,0,60,102,66,66,102,60,0,0,0,0,0,
   255,255,255,255,255,195,153,189,189,153,195,255,255,255,255,255,
   0,0,30,14,26,50,120,204,204,204,204,120,0,0,0,0,
   0,0,60,102,102,102,102,60,24,126,24,24,0,0,0,0,
   0,0,63,51,63,48,48,48,48,112,240,224,0,0,0,0,
   0,0,127,99,127,99,99,99,99,103,231,230,192,0,0,0,
   0,0,0,24,24,219,60,231,60,219,24,24,0,0,0,0,
   0,128,192,224,240,248,254,248,240,224,192,128,0,0,0,0,
   0,2,6,14,30,62,254,62,30,14,6,2,0,0,0,0,
   0,0,24,60,126,24,24,24,24,126,60,24,0,0,0,0,
   0,0,102,102,102,102,102,102,102,0,102,102,0,0,0,0,
   0,0,127,219,219,219,123,27,27,27,27,27,0,0,0,0,
   0,124,198,96,56,108,198,198,108,56,12,198,124,0,0,0,
   0,0,0,0,0,0,0,0,254,254,254,254,0,0,0,0,
   0,0,24,60,126,24,24,24,24,126,60,24,126,0,0,0,
   0,0,24,60,126,24,24,24,24,24,24,24,0,0,0,0,
   0,0,24,24,24,24,24,24,24,126,60,24,0,0,0,0,
   0,0,0,0,0,24,12,254,12,24,0,0,0,0,0,0,
   0,0,0,0,0,48,96,254,96,48,0,0,0,0,0,0,
   0,0,0,0,0,192,192,192,192,254,0,0,0,0,0,0,
   0,0,0,0,0,40,108,254,108,40,0,0,0,0,0,0,
   0,0,0,0,16,56,56,124,124,254,254,0,0,0,0,0,
   0,0,0,0,254,254,124,124,56,56,16,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,24,60,60,60,24,24,24,0,24,24,0,0,0,0,
   0,102,102,102,36,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,108,108,254,108,108,108,254,108,108,0,0,0,0,
   24,24,124,198,194,192,124,6,134,198,124,24,24,0,0,0,
   0,0,0,0,194,198,12,24,48,96,198,134,0,0,0,0,
   0,0,56,108,108,56,118,220,204,204,204,118,0,0,0,0,
   0,48,48,48,96,0,0,0,0,0,0,0,0,0,0,0,
   0,0,12,24,48,48,48,48,48,48,24,12,0,0,0,0,
   0,0,48,24,12,12,12,12,12,12,24,48,0,0,0,0,
   0,0,0,0,0,102,60,255,60,102,0,0,0,0,0,0,
   0,0,0,0,0,24,24,126,24,24,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,24,24,24,48,0,0,0,
   0,0,0,0,0,0,0,254,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,24,24,0,0,0,0,
   0,0,0,0,2,6,12,24,48,96,192,128,0,0,0,0,
   0,0,124,198,198,206,214,214,230,198,198,124,0,0,0,0,
   0,0,24,56,120,24,24,24,24,24,24,126,0,0,0,0,
   0,0,124,198,6,12,24,48,96,192,198,254,0,0,0,0,
   0,0,124,198,6,6,60,6,6,6,198,124,0,0,0,0,
   0,0,12,28,60,108,204,254,12,12,12,30,0,0,0,0,
   0,0,254,192,192,192,252,14,6,6,198,124,0,0,0,0,
   0,0,56,96,192,192,252,198,198,198,198,124,0,0,0,0,
   0,0,254,198,6,6,12,24,48,48,48,48,0,0,0,0,
   0,0,124,198,198,198,124,198,198,198,198,124,0,0,0,0,
   0,0,124,198,198,198,126,6,6,6,12,120,0,0,0,0,
   0,0,0,0,24,24,0,0,0,24,24,0,0,0,0,0,
   0,0,0,0,24,24,0,0,0,24,24,48,0,0,0,0,
   0,0,0,6,12,24,48,96,48,24,12,6,0,0,0,0,
   0,0,0,0,0,0,254,0,0,254,0,0,0,0,0,0,
   0,0,0,96,48,24,12,6,12,24,48,96,0,0,0,0,
   0,0,124,198,198,12,24,24,24,0,24,24,0,0,0,0,
   0,0,0,124,198,198,222,222,222,220,192,124,0,0,0,0,
   0,0,16,56,108,198,198,254,198,198,198,198,0,0,0,0,
   0,0,252,102,102,102,124,102,102,102,102,252,0,0,0,0,
   0,0,60,102,194,192,192,192,192,194,102,60,0,0,0,0,
   0,0,248,108,102,102,102,102,102,102,108,248,0,0,0,0,
   0,0,254,102,98,104,120,104,96,98,102,254,0,0,0,0,
   0,0,254,102,98,104,120,104,96,96,96,240,0,0,0,0,
   0,0,60,102,194,192,192,222,198,198,102,58,0,0,0,0,
   0,0,198,198,198,198,254,198,198,198,198,198,0,0,0,0,
   0,0,60,24,24,24,24,24,24,24,24,60,0,0,0,0,
   0,0,30,12,12,12,12,12,204,204,204,120,0,0,0,0,
   0,0,230,102,108,108,120,120,108,102,102,230,0,0,0,0,
   0,0,240,96,96,96,96,96,96,98,102,254,0,0,0,0,
   0,0,198,238,254,254,214,198,198,198,198,198,0,0,0,0,
   0,0,198,230,246,254,222,206,198,198,198,198,0,0,0,0,
   0,0,56,108,198,198,198,198,198,198,108,56,0,0,0,0,
   0,0,252,102,102,102,124,96,96,96,96,240,0,0,0,0,
   0,0,124,198,198,198,198,198,198,214,222,124,12,14,0,0,
   0,0,252,102,102,102,124,108,102,102,102,230,0,0,0,0,
   0,0,124,198,198,96,56,12,6,198,198,124,0,0,0,0,
   0,0,126,126,90,24,24,24,24,24,24,60,0,0,0,0,
   0,0,198,198,198,198,198,198,198,198,198,124,0,0,0,0,
   0,0,198,198,198,198,198,198,198,108,56,16,0,0,0,0,
   0,0,198,198,198,198,198,214,214,254,108,108,0,0,0,0,
   0,0,198,198,108,108,56,56,108,108,198,198,0,0,0,0,
   0,0,102,102,102,102,60,24,24,24,24,60,0,0,0,0,
   0,0,254,198,134,12,24,48,96,194,198,254,0,0,0,0,
   0,0,60,48,48,48,48,48,48,48,48,60,0,0,0,0,
   0,0,0,128,192,224,112,56,28,14,6,2,0,0,0,0,
   0,0,60,12,12,12,12,12,12,12,12,60,0,0,0,0,
   16,56,108,198,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,255,0,0,
   48,48,24,0,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,120,12,124,204,204,204,118,0,0,0,0,
   0,0,224,96,96,120,108,102,102,102,102,220,0,0,0,0,
   0,0,0,0,0,124,198,192,192,192,198,124,0,0,0,0,
   0,0,28,12,12,60,108,204,204,204,204,118,0,0,0,0,
   0,0,0,0,0,124,198,254,192,192,198,124,0,0,0,0,
   0,0,56,108,100,96,240,96,96,96,96,240,0,0,0,0,
   0,0,0,0,0,118,204,204,204,204,204,124,12,204,120,0,
   0,0,224,96,96,108,118,102,102,102,102,230,0,0,0,0,
   0,0,24,24,0,56,24,24,24,24,24,60,0,0,0,0,
   0,0,6,6,0,14,6,6,6,6,6,6,102,102,60,0,
   0,0,224,96,96,102,108,120,120,108,102,230,0,0,0,0,
   0,0,56,24,24,24,24,24,24,24,24,60,0,0,0,0,
   0,0,0,0,0,236,254,214,214,214,214,214,0,0,0,0,
   0,0,0,0,0,220,102,102,102,102,102,102,0,0,0,0,
   0,0,0,0,0,124,198,198,198,198,198,124,0,0,0,0,
   0,0,0,0,0,220,102,102,102,102,102,124,96,96,240,0,
   0,0,0,0,0,118,204,204,204,204,204,124,12,12,30,0,
   0,0,0,0,0,220,118,98,96,96,96,240,0,0,0,0,
   0,0,0,0,0,124,198,96,56,12,198,124,0,0,0,0,
   0,0,16,48,48,252,48,48,48,48,54,28,0,0,0,0,
   0,0,0,0,0,204,204,204,204,204,204,118,0,0,0,0,
   0,0,0,0,0,102,102,102,102,102,60,24,0,0,0,0,
   0,0,0,0,0,198,198,198,214,214,254,108,0,0,0,0,
   0,0,0,0,0,198,108,56,56,56,108,198,0,0,0,0,
   0,0,0,0,0,198,198,198,198,198,198,126,6,12,248,0,
   0,0,0,0,0,254,204,24,48,96,198,254,0,0,0,0,
   0,0,14,24,24,24,112,24,24,24,24,14,0,0,0,0,
   0,0,24,24,24,24,0,24,24,24,24,24,0,0,0,0,
   0,0,112,24,24,24,14,24,24,24,24,112,0,0,0,0,
   0,0,118,220,0,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,16,56,108,198,198,198,254,0,0,0,0,0,
   0,0,60,102,194,192,192,192,194,102,60,12,6,124,0,0,
   0,0,204,204,0,204,204,204,204,204,204,118,0,0,0,0,
   0,12,24,48,0,124,198,254,192,192,198,124,0,0,0,0,
   0,16,56,108,0,120,12,124,204,204,204,118,0,0,0,0,
   0,0,204,204,0,120,12,124,204,204,204,118,0,0,0,0,
   0,96,48,24,0,120,12,124,204,204,204,118,0,0,0,0,
   0,56,108,56,0,120,12,124,204,204,204,118,0,0,0,0,
   0,0,0,0,60,102,96,96,102,60,12,6,60,0,0,0,
   0,16,56,108,0,124,198,254,192,192,198,124,0,0,0,0,
   0,0,198,198,0,124,198,254,192,192,198,124,0,0,0,0,
   0,96,48,24,0,124,198,254,192,192,198,124,0,0,0,0,
   0,0,102,102,0,56,24,24,24,24,24,60,0,0,0,0,
   0,24,60,102,0,56,24,24,24,24,24,60,0,0,0,0,
   0,96,48,24,0,56,24,24,24,24,24,60,0,0,0,0,
   0,198,198,16,56,108,198,198,254,198,198,198,0,0,0,0,
   56,108,56,0,56,108,198,198,254,198,198,198,0,0,0,0,
   24,48,96,0,254,102,96,124,96,96,102,254,0,0,0,0,
   0,0,0,0,0,204,118,54,126,216,216,110,0,0,0,0,
   0,0,62,108,204,204,254,204,204,204,204,206,0,0,0,0,
   0,16,56,108,0,124,198,198,198,198,198,124,0,0,0,0,
   0,0,198,198,0,124,198,198,198,198,198,124,0,0,0,0,
   0,96,48,24,0,124,198,198,198,198,198,124,0,0,0,0,
   0,48,120,204,0,204,204,204,204,204,204,118,0,0,0,0,
   0,96,48,24,0,204,204,204,204,204,204,118,0,0,0,0,
   0,0,198,198,0,198,198,198,198,198,198,126,6,12,120,0,
   0,198,198,0,56,108,198,198,198,198,108,56,0,0,0,0,
   0,198,198,0,198,198,198,198,198,198,198,124,0,0,0,0,
   0,24,24,60,102,96,96,96,102,60,24,24,0,0,0,0,
   0,56,108,100,96,240,96,96,96,96,230,252,0,0,0,0,
   0,0,102,102,60,24,126,24,126,24,24,24,0,0,0,0,
   0,248,204,204,248,196,204,222,204,204,204,198,0,0,0,0,
   0,14,27,24,24,24,126,24,24,24,24,24,216,112,0,0,
   0,24,48,96,0,120,12,124,204,204,204,118,0,0,0,0,
   0,12,24,48,0,56,24,24,24,24,24,60,0,0,0,0,
   0,24,48,96,0,124,198,198,198,198,198,124,0,0,0,0,
   0,24,48,96,0,204,204,204,204,204,204,118,0,0,0,0,
   0,0,118,220,0,220,102,102,102,102,102,102,0,0,0,0,
   118,220,0,198,230,246,254,222,206,198,198,198,0,0,0,0,
   0,60,108,108,62,0,126,0,0,0,0,0,0,0,0,0,
   0,56,108,108,56,0,124,0,0,0,0,0,0,0,0,0,
   0,0,48,48,0,48,48,96,192,198,198,124,0,0,0,0,
   0,0,0,0,0,0,254,192,192,192,192,0,0,0,0,0,
   0,0,0,0,0,0,254,6,6,6,6,0,0,0,0,0,
   0,192,192,194,198,204,24,48,96,206,147,6,12,31,0,0,
   0,192,192,194,198,204,24,48,102,206,154,63,6,15,0,0,
   0,0,24,24,0,24,24,24,60,60,60,24,0,0,0,0,
   0,0,0,0,0,51,102,204,102,51,0,0,0,0,0,0,
   0,0,0,0,0,204,102,51,102,204,0,0,0,0,0,0,
   17,68,17,68,17,68,17,68,17,68,17,68,17,68,17,68,
   85,170,85,170,85,170,85,170,85,170,85,170,85,170,85,170,
   221,119,221,119,221,119,221,119,221,119,221,119,221,119,221,119,
   24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,
   24,24,24,24,24,24,24,248,24,24,24,24,24,24,24,24,
   24,24,24,24,24,248,24,248,24,24,24,24,24,24,24,24,
   54,54,54,54,54,54,54,246,54,54,54,54,54,54,54,54,
   0,0,0,0,0,0,0,254,54,54,54,54,54,54,54,54,
   0,0,0,0,0,248,24,248,24,24,24,24,24,24,24,24,
   54,54,54,54,54,246,6,246,54,54,54,54,54,54,54,54,
   54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,
   0,0,0,0,0,254,6,246,54,54,54,54,54,54,54,54,
   54,54,54,54,54,246,6,254,0,0,0,0,0,0,0,0,
   54,54,54,54,54,54,54,254,0,0,0,0,0,0,0,0,
   24,24,24,24,24,248,24,248,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,248,24,24,24,24,24,24,24,24,
   24,24,24,24,24,24,24,31,0,0,0,0,0,0,0,0,
   24,24,24,24,24,24,24,255,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,255,24,24,24,24,24,24,24,24,
   24,24,24,24,24,24,24,31,24,24,24,24,24,24,24,24,
   0,0,0,0,0,0,0,255,0,0,0,0,0,0,0,0,
   24,24,24,24,24,24,24,255,24,24,24,24,24,24,24,24,
   24,24,24,24,24,31,24,31,24,24,24,24,24,24,24,24,
   54,54,54,54,54,54,54,55,54,54,54,54,54,54,54,54,
   54,54,54,54,54,55,48,63,0,0,0,0,0,0,0,0,
   0,0,0,0,0,63,48,55,54,54,54,54,54,54,54,54,
   54,54,54,54,54,247,0,255,0,0,0,0,0,0,0,0,
   0,0,0,0,0,255,0,247,54,54,54,54,54,54,54,54,
   54,54,54,54,54,55,48,55,54,54,54,54,54,54,54,54,
   0,0,0,0,0,255,0,255,0,0,0,0,0,0,0,0,
   54,54,54,54,54,247,0,247,54,54,54,54,54,54,54,54,
   24,24,24,24,24,255,0,255,0,0,0,0,0,0,0,0,
   54,54,54,54,54,54,54,255,0,0,0,0,0,0,0,0,
   0,0,0,0,0,255,0,255,24,24,24,24,24,24,24,24,
   0,0,0,0,0,0,0,255,54,54,54,54,54,54,54,54,
   54,54,54,54,54,54,54,63,0,0,0,0,0,0,0,0,
   24,24,24,24,24,31,24,31,0,0,0,0,0,0,0,0,
   0,0,0,0,0,31,24,31,24,24,24,24,24,24,24,24,
   0,0,0,0,0,0,0,63,54,54,54,54,54,54,54,54,
   54,54,54,54,54,54,54,255,54,54,54,54,54,54,54,54,
   24,24,24,24,24,255,24,255,24,24,24,24,24,24,24,24,
   24,24,24,24,24,24,24,248,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,31,24,24,24,24,24,24,24,24,
   255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,
   0,0,0,0,0,0,0,255,255,255,255,255,255,255,255,255,
   240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,240,
   15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
   255,255,255,255,255,255,255,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,118,220,216,216,216,220,118,0,0,0,0,
   0,0,0,0,0,252,198,252,198,198,252,192,192,192,0,0,
   0,0,254,198,198,192,192,192,192,192,192,192,0,0,0,0,
   0,0,0,0,128,254,108,108,108,108,108,108,0,0,0,0,
   0,0,0,254,198,96,48,24,48,96,198,254,0,0,0,0,
   0,0,0,0,0,126,216,216,216,216,216,112,0,0,0,0,
   0,0,0,0,102,102,102,102,102,124,96,96,192,0,0,0,
   0,0,0,0,118,220,24,24,24,24,24,24,0,0,0,0,
   0,0,0,126,24,60,102,102,102,60,24,126,0,0,0,0,
   0,0,0,56,108,198,198,254,198,198,108,56,0,0,0,0,
   0,0,56,108,198,198,198,108,108,108,108,238,0,0,0,0,
   0,0,30,48,24,12,62,102,102,102,102,60,0,0,0,0,
   0,0,0,0,0,126,219,219,219,126,0,0,0,0,0,0,
   0,0,0,3,6,126,207,219,243,126,96,192,0,0,0,0,
   0,0,28,48,96,96,124,96,96,96,48,28,0,0,0,0,
   0,0,0,124,198,198,198,198,198,198,198,198,0,0,0,0,
   0,0,0,0,254,0,0,254,0,0,254,0,0,0,0,0,
   0,0,0,0,24,24,126,24,24,0,0,255,0,0,0,0,
   0,0,0,48,24,12,6,12,24,48,0,126,0,0,0,0,
   0,0,0,12,24,48,96,48,24,12,0,126,0,0,0,0,
   0,0,14,27,27,24,24,24,24,24,24,24,24,24,24,24,
   24,24,24,24,24,24,24,24,216,216,216,112,0,0,0,0,
   0,0,0,0,24,24,0,126,0,24,24,0,0,0,0,0,
   0,0,0,0,0,118,220,0,118,220,0,0,0,0,0,0,
   0,56,108,108,56,0,0,0,0,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,24,24,0,0,0,0,0,0,0,
   0,0,0,0,0,0,0,0,24,0,0,0,0,0,0,0,
   0,15,12,12,12,12,12,236,108,108,60,28,0,0,0,0,
   0,216,108,108,108,108,108,0,0,0,0,0,0,0,0,0,
   0,112,152,48,96,200,248,0,0,0,0,0,0,0,0,0,
   0,0,0,0,124,124,124,124,124,124,124,0,0,0,0,0,
   0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);




const _font_ptr : array[0..2] of pointer =
      (@VGA_Font_8x8,
       @VGA_Font_8x14,
       @VGA_Font_8x16);

const _font_sz : array[0..2] of longword =
      (2048, 3584, 4096);


procedure   BTConsole._SetSystemFont(id:longword);
var ps,pd :pointer;
begin
   if id > 3 then  id := 2;
   ps := _font_ptr[id];
   pd := @aFont[0];
   Move(ps^, pd^, _font_sz[id]);
end;


// just for test

procedure   BTConsole.GClear(C:longword);
var i:longword;
begin
   for i:= 0 to 307200 do xA0000[i] := c;
end;

procedure   BTConsole.GLine(x1,y1,x2,y2:longint; C:longword);
//var //x,t:longword;
    //k,b,y:single;
(*
    Dx,Dy,DDy,_2e,x,y,s:integer;
begin
            // ne boti
//   //DDA
//   k := (y2-y1)/(x2-x1);
//   b := y1 - k*x1;
//   if x2 < x1 then begin t := x1; x1 := x2; x2 := t; end;
//   for x := x1 to x2 do
//   begin
//      y := k*x+b;
//      GBox(x,round(Y),1,1,C);
//   end;


   Dx := x2 - x1;
   Dy := y2 - y1;
   DDy := 2*Dy;
   _2e := 0;
   x := x1;
   y := y1;
   GBox(x,y,1,1,C);

   while (x<>x2)and(y<>y2) do
   begin
   inc(x);
   s := _2e + DDy - Dx;
   if x > 0 then
   begin
     inc(y);
     _2e := s - Dx;
   end else _2e := s + Dx;
   GBox(x,y,1,1,C);

   end;
*)
   var
   i, x, y, x_length, y_length: integer;
   x_inc, y_inc: integer;
begin
     x := x1;
     y := y1;
     x_length := x2 - x1;
     y_length := y2 - y1;
     if x_length <> 0 then x_inc := y_length div x_length else x_inc := 0;
     if y_length <> 0 then y_inc := x_length div y_length else y_inc := 0;
     i := 1;
     while( x <= x2 ) and ( y <= y2 ) do
     begin
         // MyArray[x, y] := True;
          GBox(x,y,1,1,C);
          if( x_inc = 0 ) or ( ( i mod x_inc ) = 0 ) then
             x := x + 1;
          if( y_inc = 0 ) or ( ( i mod y_inc ) = 0 ) then
             y := y + 1;
          i := i + 1;
     end;
end;





procedure   BTConsole.GBox(X,Y,Xl,YL:longint; C:longword);
var ix,iy:longint;
begin
   ix := X + Xl -1;
   iy := Y + Yl -1;
   if ( X < 0  ) then  X := 0;
 //  if ( ix >= longint(aScrXlng) ) then  ix := longint(aScrXlng) -1;
   if ( X > ix ) then  Exit;

   if ( Y < 0  ) then  Y := 0;
 //  if ( iy >= longint(aScrYlng) ) then  iy := longint(aScrYlng) -1;
   if ( Y > iy ) then  Exit;
   Xl := ix - X +1;
   Yl := iy - Y +1;

   for iy := Y to Y+YL -1 do
     for ix := X to X+XL-1 do xA0000[ longword(longword(ix)+longword(iy)*640) ]:=c;
end;

const mask4 : array[0..7] of longword =
     ( $FFFFFFF0,
       $FFFFFF0F,
       $FFFFF0FF,
       $FFFF0FFF,
       $FFF0FFFF,
       $FF0FFFFF,
       $F0FFFFFF,
       $0FFFFFFF);
const mask8 : array[0..3] of longword =
     ( $FFFFFF00,
       $FFFF00FF,
       $FF00FFFF,
       $00FFFFFF);

function    BTConsole.GRbios(a,b,c,d,e,f:longint):longint;
var i,k,w:longint;

   procedure   _GBox(X,Y,Xl,YL:longint; C:longword);
   var ix,iy,pp,cc4,cc,ff:longint;
   begin
      cc4 := $11111111 * (c and $F);
      cc  := $01010101 * (c and $FF);
      ix := X + Xl -1;
      iy := Y + Yl -1;
      if ( X < 0  ) then  X := 0;
//      if ( ix >= longint(aScrXlng) ) then  ix := longint(aScrXlng) -1;
      if ( X > ix ) then  Exit;

      if ( Y < 0  ) then  Y := 0;
//      if ( iy >= longint(aScrYlng) ) then  iy := longint(aScrYlng) -1;
      if ( Y > iy ) then  Exit;
      Xl := ix - X +1;
      Yl := iy - Y +1;

      ff := 2;
      if aGraphic256 = 1 then ff := 3;
      XL := X+XL-1;
      for iy := Y to Y+YL -1 do
      begin
         case aGraphic256 of
            0: for ix := X to XL do xA0000[ longword(longword(ix)+longword(iy)*640) ]:=c;
            1,2: begin
               for ix := X to XL do
               begin
                  pp := longword(longword(ix shr 2)+longword(iy)*(640 shr ff));  //todo
                  xA0000[pp]:= (cc and (not mask8[ix and 3])) or (xA0000[pp] and mask8[ix and 3]);
               end;
            end;
            3: begin
               for ix := X to XL do
               begin
                  pp := longword(longword(ix shr 3)+longword(iy)*80{640 shr 3}); //todo
                  xA0000[pp]:= (cc4 and (not mask4[ix and 7])) or (xA0000[pp] and mask4[ix and 7]);
               end;
            end;
         end;
      end;
   end;

begin
   Result := 0;   // TINY GRAPHIC LIB  by Bogi aka sdex32 ;)
   if (a > 1) and not aGraphicON then Exit;
   case a of
      0: begin  // close GR mode
         SetProp(23,0);
         SetProp(34,0);
      end;
      1: begin  // init graphic mode b=mode
         SetProp(23,1);  // gr mode ON
         SetProp(34,b);  // 0 - real mode rrggbb longword
                         // 1 320x240 pal 256 colors
                         // 2 640x480 pal 256 oolors
                         // 3 640x480 pal 16 colors
      end;
      2: begin  // clear screen
         w := b;
         k := 307200;
         case aGraphic256 of
           1: begin w := $01010101 * (b and $FF); k:= 19200; end;
           2: begin w := $01010101 * (b and $FF); k:= 76800; end;
           3: begin w := $11111111 * (b and $F);  k:= 38400; end;
         end;
         for i := 0 to k - 1 do xA0000[i] := w;
      end;
      3: begin  // Put Pixel
         _GBox(b,c,1,1,d);
      end;
      4: begin  // Get pixel

      end;
      5: begin  // Line

      end;
      6: begin  // Rectangle   b=xpos c=ypos d=xlng e=ylng f=color
         i := b+d-1; //X2   x1=b
         k := c+e-1; //Y2   y1=c
         GRbios(5,b,c,b,k,f);     // bc   ic
         GRbios(5,b,c,i,c,f);     //
         GRbios(5,b,k,i,k,f);     // bk   ik
         GRbios(5,i,c,i,k,f);     //
      end;
      7: begin  // Box
         _GBox(b,c,d,e,f);
      end;
      8: begin  // Ellipse

      end;
      9: begin  // Pie

      end;
      10:begin  // Text

      end;
      11:begin  // Draw image

      end;
      12:begin  // Read image

      end;
      13:begin  // Set Pal
         SetProp(33,1);
         SetProp(b,c);
      end;
      14:begin  // Get Pal
         SetProp(33,1);
         Result := GetProp(b);
      end;
   end;
end;



end.
