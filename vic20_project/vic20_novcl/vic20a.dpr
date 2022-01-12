
program vic20a;

uses
  windows,
  messages,
  commctrl,
  BWinMMSound in '..\..\blib\BWinMMSound.pas',
  BVic20_Games in '..\vic20_emulator\BVic20_Games.pas',
  BVic20 in '..\vic20_emulator\BVic20.pas';

{ $ R 'icon.res'}

var
  WinClass : TWndClass;
  hInst : HWND;
  Handle : HWND;
  Msg : TMSG;
  Menu : HMENU;
  PMenu : HMENU;
  vic :TVic20;
  rectWindow :TRect;
  rectClient :TRect;
  Height,Width :longint;
  p:pointer;
  w:longword;

const
  menu_id_1 = 271;
  menu_id_2 = 272;
  menu_id_3 = 273;
  menu_id_4 = 274;
  menu_id_5 = 275;
  menu_id_6 = 276;
  menu_id_7 = 277;

procedure ShutDown;
begin
  UnRegisterClass('Vic20form', hInst);
  ExitProcess(hInst);
end;

function WindowProc(hwnd, msg, wparam, lparam: longint): longint; stdcall;
begin
  Result := DefWindowProc(hwnd, msg, wparam, lparam);
  case Msg of

WM_COMMAND:
begin
  case  LoWord(wParam) of
     menu_id_1 : begin // reset
        vic.MasterReset;
     end;
     menu_id_2 : begin // rom 0
        p := GetVIC20_Game_Rom(0,w);
        vic.Load_PRG(p,w);
     end;
     menu_id_3 : begin // rom 1
        p := GetVIC20_Game_Rom(1,w);
        vic.Load_PRG(p,w);
     end;
     menu_id_4 : begin // rom 2
        p := GetVIC20_Game_Rom(2,w);
        vic.Load_PRG(p,w);
     end;
     menu_id_5 : begin // rom 3
        p := GetVIC20_Game_Rom(3,w);
        vic.Load_PRG(p,w);
     end;
     menu_id_6 : begin // rom 4
        p := GetVIC20_Game_Rom(4,w);
        vic.Load_PRG(p,w);
     end;
     menu_id_7 : begin // from file
         //TODO

     end;

  end;
end;

  WM_DESTROY: ShutDown;
  end;
end;

begin
hInst := GetModuleHandle(nil);
  with WinClass do
  begin
   Style := CS_PARENTDC;
   hIcon := LoadIcon(hInst, MAKEINTRESOURCE('VIC20ICON'));
   lpfnWndProc := @WindowProc;
   hInstance := hInst;
   hbrBackground := GetStockObject(BLACK_BRUSH);  //COLOR_BTNFACE + 1;
   lpszClassName := 'Vic20form';
   hCursor := LoadCursor(0, IDC_ARROW);
  end;
InitCommonControls;
RegisterClass(WinClass);


Handle := CreateWindowEx(0, 'Vic20form', 'VIC20',
WS_OVERLAPPEDWINDOW or
WS_VISIBLE or WS_MINIMIZEBOX or WS_MAXIMIZEBOX or WS_SYSMENU,
219, 112, 550, 366,
0, 0,
hInst, nil);

Menu := CreateMenu;
PMenu := CreatePopupMenu;
AppendMenu(Menu, MF_POPUP, PMenu, 'VIC20');
AppendMenu(PMenu, MF_STRING, menu_id_1, 'Reset');
AppendMenu(PMenu, MF_STRING, menu_id_2, 'Load Blitz (type run)');
AppendMenu(PMenu, MF_STRING, menu_id_3, 'Load Bonzo (type run)');
AppendMenu(PMenu, MF_STRING, menu_id_4, 'Load Rom Avenger');
AppendMenu(PMenu, MF_STRING, menu_id_5, 'Load Rom Star Battle');
AppendMenu(PMenu, MF_STRING, menu_id_6, 'Load Rom Sargon II');
AppendMenu(PMenu, MF_STRING, menu_id_7, 'Load Rom from file');
SetMenu(Handle, Menu);

   GetWindowRect(Handle, rectWindow );
   GetClientRect(Handle, rectClient);
   Width := ((rectWindow.Right  - rectWindow .Left) - rectClient.Right) + 640 + 20;
   Height:= ((rectWindow.Bottom - rectWindow .Top)  - rectClient.Bottom) + 480 + 20;
   SetWindowPos(Handle, 0, 0, 0, Width, Height, SWP_NOZORDER or SWP_NOMOVE);


   vic := TVic20.create(Handle,10,10,640,480,0);

  while(GetMessage(Msg, 0, 0, 0)) do
  begin
    TranslateMessage(Msg);
    DispatchMessage(Msg);
  end;
   Vic.Free;

  end.

