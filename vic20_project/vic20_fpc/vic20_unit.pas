unit vic20_unit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  BVic20, BVic20_Games, BFileTools,
  Windows, Messages,
  Forms,
  Dialogs,
  Menus, Classes;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    Fille1: TMenuItem;
    Reset1: TMenuItem;
    LoadRom01: TMenuItem;
    Load1: TMenuItem;
    LoadAvengers1: TMenuItem;
    LoadAvengers2: TMenuItem;
    LoadSargonchessII1: TMenuItem;
    LoadRomFile1: TMenuItem;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure Reset1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LoadRom01Click(Sender: TObject);
    procedure Load1Click(Sender: TObject);
    procedure LoadRomFile1Click(Sender: TObject);
    procedure LoadAvengers1Click(Sender: TObject);
    procedure LoadAvengers2Click(Sender: TObject);
    procedure LoadSargonchessII1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  mvic20 :TVIC20;
implementation

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TForm1.FormCreate(Sender: TObject);
begin
  mvic20 :=TVIC20.create(handle,0,0,0,0,0);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  mvic20.free;
end;

procedure TForm1.Load1Click(Sender: TObject);
var p:pointer;
    w:longword;
begin
  p:= GetVIC20_Game_Rom(1,w);
  mvic20.Load_PRG(p,w);
end;

procedure TForm1.LoadAvengers1Click(Sender: TObject);
var p:pointer;
    w:longword;
begin
  p:= GetVIC20_Game_Rom(2,w);
  mvic20.Load_PRG(p,w);
end;

procedure TForm1.LoadAvengers2Click(Sender: TObject);
var p:pointer;
    w:longword;
begin
  p:= GetVIC20_Game_Rom(3,w);
  mvic20.Load_PRG(p,w);
end;

procedure TForm1.LoadRom01Click(Sender: TObject);
var p:pointer;
    w:longword;
begin
  p:= GetVIC20_Game_Rom(0,w);
  mvic20.Load_PRG(p,w);
end;

procedure TForm1.LoadRomFile1Click(Sender: TObject);
var p:pointer;
    w:longword;
    s:ansistring;
    n:string;
begin
  OpenDialog1.execute;
  if assigned(mVic20) then
  begin
    n := OpenDialog1.Files.strings[0];
    if FileLoad(n,s) then
    begin
       p := @s[1];
       w := length(s);
       if ExtractFileExt(n) = '.20' then w := w or $20000000;
       if ExtractFileExt(n) = '.60' then w := w or $60000000;

       mVic20.Load_PRG(p,w);
    end;
   end;

end;

procedure TForm1.LoadSargonchessII1Click(Sender: TObject);
var p:pointer;
    w:longword;
begin
  p:= GetVIC20_Game_Rom(4,w);
  mvic20.Load_PRG(p,w);
end;

procedure TForm1.Reset1Click(Sender: TObject);
begin
  mvic20.MasterReset;
end;

end.
