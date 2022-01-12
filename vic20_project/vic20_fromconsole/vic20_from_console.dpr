program vic20_from_console;

uses
  Vcl.Forms,
  vic20_from_console_Unit1 in 'vic20_from_console_Unit1.pas' {Form1},
  BConsole in '..\..\blib\BConsole.pas',
  BFileTools in '..\..\blib\BFileTools.pas',
  BStrTools in '..\..\blib\BStrTools.pas',
  BVic20 in '..\vic20_emulator\BVic20.pas',
  BVic20_Games in '..\vic20_emulator\BVic20_Games.pas',
  BWinMMSound in '..\..\blib\BWinMMSound.pas',
  vic20_from_console_Unit2 in 'vic20_from_console_Unit2.pas' {Console};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TConsole, Console);
  Application.Run;
end.
