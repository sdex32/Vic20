program vic20;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses
{$IFnDEF FPC}
{$ELSE}
  Interfaces,
{$ENDIF}
  Forms,
  vic20_unit in 'vic20_unit.pas' {Form1},
  BWinMMSound in '..\..\blib\BWinMMSound.pas',
  BFileTools in '..\..\blib\BFileTools.pas',
  BStrTools in '..\..\blib\BStrTools.pas',
  BVic20 in '..\vic20_emulator\BVic20.pas',
  BVic20_Games in '..\vic20_emulator\BVic20_Games.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
