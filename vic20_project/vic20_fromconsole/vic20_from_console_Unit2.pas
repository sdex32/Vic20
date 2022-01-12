unit vic20_from_console_Unit2;

interface

uses
  BConsole, BVic20, BVic20_Games, BFileTools, BStrTools,

  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TConsole = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
     Bcon :BTConsole;
  end;

var
  Console: TConsole;

implementation

{$R *.dfm}


function BTConsole_Run(con:BTconsole; cmd:ansistring):string; stdcall;
var done,done_cmd:boolean;
    the_cmd:string;
    the_params:string;
    scmd:string;
    Vic20 :TVic20;
    p:pointer;
    s:ansistring;
    k,w:longword;

begin
   // simple command interpreter

   scmd:=Trim(string(cmd));
   the_cmd := UpperCase(ParseStr(scmd,0,' '));
   if length(scmd) > length(the_cmd)then
   begin
      the_params := Trim(RightStr(scmd,length(scmd) - length(the_cmd)));
   end;
   if length(the_cmd) > 0 then if pos('.CMD',the_cmd) = 0  then the_cmd := the_cmd + '.CMD';
   done_cmd := false;


   if the_cmd = 'HELLO.CMD' then
   begin
      con.ClrScr;
      con.TextColor(7);
      con.Write('The ');
      con.TextColor(10);
      con.Write('CONSOLE');
      con.TextColor(7);
      con.WriteLn(' v2.2.0 2021');
      con.TextColor(15);
      con.WriteLn('    Welcome to console demo :)');
      con.TextColor(7);
      con.WriteLn;
      con.WriteLn('To test console type command and press enter ( work without .cmd)');
      con.WriteLn(' cls');
      con.WriteLn(' Hello.cmd');
      con.WriteLn(' vic20.cmd or vic20.cmd /?');
      con.WriteLn;
      done_cmd := true;
   end;

   if the_cmd = 'CLS.CMD' then
   begin
      con.ClrScr;
      done_cmd := true;
   end;

   if the_cmd = 'VIC20.CMD' then
   begin
     //  HH   HH HH  HHHHH    HHHHH   HHHHH
     //  HH   HH HH HH   HH  HH   HH HH   HH
     //  HH   HH HH HH          HHH  HH   HH
     //   HH HH  HH HH   HH   HH     HH   HH
     //    HHH   HH  HHHHH   HHHHHHH  HHHHH
      if the_params = '/?' then
      begin
      con.TextColor(4);
      con.Write(#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF);
      con.TextColor(15);
      con.WriteLn(#32#32#$DB#$DB#32#32#32#$DB#$DB#32#$DB#$DB#32#32#$DB#$DB#$DB#$DB#$DB#32#32#32#32#$DB#$DB#$DB#$DB#$DB#32#32#32#$DB#$DB#$DB#$DB#$DB+'    E M U L A T O R');
      con.TextColor(12);
      con.Write(#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF);
      con.TextColor(15);
      con.Write(#32#32#$DB#$DB#32#32#32#$DB#$DB#32#$DB#$DB#32#$DB#$DB#32#32#32#$DB#$DB#32#32#$DB#$DB#32#32#32#$DB#$DB#32#$DB#$DB#32#32#32#$DB#$DB);
      con.TextColor(8);
      con.WriteLn('     by Bogi aka SDEX32');
      con.TextColor(14);
      con.Write(#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF);
      con.TextColor(15);
      con.Write(#32#32#$DB#$DB#32#32#32#$DB#$DB#32#$DB#$DB#32#$DB#$DB#32#32#32#32#32#32#32#32#32#32#$DB#$DB#$DB#32#32#$DB#$DB#32#32#32#$DB#$DB);
      con.TextColor(8);
      con.WriteLn('     spring 2021 :)');
      con.TextColor(10);
      con.Write(#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF);
      con.TextColor(15);
      con.WriteLn(#32#32#32#$DB#$DB#32#$DB#$DB#32#32#$DB#$DB#32#$DB#$DB#32#32#32#$DB#$DB#32#32#32#$DB#$DB#32#32#32#32#32#$DB#$DB#32#32#32#$DB#$DB);
      con.TextColor(9);
      con.Write(#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF#$DF);
      con.TextColor(15);
      con.WriteLn(#32#32#32#32#$DB#$DB#$DB#32#32#32#$DB#$DB#32#32#$DB#$DB#$DB#$DB#$DB#32#32#32#$DB#$DB#$DB#$DB#$DB#$DB#$DB#32#32#$DB#$DB#$DB#$DB#$DB#32);

         con.TextColor(7);
         con.WriteLn;
         con.WriteLn('   Press ESC to exit');
         con.WriteLn('   Use param after VIC20 [0,1,2,3,4,fn]');
         con.WriteLn('   0-Blitz 1-Bonzo 2-Avenger 3-Star Battle 4-SargonII chess');
         con.WriteLn('   fn-path to the rom');
         con.Writeln;
      end else begin
      con.SetProp(23,1);  // enter graphic mode
      //                              do not create  run thread   vic thread     keyboard
      Vic20 := TVic20.create(con.GetProp(40),0,0,0,0,$80000000 or $40000000  or  $10000000 );
      p := VIC20;
      con.SetGetPtr(4,p); // set parm  before call to avid call with null
      p := @VIC20_render;
      con.SetGetPtr(3,p);
      p := VIC20;
      con.SetGetPtr(6,p);
      p := @VIC20_events;
      con.SetGetPtr(5,p);

//      con.SetProp(29,longword(Vic20));
//      con.SetProp(28,longword(@VIC20_render));
//      con.SetProp(31,longword(Vic20));
//      con.SetProp(30,longword(@VIC20_events));

      con.SetProp(32,1);

      if length(the_params) <> 0 then
      begin
         p := nil;
         if length(the_params) = 1 then
         begin
            k := toVal(the_params);
            p := GetVIC20_Game_Rom(k,w);
         end else begin
            if FileLoad(the_params,s) then
            begin
               w := length(s);
               p := @s[1];
            end;
            if ExtractFileExt(the_params) = '.20' then w := w or $20000000;
            if ExtractFileExt(the_params) = '.60' then w := w or $60000000;
         end;
         if p<> nil then
         begin
            Vic20.Load_PRG(p,w);
         end;
      end;


      done := false;
      repeat
         if con.KeyPressed then
         begin
            case con.GetKey of
             27: done := true;
            end;

         end;
         Vic20.CpuStep;
      until done;
      p:= nil;
      con.SetGetPtr(3,p);
      p:= nil;
      con.SetGetPtr(5,p);
//      con.SetProp(28,0);
//      con.SetProp(30,0);
      con.SetProp(32,0);
      sleep(200);
      Vic20.Free;
      con.SetProp(23,0); // back to text mode
      con.Reset;
      con.ClrScr;
      end;
      done_cmd := true;
   end;



   if (length(the_cmd)>0) and ( not done_cmd) then con.WriteLn('Unknown command');
end;

procedure TConsole.FormCreate(Sender: TObject);
var xl,yl:longword;
begin
   Bcon := BTConsole.create;
   Bcon.SetMode(BTC_Mode_VGA80x25,self.Handle,0,0,1280,960,1 or 2,xl,yl);
   self.ClientWidth := xl;   // set need window size
   self.ClientHeight := yl;
   Bcon.SetInterpreter(@BTConsole_Run);
   Bcon.Execute('Hello.cmd');
end;

procedure TConsole.FormDestroy(Sender: TObject);
begin
   Bcon.Free;
end;

procedure TConsole.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   Bcon.Interupt(2,longword(Key),0);
end;

procedure TConsole.FormKeyPress(Sender: TObject; var Key: Char);
begin
   Bcon.Interupt(0,longword(Key),0);
end;

procedure TConsole.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
   Bcon.Interupt(1,longword(Key),0);
end;

procedure TConsole.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   Bcon.Interupt(3,longword(X),longword(Y));
   if mbLeft = Button then Bcon.Interupt(4,1,0);
   if mbRight = Button then Bcon.Interupt(4,2,0);
   if mbMiddle = Button then Bcon.Interupt(4,3,0);
end;

procedure TConsole.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
   Bcon.Interupt(3,longword(X),longword(Y));
end;

procedure TConsole.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
   Bcon.Interupt(3,longword(X),longword(Y));
   if mbLeft = Button then Bcon.Interupt(5,1,0);
   if mbRight = Button then Bcon.Interupt(5,2,0);
   if mbMiddle = Button then Bcon.Interupt(5,3,0);
end;

end.
