unit BVic20;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$MESSAGE ' VIC20 emulator by sdex32 :)'}

{ $ IFDEF CPUX64}
{ $ MESSAGE FATAL ' VIC20 code is only for 32bit mode sorry :('} // I done it
{ $ ENDIF}

interface

{ TODO
     keyboard
     timig problem with sound
     unsuported instructions
     Screen Auto adjust Y
     on exit with esc close thread before exit
}



uses  BWinMMSound,Windows,Messages;

const
      // Status bits
      NEGATIVE = $80;
      OVERFLOW = $40;
      CONSTANT = $20;
      BREAK = $10;
      DECIMAL = $08;
      INTERRUPT = $04;
      ZERO = $02;
      CARRY = $01;
      // IRQ, reset, NMI vectors
      IRQVECTORH: Word = $FFFF;
      IRQVECTORL: Word = $FFFE;
      RSTVECTORH: Word = $FFFD;
      RSTVECTORL: Word = $FFFC;
      NMIVECTORH: Word = $FFFB;
      NMIVECTORL: Word = $FFFA;

type
      TCodeExec = procedure(Src: Word) of object;
      TAddrExec = function: Word of object;
      TInstr = record
         Addr: TAddrExec;
         Code: TCodeExec;
      end;
      PInstr = ^TInstr;
      TBusWrite = procedure(Adr: Word; Value: Byte) of object;
      TBusRead = function(Adr: Word): Byte of object;



{$DEFINE USE_INLINE}
type
      TMOS6502 = class
         private
            // consumed clock cycles
            Cycles: Cardinal;
            InstrTable: Array [0 .. 255] of TInstr;
            // read/write callbacks
            Read: TBusRead;
            Write: TBusWrite;
            // program counter
            Pc: Word;
            // registers
            A: Byte; // accumulator
            X: Byte; // X-index
            Y: Byte; // Y-index
            // stack pointer
            Sp: Byte;
            // status register
            Status: Byte;
            IllegalOpcode: Boolean;

            procedure SET_NEGATIVE(const Value: Boolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
            procedure SET_OVERFLOW(const Value: Boolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
            procedure SET_CONSTANT(const Value: Boolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
            procedure SET_BREAK(const Value: Boolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
            procedure SET_DECIMAL(const Value: Boolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
            procedure SET_INTERRUPT(const Value: Boolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
            procedure SET_ZERO(const Value: Boolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
            procedure SET_CARRY(const Value: Boolean); {$IFDEF USE_INLINE} inline; {$ENDIF}
            function IF_NEGATIVE: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
            function IF_OVERFLOW: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
            //function IF_CONSTANT: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF};
            //function IF_BREAK: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF};
            function IF_DECIMAL: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
            function IF_INTERRUPT: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
            function IF_ZERO: Boolean; {$IFDEF USE_INLINE} inline; {$ENDIF}
            function IF_CARRY: Byte; {$IFDEF USE_INLINE} inline; {$ENDIF}

            // addressing modes
            function Addr_ACC: Word; // ACCUMULATOR
            function Addr_IMM: Word; // IMMEDIATE
            function Addr_ABS: Word; // ABSOLUTE
            function Addr_ZER: Word; // ZERO PAGE
            function Addr_ZEX: Word; // INDEXED-X ZERO PAGE
            function Addr_ZEY: Word; // INDEXED-Y ZERO PAGE
            function Addr_ABX: Word; // INDEXED-X ABSOLUTE
            function Addr_ABY: Word; // INDEXED-Y ABSOLUTE
            function Addr_IMP: Word; // IMPLIED
            function Addr_REL: Word; // RELATIVE
            function Addr_INX: Word; // INDEXED-X INDIRECT
            function Addr_INY: Word; // INDEXED-Y INDIRECT
            function Addr_ABI: Word; // ABSOLUTE INDIRECT

            // opcodes (grouped as per datasheet)
            procedure Op_ADC(Src: Word);
            procedure Op_AND(Src: Word);
            procedure Op_ASL(Src: Word);
            procedure Op_ASL_ACC(Src: Word);
            procedure Op_BCC(Src: Word);
            procedure Op_BCS(Src: Word);
            procedure Op_BEQ(Src: Word);
            procedure Op_BIT(Src: Word);
            procedure Op_BMI(Src: Word);
            procedure Op_BNE(Src: Word);
            procedure Op_BPL(Src: Word);
            procedure Op_BRK(Src: Word);
            procedure Op_BVC(Src: Word);
            procedure Op_BVS(Src: Word);
            procedure Op_CLC(Src: Word);
            procedure Op_CLD(Src: Word);
            procedure Op_CLI(Src: Word);
            procedure Op_CLV(Src: Word);
            procedure Op_CMP(Src: Word);
            procedure Op_CPX(Src: Word);
            procedure Op_CPY(Src: Word);
            procedure Op_DEC(Src: Word);
            procedure Op_DEX(Src: Word);
            procedure Op_DEY(Src: Word);
            procedure Op_EOR(Src: Word);
            procedure Op_INC(Src: Word);
            procedure Op_INX(Src: Word);
            procedure Op_INY(Src: Word);
            procedure Op_JMP(Src: Word);
            procedure Op_JSR(Src: Word);
            procedure Op_LDA(Src: Word);
            procedure Op_LDX(Src: Word);
            procedure Op_LDY(Src: Word);
            procedure Op_LSR(Src: Word);
            procedure Op_LSR_ACC(Src: Word);
            procedure Op_NOP(Src: Word);
            procedure Op_ORA(Src: Word);
            procedure Op_PHA(Src: Word);
            procedure Op_PHP(Src: Word);
            procedure Op_PLA(Src: Word);
            procedure Op_PLP(Src: Word);
            procedure Op_ROL(Src: Word);
            procedure Op_ROL_ACC(Src: Word);
            procedure Op_ROR(Src: Word);
            procedure Op_ROR_ACC(Src: Word);
            procedure Op_RTI(Src: Word);
            procedure Op_RTS(Src: Word);
            procedure Op_SBC(Src: Word);
            procedure Op_SEC(Src: Word);
            procedure Op_SED(Src: Word);
            procedure Op_SEI(Src: Word);
            procedure Op_STA(Src: Word);
            procedure Op_STX(Src: Word);
            procedure Op_STY(Src: Word);
            procedure Op_TAX(Src: Word);
            procedure Op_TAY(Src: Word);
            procedure Op_TSX(Src: Word);
            procedure Op_TXA(Src: Word);
            procedure Op_TXS(Src: Word);
            procedure Op_TYA(Src: Word);
            procedure Op_ILLEGAL(Src: Word);

            // stack operations
            procedure StackPush(const Value: Byte); {$IFDEF USE_INLINE} inline; {$ENDIF}
            function  StackPop: Byte; {$IFDEF USE_INLINE} inline; {$ENDIF}

         public
            constructor Create(R: TBusRead; W: TBusWrite); overload; virtual;
            procedure NMI; //virtual;
            procedure IRQ; //virtual;
            procedure Reset; //virtual;
            function Step:longword; //virtual;
      end;


      TVIC20 = class(TMOS6502)
         private
            aWorkMode :longword;
            Host_HWND :longword;
            Host_DC :longword;
{$IFDEF CPUX64}
            Host_CBack :NativeUint;
{$ELSE}
            Host_CBack :longword;
{$ENDIF}
            Host_Xpos :longword;
            Host_Ypos :longword;
            Host_Xlng :longword;
            Host_Ylng :longword;
            Terminated :boolean;
            Thread: longword;
            Thread_ID: longword;
            ThreadVIC: longword;
            ThreadVIC_ID: longword;
            aMemExt : longword;
            aLastKey : longword;
            aKeyState : longword;
            aKeyTranslate : boolean;
            aSynch :boolean;
            aRun :boolean;
            aRunMR :boolean;
            aClockRes :int64;
            aTicksStamp :int64;
            aClk :longint;
            aPal:longword;
            KeyMatrix: Array[0 .. 7, 0 .. 7] of Byte;
            Memory: Array [0..65537] of byte; //PByte; 0-FFFF
            ScrBitmap: Array[0..(640*480)-1] of longword; //rgb  307200*4  1228800 bytes
            Pal: Array[0..15] of longword;
            //Video chip VIC
            aTxtRam : pointer;
            aColRam : pointer;
            aFntRam : pointer;
            aBgrColor : longword;
            aFrmColor : longword;
            aAuxColor : longword;
            aInvertMode : longword;
            aLeftMargin : longword;
            aTopMargin : longword;
            aLeftMarginAdj : longint;
            aTopMarginAdj : longint;
            aTxtRows : longword;
            aTxtCols : longword;
            aTxtXlng : longword;
            aTxtYlng : longword;
            a16x8char : longword;
            //VIA
            aJoyEmulation : boolean;
            aJoy : longword;
            aVIA_PIB  : array [1..2] of longword; {port B input}
            aVIA_PIA  : array [1..2] of  longword; {port A input}
            aVIA_PB  : array [1..2] of longword; {port B}
            aVIA_PA  : array [1..2] of  longword; {port A}
            aVIA_PBD : array [1..2] of  longword; {port B direction}
            aVIA_PAD : array [1..2] of  longword; {port A direction}
            aVIA_T1L : array [1..2] of  longint; {timer}
            aVIA_T1C : array [1..2] of  longint;
            aVIA_T2C : array [1..2] of  longint;
            aVIA_SR : array [1..2] of  longword; {shift register}
            aVIA_ACR : array [1..2] of  longword; {Auxilary Control Register}
            aVIA_PCR : array [1..2] of  longword; {Peripheral Control Register}
            aVIA_IFR : array [1..2] of  longword; {Interrupt Flag Register }
            aVIA_IER : array [1..2] of  longword; {Interrupt Enable Register }
            aVIA_inhibitT1Interrupt : array [1..2] of  boolean;
            aVIA_inhibitT2Interrupt : array [1..2] of  boolean;
            aVIA_acrTimedCountdown : array [1..2] of  boolean;
            aVIA_ca1 : array [1..2] of  boolean;
            aVIA_lastca1 : array [1..2] of  boolean;
            aVIA_hasInterrupt : array [1..2] of  boolean;
            aVIA_hasPreCycled : array [1..2] of  boolean;
            //sound
            aNoise:array[0..1023] of byte;
            aBSound :BTWinMMSound;
            abshand :longword;
            aSp1_ofs :longword;
            aSp2_ofs :longword;
            aSp3_ofs :longword;
            aSp4_ofs :longword;
            aSp1_dx :longword;
            aSp2_dx :longword;
            aSp3_dx :longword;
            aSp4_dx :longword;
            aSp4_noise :longword;
            aSp1_State :longword;
            aSp2_State :longword;
            aSp3_State :longword;
            aSp4_State :longword;
            aVolume : single;

            procedure SetPal;
            procedure BusWrite(Adr: Word; Value: Byte);
            function  BusRead(Adr: Word): Byte;
            procedure LoadROM(Rom:Pointer; Addr,Size: longword);
            procedure WriteDW(adr,w:longword);
            procedure VIA_CycleUp(via:longword);
            procedure VIA_CycleDown(via:longword;Cls:longint);
            procedure VIA_Reset(via:longword);
            function  ScrRender:pointer;
            function  SoundRender(data:pointer; len:longword):longint;
            procedure KeyEvent(V_Key,Key_State:longword);
         public
            constructor Create(whand,x,y,xl,yl,mode:longword);
            destructor  Destroy; override;
            procedure   Load_PRG(prg:pointer; Sz:longword);
            procedure   MasterReset;
            procedure   CpuStep;

      end;

// call backs
function VIC20_Render(obj:pointer):pointer;  stdcall;
function VIC20_Events(obj:pointer; key,prs,ks:longword):longint;  stdcall;
function VIC20_SoundGen(obj:pointer; data:pointer; len:longword):longint; stdcall;


implementation

const
      Opc_Cycles:array[0..255] of byte=(
      //M6502
      //0  1  2  3  4  5  6  7  8  9  a  b  c  d  e  f
        7, 6, 1, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6,
        2, 5, 1, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
        6, 6, 1, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6,
        2, 5, 1, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
        6, 6, 1, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6,
        2, 5, 1, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
        6, 6, 1, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6,
        2, 5, 1, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
        2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
        2, 6, 1, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5,
        2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
        2, 5, 1, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4,
        2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
        2, 5, 1, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
        2, 6, 2, 7, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
        2, 5, 1, 7, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7);


{ TMOS6502 the CPU code is from
 MOS6502 v1.0 - a MOS 6502 CPU emulator
  for Delphi 10.1 Berlin+ by Dennis Spreen
  http://blog.spreendigital.de/2017/03/09/mos6502-delphi/
  MIT License
  Copyright (c) 2017 Gianluca Ghettini (C++ implementation)
  Copyright (c) 2017 Dennis D. Spreen <dennis@spreendigital.de> (Delphi implementation) }
constructor TMOS6502.Create(R: TBusRead; W: TBusWrite);
var
  Instr: TInstr;
  I: Integer;
begin
  Write := W;
  Read := R;
  // fill jump table with ILLEGALs
  Instr.Addr := Addr_IMP;
  Instr.code := Op_ILLEGAL;
  for I := 0 to 256 - 1 do
    InstrTable[I] := Instr;
  // insert opcodes
  Instr.Addr := Addr_IMM;
  Instr.Code := Op_ADC;
  InstrTable[$69] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_ADC;
  InstrTable[$6D] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_ADC;
  InstrTable[$65] := Instr;
  Instr.Addr := Addr_INX;
  Instr.Code := Op_ADC;
  InstrTable[$61] := Instr;
  Instr.Addr := Addr_INY;
  Instr.Code := Op_ADC;
  InstrTable[$71] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_ADC;
  InstrTable[$75] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_ADC;
  InstrTable[$7D] := Instr;
  Instr.Addr := Addr_ABY;
  Instr.Code := Op_ADC;
  InstrTable[$79] := Instr;
  Instr.Addr := Addr_IMM;
  Instr.Code := Op_AND;
  InstrTable[$29] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_AND;
  InstrTable[$2D] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_AND;
  InstrTable[$25] := Instr;
  Instr.Addr := Addr_INX;
  Instr.Code := Op_AND;
  InstrTable[$21] := Instr;
  Instr.Addr := Addr_INY;
  Instr.Code := Op_AND;
  InstrTable[$31] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_AND;
  InstrTable[$35] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_AND;
  InstrTable[$3D] := Instr;
  Instr.Addr := Addr_ABY;
  Instr.Code := Op_AND;
  InstrTable[$39] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_ASL;
  InstrTable[$0E] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_ASL;
  InstrTable[$06] := Instr;
  Instr.Addr := Addr_ACC;
  Instr.Code := Op_ASL_ACC;
  InstrTable[$0A] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_ASL;
  InstrTable[$16] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_ASL;
  InstrTable[$1E] := Instr;
  Instr.Addr := Addr_REL;
  Instr.Code := Op_BCC;
  InstrTable[$90] := Instr;
  Instr.Addr := Addr_REL;
  Instr.Code := Op_BCS;
  InstrTable[$B0] := Instr;
  Instr.Addr := Addr_REL;
  Instr.Code := Op_BEQ;
  InstrTable[$F0] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_BIT;
  InstrTable[$2C] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_BIT;
  InstrTable[$24] := Instr;
  Instr.Addr := Addr_REL;
  Instr.Code := Op_BMI;
  InstrTable[$30] := Instr;
  Instr.Addr := Addr_REL;
  Instr.Code := Op_BNE;
  InstrTable[$D0] := Instr;
  Instr.Addr := Addr_REL;
  Instr.Code := Op_BPL;
  InstrTable[$10] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_BRK;
  InstrTable[$00] := Instr;
  Instr.Addr := Addr_REL;
  Instr.Code := Op_BVC;
  InstrTable[$50] := Instr;
  Instr.Addr := Addr_REL;
  Instr.Code := Op_BVS;
  InstrTable[$70] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_CLC;
  InstrTable[$18] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_CLD;
  InstrTable[$D8] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_CLI;
  InstrTable[$58] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_CLV;
  InstrTable[$B8] := Instr;
  Instr.Addr := Addr_IMM;
  Instr.Code := Op_CMP;
  InstrTable[$C9] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_CMP;
  InstrTable[$CD] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_CMP;
  InstrTable[$C5] := Instr;
  Instr.Addr := Addr_INX;
  Instr.Code := Op_CMP;
  InstrTable[$C1] := Instr;
  Instr.Addr := Addr_INY;
  Instr.Code := Op_CMP;
  InstrTable[$D1] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_CMP;
  InstrTable[$D5] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_CMP;
  InstrTable[$DD] := Instr;
  Instr.Addr := Addr_ABY;
  Instr.Code := Op_CMP;
  InstrTable[$D9] := Instr;
  Instr.Addr := Addr_IMM;
  Instr.Code := Op_CPX;
  InstrTable[$E0] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_CPX;
  InstrTable[$EC] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_CPX;
  InstrTable[$E4] := Instr;
  Instr.Addr := Addr_IMM;
  Instr.Code := Op_CPY;
  InstrTable[$C0] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_CPY;
  InstrTable[$CC] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_CPY;
  InstrTable[$C4] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_DEC;
  InstrTable[$CE] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_DEC;
  InstrTable[$C6] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_DEC;
  InstrTable[$D6] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_DEC;
  InstrTable[$DE] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_DEX;
  InstrTable[$CA] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_DEY;
  InstrTable[$88] := Instr;
  Instr.Addr := Addr_IMM;
  Instr.Code := Op_EOR;
  InstrTable[$49] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_EOR;
  InstrTable[$4D] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_EOR;
  InstrTable[$45] := Instr;
  Instr.Addr := Addr_INX;
  Instr.Code := Op_EOR;
  InstrTable[$41] := Instr;
  Instr.Addr := Addr_INY;
  Instr.Code := Op_EOR;
  InstrTable[$51] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_EOR;
  InstrTable[$55] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_EOR;
  InstrTable[$5D] := Instr;
  Instr.Addr := Addr_ABY;
  Instr.Code := Op_EOR;
  InstrTable[$59] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_INC;
  InstrTable[$EE] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_INC;
  InstrTable[$E6] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_INC;
  InstrTable[$F6] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_INC;
  InstrTable[$FE] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_INX;
  InstrTable[$E8] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_INY;
  InstrTable[$C8] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_JMP;
  InstrTable[$4C] := Instr;
  Instr.Addr := Addr_ABI;
  Instr.Code := Op_JMP;
  InstrTable[$6C] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_JSR;
  InstrTable[$20] := Instr;
  Instr.Addr := Addr_IMM;
  Instr.Code := Op_LDA;
  InstrTable[$A9] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_LDA;
  InstrTable[$AD] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_LDA;
  InstrTable[$A5] := Instr;
  Instr.Addr := Addr_INX;
  Instr.Code := Op_LDA;
  InstrTable[$A1] := Instr;
  Instr.Addr := Addr_INY;
  Instr.Code := Op_LDA;
  InstrTable[$B1] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_LDA;
  InstrTable[$B5] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_LDA;
  InstrTable[$BD] := Instr;
  Instr.Addr := Addr_ABY;
  Instr.Code := Op_LDA;
  InstrTable[$B9] := Instr;
  Instr.Addr := Addr_IMM;
  Instr.Code := Op_LDX;
  InstrTable[$A2] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_LDX;
  InstrTable[$AE] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_LDX;
  InstrTable[$A6] := Instr;
  Instr.Addr := Addr_ABY;
  Instr.Code := Op_LDX;
  InstrTable[$BE] := Instr;
  Instr.Addr := Addr_ZEY;
  Instr.Code := Op_LDX;
  InstrTable[$B6] := Instr;
  Instr.Addr := Addr_IMM;
  Instr.Code := Op_LDY;
  InstrTable[$A0] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_LDY;
  InstrTable[$AC] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_LDY;
  InstrTable[$A4] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_LDY;
  InstrTable[$B4] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_LDY;
  InstrTable[$BC] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_LSR;
  InstrTable[$4E] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_LSR;
  InstrTable[$46] := Instr;
  Instr.Addr := Addr_ACC;
  Instr.Code := Op_LSR_ACC;
  InstrTable[$4A] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_LSR;
  InstrTable[$56] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_LSR;
  InstrTable[$5E] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_NOP;
  InstrTable[$EA] := Instr;
  Instr.Addr := Addr_IMM;
  Instr.Code := Op_ORA;
  InstrTable[$09] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_ORA;
  InstrTable[$0D] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_ORA;
  InstrTable[$05] := Instr;
  Instr.Addr := Addr_INX;
  Instr.Code := Op_ORA;
  InstrTable[$01] := Instr;
  Instr.Addr := Addr_INY;
  Instr.Code := Op_ORA;
  InstrTable[$11] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_ORA;
  InstrTable[$15] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_ORA;
  InstrTable[$1D] := Instr;
  Instr.Addr := Addr_ABY;
  Instr.Code := Op_ORA;
  InstrTable[$19] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_PHA;
  InstrTable[$48] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_PHP;
  InstrTable[$08] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_PLA;
  InstrTable[$68] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_PLP;
  InstrTable[$28] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_ROL;
  InstrTable[$2E] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_ROL;
  InstrTable[$26] := Instr;
  Instr.Addr := Addr_ACC;
  Instr.Code := Op_ROL_ACC;
  InstrTable[$2A] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_ROL;
  InstrTable[$36] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_ROL;
  InstrTable[$3E] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_ROR;
  InstrTable[$6E] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_ROR;
  InstrTable[$66] := Instr;
  Instr.Addr := Addr_ACC;
  Instr.Code := Op_ROR_ACC;
  InstrTable[$6A] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_ROR;
  InstrTable[$76] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_ROR;
  InstrTable[$7E] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_RTI;
  InstrTable[$40] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_RTS;
  InstrTable[$60] := Instr;
  Instr.Addr := Addr_IMM;
  Instr.Code := Op_SBC;
  InstrTable[$E9] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_SBC;
  InstrTable[$ED] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_SBC;
  InstrTable[$E5] := Instr;
  Instr.Addr := Addr_INX;
  Instr.Code := Op_SBC;
  InstrTable[$E1] := Instr;
  Instr.Addr := Addr_INY;
  Instr.Code := Op_SBC;
  InstrTable[$F1] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_SBC;
  InstrTable[$F5] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_SBC;
  InstrTable[$FD] := Instr;
  Instr.Addr := Addr_ABY;
  Instr.Code := Op_SBC;
  InstrTable[$F9] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_SEC;
  InstrTable[$38] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_SED;
  InstrTable[$F8] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_SEI;
  InstrTable[$78] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_STA;
  InstrTable[$8D] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_STA;
  InstrTable[$85] := Instr;
  Instr.Addr := Addr_INX;
  Instr.Code := Op_STA;
  InstrTable[$81] := Instr;
  Instr.Addr := Addr_INY;
  Instr.Code := Op_STA;
  InstrTable[$91] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_STA;
  InstrTable[$95] := Instr;
  Instr.Addr := Addr_ABX;
  Instr.Code := Op_STA;
  InstrTable[$9D] := Instr;
  Instr.Addr := Addr_ABY;
  Instr.Code := Op_STA;
  InstrTable[$99] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_STX;
  InstrTable[$8E] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_STX;
  InstrTable[$86] := Instr;
  Instr.Addr := Addr_ZEY;
  Instr.Code := Op_STX;
  InstrTable[$96] := Instr;
  Instr.Addr := Addr_ABS;
  Instr.Code := Op_STY;
  InstrTable[$8C] := Instr;
  Instr.Addr := Addr_ZER;
  Instr.Code := Op_STY;
  InstrTable[$84] := Instr;
  Instr.Addr := Addr_ZEX;
  Instr.Code := Op_STY;
  InstrTable[$94] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_TAX;
  InstrTable[$AA] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_TAY;
  InstrTable[$A8] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_TSX;
  InstrTable[$BA] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_TXA;
  InstrTable[$8A] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_TXS;
  InstrTable[$9A] := Instr;
  Instr.Addr := Addr_IMP;
  Instr.Code := Op_TYA;
  InstrTable[$98] := Instr;
end;
procedure TMOS6502.SET_NEGATIVE(const Value: Boolean);
begin
  if Value then
    Status := Status or NEGATIVE
  else
    Status := Status and (not NEGATIVE);
end;
procedure TMOS6502.SET_OVERFLOW(const Value: Boolean);
begin
  if Value then
    Status := Status or OVERFLOW
  else
    Status := Status and (not OVERFLOW);
end;
procedure TMOS6502.SET_CONSTANT(const Value: Boolean);
begin
  if Value then
    Status := Status or CONSTANT
  else
    Status := Status and (not CONSTANT);
end;
procedure TMOS6502.SET_BREAK(const Value: Boolean);
begin
  if Value then
    Status := Status or BREAK
  else
    Status := Status and (not BREAK);
end;
procedure TMOS6502.SET_DECIMAL(const Value: Boolean);
begin
  if Value then
    Status := Status or DECIMAL
  else
    Status := Status and (not DECIMAL);
end;
procedure TMOS6502.SET_INTERRUPT(const Value: Boolean);
begin
  if Value then
    Status := Status or INTERRUPT
  else
    Status := Status and (not INTERRUPT);
end;
procedure TMOS6502.SET_ZERO(const Value: Boolean);
begin
  if Value then
    Status := Status or ZERO
  else
    Status := Status and (not ZERO);
end;
procedure TMOS6502.SET_CARRY(const Value: Boolean);
begin
  if Value then
    Status := Status or CARRY
  else
    Status := Status and (not CARRY);
end;

function TMOS6502.IF_NEGATIVE: Boolean;
begin
  Result := ((Status and NEGATIVE) <> 0);
end;
function TMOS6502.IF_OVERFLOW: Boolean;
begin
  Result := ((Status and OVERFLOW) <> 0);
end;
//function TMOS6502.IF_CONSTANT: Boolean;
//begin
//  Result := ((Status and CONSTANT) <> 0);
//end;
//function TMOS6502.IF_BREAK: Boolean;
//begin
//  Result := ((Status and BREAK) <> 0);
//end;
function TMOS6502.IF_DECIMAL: Boolean;
begin
  Result := ((Status and DECIMAL) <> 0);
end;
function TMOS6502.IF_INTERRUPT: Boolean;
begin
  Result := ((Status and INTERRUPT) <> 0);
end;
function TMOS6502.IF_ZERO: Boolean;
begin
  Result := ((Status and ZERO) <> 0);
end;
function TMOS6502.IF_CARRY: Byte;
begin
  if (Status and CARRY) <> 0 then
    Result := 1
  else
    Result := 0;
end;
function TMOS6502.Addr_ACC: Word;
begin
  Result := 0; // not used
end;
function TMOS6502.Addr_IMM: Word;
begin
  Result := Pc;
  Inc(Pc);
end;
function TMOS6502.Addr_ABS: Word;
var
  AddrL, AddrH, Addr: Word;
begin
  AddrL := Read(Pc);
  Inc(Pc);
  AddrH := Read(Pc);
  Inc(Pc);
  Addr := AddrL + (AddrH shl 8);
  Result := Addr;
end;
function TMOS6502.Addr_ZER: Word;
begin
  Result := Read(Pc);
  Inc(Pc);
end;
function TMOS6502.Addr_IMP: Word;
begin
  Result := 0; // not used
end;
function TMOS6502.Addr_REL: Word;
var
  Offset, Addr: Word;
begin
  Offset := Read(Pc);
  Inc(Pc);
  if (Offset and $80) <> 0 then
    Offset := Offset or $FF00;
  Addr := Pc + Offset;
  Result := Addr;
end;
function TMOS6502.Addr_ABI: Word;
var
  AddrL, AddrH, EffL, EffH, Abs, Addr: Word;
begin
  AddrL := Read(Pc);
  Inc(Pc);
  AddrH := Read(Pc);
  Inc(Pc);
  Abs := (AddrH shl 8) or AddrL;
  EffL := Read(Abs);
  EffH := Read((Abs and $FF00) + ((Abs + 1) and $00FF));
  Addr := EffL + $100 * EffH;
  Result := Addr;
end;
function TMOS6502.Addr_ZEX: Word;
var
  Addr: Word;
begin
  Addr := (Read(Pc) + X) mod 256;
  Inc(Pc);
  Result := Addr;
end;
function TMOS6502.Addr_ZEY: Word;
var
  Addr: Word;
begin
  Addr := (Read(Pc) + Y) mod 256;
  Inc(Pc);
  Result := Addr;
end;
function TMOS6502.Addr_ABX: Word;
var
  AddrL: Word;
  AddrH: Word;
  Addr: Word;
begin
  AddrL := Read(Pc);
  Inc(Pc);
  AddrH := Read(Pc);
  Inc(Pc);
  Addr := AddrL + (AddrH shl 8) + X;
  Result := Addr;
end;
function TMOS6502.Addr_ABY: Word;
var
  AddrL: Word;
  AddrH: Word;
  Addr: Word;
begin
  AddrL := Read(Pc);
  Inc(Pc);
  AddrH := Read(Pc);
  Inc(Pc);
  Addr := AddrL + (AddrH shl 8) + Y;
  Result := Addr;
end;
function TMOS6502.Addr_INX: Word;
var
  ZeroL, ZeroH: Word;
  Addr: Word;
begin
  ZeroL := (Read(Pc) + X) mod 256;
  Inc(Pc);
  ZeroH := (ZeroL + 1) mod 256;
  Addr := Read(ZeroL) + (Read(ZeroH) shl 8);
  Result := Addr;
end;
function TMOS6502.Addr_INY: Word;
var
  ZeroL, ZeroH: Word;
  Addr: Word;
begin
  ZeroL := Read(Pc);
  Inc(Pc);
  ZeroH := (ZeroL + 1) mod 256;
  Addr := Read(ZeroL) + (Read(ZeroH) shl 8) + Y;
  Result := Addr;
end;
procedure TMOS6502.Reset;
begin
  A := $aa;
  X := $00;
  Y := $00;
  Status := BREAK or INTERRUPT OR ZERO or CONSTANT;
  Sp := $FD;
  Pc := (Read(RSTVECTORH) shl 8) + Read(RSTVECTORL); // load PC from reset vector
  Cycles := 6; // according to the datasheet, the reset routine takes 6 clock cycles
  IllegalOpcode := false;
end;
procedure TMOS6502.StackPush(const Value: Byte);
begin
  Write($0100 + Sp, Value);
  if Sp = $00 then
    Sp := $FF
  else
    Dec(Sp);
end;
function TMOS6502.StackPop: Byte;
begin
  if Sp = $FF then
    Sp := $00
  else
    Inc(Sp);
  Result := Read($0100 + Sp);
end;
procedure TMOS6502.IRQ;
begin
  if (not IF_INTERRUPT) then
  begin
    SET_BREAK(False);
    StackPush((Pc shr 8) and $FF);
    StackPush(Pc and $FF);
    StackPush(Status);
    SET_INTERRUPT(True);
    Pc := (Read(IRQVECTORH) shl 8) + Read(IRQVECTORL);
  end;
end;
procedure TMOS6502.NMI;
begin
  SET_BREAK(false);
  StackPush((Pc shr 8) and $FF);
  StackPush(Pc and $FF);
  StackPush(Status);
  SET_INTERRUPT(True);
  Pc := (Read(NMIVECTORH) shl 8) + Read(NMIVECTORL);
end;
function TMOS6502.Step:longword;
var
  Opcode: Byte;
  Instr: PInstr;
  Src: Word;
begin
  // fetch
  Opcode := Read(Pc);
  Result := Opc_Cycles[Opcode];
  Inc(Pc);
  // decode and execute
  Instr := @InstrTable[Opcode];
  Src := Instr.Addr;
  Instr.Code(Src);
  Inc(Cycles);
end;
procedure TMOS6502.Op_ILLEGAL(Src: Word);
begin
  IllegalOpcode := true;
end;
procedure TMOS6502.Op_AND(Src: Word);
var
  M: Byte;
  Res: Byte;
begin
  M := Read(Src);
  Res := M and A;
  SET_NEGATIVE((Res and $80) <> 0);
  SET_ZERO(Res = 0);
  A := Res;
end;
procedure TMOS6502.Op_ASL(Src: Word);
var
  M: Byte;
begin
  M := Read(Src);
  SET_CARRY((M and $80) <> 0);
  M := M shl 1;
  M := M and $FF;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  Write(Src, M);
end;
procedure TMOS6502.Op_ASL_ACC(Src: Word);
var
  M: Byte;
begin
  M := A;
  SET_CARRY((M and $80) <> 0);
  M := M shl 1;
  M := M and $FF;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  A := M;
end;
procedure TMOS6502.Op_BCC(Src: Word);
begin
  if IF_CARRY = 0 then
    Pc := Src;
end;
procedure TMOS6502.Op_BCS(Src: Word);
begin
  if IF_CARRY = 1 then
    Pc := Src;
end;
procedure TMOS6502.Op_BEQ(Src: Word);
begin
  if IF_ZERO then
    Pc := Src;
end;
procedure TMOS6502.Op_BIT(Src: Word);
var
  M: Byte;
  Res: Byte;
begin
  M := Read(Src);
  Res := M and A;
  SET_NEGATIVE((Res and $80) <> 0);
  Status := (Status and $3F) or (M and $C0);
  SET_ZERO(Res = 0);
end;
procedure TMOS6502.Op_BMI(Src: Word);
begin
  if IF_NEGATIVE then
    Pc := Src;
end;
procedure TMOS6502.Op_BNE(Src: Word);
begin
  if not (IF_ZERO) then
    Pc := Src;
end;
procedure TMOS6502.Op_BPL(Src: Word);
begin
  if not (IF_NEGATIVE) then
    Pc := Src;
end;
procedure TMOS6502.Op_BRK(Src: Word);
begin
  Inc(Pc);
  StackPush((Pc shr 8) and $FF);
  StackPush(Pc and $FF);
  StackPush(Status or BREAK);
  SET_INTERRUPT(True);
  Pc := (Read(IRQVECTORH) shl 8) + Read(IRQVECTORL);
end;
procedure TMOS6502.Op_BVC(Src: Word);
begin
  if not (IF_OVERFLOW) then
    Pc := Src;
end;
procedure TMOS6502.Op_BVS(Src: Word);
begin
  if IF_OVERFLOW then
    Pc := Src;
end;
procedure TMOS6502.Op_CLC(Src: Word);
begin
  SET_CARRY(False);
end;
procedure TMOS6502.Op_CLD(Src: Word);
begin
  SET_DECIMAL(False);
end;
procedure TMOS6502.Op_CLI(Src: Word);
begin
  SET_INTERRUPT(False);
end;
procedure TMOS6502.Op_CLV(Src: Word);
begin
  SET_OVERFLOW(False);
end;
procedure TMOS6502.Op_CMP(Src: Word);
var
  Tmp: Cardinal;
begin
  Tmp := A - Read(Src);
  SET_CARRY(Tmp < $100);
  SET_NEGATIVE((Tmp and $80) <> 0);
  SET_ZERO((Tmp and $FF)=0);
end;
procedure TMOS6502.Op_CPX(Src: Word);
var
  Tmp: Cardinal;
begin
  Tmp := X - Read(Src);
  SET_CARRY(Tmp < $100);
  SET_NEGATIVE((Tmp and $80) <> 0);
  SET_ZERO((Tmp and $FF)=0);
end;
procedure TMOS6502.Op_CPY(Src: Word);
var
  Tmp: Cardinal;
begin
  Tmp := Y - Read(Src);
  SET_CARRY(Tmp < $100);
  SET_NEGATIVE((Tmp and $80) <> 0);
  SET_ZERO((Tmp and $FF)=0);
end;
procedure TMOS6502.Op_DEC(Src: Word);
var
  M: Byte;
begin
  M := Read(Src);
  M := M - 1;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  Write(Src, M);
end;
procedure TMOS6502.Op_DEX(Src: Word);
var
  M: Byte;
begin
  M := X;
  M := M - 1;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  X := M;
end;
procedure TMOS6502.Op_DEY(Src: Word);
var
  M: Byte;
begin
  M := Y;
  M := M - 1;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  Y := M;
end;
procedure TMOS6502.Op_EOR(Src: Word);
var
  M: Byte;
begin
  M := Read(Src);
  M := A xor M;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  A := M;
end;
procedure TMOS6502.Op_INC(Src: Word);
var
  M: Byte;
begin
  M := Read(Src);
  M := M + 1;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  Write(Src, M);
end;
procedure TMOS6502.Op_INX(Src: Word);
var
  M: Byte;
begin
  M := X;
  M := M + 1;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  X := M;
end;
procedure TMOS6502.Op_INY(Src: Word);
var
  M: Byte;
begin
  M := Y;
  M := M + 1;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  Y := M;
end;
procedure TMOS6502.Op_JMP(Src: Word);
begin
  Pc := Src;
end;
procedure TMOS6502.Op_JSR(Src: Word);
begin
  Dec(Pc);
  StackPush((Pc shr 8) and $FF);
  StackPush(Pc and $FF);
  Pc := Src;
end;
procedure TMOS6502.Op_LDA(Src: Word);
var
  M: Byte;
begin
  M := Read(Src);
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  A := M;
end;
procedure TMOS6502.Op_LDX(Src: Word);
var
  M: Byte;
begin
  M := Read(Src);
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  X := M;
end;
procedure TMOS6502.Op_LDY(Src: Word);
var
  M: Byte;
begin
  M := Read(Src);
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  Y := M;
end;
procedure TMOS6502.Op_LSR(Src: Word);
var
  M: Byte;
begin
  M := Read(Src);
  SET_CARRY((M and $01) <> 0);
  M := M shr 1;
  SET_NEGATIVE(False);
  SET_ZERO(M = 0);
  Write(Src, M);
end;
procedure TMOS6502.Op_LSR_ACC(Src: Word);
var
  M: Byte;
begin
  M := A;
  SET_CARRY((M and $01) <> 0);
  M := M shr 1;
  SET_NEGATIVE(False);
  SET_ZERO(M = 0);
  A := M;
end;
procedure TMOS6502.Op_NOP(Src: Word);
begin
  // no operation
end;
procedure TMOS6502.Op_ORA(Src: Word);
var
  M: Byte;
begin
  M := Read(Src);
  M := A or M;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  A := M;
end;
procedure TMOS6502.Op_PHA(Src: Word);
begin
  StackPush(A);
end;
procedure TMOS6502.Op_PHP(Src: Word);
begin
  StackPush(Status or BREAK);
end;
procedure TMOS6502.Op_PLA(Src: Word);
begin
  A := StackPop;
  SET_NEGATIVE((A and $80) <> 0);
  SET_ZERO(A = 0);
end;
procedure TMOS6502.Op_PLP(Src: Word);
begin
  Status := StackPop;
  SET_CONSTANT(True);
end;
procedure TMOS6502.Op_ROL(Src: Word);
var
  M: Word;
begin
  M := Read(Src);
  M := M shl 1;
  if IF_CARRY = 1 then
    M := M or $01;
  SET_CARRY(M > $FF);
  M := M and $FF;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  Write(Src, M);
end;
procedure TMOS6502.Op_ROL_ACC(Src: Word);
var
  M: Word;
begin
  M := A;
  M := M shl 1;
  if IF_CARRY = 1 then
    M := M or $01;
  SET_CARRY(M > $FF);
  M := M and $FF;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  A := M;
end;
procedure TMOS6502.Op_ROR(Src: Word);
var
  M: Word;
begin
  M := Read(Src);
  if IF_CARRY = 1 then
    M := M or $100;
  SET_CARRY((M and $01) <> 0);
  M := M shr 1;
  M := M and $FF;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  Write(Src, M);
end;
procedure TMOS6502.Op_ROR_ACC(Src: Word);
var
  M: Word;
begin
  M := A;
  if IF_CARRY = 1 then
    M := M or $100;
    SET_CARRY((M and $01) <> 0);
  M := M shr 1;
  M := M and $FF;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  A := M;
end;
procedure TMOS6502.Op_RTI(Src: Word);
var
  Lo, Hi: Byte;
begin
  Status := StackPop;
  Lo := StackPop;
  Hi := StackPop;
  Pc := (Hi shl 8) or Lo;
end;
procedure TMOS6502.Op_RTS(Src: Word);
var
  Lo, Hi: Byte;
begin
  Lo := StackPop;
  Hi := StackPop;
  Pc := (Hi shl 8) or Lo + 1;
end;
procedure TMOS6502.Op_ADC(Src: Word);
var
  M: Byte;
  Tmp: Cardinal;
begin
  M := Read(Src);
  Tmp := M + A + IF_CARRY;
  SET_ZERO((Tmp and $FF)=0);
  if IF_DECIMAL then
  begin
    if (((A and $F) + (M and $F) + IF_CARRY) > 9) then
      Tmp := Tmp + 6;
    SET_NEGATIVE((Tmp and $80) <> 0);
    SET_OVERFLOW( (((A xor M) and $80) = 0) and (((A xor Tmp) and $80) <> 0));
    if Tmp > $99 then
      Tmp := Tmp + $60;
    SET_CARRY(Tmp > $99);
  end
  else
  begin
    SET_NEGATIVE((Tmp and $80) <> 0);
    SET_OVERFLOW( (((A xor M) and $80)=0) and (((A xor Tmp) and $80) <> 0));
    SET_CARRY(Tmp > $FF);
  end;
  A := Tmp and $FF;
end;

procedure TMOS6502.Op_SBC(Src: Word);
var
  M: Byte;
  Tmp: Word;
begin
  M := Read(Src);
  Tmp := A - M - (1-IF_CARRY);
  SET_NEGATIVE((Tmp and $80) <> 0);
  SET_ZERO((Tmp and $FF) = 0);
   SET_OVERFLOW( (((A xor Tmp) and $80) <> 0)  and (((A xor M) and $80) <> 0));
  if IF_DECIMAL then
  begin
    if (((A and $0F) - (1-IF_CARRY)) < (M and $0F)) then
      Tmp := Tmp - 6;
    if Tmp > $99 then
      Tmp := Tmp - $60;
  end;
  SET_CARRY(Tmp < $100);
  A := (Tmp and $FF);
end;
procedure TMOS6502.Op_SEC(Src: Word);
begin
  SET_CARRY(True);
end;
procedure TMOS6502.Op_SED(Src: Word);
begin
  SET_DECIMAL(True);
end;
procedure TMOS6502.Op_SEI(Src: Word);
begin
  SET_INTERRUPT(True);
end;
procedure TMOS6502.Op_STA(Src: Word);
begin
  Write(Src, A);
end;
procedure TMOS6502.Op_STX(Src: Word);
begin
  Write(Src, X);
end;
procedure TMOS6502.Op_STY(Src: Word);
begin
  Write(Src, Y);
end;
procedure TMOS6502.Op_TAX(Src: Word);
var
  M: Byte;
begin
  M := A;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  X := M;
end;
procedure TMOS6502.Op_TAY(Src: Word);
var
  M: Byte;
begin
  M := A;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  Y := M;
end;
procedure TMOS6502.Op_TSX(Src: Word);
var
  M: Byte;
begin
  M := Sp;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  x := M;
end;
procedure TMOS6502.Op_TXA(Src: Word);
var
  M: Byte;
begin
  M := X;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  A := M;
end;
procedure TMOS6502.Op_TXS(Src: Word);
begin
  Sp := X;
end;
procedure TMOS6502.Op_TYA(Src: Word);
var
  M: Byte;
begin
  M := Y;
  SET_NEGATIVE((M and $80) <> 0);
  SET_ZERO(M = 0);
  A := M;
end;

{===============================================================================
  VV   VV  II   CCCCC        22222    00000    E M U L A T O R
  VV   VV  II  CC   CC      22   22  00   00
   VV VV   II  CC       --     2222  00   00           by SDEX32
   VV VV   II  CC   CC      222      00   00   for all who love and use PASCAL
    VVV    II   CCCCC       2222222   00000       my first computer
}

const

vic20_pal_1 :array [0..15,0..2] of byte = ( //vic20
(0,0,0),
(255,255,255),
(182,31,33),
(77,240,255),
(180,63,255),
(68,226,55),
(16,87,247),
(220,215,27),
(202,84,16),
(233,176,114),
(231,146,147),
(154,247,253),
(224,159,255),
(143,228,147),
(130,144,255),
(229,222,133));

vic20_pal_4 :array [0..15,0..2] of byte = ( //vic20    PAL
     (0,0,0),         //  0 - 0000   Black
     ($FF,$FF,$FF),   //  1 - 0001   White
     ($77,$2d,$26),   //  2 - 0010   Red
     ($85,$d4,$dc),   //  3 - 0011   Cyan
     ($a8,$5f,$b4),   //  4 - 0100   Purple
     ($55,$9e,$4a),   //  5 - 0101   Green
     ($42,$34,$8b),   //  6 - 0110   Blue
     ($bd,$cc,$71),   //  7 - 0111   Yellow
     ($b6,$68,$62),   //  8 - 1000   Orange
     ($c5,$ff,$ff),   //  9 - 1001   Light orange
     ($e9,$9d,$f5),   // 10 - 1010   Pink
     ($92,$df,$87),   // 11 - 1011   Light cyan
     ($7e,$70,$ca),   // 12 - 1100   Light purple
     ($ff,$ff,$b0),   // 13 - 1101   Light green
     ($a8,$73,$4a),   // 14 - 1110   Light blue
     ($e9,$b2,$87)    // 15 - 1111   Light yellow
    );




vic20_pal_2 :array [0..15,0..2] of byte = ( //vic20 vice //Light
// VICE palette without dither
{BLACK}           ($00,$00,$00),
{WHITE}           ($ff,$ff,$ff),
{RED}             ($B6,$1F,$21),
{CYAN}            ($4D,$F0,$FF),
{PURPLE}          ($B4,$3F,$FF),
{GREEN}           ($44,$E2,$37),
{BLUE}            ($1A,$34,$FF),
{YELLOW}          ($DC,$D7,$1B),
{ORANGE}          ($CA,$54,$00),
{LIGHT_ORANGE}    ($E9,$B0,$72),
{LIGHT_RED}       ($E7,$92,$93),
{LIGHT_CYAN}      ($9A,$F7,$FD),
{LIGHT_PURPLE}    ($E0,$9F,$FF),
{LIGHT_GREEN}     ($8F,$E4,$93),
{LIGHT_BLUE}      ($82,$90,$FF),
{LIGHT_YELLOW}    ($E5,$DE,$85));


vic20_pal_3 :array [0..15,0..2] of byte = ( //vic20  OT EMULATOR  //Light
{BLACK}           ($00,$00,$00),
{WHITE}           ($ff,$ff,$ff),
{RED}             ($78,$29,$22),
{CYAN}            ($87,$d6,$dd),
{PURPLE}          ($a9,$5f,$b6),
{GREEN}           ($56,$a0,$49),
{BLUE}            ($40,$31,$8e),
{YELLOW}          ($bf,$ce,$73),
{ORANGE}          ($aa,$74,$48),
{ORANGE}          ($ea,$b4,$88),
{PINK}            ($b8,$69,$62),
{LIGHT_CYAN}      ($c7,$ff,$ff),
{LIGHT_PURPLE}    ($e9,$9f,$f6),
{LIGHT_GREEN}     ($94,$e0,$88),
{LIGHT_BLUE}      ($80,$71,$cc),
{LIGHT_YELLOW}    ($fe,$ff,$b3));




{
      if aVic20 then // map chars to ASCII
      begin
         w := (ch and $1F) + 32 * ( longword(ord(ch < 64)) + longword(ord(ch > 95))*2 + longword(ord(ch > 159)));
         if ch < 32 then w := 0;
         if (ch > 127) and (ch < 160)  then w:= 0;
         if ch > 191 then w := 0;
         ch := w;
      end;
}




{ TVIC20 }
//------------------------------------------------------------------------------
function Vic20_VIC_Thread(a:pointer):longint; stdcall;
var o:TVIC20;
    ScrBitmapPtr:pointer;
    x,y:longword;
    bisize : longword;
    cmask : array[0..2] of longint;
    pbitmapinfo : array[0..2048] of byte; // sizeof(BITMAPINFOHEADER)+512  { 512 for 256 di colors word }
    w,t:longword;
begin
   o := TVIC20(a);
   bisize:=sizeof(BITMAPINFOHEADER);
   fillchar(pbitmapinfo, bisize+512, 0);

   with BITMAPINFO((@pbitmapinfo)^) do
   begin {BitmapInfoHeader 16Bit}
      bmiHeader.biSize        := bisize;
      bmiHeader.biWidth       := 640;
      bmiHeader.biHeight      := -480;
      bmiHeader.biPlanes      := 1;
      bmiHeader.biBitCount    := 32; //bpp
      bmiHeader.biCompression := BI_BITFIELDS;

//      cmask[0]:=$FF0000;
//      cmask[1]:=$00FF00;
//      cmask[2]:=$0000FF;

      cmask[0]:=$0000FF;                       {Bit-Positions R G B 24/32Bit }
      cmask[1]:=$00FF00;     // work faster
      cmask[2]:=$FF0000;

      move(cmask,pointer(longword(@pbitmapinfo)+ bisize)^,sizeof(cmask));
   end;

   while (not o.Terminated) do
   begin
      t := GetTickCount;
      ScrBitmapPtr := o.ScrRender;


      if (o.Host_Xlng + o.Host_Ylng) = 0 then
      begin
         SetDIBitsToDevice(o.Host_dc, o.Host_Xpos, o.Host_Ypos,
                     640, 480, 0, 0, 0,
                     480, ScrBitmapPtr, bitmapinfo((@pbitmapinfo)^),
                     DIB_RGB_COLORS);
      end else begin
          x := 640;
          if o.Host_Xlng <> 0 then  x := o.Host_Xlng;
          y := 480;
          if o.Host_Ylng <> 0 then  y := o.Host_Ylng;
          SetStretchBltMode(o.Host_dc, HALFTONE);  // dosnt work //TODO
          StretchDibits(o.Host_dc, o.Host_Xpos, o.Host_Ypos, x, y,
                     0,0,640, 480, ScrBitmapPtr , bitmapinfo((@pbitmapinfo)^),
                     DIB_RGB_COLORS,SRCCOPY);
      end;

      w := GetTickCount - t;
      if w < 42 then
      begin
         sleep(42 - w);   //24 frames per seconds
      end;

   end;
   Result :=0;
end;

//------------------------------------------------------------------------------
function Vic20_run_Thread(a:pointer):longint; stdcall;
var o:TVIC20;
begin
   o := TVIC20(a);
   sleep(20); // to setup external renders and events handlers
   while (not o.Terminated) do
   begin
      if o.aRun then o.CpuStep;
   end;
   Result :=0;
end;

//------------------------------------------------------------------------------
procedure  TVIC20.VIA_Reset(via:longword);
begin
   aVIA_PIB[via] := 0;
   aVIA_PIA[via] := 0;
   aVIA_PB[via] := 0;
   aVIA_PA[via] := 0;
   aVIA_PBD[via] := 0;
   aVIA_PAD[via] := 0;
   aVIA_T1C[via] := $FFFF;
   aVIA_T1L[via] := $FFFF;
   aVIA_T2C[via] := $FFFF;
   aVIA_SR[via] := 0;
   aVIA_ACR[via] := 0;
   aVIA_PCR[via] := 0;
   aVIA_IFR[via] := 0;
   aVIA_IER[via] := 0;

   aVIA_inhibitT1Interrupt[via] := true;
   aVIA_inhibitT2Interrupt[via] := true;
 //   pins_ira = pins_irb  = 0;
    aVIA_ca1[via] := true;
    aVIA_lastca1[via] := true;
//    ca2 = newca2 = true;
//    cb1 = cb2 = false;
    aVIA_acrTimedCountdown[via] := true;
    aVIA_hasInterrupt[via] := false;
    aVIA_hasPreCycled[via] := false;
end;

//------------------------------------------------------------------------------
{ 6522



}

const
IRQ_FLAG_SET   = $80;
IRQ_FLAG_T1C   = $40;
IRQ_FLAG_T2C   = $20;
IRQ_FLAG_CB1   = $10;
IRQ_FLAG_CB2   = $08;
IRQ_FLAG_SR    = $04;
IRQ_FLAG_CA1   = $02;
IRQ_FLAG_CA2   = $01;


procedure  TVIC20.VIA_CycleUp(via:longword);
begin
   // raise interrupts
   if aVIA_T1C[via] < 0  then
   begin
      if not aVIA_inhibitT1Interrupt[via] then
      begin
         aVIA_IFR[via] := aVIA_IFR[via] or IRQ_FLAG_T1C; { $40} // Set interrupt flag
      end;
      aVIA_inhibitT1Interrupt[via] := true;
   end;
   if aVIA_T2C[via] < 0 then
   begin
      if aVIA_acrTimedCountdown[via] and ( not aVIA_inhibitT2Interrupt[via]) then
      begin
         aVIA_IFR[via] := aVIA_IFR[via] or IRQ_FLAG_T2C; { $20} // Set interrupt flag
      end;
      aVIA_inhibitT2Interrupt[via] := true;
   end;
   if (aVIA_PCR[via] {12 $C} and 1 ) <> 0 then
   begin
      if (aVIA_ca1[via] <> aVIA_lastca1[via]) then
      begin
        aVIA_lastca1[via] := aVIA_ca1[via];
//        if aVIA_ca1[via] <> (( aVIA_PCR[via] and 1) <> 0) then  // CA1 control
        if aVIA_ca1[via] then  // CA1 control
        begin
           aVIA_IFR[via] {13 $D}  := aVIA_IFR[via] or IRQ_FLAG_CA1 {2};
        end;
      end;
   end;
   aVIA_hasInterrupt[via] := ( aVIA_IER[via] and aVIA_IFR[via] and $7F) <> 0 ;
end;

//------------------------------------------------------------------------------
procedure  TVIC20.VIA_CycleDown(via:longword; Cls:longint);
begin
   if aVIA_hasPreCycled[via] then
   begin
      aVIA_hasPreCycled[via] := false;
   end else begin

      if AVIA_T1C[via] < 0  then
      begin // continuous interrupt
         if (aVIA_ACR[via] {11} and IRQ_FLAG_T1C  { $40} ) <> 0 then
         begin
            aVIA_T1C[via] := aVIA_T1L[via];
            aVIA_inhibitT1Interrupt[via] := false;
         end else begin // one-shot
            aVIA_T1C[via] := $FFFE;
         end;
      end;
      if aVIA_T2C[via] < 0  then
      begin
         aVIA_T2C[via] := $FFFE;
      end;
      if ((not aVIA_PCR[via] {12}) and 1) <> 0 then
      begin
         if (aVIA_ca1[via] <> aVIA_lastca1[via]) then
         begin
            aVIA_lastca1[via] := aVIA_ca1[via];
            if not aVIA_ca1[via] then
            begin
              aVIA_IFR[via] {13 $D}  := aVIA_IFR[via] or IRQ_FLAG_CA1 {2};
            end;
         end;
      end;

      aVIA_T1C[via] := aVIA_T1C[via] - Cls;
      aVIA_T2C[via] := aVIA_T2C[via] - Cls;
   end;
end;


var dump_a,dump_b:int64;
//------------------------------------------------------------------------------
procedure  TVIC20.CpuStep; // the speed of the computer is 1 MHz
var Cls:longint;
    ticks:int64;
    done:boolean;
begin
   if aSynch then // synchronize to 1mhz cpu speed including outer staff
   begin
      done := false;
      repeat
         QueryPerformanceCounter(ticks);
         if ticks > aTicksStamp then done := true;
         //do something but nothing
         {
         asm
           xor eax, eax
           mov eax, 0
           and eax, 0
           lea eax,[eax + 2]
           xor eax, $ff
         end;
         }
         dump_a := 1;
         dump_b := 2;
         dump_b := dump_b*dump_a+dump_a;
      until done;
   end;

//      Cls := longint(Step); // do cpu step and Get Cycles per opcode

      VIA_CycleUp(1);
      if aVIA_hasInterrupt[1] then
      begin
         NMI;
      end;
      VIA_CycleUp(2);
      if aVIA_hasInterrupt[2] then
      begin
         IRQ;
      end;

      Cls := longint(Step); // do cpu step and Get Cycles per opcode

      VIA_CycleDown(1,Cls + aClk);
      VIA_CycleDown(2,Cls + aClk);

      aClk := 0; //longint(Step); //???????? to do betert cicle

   if aSynch then // synchronize to 1mhz cpu speed
   begin
      QueryPerformanceCounter(aTicksStamp);
      aTicksStamp := aTicksStamp + aClockRes * (Cls + aClk);
   end;

end;



{  MEMORY BANKS                   unexpand    3K          8K
   E000  bank 7  karnel-rom
   C000  bank 6  basic-rom
   A000  bank 5  EXP
                          9800       I/O         I/O         I/O
                          9600    Color Mem   Color Mem      --
                          9400       --          --       Color Mem
                          9000     VIC6560     VIC6560     VIC6560
   8000  bank 4  ******** 8000    char-rom    char-rom    char-rom
   6000  bank 3  EXP
   4000  bank 2  EXP
   2000  bank 1  EXP
                          1E00    Screen mem  Screen Mem   User Mem
                          1200     User Mem    User Mem    User Mem
                          1000     User Mem    User Mem   Screen Mem
                          0400       --        User mem    3k avail    EXP
   0000  bank 0  ******** 0000    Basic Mem   Basic Mem   Basic Mem
               Basic Start ->      4097        1025        4609
basic start peek(43) + peek(44)*256   2B 2C
Basic end   peek(55) + peek(56)*356   37 38
   peek(648) - screen mem start page
   30 = $1E
   16 = $10

}
{ VIC chip 6560  6561
9000-900F    36864-36879   Address of VIC chip registers
9000     36864   bits 0-6 horizontal centering
                           bit 7 sets interlace scan
9001     36865   vertical centering
9002     36866   bits 0-6 set # of columns
                           bit 7 is part of video matrix address
9003     36867   bits 1-6 set # of rows
                           bit 0 sets 8x8 or 16x8 chars
9004     36868   TV raster beam line
9005     36869   bits 0-3 start of character memory
                           (default = 0)
                           bits 4-7 is rest of video address
                           (default= F)
                           BITS 3,2,1,0 CM startinq address
                                        HEX   DEC
                           0000   ROM   8000  32768
                           0001         8400  33792
                           0010         8800  34816
                           0011         8C00  35840
                           1000   RAM   0000  0000
                           1001 xxxx
                           1010 xxxx  unavail.
                           1011 xxxx
                           1100         1000  4096
                           1101         1400  5120
                           1110         1800  6144
                           1111         1C00  7168
9006     36870    horizontal position of light pen
9007     36871    vertical position of light pen
9008     36872    Digitized value of paddle X
9009     36873    Digitized value of paddle Y
900A     36874    Frequency for oscillator 1 (low)
                  (on: 128-255)
900B     36875    Frequency for oscillator 2 (medium)
                  (on: 128-255)
900C     36876    Frequency for oscillator 3 (high)
                  (on: 128-255)
900D     36877    Frequency of noise source
900E     36878    bit 0-3 sets volume of all sound
                  bits 4-7 are auxiliary color information
900F     36879    Screen and border color register
                   bits 4-7 select background color
                   bits 0-2 select border color
                   bit 3 selects inverted or normal mode
9110-911F  37136-37151 6522 VIA#1  ----------------------------------
   9110    37136     Port B output register
                     (user port and RS-232 lines)
           PIN    6522 DESCRIPTION         EIA   ABV
           ID     ID
           C      PB0 Received data       (BB)  Sin
           D      PB1 Request to Send     (CA)  RTS
           E      PB2 Data terminal ready (CD)  DTR
           F      PB3 Ring indicator      (CE)  RI
           H      PB4 Received line signal (CF)  DCD
           J      PB5 Unassigned          ( )   XXX
           K      PB6 Clear to send       (CB)  CTS
           L      PB7 Data set ready      (CC)  DSR
           B      CB1 Interrupt for Sin   (BB)  Sin
           M      CB2 Transmitted data    (BA)  Sout
           A      GND Protective ground   (M)   GND
           N      GND Signal ground       (AB)  GND
  9111     37137     Port A output register
                   (PA0) Bit 0=Serial CLK IN
                   (PA1) Bit 1=Serial DATA IN
                   (PA2) Bit 2=Joy 0
                   (PA3) Bit 3=Joy 1
                   (PA4) Bit 4=Joy 2
                   (PA5) Bit 5 = Lightpen/Fire button
                   (PA6) Bit 6=Cassette switch sense
                   (PA7) Bit 7=Serial ATN out
9112       37138  Data direction register B
9113       37139  Data direction register A
9114       37140  Timer 1 low byte
9115       37141  Timer 1 high byte & counter
9116       37142  Timer 1 low byte
9117       37143  Timer 1 high byte
9118       37144  Timer 2 low byte
9119       37145  Timer 2 high byte
911A       37146  Shift register
911B       37147  Auxiliary control register
911C       37148  Peripheral control register
                  (CA1, CA2, CB1, CB2)
                  CA1 = restore key (Bit 0)
                  CA2 = cassette motor control (Bits 1-3)
                  CB1 = interrupt signal for received
                      RS-232 data (Bit 4)
                  CB2=transmitted RS-232 data (Bits
                      5-7)
  911D     37149  Interrupt flag register
                        176

    HEX      DECIMAL        DESCRIPTION
   911E      37150            Interrupt enable register
   911F      37151            Port A (Sense cassette switch)

 9120-912F   37152-37167        6522 VIA#2  -------------------
   9120      37152            Port B output register
                                keyboard column scan
                                (PB3) Bit 3 =cassette write line
                                (PB7) Bit 7 =Joy 3
   9121      37153            Port A output register
                                keyboard row scan
   9122      37154            Data direction register B
   9123      37155            Data direction register A
   9124      37156            Timer 1, low byte latch
   9125      37157            Timer 1, high byte latch
   9126      37158            Timer 1, low byte counter
   9127      37159            Timer 1, high byte counter
                                timer 1 is used for the
                                60 time/second interrupt
   9128      37160            Timer 2, low byte latch
   9129      37161            Timer 2, high byte latch
   912A      37162            Shift register
   912B      37163            Auxiliary control register
   912C      37164            Peripheral control register
                                CA1 Cassette read line (Bit 0)
                                CA2 Serial clock out (Bits 1-3)
                                CB1 Serial SRQ IN (Bit 4)
                                CB2 Serial data out (Bits 5-7)
   912D      37165            Interrupt flag register
   912E      37166            Interrupt enable register
   912F      37167            Port A output register

}


//------------------------------------------------------------------------------
function TVIC20.BusRead(Adr: Word): Byte;
var m,i,column:longword;
    via:longword;
begin
   Result := Memory[Adr];


   if ((Adr >= $9110) and (Adr <= $911F)) or ((Adr >= $9120) and (Adr <= $912F)) then //VIA 6522
   begin
      via := (Adr shr 4) and $F; //1 or 2
      // diferrence in via1 via2 special code
      case via of
         1: begin  //VIA1 specific
            aVIA_PIB[via] := 1;
            aVIA_PIA[via] := $FF;
            { aJoy $1-Fire $10-Left $20-Right $40-Up $80-Down }

            if (aJoy and $1 {Fire}) <>0 then  aVIA_PIA[via] := aVIA_PIA[via] or 32 else aVIA_PIA[via] := aVIA_PIA[via] and (32 xor $FF);
            if (aJoy and $10{Left}) <>0 then  aVIA_PIA[via] := aVIA_PIA[via] or 16 else aVIA_PIA[via] := aVIA_PIA[via] and (16 xor $FF);
            if (aJoy and $40{Up}  ) <>0 then  aVIA_PIA[via] := aVIA_PIA[via] or 4  else aVIA_PIA[via] := aVIA_PIA[via] and (4 xor $FF);
            if (aJoy and $80{Down}) <>0 then  aVIA_PIA[via] := aVIA_PIA[via] or 8  else aVIA_PIA[via] := aVIA_PIA[via] and (8 xor $FF);
         end;
         2: begin  //VIA2 specific
             m := Adr and $F;
             if (m=15) or (m < 2) then  // only 0,1,15
             begin
                aVIA_PIB[via] := 0;
                aVIA_PIA[via] := 0;

                column := ((aVIA_PB[via] and aVIA_PBD[via]) xor $FF) and $FF;
                //row    := ((aVIA_PA[via] and aVIA_PAD[via]) xor $FF) and $FF;
                for m := 0 to 7 do
                begin
                   if (column and (1 shl m)) <> 0 then
                   begin
                      //  KeyMatrix[i,m {[Row,Col]}]
                      //prepare row $9120 37152 poer B
                      aVIA_PIB[via] := 0; ; //TODO ?????????
                      //prepare row $9121 37153 poer A
                      for i := 0 to 7 do aVIA_PIA[via] := aVIA_PIA[via] or (KeyMatrix[i,m {[Row,Col]}] shl i);
                   end;

                end;

                //add joystick
                if (aJoy and $20{RIGHT}) <> 0 then aVIA_PIB[via] := aVIA_PIB[via] or $80;
             end;
         end;
      end;


      case Adr and $F of  //common part
         0: begin //R//$9110 - 37136 //$9120 - 37152  {Port B}
               Result := ((aVIA_PB[via] and aVIA_PBD[via])
                      or  ((not aVIA_PIB[via]) and (not aVIA_PBD[via]))) and $FF;
            end;
         1: begin //R /$9111 - 37137 /$9121 - 37153
               // Clear ca1,ca2 interrupt flags, only if not "independent"
               m := (aVIA_PCR[via] shr 1) and 7;
               if ((m <> 1) and ( m <> 3)) then  m := (not longword($3)) else m := (not longword($1));
               aVIA_IFR[via] := aVIA_IFR[via] and m;
               Result := ((not aVIA_PIA[via]) and (not aVIA_PAD[via])) and $FF
             end;
         2: begin //R /$9112 - 37138 /$9122 - 37154  {Data direction register B}
               Result := aVIA_PBD[via];
            end;
         3: begin //R /$9113 - 37139 /$9123 - 37155  {Data direction register A}
               Result := aVIA_PAD[via];
            end;
         4: begin //R /$9114 - 37140 /$9124 - 37156   {Timer 1, low byte}
               Result := aVIA_T1C[via] and $FF;
               // clear Timer-1 IRQ flag
               aVIA_IFR[via] := aVIA_IFR[via] and (not IRQ_FLAG_T1C{ $40}); // via->IFR &= ~IRQ_FLAG_T1C;
               aVIA_inhibitT1Interrupt[via] := false;

               //??// Clear interrupt signal if Timer1 IRQ flag was last interrupt source
               //??//if (aVIA_IFR[via] and (not IRQ_FLAG_SET)) = 0 then aVIA_IFR[via] := 0;   // if(!(via->IFR & ~IRQ_FLAG_SET)) via->IFR = 0;
            end;
         5: begin //R /$9115 - 37141 /$9125 - 37157   {Timer 1, hi byte}
               Result := (aVIA_T1C[via] shr 8) and $FF;
            end;
         6: begin //R /$9116 - 37142 /$9126 - 37158  {Timer1 Lo Latch}
               Result := aVIA_T1L[via] and $FF;
            end;
         7: begin //R /$9117 - 37143 /$9127 - 37159  {Timer1 Hi latch}
               Result := (aVIA_T1L[via] shr 8) and $FF;
            end;
         8: begin //R /$9118 - 37144 /$9128 - 37160  {Timer 2 Lo}
               Result := aVIA_T2C[via] and $FF;
               // clear Timer-2 IRQ flag
               aVIA_IFR[via] := aVIA_IFR[via] and (not IRQ_FLAG_T2C);
               aVIA_inhibitT2Interrupt[via] := false;
               //??// Clear interrupt signal if Timer2 IRQ flag was last interrupt source
               //??//if (aVIA_IFR[via] and (not IRQ_FLAG_SET)) = 0 then aVIA_IFR[via] := 0;   // if(!(via->IFR & ~IRQ_FLAG_SET)) via->IFR = 0;
            end;
         9: begin //R /$9119 - 37145 /$9129 - 37161  {Timer 2 Hi}
               Result := (aVIA_T2C[via] shr 8) and $FF;
            end;
        $A: begin //R /$911A - 37146 /$912A - 37162
               Result := aVIA_SR[via]; {shift register}
            end;
        $B: begin //R /$911B - 37147 /$912B - 37163
               Result := aVIA_ACR[via]; {Auxilary Control Register}
            end;
        $C: begin //R /$911C - 37148 /$912C - 37164
               Result := aVIA_PCR[via]; {Peripheral Control Register}
            end;
        $D: begin //R /$911D - 37149 /$912D - 37165
               Result := aVIA_IFR[via] and $7F; {Interrupt Flag Register}
               // If any flag set top bit must be set
               if Result <> 0 then Result := Result or $80;
            end;
        $E: begin //R /$911E - 37150 /$912E - 37166
               // bit 7 IER is always read as a 1
               Result := aVIA_IER[via] or $80; {Interrupt Enable Register}
            end;
        $F: begin //R /$911F - 37151 /$912F - 37167
               Result := ((not aVIA_PIA[via]) and (not aVIA_PAD[via])) and $FF
            end;
      end;
   end;


end;

const Char_Table_Map:array[0..15] of longword = (
       $8000, $8400, $8800, $8C00, $9000, $9400, $9800, $9C00,
       $0000, $0400, $0800, $0C00, $1000, $1400, $1800, $1C00 );

//------------------------------------------------------------------------------
procedure TVIC20.BusWrite(Adr: Word; Value: Byte);
var m,via:longword;
begin
   //VIC chip 6561  VIA1 VIA2 Color map  I/O area
   if (Adr >= $9000) and (Adr <= $9FFF) then //vic chip plus color plus IO 6522
   begin

      if (Adr >= $9000) and (Adr <= $900F) then //VIC video chip
      begin
         case Adr and $F of
            0: begin //W /$9000 - 36864
                  // $80 - bit interlease
                  // Bit 0-6 Left margin
                  aLeftMargin := (Value and $7F) * 3 + 20;  //LeftMargin
               end;
            1: begin //W /$9001 - 36865
                  aTopMargin := (Value and $FF) * 2 - 20;  //TopMargin
                  if longint(aTopMargin) < 0 then aTopMargin := 0;
               end;
            2: begin //W /$9002 - 36866
                  //Bit 7 is A9 of TxtRam
//32                  aTxtRam := pointer(longword(@Memory[0]) + (Memory[$9005] and $70) * 64 + (Value and $80)*4); // $80 - A09 of TxtRam
                  aTxtRam := @Memory[(Memory[$9005] and $70) * 64 + (Value and $80)*4]; // $80 - A09 of TxtRam
                  aColRam := @Memory[$9400 + (word(Value and $80) shl 2)]; //+512 $200
                  //Bit 0-6 Columns count
                  aTxtCols := Value and $7F;
                  if aTxtCols > 27 then aTxtCols := 26;
                  aTxtXlng := aTxtCols * 8 * 3;
                  //Auto adjust
                  aLeftMarginAdj := - ((aTxtCols - 22) * 8 * 3 ) div 2;
               end;
            3: begin //W /$9003 - 36867
                  //Bit 1-6 Rows count
                  //Bit 0   16x8 char gen
                  //Bit 7   raster counter Bit 0
                  a16x8char := Value and 1;    //bit 0 16x8 chars
                  aTxtRows := (Value and $7F) shr 1;
                  if aTxtRows > 32 then aTxtRows := 31;
                  aTxtYlng := aTxtRows * (8 + 8*a16x8char) * 2;
                  if aTxtRows > 23 then aTopMarginAdj := - ((longint(aTxtRows) - 23) * (8 + 8*longint(a16x8char)) * 2 ) div 2
                                   else aTopMarginAdj := - ((23 - longint(aTxtRows)) * (8 + 8*longint(a16x8char)) * 2 ) div 2;
               end;
            4: begin //W /$9004 - 36868
                  //Raster Count Bit 1-8       ( in render ) for read
               end;
            5: begin //W /$9005 - 36869
//32                  aFntRam := pointer(longword(@Memory[0]) + Char_Table_map[Value and $F]); //0000XXXX = Char table map
                  aFntRam := @Memory[Char_Table_map[Value and $F]]; //0000XXXX = Char table map
//32                  aTxtRam := pointer(longword(@Memory[0]) + (Value and $70) * 64 + (Memory[$9002] and $80)*4); // A13-A10 XXXX0000
                  aTxtRam := @Memory[(Value and $70) * 64 + (Memory[$9002] and $80)*4]; // A13-A10 XXXX0000
               end;
            6: begin //W /$9006 - 36870
                  //Light Pen
               end;
            7: begin //W /$9007 - 36871
                  //Light Pen
               end;
            8: begin //W /$9008 - 36872
                  //potentiometer 1
               end;
            9: begin //W /$9009 - 36873
                  //potentiometer 2
               end;
           $A: begin //W /$900A - 36874 Speaker-1 the alto voice
                  //Bit 7 is the on/off control bit, whilst bits 0-6 select the actual note. Speaker 1 has an alto voice.
                  //  sp-1 Clock  4329     Freq = Clock /(127 - N)   N = 130..255
                  //  sp-2 Clock  8659
                  //  sp-3 Clock 17320
                  //  sp-4 Cloxk 34640

                  //48000 samples  1 up 1 down = 24000Hz max frec with step 1
                  //dx = half interval
                  aSp1_ofs :=0;
                  aSp1_dx := trunc(24000 / (4329 / ($80 - (Value and $7F))));
//                  aSp1_state := 1;
               end;
           $B: begin //W /$900B - 36875 Speaker-2 the tenor voice
                  aSp2_ofs :=0;
                  aSp2_dx := trunc(24000 / (8659 / ($80 - (Value and $7F))));
//                  aSp2_state := 1;
               end;
           $C: begin //W /$900C - 36876 Speaker-3 the soprano voice
                  aSp3_ofs :=0;
                  aSp3_dx := trunc(24000 / (17320 / ($80 - (Value and $7F))));
//                  aSp3_state := 1;
               end;
           $D: begin //W /$900D - 36877 speaker-4 the noise voice
                  aSp4_ofs :=0;
                  aSp4_dx := trunc(24000 / (34640 / ($80 - (Value and $7F))));
//                  aSp4_state := 1;
//                  aSp4_noise := 0;
               end;
           $E: begin //W /$900E - 36878
                  //Bits 0-3 volume of four speakers.
                  if (Value and $F) = 0 then aVolume := 0
                                         else aVolume := (Value and $F) / $F;
                  //Bits 4-7 hold multicolour
                  aAuxColor := (Value shr 4) and $F;
               end;
           $F: begin //W /$900F  36879
                  aFrmColor := Value and $7;   // Frame color 0-7
                  aBgrColor := (Value shr 4) and $F; //Background color 0-15
                  aInvertMode := (Value shr 3) and 1;
               end;
         end;
      end;


      if ((Adr >= $9110) and (Adr <= $911F)) or ((Adr >= $9120) and (Adr <= $912F)) then //VIA 6522
      begin

         via := (Adr shr 4) and $F; //1 or 2 VIA

         VIA_CycleDown(via,1);  //when touch do cycle down
         aVIA_HasPreCycled[via] := true;

         case Adr and $F of
            0: begin //W /$9110 - 37136 /$9120 - 37152  Port B
                  aVIA_PB[via] := Value;
               end;
            1: begin //W /$9111 - 37137 /$9121 - 37153 Port A
                  aVIA_PA[via] := value;
                  m := (aVIA_PCR[via] shr 1) and 7;
                  if ((m <> 1) and ( m <> 3)) then  m := (not longword($3)) else m := (not longword($1));
                  aVIA_IFR[via] := aVIA_IFR[via] and m;
               end;
            2: begin //W /$9112 - 37138 /$9122 - 37154 {Data direction register B}
                  // bitmask If Bit X = 0 read from port   else 1 = write to port
                  aVIA_PBD[via] := Value;
               end;
            3: begin //W /$9113 - 37139 /$9123 - 37155 {Data direction register A}
                  aVIA_PAD[via] := Value;
               end;
            4: begin //W /$9114 - 37140 /$9124 - 37156   {Timer 1, low byte}
                  longword(aVIA_T1L[via]) := (longword(aVIA_T1L[via]) and $FF00) or Value;  //set
               end;
            5: begin //W /$9115 - 37141 /$9125 - 37157   {Timer 1, hi byte}
                  longword(aVIA_T1L[via]) := (longword(aVIA_T1L[via]) and $FF) or (longword(Value) shl 8) ;
                  aVIA_T1C[via] := aVIA_T1L[via];
                  aVIA_IFR[via] := aVIA_IFR[via] and (not IRQ_FLAG_T1C{ $40}); // flag not to be reset until next cycle?
                  aVIA_inhibitT1Interrupt[via] := false;
               end;
            6: begin //W /$9116 - 37142 /$9126 - 37158 {Timer 1 lo latch}
                  longword(aVIA_T1L[via]) := (longword(aVIA_T1L[via]) and $FF00) or Value;
               end;
            7: begin //W /$9117 - 37143 /$9127 - 37159 {Timer 1 hi latch}
                  longword(aVIA_T1L[via]) := (longword(aVIA_T1L[via]) and $FF) or (longword(Value) shl 8) ;
                  aVIA_IFR[via] := aVIA_IFR[via] and (not IRQ_FLAG_T1C{ $40}); // flag not to be reset until next cycle?
               end;
            8: begin //W /$9118 - 37144 /$9128 - 37160 {Timer 2 lo}
                  longword(aVIA_T2C[via]) := (longword(aVIA_T2C[via]) and $FF00) or Value;
               end;
            9: begin //W /$9119 - 37145 /$9129 - 37161 {Timer 2 hi}
                  longword(aVIA_T2C[via]) := (longword(aVIA_T2C[via]) and $FF) or (longword(Value) shl 8) ;
                  aVIA_IFR[via] := aVIA_IFR[via] and (not IRQ_FLAG_T2C{ $20}); // flag not to be reset until next cycle?
                  aVIA_inhibitT2Interrupt[via] := false;
               end;
           $A: begin //W /$911A - 37146 /$912A - 37162 {Shift register}
                  aVIA_SR[via] := Value;
               end;
           $B: begin //W /$911B - 37147 /$912B - 37163 {Auxilary Control Register}
                  aVIA_ACR[via] := Value;
                  aVIA_acrTimedCountdown[via] := (aVIA_ACR[via] and $20) = 0;
               end;
           $C: begin //W /$911C - 37148 /$912C - 37164 {Peripheral Control Register}
                  aVIA_PCR[via] := Value;
               end;
           $D: begin //W /$911D - 37149 /$912D - 37165 {Interrupt Flag Register}
                  aVIA_IFR[via] := aVIA_IFR[via] and (not Value);    //($80 or ( not Value));
               end;
           $E: begin //W /$911E - 37150 /$912E - 37166 {Interrupt Enable Register}
                  if (Value and $80) <> 0 then aVIA_IER[via] := aVIA_IER[via] or Value
                                          else aVIA_IER[via] := aVIA_IER[via] and (not Value);
               end;
           $F: begin //W /$911F - 37151 /$912F - 37167
                  aVIA_PA[via] := value;
               end;
         end;
      end;

       Memory[Adr] := Value; // write color and VIC data + VIA
      Exit;
   end;



   //Permision depend on memory Ext
   if aMemExt = 0 then //native
   begin
      if (Adr >= $0400) and (Adr <= $0FFF) then Exit;  // Native   area before screen  gap
      if (Adr >= $2000) then Exit  // treat anything above as ROM
   end;
   if aMemExt = 1 then //+3K ram
   begin
      if (Adr >= $2000) then Exit  // treat anything above as ROM
   end;
   if aMemExt = 2 then //+8K ram
   begin
      if (Adr >= $0400) and (Adr <= $0FFF) then Exit;  // 8K ram ext
     if (Adr >= $4000) then Exit  // treat anything above as ROM
//      if (Adr >= $C000) then Exit  // treat anything above as ROM max 32k ram
   end;

//   if (Adr >= $8000) and (Adr <= $8FFF) then Exit;//char rom 4196           // I dont need this
//   if (Adr >= $C000) and (Adr <= $FFFF) then Exit;//roms     2 x 8192

   Memory[Adr] := Value; //std write mem
end;


//------------------------------------------------------------------------------
function GetVIC20_Rom(id:longword; var sz:longword):pointer; forward;

procedure TVIC20.MasterReset;
var i,j:longword;
    p:pointer; sz:longword;
begin
   aRun := false;
   sleep(10);
   aSynch := true;


   for i := 0 to $FFFF do Memory[i] := 0;
   for i := 0 to 7 do for j := 0 to 7 do KeyMatrix[i,j] := 0;

   p := GetVIC20_Rom(0,sz); //kernal
   LoadROM(p,$E000,sz); // E000-FFFF
   p := GetVIC20_Rom(1,sz); //basic
   LoadROM(p,$C000,sz); // C000-DFFF
   p := GetVIC20_Rom(2,sz); //chargen
   LoadROM(p,$8000,sz);

   aTxtRam := @Memory[$1E00];
   aColRam := @Memory[$9600];
   if aMemExt = 2 then aColRam := @Memory[$9400];  //in 8k dif color page
   aFntRam := @Memory[$8000];
   aTopMargin := 0;
   aLeftMargin := 0;
   aTopMarginAdj := 0;
   aLeftMarginAdj := 0;

   VIA_Reset(1);
   VIA_Reset(2);
   Memory[$900E] := $0F;  //volume

   aLastKey := 13;

   Reset; //CPU

   aKeyState := 0;
   aTicksStamp := 0;

   aRun := aRunMR;
end;


//------------------------------------------------------------------------------
function vic20_cbwin(aWindow: HWnd; aMessage: UINT; aWParam : WPARAM;
                                                 aLParam: LPARAM): LRESULT; stdcall;
var ses :TVic20;
    a:longword;
{$IFDEF CPUX64}
    obj:NativeUint;
{$ELSE}
    obj:longword;
{$ENDIF}
begin
   Result := 0;
//   if (aMessage = WM_ERASEBKGND) then // or (aMessage = WM_SETCURSOR) then
//   begin
//       Result := 1; // I do this
//   end else begin
      ses := TVIC20(windows.GetProp(aWindow,'tVic20'));
      if Assigned(ses) then
      begin
         a := (aWParam shr 16) and $FF;
         case aMessage of
            //W_Parm 0-15 repeat count
            //      16-23 scan code
            //      24    extendet key
            //      25-28 not used
            //      29    0 for keyup
            //      30    1 for keyup
            //      31    1 for keyup
            //
            WM_KEYUP :
            begin
               if (ses.aWorkMode and $01000000) <> 0 then
               begin
                  VIC20_Events(ses,a,1,0); //Not work in DELPHI :(
                  Result := 1;
               end;
            end;
            WM_KEYDOWN :
            begin
               if (ses.aWorkMode and $01000000) <> 0 then
               begin
                  VIC20_Events(ses,a,2,0);
                  Result := 1;
               end;
            end;
            WM_USER :   //up
            begin
               if (ses.aWorkMode and $01000000) = 0 then
               begin
                  VIC20_Events(ses,aWParam,1,0);
                  Result := 1;
               end;
            end;
            WM_USER+1 : //down
            begin
               if (ses.aWorkMode and $01000000) = 0 then
               begin
                  VIC20_Events(ses,aWParam,0,0);
                  Result := 1;
               end;
            end;
         end; // message case

      if Result = 0 then
      begin
         obj := GetProp(aWindow,'tVic20cb');
         if obj <> 0 then result := CallWindowProc(pointer(Obj), aWindow, AMessage, aWParam, aLParam)
                     else result := DefWindowProc(aWindow, AMessage, aWParam, aLParam);
      end;
   end;
end;


var KBHook:longword;
//                                       WordParam         LongParam
function KeyboardHookProc(Code: Integer; VirtualKey: Word; KeyStroke: LongInt): LongInt; stdcall;
begin
   if Code = HC_ACTION then
   begin
      if ((KeyStroke and (1 shl 31)) = 0) then // a key is down
      begin
         PostMessage(GetActiveWindow,WM_USER+1,VirtualKey,0); // down
      end else begin
         PostMessage(GetActiveWindow,WM_USER,VirtualKey,0); // up
      end;
      Result := 255;

   end else begin
      Result := CallNextHookEx(KBHook, Code, VirtualKey, KeyStroke);
   end;
end;




procedure TVIC20.SetPal;
var i:integer;
    w:longword;
begin
   for i := 0 to 15 do
   begin
       W := 0;
       case aPal of
//          1: w := rgb(vic20_pal_1[i,0],vic20_pal_1[i,1],vic20_pal_1[i,2]);
//          0: w := rgb(vic20_pal_3[i,0],vic20_pal_3[i,1],vic20_pal_3[i,2]);
//          2: w := rgb(vic20_pal_2[i,0],vic20_pal_2[i,1],vic20_pal_2[i,2]);
//          3: w := rgb(vic20_pal_4[i,0],vic20_pal_4[i,1],vic20_pal_4[i,2]);

          // Light
//          0: w := rgb(vic20_pal_3[i,0],vic20_pal_3[i,1],vic20_pal_3[i,2]);
//          1: w := rgb(vic20_pal_2[i,0],vic20_pal_2[i,1],vic20_pal_2[i,2]);
          // Dark
          0,2: w := rgb(vic20_pal_1[i,0],vic20_pal_1[i,1],vic20_pal_1[i,2]);
          1,3: w := rgb(vic20_pal_4[i,0],vic20_pal_4[i,1],vic20_pal_4[i,2]);



       end;
       Pal[i] := w;
   end;
end;

//------------------------------------------------------------------------------
constructor TVIC20.Create(whand,x,y,xl,yl,mode:longword);
var i:longword;
begin
   Host_HWND := whand;
   Host_Xpos := x;
   Host_Ypos := y;
   Host_Xlng := xl;
   Host_Ylng := yl;
   Host_DC := GetDC(Host_HWND);

   inherited Create(BusRead, BusWrite);

   QueryPerformanceFrequency(aClockRes);
   aClockRes := aClockRes div 1108404; //1Mhz pal

   aKeyTranslate := false;
   aJoyEmulation := false;

   aPal := 1;
   SetPal;

   aWorkMode := mode;
   aMemExt := mode and 3;
   aMemExt := 0; // debug
   //Extention
   //0 -no   3583 ram
   //1 3k    6655 ram
   //2 8k    11775 ram
   if aMemExt = 3 then aMemExt := 2; //max for now


   aRunMR := true;
   MasterReset;
   aRun := false; // stop  cpu prepare for thread

   //FUCK DELPHI i Tray get WM_KEYOP/WM_KEYDOWN with preload wndproc but delphi catch them before :) not possible too handle
   Host_Cback := 0;
   KBHook := 0;
   //Key get by WMessages
   if (mode and $10000000) = 0 then
   begin
      if (mode and $01000000) = 0 then  // delphi focking hook because thay did not provide wm_keyup/down
      begin
         KBHook:=SetWindowsHookEx(WH_KEYBOARD,
              {callback >} @KeyboardHookProc,
                           HInstance,
                           GetCurrentThreadId());
      end;

      Host_CBack := GetWindowLong(Host_HWND,GWL_WNDPROC);
      SetProp(Host_HWND,'tVic20cb',Host_Cback);
{$IFDEF CPUX64}
      SetProp(Host_HWND,'tVic20',nativeUInt(self));
      SetWindowLong(Host_HWND,GWL_WNDPROC,nativeUInt(@vic20_cbwin));
{$ELSE}
      SetProp(Host_HWND,'tVic20',longword(self));
      SetWindowLong(Host_HWND,GWL_WNDPROC,longword(@vic20_cbwin));
{$ENDIF}

   end;

   // set up flags if you use external threads
   //create audio
   aSp1_ofs := 0;
   aSp2_ofs := 0;
   aSp3_ofs := 0;
   aSp4_ofs := 0;
   aSp1_dx := 0;
   aSp2_dx := 0;
   aSp3_dx := 0;
   aSp4_dx := 0;
   aSp1_state := 1;
   aSp2_state := 1;
   aSp3_state := 1;
   aSp4_state := 1;
   aSp4_noise := 0;
   aVolume := 1;
   for i := 0 to 1023 do aNoise[i] := random(255);

   aBSound := nil;
   if (mode and $20000000) = 0 then
   begin
      abshand := 0;
      aBSound := BTWinMMSound.Create;
      if assigned(aBSound) then
      begin                                                 //mono
         abshand := aBSound.OpenRawSound(48000{sample_rate},1 {channels}, 32 {bits}, 255{volume}, 8{flag}, @VIC20_SoundGen, self);
//         abshand := aBSound.OpenRawSound(48000{sample_rate},1 {channels}, 16 {bits}, 255{volume}, 0{flag}, @VIC20_SoundGen, longword(self));
         if abshand <> 0 then
         begin
            aBSound.Play(abshand);
         end;
      end;
   end;

   Terminated := false; //common thread terminator
   ThreadVIC := 0;
   //create video thread
   if (mode and $40000000) = 0 then ThreadVIC := CreateThread(nil,0,@Vic20_VIC_Thread,pointer(self),0,ThreadVIC_ID); //Exec the emulator

   // create run thread
   Thread := 0;
   if (mode and $80000000) = 0 then Thread := CreateThread(nil,0,@Vic20_run_Thread,pointer(self),0,Thread_ID); //Exec the emulator

   aRun := true;
end;

//------------------------------------------------------------------------------
destructor TVIC20.Destroy;
begin
   aRun := false;
   if KBHook <> 0 then UnHookWindowsHookEx(KBHook) ;
   if assigned(aBSound) then //Close AUDIO
   begin
      if abshand <> 0 then
      begin
         aBSound.Stop(abshand);
         aBSound.Close(abshand);
      end;
      sleep(100);
      aBSound.free;
   end;
   Terminated := true;
   sleep(200);
   if Thread <> 0 then //Close CPU
   begin
      TerminateThread(Thread,0);
      CloseHandle(Thread);
   end;
   if ThreadVIC <> 0 then //Close VIC
   begin
      TerminateThread(ThreadVIC,0);
      CloseHandle(ThreadVIC);
   end;

   if Host_DC <> 0 then ReleaseDC(Host_HWND,Host_DC);
   if Host_CBack <> 0 then SetWindowLong(Host_HWND,GWL_WNDPROC,Host_CBack);
   inherited;
end;

//------------------------------------------------------------------------------
type  btarr = array[0..$FFFF] of byte;
procedure TVIC20.WriteDW(adr,w:longword);
begin
   Memory[Adr] := byte(w and $FF);
   Memory[Adr+1] := byte((w shr 8) and $FF);
end;

//------------------------------------------------------------------------------
procedure TVIC20.LoadROM(rom:pointer; Addr,Size: longword);
var p:^btarr;
    i:longword;
begin
   if Addr + size > $10000 then Exit;
   p := rom;
   for i := 0 to Size -1 do Memory[Addr+i] := p[i];
end;

//------------------------------------------------------------------------------
procedure  TVIC20.Load_PRG(prg:pointer; Sz:longword);
var p:pointer;
    a:^btarr;
    StartAddress,EndAddress,i,flg:longword;

   procedure CopyPrg(c:longword);
   begin
//      p := pointer(longword(@Memory[0]) + StartAddress);
      p := @Memory[StartAddress];
{$IFDEF CPUX64}
      prg := pointer(nativeUInt(prg) + c);
{$ELSE}
      prg := pointer(longword(prg) + c);
{$ENDIF}
      move(prg^,p^,sz);
   end;

   procedure MakeMasterReset;
   var j:longword;
   begin
      aRunMR := false; // go not make run true
      MasterReset;  // restart machine with new memory
      aRunMR := true; // go not make run true
      aSynch := false;
      for j := 0 to $FFFFF do CpuStep; // to fill up new machine
      aSynch := true;
   end;

   procedure SetBasicVars;
   begin
      EndAddress := StartAddress + sz - 1;
      // Set basic memory pointers
      WriteDW($2B, StartAddress);
      WriteDW($AC, StartAddress);
      WriteDW($2D, EndAddress);
      WriteDW($2F, EndAddress);
      WriteDW($31, EndAddress);
      WriteDW($AE, EndAddress);
      //WriteDW($2D, StartAddress);
      //WriteDW($2F, EndAddress);
      //WriteDW($31, EndAddress);
      //WriteDW($33, EndAddress);
      WriteDW($37, $FFFF);
      //ne raboti pc := $c871;
      //todo type run + enter   (autostart)
      //WriteDW($AE, EndAddress);
   end;

begin
   flg := sz and $F0000000;
   sz  := sz and $0FFFFFFF;
   if Sz < 4 then Exit;

   aRun := false; // stop thread CpuStep
   sleep(10);
   a := prg;
   aMemExt := 0;


   StartAddress := longword(a[0]) or ( longword(a[1]) shl 8 );
   if (StartAddress = $1201) or (StartAddress = $1001) then
   begin // Basic program
      if Sz > 3583 then  aMemExt := 2; //8K;
      MakeMasterReset;
      Sz := Sz - 2;
      CopyPrg(2);      //Copy

      SetBasicVars;
   end else begin
      if (Sz = 8192) or (Sz = 8194) then
      begin  //catrage
         MakeMasterReset;
         StartAddress := $A000;
         i := 0;
         if Sz = 8194 then
         begin
            StartAddress := longword(a[0]) or ( longword(a[1]) shl 8 );
            Sz := Sz - 2;
            i := 2; // skip 2 bytes
         end;
         CopyPrg(i);
         Reset;  //Autorun it cartrage
      end else begin
         if sz = 16384 then
         begin  //2bank catrage  2000 or 6000   and A000
            if flg <> 0 then
            begin
               MakeMasterReset;
               sz := 8192;
               StartAddress := $6000;
               if flg = $60000000 then StartAddress := $6000;
               if flg = $20000000 then StartAddress := $2000;
               CopyPrg(0);  // first half
               StartAddress := $A000;
               CopyPrg(8192);  // second half
               Reset;
            end;
         end else begin
            // unknown
            StartAddress := $1001;
            if Sz > 3583 then
            begin
               aMemExt := 2; //8K;
               StartAddress := $1201;
            end;
            MakeMasterReset;
            CopyPrg(0);  //Copy

            SetBasicVars;
         end;
      end;
   end;
   aRun := true;
end;

//------------------------------------------------------------------------------
{
VIC20 Keyboard Matrix

Write to Port B($9120)column
Read from Port A($9121)row

     7   6   5   4   3   2   1   0   Col
    --------------------------------
  7| F7  F5  F3  F1  CDN CRT RET DEL    CRT=Cursor-Right, CDN=Cursor-Down
   |
  6| HOM UA  =   RSH /   ;   *   BP     BP=British Pound, RSH=Should be Right-SHIFT,
   |                                    UA=Up Arrow
  5| -   @   :   .   ,   L   P   +
   |
  4| 0   O   K   M   N   J   I   9
   |
  3| 8   U   H   B   V   G   Y   7
   |
  2| 6   T   F   C   X   D   R   5
   |
  1| 4   E   S   Z   LSH A   W   3      LSH=Should be Left-SHIFT
   |
  0| 2   Q   CBM SPC STP CTL LA  1      LA=Left Arrow, CTL=Should be CTRL, STP=RUN/STOP
Row|                                    CBM=Commodore key

C64/VIC20 Keyboard Layout
      !  "  #  $  %  &  '  (  )                  INS          shift
  LA  1  2  3  4  5  6  7  8  9  0  +  -  BP HOM DEL    F1
  CTRL Q  W  E  R  T  Y  U  I  O  P  @  *  UA RESTORE   F3
                                   [  ]                       shift
STOP SL A  S  D  F  G  H  J  K  L  :  ;  =  RETURN      F5
                              <  >  ?                         shift
C= SHIFT Z  X  C  V  B  N  M  ,  .  /  SHIFT  CDN CRT   F7
         [        SPACE BAR       ]


PC -keayboard
 ESC   F1  F2  F3  F4    F5  F6  F7  F8    F9  F10 F11 F12     INS  HOME PGUP
  27   112 113 114 115   116 117 118 119   120 121 122 123     45   36   33
  `    1  2  3  4  5  6  7  8  9  0    -   =    BS             DEL  END  PGDW
 192   49 50 51 52 53 54 55 56 57 48  189 187    8             46   35   34
 TAB*  Q  W  E  R  T  Y  U  I  O  P   [   ]    ENTER            *-only up
  9    81 87 69 82 84 89 85 73 79 80  219 221   13
 CAPS  A  S  D  F  G  H  J  K  L    ;   '   \                       UP
  20   65 83 68 70 71 72 74 75 76   186 222 220                     38
 SHIFT Z  X  C  V  B  N  M   ,   .   /    SHIFT                LEFT DOWN RIGHT
  16   90 88 67 86 66 78 77  188 190 191   16                  37   40   39
 CTRL WIN* ALT     SPACE        ALT WIN* MENU CTRL              *-open win start
  17   91  18        32          18  91   93   17
}




                 //       Row / Col [0..7,0..7]  Row by Row
const Vic20_Key_Map :array [0..63] of longword = ( // Direct map
        byte('1') ,192  {LA} ,VK_CONTROL,VK_PAUSE  ,VK_SPACE  ,VK_MENU   ,byte('Q') ,byte('2'),
        byte('3') ,byte('W') ,byte('A') ,VK_SHIFT  ,byte('Z') ,byte('S') ,byte('E') ,byte('4'),
        byte('5') ,byte('R') ,byte('D') ,byte('X') ,byte('C') ,byte('F') ,byte('T') ,byte('6'),
        byte('7') ,byte('Y') ,byte('G') ,byte('V') ,byte('B') ,byte('H') ,byte('U') ,byte('8'),
        byte('9') ,byte('I') ,byte('J') ,byte('N') ,byte('M') ,byte('K') ,byte('O') ,byte('0'),
        189  {+}  ,byte('P') ,byte('L') ,188  {,}  ,190  {.}  ,186  {:}  ,219  {@}  ,187  {-} ,
        8    {&}  ,221  {*}  ,222  {;}  ,191  {/}  ,0   {RSH} ,220  {=}  ,$DC  {|}  ,VK_HOME  ,
        VK_DELETE ,VK_RETURN ,VK_RIGHT  ,VK_DOWN   ,VK_F1     ,VK_F3     ,VK_F5     ,VK_F7    );


const Vic20_Key_Map_Tr :array [0..63] of longword = ( // Translated map
        byte('1') ,0         ,VK_CONTROL,VK_PAUSE  ,VK_SPACE  ,VK_MENU   ,byte('Q') ,byte('2'),
        byte('3') ,byte('W') ,byte('A') ,VK_SHIFT  ,byte('Z') ,byte('S') ,byte('E') ,byte('4'),
        byte('5') ,byte('R') ,byte('D') ,byte('X') ,byte('C') ,byte('F') ,byte('T') ,byte('6'),
        byte('7') ,byte('Y') ,byte('G') ,byte('V') ,byte('B') ,byte('H') ,byte('U') ,byte('8'),
        byte('9') ,byte('I') ,byte('J') ,byte('N') ,byte('M') ,byte('K') ,byte('O') ,byte('0'),
        0    {+}  ,byte('P') ,byte('L') ,188  {,}  ,190  {.}  ,0    {:}  ,0    {@}  ,0    {-} ,
        0    {&}  ,0    {*}  ,0    {;}  ,191  {/}  ,0   {RSH} ,0    {=}  ,0    {|}  ,VK_HOME  ,
        VK_DELETE ,VK_RETURN ,VK_RIGHT  ,VK_DOWN   ,VK_F1     ,VK_F3     ,VK_F5     ,VK_F7    );


//------------------------------------------------------------------------------
procedure  TVIC20.KeyEvent(V_Key,Key_State:longword); // KeyState 0-down 1-up
var c,d:longword;

   procedure PokeKeyMat(mat_id:longword);
   var cc,rr:longword;
   begin
      //  [Row,Col] [0..7,0..7]
      rr := mat_id shr 3;
      cc := mat_id and 7;
      if Key_State = 0 then // down
      begin
//       aVIA_ca1[2] := false; //!!!!! only for restore key
         KeyMatrix[rr,cc]:= 1; // Key-Down
      end;
      if Key_State = 1 then // up
      begin
//       aVIA_ca1[2] := true;  //!!!!! only for restore key
         KeyMatrix[rr,cc]:= 0; // Key-Up
      end;
   end;

begin

   //System
   if Key_State = 1 then // up
   begin
      if V_Key = VK_SHIFT  then aKeyState := aKeyState and (not $1);
      if V_Key = VK_CONTROL then aKeyState := aKeyState and (not $2);
      if V_Key = VK_MENU   then aKeyState := aKeyState and (not $4); {ALT}
   end;

   if Key_State = 0 then // down
   begin
      if V_Key = VK_SHIFT  then aKeyState := aKeyState or $1;
      if V_Key = VK_CONTROL then aKeyState := aKeyState or $2;
      if V_Key = VK_MENU   then aKeyState := aKeyState or $4; {ALT}
   end;

   if (V_key = 123 {F12}) and (Key_State=1{up}) then
   begin // change pal
      aPal := (aPal + 1) and $3;
      SetPal;
   end;
   if (V_key = 122 {F11}) and (Key_State=1{up}) then
   begin
      aKeyTranslate := not aKeyTranslate;
   end;
   if (V_key = 121 {F10}) and (Key_State=1{up}) then
   begin
      aJoyEmulation := not aJoyEmulation;
   end;
   if aJoyEmulation then
   begin
      { aJoy $1-Fire $10-Left $20-Right $40-Up $80-Down }
      c := 0;
      if Key_state = 0 then // Down
      begin
         if V_Key = VK_LEFT  then begin c:=1;  aJoy := aJoy or $10; end;
         if V_Key = VK_RIGHT then begin c:=1;  aJoy := aJoy or $20; end;
         if V_Key = VK_UP    then begin c:=1;  aJoy := aJoy or $40; end;
         if V_Key = VK_DOWN  then begin c:=1;  aJoy := aJoy or $80; end;
         if V_Key = VK_CONTROL  then begin c:=1;  aJoy := aJoy or $1;  end;
      end;
      if Key_state = 1 then // up
      begin
         if V_Key = VK_LEFT  then begin c:=1;  aJoy := aJoy and (not $10); end;
         if V_Key = VK_RIGHT then begin c:=1;  aJoy := aJoy and (not $20); end;
         if V_Key = VK_UP    then begin c:=1;  aJoy := aJoy and (not $40); end;
         if V_Key = VK_DOWN  then begin c:=1;  aJoy := aJoy and (not $80); end;
         if V_Key = VK_CONTROL  then begin c:=1;  aJoy := aJoy and (not $1);  end;
      end;
      if c = 1 then Exit; // do not proceed keys
   end;








   if aKeyTranslate then
   begin
      // Key board tranlation mode
      d := 0;
      for c := 0 to 63 do
      begin
         if Vic20_Key_Map[c] = V_Key then
         begin
            PokeKeyMat(c);
            d := 1;
            //break;  //?// :( delphi error   E2014 Statement expected. but expression of type 'integer' found on line
         end;
      end;
      if d = 0 then
      begin

      end;
   end else begin
      // native mode
      for c := 0 to 63 do
      begin
         if Vic20_Key_Map[c] = V_Key then
         begin
            PokeKeyMat(c);
            //break;  //?// :( delphi error   E2014 Statement expected. but expression of type 'integer' found on line
         end;
      end;
   end;
end;

//------------------------------------------------------------------------------
function VIC20_Events(obj:pointer; key{v_key},prs{0-down 1-up 2-char},ks:longword):longint;  stdcall;
var o:TVIC20;
begin
   Result := 0;
   try
      o := TVIC20(obj);
      if prs = 0 then o.KeyEvent(key,0); // down
      if prs = 1 then o.KeyEvent(key,1); // up
   except
      Result := -1;
   end;
end;


//------------------------------------------------------------------------------
// Sound Generator



type starr = array [0..0] of single;
//type wtarr = array [0..0] of word;

function  TVIC20.SoundRender(data:pointer; len:longword):longint;
var i :longword;
    f,fo :single;
//    w:longword;
    a:^starr;
  //  a:^wtarr;
begin //FILL DATA WITH len generating sound
   len := len div 4; // 4 bytes per sample
// len := len div 2; // 4 bytes per sample
   a := data;
   fo := 0;
   for i := 0 to len-1 do
   begin
      // vic audio registers
      // $900A - 36874 Speaker-1 the alto voice
      // Bit 7 is the on/off control bit, whilst bits 0-6 select the actual note. Speaker 1 has an alto voice.
      // $900B - 36875 Speaker-2 the tenor voice
      // $900C - 36876 Speaker-3 the soprano voice
      // $900D - 36877 speaker-4 the noise voice
      // $900E - 36878
      // Bits 0-3 volume of four speakers.
      //  sp-1 Clock  4329     Freq = Clock /((127 - N) + 1)   N = 128..255
      //  sp-2 Clock  8659
      //  sp-3 Clock 17320
      //  sp-4 Cloxk 34640


//    if (i and 1)  =0 then
//    begin
      // input  the buffer if 32bit float 0..1
      f := 0;
//  w := 0;
      if (Memory[$900A] and $80) <> 0  then // Speaker 1    ALTO
      begin
         if (aSp1_state and 1) = 0 then f := f - 0.24  //pulse down
                                   else f := f + 0.24; //pulse up
//         if (aSp1_state and 1) = 0 then w := w + 0  //pulse down
//                                   else w := w + 4096; //pulse up

         inc(aSp1_ofs);
         if aSp1_ofs > aSp1_dx then
         begin
            aSp1_ofs := 0;
            aSp1_State := aSp1_State xor 1;
         end;
      end;
      if (Memory[$900B] and $80) <> 0  then // Speaker 2    TENOR
      begin
         if (aSp2_state and 1) = 0 then f := f - 0.24
                                   else f := f + 0.24;
//         if (aSp2_state and 1) = 0 then w := w + 0  //pulse down
//                                   else w := w + 4096; //pulse up

         inc(aSp2_ofs);
         if aSp2_ofs > aSp2_dx then
         begin
            aSp2_ofs := 0;
            aSp2_State := aSp2_State xor 1;
         end;
      end;
      if (Memory[$900C] and $80) <> 0  then // Speaker 3    SOPRANO
      begin
         if (aSp3_state and 1) = 0 then f := f - 0.24
                                   else f := f + 0.24;
//         if (aSp3_state and 1) = 0 then w := w + 0  //pulse down
//                                   else w := w + 4096; //pulse up

         inc(aSp3_ofs);
         if aSp3_ofs > aSp3_dx then
         begin
            aSp3_ofs := 0;
            aSp3_State := aSp3_State xor 1;
         end;
      end;
      if (Memory[$900D] and $80) <> 0  then // Speaker 4    NOISE
      begin
         if (aSp4_state and 1) = 0 then f := f - 0.24
                                   else f := f + 0.24;
//         if (aSp4_state and 1) = 0 then w := w + 0  //pulse down
//                                   else w := w + 4096; //pulse up

         inc(aSp4_ofs);
         if aSp4_ofs > aSp4_dx then
         begin
            aSp4_ofs := 0;
            inc(aSp4_noise);
            {this make 8k note from 1k patter by shift}
            aSp4_State := (aNoise[aSp4_noise and $3FF] shr ((aSp4_noise and 10) and $7)) and 1;
         end;
      end;
//    end;
  //    a[i] := w;
        a[i] := ((f + fo ) / 2) * aVolume;// + (fo+f)*psf;;
        fo := f;
   end;


   Result := 0; // do not stop hread
end;

//------------------------------------------------------------------------------
function VIC20_SoundGen(obj:pointer; data:pointer; len:longword):longint; stdcall;
var o:TVIC20;
begin
   try
      o := TVIC20(obj);
      Result := o.SoundRender(data,len);
   except
      Result := 0;
   end;
end;

//------------------------------------------------------------------------------
function VIC20_render(obj:pointer):pointer;  stdcall;
var o:TVIC20;
begin
   try
      o := TVIC20(obj);
      Result := o.ScrRender;
   except
      Result := nil;
   end;
end;

const btmask  : array[0..7] of byte = ($80,$40,$20,$10,$8,$4,$2,$1);
const mbtmask : array[0..7] of byte = ($C0,$C0,$30,$30,$C,$C,$3,$3);
const mbtshft : array[0..7] of byte = (  6,  6,  4,  4, 2, 2, 0, 0);
function  TVIC20.ScrRender:pointer;
var ptc,y,x,xb,yb,cx,cy,xi,yi:longword;
    w,tc,rc,bc,cc,ac:longword;
    Tram,Cram,cp:^btarr;
    mL,mR,mT,mB,a8div,a8mask:longword;
    xa,ya:longint;
begin
   //screen is 640x480   22*8*3wide=528 640-528=112 112/2=56space  56+528=584
   //                    23*8*2wide=368 480-368=112 112/2=56space  56+368=424
   //
   a8div := 3; // div by 8
   a8mask := $7;  //8
   if a16x8char <> 0 then
   begin
      a8div := 4; // div by 16
      a8mask := $F;  //16
   end;


   ptc := 0;
   ac := Pal[aAuxColor and $F]; //auxiliary color
   rc := Pal[aFrmColor and $F];
   bc := Pal[aBgrColor and $F];
   Cram := aColRam;
   Tram := aTxtRam;

   xa := longint(aLeftMargin) + aLeftMarginAdj;
   if xa < 0 then xa:= 0;
   ya := longint(aTopMargin) + aTopMarginAdj;
   if ya < 0 then ya:= 0;

   mL := longword(xa);  mR := mL + aTxtXlng; //584
   mT := longword(ya);   mB := mt + aTxtYlng; // 424
   for y := 0 to 479 do //Y=480
   begin
      for x := 0 to 639 do // X=640
      begin //row by row of
         if y < mT then
         begin
            ScrBitmap[x + ptc] := rc;
            continue;
         end else begin
            if y >= mB then
            begin
               ScrBitmap[x + ptc] := rc;
               continue;
            end else begin
              if x < mL then
              begin
                 ScrBitmap[x + ptc] := rc;
                 continue;
              end else begin
                  if x >= mR then
                  begin
                     ScrBitmap[x + ptc] := rc;
                     continue;
                  end else begin
                     // Real screen 2x3 to screen 1x1 pixel xi,yi
                     yi := (y - mT) shr 1; //yi = y div 2 2wide
                     xi := (x - mL) div 3; // 1024/3=341    (x*341)/1024 = x/3 ne raboti
                     // yb = byte position  cy = on txt screen row pos
                     yb := yi and a8mask;
                     cy := yi shr a8div; //row  div 8
                     xb := xi and $7;
                     cx := xi shr 3; //column
                     w := longword(Tram[cx + cy * aTxtCols]) and $FF;
                     cc := longword(Cram[cx + cy * aTxtCols]);
                     tc := Pal[cc and $7]; // 8 colors hi byte $8 is invert color
                     // get char row mask  from  font matrix  w-char  yb- row in char 0-7
//                     cp := pointer(longword(aFntRam) + (w shl a8div) + yb ); // char size is 8x8 row of char bitmap
{$IFDEF CPUX64}
                     cp := pointer(nativeUInt(aFntRam) + (w shl a8div) + yb ); // char size is 8x8 row of char bitmap
{$ELSE}
                     cp := pointer(longword(aFntRam) + (w shl a8div) + yb ); // char size is 8x8 row of char bitmap
{$ENDIF}
                     if (cc and $8) = 0 then
                     begin
                     //   if (Y and 1) <> 0  then
                     //   begin
                        if (aInvertMode and 1) = 1 then
                        begin
                           if (cp[0] and btmask[xb]) <> 0   // Normal
                           then ScrBitmap[x + ptc] := tc
                           else ScrBitmap[x + ptc] := bc; //background
                        end else begin
                           if (cp[0] and btmask[xb]) <> 0  //Inverted
                           then ScrBitmap[x + ptc] := bc
                           else ScrBitmap[x + ptc] := tc; //background
                        end;
                     end else begin
                        //multicolor
                        cc := (cp[0] and mbtmask[xb]) shr mbtshft[xb];
                        if (aInvertMode and 1) = 1 then
                        begin
                           case cc of                           // multi color mode 4x8 char
                              0: ScrBitmap[x + ptc] := bc; //00-screen color
                              1: ScrBitmap[x + ptc] := rc; //01-border color
                              2: ScrBitmap[x + ptc] := tc; //10-char color
                              3: ScrBitmap[x + ptc] := ac; //11-aux color
                           end;
                        end else begin
                           // inverted bc <> tc is this right ????
                           case cc of                           // multi color mode 4x8 char
                              0: ScrBitmap[x + ptc] := tc; //00-screen color
                              1: ScrBitmap[x + ptc] := rc; //01-border color
                              2: ScrBitmap[x + ptc] := bc; //10-char color
                              3: ScrBitmap[x + ptc] := ac; //11-aux color
                           end;
                        end;
                     end;
                     Memory[$9003] := (Memory[$9003] and $7F) or ((yi and 1) shl 7); // Raster value VIC 6561
                     Memory[$9004] := (yi shr 1) and $FF;
                  end;
               end;
            end;
         end;
      end;
      if (Y and 1) <> 0 then //tv effect
      begin
         for x := 0 to 639 do // X=640
         if ScrBitmap[x + ptc] <> 0 then ScrBitmap[x + ptc] := ScrBitmap[x + ptc] - $00101010
      end;
      ptc := ptc + 640;
   end;
   Result := @ScrBitmap[0];
end;












// BIN data of chargen ---------------------------------------------------------
// length = 4096

const
   Bin_chargen_len = 4096;
var
   Bin_chargen : array [0..4095] of byte = (
      $1C,$22,$4A,$56,$4C,$20,$1E,$00,$18,$24,$42,$7E,$42,$42,$42,$00,
      $7C,$22,$22,$3C,$22,$22,$7C,$00,$1C,$22,$40,$40,$40,$22,$1C,$00,
      $78,$24,$22,$22,$22,$24,$78,$00,$7E,$40,$40,$78,$40,$40,$7E,$00,
      $7E,$40,$40,$78,$40,$40,$40,$00,$1C,$22,$40,$4E,$42,$22,$1C,$00,
      $42,$42,$42,$7E,$42,$42,$42,$00,$1C,$08,$08,$08,$08,$08,$1C,$00,
      $0E,$04,$04,$04,$04,$44,$38,$00,$42,$44,$48,$70,$48,$44,$42,$00,
      $40,$40,$40,$40,$40,$40,$7E,$00,$42,$66,$5A,$5A,$42,$42,$42,$00,
      $42,$62,$52,$4A,$46,$42,$42,$00,$18,$24,$42,$42,$42,$24,$18,$00,
      $7C,$42,$42,$7C,$40,$40,$40,$00,$18,$24,$42,$42,$4A,$24,$1A,$00,
      $7C,$42,$42,$7C,$48,$44,$42,$00,$3C,$42,$40,$3C,$02,$42,$3C,$00,
      $3E,$08,$08,$08,$08,$08,$08,$00,$42,$42,$42,$42,$42,$42,$3C,$00,
      $42,$42,$42,$24,$24,$18,$18,$00,$42,$42,$42,$5A,$5A,$66,$42,$00,
      $42,$42,$24,$18,$24,$42,$42,$00,$22,$22,$22,$1C,$08,$08,$08,$00,
      $7E,$02,$04,$18,$20,$40,$7E,$00,$3C,$20,$20,$20,$20,$20,$3C,$00,
      $0C,$10,$10,$3C,$10,$70,$6E,$00,$3C,$04,$04,$04,$04,$04,$3C,$00,
      $00,$08,$1C,$2A,$08,$08,$08,$08,$00,$00,$10,$20,$7F,$20,$10,$00,
      $00,$00,$00,$00,$00,$00,$00,$00,$08,$08,$08,$08,$00,$00,$08,$00,
      $24,$24,$24,$00,$00,$00,$00,$00,$24,$24,$7E,$24,$7E,$24,$24,$00,
      $08,$1E,$28,$1C,$0A,$3C,$08,$00,$00,$62,$64,$08,$10,$26,$46,$00,
      $30,$48,$48,$30,$4A,$44,$3A,$00,$04,$08,$10,$00,$00,$00,$00,$00,
      $04,$08,$10,$10,$10,$08,$04,$00,$20,$10,$08,$08,$08,$10,$20,$00,
      $08,$2A,$1C,$3E,$1C,$2A,$08,$00,$00,$08,$08,$3E,$08,$08,$00,$00,
      $00,$00,$00,$00,$00,$08,$08,$10,$00,$00,$00,$7E,$00,$00,$00,$00,
      $00,$00,$00,$00,$00,$18,$18,$00,$00,$02,$04,$08,$10,$20,$40,$00,
      $3C,$42,$46,$5A,$62,$42,$3C,$00,$08,$18,$28,$08,$08,$08,$3E,$00,
      $3C,$42,$02,$0C,$30,$40,$7E,$00,$3C,$42,$02,$1C,$02,$42,$3C,$00,
      $04,$0C,$14,$24,$7E,$04,$04,$00,$7E,$40,$78,$04,$02,$44,$38,$00,
      $1C,$20,$40,$7C,$42,$42,$3C,$00,$7E,$42,$04,$08,$10,$10,$10,$00,
      $3C,$42,$42,$3C,$42,$42,$3C,$00,$3C,$42,$42,$3E,$02,$04,$38,$00,
      $00,$00,$08,$00,$00,$08,$00,$00,$00,$00,$08,$00,$00,$08,$08,$10,
      $0E,$18,$30,$60,$30,$18,$0E,$00,$00,$00,$7E,$00,$7E,$00,$00,$00,
      $70,$18,$0C,$06,$0C,$18,$70,$00,$3C,$42,$02,$0C,$10,$00,$10,$00,
      $00,$00,$00,$00,$FF,$00,$00,$00,$08,$1C,$3E,$7F,$7F,$1C,$3E,$00,
      $10,$10,$10,$10,$10,$10,$10,$10,$00,$00,$00,$FF,$00,$00,$00,$00,
      $00,$00,$FF,$00,$00,$00,$00,$00,$00,$FF,$00,$00,$00,$00,$00,$00,
      $00,$00,$00,$00,$00,$FF,$00,$00,$20,$20,$20,$20,$20,$20,$20,$20,
      $04,$04,$04,$04,$04,$04,$04,$04,$00,$00,$00,$00,$E0,$10,$08,$08,
      $08,$08,$08,$04,$03,$00,$00,$00,$08,$08,$08,$10,$E0,$00,$00,$00,
      $80,$80,$80,$80,$80,$80,$80,$FF,$80,$40,$20,$10,$08,$04,$02,$01,
      $01,$02,$04,$08,$10,$20,$40,$80,$FF,$80,$80,$80,$80,$80,$80,$80,
      $FF,$01,$01,$01,$01,$01,$01,$01,$00,$3C,$7E,$7E,$7E,$7E,$3C,$00,
      $00,$00,$00,$00,$00,$00,$FF,$00,$36,$7F,$7F,$7F,$3E,$1C,$08,$00,
      $40,$40,$40,$40,$40,$40,$40,$40,$00,$00,$00,$00,$03,$04,$08,$08,
      $81,$42,$24,$18,$18,$24,$42,$81,$00,$3C,$42,$42,$42,$42,$3C,$00,
      $08,$1C,$2A,$77,$2A,$08,$08,$00,$02,$02,$02,$02,$02,$02,$02,$02,
      $08,$1C,$3E,$7F,$3E,$1C,$08,$00,$08,$08,$08,$08,$FF,$08,$08,$08,
      $A0,$50,$A0,$50,$A0,$50,$A0,$50,$08,$08,$08,$08,$08,$08,$08,$08,
      $00,$00,$01,$3E,$54,$14,$14,$00,$FF,$7F,$3F,$1F,$0F,$07,$03,$01,
      $00,$00,$00,$00,$00,$00,$00,$00,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,
      $00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,
      $00,$00,$00,$00,$00,$00,$00,$FF,$80,$80,$80,$80,$80,$80,$80,$80,
      $AA,$55,$AA,$55,$AA,$55,$AA,$55,$01,$01,$01,$01,$01,$01,$01,$01,
      $00,$00,$00,$00,$AA,$55,$AA,$55,$FF,$FE,$FC,$F8,$F0,$E0,$C0,$80,
      $03,$03,$03,$03,$03,$03,$03,$03,$08,$08,$08,$08,$0F,$08,$08,$08,
      $00,$00,$00,$00,$0F,$0F,$0F,$0F,$08,$08,$08,$08,$0F,$00,$00,$00,
      $00,$00,$00,$00,$F8,$08,$08,$08,$00,$00,$00,$00,$00,$00,$FF,$FF,
      $00,$00,$00,$00,$0F,$08,$08,$08,$08,$08,$08,$08,$FF,$00,$00,$00,
      $00,$00,$00,$00,$FF,$08,$08,$08,$08,$08,$08,$08,$F8,$08,$08,$08,
      $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$E0,$E0,$E0,$E0,$E0,$E0,$E0,$E0,
      $07,$07,$07,$07,$07,$07,$07,$07,$FF,$FF,$00,$00,$00,$00,$00,$00,
      $FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,
      $01,$01,$01,$01,$01,$01,$01,$FF,$00,$00,$00,$00,$F0,$F0,$F0,$F0,
      $0F,$0F,$0F,$0F,$00,$00,$00,$00,$08,$08,$08,$08,$F8,$00,$00,$00,
      $F0,$F0,$F0,$F0,$00,$00,$00,$00,$F0,$F0,$F0,$F0,$0F,$0F,$0F,$0F,
      $E3,$DD,$B5,$A9,$B3,$DF,$E1,$FF,$E7,$DB,$BD,$81,$BD,$BD,$BD,$FF,
      $83,$DD,$DD,$C3,$DD,$DD,$83,$FF,$E3,$DD,$BF,$BF,$BF,$DD,$E3,$FF,
      $87,$DB,$DD,$DD,$DD,$DB,$87,$FF,$81,$BF,$BF,$87,$BF,$BF,$81,$FF,
      $81,$BF,$BF,$87,$BF,$BF,$BF,$FF,$E3,$DD,$BF,$B1,$BD,$DD,$E3,$FF,
      $BD,$BD,$BD,$81,$BD,$BD,$BD,$FF,$E3,$F7,$F7,$F7,$F7,$F7,$E3,$FF,
      $F1,$FB,$FB,$FB,$FB,$BB,$C7,$FF,$BD,$BB,$B7,$8F,$B7,$BB,$BD,$FF,
      $BF,$BF,$BF,$BF,$BF,$BF,$81,$FF,$BD,$99,$A5,$A5,$BD,$BD,$BD,$FF,
      $BD,$9D,$AD,$B5,$B9,$BD,$BD,$FF,$E7,$DB,$BD,$BD,$BD,$DB,$E7,$FF,
      $83,$BD,$BD,$83,$BF,$BF,$BF,$FF,$E7,$DB,$BD,$BD,$B5,$DB,$E5,$FF,
      $83,$BD,$BD,$83,$B7,$BB,$BD,$FF,$C3,$BD,$BF,$C3,$FD,$BD,$C3,$FF,
      $C1,$F7,$F7,$F7,$F7,$F7,$F7,$FF,$BD,$BD,$BD,$BD,$BD,$BD,$C3,$FF,
      $BD,$BD,$BD,$DB,$DB,$E7,$E7,$FF,$BD,$BD,$BD,$A5,$A5,$99,$BD,$FF,
      $BD,$BD,$DB,$E7,$DB,$BD,$BD,$FF,$DD,$DD,$DD,$E3,$F7,$F7,$F7,$FF,
      $81,$FD,$FB,$E7,$DF,$BF,$81,$FF,$C3,$DF,$DF,$DF,$DF,$DF,$C3,$FF,
      $F3,$EF,$EF,$C3,$EF,$8F,$91,$FF,$C3,$FB,$FB,$FB,$FB,$FB,$C3,$FF,
      $FF,$F7,$E3,$D5,$F7,$F7,$F7,$F7,$FF,$FF,$EF,$DF,$80,$DF,$EF,$FF,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$F7,$F7,$F7,$F7,$FF,$FF,$F7,$FF,
      $DB,$DB,$DB,$FF,$FF,$FF,$FF,$FF,$DB,$DB,$81,$DB,$81,$DB,$DB,$FF,
      $F7,$E1,$D7,$E3,$F5,$C3,$F7,$FF,$FF,$9D,$9B,$F7,$EF,$D9,$B9,$FF,
      $CF,$B7,$B7,$CF,$B5,$BB,$C5,$FF,$FB,$F7,$EF,$FF,$FF,$FF,$FF,$FF,
      $FB,$F7,$EF,$EF,$EF,$F7,$FB,$FF,$DF,$EF,$F7,$F7,$F7,$EF,$DF,$FF,
      $F7,$D5,$E3,$C1,$E3,$D5,$F7,$FF,$FF,$F7,$F7,$C1,$F7,$F7,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$F7,$F7,$EF,$FF,$FF,$FF,$81,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$E7,$E7,$FF,$FF,$FD,$FB,$F7,$EF,$DF,$BF,$FF,
      $C3,$BD,$B9,$A5,$9D,$BD,$C3,$FF,$F7,$E7,$D7,$F7,$F7,$F7,$C1,$FF,
      $C3,$BD,$FD,$F3,$CF,$BF,$81,$FF,$C3,$BD,$FD,$E3,$FD,$BD,$C3,$FF,
      $FB,$F3,$EB,$DB,$81,$FB,$FB,$FF,$81,$BF,$87,$FB,$FD,$BB,$C7,$FF,
      $E3,$DF,$BF,$83,$BD,$BD,$C3,$FF,$81,$BD,$FB,$F7,$EF,$EF,$EF,$FF,
      $C3,$BD,$BD,$C3,$BD,$BD,$C3,$FF,$C3,$BD,$BD,$C1,$FD,$FB,$C7,$FF,
      $FF,$FF,$F7,$FF,$FF,$F7,$FF,$FF,$FF,$FF,$F7,$FF,$FF,$F7,$F7,$EF,
      $F1,$E7,$CF,$9F,$CF,$E7,$F1,$FF,$FF,$FF,$81,$FF,$81,$FF,$FF,$FF,
      $8F,$E7,$F3,$F9,$F3,$E7,$8F,$FF,$C3,$BD,$FD,$F3,$EF,$FF,$EF,$FF,
      $FF,$FF,$FF,$FF,$00,$FF,$FF,$FF,$F7,$E3,$C1,$80,$80,$E3,$C1,$FF,
      $EF,$EF,$EF,$EF,$EF,$EF,$EF,$EF,$FF,$FF,$FF,$00,$FF,$FF,$FF,$FF,
      $FF,$FF,$00,$FF,$FF,$FF,$FF,$FF,$FF,$00,$FF,$FF,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$00,$FF,$FF,$DF,$DF,$DF,$DF,$DF,$DF,$DF,$DF,
      $FB,$FB,$FB,$FB,$FB,$FB,$FB,$FB,$FF,$FF,$FF,$FF,$1F,$EF,$F7,$F7,
      $F7,$F7,$F7,$FB,$FC,$FF,$FF,$FF,$F7,$F7,$F7,$EF,$1F,$FF,$FF,$FF,
      $7F,$7F,$7F,$7F,$7F,$7F,$7F,$00,$7F,$BF,$DF,$EF,$F7,$FB,$FD,$FE,
      $FE,$FD,$FB,$F7,$EF,$DF,$BF,$7F,$00,$7F,$7F,$7F,$7F,$7F,$7F,$7F,
      $00,$FE,$FE,$FE,$FE,$FE,$FE,$FE,$FF,$C3,$81,$81,$81,$81,$C3,$FF,
      $FF,$FF,$FF,$FF,$FF,$FF,$00,$FF,$C9,$80,$80,$80,$C1,$E3,$F7,$FF,
      $BF,$BF,$BF,$BF,$BF,$BF,$BF,$BF,$FF,$FF,$FF,$FF,$FC,$FB,$F7,$F7,
      $7E,$BD,$DB,$E7,$E7,$DB,$BD,$7E,$FF,$C3,$BD,$BD,$BD,$BD,$C3,$FF,
      $F7,$E3,$D5,$88,$D5,$F7,$F7,$FF,$FD,$FD,$FD,$FD,$FD,$FD,$FD,$FD,
      $F7,$E3,$C1,$80,$C1,$E3,$F7,$FF,$F7,$F7,$F7,$F7,$00,$F7,$F7,$F7,
      $5F,$AF,$5F,$AF,$5F,$AF,$5F,$AF,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,
      $FF,$FF,$FE,$C1,$AB,$EB,$EB,$FF,$00,$80,$C0,$E0,$F0,$F8,$FC,$FE,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,
      $FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,
      $55,$AA,$55,$AA,$55,$AA,$55,$AA,$FE,$FE,$FE,$FE,$FE,$FE,$FE,$FE,
      $FF,$FF,$FF,$FF,$55,$AA,$55,$AA,$00,$01,$03,$07,$0F,$1F,$3F,$7F,
      $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$F7,$F7,$F7,$F7,$F0,$F7,$F7,$F7,
      $FF,$FF,$FF,$FF,$F0,$F0,$F0,$F0,$F7,$F7,$F7,$F7,$F0,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$07,$F7,$F7,$F7,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,
      $FF,$FF,$FF,$FF,$F0,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$00,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$00,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$07,$F7,$F7,$F7,
      $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,
      $F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,
      $00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,
      $FE,$FE,$FE,$FE,$FE,$FE,$FE,$00,$FF,$FF,$FF,$FF,$0F,$0F,$0F,$0F,
      $F0,$F0,$F0,$F0,$FF,$FF,$FF,$FF,$F7,$F7,$F7,$F7,$07,$FF,$FF,$FF,
      $0F,$0F,$0F,$0F,$FF,$FF,$FF,$FF,$0F,$0F,$0F,$0F,$F0,$F0,$F0,$F0,
      $1C,$22,$4A,$56,$4C,$20,$1E,$00,$00,$00,$38,$04,$3C,$44,$3A,$00,
      $40,$40,$5C,$62,$42,$62,$5C,$00,$00,$00,$3C,$42,$40,$42,$3C,$00,
      $02,$02,$3A,$46,$42,$46,$3A,$00,$00,$00,$3C,$42,$7E,$40,$3C,$00,
      $0C,$12,$10,$7C,$10,$10,$10,$00,$00,$00,$3A,$46,$46,$3A,$02,$3C,
      $40,$40,$5C,$62,$42,$42,$42,$00,$08,$00,$18,$08,$08,$08,$1C,$00,
      $04,$00,$0C,$04,$04,$04,$44,$38,$40,$40,$44,$48,$50,$68,$44,$00,
      $18,$08,$08,$08,$08,$08,$1C,$00,$00,$00,$76,$49,$49,$49,$49,$00,
      $00,$00,$5C,$62,$42,$42,$42,$00,$00,$00,$3C,$42,$42,$42,$3C,$00,
      $00,$00,$5C,$62,$62,$5C,$40,$40,$00,$00,$3A,$46,$46,$3A,$02,$02,
      $00,$00,$5C,$62,$40,$40,$40,$00,$00,$00,$3E,$40,$3C,$02,$7C,$00,
      $10,$10,$7C,$10,$10,$12,$0C,$00,$00,$00,$42,$42,$42,$46,$3A,$00,
      $00,$00,$42,$42,$42,$24,$18,$00,$00,$00,$41,$49,$49,$49,$36,$00,
      $00,$00,$42,$24,$18,$24,$42,$00,$00,$00,$42,$42,$46,$3A,$02,$3C,
      $00,$00,$7E,$04,$18,$20,$7E,$00,$3C,$20,$20,$20,$20,$20,$3C,$00,
      $0C,$10,$10,$3C,$10,$70,$6E,$00,$3C,$04,$04,$04,$04,$04,$3C,$00,
      $00,$08,$1C,$2A,$08,$08,$08,$08,$00,$00,$10,$20,$7F,$20,$10,$00,
      $00,$00,$00,$00,$00,$00,$00,$00,$08,$08,$08,$08,$00,$00,$08,$00,
      $24,$24,$24,$00,$00,$00,$00,$00,$24,$24,$7E,$24,$7E,$24,$24,$00,
      $08,$1E,$28,$1C,$0A,$3C,$08,$00,$00,$62,$64,$08,$10,$26,$46,$00,
      $30,$48,$48,$30,$4A,$44,$3A,$00,$04,$08,$10,$00,$00,$00,$00,$00,
      $04,$08,$10,$10,$10,$08,$04,$00,$20,$10,$08,$08,$08,$10,$20,$00,
      $08,$2A,$1C,$3E,$1C,$2A,$08,$00,$00,$08,$08,$3E,$08,$08,$00,$00,
      $00,$00,$00,$00,$00,$08,$08,$10,$00,$00,$00,$7E,$00,$00,$00,$00,
      $00,$00,$00,$00,$00,$18,$18,$00,$00,$02,$04,$08,$10,$20,$40,$00,
      $3C,$42,$46,$5A,$62,$42,$3C,$00,$08,$18,$28,$08,$08,$08,$3E,$00,
      $3C,$42,$02,$0C,$30,$40,$7E,$00,$3C,$42,$02,$1C,$02,$42,$3C,$00,
      $04,$0C,$14,$24,$7E,$04,$04,$00,$7E,$40,$78,$04,$02,$44,$38,$00,
      $1C,$20,$40,$7C,$42,$42,$3C,$00,$7E,$42,$04,$08,$10,$10,$10,$00,
      $3C,$42,$42,$3C,$42,$42,$3C,$00,$3C,$42,$42,$3E,$02,$04,$38,$00,
      $00,$00,$08,$00,$00,$08,$00,$00,$00,$00,$08,$00,$00,$08,$08,$10,
      $0E,$18,$30,$60,$30,$18,$0E,$00,$00,$00,$7E,$00,$7E,$00,$00,$00,
      $70,$18,$0C,$06,$0C,$18,$70,$00,$3C,$42,$02,$0C,$10,$00,$10,$00,
      $00,$00,$00,$00,$FF,$00,$00,$00,$18,$24,$42,$7E,$42,$42,$42,$00,
      $7C,$22,$22,$3C,$22,$22,$7C,$00,$1C,$22,$40,$40,$40,$22,$1C,$00,
      $78,$24,$22,$22,$22,$24,$78,$00,$7E,$40,$40,$78,$40,$40,$7E,$00,
      $7E,$40,$40,$78,$40,$40,$40,$00,$1C,$22,$40,$4E,$42,$22,$1C,$00,
      $42,$42,$42,$7E,$42,$42,$42,$00,$1C,$08,$08,$08,$08,$08,$1C,$00,
      $0E,$04,$04,$04,$04,$44,$38,$00,$42,$44,$48,$70,$48,$44,$42,$00,
      $40,$40,$40,$40,$40,$40,$7E,$00,$42,$66,$5A,$5A,$42,$42,$42,$00,
      $42,$62,$52,$4A,$46,$42,$42,$00,$18,$24,$42,$42,$42,$24,$18,$00,
      $7C,$42,$42,$7C,$40,$40,$40,$00,$18,$24,$42,$42,$4A,$24,$1A,$00,
      $7C,$42,$42,$7C,$48,$44,$42,$00,$3C,$42,$40,$3C,$02,$42,$3C,$00,
      $3E,$08,$08,$08,$08,$08,$08,$00,$42,$42,$42,$42,$42,$42,$3C,$00,
      $42,$42,$42,$24,$24,$18,$18,$00,$42,$42,$42,$5A,$5A,$66,$42,$00,
      $42,$42,$24,$18,$24,$42,$42,$00,$22,$22,$22,$1C,$08,$08,$08,$00,
      $7E,$02,$04,$18,$20,$40,$7E,$00,$08,$08,$08,$08,$FF,$08,$08,$08,
      $A0,$50,$A0,$50,$A0,$50,$A0,$50,$08,$08,$08,$08,$08,$08,$08,$08,
      $CC,$CC,$33,$33,$CC,$CC,$33,$33,$CC,$66,$33,$99,$CC,$66,$33,$99,
      $00,$00,$00,$00,$00,$00,$00,$00,$F0,$F0,$F0,$F0,$F0,$F0,$F0,$F0,
      $00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,
      $00,$00,$00,$00,$00,$00,$00,$FF,$80,$80,$80,$80,$80,$80,$80,$80,
      $AA,$55,$AA,$55,$AA,$55,$AA,$55,$01,$01,$01,$01,$01,$01,$01,$01,
      $00,$00,$00,$00,$AA,$55,$AA,$55,$99,$33,$66,$CC,$99,$33,$66,$CC,
      $03,$03,$03,$03,$03,$03,$03,$03,$08,$08,$08,$08,$0F,$08,$08,$08,
      $00,$00,$00,$00,$0F,$0F,$0F,$0F,$08,$08,$08,$08,$0F,$00,$00,$00,
      $00,$00,$00,$00,$F8,$08,$08,$08,$00,$00,$00,$00,$00,$00,$FF,$FF,
      $00,$00,$00,$00,$0F,$08,$08,$08,$08,$08,$08,$08,$FF,$00,$00,$00,
      $00,$00,$00,$00,$FF,$08,$08,$08,$08,$08,$08,$08,$F8,$08,$08,$08,
      $C0,$C0,$C0,$C0,$C0,$C0,$C0,$C0,$E0,$E0,$E0,$E0,$E0,$E0,$E0,$E0,
      $07,$07,$07,$07,$07,$07,$07,$07,$FF,$FF,$00,$00,$00,$00,$00,$00,
      $FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,
      $01,$02,$44,$48,$50,$60,$40,$00,$00,$00,$00,$00,$F0,$F0,$F0,$F0,
      $0F,$0F,$0F,$0F,$00,$00,$00,$00,$08,$08,$08,$08,$F8,$00,$00,$00,
      $F0,$F0,$F0,$F0,$00,$00,$00,$00,$F0,$F0,$F0,$F0,$0F,$0F,$0F,$0F,
      $E3,$DD,$B5,$A9,$B3,$DF,$E1,$FF,$FF,$FF,$C7,$FB,$C3,$BB,$C5,$FF,
      $BF,$BF,$A3,$9D,$BD,$9D,$A3,$FF,$FF,$FF,$C3,$BD,$BF,$BD,$C3,$FF,
      $FD,$FD,$C5,$B9,$BD,$B9,$C5,$FF,$FF,$FF,$C3,$BD,$81,$BF,$C3,$FF,
      $F3,$ED,$EF,$83,$EF,$EF,$EF,$FF,$FF,$FF,$C5,$B9,$B9,$C5,$FD,$C3,
      $BF,$BF,$A3,$9D,$BD,$BD,$BD,$FF,$F7,$FF,$E7,$F7,$F7,$F7,$E3,$FF,
      $FB,$FF,$F3,$FB,$FB,$FB,$BB,$C7,$BF,$BF,$BB,$B7,$AF,$97,$BB,$FF,
      $E7,$F7,$F7,$F7,$F7,$F7,$E3,$FF,$FF,$FF,$89,$B6,$B6,$B6,$B6,$FF,
      $FF,$FF,$A3,$9D,$BD,$BD,$BD,$FF,$FF,$FF,$C3,$BD,$BD,$BD,$C3,$FF,
      $FF,$FF,$A3,$9D,$9D,$A3,$BF,$BF,$FF,$FF,$C5,$B9,$B9,$C5,$FD,$FD,
      $FF,$FF,$A3,$9D,$BF,$BF,$BF,$FF,$FF,$FF,$C1,$BF,$C3,$FD,$83,$FF,
      $EF,$EF,$83,$EF,$EF,$ED,$F3,$FF,$FF,$FF,$BD,$BD,$BD,$B9,$C5,$FF,
      $FF,$FF,$BD,$BD,$BD,$DB,$E7,$FF,$FF,$FF,$BE,$B6,$B6,$B6,$C9,$FF,
      $FF,$FF,$BD,$DB,$E7,$DB,$BD,$FF,$FF,$FF,$BD,$BD,$B9,$C5,$FD,$C3,
      $FF,$FF,$81,$FB,$E7,$DF,$81,$FF,$C3,$DF,$DF,$DF,$DF,$DF,$C3,$FF,
      $F3,$EF,$EF,$C3,$EF,$8F,$91,$FF,$C3,$FB,$FB,$FB,$FB,$FB,$C3,$FF,
      $FF,$F7,$E3,$D5,$F7,$F7,$F7,$F7,$FF,$FF,$EF,$DF,$80,$DF,$EF,$FF,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$F7,$F7,$F7,$F7,$FF,$FF,$F7,$FF,
      $DB,$DB,$DB,$FF,$FF,$FF,$FF,$FF,$DB,$DB,$81,$DB,$81,$DB,$DB,$FF,
      $F7,$E1,$D7,$E3,$F5,$C3,$F7,$FF,$FF,$9D,$9B,$F7,$EF,$D9,$B9,$FF,
      $CF,$B7,$B7,$CF,$B5,$BB,$C5,$FF,$FB,$F7,$EF,$FF,$FF,$FF,$FF,$FF,
      $FB,$F7,$EF,$EF,$EF,$F7,$FB,$FF,$DF,$EF,$F7,$F7,$F7,$EF,$DF,$FF,
      $F7,$D5,$E3,$C1,$E3,$D5,$F7,$FF,$FF,$F7,$F7,$C1,$F7,$F7,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$F7,$F7,$EF,$FF,$FF,$FF,$81,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$E7,$E7,$FF,$FF,$FD,$FB,$F7,$EF,$DF,$BF,$FF,
      $C3,$BD,$B9,$A5,$9D,$BD,$C3,$FF,$F7,$E7,$D7,$F7,$F7,$F7,$C1,$FF,
      $C3,$BD,$FD,$F3,$CF,$BF,$81,$FF,$C3,$BD,$FD,$E3,$FD,$BD,$C3,$FF,
      $FB,$F3,$EB,$DB,$81,$FB,$FB,$FF,$81,$BF,$87,$FB,$FD,$BB,$C7,$FF,
      $E3,$DF,$BF,$83,$BD,$BD,$C3,$FF,$81,$BD,$FB,$F7,$EF,$EF,$EF,$FF,
      $C3,$BD,$BD,$C3,$BD,$BD,$C3,$FF,$C3,$BD,$BD,$C1,$FD,$FB,$C7,$FF,
      $FF,$FF,$F7,$FF,$FF,$F7,$FF,$FF,$FF,$FF,$F7,$FF,$FF,$F7,$F7,$EF,
      $F1,$E7,$CF,$9F,$CF,$E7,$F1,$FF,$FF,$FF,$81,$FF,$81,$FF,$FF,$FF,
      $8F,$E7,$F3,$F9,$F3,$E7,$8F,$FF,$C3,$BD,$FD,$F3,$EF,$FF,$EF,$FF,
      $FF,$FF,$FF,$FF,$00,$FF,$FF,$FF,$E7,$DB,$BD,$81,$BD,$BD,$BD,$FF,
      $83,$DD,$DD,$C3,$DD,$DD,$83,$FF,$E3,$DD,$BF,$BF,$BF,$DD,$E3,$FF,
      $87,$DB,$DD,$DD,$DD,$DB,$87,$FF,$81,$BF,$BF,$87,$BF,$BF,$81,$FF,
      $81,$BF,$BF,$87,$BF,$BF,$BF,$FF,$E3,$DD,$BF,$B1,$BD,$DD,$E3,$FF,
      $BD,$BD,$BD,$81,$BD,$BD,$BD,$FF,$E3,$F7,$F7,$F7,$F7,$F7,$E3,$FF,
      $F1,$FB,$FB,$FB,$FB,$BB,$C7,$FF,$BD,$BB,$B7,$8F,$B7,$BB,$BD,$FF,
      $BF,$BF,$BF,$BF,$BF,$BF,$81,$FF,$BD,$99,$A5,$A5,$BD,$BD,$BD,$FF,
      $BD,$9D,$AD,$B5,$B9,$BD,$BD,$FF,$E7,$DB,$BD,$BD,$BD,$DB,$E7,$FF,
      $83,$BD,$BD,$83,$BF,$BF,$BF,$FF,$E7,$DB,$BD,$BD,$B5,$DB,$E5,$FF,
      $83,$BD,$BD,$83,$B7,$BB,$BD,$FF,$C3,$BD,$BF,$C3,$FD,$BD,$C3,$FF,
      $C1,$F7,$F7,$F7,$F7,$F7,$F7,$FF,$BD,$BD,$BD,$BD,$BD,$BD,$C3,$FF,
      $BD,$BD,$BD,$DB,$DB,$E7,$E7,$FF,$BD,$BD,$BD,$A5,$A5,$99,$BD,$FF,
      $BD,$BD,$DB,$E7,$DB,$BD,$BD,$FF,$DD,$DD,$DD,$E3,$F7,$F7,$F7,$FF,
      $81,$FD,$FB,$E7,$DF,$BF,$81,$FF,$F7,$F7,$F7,$F7,$00,$F7,$F7,$F7,
      $5F,$AF,$5F,$AF,$5F,$AF,$5F,$AF,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$F7,
      $33,$33,$CC,$CC,$33,$33,$CC,$CC,$33,$99,$CC,$66,$33,$99,$CC,$66,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$0F,$0F,$0F,$0F,$0F,$0F,$0F,$0F,
      $FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$7F,$7F,$7F,$7F,$7F,$7F,$7F,$7F,
      $55,$AA,$55,$AA,$55,$AA,$55,$AA,$FE,$FE,$FE,$FE,$FE,$FE,$FE,$FE,
      $FF,$FF,$FF,$FF,$55,$AA,$55,$AA,$66,$CC,$99,$33,$66,$CC,$99,$33,
      $FC,$FC,$FC,$FC,$FC,$FC,$FC,$FC,$F7,$F7,$F7,$F7,$F0,$F7,$F7,$F7,
      $FF,$FF,$FF,$FF,$F0,$F0,$F0,$F0,$F7,$F7,$F7,$F7,$F0,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$07,$F7,$F7,$F7,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,
      $FF,$FF,$FF,$FF,$F0,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$00,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$00,$F7,$F7,$F7,$F7,$F7,$F7,$F7,$07,$F7,$F7,$F7,
      $3F,$3F,$3F,$3F,$3F,$3F,$3F,$3F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,$1F,
      $F8,$F8,$F8,$F8,$F8,$F8,$F8,$F8,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,
      $00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,
      $FE,$FD,$BB,$B7,$AF,$9F,$BF,$FF,$FF,$FF,$FF,$FF,$0F,$0F,$0F,$0F,
      $F0,$F0,$F0,$F0,$FF,$FF,$FF,$FF,$F7,$F7,$F7,$F7,$07,$FF,$FF,$FF,
      $0F,$0F,$0F,$0F,$FF,$FF,$FF,$FF,$0F,$0F,$0F,$0F,$F0,$F0,$F0,$F0
         );

// BIN data of kernal-----------------------------------------------------------
// length = 8192

const
   Bin_kernal_len = 8192;
var
   Bin_kernal : array [0..8191] of byte = (
      $0F,$DC,$A5,$61,$C9,$88,$90,$03,$20,$D4,$DA,$20,$CC,$DC,$A5,$07,
      $18,$69,$81,$F0,$F3,$38,$E9,$01,$48,$A2,$05,$B5,$69,$B4,$61,$95,
      $61,$94,$69,$CA,$10,$F5,$A5,$56,$85,$70,$20,$53,$D8,$20,$B4,$DF,
      $A9,$C4,$A0,$DF,$20,$56,$E0,$A9,$00,$85,$6F,$68,$20,$B9,$DA,$60,
      $85,$71,$84,$72,$20,$CA,$DB,$A9,$57,$20,$28,$DA,$20,$5A,$E0,$A9,
      $57,$A0,$00,$4C,$28,$DA,$85,$71,$84,$72,$20,$C7,$DB,$B1,$71,$85,
      $67,$A4,$71,$C8,$98,$D0,$02,$E6,$72,$85,$71,$A4,$72,$20,$28,$DA,
      $A5,$71,$A4,$72,$18,$69,$05,$90,$01,$C8,$85,$71,$84,$72,$20,$67,
      $D8,$A9,$5C,$A0,$00,$C6,$67,$D0,$E4,$60,$98,$35,$44,$7A,$00,$68,
      $28,$B1,$46,$00,$20,$2B,$DC,$30,$37,$D0,$20,$20,$F3,$FF,$86,$22,
      $84,$23,$A0,$04,$B1,$22,$85,$62,$C8,$B1,$22,$85,$64,$A0,$08,$B1,
      $22,$85,$63,$C8,$B1,$22,$85,$65,$4C,$E0,$E0,$A9,$8B,$A0,$00,$20,
      $A2,$DB,$A9,$8A,$A0,$E0,$20,$28,$DA,$A9,$8F,$A0,$E0,$20,$67,$D8,
      $A6,$65,$A5,$62,$85,$65,$86,$62,$A6,$63,$A5,$64,$85,$63,$86,$64,
      $A9,$00,$85,$66,$A5,$61,$85,$70,$A9,$80,$85,$61,$20,$D7,$D8,$A2,
      $8B,$A0,$00,$4C,$D4,$DB,$C9,$F0,$D0,$07,$84,$38,$86,$37,$4C,$63,
      $C6,$AA,$D0,$02,$A2,$1E,$4C,$37,$C4,$20,$D2,$FF,$B0,$E8,$60,$20,
      $CF,$FF,$B0,$E2,$60,$20,$C9,$FF,$B0,$DC,$60,$20,$C6,$FF,$B0,$D6,
      $60,$20,$E4,$FF,$B0,$D0,$60,$20,$8A,$CD,$20,$F7,$D7,$A9,$E1,$48,
      $A9,$43,$48,$AD,$0F,$03,$48,$AD,$0C,$03,$AE,$0D,$03,$AC,$0E,$03,
      $28,$6C,$14,$00,$08,$8D,$0C,$03,$8E,$0D,$03,$8C,$0E,$03,$68,$8D,
      $0F,$03,$60,$20,$D1,$E1,$A6,$2D,$A4,$2E,$A9,$2B,$20,$D8,$FF,$B0,
      $95,$60,$A9,$01,$2C,$A9,$00,$85,$0A,$20,$D1,$E1,$A5,$0A,$A6,$2B,
      $A4,$2C,$20,$D5,$FF,$B0,$57,$A5,$0A,$F0,$1A,$A2,$1C,$20,$B7,$FF,
      $29,$10,$F0,$03,$4C,$37,$C4,$A5,$7A,$C9,$02,$F0,$07,$A9,$64,$A0,
      $C3,$4C,$1E,$CB,$60,$20,$B7,$FF,$29,$BF,$F0,$05,$A2,$1D,$4C,$37,
      $C4,$A5,$7B,$C9,$02,$D0,$0E,$86,$2D,$84,$2E,$A9,$76,$A0,$C3,$20,
      $1E,$CB,$4C,$2A,$C5,$20,$8E,$C6,$4C,$76,$E4,$20,$16,$E2,$20,$C0,
      $FF,$B0,$0B,$60,$20,$16,$E2,$A5,$49,$20,$C3,$FF,$90,$C6,$4C,$F6,
      $E0,$A9,$00,$20,$BD,$FF,$A2,$01,$A0,$00,$20,$BA,$FF,$20,$03,$E2,
      $20,$54,$E2,$20,$03,$E2,$20,$FD,$E1,$A0,$00,$86,$49,$20,$BA,$FF,
      $20,$03,$E2,$20,$FD,$E1,$8A,$A8,$A6,$49,$4C,$BA,$FF,$20,$0B,$E2,
      $4C,$9E,$D7,$20,$79,$00,$D0,$02,$68,$68,$60,$20,$FD,$CE,$20,$79,
      $00,$D0,$F7,$4C,$08,$CF,$A9,$00,$20,$BD,$FF,$20,$0E,$E2,$20,$9E,
      $D7,$86,$49,$8A,$A2,$01,$A0,$00,$20,$BA,$FF,$20,$03,$E2,$20,$FD,
      $E1,$86,$4A,$A0,$00,$A5,$49,$E0,$03,$90,$01,$88,$20,$BA,$FF,$20,
      $03,$E2,$20,$FD,$E1,$8A,$A8,$A6,$4A,$A5,$49,$20,$BA,$FF,$20,$03,
      $E2,$20,$0B,$E2,$20,$9E,$CD,$20,$A3,$D6,$A6,$22,$A4,$23,$4C,$BD,
      $FF,$A9,$DD,$A0,$E2,$20,$67,$D8,$20,$0C,$DC,$A9,$E2,$A0,$E2,$A6,
      $6E,$20,$07,$DB,$20,$0C,$DC,$20,$CC,$DC,$A9,$00,$85,$6F,$20,$53,
      $D8,$A9,$E7,$A0,$E2,$20,$50,$D8,$A5,$66,$48,$10,$0D,$20,$49,$D8,
      $A5,$66,$30,$09,$A5,$12,$49,$FF,$85,$12,$20,$B4,$DF,$A9,$E7,$A0,
      $E2,$20,$67,$D8,$68,$10,$03,$20,$B4,$DF,$A9,$EC,$A0,$E2,$4C,$40,
      $E0,$20,$CA,$DB,$A9,$00,$85,$12,$20,$68,$E2,$A2,$4E,$A0,$00,$20,
      $F3,$E0,$A9,$57,$A0,$00,$20,$A2,$DB,$A9,$00,$85,$66,$A5,$12,$20,
      $D9,$E2,$A9,$4E,$A0,$00,$4C,$0F,$DB,$48,$4C,$9A,$E2,$81,$49,$0F,
      $DA,$A2,$83,$49,$0F,$DA,$A2,$7F,$00,$00,$00,$00,$05,$84,$E6,$1A,
      $2D,$1B,$86,$28,$07,$FB,$F8,$87,$99,$68,$89,$01,$87,$23,$35,$DF,
      $E1,$86,$A5,$5D,$E7,$28,$83,$49,$0F,$DA,$A2,$A5,$66,$48,$10,$03,
      $20,$B4,$DF,$A5,$61,$48,$C9,$81,$90,$07,$A9,$BC,$A0,$D9,$20,$0F,
      $DB,$A9,$3B,$A0,$E3,$20,$40,$E0,$68,$C9,$81,$90,$07,$A9,$DD,$A0,
      $E2,$20,$50,$D8,$68,$10,$03,$4C,$B4,$DF,$60,$0B,$76,$B3,$83,$BD,
      $D3,$79,$1E,$F4,$A6,$F5,$7B,$83,$FC,$B0,$10,$7C,$0C,$1F,$67,$CA,
      $7C,$DE,$53,$CB,$C1,$7D,$14,$64,$70,$4C,$7D,$B7,$EA,$51,$7A,$7D,
      $63,$30,$88,$7E,$7E,$92,$44,$99,$3A,$7E,$4C,$CC,$91,$C7,$7F,$AA,
      $AA,$AA,$13,$81,$00,$00,$00,$00,$20,$5B,$E4,$20,$A4,$E3,$20,$04,
      $E4,$A2,$FB,$9A,$4C,$74,$C4,$E6,$7A,$D0,$02,$E6,$7B,$AD,$60,$EA,
      $C9,$3A,$B0,$0A,$C9,$20,$F0,$EF,$38,$E9,$30,$38,$E9,$D0,$60,$80,
      $4F,$C7,$52,$58,$A9,$4C,$85,$54,$85,$00,$A9,$48,$A0,$D2,$85,$01,
      $84,$02,$A9,$91,$A0,$D3,$85,$05,$84,$06,$A9,$AA,$A0,$D1,$85,$03,
      $84,$04,$A2,$1C,$BD,$87,$E3,$95,$73,$CA,$10,$F8,$A9,$03,$85,$53,
      $A9,$00,$85,$68,$85,$13,$85,$18,$A2,$01,$8E,$FD,$01,$8E,$FC,$01,
      $A2,$19,$86,$16,$38,$20,$9C,$FF,$86,$2B,$84,$2C,$38,$20,$99,$FF,
      $86,$37,$84,$38,$86,$33,$84,$34,$A0,$00,$98,$91,$2B,$E6,$2B,$D0,
      $02,$E6,$2C,$60,$A5,$2B,$A4,$2C,$20,$08,$C4,$A9,$36,$A0,$E4,$20,
      $1E,$CB,$A5,$37,$38,$E5,$2B,$AA,$A5,$38,$E5,$2C,$20,$CD,$DD,$A9,
      $29,$A0,$E4,$20,$1E,$CB,$4C,$44,$C6,$20,$42,$59,$54,$45,$53,$20,
      $46,$52,$45,$45,$0D,$00,$93,$2A,$2A,$2A,$2A,$20,$43,$42,$4D,$20,
      $42,$41,$53,$49,$43,$20,$56,$32,$20,$2A,$2A,$2A,$2A,$0D,$00,$3A,
      $C4,$83,$C4,$7C,$C5,$1A,$C7,$E4,$C7,$86,$CE,$A2,$0B,$BD,$4F,$E4,
      $9D,$00,$03,$CA,$10,$F7,$60,$20,$CC,$FF,$A9,$00,$85,$13,$20,$7A,
      $C6,$58,$4C,$74,$C4,$E8,$20,$33,$C5,$4C,$77,$C6,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
      $AD,$2C,$91,$29,$DF,$8D,$2C,$91,$60,$AD,$2C,$91,$09,$20,$8D,$2C,
      $91,$60,$AD,$1F,$91,$CD,$1F,$91,$D0,$F8,$4A,$60,$A6,$B9,$4C,$47,
      $F6,$8A,$D0,$08,$A5,$C3,$85,$AE,$A5,$C4,$85,$AF,$4C,$6A,$F6,$20,
      $E3,$F8,$90,$03,$68,$A9,$00,$4C,$9E,$F3,$FF,$FF,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
      $A2,$10,$A0,$91,$60,$A2,$16,$A0,$17,$60,$B0,$07,$86,$D6,$84,$D3,
      $20,$87,$E5,$A6,$D6,$A4,$D3,$60,$20,$BB,$E5,$AD,$88,$02,$29,$FD,
      $0A,$0A,$09,$80,$8D,$05,$90,$AD,$88,$02,$29,$02,$F0,$08,$A9,$80,
      $0D,$02,$90,$8D,$02,$90,$A9,$00,$8D,$91,$02,$85,$CF,$A9,$DC,$8D,
      $8F,$02,$A9,$EB,$8D,$90,$02,$A9,$0A,$8D,$89,$02,$8D,$8C,$02,$A9,
      $06,$8D,$86,$02,$A9,$04,$8D,$8B,$02,$A9,$0C,$85,$CD,$85,$CC,$AD,
      $88,$02,$09,$80,$A8,$A9,$00,$AA,$94,$D9,$18,$69,$16,$90,$01,$C8,
      $E8,$E0,$18,$D0,$F3,$A9,$FF,$95,$D9,$A2,$16,$20,$8D,$EA,$CA,$10,
      $FA,$A0,$00,$84,$D3,$84,$D6,$A6,$D6,$A5,$D3,$B4,$D9,$30,$08,$18,
      $69,$16,$85,$D3,$CA,$10,$F4,$B5,$D9,$29,$03,$0D,$88,$02,$85,$D2,
      $BD,$FD,$ED,$85,$D1,$A9,$15,$E8,$B4,$D9,$30,$06,$18,$69,$16,$E8,
      $10,$F6,$85,$D5,$60,$20,$BB,$E5,$4C,$81,$E5,$A9,$03,$85,$9A,$A9,
      $00,$85,$99,$A2,$10,$BD,$E3,$ED,$9D,$FF,$8F,$CA,$D0,$F7,$60,$AC,
      $77,$02,$A2,$00,$BD,$78,$02,$9D,$77,$02,$E8,$E4,$C6,$D0,$F5,$C6,
      $C6,$98,$58,$18,$60,$20,$42,$E7,$A5,$C6,$85,$CC,$8D,$92,$02,$F0,
      $F7,$78,$A5,$CF,$F0,$0C,$A5,$CE,$AE,$87,$02,$A0,$00,$84,$CF,$20,
      $A1,$EA,$20,$CF,$E5,$C9,$83,$D0,$10,$A2,$09,$78,$86,$C6,$BD,$F3,
      $ED,$9D,$76,$02,$CA,$D0,$F7,$F0,$CF,$C9,$0D,$D0,$C8,$A4,$D5,$84,
      $D0,$B1,$D1,$C9,$20,$D0,$03,$88,$D0,$F7,$C8,$84,$C8,$A0,$00,$8C,
      $92,$02,$84,$D3,$84,$D4,$A5,$C9,$30,$1D,$A6,$D6,$20,$19,$E7,$E4,
      $C9,$D0,$14,$D0,$12,$A5,$CA,$85,$D3,$C5,$C8,$90,$0A,$B0,$42,$98,
      $48,$8A,$48,$A5,$D0,$F0,$91,$A4,$D3,$B1,$D1,$EA,$EA,$EA,$EA,$EA,
      $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,
      $EA,$EA,$85,$D7,$29,$3F,$06,$D7,$24,$D7,$10,$02,$09,$80,$90,$04,
      $A6,$D4,$D0,$04,$70,$02,$09,$40,$E6,$D3,$20,$B8,$E6,$C4,$C8,$D0,
      $17,$A9,$00,$85,$D0,$A9,$0D,$A6,$99,$E0,$03,$F0,$06,$A6,$9A,$E0,
      $03,$F0,$03,$20,$42,$E7,$A9,$0D,$85,$D7,$68,$AA,$68,$A8,$A5,$D7,
      $C9,$DE,$D0,$02,$A9,$FF,$18,$60,$C9,$22,$D0,$08,$A5,$D4,$49,$01,
      $85,$D4,$A9,$22,$60,$09,$40,$A6,$C7,$F0,$02,$09,$80,$A6,$D8,$F0,
      $02,$C6,$D8,$AE,$86,$02,$20,$A1,$EA,$20,$EA,$E6,$68,$A8,$A5,$D8,
      $F0,$02,$46,$D4,$68,$AA,$68,$18,$58,$60,$20,$FA,$E8,$E6,$D3,$A5,
      $D5,$C5,$D3,$B0,$37,$C9,$57,$F0,$2A,$AD,$92,$02,$F0,$03,$4C,$F0,
      $E9,$A6,$D6,$E0,$17,$90,$07,$20,$75,$E9,$C6,$D6,$A6,$D6,$16,$D9,
      $56,$D9,$4C,$5B,$ED,$69,$16,$85,$D5,$B5,$D9,$30,$03,$CA,$D0,$F9,
      $4C,$7E,$EA,$C6,$D6,$20,$C3,$E8,$A9,$00,$85,$D3,$60,$A6,$D6,$D0,
      $06,$86,$D3,$68,$68,$D0,$A5,$CA,$86,$D6,$20,$87,$E5,$A4,$D5,$84,
      $D3,$60,$48,$85,$D7,$8A,$48,$98,$48,$A9,$00,$85,$D0,$A4,$D3,$A5,
      $D7,$10,$03,$4C,$00,$E8,$C9,$0D,$D0,$03,$4C,$D8,$E8,$C9,$20,$90,
      $10,$C9,$60,$90,$04,$29,$DF,$D0,$02,$29,$3F,$20,$B8,$E6,$4C,$C7,
      $E6,$A6,$D8,$F0,$03,$4C,$CB,$E6,$C9,$14,$D0,$2E,$98,$D0,$06,$20,
      $2D,$E7,$4C,$9F,$E7,$20,$E8,$E8,$88,$84,$D3,$20,$B2,$EA,$C8,$B1,
      $D1,$88,$91,$D1,$C8,$B1,$F3,$88,$91,$F3,$C8,$C4,$D5,$D0,$EF,$A9,
      $20,$91,$D1,$AD,$86,$02,$91,$F3,$10,$4D,$A6,$D4,$F0,$03,$4C,$CB,
      $E6,$C9,$12,$D0,$02,$85,$C7,$C9,$13,$D0,$03,$20,$81,$E5,$C9,$1D,
      $D0,$17,$C8,$20,$FA,$E8,$84,$D3,$88,$C4,$D5,$90,$09,$C6,$D6,$20,
      $C3,$E8,$A0,$00,$84,$D3,$4C,$DC,$E6,$C9,$11,$D0,$1D,$18,$98,$69,
      $16,$A8,$E6,$D6,$C5,$D5,$90,$EC,$F0,$EA,$C6,$D6,$E9,$16,$90,$04,
      $85,$D3,$D0,$F8,$20,$C3,$E8,$4C,$DC,$E6,$20,$12,$E9,$4C,$21,$ED,
      $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,
      $EA,$EA,$EA,$EA,$EA,$29,$7F,$C9,$7F,$D0,$02,$A9,$5E,$EA,$EA,$EA,
      $EA,$EA,$EA,$C9,$20,$90,$03,$4C,$C5,$E6,$C9,$0D,$D0,$03,$4C,$D8,
      $E8,$A6,$D4,$D0,$3F,$C9,$14,$D0,$37,$A4,$D5,$B1,$D1,$C9,$20,$D0,
      $04,$C4,$D3,$D0,$07,$C0,$57,$F0,$24,$20,$EE,$E9,$A4,$D5,$20,$B2,
      $EA,$88,$B1,$D1,$C8,$91,$D1,$88,$B1,$F3,$C8,$91,$F3,$88,$C4,$D3,
      $D0,$EF,$A9,$20,$91,$D1,$AD,$86,$02,$91,$F3,$E6,$D8,$4C,$DC,$E6,
      $A6,$D8,$F0,$05,$09,$40,$4C,$CB,$E6,$C9,$11,$D0,$16,$A6,$D6,$F0,
      $37,$C6,$D6,$A5,$D3,$38,$E9,$16,$90,$04,$85,$D3,$10,$2A,$20,$87,
      $E5,$D0,$25,$C9,$12,$D0,$04,$A9,$00,$85,$C7,$C9,$1D,$D0,$12,$98,
      $F0,$09,$20,$E8,$E8,$88,$84,$D3,$4C,$DC,$E6,$20,$2D,$E7,$4C,$DC,
      $E6,$C9,$13,$D0,$06,$20,$5F,$E5,$4C,$DC,$E6,$09,$80,$20,$12,$E9,
      $4C,$30,$ED,$46,$C9,$A6,$D6,$E8,$E0,$17,$D0,$03,$20,$75,$E9,$B5,
      $D9,$10,$F4,$86,$D6,$4C,$87,$E5,$A2,$00,$86,$D8,$86,$C7,$86,$D4,
      $86,$D3,$20,$C3,$E8,$4C,$DC,$E6,$A2,$04,$A9,$00,$C5,$D3,$F0,$07,
      $18,$69,$16,$CA,$D0,$F6,$60,$C6,$D6,$60,$A2,$04,$A9,$15,$C5,$D3,
      $F0,$07,$18,$69,$16,$CA,$D0,$F6,$60,$A6,$D6,$E0,$17,$F0,$02,$E6,
      $D6,$60,$A2,$07,$DD,$21,$E9,$F0,$04,$CA,$10,$F8,$60,$8E,$86,$02,
      $60,$90,$05,$1C,$9F,$9C,$1E,$1F,$9E,$EF,$A1,$DF,$A6,$E1,$B1,$E2,
      $B2,$E3,$B3,$E4,$B4,$E5,$B5,$E6,$B6,$E7,$B7,$E8,$B8,$E9,$B9,$FA,
      $BA,$FB,$BB,$FC,$BC,$EC,$BD,$FE,$BE,$84,$BF,$F7,$C0,$F8,$DB,$F9,
      $DD,$EA,$DE,$5E,$E0,$5B,$E1,$5D,$E2,$40,$B0,$61,$B1,$78,$DB,$79,
      $DD,$66,$B6,$77,$C0,$70,$F0,$71,$F1,$72,$F2,$73,$F3,$74,$F4,$75,
      $F5,$76,$F6,$7D,$FD,$A5,$AC,$48,$A5,$AD,$48,$A5,$AE,$48,$A5,$AF,
      $48,$A2,$FF,$C6,$D6,$C6,$C9,$C6,$F2,$E8,$20,$7E,$EA,$E0,$16,$B0,
      $0C,$BD,$FE,$ED,$85,$AC,$B5,$DA,$20,$56,$EA,$30,$EC,$20,$8D,$EA,
      $A2,$00,$B5,$D9,$29,$7F,$B4,$DA,$10,$02,$09,$80,$95,$D9,$E8,$E0,
      $16,$D0,$EF,$A5,$EF,$09,$80,$85,$EF,$A5,$D9,$10,$C4,$E6,$D6,$E6,
      $F2,$A9,$FB,$8D,$20,$91,$AD,$21,$91,$C9,$FE,$08,$A9,$F7,$8D,$20,
      $91,$28,$D0,$0B,$A0,$00,$EA,$CA,$D0,$FC,$88,$D0,$F9,$84,$C6,$A6,
      $D6,$68,$85,$AF,$68,$85,$AE,$68,$85,$AD,$68,$85,$AC,$60,$A6,$D6,
      $E8,$B5,$D9,$10,$FB,$86,$F2,$E0,$16,$F0,$0D,$90,$0B,$20,$75,$E9,
      $A6,$F2,$CA,$C6,$D6,$4C,$0E,$E7,$A5,$AC,$48,$A5,$AD,$48,$A5,$AE,
      $48,$A5,$AF,$48,$A2,$17,$CA,$20,$7E,$EA,$E4,$F2,$90,$0E,$F0,$0C,
      $BD,$FC,$ED,$85,$AC,$B5,$D8,$20,$56,$EA,$30,$EA,$20,$8D,$EA,$A2,
      $15,$E4,$F2,$90,$0F,$B5,$DA,$29,$7F,$B4,$D9,$10,$02,$09,$80,$95,
      $DA,$CA,$D0,$ED,$A6,$F2,$20,$0E,$E7,$68,$85,$AF,$68,$85,$AE,$68,
      $85,$AD,$68,$85,$AC,$60,$29,$03,$0D,$88,$02,$85,$AD,$20,$6E,$EA,
      $A0,$15,$B1,$AC,$91,$D1,$B1,$AE,$91,$F3,$88,$10,$F5,$60,$20,$B2,
      $EA,$A5,$AC,$85,$AE,$A5,$AD,$29,$03,$09,$94,$85,$AF,$60,$BD,$FD,
      $ED,$85,$D1,$B5,$D9,$29,$03,$0D,$88,$02,$85,$D2,$60,$A0,$15,$20,
      $7E,$EA,$20,$B2,$EA,$A9,$20,$91,$D1,$A9,$01,$91,$F3,$88,$10,$F5,
      $60,$A8,$A9,$02,$85,$CD,$20,$B2,$EA,$98,$A4,$D3,$91,$D1,$8A,$91,
      $F3,$60,$A5,$D1,$85,$F3,$A5,$D2,$29,$03,$09,$94,$85,$F4,$60,$20,
      $EA,$FF,$A5,$CC,$D0,$29,$C6,$CD,$D0,$25,$A9,$14,$85,$CD,$A4,$D3,
      $46,$CF,$AE,$87,$02,$B1,$D1,$B0,$11,$E6,$CF,$85,$CE,$20,$B2,$EA,
      $B1,$F3,$8D,$87,$02,$AE,$86,$02,$A5,$CE,$49,$80,$20,$AA,$EA,$AD,
      $1F,$91,$29,$40,$F0,$0B,$A0,$00,$84,$C0,$AD,$1C,$91,$09,$02,$D0,
      $09,$A5,$C0,$D0,$0D,$AD,$1C,$91,$29,$FD,$2C,$1E,$91,$70,$03,$8D,
      $1C,$91,$20,$1E,$EB,$2C,$24,$91,$68,$A8,$68,$AA,$68,$40,$A9,$00,
      $8D,$8D,$02,$A0,$40,$84,$CB,$8D,$20,$91,$AE,$21,$91,$E0,$FF,$F0,
      $5E,$A9,$FE,$8D,$20,$91,$A0,$00,$A9,$5E,$85,$F5,$A9,$EC,$85,$F6,
      $A2,$08,$AD,$21,$91,$CD,$21,$91,$D0,$F6,$4A,$B0,$16,$48,$B1,$F5,
      $C9,$05,$B0,$0C,$C9,$03,$F0,$08,$0D,$8D,$02,$8D,$8D,$02,$10,$02,
      $84,$CB,$68,$C8,$C0,$41,$B0,$09,$CA,$D0,$DF,$38,$2E,$20,$91,$D0,
      $CF,$6C,$8F,$02,$A4,$CB,$B1,$F5,$AA,$C4,$C5,$F0,$07,$A0,$10,$8C,
      $8C,$02,$D0,$36,$29,$7F,$2C,$8A,$02,$30,$16,$70,$49,$C9,$7F,$F0,
      $29,$C9,$14,$F0,$0C,$C9,$20,$F0,$08,$C9,$1D,$F0,$04,$C9,$11,$D0,
      $35,$AC,$8C,$02,$F0,$05,$CE,$8C,$02,$D0,$2B,$CE,$8B,$02,$D0,$26,
      $A0,$04,$8C,$8B,$02,$A4,$C6,$88,$10,$1C,$A4,$CB,$84,$C5,$AC,$8D,
      $02,$8C,$8E,$02,$E0,$FF,$F0,$0E,$8A,$A6,$C6,$EC,$89,$02,$B0,$06,
      $9D,$77,$02,$E8,$86,$C6,$A9,$F7,$8D,$20,$91,$60,$AD,$8D,$02,$C9,
      $03,$D0,$2C,$CD,$8E,$02,$F0,$EE,$AD,$91,$02,$30,$56,$EA,$EA,$EA,
      $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,
      $AD,$05,$90,$49,$02,$8D,$05,$90,$EA,$EA,$EA,$EA,$4C,$43,$EC,$0A,
      $C9,$08,$90,$04,$A9,$06,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,
      $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,
      $EA,$EA,$EA,$EA,$EA,$EA,$EA,$EA,$AA,$BD,$46,$EC,$85,$F5,$BD,$47,
      $EC,$85,$F6,$4C,$74,$EB,$5E,$EC,$9F,$EC,$E0,$EC,$A3,$ED,$5E,$EC,
      $9F,$EC,$69,$ED,$A3,$ED,$21,$ED,$69,$ED,$69,$ED,$A3,$ED,$31,$33,
      $35,$37,$39,$2B,$5C,$14,$5F,$57,$52,$59,$49,$50,$2A,$0D,$04,$41,
      $44,$47,$4A,$4C,$3B,$1D,$03,$01,$58,$56,$4E,$2C,$2F,$11,$20,$5A,
      $43,$42,$4D,$2E,$01,$85,$02,$53,$46,$48,$4B,$3A,$3D,$86,$51,$45,
      $54,$55,$4F,$40,$5E,$87,$32,$34,$36,$38,$30,$2D,$13,$88,$FF,$21,
      $23,$25,$27,$29,$DB,$A9,$94,$5F,$D7,$D2,$D9,$C9,$D0,$C0,$8D,$04,
      $C1,$C4,$C7,$CA,$CC,$5D,$9D,$83,$01,$D8,$D6,$CE,$3C,$3F,$91,$A0,
      $DA,$C3,$C2,$CD,$3E,$01,$89,$02,$D3,$C6,$C8,$CB,$5B,$3D,$8A,$D1,
      $C5,$D4,$D5,$CF,$BA,$DE,$8B,$22,$24,$26,$28,$30,$DD,$93,$8C,$FF,
      $21,$23,$25,$27,$29,$A6,$A8,$94,$5F,$B3,$B2,$B7,$A2,$AF,$DF,$8D,
      $04,$B0,$AC,$A5,$B5,$B6,$5D,$9D,$83,$01,$BD,$BE,$AA,$3C,$3F,$91,
      $A0,$AD,$BC,$BF,$A7,$3E,$01,$89,$02,$AE,$BB,$B4,$A1,$5B,$3D,$8A,
      $AB,$B1,$A3,$B8,$B9,$A4,$DE,$8B,$22,$24,$26,$28,$30,$DC,$93,$8C,
      $FF,$C9,$0E,$D0,$0B,$A9,$02,$0D,$05,$90,$8D,$05,$90,$4C,$DC,$E6,
      $C9,$8E,$D0,$0B,$A9,$FD,$2D,$05,$90,$8D,$05,$90,$4C,$DC,$E6,$C9,
      $08,$D0,$0A,$A9,$80,$0D,$91,$02,$8D,$91,$02,$30,$EF,$C9,$09,$D0,
      $EB,$A9,$7F,$2D,$91,$02,$8D,$91,$02,$10,$E1,$E8,$B5,$D9,$09,$80,
      $95,$D9,$CA,$A5,$D5,$18,$4C,$15,$E7,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
      $FF,$FF,$04,$FF,$FF,$FF,$FF,$FF,$E2,$9D,$83,$01,$FF,$FF,$FF,$FF,
      $FF,$91,$A0,$FF,$FF,$FF,$FF,$EE,$01,$89,$02,$FF,$FF,$FF,$FF,$E1,
      $FD,$8A,$FF,$FF,$FF,$FF,$FF,$B0,$E0,$8B,$F2,$F4,$F6,$FF,$F0,$ED,
      $93,$8C,$FF,$90,$1C,$9C,$1F,$12,$FF,$FF,$FF,$06,$FF,$12,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$05,$9F,$1E,$9E,$92,
      $FF,$FF,$FF,$FF,$0C,$26,$16,$2E,$00,$C0,$00,$00,$00,$00,$00,$00,
      $00,$00,$00,$1B,$4C,$4F,$41,$44,$0D,$52,$55,$4E,$0D,$00,$16,$2C,
      $42,$58,$6E,$84,$9A,$B0,$C6,$DC,$F2,$08,$1E,$34,$4A,$60,$76,$8C,
      $A2,$B8,$CE,$E4,$09,$40,$2C,$09,$20,$20,$60,$F1,$48,$24,$94,$10,
      $0A,$38,$66,$A3,$20,$49,$EE,$46,$94,$46,$A3,$68,$85,$95,$20,$A0,
      $E4,$C9,$3F,$D0,$03,$20,$84,$EF,$AD,$1F,$91,$09,$80,$8D,$1F,$91,
      $20,$8D,$EF,$20,$A0,$E4,$20,$96,$EF,$78,$20,$A0,$E4,$20,$B2,$E4,
      $4A,$B0,$61,$20,$84,$EF,$24,$A3,$10,$0C,$20,$B2,$E4,$4A,$90,$FA,
      $20,$B2,$E4,$4A,$B0,$FA,$20,$B2,$E4,$4A,$90,$FA,$20,$8D,$EF,$A9,
      $08,$85,$A5,$AD,$1F,$91,$CD,$1F,$91,$D0,$F8,$4A,$4A,$90,$38,$66,
      $95,$B0,$05,$20,$A9,$E4,$D0,$03,$20,$A0,$E4,$20,$84,$EF,$EA,$EA,
      $EA,$EA,$AD,$2C,$91,$29,$DF,$09,$02,$8D,$2C,$91,$C6,$A5,$D0,$D3,
      $A9,$04,$8D,$29,$91,$AD,$2D,$91,$29,$20,$D0,$0B,$20,$B2,$E4,$4A,
      $B0,$F3,$58,$60,$A9,$80,$2C,$A9,$03,$20,$6A,$FE,$58,$18,$90,$49,
      $85,$95,$20,$40,$EE,$AD,$1F,$91,$29,$7F,$8D,$1F,$91,$60,$85,$95,
      $20,$40,$EE,$78,$20,$A9,$E4,$20,$C5,$EE,$20,$84,$EF,$20,$B2,$E4,
      $B0,$FB,$58,$60,$24,$94,$30,$05,$38,$66,$94,$D0,$05,$48,$20,$49,
      $EE,$68,$85,$95,$18,$60,$20,$8D,$EF,$AD,$1F,$91,$09,$80,$8D,$1F,
      $91,$A9,$5F,$2C,$A9,$3F,$20,$1C,$EE,$20,$C5,$EE,$8A,$A2,$0B,$CA,
      $D0,$FD,$AA,$20,$84,$EF,$4C,$A0,$E4,$78,$A9,$00,$85,$A5,$20,$84,
      $EF,$20,$B2,$E4,$90,$FB,$20,$A0,$E4,$A9,$01,$8D,$29,$91,$AD,$2D,
      $91,$29,$20,$D0,$07,$20,$B2,$E4,$B0,$F4,$90,$18,$A5,$A5,$F0,$05,
      $A9,$02,$4C,$B9,$EE,$20,$A9,$E4,$20,$0C,$EF,$A9,$40,$20,$6A,$FE,
      $E6,$A5,$D0,$D5,$A9,$08,$85,$A5,$AD,$1F,$91,$CD,$1F,$91,$D0,$F8,
      $4A,$90,$F5,$4A,$66,$A4,$AD,$1F,$91,$CD,$1F,$91,$D0,$F8,$4A,$B0,
      $F5,$C6,$A5,$D0,$E3,$20,$A9,$E4,$A5,$90,$F0,$03,$20,$0C,$EF,$A5,
      $A4,$58,$18,$60,$AD,$2C,$91,$29,$FD,$8D,$2C,$91,$60,$AD,$2C,$91,
      $09,$02,$8D,$2C,$91,$60,$A9,$04,$8D,$29,$91,$AD,$2D,$91,$29,$20,
      $F0,$F9,$60,$A5,$B4,$F0,$47,$30,$3F,$46,$B6,$A2,$00,$90,$01,$CA,
      $8A,$45,$BD,$85,$BD,$C6,$B4,$F0,$06,$8A,$29,$20,$85,$B5,$60,$A9,
      $20,$2C,$94,$02,$F0,$14,$30,$1C,$70,$14,$A5,$BD,$D0,$01,$CA,$C6,
      $B4,$AD,$93,$02,$10,$E3,$C6,$B4,$D0,$DF,$E6,$B4,$D0,$F0,$A5,$BD,
      $F0,$ED,$D0,$EA,$70,$E9,$50,$E6,$E6,$B4,$A2,$FF,$D0,$CB,$AD,$94,
      $02,$4A,$90,$07,$2C,$20,$91,$10,$1D,$50,$1E,$A9,$00,$85,$BD,$85,
      $B5,$AE,$98,$02,$86,$B4,$AC,$9D,$02,$CC,$9E,$02,$F0,$13,$B1,$F9,
      $85,$B6,$EE,$9D,$02,$60,$A9,$40,$2C,$A9,$10,$0D,$97,$02,$8D,$97,
      $02,$A9,$40,$8D,$1E,$91,$60,$A2,$09,$A9,$20,$2C,$93,$02,$F0,$01,
      $CA,$50,$02,$CA,$CA,$60,$A6,$A9,$D0,$2E,$C6,$A8,$F0,$31,$30,$0D,
      $A5,$A7,$45,$AB,$85,$AB,$46,$A7,$66,$AA,$60,$C6,$A8,$A5,$A7,$F0,
      $62,$AD,$93,$02,$0A,$A9,$01,$65,$A8,$D0,$EF,$A9,$90,$8D,$1E,$91,
      $85,$A9,$A9,$20,$8D,$1E,$91,$60,$A5,$A7,$D0,$EF,$85,$A9,$60,$AC,
      $9B,$02,$C8,$CC,$9C,$02,$F0,$2A,$8C,$9B,$02,$88,$A5,$AA,$AE,$98,
      $02,$E0,$09,$F0,$04,$4A,$E8,$D0,$F8,$91,$F7,$A9,$20,$2C,$94,$02,
      $F0,$B9,$30,$B6,$A5,$A7,$45,$AB,$F0,$03,$70,$AE,$2C,$50,$AB,$A9,
      $01,$2C,$A9,$04,$2C,$A9,$80,$2C,$A9,$02,$0D,$97,$02,$8D,$97,$02,
      $4C,$5B,$F0,$A5,$AA,$D0,$F1,$F0,$EC,$4C,$96,$F7,$85,$9A,$AD,$94,
      $02,$4A,$90,$27,$A9,$02,$2C,$10,$91,$10,$1D,$D0,$1E,$AD,$1E,$91,
      $29,$30,$D0,$F9,$2C,$10,$91,$70,$FB,$AD,$10,$91,$09,$02,$8D,$10,
      $91,$2C,$10,$91,$70,$05,$30,$F9,$20,$16,$F0,$18,$60,$AC,$9E,$02,
      $C8,$CC,$9D,$02,$F0,$F7,$8C,$9E,$02,$88,$91,$F9,$2C,$1E,$91,$50,
      $01,$60,$AD,$99,$02,$8D,$14,$91,$AD,$9A,$02,$8D,$15,$91,$A9,$C0,
      $8D,$1E,$91,$4C,$EE,$EF,$85,$99,$AD,$94,$02,$4A,$90,$28,$29,$08,
      $F0,$24,$A9,$02,$2C,$10,$91,$10,$BF,$F0,$19,$2C,$1E,$91,$70,$FB,
      $AD,$10,$91,$29,$FD,$8D,$10,$91,$AD,$10,$91,$29,$04,$F0,$F9,$A9,
      $90,$8D,$1E,$91,$18,$60,$AD,$1E,$91,$29,$30,$F0,$F2,$18,$60,$AC,
      $9C,$02,$CC,$9B,$02,$F0,$06,$B1,$F7,$EE,$9C,$02,$60,$A9,$00,$60,
      $48,$AD,$1E,$91,$F0,$0C,$AD,$1E,$91,$29,$60,$D0,$F9,$A9,$10,$8D,
      $1E,$91,$68,$60,$0D,$49,$2F,$4F,$20,$45,$52,$52,$4F,$52,$20,$A3,
      $0D,$53,$45,$41,$52,$43,$48,$49,$4E,$47,$A0,$46,$4F,$52,$A0,$0D,
      $50,$52,$45,$53,$53,$20,$50,$4C,$41,$59,$20,$4F,$4E,$20,$54,$41,
      $50,$C5,$50,$52,$45,$53,$53,$20,$52,$45,$43,$4F,$52,$44,$20,$26,
      $20,$50,$4C,$41,$59,$20,$4F,$4E,$20,$54,$41,$50,$C5,$0D,$4C,$4F,
      $41,$44,$49,$4E,$C7,$0D,$53,$41,$56,$49,$4E,$47,$A0,$0D,$56,$45,
      $52,$49,$46,$59,$49,$4E,$C7,$0D,$46,$4F,$55,$4E,$44,$A0,$0D,$4F,
      $4B,$8D,$24,$9D,$10,$0D,$B9,$74,$F1,$08,$29,$7F,$20,$D2,$FF,$C8,
      $28,$10,$F3,$18,$60,$A5,$99,$D0,$08,$A5,$C6,$F0,$6D,$78,$4C,$CF,
      $E5,$C9,$02,$D0,$18,$84,$97,$20,$4F,$F1,$A4,$97,$18,$60,$A5,$99,
      $D0,$0B,$A5,$D3,$85,$CA,$A5,$D6,$85,$C9,$4C,$4F,$E6,$C9,$03,$D0,
      $09,$85,$D0,$A5,$D5,$85,$C8,$4C,$4F,$E6,$B0,$38,$C9,$02,$F0,$3F,
      $86,$97,$20,$50,$F2,$B0,$16,$48,$20,$50,$F2,$B0,$0D,$D0,$05,$A9,
      $40,$20,$6A,$FE,$C6,$A6,$A6,$97,$68,$60,$AA,$68,$8A,$A6,$97,$60,
      $20,$8A,$F8,$D0,$0B,$20,$C0,$F8,$B0,$11,$A9,$00,$85,$A6,$F0,$F0,
      $B1,$B2,$18,$60,$A5,$90,$F0,$04,$A9,$0D,$18,$60,$4C,$19,$EF,$20,
      $05,$F2,$B0,$05,$C9,$00,$F0,$F7,$18,$60,$48,$A5,$9A,$C9,$03,$D0,
      $04,$68,$4C,$42,$E7,$90,$04,$68,$4C,$E4,$EE,$C9,$02,$F0,$2A,$68,
      $85,$9E,$48,$8A,$48,$98,$48,$20,$8A,$F8,$D0,$0E,$20,$E3,$F8,$B0,
      $0E,$A9,$02,$A0,$00,$91,$B2,$C8,$84,$A6,$A5,$9E,$91,$B2,$18,$68,
      $A8,$68,$AA,$68,$90,$02,$A9,$00,$60,$68,$86,$97,$84,$9E,$20,$ED,
      $F0,$A6,$97,$A4,$9E,$18,$60,$20,$CF,$F3,$F0,$03,$4C,$84,$F7,$20,
      $DF,$F3,$A5,$BA,$F0,$16,$C9,$03,$F0,$12,$B0,$14,$C9,$02,$D0,$03,
      $4C,$16,$F1,$A6,$B9,$E0,$60,$F0,$03,$4C,$8D,$F7,$85,$99,$18,$60,
      $AA,$20,$14,$EE,$A5,$B9,$10,$06,$20,$D3,$EE,$4C,$01,$F3,$20,$CE,
      $EE,$8A,$24,$90,$10,$E6,$4C,$8A,$F7,$20,$CF,$F3,$F0,$03,$4C,$84,
      $F7,$20,$DF,$F3,$A5,$BA,$D0,$03,$4C,$90,$F7,$C9,$03,$F0,$0F,$B0,
      $11,$C9,$02,$D0,$03,$4C,$BC,$F0,$A6,$B9,$E0,$60,$F0,$EA,$85,$9A,
      $18,$60,$AA,$20,$17,$EE,$A5,$B9,$10,$05,$20,$C5,$EE,$D0,$03,$20,
      $C0,$EE,$8A,$24,$90,$10,$E7,$4C,$8A,$F7,$20,$D4,$F3,$F0,$02,$18,
      $60,$20,$DF,$F3,$8A,$48,$A5,$BA,$F0,$57,$C9,$03,$F0,$53,$B0,$4E,
      $C9,$02,$D0,$29,$68,$20,$B2,$F3,$A9,$7D,$8D,$1E,$91,$A9,$06,$8D,
      $10,$91,$A9,$EE,$8D,$1C,$91,$20,$75,$FE,$A5,$F8,$F0,$01,$C8,$A5,
      $FA,$F0,$01,$C8,$A9,$00,$85,$F8,$85,$FA,$4C,$3C,$F5,$A5,$B9,$29,
      $0F,$F0,$1E,$20,$4D,$F8,$A9,$00,$20,$90,$F2,$4C,$CF,$E4,$B0,$2E,
      $A5,$B9,$C9,$62,$D0,$0B,$A9,$05,$20,$E7,$F7,$4C,$B1,$F3,$20,$DA,
      $F6,$68,$AA,$C6,$98,$E4,$98,$F0,$14,$A4,$98,$B9,$59,$02,$9D,$59,
      $02,$B9,$63,$02,$9D,$63,$02,$B9,$6D,$02,$9D,$6D,$02,$18,$60,$A9,
      $00,$85,$90,$8A,$A6,$98,$CA,$30,$15,$DD,$59,$02,$D0,$F8,$60,$BD,
      $59,$02,$85,$B8,$BD,$63,$02,$85,$BA,$BD,$6D,$02,$85,$B9,$60,$A9,
      $00,$85,$98,$A2,$03,$E4,$9A,$B0,$03,$20,$04,$EF,$E4,$99,$B0,$03,
      $20,$F6,$EE,$86,$9A,$A9,$00,$85,$99,$60,$A6,$B8,$D0,$03,$4C,$8D,
      $F7,$20,$CF,$F3,$D0,$03,$4C,$81,$F7,$A6,$98,$E0,$0A,$90,$03,$4C,
      $7E,$F7,$E6,$98,$A5,$B8,$9D,$59,$02,$A5,$B9,$09,$60,$85,$B9,$9D,
      $6D,$02,$A5,$BA,$9D,$63,$02,$F0,$5A,$C9,$03,$F0,$56,$90,$05,$20,
      $95,$F4,$90,$4F,$C9,$02,$D0,$03,$4C,$C7,$F4,$20,$4D,$F8,$B0,$03,
      $4C,$96,$F7,$A5,$B9,$29,$0F,$D0,$1F,$20,$94,$F8,$B0,$36,$20,$47,
      $F6,$A5,$B7,$F0,$0A,$20,$67,$F8,$90,$18,$F0,$28,$4C,$87,$F7,$20,
      $AF,$F7,$F0,$20,$90,$0C,$B0,$F4,$20,$B7,$F8,$B0,$17,$A9,$04,$20,
      $E7,$F7,$A9,$BF,$A4,$B9,$C0,$60,$F0,$07,$A0,$00,$A9,$02,$91,$B2,
      $98,$85,$A6,$18,$60,$A5,$B9,$30,$2C,$A4,$B7,$F0,$28,$A5,$BA,$20,
      $17,$EE,$A5,$B9,$09,$F0,$20,$C0,$EE,$A5,$90,$10,$05,$68,$68,$4C,
      $8A,$F7,$A5,$B7,$F0,$0C,$A0,$00,$B1,$BB,$20,$E4,$EE,$C8,$C4,$B7,
      $D0,$F6,$20,$04,$EF,$18,$60,$A9,$06,$8D,$12,$91,$8D,$10,$91,$A9,
      $EE,$8D,$1C,$91,$A0,$00,$8C,$97,$02,$C4,$B7,$F0,$0A,$B1,$BB,$99,
      $93,$02,$C8,$C0,$04,$D0,$F2,$20,$27,$F0,$8E,$98,$02,$AD,$93,$02,
      $29,$0F,$D0,$00,$0A,$AA,$BD,$5A,$FF,$0A,$A8,$BD,$5B,$FF,$2A,$48,
      $98,$69,$C8,$8D,$99,$02,$68,$69,$00,$8D,$9A,$02,$AD,$94,$02,$4A,
      $90,$09,$AD,$20,$91,$0A,$B0,$03,$4C,$16,$F0,$AD,$9B,$02,$8D,$9C,
      $02,$AD,$9E,$02,$8D,$9D,$02,$20,$75,$FE,$A5,$F8,$D0,$05,$88,$84,
      $F8,$86,$F7,$A5,$FA,$D0,$05,$88,$84,$FA,$86,$F9,$38,$A9,$F0,$4C,
      $7B,$FE,$86,$C3,$84,$C4,$6C,$30,$03,$85,$93,$A9,$00,$85,$90,$A5,
      $BA,$D0,$03,$4C,$96,$F7,$C9,$03,$F0,$F9,$90,$6E,$A4,$B7,$D0,$03,
      $4C,$93,$F7,$20,$BC,$E4,$A9,$60,$85,$B9,$20,$95,$F4,$A5,$BA,$20,
      $14,$EE,$A5,$B9,$20,$CE,$EE,$20,$19,$EF,$85,$AE,$A5,$90,$4A,$4A,
      $B0,$45,$20,$19,$EF,$85,$AF,$20,$C1,$E4,$A9,$FD,$25,$90,$85,$90,
      $20,$E1,$FF,$D0,$03,$4C,$CB,$F6,$20,$19,$EF,$AA,$A5,$90,$4A,$4A,
      $B0,$E8,$8A,$A4,$93,$F0,$0C,$A0,$00,$D1,$AE,$F0,$08,$A9,$10,$20,
      $6A,$FE,$2C,$91,$AE,$E6,$AE,$D0,$02,$E6,$AF,$24,$90,$50,$CB,$20,
      $F6,$EE,$20,$DA,$F6,$90,$7A,$4C,$87,$F7,$C9,$02,$D0,$03,$4C,$B9,
      $F0,$20,$4D,$F8,$B0,$03,$4C,$96,$F7,$20,$94,$F8,$B0,$68,$20,$47,
      $F6,$A5,$B7,$F0,$09,$20,$67,$F8,$90,$0B,$F0,$5A,$B0,$D9,$20,$AF,
      $F7,$F0,$53,$B0,$D2,$A5,$90,$29,$10,$38,$D0,$4A,$E0,$01,$F0,$11,
      $E0,$03,$D0,$DD,$A0,$01,$B1,$B2,$85,$C3,$C8,$B1,$B2,$85,$C4,$B0,
      $04,$A5,$B9,$D0,$EF,$A0,$03,$B1,$B2,$A0,$01,$F1,$B2,$AA,$A0,$04,
      $B1,$B2,$A0,$02,$F1,$B2,$A8,$18,$8A,$65,$C3,$85,$AE,$98,$65,$C4,
      $85,$AF,$A5,$C3,$85,$C1,$A5,$C4,$85,$C2,$20,$6A,$F6,$20,$C9,$F8,
      $24,$18,$A6,$AE,$A4,$AF,$60,$A5,$9D,$10,$1E,$A0,$0C,$20,$E6,$F1,
      $A5,$B7,$F0,$15,$A0,$17,$20,$E6,$F1,$A4,$B7,$F0,$0C,$A0,$00,$B1,
      $BB,$20,$D2,$FF,$C8,$C4,$B7,$D0,$F6,$60,$A0,$49,$A5,$93,$F0,$02,
      $A0,$59,$4C,$E2,$F1,$86,$AE,$84,$AF,$AA,$B5,$00,$85,$C1,$B5,$01,
      $85,$C2,$6C,$32,$03,$A5,$BA,$D0,$03,$4C,$96,$F7,$C9,$03,$F0,$F9,
      $90,$5F,$A9,$61,$85,$B9,$A4,$B7,$D0,$03,$4C,$93,$F7,$20,$95,$F4,
      $20,$28,$F7,$A5,$BA,$20,$17,$EE,$A5,$B9,$20,$C0,$EE,$A0,$00,$20,
      $D2,$FB,$A5,$AC,$20,$E4,$EE,$A5,$AD,$20,$E4,$EE,$20,$11,$FD,$B0,
      $16,$B1,$AC,$20,$E4,$EE,$20,$E1,$FF,$D0,$07,$20,$DA,$F6,$A9,$00,
      $38,$60,$20,$1B,$FD,$D0,$E5,$20,$04,$EF,$24,$B9,$30,$11,$A5,$BA,
      $20,$17,$EE,$A5,$B9,$29,$EF,$09,$E0,$20,$C0,$EE,$20,$04,$EF,$18,
      $60,$C9,$02,$D0,$03,$4C,$B9,$F0,$20,$4D,$F8,$90,$8C,$20,$B7,$F8,
      $B0,$25,$20,$28,$F7,$A2,$03,$A5,$B9,$29,$01,$D0,$02,$A2,$01,$8A,
      $20,$E7,$F7,$B0,$12,$20,$E6,$F8,$B0,$0D,$A5,$B9,$29,$02,$F0,$06,
      $A9,$05,$20,$E7,$F7,$24,$18,$60,$A5,$9D,$10,$FB,$A0,$51,$20,$E6,
      $F1,$4C,$59,$F6,$A2,$00,$E6,$A2,$D0,$06,$E6,$A1,$D0,$02,$E6,$A0,
      $38,$A5,$A2,$E9,$01,$A5,$A1,$E9,$1A,$A5,$A0,$E9,$4F,$90,$06,$86,
      $A0,$86,$A1,$86,$A2,$AD,$2F,$91,$CD,$2F,$91,$D0,$F8,$85,$91,$60,
      $78,$A5,$A2,$A6,$A1,$A4,$A0,$78,$85,$A2,$86,$A1,$84,$A0,$58,$60,
      $A5,$91,$C9,$FE,$D0,$07,$08,$20,$CC,$FF,$85,$C6,$28,$60,$A9,$01,
      $2C,$A9,$02,$2C,$A9,$03,$2C,$A9,$04,$2C,$A9,$05,$2C,$A9,$06,$2C,
      $A9,$07,$2C,$A9,$08,$2C,$A9,$09,$48,$20,$CC,$FF,$A0,$00,$24,$9D,
      $50,$0A,$20,$E6,$F1,$68,$48,$09,$30,$20,$D2,$FF,$68,$38,$60,$A5,
      $93,$48,$20,$C0,$F8,$68,$85,$93,$B0,$2C,$A0,$00,$B1,$B2,$C9,$05,
      $F0,$24,$C9,$01,$F0,$08,$C9,$03,$F0,$04,$C9,$04,$D0,$E1,$AA,$24,
      $9D,$10,$11,$A0,$63,$20,$E6,$F1,$A0,$05,$B1,$B2,$20,$D2,$FF,$C8,
      $C0,$15,$D0,$F6,$18,$88,$60,$85,$9E,$20,$4D,$F8,$90,$5E,$A5,$C2,
      $48,$A5,$C1,$48,$A5,$AF,$48,$A5,$AE,$48,$A0,$BF,$A9,$20,$91,$B2,
      $88,$D0,$FB,$A5,$9E,$91,$B2,$C8,$A5,$C1,$91,$B2,$C8,$A5,$C2,$91,
      $B2,$C8,$A5,$AE,$91,$B2,$C8,$A5,$AF,$91,$B2,$C8,$84,$9F,$A0,$00,
      $84,$9E,$A4,$9E,$C4,$B7,$F0,$0C,$B1,$BB,$A4,$9F,$91,$B2,$E6,$9E,
      $E6,$9F,$D0,$EE,$20,$54,$F8,$A9,$69,$85,$AB,$20,$EA,$F8,$A8,$68,
      $85,$AE,$68,$85,$AF,$68,$85,$C1,$68,$85,$C2,$98,$60,$A6,$B2,$A4,
      $B3,$C0,$02,$60,$20,$4D,$F8,$8A,$85,$C1,$18,$69,$C0,$85,$AE,$98,
      $85,$C2,$69,$00,$85,$AF,$60,$20,$AF,$F7,$B0,$1D,$A0,$05,$84,$9F,
      $A0,$00,$84,$9E,$C4,$B7,$F0,$10,$B1,$BB,$A4,$9F,$D1,$B2,$D0,$E7,
      $E6,$9E,$E6,$9F,$A4,$9E,$D0,$EC,$18,$60,$20,$4D,$F8,$E6,$A6,$A4,
      $A6,$C0,$C0,$60,$20,$AB,$F8,$F0,$1C,$A0,$1B,$20,$E6,$F1,$20,$4B,
      $F9,$20,$AB,$F8,$D0,$F8,$A0,$6A,$4C,$E6,$F1,$A9,$40,$2C,$1F,$91,
      $D0,$03,$2C,$1F,$91,$18,$60,$20,$AB,$F8,$F0,$F9,$A0,$2E,$D0,$DB,
      $A9,$00,$85,$90,$85,$93,$20,$54,$F8,$20,$94,$F8,$B0,$1F,$78,$A9,
      $00,$85,$AA,$85,$B4,$85,$B0,$85,$9E,$85,$9F,$85,$9C,$A9,$82,$A2,
      $0E,$D0,$11,$20,$54,$F8,$A9,$14,$85,$AB,$20,$B7,$F8,$B0,$68,$78,
      $A9,$A0,$A2,$08,$A0,$7F,$8C,$2E,$91,$8D,$2E,$91,$20,$60,$F1,$AD,
      $14,$03,$8D,$9F,$02,$AD,$15,$03,$8D,$A0,$02,$20,$FB,$FC,$A9,$02,
      $85,$BE,$20,$DB,$FB,$AD,$1C,$91,$29,$FD,$09,$0C,$8D,$1C,$91,$85,
      $C0,$A2,$FF,$A0,$FF,$88,$D0,$FD,$CA,$D0,$F8,$8D,$29,$91,$58,$AD,
      $A0,$02,$CD,$15,$03,$18,$F0,$1F,$20,$4B,$F9,$AD,$2D,$91,$29,$40,
      $F0,$ED,$AD,$14,$91,$20,$34,$F7,$4C,$2F,$F9,$20,$E1,$FF,$18,$D0,
      $0B,$20,$CF,$FC,$38,$68,$68,$A9,$00,$8D,$A0,$02,$60,$86,$B1,$A5,
      $B0,$0A,$0A,$18,$65,$B0,$18,$65,$B1,$85,$B1,$A9,$00,$24,$B0,$30,
      $01,$2A,$06,$B1,$2A,$06,$B1,$2A,$AA,$AD,$28,$91,$C9,$15,$90,$F9,
      $65,$B1,$8D,$24,$91,$8A,$6D,$29,$91,$8D,$25,$91,$58,$60,$AE,$29,
      $91,$A0,$FF,$98,$ED,$28,$91,$EC,$29,$91,$D0,$F2,$86,$B1,$AA,$8C,
      $28,$91,$8C,$29,$91,$98,$E5,$B1,$86,$B1,$4A,$66,$B1,$4A,$66,$B1,
      $A5,$B0,$18,$69,$3C,$2C,$21,$91,$C5,$B1,$B0,$4A,$A6,$9C,$F0,$03,
      $4C,$AD,$FA,$A6,$A3,$30,$1B,$A2,$00,$69,$30,$65,$B0,$C5,$B1,$B0,
      $1C,$E8,$69,$26,$65,$B0,$C5,$B1,$B0,$17,$69,$2C,$65,$B0,$C5,$B1,
      $90,$03,$4C,$60,$FA,$A5,$B4,$F0,$1D,$85,$A8,$D0,$19,$E6,$A9,$B0,
      $02,$C6,$A9,$38,$E9,$13,$E5,$B1,$65,$92,$85,$92,$A5,$A4,$49,$01,
      $85,$A4,$F0,$21,$86,$D7,$A5,$B4,$F0,$18,$2C,$2D,$91,$50,$13,$A9,
      $00,$85,$A4,$A5,$A3,$10,$30,$30,$C9,$A2,$A6,$20,$5D,$F9,$A5,$9B,
      $D0,$C3,$4C,$56,$FF,$A5,$92,$F0,$07,$30,$03,$C6,$B0,$2C,$E6,$B0,
      $A9,$00,$85,$92,$E4,$D7,$D0,$0F,$8A,$D0,$AA,$A5,$A9,$30,$C7,$C9,
      $10,$90,$C3,$85,$96,$B0,$BF,$8A,$45,$9B,$85,$9B,$A5,$B4,$F0,$D2,
      $C6,$A3,$30,$C5,$46,$D7,$66,$BF,$A2,$DA,$20,$5D,$F9,$4C,$56,$FF,
      $A5,$96,$F0,$04,$A5,$B4,$F0,$04,$A5,$A3,$10,$85,$46,$B1,$A9,$93,
      $38,$E5,$B1,$65,$B0,$0A,$AA,$20,$5D,$F9,$E6,$9C,$A5,$B4,$D0,$11,
      $A5,$96,$F0,$26,$85,$A8,$A9,$00,$85,$96,$A9,$C0,$8D,$2E,$91,$85,
      $B4,$A5,$96,$85,$B5,$F0,$09,$A9,$00,$85,$B4,$A9,$40,$8D,$2E,$91,
      $A5,$BF,$85,$BD,$A5,$A8,$05,$A9,$85,$B6,$4C,$56,$FF,$20,$DB,$FB,
      $85,$9C,$A2,$DA,$20,$5D,$F9,$A5,$BE,$F0,$02,$85,$A7,$A9,$0F,$24,
      $AA,$10,$17,$A5,$B5,$D0,$0C,$A6,$BE,$CA,$D0,$0B,$A9,$08,$20,$6A,
      $FE,$D0,$04,$A9,$00,$85,$AA,$4C,$56,$FF,$70,$31,$D0,$18,$A5,$B5,
      $D0,$F5,$A5,$B6,$D0,$F1,$A5,$A7,$4A,$A5,$BD,$30,$03,$90,$18,$18,
      $B0,$15,$29,$0F,$85,$AA,$C6,$AA,$D0,$DD,$A9,$40,$85,$AA,$20,$D2,
      $FB,$A9,$00,$85,$AB,$F0,$D0,$A9,$80,$85,$AA,$D0,$CA,$A5,$B5,$F0,
      $0A,$A9,$04,$20,$6A,$FE,$A9,$00,$4C,$97,$FB,$20,$11,$FD,$90,$03,
      $4C,$95,$FB,$A6,$A7,$CA,$F0,$2D,$A5,$93,$F0,$0C,$A0,$00,$A5,$BD,
      $D1,$AC,$F0,$04,$A9,$01,$85,$B6,$A5,$B6,$F0,$4B,$A2,$3D,$E4,$9E,
      $90,$3E,$A6,$9E,$A5,$AD,$9D,$01,$01,$A5,$AC,$9D,$00,$01,$E8,$E8,
      $86,$9E,$4C,$87,$FB,$A6,$9F,$E4,$9E,$F0,$35,$A5,$AC,$DD,$00,$01,
      $D0,$2E,$A5,$AD,$DD,$01,$01,$D0,$27,$E6,$9F,$E6,$9F,$A5,$93,$F0,
      $0B,$A5,$BD,$A0,$00,$D1,$AC,$F0,$17,$C8,$84,$B6,$A5,$B6,$F0,$07,
      $A9,$10,$20,$6A,$FE,$D0,$09,$A5,$93,$D0,$05,$A8,$A5,$BD,$91,$AC,
      $20,$1B,$FD,$D0,$3A,$A9,$80,$85,$AA,$A6,$BE,$CA,$30,$02,$86,$BE,
      $C6,$A7,$F0,$08,$A5,$9E,$D0,$27,$85,$BE,$F0,$23,$20,$CF,$FC,$20,
      $D2,$FB,$A0,$00,$84,$AB,$B1,$AC,$45,$AB,$85,$AB,$20,$1B,$FD,$20,
      $11,$FD,$90,$F2,$A5,$AB,$45,$BD,$F0,$05,$A9,$20,$20,$6A,$FE,$4C,
      $56,$FF,$A5,$C2,$85,$AD,$A5,$C1,$85,$AC,$60,$A9,$08,$85,$A3,$A9,
      $00,$85,$A4,$85,$A8,$85,$9B,$85,$A9,$60,$A5,$BD,$4A,$A9,$60,$90,
      $02,$A9,$B0,$A2,$00,$8D,$28,$91,$8E,$29,$91,$AD,$20,$91,$49,$08,
      $8D,$20,$91,$29,$08,$60,$38,$66,$AD,$30,$3C,$A5,$A8,$D0,$12,$A9,
      $10,$A2,$01,$20,$F5,$FB,$D0,$2F,$E6,$A8,$A5,$AD,$10,$29,$4C,$95,
      $FC,$A5,$A9,$D0,$09,$20,$F1,$FB,$D0,$1D,$E6,$A9,$D0,$19,$20,$EA,
      $FB,$D0,$14,$A5,$A4,$49,$01,$85,$A4,$F0,$0F,$A5,$BD,$49,$01,$85,
      $BD,$29,$01,$45,$9B,$85,$9B,$4C,$56,$FF,$46,$BD,$C6,$A3,$A5,$A3,
      $F0,$3A,$10,$F3,$20,$DB,$FB,$58,$A5,$A5,$F0,$12,$A2,$00,$86,$D7,
      $C6,$A5,$A6,$BE,$E0,$02,$D0,$02,$09,$80,$85,$BD,$D0,$D9,$20,$11,
      $FD,$90,$0A,$D0,$91,$E6,$AD,$A5,$D7,$85,$BD,$B0,$CA,$A0,$00,$B1,
      $AC,$85,$BD,$45,$D7,$85,$D7,$20,$1B,$FD,$D0,$BB,$A5,$9B,$49,$01,
      $85,$BD,$4C,$56,$FF,$C6,$BE,$D0,$03,$20,$08,$FD,$A9,$50,$85,$A7,
      $A2,$08,$78,$20,$FB,$FC,$D0,$EA,$A9,$78,$20,$F3,$FB,$D0,$E3,$C6,
      $A7,$D0,$DF,$20,$DB,$FB,$C6,$AB,$10,$D8,$A2,$0A,$20,$FB,$FC,$58,
      $E6,$AB,$A5,$BE,$F0,$30,$20,$D2,$FB,$A2,$09,$86,$A5,$D0,$85,$08,
      $78,$20,$08,$FD,$A9,$7F,$8D,$2E,$91,$A9,$F7,$8D,$20,$91,$A9,$40,
      $8D,$2B,$91,$20,$39,$FE,$AD,$A0,$02,$F0,$09,$8D,$15,$03,$AD,$9F,
      $02,$8D,$14,$03,$28,$60,$20,$CF,$FC,$F0,$97,$BD,$E9,$FD,$8D,$14,
      $03,$BD,$EA,$FD,$8D,$15,$03,$60,$AD,$1C,$91,$09,$0E,$8D,$1C,$91,
      $60,$38,$A5,$AC,$E5,$AE,$A5,$AD,$E5,$AF,$60,$E6,$AC,$D0,$02,$E6,
      $AD,$60,$A2,$FF,$78,$9A,$D8,$20,$3F,$FD,$D0,$03,$6C,$00,$A0,$20,
      $8D,$FD,$20,$52,$FD,$20,$F9,$FD,$20,$18,$E5,$58,$6C,$00,$C0,$A2,
      $05,$BD,$4C,$FD,$DD,$03,$A0,$D0,$03,$CA,$D0,$F5,$60,$41,$30,$C3,
      $C2,$CD,$A2,$6D,$A0,$FD,$18,$86,$C3,$84,$C4,$A0,$1F,$B9,$14,$03,
      $B0,$02,$B1,$C3,$91,$C3,$99,$14,$03,$88,$10,$F1,$60,$BF,$EA,$D2,
      $FE,$AD,$FE,$0A,$F4,$4A,$F3,$C7,$F2,$09,$F3,$F3,$F3,$0E,$F2,$7A,
      $F2,$70,$F7,$F5,$F1,$EF,$F3,$D2,$FE,$49,$F5,$85,$F6,$A9,$00,$AA,
      $95,$00,$9D,$00,$02,$9D,$00,$03,$E8,$D0,$F5,$A2,$3C,$A0,$03,$86,
      $B2,$84,$B3,$85,$C1,$85,$97,$8D,$81,$02,$A8,$A9,$04,$85,$C2,$E6,
      $C1,$D0,$02,$E6,$C2,$20,$91,$FE,$A5,$97,$F0,$22,$B0,$F1,$A4,$C2,
      $A6,$C1,$C0,$20,$90,$25,$C0,$21,$B0,$08,$A0,$1E,$8C,$88,$02,$4C,
      $7B,$FE,$A9,$12,$8D,$82,$02,$A9,$10,$8D,$88,$02,$D0,$F1,$90,$CF,
      $A5,$C2,$8D,$82,$02,$85,$97,$C9,$11,$90,$C4,$20,$C3,$E5,$4C,$EB,
      $FD,$A8,$FC,$0B,$FC,$BF,$EA,$8E,$F9,$A9,$7F,$8D,$1E,$91,$8D,$2E,
      $91,$A9,$40,$8D,$2B,$91,$A9,$40,$8D,$1B,$91,$A9,$FE,$8D,$1C,$91,
      $A9,$DE,$8D,$2C,$91,$A2,$00,$8E,$12,$91,$A2,$FF,$8E,$22,$91,$A2,
      $00,$8E,$23,$91,$A2,$80,$8E,$13,$91,$A2,$00,$8E,$1F,$91,$20,$84,
      $EF,$A9,$82,$8D,$1E,$91,$20,$8D,$EF,$A9,$C0,$8D,$2E,$91,$A9,$26,
      $8D,$24,$91,$A9,$48,$8D,$25,$91,$60,$85,$B7,$86,$BB,$84,$BC,$60,
      $85,$B8,$86,$BA,$84,$B9,$60,$A5,$BA,$C9,$02,$D0,$0B,$AD,$97,$02,
      $A9,$00,$8D,$97,$02,$60,$85,$9D,$A5,$90,$05,$90,$85,$90,$60,$8D,
      $85,$02,$60,$90,$06,$AE,$83,$02,$AC,$84,$02,$8E,$83,$02,$8C,$84,
      $02,$60,$90,$06,$AE,$81,$02,$AC,$82,$02,$8E,$81,$02,$8C,$82,$02,
      $60,$B1,$C1,$AA,$A9,$55,$91,$C1,$D1,$C1,$D0,$08,$6A,$91,$C1,$D1,
      $C1,$D0,$01,$A9,$18,$8A,$91,$C1,$60,$78,$6C,$18,$03,$48,$8A,$48,
      $98,$48,$AD,$1D,$91,$10,$48,$2D,$1E,$91,$AA,$29,$02,$F0,$1F,$20,
      $3F,$FD,$D0,$03,$6C,$02,$A0,$2C,$11,$91,$20,$34,$F7,$20,$E1,$FF,
      $D0,$2D,$20,$52,$FD,$20,$F9,$FD,$20,$18,$E5,$6C,$02,$C0,$AD,$1E,
      $91,$09,$80,$48,$A9,$7F,$8D,$1E,$91,$8A,$29,$40,$F0,$14,$A9,$CE,
      $05,$B5,$8D,$1C,$91,$AD,$14,$91,$68,$8D,$1E,$91,$20,$A3,$EF,$4C,
      $56,$FF,$8A,$29,$20,$F0,$25,$AD,$10,$91,$29,$01,$85,$A7,$AD,$18,
      $91,$E9,$16,$6D,$99,$02,$8D,$18,$91,$AD,$19,$91,$6D,$9A,$02,$8D,
      $19,$91,$68,$8D,$1E,$91,$20,$36,$F0,$4C,$56,$FF,$8A,$29,$10,$F0,
      $25,$AD,$93,$02,$29,$0F,$D0,$00,$0A,$AA,$BD,$5A,$FF,$8D,$18,$91,
      $BD,$5B,$FF,$8D,$19,$91,$AD,$10,$91,$68,$09,$20,$29,$EF,$8D,$1E,
      $91,$AE,$98,$02,$86,$A8,$68,$A8,$68,$AA,$68,$40,$E6,$2A,$78,$1C,
      $49,$13,$B1,$0F,$0A,$0E,$D3,$06,$38,$03,$6A,$01,$D0,$00,$83,$00,
      $36,$00,$48,$8A,$48,$98,$48,$BA,$BD,$04,$01,$29,$10,$F0,$03,$6C,
      $16,$03,$6C,$14,$03,$FF,$FF,$FF,$FF,$FF,$4C,$52,$FD,$4C,$57,$FD,
      $4C,$66,$FE,$4C,$C0,$EE,$4C,$CE,$EE,$4C,$73,$FE,$4C,$82,$FE,$4C,
      $1E,$EB,$4C,$6F,$FE,$4C,$19,$EF,$4C,$E4,$EE,$4C,$F6,$EE,$4C,$04,
      $EF,$4C,$17,$EE,$4C,$14,$EE,$4C,$57,$FE,$4C,$50,$FE,$4C,$49,$FE,
      $6C,$1A,$03,$6C,$1C,$03,$6C,$1E,$03,$6C,$20,$03,$6C,$22,$03,$6C,
      $24,$03,$6C,$26,$03,$4C,$42,$F5,$4C,$75,$F6,$4C,$67,$F7,$4C,$60,
      $F7,$6C,$28,$03,$6C,$2A,$03,$6C,$2C,$03,$4C,$34,$F7,$4C,$05,$E5,
      $4C,$0A,$E5,$4C,$00,$E5,$FF,$FF,$FF,$FF,$A9,$FE,$22,$FD,$72,$FF
         );

// BIN data of basic -----------------------------------------------------------
// length = 8192

const
   Bin_basic_len = 8192;
var
   Bin_basic : array [0..8191] of byte = (
      $78,$E3,$67,$E4,$43,$42,$4D,$42,$41,$53,$49,$43,$30,$C8,$41,$C7,
      $1D,$CD,$F7,$C8,$A4,$CB,$BE,$CB,$80,$D0,$05,$CC,$A4,$C9,$9F,$C8,
      $70,$C8,$27,$C9,$1C,$C8,$82,$C8,$D1,$C8,$3A,$C9,$2E,$C8,$4A,$C9,
      $2C,$D8,$64,$E1,$52,$E1,$61,$E1,$B2,$D3,$23,$D8,$7F,$CA,$9F,$CA,
      $56,$C8,$9B,$C6,$5D,$C6,$85,$CA,$26,$E1,$BA,$E1,$C3,$E1,$7A,$CB,
      $41,$C6,$39,$DC,$CC,$DC,$58,$DC,$00,$00,$7D,$D3,$9E,$D3,$71,$DF,
      $94,$E0,$EA,$D9,$ED,$DF,$61,$E2,$68,$E2,$B1,$E2,$0B,$E3,$0D,$D8,
      $7C,$D7,$65,$D4,$AD,$D7,$8B,$D7,$EC,$D6,$00,$D7,$2C,$D7,$37,$D7,
      $79,$69,$D8,$79,$52,$D8,$7B,$2A,$DA,$7B,$11,$DB,$7F,$7A,$DF,$50,
      $E8,$CF,$46,$E5,$CF,$7D,$B3,$DF,$5A,$D3,$CE,$64,$15,$D0,$45,$4E,
      $C4,$46,$4F,$D2,$4E,$45,$58,$D4,$44,$41,$54,$C1,$49,$4E,$50,$55,
      $54,$A3,$49,$4E,$50,$55,$D4,$44,$49,$CD,$52,$45,$41,$C4,$4C,$45,
      $D4,$47,$4F,$54,$CF,$52,$55,$CE,$49,$C6,$52,$45,$53,$54,$4F,$52,
      $C5,$47,$4F,$53,$55,$C2,$52,$45,$54,$55,$52,$CE,$52,$45,$CD,$53,
      $54,$4F,$D0,$4F,$CE,$57,$41,$49,$D4,$4C,$4F,$41,$C4,$53,$41,$56,
      $C5,$56,$45,$52,$49,$46,$D9,$44,$45,$C6,$50,$4F,$4B,$C5,$50,$52,
      $49,$4E,$54,$A3,$50,$52,$49,$4E,$D4,$43,$4F,$4E,$D4,$4C,$49,$53,
      $D4,$43,$4C,$D2,$43,$4D,$C4,$53,$59,$D3,$4F,$50,$45,$CE,$43,$4C,
      $4F,$53,$C5,$47,$45,$D4,$4E,$45,$D7,$54,$41,$42,$A8,$54,$CF,$46,
      $CE,$53,$50,$43,$A8,$54,$48,$45,$CE,$4E,$4F,$D4,$53,$54,$45,$D0,
      $AB,$AD,$AA,$AF,$DE,$41,$4E,$C4,$4F,$D2,$BE,$BD,$BC,$53,$47,$CE,
      $49,$4E,$D4,$41,$42,$D3,$55,$53,$D2,$46,$52,$C5,$50,$4F,$D3,$53,
      $51,$D2,$52,$4E,$C4,$4C,$4F,$C7,$45,$58,$D0,$43,$4F,$D3,$53,$49,
      $CE,$54,$41,$CE,$41,$54,$CE,$50,$45,$45,$CB,$4C,$45,$CE,$53,$54,
      $52,$A4,$56,$41,$CC,$41,$53,$C3,$43,$48,$52,$A4,$4C,$45,$46,$54,
      $A4,$52,$49,$47,$48,$54,$A4,$4D,$49,$44,$A4,$47,$CF,$00,$54,$4F,
      $4F,$20,$4D,$41,$4E,$59,$20,$46,$49,$4C,$45,$D3,$46,$49,$4C,$45,
      $20,$4F,$50,$45,$CE,$46,$49,$4C,$45,$20,$4E,$4F,$54,$20,$4F,$50,
      $45,$CE,$46,$49,$4C,$45,$20,$4E,$4F,$54,$20,$46,$4F,$55,$4E,$C4,
      $44,$45,$56,$49,$43,$45,$20,$4E,$4F,$54,$20,$50,$52,$45,$53,$45,
      $4E,$D4,$4E,$4F,$54,$20,$49,$4E,$50,$55,$54,$20,$46,$49,$4C,$C5,
      $4E,$4F,$54,$20,$4F,$55,$54,$50,$55,$54,$20,$46,$49,$4C,$C5,$4D,
      $49,$53,$53,$49,$4E,$47,$20,$46,$49,$4C,$45,$20,$4E,$41,$4D,$C5,
      $49,$4C,$4C,$45,$47,$41,$4C,$20,$44,$45,$56,$49,$43,$45,$20,$4E,
      $55,$4D,$42,$45,$D2,$4E,$45,$58,$54,$20,$57,$49,$54,$48,$4F,$55,
      $54,$20,$46,$4F,$D2,$53,$59,$4E,$54,$41,$D8,$52,$45,$54,$55,$52,
      $4E,$20,$57,$49,$54,$48,$4F,$55,$54,$20,$47,$4F,$53,$55,$C2,$4F,
      $55,$54,$20,$4F,$46,$20,$44,$41,$54,$C1,$49,$4C,$4C,$45,$47,$41,
      $4C,$20,$51,$55,$41,$4E,$54,$49,$54,$D9,$4F,$56,$45,$52,$46,$4C,
      $4F,$D7,$4F,$55,$54,$20,$4F,$46,$20,$4D,$45,$4D,$4F,$52,$D9,$55,
      $4E,$44,$45,$46,$27,$44,$20,$53,$54,$41,$54,$45,$4D,$45,$4E,$D4,
      $42,$41,$44,$20,$53,$55,$42,$53,$43,$52,$49,$50,$D4,$52,$45,$44,
      $49,$4D,$27,$44,$20,$41,$52,$52,$41,$D9,$44,$49,$56,$49,$53,$49,
      $4F,$4E,$20,$42,$59,$20,$5A,$45,$52,$CF,$49,$4C,$4C,$45,$47,$41,
      $4C,$20,$44,$49,$52,$45,$43,$D4,$54,$59,$50,$45,$20,$4D,$49,$53,
      $4D,$41,$54,$43,$C8,$53,$54,$52,$49,$4E,$47,$20,$54,$4F,$4F,$20,
      $4C,$4F,$4E,$C7,$46,$49,$4C,$45,$20,$44,$41,$54,$C1,$46,$4F,$52,
      $4D,$55,$4C,$41,$20,$54,$4F,$4F,$20,$43,$4F,$4D,$50,$4C,$45,$D8,
      $43,$41,$4E,$27,$54,$20,$43,$4F,$4E,$54,$49,$4E,$55,$C5,$55,$4E,
      $44,$45,$46,$27,$44,$20,$46,$55,$4E,$43,$54,$49,$4F,$CE,$56,$45,
      $52,$49,$46,$D9,$4C,$4F,$41,$C4,$9E,$C1,$AC,$C1,$B5,$C1,$C2,$C1,
      $D0,$C1,$E2,$C1,$F0,$C1,$FF,$C1,$10,$C2,$25,$C2,$35,$C2,$3B,$C2,
      $4F,$C2,$5A,$C2,$6A,$C2,$72,$C2,$7F,$C2,$90,$C2,$9D,$C2,$AA,$C2,
      $BA,$C2,$C8,$C2,$D5,$C2,$E4,$C2,$ED,$C2,$00,$C3,$0E,$C3,$1E,$C3,
      $24,$C3,$83,$C3,$0D,$4F,$4B,$0D,$00,$0D,$20,$45,$52,$52,$4F,$52,
      $00,$20,$49,$4E,$20,$00,$0D,$0A,$52,$45,$41,$44,$59,$2E,$0D,$0A,
      $00,$0D,$0A,$42,$52,$45,$41,$4B,$00,$A0,$BA,$E8,$E8,$E8,$E8,$BD,
      $01,$01,$C9,$81,$D0,$21,$A5,$4A,$D0,$0A,$BD,$02,$01,$85,$49,$BD,
      $03,$01,$85,$4A,$DD,$03,$01,$D0,$07,$A5,$49,$DD,$02,$01,$F0,$07,
      $8A,$18,$69,$12,$AA,$D0,$D8,$60,$20,$08,$C4,$85,$31,$84,$32,$38,
      $A5,$5A,$E5,$5F,$85,$22,$A8,$A5,$5B,$E5,$60,$AA,$E8,$98,$F0,$23,
      $A5,$5A,$38,$E5,$22,$85,$5A,$B0,$03,$C6,$5B,$38,$A5,$58,$E5,$22,
      $85,$58,$B0,$08,$C6,$59,$90,$04,$B1,$5A,$91,$58,$88,$D0,$F9,$B1,
      $5A,$91,$58,$C6,$5B,$C6,$59,$CA,$D0,$F2,$60,$0A,$69,$3E,$B0,$35,
      $85,$22,$BA,$E4,$22,$90,$2E,$60,$C4,$34,$90,$28,$D0,$04,$C5,$33,
      $90,$22,$48,$A2,$09,$98,$48,$B5,$57,$CA,$10,$FA,$20,$26,$D5,$A2,
      $F7,$68,$95,$61,$E8,$30,$FA,$68,$A8,$68,$C4,$34,$90,$06,$D0,$05,
      $C5,$33,$B0,$01,$60,$A2,$10,$6C,$00,$03,$8A,$0A,$AA,$BD,$26,$C3,
      $85,$22,$BD,$27,$C3,$85,$23,$20,$CC,$FF,$A9,$00,$85,$13,$20,$D7,
      $CA,$20,$45,$CB,$A0,$00,$B1,$22,$48,$29,$7F,$20,$47,$CB,$C8,$68,
      $10,$F4,$20,$7A,$C6,$A9,$69,$A0,$C3,$20,$1E,$CB,$A4,$3A,$C8,$F0,
      $03,$20,$C2,$DD,$A9,$76,$A0,$C3,$20,$1E,$CB,$A9,$80,$20,$90,$FF,
      $6C,$02,$03,$20,$60,$C5,$86,$7A,$84,$7B,$20,$73,$00,$AA,$F0,$F0,
      $A2,$FF,$86,$3A,$90,$06,$20,$79,$C5,$4C,$E1,$C7,$20,$6B,$C9,$20,
      $79,$C5,$84,$0B,$20,$13,$C6,$90,$44,$A0,$01,$B1,$5F,$85,$23,$A5,
      $2D,$85,$22,$A5,$60,$85,$25,$A5,$5F,$88,$F1,$5F,$18,$65,$2D,$85,
      $2D,$85,$24,$A5,$2E,$69,$FF,$85,$2E,$E5,$60,$AA,$38,$A5,$5F,$E5,
      $2D,$A8,$B0,$03,$E8,$C6,$25,$18,$65,$22,$90,$03,$C6,$23,$18,$B1,
      $22,$91,$24,$C8,$D0,$F9,$E6,$23,$E6,$25,$CA,$D0,$F2,$20,$59,$C6,
      $20,$33,$C5,$AD,$00,$02,$F0,$88,$18,$A5,$2D,$85,$5A,$65,$0B,$85,
      $58,$A4,$2E,$84,$5B,$90,$01,$C8,$84,$59,$20,$B8,$C3,$A5,$14,$A4,
      $15,$8D,$FE,$01,$8C,$FF,$01,$A5,$31,$A4,$32,$85,$2D,$84,$2E,$A4,
      $0B,$88,$B9,$FC,$01,$91,$5F,$88,$10,$F8,$20,$59,$C6,$20,$33,$C5,
      $4C,$80,$C4,$A5,$2B,$A4,$2C,$85,$22,$84,$23,$18,$A0,$01,$B1,$22,
      $F0,$1D,$A0,$04,$C8,$B1,$22,$D0,$FB,$C8,$98,$65,$22,$AA,$A0,$00,
      $91,$22,$A5,$23,$69,$00,$C8,$91,$22,$86,$22,$85,$23,$90,$DD,$60,
      $A2,$00,$20,$0F,$E1,$C9,$0D,$F0,$0D,$9D,$00,$02,$E8,$E0,$59,$90,
      $F1,$A2,$17,$4C,$37,$C4,$4C,$CA,$CA,$6C,$04,$03,$A6,$7A,$A0,$04,
      $84,$0F,$BD,$00,$02,$10,$07,$C9,$FF,$F0,$3E,$E8,$D0,$F4,$C9,$20,
      $F0,$37,$85,$08,$C9,$22,$F0,$56,$24,$0F,$70,$2D,$C9,$3F,$D0,$04,
      $A9,$99,$D0,$25,$C9,$30,$90,$04,$C9,$3C,$90,$1D,$84,$71,$A0,$00,
      $84,$0B,$88,$86,$7A,$CA,$C8,$E8,$BD,$00,$02,$38,$F9,$9E,$C0,$F0,
      $F5,$C9,$80,$D0,$30,$05,$0B,$A4,$71,$E8,$C8,$99,$FB,$01,$B9,$FB,
      $01,$F0,$36,$38,$E9,$3A,$F0,$04,$C9,$49,$D0,$02,$85,$0F,$38,$E9,
      $55,$D0,$9F,$85,$08,$BD,$00,$02,$F0,$DF,$C5,$08,$F0,$DB,$C8,$99,
      $FB,$01,$E8,$D0,$F0,$A6,$7A,$E6,$0B,$C8,$B9,$9D,$C0,$10,$FA,$B9,
      $9E,$C0,$D0,$B4,$BD,$00,$02,$10,$BE,$99,$FD,$01,$C6,$7B,$A9,$FF,
      $85,$7A,$60,$A5,$2B,$A6,$2C,$A0,$01,$85,$5F,$86,$60,$B1,$5F,$F0,
      $1F,$C8,$C8,$A5,$15,$D1,$5F,$90,$18,$F0,$03,$88,$D0,$09,$A5,$14,
      $88,$D1,$5F,$90,$0C,$F0,$0A,$88,$B1,$5F,$AA,$88,$B1,$5F,$B0,$D7,
      $18,$60,$D0,$FD,$A9,$00,$A8,$91,$2B,$C8,$91,$2B,$A5,$2B,$18,$69,
      $02,$85,$2D,$A5,$2C,$69,$00,$85,$2E,$20,$8E,$C6,$A9,$00,$D0,$2D,
      $20,$E7,$FF,$A5,$37,$A4,$38,$85,$33,$84,$34,$A5,$2D,$A4,$2E,$85,
      $2F,$84,$30,$85,$31,$84,$32,$20,$1D,$C8,$A2,$19,$86,$16,$68,$A8,
      $68,$A2,$FA,$9A,$48,$98,$48,$A9,$00,$85,$3E,$85,$10,$60,$18,$A5,
      $2B,$69,$FF,$85,$7A,$A5,$2C,$69,$FF,$85,$7B,$60,$90,$06,$F0,$04,
      $C9,$AB,$D0,$E9,$20,$6B,$C9,$20,$13,$C6,$20,$79,$00,$F0,$0C,$C9,
      $AB,$D0,$8E,$20,$73,$00,$20,$6B,$C9,$D0,$86,$68,$68,$A5,$14,$05,
      $15,$D0,$06,$A9,$FF,$85,$14,$85,$15,$A0,$01,$84,$0F,$B1,$5F,$F0,
      $43,$20,$2C,$C8,$20,$D7,$CA,$C8,$B1,$5F,$AA,$C8,$B1,$5F,$C5,$15,
      $D0,$04,$E4,$14,$F0,$02,$B0,$2C,$84,$49,$20,$CD,$DD,$A9,$20,$A4,
      $49,$29,$7F,$20,$47,$CB,$C9,$22,$D0,$06,$A5,$0F,$49,$FF,$85,$0F,
      $C8,$F0,$11,$B1,$5F,$D0,$10,$A8,$B1,$5F,$AA,$C8,$B1,$5F,$86,$5F,
      $85,$60,$D0,$B5,$4C,$74,$C4,$6C,$06,$03,$10,$D7,$C9,$FF,$F0,$D3,
      $24,$0F,$30,$CF,$38,$E9,$7F,$AA,$84,$49,$A0,$FF,$CA,$F0,$08,$C8,
      $B9,$9E,$C0,$10,$FA,$30,$F5,$C8,$B9,$9E,$C0,$30,$B2,$20,$47,$CB,
      $D0,$F5,$A9,$80,$85,$10,$20,$A5,$C9,$20,$8A,$C3,$D0,$05,$8A,$69,
      $0F,$AA,$9A,$68,$68,$A9,$09,$20,$FB,$C3,$20,$06,$C9,$18,$98,$65,
      $7A,$48,$A5,$7B,$69,$00,$48,$A5,$3A,$48,$A5,$39,$48,$A9,$A4,$20,
      $FF,$CE,$20,$8D,$CD,$20,$8A,$CD,$A5,$66,$09,$7F,$25,$62,$85,$62,
      $A9,$8B,$A0,$C7,$85,$22,$84,$23,$4C,$43,$CE,$A9,$BC,$A0,$D9,$20,
      $A2,$DB,$20,$79,$00,$C9,$A9,$D0,$06,$20,$73,$00,$20,$8A,$CD,$20,
      $2B,$DC,$20,$38,$CE,$A5,$4A,$48,$A5,$49,$48,$A9,$81,$48,$20,$2C,
      $C8,$A5,$7A,$A4,$7B,$C0,$02,$EA,$F0,$04,$85,$3D,$84,$3E,$A0,$00,
      $B1,$7A,$D0,$43,$A0,$02,$B1,$7A,$18,$D0,$03,$4C,$4B,$C8,$C8,$B1,
      $7A,$85,$39,$C8,$B1,$7A,$85,$3A,$98,$65,$7A,$85,$7A,$90,$02,$E6,
      $7B,$6C,$08,$03,$20,$73,$00,$20,$ED,$C7,$4C,$AE,$C7,$F0,$3C,$E9,
      $80,$90,$11,$C9,$23,$B0,$17,$0A,$A8,$B9,$0D,$C0,$48,$B9,$0C,$C0,
      $48,$4C,$73,$00,$4C,$A5,$C9,$C9,$3A,$F0,$D6,$4C,$08,$CF,$C9,$4B,
      $D0,$F9,$20,$73,$00,$A9,$A4,$20,$FF,$CE,$4C,$A0,$C8,$38,$A5,$2B,
      $E9,$01,$A4,$2C,$B0,$01,$88,$85,$41,$84,$42,$60,$20,$E1,$FF,$B0,
      $01,$18,$D0,$3C,$A5,$7A,$A4,$7B,$A6,$3A,$E8,$F0,$0C,$85,$3D,$84,
      $3E,$A5,$39,$A4,$3A,$85,$3B,$84,$3C,$68,$68,$A9,$81,$A0,$C3,$90,
      $03,$4C,$69,$C4,$4C,$74,$C4,$D0,$17,$A2,$1A,$A4,$3E,$D0,$03,$4C,
      $37,$C4,$A5,$3D,$85,$7A,$84,$7B,$A5,$3B,$A4,$3C,$85,$39,$84,$3A,
      $60,$08,$A9,$00,$20,$90,$FF,$28,$D0,$03,$4C,$59,$C6,$20,$60,$C6,
      $4C,$97,$C8,$A9,$03,$20,$FB,$C3,$A5,$7B,$48,$A5,$7A,$48,$A5,$3A,
      $48,$A5,$39,$48,$A9,$8D,$48,$20,$79,$00,$20,$A0,$C8,$4C,$AE,$C7,
      $20,$6B,$C9,$20,$09,$C9,$38,$A5,$39,$E5,$14,$A5,$3A,$E5,$15,$B0,
      $0B,$98,$38,$65,$7A,$A6,$7B,$90,$07,$E8,$B0,$04,$A5,$2B,$A6,$2C,
      $20,$17,$C6,$90,$1E,$A5,$5F,$E9,$01,$85,$7A,$A5,$60,$E9,$00,$85,
      $7B,$60,$D0,$FD,$A9,$FF,$85,$4A,$20,$8A,$C3,$9A,$C9,$8D,$F0,$0B,
      $A2,$0C,$2C,$A2,$11,$4C,$37,$C4,$4C,$08,$CF,$68,$68,$85,$39,$68,
      $85,$3A,$68,$85,$7A,$68,$85,$7B,$20,$06,$C9,$98,$18,$65,$7A,$85,
      $7A,$90,$02,$E6,$7B,$60,$A2,$3A,$2C,$A2,$00,$86,$07,$A0,$00,$84,
      $08,$A5,$08,$A6,$07,$85,$07,$86,$08,$B1,$7A,$F0,$E8,$C5,$08,$F0,
      $E4,$C8,$C9,$22,$D0,$F3,$F0,$E9,$20,$9E,$CD,$20,$79,$00,$C9,$89,
      $F0,$05,$A9,$A7,$20,$FF,$CE,$A5,$61,$D0,$05,$20,$09,$C9,$F0,$BB,
      $20,$79,$00,$B0,$03,$4C,$A0,$C8,$4C,$ED,$C7,$20,$9E,$D7,$48,$C9,
      $8D,$F0,$04,$C9,$89,$D0,$91,$C6,$65,$D0,$04,$68,$4C,$EF,$C7,$20,
      $73,$00,$20,$6B,$C9,$C9,$2C,$F0,$EE,$68,$60,$A2,$00,$86,$14,$86,
      $15,$B0,$F7,$E9,$2F,$85,$07,$A5,$15,$85,$22,$C9,$19,$B0,$D4,$A5,
      $14,$0A,$26,$22,$0A,$26,$22,$65,$14,$85,$14,$A5,$22,$65,$15,$85,
      $15,$06,$14,$26,$15,$A5,$14,$65,$07,$85,$14,$90,$02,$E6,$15,$20,
      $73,$00,$4C,$71,$C9,$20,$8B,$D0,$85,$49,$84,$4A,$A9,$B2,$20,$FF,
      $CE,$A5,$0E,$48,$A5,$0D,$48,$20,$9E,$CD,$68,$2A,$20,$90,$CD,$D0,
      $18,$68,$10,$12,$20,$1B,$DC,$20,$BF,$D1,$A0,$00,$A5,$64,$91,$49,
      $C8,$A5,$65,$91,$49,$60,$4C,$D0,$DB,$68,$A4,$4A,$C0,$DF,$D0,$4C,
      $20,$A6,$D6,$C9,$06,$D0,$3D,$A0,$00,$84,$61,$84,$66,$84,$71,$20,
      $1D,$CA,$20,$E2,$DA,$E6,$71,$A4,$71,$20,$1D,$CA,$20,$0C,$DC,$AA,
      $F0,$05,$E8,$8A,$20,$ED,$DA,$A4,$71,$C8,$C0,$06,$D0,$DF,$20,$E2,
      $DA,$20,$9B,$DC,$A6,$64,$A4,$63,$A5,$65,$4C,$DB,$FF,$B1,$22,$20,
      $80,$00,$90,$03,$4C,$48,$D2,$E9,$2F,$4C,$7E,$DD,$A0,$02,$B1,$64,
      $C5,$34,$90,$17,$D0,$07,$88,$B1,$64,$C5,$33,$90,$0E,$A4,$65,$C4,
      $2E,$90,$08,$D0,$0D,$A5,$64,$C5,$2D,$B0,$07,$A5,$64,$A4,$65,$4C,
      $68,$CA,$A0,$00,$B1,$64,$20,$75,$D4,$A5,$50,$A4,$51,$85,$6F,$84,
      $70,$20,$7A,$D6,$A9,$61,$A0,$00,$85,$50,$84,$51,$20,$DB,$D6,$A0,
      $00,$B1,$50,$91,$49,$C8,$B1,$50,$91,$49,$C8,$B1,$50,$91,$49,$60,
      $20,$86,$CA,$4C,$B5,$CB,$20,$9E,$D7,$F0,$05,$A9,$2C,$20,$FF,$CE,
      $08,$86,$13,$20,$15,$E1,$28,$4C,$A0,$CA,$20,$21,$CB,$20,$79,$00,
      $F0,$35,$F0,$43,$C9,$A3,$F0,$50,$C9,$A6,$18,$F0,$4B,$C9,$2C,$F0,
      $37,$C9,$3B,$F0,$5E,$20,$9E,$CD,$24,$0D,$30,$DE,$20,$DD,$DD,$20,
      $87,$D4,$20,$21,$CB,$20,$3B,$CB,$D0,$D3,$A9,$00,$9D,$00,$02,$A2,
      $FF,$A0,$01,$A5,$13,$D0,$10,$A9,$0D,$20,$47,$CB,$24,$13,$10,$05,
      $A9,$0A,$20,$47,$CB,$49,$FF,$60,$38,$20,$F0,$FF,$98,$38,$E9,$0B,
      $B0,$FC,$49,$FF,$69,$01,$D0,$16,$08,$38,$20,$F0,$FF,$84,$09,$20,
      $9B,$D7,$C9,$29,$D0,$59,$28,$90,$06,$8A,$E5,$09,$90,$05,$AA,$E8,
      $CA,$D0,$06,$20,$73,$00,$4C,$A2,$CA,$20,$3B,$CB,$D0,$F2,$20,$87,
      $D4,$20,$A6,$D6,$AA,$A0,$00,$E8,$CA,$F0,$BC,$B1,$22,$20,$47,$CB,
      $C8,$C9,$0D,$D0,$F3,$20,$E5,$CA,$4C,$28,$CB,$A5,$13,$F0,$03,$A9,
      $20,$2C,$A9,$1D,$2C,$A9,$3F,$20,$09,$E1,$29,$FF,$60,$A5,$11,$F0,
      $11,$30,$04,$A0,$FF,$D0,$04,$A5,$3F,$A4,$40,$85,$39,$84,$3A,$4C,
      $08,$CF,$A5,$13,$F0,$05,$A2,$18,$4C,$37,$C4,$A9,$0C,$A0,$CD,$20,
      $1E,$CB,$A5,$3D,$A4,$3E,$85,$7A,$84,$7B,$60,$20,$A6,$D3,$C9,$23,
      $D0,$10,$20,$73,$00,$20,$9E,$D7,$A9,$2C,$20,$FF,$CE,$86,$13,$20,
      $1B,$E1,$A2,$01,$A0,$02,$A9,$00,$8D,$01,$02,$A9,$40,$20,$0F,$CC,
      $A6,$13,$D0,$13,$60,$20,$9E,$D7,$A9,$2C,$20,$FF,$CE,$86,$13,$20,
      $1B,$E1,$20,$CE,$CB,$A5,$13,$20,$CC,$FF,$A2,$00,$86,$13,$60,$C9,
      $22,$D0,$0B,$20,$BD,$CE,$A9,$3B,$20,$FF,$CE,$20,$21,$CB,$20,$A6,
      $D3,$A9,$2C,$8D,$FF,$01,$20,$F9,$CB,$A5,$13,$F0,$0D,$20,$B7,$FF,
      $29,$02,$F0,$06,$20,$B5,$CB,$4C,$F8,$C8,$AD,$00,$02,$D0,$1E,$A5,
      $13,$D0,$E3,$20,$06,$C9,$4C,$FB,$C8,$A5,$13,$D0,$06,$20,$45,$CB,
      $20,$3B,$CB,$4C,$60,$C5,$A6,$41,$A4,$42,$A9,$98,$2C,$A9,$00,$85,
      $11,$86,$43,$84,$44,$20,$8B,$D0,$85,$49,$84,$4A,$A5,$7A,$A4,$7B,
      $85,$4B,$84,$4C,$A6,$43,$A4,$44,$86,$7A,$84,$7B,$20,$79,$00,$D0,
      $20,$24,$11,$50,$0C,$20,$21,$E1,$8D,$00,$02,$A2,$FF,$A0,$01,$D0,
      $0C,$30,$75,$A5,$13,$D0,$03,$20,$45,$CB,$20,$F9,$CB,$86,$7A,$84,
      $7B,$20,$73,$00,$24,$0D,$10,$31,$24,$11,$50,$09,$E8,$86,$7A,$A9,
      $00,$85,$07,$F0,$0C,$85,$07,$C9,$22,$F0,$07,$A9,$3A,$85,$07,$A9,
      $2C,$18,$85,$08,$A5,$7A,$A4,$7B,$69,$00,$90,$01,$C8,$20,$8D,$D4,
      $20,$E2,$D7,$20,$DA,$C9,$4C,$91,$CC,$20,$F3,$DC,$A5,$0E,$20,$C2,
      $C9,$20,$79,$00,$F0,$07,$C9,$2C,$F0,$03,$4C,$4D,$CB,$A5,$7A,$A4,
      $7B,$85,$43,$84,$44,$A5,$4B,$A4,$4C,$85,$7A,$84,$7B,$20,$79,$00,
      $F0,$2D,$20,$FD,$CE,$4C,$15,$CC,$20,$06,$C9,$C8,$AA,$D0,$12,$A2,
      $0D,$C8,$B1,$7A,$F0,$6C,$C8,$B1,$7A,$85,$3F,$C8,$B1,$7A,$C8,$85,
      $40,$20,$FB,$C8,$20,$79,$00,$AA,$E0,$83,$D0,$DC,$4C,$51,$CC,$A5,
      $43,$A4,$44,$A6,$11,$10,$03,$4C,$27,$C8,$A0,$00,$B1,$43,$F0,$0B,
      $A5,$13,$D0,$07,$A9,$FC,$A0,$CC,$4C,$1E,$CB,$60,$3F,$45,$58,$54,
      $52,$41,$20,$49,$47,$4E,$4F,$52,$45,$44,$0D,$00,$3F,$52,$45,$44,
      $4F,$20,$46,$52,$4F,$4D,$20,$53,$54,$41,$52,$54,$0D,$00,$D0,$04,
      $A0,$00,$F0,$03,$20,$8B,$D0,$85,$49,$84,$4A,$20,$8A,$C3,$F0,$05,
      $A2,$0A,$4C,$37,$C4,$9A,$8A,$18,$69,$04,$48,$69,$06,$85,$24,$68,
      $A0,$01,$20,$A2,$DB,$BA,$BD,$09,$01,$85,$66,$A5,$49,$A4,$4A,$20,
      $67,$D8,$20,$D0,$DB,$A0,$01,$20,$5D,$DC,$BA,$38,$FD,$09,$01,$F0,
      $17,$BD,$0F,$01,$85,$39,$BD,$10,$01,$85,$3A,$BD,$12,$01,$85,$7A,
      $BD,$11,$01,$85,$7B,$4C,$AE,$C7,$8A,$69,$11,$AA,$9A,$20,$79,$00,
      $C9,$2C,$D0,$F1,$20,$73,$00,$20,$24,$CD,$20,$9E,$CD,$18,$24,$38,
      $24,$0D,$30,$03,$B0,$03,$60,$B0,$FD,$A2,$16,$4C,$37,$C4,$A6,$7A,
      $D0,$02,$C6,$7B,$C6,$7A,$A2,$00,$24,$48,$8A,$48,$A9,$01,$20,$FB,
      $C3,$20,$83,$CE,$A9,$00,$85,$4D,$20,$79,$00,$38,$E9,$B1,$90,$17,
      $C9,$03,$B0,$13,$C9,$01,$2A,$49,$01,$45,$4D,$C5,$4D,$90,$61,$85,
      $4D,$20,$73,$00,$4C,$BB,$CD,$A6,$4D,$D0,$2C,$B0,$7B,$69,$07,$90,
      $77,$65,$0D,$D0,$03,$4C,$3D,$D6,$69,$FF,$85,$22,$0A,$65,$22,$A8,
      $68,$D9,$80,$C0,$B0,$67,$20,$8D,$CD,$48,$20,$20,$CE,$68,$A4,$4B,
      $10,$17,$AA,$F0,$56,$D0,$5F,$46,$0D,$8A,$2A,$A6,$7A,$D0,$02,$C6,
      $7B,$C6,$7A,$A0,$1B,$85,$4D,$D0,$D7,$D9,$80,$C0,$B0,$48,$90,$D9,
      $B9,$82,$C0,$48,$B9,$81,$C0,$48,$20,$33,$CE,$A5,$4D,$4C,$A9,$CD,
      $4C,$08,$CF,$A5,$66,$BE,$80,$C0,$A8,$68,$85,$22,$E6,$22,$68,$85,
      $23,$98,$48,$20,$1B,$DC,$A5,$65,$48,$A5,$64,$48,$A5,$63,$48,$A5,
      $62,$48,$A5,$61,$48,$6C,$22,$00,$A0,$FF,$68,$F0,$23,$C9,$64,$F0,
      $03,$20,$8D,$CD,$84,$4B,$68,$4A,$85,$12,$68,$85,$69,$68,$85,$6A,
      $68,$85,$6B,$68,$85,$6C,$68,$85,$6D,$68,$85,$6E,$45,$66,$85,$6F,
      $A5,$61,$60,$6C,$0A,$03,$A9,$00,$85,$0D,$20,$73,$00,$B0,$03,$4C,
      $F3,$DC,$20,$13,$D1,$90,$03,$4C,$28,$CF,$C9,$FF,$D0,$0F,$A9,$A8,
      $A0,$CE,$20,$A2,$DB,$4C,$73,$00,$82,$49,$0F,$DA,$A1,$C9,$2E,$F0,
      $DE,$C9,$AB,$F0,$58,$C9,$AA,$F0,$D1,$C9,$22,$D0,$0F,$A5,$7A,$A4,
      $7B,$69,$00,$90,$01,$C8,$20,$87,$D4,$4C,$E2,$D7,$C9,$A8,$D0,$13,
      $A0,$18,$D0,$3B,$20,$BF,$D1,$A5,$65,$49,$FF,$A8,$A5,$64,$49,$FF,
      $4C,$91,$D3,$C9,$A5,$D0,$03,$4C,$F4,$D3,$C9,$B4,$90,$03,$4C,$A7,
      $CF,$20,$FA,$CE,$20,$9E,$CD,$A9,$29,$2C,$A9,$28,$2C,$A9,$2C,$A0,
      $00,$D1,$7A,$D0,$03,$4C,$73,$00,$A2,$0B,$4C,$37,$C4,$A0,$15,$68,
      $68,$4C,$FA,$CD,$38,$A5,$64,$E9,$00,$A5,$65,$E9,$C0,$90,$08,$A9,
      $87,$E5,$64,$A9,$E3,$E5,$65,$60,$20,$8B,$D0,$85,$64,$84,$65,$A6,
      $45,$A4,$46,$A5,$0D,$F0,$26,$A9,$00,$85,$70,$20,$14,$CF,$90,$1C,
      $E0,$54,$D0,$18,$C0,$C9,$D0,$14,$20,$84,$CF,$84,$5E,$88,$84,$71,
      $A0,$06,$84,$5D,$A0,$24,$20,$68,$DE,$4C,$6F,$D4,$60,$24,$0E,$10,
      $0D,$A0,$00,$B1,$64,$AA,$C8,$B1,$64,$A8,$8A,$4C,$91,$D3,$20,$14,
      $CF,$90,$2D,$E0,$54,$D0,$1B,$C0,$49,$D0,$25,$20,$84,$CF,$98,$A2,
      $A0,$4C,$4F,$DC,$20,$DE,$FF,$86,$64,$84,$63,$85,$65,$A0,$00,$84,
      $62,$60,$E0,$53,$D0,$0A,$C0,$54,$D0,$06,$20,$B7,$FF,$4C,$3C,$DC,
      $A5,$64,$A4,$65,$4C,$A2,$DB,$0A,$48,$AA,$20,$73,$00,$E0,$8F,$90,
      $20,$20,$FA,$CE,$20,$9E,$CD,$20,$FD,$CE,$20,$8F,$CD,$68,$AA,$A5,
      $65,$48,$A5,$64,$48,$8A,$48,$20,$9E,$D7,$68,$A8,$8A,$48,$4C,$D6,
      $CF,$20,$F1,$CE,$68,$A8,$B9,$EA,$BF,$85,$55,$B9,$EB,$BF,$85,$56,
      $20,$54,$00,$4C,$8D,$CD,$A0,$FF,$2C,$A0,$00,$84,$0B,$20,$BF,$D1,
      $A5,$64,$45,$0B,$85,$07,$A5,$65,$45,$0B,$85,$08,$20,$FC,$DB,$20,
      $BF,$D1,$A5,$65,$45,$0B,$25,$08,$45,$0B,$A8,$A5,$64,$45,$0B,$25,
      $07,$45,$0B,$4C,$91,$D3,$20,$90,$CD,$B0,$13,$A5,$6E,$09,$7F,$25,
      $6A,$85,$6A,$A9,$69,$A0,$00,$20,$5B,$DC,$AA,$4C,$61,$D0,$A9,$00,
      $85,$0D,$C6,$4D,$20,$A6,$D6,$85,$61,$86,$62,$84,$63,$A5,$6C,$A4,
      $6D,$20,$AA,$D6,$86,$6C,$84,$6D,$AA,$38,$E5,$61,$F0,$08,$A9,$01,
      $90,$04,$A6,$61,$A9,$FF,$85,$66,$A0,$FF,$E8,$C8,$CA,$D0,$07,$A6,
      $66,$30,$0F,$18,$90,$0C,$B1,$6C,$D1,$62,$F0,$EF,$A2,$FF,$B0,$02,
      $A2,$01,$E8,$8A,$2A,$25,$12,$F0,$02,$A9,$FF,$4C,$3C,$DC,$20,$FD,
      $CE,$AA,$20,$90,$D0,$20,$79,$00,$D0,$F4,$60,$A2,$00,$20,$79,$00,
      $86,$0C,$85,$45,$20,$79,$00,$20,$13,$D1,$B0,$03,$4C,$08,$CF,$A2,
      $00,$86,$0D,$86,$0E,$20,$73,$00,$90,$05,$20,$13,$D1,$90,$0B,$AA,
      $20,$73,$00,$90,$FB,$20,$13,$D1,$B0,$F6,$C9,$24,$D0,$06,$A9,$FF,
      $85,$0D,$D0,$10,$C9,$25,$D0,$13,$A5,$10,$D0,$D0,$A9,$80,$85,$0E,
      $05,$45,$85,$45,$8A,$09,$80,$AA,$20,$73,$00,$86,$46,$38,$05,$10,
      $E9,$28,$D0,$03,$4C,$D1,$D1,$A0,$00,$84,$10,$A5,$2D,$A6,$2E,$86,
      $60,$85,$5F,$E4,$30,$D0,$04,$C5,$2F,$F0,$22,$A5,$45,$D1,$5F,$D0,
      $08,$A5,$46,$C8,$D1,$5F,$F0,$7D,$88,$18,$A5,$5F,$69,$07,$90,$E1,
      $E8,$D0,$DC,$C9,$41,$90,$05,$E9,$5B,$38,$E9,$A5,$60,$68,$48,$C9,
      $2A,$D0,$05,$A9,$13,$A0,$DF,$60,$A5,$45,$A4,$46,$C9,$54,$D0,$0B,
      $C0,$C9,$F0,$EF,$C0,$49,$D0,$03,$4C,$08,$CF,$C9,$53,$D0,$04,$C0,
      $54,$F0,$F5,$A5,$2F,$A4,$30,$85,$5F,$84,$60,$A5,$31,$A4,$32,$85,
      $5A,$84,$5B,$18,$69,$07,$90,$01,$C8,$85,$58,$84,$59,$20,$B8,$C3,
      $A5,$58,$A4,$59,$C8,$85,$2F,$84,$30,$A0,$00,$A5,$45,$91,$5F,$C8,
      $A5,$46,$91,$5F,$A9,$00,$C8,$91,$5F,$C8,$91,$5F,$C8,$91,$5F,$C8,
      $91,$5F,$C8,$91,$5F,$A5,$5F,$18,$69,$02,$A4,$60,$90,$01,$C8,$85,
      $47,$84,$48,$60,$A5,$0B,$0A,$69,$05,$65,$5F,$A4,$60,$90,$01,$C8,
      $85,$58,$84,$59,$60,$90,$80,$00,$00,$00,$20,$BF,$D1,$A5,$64,$A4,
      $65,$60,$20,$73,$00,$20,$9E,$CD,$20,$8D,$CD,$A5,$66,$30,$0D,$A5,
      $61,$C9,$90,$90,$09,$A9,$A5,$A0,$D1,$20,$5B,$DC,$D0,$7A,$4C,$9B,
      $DC,$A5,$0C,$05,$0E,$48,$A5,$0D,$48,$A0,$00,$98,$48,$A5,$46,$48,
      $A5,$45,$48,$20,$B2,$D1,$68,$85,$45,$68,$85,$46,$68,$A8,$BA,$BD,
      $02,$01,$48,$BD,$01,$01,$48,$A5,$64,$9D,$02,$01,$A5,$65,$9D,$01,
      $01,$C8,$20,$79,$00,$C9,$2C,$F0,$D2,$84,$0B,$20,$F7,$CE,$68,$85,
      $0D,$68,$85,$0E,$29,$7F,$85,$0C,$A6,$2F,$A5,$30,$86,$5F,$85,$60,
      $C5,$32,$D0,$04,$E4,$31,$F0,$39,$A0,$00,$B1,$5F,$C8,$C5,$45,$D0,
      $06,$A5,$46,$D1,$5F,$F0,$16,$C8,$B1,$5F,$18,$65,$5F,$AA,$C8,$B1,
      $5F,$65,$60,$90,$D7,$A2,$12,$2C,$A2,$0E,$4C,$37,$C4,$A2,$13,$A5,
      $0C,$D0,$F7,$20,$94,$D1,$A5,$0B,$A0,$04,$D1,$5F,$D0,$E7,$4C,$EA,
      $D2,$20,$94,$D1,$20,$08,$C4,$A0,$00,$84,$72,$A2,$05,$A5,$45,$91,
      $5F,$10,$01,$CA,$C8,$A5,$46,$91,$5F,$10,$02,$CA,$CA,$86,$71,$A5,
      $0B,$C8,$C8,$C8,$91,$5F,$A2,$0B,$A9,$00,$24,$0C,$50,$08,$68,$18,
      $69,$01,$AA,$68,$69,$00,$C8,$91,$5F,$C8,$8A,$91,$5F,$20,$4C,$D3,
      $86,$71,$85,$72,$A4,$22,$C6,$0B,$D0,$DC,$65,$59,$B0,$5D,$85,$59,
      $A8,$8A,$65,$58,$90,$03,$C8,$F0,$52,$20,$08,$C4,$85,$31,$84,$32,
      $A9,$00,$E6,$72,$A4,$71,$F0,$05,$88,$91,$58,$D0,$FB,$C6,$59,$C6,
      $72,$D0,$F5,$E6,$59,$38,$A5,$31,$E5,$5F,$A0,$02,$91,$5F,$A5,$32,
      $C8,$E5,$60,$91,$5F,$A5,$0C,$D0,$62,$C8,$B1,$5F,$85,$0B,$A9,$00,
      $85,$71,$85,$72,$C8,$68,$AA,$85,$64,$68,$85,$65,$D1,$5F,$90,$0E,
      $D0,$06,$C8,$8A,$D1,$5F,$90,$07,$4C,$45,$D2,$4C,$35,$C4,$C8,$A5,
      $72,$05,$71,$18,$F0,$0A,$20,$4C,$D3,$8A,$65,$64,$AA,$98,$A4,$22,
      $65,$65,$86,$71,$C6,$0B,$D0,$CA,$85,$72,$A2,$05,$A5,$45,$10,$01,
      $CA,$A5,$46,$10,$02,$CA,$CA,$86,$28,$A9,$00,$20,$55,$D3,$8A,$65,
      $58,$85,$47,$98,$65,$59,$85,$48,$A8,$A5,$47,$60,$84,$22,$B1,$5F,
      $85,$28,$88,$B1,$5F,$85,$29,$A9,$10,$85,$5D,$A2,$00,$A0,$00,$8A,
      $0A,$AA,$98,$2A,$A8,$B0,$A4,$06,$71,$26,$72,$90,$0B,$18,$8A,$65,
      $28,$AA,$98,$65,$29,$A8,$B0,$93,$C6,$5D,$D0,$E3,$60,$A5,$0D,$F0,
      $03,$20,$A6,$D6,$20,$26,$D5,$38,$A5,$33,$E5,$31,$A8,$A5,$34,$E5,
      $32,$A2,$00,$86,$0D,$85,$62,$84,$63,$A2,$90,$4C,$44,$DC,$38,$20,
      $F0,$FF,$A9,$00,$F0,$EB,$A6,$3A,$E8,$D0,$A0,$A2,$15,$2C,$A2,$1B,
      $4C,$37,$C4,$20,$E1,$D3,$20,$A6,$D3,$20,$FA,$CE,$A9,$80,$85,$10,
      $20,$8B,$D0,$20,$8D,$CD,$20,$F7,$CE,$A9,$B2,$20,$FF,$CE,$48,$A5,
      $48,$48,$A5,$47,$48,$A5,$7B,$48,$A5,$7A,$48,$20,$F8,$C8,$4C,$4F,
      $D4,$A9,$A5,$20,$FF,$CE,$09,$80,$85,$10,$20,$92,$D0,$85,$4E,$84,
      $4F,$4C,$8D,$CD,$20,$E1,$D3,$A5,$4F,$48,$A5,$4E,$48,$20,$F1,$CE,
      $20,$8D,$CD,$68,$85,$4E,$68,$85,$4F,$A0,$02,$B1,$4E,$85,$47,$AA,
      $C8,$B1,$4E,$F0,$99,$85,$48,$C8,$B1,$47,$48,$88,$10,$FA,$A4,$48,
      $20,$D4,$DB,$A5,$7B,$48,$A5,$7A,$48,$B1,$4E,$85,$7A,$C8,$B1,$4E,
      $85,$7B,$A5,$48,$48,$A5,$47,$48,$20,$8A,$CD,$68,$85,$4E,$68,$85,
      $4F,$20,$79,$00,$F0,$03,$4C,$08,$CF,$68,$85,$7A,$68,$85,$7B,$A0,
      $00,$68,$91,$4E,$68,$C8,$91,$4E,$68,$C8,$91,$4E,$68,$C8,$91,$4E,
      $68,$C8,$91,$4E,$60,$20,$8D,$CD,$A0,$00,$20,$DF,$DD,$68,$68,$A9,
      $FF,$A0,$00,$F0,$12,$A6,$64,$A4,$65,$86,$50,$84,$51,$20,$F4,$D4,
      $86,$62,$84,$63,$85,$61,$60,$A2,$22,$86,$07,$86,$08,$85,$6F,$84,
      $70,$85,$62,$84,$63,$A0,$FF,$C8,$B1,$6F,$F0,$0C,$C5,$07,$F0,$04,
      $C5,$08,$D0,$F3,$C9,$22,$F0,$01,$18,$84,$61,$98,$65,$6F,$85,$71,
      $A6,$70,$90,$01,$E8,$86,$72,$A5,$70,$F0,$04,$C9,$02,$D0,$0B,$98,
      $20,$75,$D4,$A6,$6F,$A4,$70,$20,$88,$D6,$A6,$16,$E0,$22,$D0,$05,
      $A2,$19,$4C,$37,$C4,$A5,$61,$95,$00,$A5,$62,$95,$01,$A5,$63,$95,
      $02,$A0,$00,$86,$64,$84,$65,$84,$70,$88,$84,$0D,$86,$17,$E8,$E8,
      $E8,$86,$16,$60,$46,$0F,$48,$49,$FF,$38,$65,$33,$A4,$34,$B0,$01,
      $88,$C4,$32,$90,$11,$D0,$04,$C5,$31,$90,$0B,$85,$33,$84,$34,$85,
      $35,$84,$36,$AA,$68,$60,$A2,$10,$A5,$0F,$30,$B6,$20,$26,$D5,$A9,
      $80,$85,$0F,$68,$D0,$D0,$A6,$37,$A5,$38,$86,$33,$85,$34,$A0,$00,
      $84,$4F,$84,$4E,$A5,$31,$A6,$32,$85,$5F,$86,$60,$A9,$19,$A2,$00,
      $85,$22,$86,$23,$C5,$16,$F0,$05,$20,$C7,$D5,$F0,$F7,$A9,$07,$85,
      $53,$A5,$2D,$A6,$2E,$85,$22,$86,$23,$E4,$30,$D0,$04,$C5,$2F,$F0,
      $05,$20,$BD,$D5,$F0,$F3,$85,$58,$86,$59,$A9,$03,$85,$53,$A5,$58,
      $A6,$59,$E4,$32,$D0,$07,$C5,$31,$D0,$03,$4C,$06,$D6,$85,$22,$86,
      $23,$A0,$00,$B1,$22,$AA,$C8,$B1,$22,$08,$C8,$B1,$22,$65,$58,$85,
      $58,$C8,$B1,$22,$65,$59,$85,$59,$28,$10,$D3,$8A,$30,$D0,$C8,$B1,
      $22,$A0,$00,$0A,$69,$05,$65,$22,$85,$22,$90,$02,$E6,$23,$A6,$23,
      $E4,$59,$D0,$04,$C5,$58,$F0,$BA,$20,$C7,$D5,$F0,$F3,$B1,$22,$30,
      $35,$C8,$B1,$22,$10,$30,$C8,$B1,$22,$F0,$2B,$C8,$B1,$22,$AA,$C8,
      $B1,$22,$C5,$34,$90,$06,$D0,$1E,$E4,$33,$B0,$1A,$C5,$60,$90,$16,
      $D0,$04,$E4,$5F,$90,$10,$86,$5F,$85,$60,$A5,$22,$A6,$23,$85,$4E,
      $86,$4F,$A5,$53,$85,$55,$A5,$53,$18,$65,$22,$85,$22,$90,$02,$E6,
      $23,$A6,$23,$A0,$00,$60,$A5,$4F,$05,$4E,$F0,$F5,$A5,$55,$29,$04,
      $4A,$A8,$85,$55,$B1,$4E,$65,$5F,$85,$5A,$A5,$60,$69,$00,$85,$5B,
      $A5,$33,$A6,$34,$85,$58,$86,$59,$20,$BF,$C3,$A4,$55,$C8,$A5,$58,
      $91,$4E,$AA,$E6,$59,$A5,$59,$C8,$91,$4E,$4C,$2A,$D5,$A5,$65,$48,
      $A5,$64,$48,$20,$83,$CE,$20,$8F,$CD,$68,$85,$6F,$68,$85,$70,$A0,
      $00,$B1,$6F,$18,$71,$64,$90,$05,$A2,$17,$4C,$37,$C4,$20,$75,$D4,
      $20,$7A,$D6,$A5,$50,$A4,$51,$20,$AA,$D6,$20,$8C,$D6,$A5,$6F,$A4,
      $70,$20,$AA,$D6,$20,$CA,$D4,$4C,$B8,$CD,$A0,$00,$B1,$6F,$48,$C8,
      $B1,$6F,$AA,$C8,$B1,$6F,$A8,$68,$86,$22,$84,$23,$A8,$F0,$0A,$48,
      $88,$B1,$22,$91,$35,$98,$D0,$F8,$68,$18,$65,$35,$85,$35,$90,$02,
      $E6,$36,$60,$20,$8F,$CD,$A5,$64,$A4,$65,$85,$22,$84,$23,$20,$DB,
      $D6,$08,$A0,$00,$B1,$22,$48,$C8,$B1,$22,$AA,$C8,$B1,$22,$A8,$68,
      $28,$D0,$13,$C4,$34,$D0,$0F,$E4,$33,$D0,$0B,$48,$18,$65,$33,$85,
      $33,$90,$02,$E6,$34,$68,$86,$22,$84,$23,$60,$C4,$18,$D0,$0C,$C5,
      $17,$D0,$08,$85,$16,$E9,$03,$85,$17,$A0,$00,$60,$20,$A1,$D7,$8A,
      $48,$A9,$01,$20,$7D,$D4,$68,$A0,$00,$91,$62,$68,$68,$4C,$CA,$D4,
      $20,$61,$D7,$D1,$50,$98,$90,$04,$B1,$50,$AA,$98,$48,$8A,$48,$20,
      $7D,$D4,$A5,$50,$A4,$51,$20,$AA,$D6,$68,$A8,$68,$18,$65,$22,$85,
      $22,$90,$02,$E6,$23,$98,$20,$8C,$D6,$4C,$CA,$D4,$20,$61,$D7,$18,
      $F1,$50,$49,$FF,$4C,$06,$D7,$A9,$FF,$85,$65,$20,$79,$00,$C9,$29,
      $F0,$06,$20,$FD,$CE,$20,$9E,$D7,$20,$61,$D7,$F0,$4B,$CA,$8A,$48,
      $18,$A2,$00,$F1,$50,$B0,$B6,$49,$FF,$C5,$65,$90,$B1,$A5,$65,$B0,
      $AD,$20,$F7,$CE,$68,$A8,$68,$85,$55,$68,$68,$68,$AA,$68,$85,$50,
      $68,$85,$51,$A5,$55,$48,$98,$48,$A0,$00,$8A,$60,$20,$82,$D7,$4C,
      $A2,$D3,$20,$A3,$D6,$A2,$00,$86,$0D,$A8,$60,$20,$82,$D7,$F0,$08,
      $A0,$00,$B1,$22,$A8,$4C,$A2,$D3,$4C,$48,$D2,$20,$73,$00,$20,$8A,
      $CD,$20,$B8,$D1,$A6,$64,$D0,$F0,$A6,$65,$4C,$79,$00,$20,$82,$D7,
      $D0,$03,$4C,$F7,$D8,$A6,$7A,$A4,$7B,$86,$71,$84,$72,$A6,$22,$86,
      $7A,$18,$65,$22,$85,$24,$A6,$23,$86,$7B,$90,$01,$E8,$86,$25,$A0,
      $00,$B1,$24,$48,$98,$91,$24,$20,$79,$00,$20,$F3,$DC,$68,$A0,$00,
      $91,$24,$A6,$71,$A4,$72,$86,$7A,$84,$7B,$60,$20,$8A,$CD,$20,$F7,
      $D7,$20,$FD,$CE,$4C,$9E,$D7,$A5,$66,$30,$9D,$A5,$61,$C9,$91,$B0,
      $97,$20,$9B,$DC,$A5,$64,$A4,$65,$84,$14,$85,$15,$60,$A5,$15,$48,
      $A5,$14,$48,$20,$F7,$D7,$A0,$00,$B1,$14,$A8,$68,$85,$14,$68,$85,
      $15,$4C,$A2,$D3,$20,$EB,$D7,$8A,$A0,$00,$91,$14,$60,$20,$EB,$D7,
      $86,$49,$A2,$00,$20,$79,$00,$F0,$03,$20,$F1,$D7,$86,$4A,$A0,$00,
      $B1,$14,$45,$4A,$25,$49,$F0,$F8,$60,$A9,$11,$A0,$DF,$4C,$67,$D8,
      $20,$8C,$DA,$A5,$66,$49,$FF,$85,$66,$45,$6E,$85,$6F,$A5,$61,$4C,
      $6A,$D8,$20,$99,$D9,$90,$3C,$20,$8C,$DA,$D0,$03,$4C,$FC,$DB,$A6,
      $70,$86,$56,$A2,$69,$A5,$69,$A8,$F0,$CE,$38,$E5,$61,$F0,$24,$90,
      $12,$84,$61,$A4,$6E,$84,$66,$49,$FF,$69,$00,$A0,$00,$84,$56,$A2,
      $61,$D0,$04,$A0,$00,$84,$70,$C9,$F9,$30,$C7,$A8,$A5,$70,$56,$01,
      $20,$B0,$D9,$24,$6F,$10,$57,$A0,$61,$E0,$69,$F0,$02,$A0,$69,$38,
      $49,$FF,$65,$56,$85,$70,$B9,$04,$00,$F5,$04,$85,$65,$B9,$03,$00,
      $F5,$03,$85,$64,$B9,$02,$00,$F5,$02,$85,$63,$B9,$01,$00,$F5,$01,
      $85,$62,$B0,$03,$20,$47,$D9,$A0,$00,$98,$18,$A6,$62,$D0,$4A,$A6,
      $63,$86,$62,$A6,$64,$86,$63,$A6,$65,$86,$64,$A6,$70,$86,$65,$84,
      $70,$69,$08,$C9,$20,$D0,$E4,$A9,$00,$85,$61,$85,$66,$60,$65,$56,
      $85,$70,$A5,$65,$65,$6D,$85,$65,$A5,$64,$65,$6C,$85,$64,$A5,$63,
      $65,$6B,$85,$63,$A5,$62,$65,$6A,$85,$62,$4C,$36,$D9,$69,$01,$06,
      $70,$26,$65,$26,$64,$26,$63,$26,$62,$10,$F2,$38,$E5,$61,$B0,$C7,
      $49,$FF,$69,$01,$85,$61,$90,$0E,$E6,$61,$F0,$42,$66,$62,$66,$63,
      $66,$64,$66,$65,$66,$70,$60,$A5,$66,$49,$FF,$85,$66,$A5,$62,$49,
      $FF,$85,$62,$A5,$63,$49,$FF,$85,$63,$A5,$64,$49,$FF,$85,$64,$A5,
      $65,$49,$FF,$85,$65,$A5,$70,$49,$FF,$85,$70,$E6,$70,$D0,$0E,$E6,
      $65,$D0,$0A,$E6,$64,$D0,$06,$E6,$63,$D0,$02,$E6,$62,$60,$A2,$0F,
      $4C,$37,$C4,$A2,$25,$B4,$04,$84,$70,$B4,$03,$94,$04,$B4,$02,$94,
      $03,$B4,$01,$94,$02,$A4,$68,$94,$01,$69,$08,$30,$E8,$F0,$E6,$E9,
      $08,$A8,$A5,$70,$B0,$14,$16,$01,$90,$02,$F6,$01,$76,$01,$76,$01,
      $76,$02,$76,$03,$76,$04,$6A,$C8,$D0,$EC,$18,$60,$81,$00,$00,$00,
      $00,$03,$7F,$5E,$56,$CB,$79,$80,$13,$9B,$0B,$64,$80,$76,$38,$93,
      $16,$82,$38,$AA,$3B,$20,$80,$35,$04,$F3,$34,$81,$35,$04,$F3,$34,
      $80,$80,$00,$00,$00,$80,$31,$72,$17,$F8,$20,$2B,$DC,$F0,$02,$10,
      $03,$4C,$48,$D2,$A5,$61,$E9,$7F,$48,$A9,$80,$85,$61,$A9,$D6,$A0,
      $D9,$20,$67,$D8,$A9,$DB,$A0,$D9,$20,$0F,$DB,$A9,$BC,$A0,$D9,$20,
      $50,$D8,$A9,$C1,$A0,$D9,$20,$40,$E0,$A9,$E0,$A0,$D9,$20,$67,$D8,
      $68,$20,$7E,$DD,$A9,$E5,$A0,$D9,$20,$8C,$DA,$D0,$03,$4C,$8B,$DA,
      $20,$B7,$DA,$A9,$00,$85,$26,$85,$27,$85,$28,$85,$29,$A5,$70,$20,
      $59,$DA,$A5,$65,$20,$59,$DA,$A5,$64,$20,$59,$DA,$A5,$63,$20,$59,
      $DA,$A5,$62,$20,$5E,$DA,$4C,$8F,$DB,$D0,$03,$4C,$83,$D9,$4A,$09,
      $80,$A8,$90,$19,$18,$A5,$29,$65,$6D,$85,$29,$A5,$28,$65,$6C,$85,
      $28,$A5,$27,$65,$6B,$85,$27,$A5,$26,$65,$6A,$85,$26,$66,$26,$66,
      $27,$66,$28,$66,$29,$66,$70,$98,$4A,$D0,$D6,$60,$85,$22,$84,$23,
      $A0,$04,$B1,$22,$85,$6D,$88,$B1,$22,$85,$6C,$88,$B1,$22,$85,$6B,
      $88,$B1,$22,$85,$6E,$45,$66,$85,$6F,$A5,$6E,$09,$80,$85,$6A,$88,
      $B1,$22,$85,$69,$A5,$61,$60,$A5,$69,$F0,$1F,$18,$65,$61,$90,$04,
      $30,$1D,$18,$2C,$10,$14,$69,$80,$85,$61,$D0,$03,$4C,$FB,$D8,$A5,
      $6F,$85,$66,$60,$A5,$66,$49,$FF,$30,$05,$68,$68,$4C,$F7,$D8,$4C,
      $7E,$D9,$20,$0C,$DC,$AA,$F0,$10,$18,$69,$02,$B0,$F2,$A2,$00,$86,
      $6F,$20,$77,$D8,$E6,$61,$F0,$E7,$60,$84,$20,$00,$00,$00,$20,$0C,
      $DC,$A9,$F9,$A0,$DA,$A2,$00,$86,$6F,$20,$A2,$DB,$4C,$12,$DB,$20,
      $8C,$DA,$F0,$76,$20,$1B,$DC,$A9,$00,$38,$E5,$61,$85,$61,$20,$B7,
      $DA,$E6,$61,$F0,$BA,$A2,$FC,$A9,$01,$A4,$6A,$C4,$62,$D0,$10,$A4,
      $6B,$C4,$63,$D0,$0A,$A4,$6C,$C4,$64,$D0,$04,$A4,$6D,$C4,$65,$08,
      $2A,$90,$09,$E8,$95,$29,$F0,$32,$10,$34,$A9,$01,$28,$B0,$0E,$06,
      $6D,$26,$6C,$26,$6B,$26,$6A,$B0,$E6,$30,$CE,$10,$E2,$A8,$A5,$6D,
      $E5,$65,$85,$6D,$A5,$6C,$E5,$64,$85,$6C,$A5,$6B,$E5,$63,$85,$6B,
      $A5,$6A,$E5,$62,$85,$6A,$98,$4C,$4F,$DB,$A9,$40,$D0,$CE,$0A,$0A,
      $0A,$0A,$0A,$0A,$85,$70,$28,$4C,$8F,$DB,$A2,$14,$4C,$37,$C4,$A5,
      $26,$85,$62,$A5,$27,$85,$63,$A5,$28,$85,$64,$A5,$29,$85,$65,$4C,
      $D7,$D8,$85,$22,$84,$23,$A0,$04,$B1,$22,$85,$65,$88,$B1,$22,$85,
      $64,$88,$B1,$22,$85,$63,$88,$B1,$22,$85,$66,$09,$80,$85,$62,$88,
      $B1,$22,$85,$61,$84,$70,$60,$A2,$5C,$2C,$A2,$57,$A0,$00,$F0,$04,
      $A6,$49,$A4,$4A,$20,$1B,$DC,$86,$22,$84,$23,$A0,$04,$A5,$65,$91,
      $22,$88,$A5,$64,$91,$22,$88,$A5,$63,$91,$22,$88,$A5,$66,$09,$7F,
      $25,$62,$91,$22,$88,$A5,$61,$91,$22,$84,$70,$60,$A5,$6E,$85,$66,
      $A2,$05,$B5,$68,$95,$60,$CA,$D0,$F9,$86,$70,$60,$20,$1B,$DC,$A2,
      $06,$B5,$60,$95,$68,$CA,$D0,$F9,$86,$70,$60,$A5,$61,$F0,$FB,$06,
      $70,$90,$F7,$20,$6F,$D9,$D0,$F2,$4C,$38,$D9,$A5,$61,$F0,$09,$A5,
      $66,$2A,$A9,$FF,$B0,$02,$A9,$01,$60,$20,$2B,$DC,$85,$62,$A9,$00,
      $85,$63,$A2,$88,$A5,$62,$49,$FF,$2A,$A9,$00,$85,$65,$85,$64,$86,
      $61,$85,$70,$85,$66,$4C,$D2,$D8,$46,$66,$60,$85,$24,$84,$25,$A0,
      $00,$B1,$24,$C8,$AA,$F0,$C4,$B1,$24,$45,$66,$30,$C2,$E4,$61,$D0,
      $21,$B1,$24,$09,$80,$C5,$62,$D0,$19,$C8,$B1,$24,$C5,$63,$D0,$12,
      $C8,$B1,$24,$C5,$64,$D0,$0B,$C8,$A9,$7F,$C5,$70,$B1,$24,$E5,$65,
      $F0,$28,$A5,$66,$90,$02,$49,$FF,$4C,$31,$DC,$A5,$61,$F0,$4A,$38,
      $E9,$A0,$24,$66,$10,$09,$AA,$A9,$FF,$85,$68,$20,$4D,$D9,$8A,$A2,
      $61,$C9,$F9,$10,$06,$20,$99,$D9,$84,$68,$60,$A8,$A5,$66,$29,$80,
      $46,$62,$05,$62,$85,$62,$20,$B0,$D9,$84,$68,$60,$A5,$61,$C9,$A0,
      $B0,$20,$20,$9B,$DC,$84,$70,$A5,$66,$84,$66,$49,$80,$2A,$A9,$A0,
      $85,$61,$A5,$65,$85,$07,$4C,$D2,$D8,$85,$62,$85,$63,$85,$64,$85,
      $65,$A8,$60,$A0,$00,$A2,$0A,$94,$5D,$CA,$10,$FB,$90,$0F,$C9,$2D,
      $D0,$04,$86,$67,$F0,$04,$C9,$2B,$D0,$05,$20,$73,$00,$90,$5B,$C9,
      $2E,$F0,$2E,$C9,$45,$D0,$30,$20,$73,$00,$90,$17,$C9,$AB,$F0,$0E,
      $C9,$2D,$F0,$0A,$C9,$AA,$F0,$08,$C9,$2B,$F0,$04,$D0,$07,$66,$60,
      $20,$73,$00,$90,$5C,$24,$60,$10,$0E,$A9,$00,$38,$E5,$5E,$4C,$49,
      $DD,$66,$5F,$24,$5F,$50,$C3,$A5,$5E,$38,$E5,$5D,$85,$5E,$F0,$12,
      $10,$09,$20,$FE,$DA,$E6,$5E,$D0,$F9,$F0,$07,$20,$E2,$DA,$C6,$5E,
      $D0,$F9,$A5,$67,$30,$01,$60,$4C,$B4,$DF,$48,$24,$5F,$10,$02,$E6,
      $5D,$20,$E2,$DA,$68,$38,$E9,$30,$20,$7E,$DD,$4C,$0A,$DD,$48,$20,
      $0C,$DC,$68,$20,$3C,$DC,$A5,$6E,$45,$66,$85,$6F,$A6,$61,$4C,$6A,
      $D8,$A5,$5E,$C9,$0A,$90,$09,$A9,$64,$24,$60,$30,$11,$4C,$7E,$D9,
      $0A,$0A,$18,$65,$5E,$0A,$18,$A0,$00,$71,$7A,$38,$E9,$30,$85,$5E,
      $4C,$30,$DD,$9B,$3E,$BC,$1F,$FD,$9E,$6E,$6B,$27,$FD,$9E,$6E,$6B,
      $28,$00,$A9,$71,$A0,$C3,$20,$DA,$DD,$A5,$3A,$A6,$39,$85,$62,$86,
      $63,$A2,$90,$38,$20,$49,$DC,$20,$DF,$DD,$4C,$1E,$CB,$A0,$01,$A9,
      $20,$24,$66,$10,$02,$A9,$2D,$99,$FF,$00,$85,$66,$84,$71,$C8,$A9,
      $30,$A6,$61,$D0,$03,$4C,$04,$DF,$A9,$00,$E0,$80,$F0,$02,$B0,$09,
      $A9,$BD,$A0,$DD,$20,$28,$DA,$A9,$F7,$85,$5D,$A9,$B8,$A0,$DD,$20,
      $5B,$DC,$F0,$1E,$10,$12,$A9,$B3,$A0,$DD,$20,$5B,$DC,$F0,$02,$10,
      $0E,$20,$E2,$DA,$C6,$5D,$D0,$EE,$20,$FE,$DA,$E6,$5D,$D0,$DC,$20,
      $49,$D8,$20,$9B,$DC,$A2,$01,$A5,$5D,$18,$69,$0A,$30,$09,$C9,$0B,
      $B0,$06,$69,$FF,$AA,$A9,$02,$38,$E9,$02,$85,$5E,$86,$5D,$8A,$F0,
      $02,$10,$13,$A4,$71,$A9,$2E,$C8,$99,$FF,$00,$8A,$F0,$06,$A9,$30,
      $C8,$99,$FF,$00,$84,$71,$A0,$00,$A2,$80,$A5,$65,$18,$79,$19,$DF,
      $85,$65,$A5,$64,$79,$18,$DF,$85,$64,$A5,$63,$79,$17,$DF,$85,$63,
      $A5,$62,$79,$16,$DF,$85,$62,$E8,$B0,$04,$10,$DE,$30,$02,$30,$DA,
      $8A,$90,$04,$49,$FF,$69,$0A,$69,$2F,$C8,$C8,$C8,$C8,$84,$47,$A4,
      $71,$C8,$AA,$29,$7F,$99,$FF,$00,$C6,$5D,$D0,$06,$A9,$2E,$C8,$99,
      $FF,$00,$84,$71,$A4,$47,$8A,$49,$FF,$29,$80,$AA,$C0,$24,$F0,$04,
      $C0,$3C,$D0,$A6,$A4,$71,$B9,$FF,$00,$88,$C9,$30,$F0,$F8,$C9,$2E,
      $F0,$01,$C8,$A9,$2B,$A6,$5E,$F0,$2E,$10,$08,$A9,$00,$38,$E5,$5E,
      $AA,$A9,$2D,$99,$01,$01,$A9,$45,$99,$00,$01,$8A,$A2,$2F,$38,$E8,
      $E9,$0A,$B0,$FB,$69,$3A,$99,$03,$01,$8A,$99,$02,$01,$A9,$00,$99,
      $04,$01,$F0,$08,$99,$FF,$00,$A9,$00,$99,$00,$01,$A9,$00,$A0,$01,
      $60,$80,$00,$00,$00,$00,$FA,$0A,$1F,$00,$00,$98,$96,$80,$FF,$F0,
      $BD,$C0,$00,$01,$86,$A0,$FF,$FF,$D8,$F0,$00,$00,$03,$E8,$FF,$FF,
      $FF,$9C,$00,$00,$00,$0A,$FF,$FF,$FF,$FF,$FF,$DF,$0A,$80,$00,$03,
      $4B,$C0,$FF,$FF,$73,$60,$00,$00,$0E,$10,$FF,$FF,$FD,$A8,$00,$00,
      $00,$3C,$BF,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,
      $AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,$AA,
      $AA,$20,$0C,$DC,$A9,$11,$A0,$DF,$20,$A2,$DB,$F0,$70,$A5,$69,$D0,
      $03,$4C,$F9,$D8,$A2,$4E,$A0,$00,$20,$D4,$DB,$A5,$6E,$10,$0F,$20,
      $CC,$DC,$A9,$4E,$A0,$00,$20,$5B,$DC,$D0,$03,$98,$A4,$07,$20,$FE,
      $DB,$98,$48,$20,$EA,$D9,$A9,$4E,$A0,$00,$20,$28,$DA,$20,$ED,$DF,
      $68,$4A,$90,$0A,$A5,$61,$F0,$06,$A5,$66,$49,$FF,$85,$66,$60,$81,
      $38,$AA,$3B,$29,$07,$71,$34,$58,$3E,$56,$74,$16,$7E,$B3,$1B,$77,
      $2F,$EE,$E3,$85,$7A,$1D,$84,$1C,$2A,$7C,$63,$59,$58,$0A,$7E,$75,
      $FD,$E7,$C6,$80,$31,$72,$18,$10,$81,$00,$00,$00,$00,$A9,$BF,$A0,
      $DF,$20,$28,$DA,$A5,$70,$69,$50,$90,$03,$20,$23,$DC,$85,$56,$20
         );

function GetVIC20_Rom(id:longword; var sz:longword):pointer;
begin
   Result :=  @Bin_kernal;  //0
   sz := Bin_kernal_len;
   if id = 1 then
   begin
      Result :=  @Bin_basic;  //1
      sz := Bin_basic_len;
   end;
   if id = 2 then
   begin
      Result :=  @Bin_chargen;  //2
      sz := Bin_chargen_len;
   end;
end;


end.
