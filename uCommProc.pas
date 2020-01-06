﻿unit uCommProc;

{
  Tony :
  使用 readBuffer 當作 Queue 去暫存 Comm.Receive 收到的 Byte
  使用 TThreadProc 去解析 固定長度(8) 的指令
}
interface

uses
  System.Classes, System.SysUtils, uCiaComport, Vcl.StdCtrls;

const
  BUFF_SIZE = 100;
  CMD_LENGTH = 8;

type
  TCommProc = class(TComponent)
  private
    Comm: TCiaComPort;
    readIndex, procIndex: Integer;
    readBuffer: array [1 .. BUFF_SIZE] of Byte;
    procedure PortDataAvailable(Sender: TObject);

  public
    Memo: TMemo;
    constructor Create();
    destructor Destroy; override;
    procedure Send();
    procedure ParseCmd();
  end;

type
  TThreadProc = class(TThread)
  protected
    procedure Execute; override;
  end;

var
  CommProc: TCommProc;

implementation

var
  ThreadProc: TThreadProc;

constructor TCommProc.Create();
begin
  readIndex := 1;
  procIndex := 1;

  Comm := TCiaComPort.Create(self);
  Comm.Port := 1;
  Comm.Baudrate := 9600;
  Comm.ByteSize := 8;
  Comm.Parity := ptNone;
  Comm.StopBits := sbOne;
  Comm.LineMode := false;
  Comm.OnDataAvailable := PortDataAvailable;

  Comm.Open := true;
  ThreadProc := TThreadProc.Create();
end;

destructor TCommProc.Destroy;
begin
  ThreadProc.Terminate();
  Comm.Destroy();
end;

procedure TCommProc.Send;
var
  arr: array [1 .. 13] of Byte;
  str : string;
  ck: Byte;
  i: Integer;
begin
  arr[1] := $A5;
  arr[2] := $AC;
  arr[3] := $0D; // Byte3
  arr[4] := $00;
  arr[5] := $01; // cmd
  arr[6] := $01; // p1
  arr[7] := $00; // p2
  arr[8] := $00;
  arr[9] := $00;
  arr[10] := $0; // ch
  arr[11] := $0; // ch

  str := 'A5 AC ';

  ck := $00;
  for i := 3 to 11 do
  begin
    ck := ck + arr[i];
    str := str + IntToHex(arr[i],2) + ' ';
  end;

  arr[12] := ck;
  Comm.Send(@arr, 12);

  str := str + IntToHex(ck,2);
  Memo.Lines.Add(str);

  // Comm.SendStr('test1234');
end;

procedure TCommProc.PortDataAvailable(Sender: TObject);
var
  count, tail, head: Integer;
  P: PByte;
  str: String;
begin
  count := Comm.RxCount;

  try

    if readIndex + count <= BUFF_SIZE then
    begin
      Comm.Receive(@readBuffer[readIndex], count);
      readIndex := readIndex + count;
    end
    else
    begin
      tail := BUFF_SIZE - readIndex;
      Comm.Receive(@readBuffer[readIndex], tail);
      head := count - tail;

      Comm.Receive(@readBuffer[1], head);
      readIndex := head + 1;

    end;

    // Memo.Lines.Add(Comm.ReceiveStr);
  finally

  end;

end;

procedure TCommProc.ParseCmd;
var
  len, i, index: Integer;
  str: String;
begin

  len := readIndex - procIndex;

  if len = 0 then
    exit;

  if (len > 0) and (len < CMD_LENGTH) then
    exit;

  if (len < 0) then
    if BUFF_SIZE - procIndex + readIndex < CMD_LENGTH then
      exit;

  for i := 0 to CMD_LENGTH - 1 do
  begin
    index := procIndex + i;
    if index <= BUFF_SIZE then
      str := str + readBuffer[index].ToHexString(2) + ' '
    else
      str := str + readBuffer[index - BUFF_SIZE].ToHexString(2) + ' ';
  end;

  procIndex := procIndex + CMD_LENGTH;
  if procIndex > BUFF_SIZE then
    procIndex := procIndex - BUFF_SIZE + 1;

  if Assigned(Memo) then
    Memo.Lines.Add(str);

end;

procedure TThreadProc.Execute;
begin
  NameThreadForDebugging('TThreadProc');
  { Place thread code here }
  while not Terminated do
  begin
    Synchronize(CommProc.ParseCmd);
    Sleep(200);
  end;
end;

end.
