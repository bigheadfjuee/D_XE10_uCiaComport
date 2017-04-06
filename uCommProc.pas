unit uCommProc;

{
  Tony :
  �ϥ� readBuffer ���@ Queue �h�Ȧs Comm.Receive ���쪺 Byte
  �ϥ� TThreadProc �h�ѪR �T�w����(8) �����O
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
  Comm.Baudrate := 9200;
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
  arr: array [1 .. 8] of Byte;
  lrc: Byte;
  i: Integer;
begin
  arr[1] := $01;
  arr[2] := $02;
  arr[3] := $03;
  arr[4] := $04;
  arr[5] := $05;
  arr[6] := $06;
  arr[7] := $07;

  lrc := $00;
  for i := 1 to 7 do
    lrc := lrc xor arr[i];

  arr[8] := lrc;

  Comm.Send(@arr[1], 8);
end;

procedure TCommProc.PortDataAvailable(Sender: TObject);
var
  i, count, tail, head: Integer;
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