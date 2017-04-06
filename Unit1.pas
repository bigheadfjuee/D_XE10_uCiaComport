unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, uCiaComport,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    btnSend: TButton;
    procedure btnSendClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    procedure PortDataAvailable(Sender: TObject);
  public
    { Public declarations }
    comm: TCiaComPort;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnSendClick(Sender: TObject);
var
  arr: array [1 .. 8] of Byte;
begin
  arr[1] := $02;
  arr[2] := $00;
  arr[3] := $00;
  arr[4] := $00;
  arr[5] := $0A;
  arr[6] := $00;
  arr[7] := $FF;
  arr[8] := $F7;

  comm.Send(@arr[1], 8);

end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  comm.DisposeOf();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  comm := TCiaComPort.Create(self);
  comm.Port := 4;
  comm.Baudrate := 9200;
  comm.ByteSize := 8;
  comm.Parity := ptNone;
  comm.StopBits := sbOne;
  comm.LineMode := false;
  comm.OnDataAvailable := PortDataAvailable;

  comm.Open := true;
end;

procedure TForm1.PortDataAvailable(Sender: TObject);
var
  i, j: integer;
  P: PByte;
  str: String;
begin
  j := comm.RxCount;
  GetMem(P, j);

  try
    comm.Receive(P, j);

    for i := 0 to j do
      str := str + P[i].ToHexString + ' ';

  finally
    FreeMem(P);
  end;

  Memo1.Lines.Add(str)

end;

end.
