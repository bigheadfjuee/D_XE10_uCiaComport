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
  public
    { Public declarations }

  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses uCommProc;

procedure TForm1.btnSendClick(Sender: TObject);
begin
  CommProc.Send();
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CommProc.Destroy();
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  CommProc := TCommProc.Create();
  CommProc.Memo := Memo1;
end;

end.
