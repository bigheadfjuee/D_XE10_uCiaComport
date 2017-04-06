unit uCiaComport;

interface

uses  Windows, Classes, Messages, SysUtils;

const
   CIA_COMMVERSION    = '1.17';
   WM_CLOSEPORT       = WM_USER + 1;
   WM_RELEASEPORT     = WM_USER + 2;
   WM_DATAAVAILABLE   = WM_USER + 3;
   WM_DATASENT        = WM_USER + 4;
   WM_CTSCHANGED      = WM_USER + 5;
   WM_DSRCHANGED      = WM_USER + 6;
   WM_PORTERROR       = WM_USER + 7;
   WM_RESET_RX_TIMOUT = WM_USER + 8;
//   WM_OPENPORT        = WM_USER + 9;

type
  TPortSendProgressEvent = procedure(Sender: TObject; Count: integer) of object;
  TPortLineEvent         = procedure(Sender: TObject; State: boolean) of object;
  TPortErrEvent          = procedure(Sender: TObject; Error: Cardinal) of object;
  TPortExceptionEvent    = procedure(Sender: TObject; E: Exception) of object;

  TCiaComPort = class;
  ECiaComPort = class(Exception);
  TStopBits   = (sbOne, sbOne_5, sbTwo);
  TParity     = (ptNone, ptOdd, ptEven, ptMark, ptSpace);

  TCiaTimer = class(TObject)
  private
    FInterval: Cardinal;
    FHandle: HWND;
    FEnabled: Boolean;
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
  public
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property Handle: HWND read FHandle write FHandle;
  end;

  TCiaCustomBuffer = class
  private
    FData: PByte;
    FCapacity: cardinal;
    FReadPtr: integer;
    FWritePtr: integer;
  public
    destructor Destroy; override;
    procedure Grow(Count: cardinal);
  end;

  TCiaCommBuffer = class(TCiaCustomBuffer)
  private
    FLineEndPtr: Integer;
  end;

  TCiaTxBuffer = class(TCiaCustomBuffer)
  private
    FCount: integer;
  public
    procedure Clear;
    procedure Empty;
    procedure Write(Data: Pointer; Len: integer);
    function  Read(var Data: PByte; var Len: integer): boolean;
    property Count: integer read FCount;
  end;

  TCiaCommThread = class(TThread)
  private
    FModemStatus: Cardinal;
    FCiaComPort: TCiaComPort;
    FEventMask: cardinal;
    FRxCount: cardinal;
    FInternalRxCount: cardinal;
    FRcvBuffer: TCiaCommBuffer;  // if LineMode then we receive in our own buffer
    FCloseFlag: Boolean;         // F.Piette
    procedure PortEvent;
    procedure InternalReceive;
    function  CheckLineEnd(P: PByte): boolean;
  public
    ComHandle: THandle;
    CloseEvent: THandle;
    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override; // F.Piette
    procedure Execute; override;
  end;

  TDtrControl = (dtrDisable, dtrEnable, dtrHandshake);
  TRtsControl = (rtsDisable, rtsEnable, rtsHandshake, rtsToggle);
  TFlowCtrl = class(TPersistent)
  private
    FFlags: LongInt;
    FComPort: TCiaComPort;
    FRxDtrControl: TDtrControl;
    FRxRtsControl: TRtsControl;
    FRxDsrSensivity: boolean;
    FTxContinueXoff: boolean;
    FTxCtsFlow: boolean;
    FTxDsrFlow: boolean;
    FXonOff: boolean;
    constructor Create(Port: TCiaComPort);
    procedure SetRxDtrControl(Value: TDtrControl);
    procedure SetRxRtsControl(Value: TRtsControl);
    procedure SetRxDsrSensivity(Value: boolean);
    procedure SetTxContinueXoff(Value: boolean);
    procedure SetTxCtsFlow(Value: boolean);
    procedure SetTxDsrFlow(Value: boolean);
    procedure SetXonOff(Value: boolean);
    procedure ChangeCommState;
  published
    property RxDsrSensivity: boolean read FRxDsrSensivity write SetRxDsrSensivity;
    property RxDtrControl: TDtrControl read FRxDtrControl write SetRxDtrControl;
    property RxRtsControl: TRtsControl read FRxRtsControl write SetRxRtsControl;
    property TxContinueXoff: boolean read FTxContinueXoff write SetTxContinueXoff;
    property TxCtsFlow: boolean read FTxCtsFlow write SetTxCtsFlow;
    property TxDsrFlow: boolean read FTxDsrFlow write SetTxDsrFlow;
    property XonXoff: boolean read FXonOff write SetXOnOff;
  end;

  TCiaPortOptions = (EventCtsDsr, EventError, DontUseInternalBuffer);
  TPortOptions    = set of TCiaPortOptions;

  TCiaComPort = class(TComponent)
  private
    FSendCount: integer;
    FSending: boolean;
    FTxBuf: TCiaTxBuffer;
    FDestroying: boolean;
    FHandle: THandle;
    FPortOptions: TPortOptions;
    FRxThresholdAmount: integer;
    FRxThresholdUse: boolean;
    FFlowCtrl: TFlowCtrl;
    FLineMode: boolean;
    FLineEnd: RawByteString;
    FRxTimeout: integer;
    FTimeoutTimer: TCiaTimer;
    FBaudrate: integer;
    FByteSize: byte;
    FStopbits: TStopBits;
    FParity: TParity;
    FVersion: string;
    FOpen: boolean;
    FPort: integer;
    FRxBuffer: cardinal;
    FXOffLimit: dword;
    FXOnLimit: dword;
    FTxBuffer: cardinal;
    FCommThread: TCiaCommThread;
    FOnDataAvailable: TNotifyEvent;
    FOnDataSent: TNotifyEvent;
    FOnCtsChanged: TPortLineEvent;
    FOnDsrChanged: TPortLineEvent;
    FOnError: TPortErrEvent;
    FOnBGException: TPortExceptionEvent;
    OverlapTx: TOverlapped;
    FTerminated: boolean;
    FOnSendProgress: TPortSendProgressEvent;
    //procedure TimeoutTimerTimer(Sender: TObject);
    procedure InternalDataSent(var Msg: TMessage); message WM_DATASENT;
    procedure WMTimer(var Msg: TMessage); message WM_TIMER;
    procedure WMResetTimeoutTimer;
    function  GetRxCount: cardinal;
    procedure ClosePort;
    procedure OpenPort;
    procedure SetOpen(Value: boolean);
    procedure SetVersion(Value: string);
    procedure SetBaudRate(Value: integer);
    procedure SetByteSize(Value: byte);
    procedure SetParity(Value: TParity);
    procedure SetStopBits(Value: TStopBits);
    procedure SetRxBuffer(Value: cardinal);
    procedure SetLineMode(Value: boolean);
    procedure SetRxTimeout(Value: integer);
    procedure SetRxThresholdAmount(Value: integer);
    procedure SetRxThresholdUse(Value: boolean);
    procedure WndProc(var MsgRec: TMessage);
    function  InternalSend(Buffer: Pointer; Len: integer): cardinal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  Accessible: boolean;
    procedure ClearTxBuf;
    procedure CommFunction(Value: dword);
    procedure GetAvailablePorts(PortList: TStrings);
    function  GetFreeOutBuf: integer;
    function  PortOpen: boolean;
    function  PortClose: boolean;
    procedure PurgeRx;
    procedure PurgeTx;
    function  Send(Buffer: Pointer; Len: integer): cardinal;
    procedure SendChar(Chr: char);
    procedure SendStr(const Tx: string);
    function  Receive(Buffer: Pointer; Len: integer): cardinal;
    function  ReceiveStr: string;
    function  ReceiveRawByteStr: RawByteString;
    procedure CloseDelayed;
    procedure Release;
    function  ProcessMessage: boolean;
    procedure ProcessMessages;
    procedure MessageLoop;
    function  CheckOpen : Boolean;    // F.Piette: Added function
//    procedure OpenDelayed(ms: integer);
    property Open: boolean read FOpen write SetOpen;
    property RxCount: cardinal read GetRxCount;
    property Terminated: boolean read FTerminated;
  published
    property Baudrate: integer read FBaudrate write SetBaudrate;
    property ByteSize: byte read FByteSize write SetByteSize;
    property FlowCtrl: TFlowCtrl read FFlowCtrl write FFlowCtrl;
    property LineEnd: RawByteString read FLineEnd write FLineEnd;
    property LineMode: boolean read FLineMode write SetLineMode;
    property Parity: TParity read FParity write SetParity;
    property Port: integer read FPort write FPort;
    property PortOptions: TPortOptions read FPortOptions write FPortOptions;
    property RxBuffer: cardinal read FRxBuffer write SetRxBuffer;
    property RxTimeout: integer read FRxTimeout write SetRxTimeout;
    property RxThresholdAmount: integer read FRxThresholdAmount write SetRxThresholdAmount;
    property RxThresholdUse: boolean read FRxThresholdUse write SetRxThresholdUse;
    property StopBits: TStopBits read FStopBits write SetStopBits;
    property TxBuffer: cardinal read FTxBuffer write FTxBuffer;
    property Version: string read FVersion write SetVersion;
    property OnBGException: TPortExceptionEvent read FOnBGException write FOnBGException;
    property OnDataAvailable: TNotifyEvent read FOnDataAvailable write FOnDataAvailable;
    property OnDataSent: TNotifyEvent read FOnDataSent write FOnDataSent;
    property OnCtsChanged: TPortLineEvent read FOnCtsChanged write FOnCtsChanged;
    property OnDsrChanged: TPortLineEvent read FOnDsrChanged write FOnDsrChanged;
    property OnError: TPortErrEvent read FOnError write FOnError;
    property OnSendProgress: TPortSendProgressEvent read FOnSendProgress write FOnSendProgress;
  end;

  PDataRec = ^TDataRec;
  TDataRec = record
    Sender: TObject;
    Data: PByte;
    Len: integer;    // Length of the Data
    Size: integer;   // Size of the allocated memory for Data
  end;

  TDelayQueue = class
  private
    BusyList: TList;
    FreeList: TList;
    function GetCount: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Push(Sender: TObject; Data: PByte; Len: integer);
    function  Pop: PDataRec;
    property Count: integer read GetCount;
  end;

  TDelayData = procedure(Sender: TObject; Data: PByte; Len: integer) of object;
  TCiaDelay = class
  private
    FQueue: TDelayQueue;
    FHandle: HWND;
    FTimer: TCiaTimer;
    FOnSend: TDelayData;
    FOnReceive: TDelayData;
    FDelayTime: integer;
    FTimerRunning: boolean;
    FBaudrate: integer;
    procedure WMTimer(var Msg: TMessage); message WM_TIMER;
    procedure WndProc(var MsgRec: TMessage);
    procedure StopTimer;
    procedure TriggerSend(Sender: TObject; Data: PByte; Len: integer);
    procedure TriggerReceive(Sender: TObject; Data: PByte; Len: integer);
    procedure UpdateTimer(DataLen: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Receive(Sender: TObject; Data: PByte; Len: integer);
    procedure Send(Sender: TObject; Data: PByte; Len: integer);
    property Baudrate: integer read FBaudrate write FBaudrate;
    property DelayTime: integer read FDelayTime write FDelayTime;
    property OnSend: TDelayData read FOnSend write FOnSend;
    property OnReceive: TDelayData read FOnReceive write FOnReceive;
  end;

procedure CiaEnumPorts(PortList: TStrings);
procedure CiaEnumAccessiblePorts(PortList: TStrings);

implementation

const
//   Debug = True;
   Debug = False;

//------------------------------------------------------------------------------
//---- Global functions --------------------------------------------------------
//------------------------------------------------------------------------------
procedure CiaEnumPorts(PortList: TStrings);
var
   n, MaxPorts: integer;
   Port: THandle;
   PortName: string;
begin
   if Win32PlatForm = VER_PLATFORM_WIN32_NT then
      MaxPorts := 256
   else { if VER_PLATFORM_WIN32_WINDOWS }
      MaxPorts := 9;

   for n := 1 to MaxPorts do
   begin
      PortName := '\\.\COM' + IntToStr(n);
      Port := CreateFile(PChar(PortName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);

      if (Port <> INVALID_HANDLE_VALUE) or (GetLastError = ERROR_ACCESS_DENIED) then
         PortList.Add(IntToStr(n));

     CloseHandle(Port);
   end;
end;

//------------------------------------------------------------------------------
procedure CiaEnumAccessiblePorts(PortList: TStrings);
var
   n, MaxPorts: integer;
   Port: THandle;
   PortName: string;
begin
   if Win32PlatForm = VER_PLATFORM_WIN32_NT then
      MaxPorts := 256
   else { if VER_PLATFORM_WIN32_WINDOWS }
      MaxPorts := 9;

   for n := 1 to MaxPorts do
   begin
      PortName := '\\.\COM' + IntToStr(n);
      Port := CreateFile(PChar(PortName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);

      if (Port <> INVALID_HANDLE_VALUE) then
         PortList.Add(IntToStr(n));

     CloseHandle(Port);
   end;
end;

//------------------------------------------------------------------------------
//---- Internal functions ------------------------------------------------------
//------------------------------------------------------------------------------
function CIAPORT_WindowProc(ahWnd: HWND; auMsg: Integer; awParam: WPARAM; alParam: LPARAM): Integer; stdcall;
var
   Obj: TObject;
   MsgRec: TMessage;
begin
   Obj := TObject(GetWindowLong(ahWnd, 0));

   if not (Obj is TCiaComPort) then
      Result := DefWindowProc(ahWnd, auMsg, awParam, alParam)
   else begin
      MsgRec.Msg    := auMsg;
      MsgRec.WParam := awParam;
      MsgRec.LParam := alParam;
      TCiaComPort(Obj).WndProc(MsgRec);
      Result := MsgRec.Result;
   end;
end;

//------------------------------------------------------------------------------
var
   CIAPORT_WindowClass: TWndClass = ( style        : 0;
                                      lpfnWndProc  : @CIAPORT_WindowProc;
                                      cbClsExtra   : 0;
                                      cbWndExtra   : SizeOf(Pointer);
                                      hInstance    : 0;
                                      hIcon        : 0;
                                      hCursor      : 0;
                                      hbrBackground: 0;
                                      lpszMenuName : nil;
                                      lpszClassName: 'CIAPORT_WindowClass' );

//------------------------------------------------------------------------------
function CIAPORT_AllocateHWnd(Obj: TObject): HWND;
var
   TempClass: TWndClass;
   ClassRegistered: Boolean;
begin
   CIAPORT_WindowClass.hInstance := HInstance;
   ClassRegistered := GetClassInfo(HInstance,
                                   CIAPORT_WindowClass.lpszClassName,
                                   TempClass);
   if not ClassRegistered then begin
      Result := Windows.RegisterClass(CIAPORT_WindowClass);
      if Result = 0 then
         Exit;
   end;

   Result := CreateWindowEx(WS_EX_TOOLWINDOW,
                            CIAPORT_WindowClass.lpszClassName,
                            '',
                            WS_POPUP,
                            0, 0,
                            0, 0,
                            0,
                            0,
                            HInstance,
                            nil);

   if (Result <> 0) and Assigned(Obj) then
      SetWindowLong(Result, 0, Integer(Obj));
end;

//------------------------------------------------------------------------------
function CIAPORT_DeAllocateHWnd(Wnd: HWND): boolean;
begin
   Result := DestroyWindow(Wnd);
end;

//------------------------------------------------------------------------------
function Min(a, b: integer): integer;
begin
   if a <= b then
      Result := a
   else
      Result := b;
end;

//------------------------------------------------------------------------------
//---- TCiaTimer ---------------------------------------------------------------
//------------------------------------------------------------------------------
destructor TCiaTimer.Destroy;
begin
   FEnabled := False;
   UpdateTimer;
   inherited;
end;

//------------------------------------------------------------------------------
procedure TCiaTimer.SetEnabled(Value: Boolean);
begin
   if Value = FEnabled then
      Exit;

   FEnabled := Value;
   UpdateTimer;
end;

//------------------------------------------------------------------------------
procedure TCiaTimer.SetInterval(Value: Cardinal);
begin
   if Value = FInterval then
      Exit;

   FInterval := Value;
   UpdateTimer;
end;

//------------------------------------------------------------------------------
procedure TCiaTimer.UpdateTimer;
begin
   KillTimer(FHandle, 1);
   if (FInterval <> 0) and FEnabled then
      if SetTimer(FHandle, 1, FInterval, nil) = 0 then
         raise EOutOfResources.Create('TCiaTimer: No timers available');
end;

//------------------------------------------------------------------------------
//---- TCiaCustomBuffer ----------------------------------------------------------
//------------------------------------------------------------------------------
destructor TCiaCustomBuffer.Destroy;
begin
   if Assigned(FData) then
      FreeMem(FData);
   //if Assigned(FRcvd) then
   //   FreeMem(FRcvd);
   inherited;
end;

//------------------------------------------------------------------------------
procedure TCiaCustomBuffer.Grow(Count: Cardinal);
begin
   ReallocMem(FData, FCapacity + Count);
   inc(FCapacity, Count);
   //ReallocMem(FRcvd, FRcvdSize + Len);
   //inc(FRcvdSize, Len);
end;

//------------------------------------------------------------------------------
//---- TCiaTxBuffer ------------------------------------------------------------
//------------------------------------------------------------------------------
procedure TCiaTxBuffer.Clear;
begin
   ReAllocMem(FData, 0);
   FCount    := 0;
   FCapacity := 0;
   FReadPtr  := 0;
   FWritePtr := 0;
end;

//------------------------------------------------------------------------------
procedure TCiaTxBuffer.Empty;
begin
   FCount    := 0;
   FReadPtr  := 0;
   FWritePtr := 0;
end;

//------------------------------------------------------------------------------
function TCiaTxBuffer.Read(var Data: PByte; var Len: integer): boolean;
begin
   Result := False;
//   if (FCount = 0) or (Len <= 0) or not Assigned(Data) then
   if (FCount = 0) or (Len <= 0) then
      Exit;
   // hier (Carlos Soto)

   if Len > FCount then
      Len := FCount;

   Data := FData + FReadPtr;

   Dec(FCount, Len);
   Inc(FReadPtr, Len);
   if FCount = 0 then begin
      FReadPtr  := 0;
      FWritePtr := 0;
   end;

   Result := True;
end;

//------------------------------------------------------------------------------
procedure TCiaTxBuffer.Write(Data: Pointer; Len: integer);
var
   FreeSpace: integer;
begin
   if Len <= 0 then
      Exit;

   FreeSpace := FCapacity - cardinal(FWritePtr);
   if FreeSpace < Len then
      Grow(Len - FreeSpace);
   Move(Data^, FData[FWritePtr], Len);
   Inc(FWritePtr, Len);
   Inc(FCount, Len);
end;

//------------------------------------------------------------------------------
//---- TFlowCtrl ---------------------------------------------------------------
//------------------------------------------------------------------------------
procedure TFlowCtrl.ChangeCommState;
var
   DCB: TDCB;
begin
   if not FComport.Open then
      Exit;

   GetCommState(FComPort.FCommThread.ComHandle, DCB);
   DCB.Flags := FFlags;
   SetCommState(FComPort.FCommThread.ComHandle, DCB);
end;

//------------------------------------------------------------------------------
constructor TFlowCtrl.Create(Port: TCiaComPort);
begin
   inherited Create;
   FComPort := Port;
   FRxDtrControl := dtrEnable;
   FRxRtsControl := rtsEnable;
   FFlags := 1   or     // binary mode
             $10 or     // dtrEnable
             $1000;     // rtsEnable
end;

//------------------------------------------------------------------------------
procedure TFlowCtrl.SetRxDsrSensivity(Value: boolean);
begin
   if Value = FRxDsrSensivity then
      Exit;

   FRxDsrSensivity := Value;
   if Value then
      FFlags := FFlags or  $40
   else
      FFlags := FFlags and $FFFFFFBF;

   ChangeCommState;
end;

//------------------------------------------------------------------------------
procedure TFlowCtrl.SetRxDtrControl(Value: TDtrControl);
begin
   if Value = FRxDtrControl then
      Exit;

   FRxDtrControl := Value;
   FFlags := FFlags and $FFFFFFCF;     // reset fDTRControl bits
   case Value of
      dtrDisable:
         ;
      dtrEnable:
         FFlags := FFlags or $10;
      dtrHandshake:
         FFlags := FFlags or $20;
   end;

   ChangeCommState;
end;

//------------------------------------------------------------------------------
procedure TFlowCtrl.SetRxRtsControl(Value: TRtsControl);
begin
   if Value = FRxRtsControl then
      Exit;

   FRxRtsControl := Value;
   FFlags := FFlags and $FFFFCFFF;     // reset fRTSControl bits
   case Value of
      rtsDisable:
         ;
      rtsEnable:
         FFlags := FFlags or $1000;
      rtsHandshake:
         FFlags := FFlags or $2000;
      rtsToggle:
         FFlags := FFlags or $3000;
   end;

   ChangeCommState;
end;

//------------------------------------------------------------------------------
procedure TFlowCtrl.SetTxContinueXoff(Value: boolean);
begin
   if Value = FTxContinueXoff then
      Exit;

   FTxContinueXoff := Value;
   if Value then
      FFlags := FFlags or  $80
   else
      FFlags := FFlags and $FFFFFF7F;

   ChangeCommState;
end;

//------------------------------------------------------------------------------
procedure TFlowCtrl.SetTxCtsFlow(Value: boolean);
begin
   if Value = FTxCtsFlow then
      Exit;

   FTxCtsFlow := Value;
   if Value then
      FFlags := FFlags or  $4
   else
      FFlags := FFlags and $FFFFFFFB;

   ChangeCommState;
end;

//------------------------------------------------------------------------------
procedure TFlowCtrl.SetTxDsrFlow(Value: boolean);
begin
   if Value = FTxDsrFlow then
      Exit;

   FTxDsrFlow := Value;
   if Value then
      FFlags := FFlags or  $8
   else
      FFlags := FFlags and $FFFFFFF7;

   ChangeCommState;
end;

//------------------------------------------------------------------------------
procedure TFlowCtrl.SetXonOff(Value: boolean);
begin
   if Value = FXonOff then
      Exit;

   FXonOff := Value;
   if Value then
      FFlags := FFlags or  $300
   else
      FFlags := FFlags and $FFFFFCFF;

   ChangeCommState;
end;

//------------------------------------------------------------------------------
//---- TCiaComPort -------------------------------------------------------------
//------------------------------------------------------------------------------
function TCiaComPort.Accessible: boolean;
begin
   try
      Result := True;
      if Open then
         Exit;
      Open := True;
      Open := False;
   except
      Result := False;
   end;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.ClearTxBuf;
begin
   FTxBuf.Clear;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.CloseDelayed;
begin
   PostMessage(FHandle, WM_CLOSEPORT, 0, 0);
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.ClosePort;
begin
    if not Assigned(FCommThread) then
        Exit;

    if not FDestroying then
      if (LineMode or FRxThresholdUse or (FRxTimeout > 0)) and
         (FCommThread.FRxCount > 0) and
         Assigned(FOnDataAvailable) then   // F.Piette
         FOnDataAvailable(Self);

    FCommThread.FCloseFlag := TRUE;        // F.Piette
    SetEvent(FCommThread.CloseEvent);
//  FCommThread.FRcvBuffer.Destroy;        // F.Piette: destroy buffer after thread termination
    FCommThread.WaitFor;
    //WaitForSingleObject(Handle, Infinite);
    FCommThread.FRcvBuffer.Destroy;        // F.Piette: destroy buffer after thread termination
    FCommThread.Free;
    FCommThread := nil;  // F.Piette
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.CommFunction(Value: dword);
begin
   EscapeCommFunction(FCommThread.ComHandle, Value);
end;

//------------------------------------------------------------------------------
constructor TCiaComPort.Create(AOwner: TComponent);
begin
   inherited;
   FHandle    := CIAPORT_AllocateHWND(Self);
   FTxBuf     := TCiaTxBuffer.Create;
   FFlowCtrl  := TFlowCtrl.Create(Self);
   FVersion   := CIA_COMMVERSION;
   FLineEnd   := #13#10;
   FRxBuffer  := 8192;
   FTxBuffer  := 8192;
   FXOffLimit := FRxBuffer div 2;
   FXOnLimit  := FRxBuffer div 4 * 3;
   FBaudrate  := 9600;
   FByteSize  := 8;
   FStopBits  := sbOne;
   FParity    := ptNone;
end;

//------------------------------------------------------------------------------
destructor TCiaComPort.Destroy;
begin
   FDestroying := True;

   if FOpen then
      ClosePort;
   if Assigned(FTxBuf) then
      FTxBuf.Destroy;
   if Assigned(FTimeoutTimer) then
      FTimeoutTimer.Destroy;
   if Assigned(FFlowCtrl) then
      FFlowCtrl.Destroy;
   CIAPORT_DeAllocateHWND(FHandle);
   inherited;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.GetAvailablePorts(PortList: TStrings);
begin
   CiaEnumPorts(PortList);
end;

//------------------------------------------------------------------------------
function TCiaComPort.GetFreeOutBuf: integer;
var
   ComStat: TComStat;
   ErrorMask: cardinal;
begin
   ClearCommError(FCommThread.ComHandle, ErrorMask, @ComStat);
   Result := TxBuffer - ComStat.cbOutQue;
end;

//------------------------------------------------------------------------------
function TCiaComPort.GetRxCount: cardinal;
begin
   if Assigned(FCommThread) then  // F.Piette
       Result := FCommThread.FRxCount
   else
       Result := 0;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.InternalDataSent(var Msg: TMessage);
var
   P: Pointer;
   Len: integer;
begin
   if Debug then
      WriteLn('internaldatasent');
   if Assigned(FOnSendProgress) then
      FOnSendProgress(Self, FSendCount);

   Len := FTxBuffer;
   if FTxBuf.Read(PByte(P), Len) then begin
      InternalSend(P, Len);
      FSending   := True;
      FSendCount := len;
      Exit;
   end;
   FSending := False;

   if Assigned(FOnDataSent) then
      FOnDataSent(Self);
end;

//------------------------------------------------------------------------------
function TCiaComPort.InternalSend(Buffer: Pointer; Len: integer): cardinal;
begin
   if not Assigned(FCommThread) then  // F.Piette
       Exit;
   FillChar(OverLapTx, SizeOf(TOverLapped), 0);
   WriteFile(FCommThread.ComHandle, Buffer^, Len, Result, @OverLapTx);
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.MessageLoop;
var
   MsgRec : TMsg;
begin
   while GetMessage(MsgRec, 0, 0, 0) do begin
      TranslateMessage(MsgRec);
      DispatchMessage(MsgRec)
   end;
   FTerminated := True;
end;
//------------------------------------------------------------------------------
// F.Piette: Added function
// CheckOpen try to open the COM port and return TRUE/FALSE if port can be
// opened or not, but don't raise any exception.
function TCiaComPort.CheckOpen : Boolean;
var
   P: string;
   hPort: THandle;
begin
   P := '\\.\COM' + IntToStr(FPort);
   hPort := CreateFile(PChar(P), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
   if hPort = INVALID_HANDLE_VALUE then
      Result := FALSE
   else begin
      Result := TRUE;
      CloseHandle(hPort);
   end;
end;


//------------------------------------------------------------------------------
procedure TCiaComPort.OpenPort;
var
   P: string;
   hPort: THandle;
   DCB: TDCB;
   EvtMask: DWord;
begin
   P := '\\.\COM' + IntToStr(FPort);
   hPort := CreateFile(PChar(P), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_FLAG_OVERLAPPED, 0);
   if hPort = INVALID_HANDLE_VALUE then
      raise ECiaComPort.Create('Cannot open port ' + '''' + P + '''');

   GetCommState(hPort, DCB);

   with DCB do begin
      Baudrate := FBaudrate;
      XoffLim  := FXOffLimit;
      XonLim   := FXOnLimit;
      ByteSize := FByteSize;
      Parity   := Ord(FParity);
      StopBits := Ord(FStopBits);
      Flags    := FFlowCtrl.FFlags;
   end;

   SetupComm(hPort, FRxBuffer, FTxBuffer);
   SetCommState(hPort, DCB);

   //if EventCtsDsr in FPortoptions then
   //   SetCommMask(hPort, EV_RXCHAR or EV_TXEMPTY or EV_CTS or EV_DSR)
   //else
   //   SetCommMask(hPort, EV_RXCHAR or EV_TXEMPTY);

   EvtMask := EV_RXCHAR or EV_TXEMPTY;
   if EventCtsDsr in FPortOptions then
      EvtMask := EvtMask or EV_CTS or EV_DSR;
   if EventError in FPortOptions then
      EvtMask := EvtMask or EV_ERR;
   SetCommMask(hPort, EvtMask);

   FCommThread := TCiaCommThread.Create(True);
   with FCommThread do begin
      FCiaComPort     := Self;
      ComHandle       := hPort;
      FRcvBuffer      := TCiaCommBuffer.Create;
      Start; // FP 20120731 Resume thread created suspended
   end;
end;

//------------------------------------------------------------------------------
function TCiaComPort.PortClose: boolean;
begin
   Result := FOpen;
   SetOpen(False);
end;

//------------------------------------------------------------------------------
function TCiaComPort.PortOpen: boolean;
begin
   try
      SetOpen(True);
      Result := True;
   except
      Result := False;
   end;
end;

//------------------------------------------------------------------------------
function TCiaComPort.ProcessMessage: boolean;
var
   Msg : TMsg;
begin
   Result := FALSE;
   if PeekMessage(Msg, 0, 0, 0, PM_REMOVE) then begin
      Result := True;
      if Msg.Message = WM_QUIT then
         FTerminated := True
      else begin
         TranslateMessage(Msg);
         DispatchMessage(Msg);
      end;
   end;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.ProcessMessages;
begin
   while Self.ProcessMessage do
      ;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.PurgeRx;
begin
   if Assigned(FCommThread) then    // F.Piette
       PurgeComm(FCommThread.ComHandle, PURGE_RXCLEAR or PURGE_RXABORT);
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.PurgeTx;
begin
   if Assigned(FCommThread) then    // F.Piette
       PurgeComm(FCommThread.ComHandle, PURGE_TXCLEAR or PURGE_TXABORT);
   FTxBuf.Empty;
   FSending := False;
end;

//------------------------------------------------------------------------------
function TCiaComPort.Receive(Buffer: Pointer; Len: integer): cardinal;
var
   OverLapped: TOverLapped;
   Buf: TCiaCommBuffer;
begin
   if (Len <= 0) or (not Assigned(FCommThread)) then begin  // F.Piette
      Result := 0;
      Exit;
   end;

   if Cardinal(Len) > FCommThread.FRxCount then
      Len := FCommThread.FRxCount;

   if FLineMode or FRxThresholdUse or (FRxTimeout > 0) then begin
      Buf := FCommThread.FRcvBuffer;
      //Move(Buf.FRcvd[Buf.FReadPtr], Buffer^, Len);
      Move(Buf.FData[Buf.FReadPtr], Buffer^, Len);
      Inc(Buf.FReadPtr, Len);
      if Buf.FReadPtr >= Buf.FWritePtr then begin
         Buf.FReadPtr    := 0;
         Buf.FWritePtr   := 0;
         Buf.FLineEndPtr := 0;
      end;
      Result := Len;
   end
   else begin
      FillChar(OverLapped, SizeOf(OverLapped), 0);
      Readfile(FCommThread.ComHandle, Buffer^, Len, Result, @OverLapped);
   end;

   Dec(FCommThread.FRxCount, Result);
end;

//------------------------------------------------------------------------------
// Warning: The cast used in this routine will cause unexpected conversion
// from ansi to unicode.
// Use ReceiveRawByteStr if you don't want such conversion.
function TCiaComPort.ReceiveStr: string;
Var
  CiaString : RawByteString;
begin
   SetLength(CiaString, RxCount);
   Receive(Pointer(CiaString), RxCount);
   Result := String(CiaString);
end;

//------------------------------------------------------------------------------
function TCiaComPort.ReceiveRawByteStr: RawByteString;
begin
   SetLength(Result, RxCount);
   Receive(Pointer(Result), RxCount);
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.Release;
begin
   PostMessage(FHandle, WM_RELEASEPORT, 0, 0);
end;

//------------------------------------------------------------------------------
function TCiaComPort.Send(Buffer: Pointer; Len: integer): cardinal;
var
   TxLen: cardinal;
begin
   Result := 0;
   if not FOpen then
      raise ECiaComPort.Create('Port not open');

   if (Len <= 0) or not Assigned(Buffer) then
      Exit;

   if not (DontUseInternalBuffer in FPortOptions) then
      if FSending or (FTxBuf.Count > 0) then begin
         if Debug then
            WriteLn('add to txbuf');
         Result := Len;
         FTxBuf.Write(Buffer, Len);
         Exit;
      end;

   if Debug then
      WriteLn('sending');
   TxLen := Min(Len, FTxBuffer);
   InternalSend(Buffer, TxLen);
   FSending   := True;
   FSendCount := TxLen;

   // if FTxBuffer is full then we have to save the part that is not sent
   FTxBuf.Write(PByte(Buffer) + TxLen, cardinal(Len) - TxLen);
   Result := Len; // obsolete because the component will buffer all data
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SendChar(Chr: char);
begin
   Send(@Chr, 1);
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SendStr(const Tx: string);
Var
   CiaString : RawByteString;
begin
   CiaString := RawByteString(Tx);         // F.Piette: Added cast RawByteString()
   Send(Pointer(CiaString), Length(CiaString));
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SetBaudRate(Value: integer);
var
   DCB: TDCB;
begin
   if Value = FBaudRate then
      Exit;

   FBaudRate := Value;

   if (csDesigning in ComponentState) or not Open then
      Exit;

   if Assigned(FCommThread) then begin   // F.Piette
       GetCommState(FCommThread.ComHandle, DCB);
       DCB.BaudRate := Value;
       SetCommState(FCommThread.ComHandle, DCB);
   end;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SetByteSize(Value: byte);
var
   DCB: TDCB;
begin
   if Value = FByteSize then
      Exit;

   FByteSize := Value;

   if (csDesigning in ComponentState) or not Open then
      Exit;

   if Assigned(FCommThread) then begin   // F.Piette
       GetCommState(FCommThread.ComHandle, DCB);
       DCB.ByteSize := Value;
       SetCommState(FCommThread.ComHandle, DCB);
   end;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SetLineMode(Value: boolean);
begin
   if Value = FLineMode then
      Exit;

   FLineMode := Value;

   if FOpen and (not Value) and
      Assigned(FCommThread) and (FCommThread.FRxCount > 0) then  // F.Piette
      PostMessage(FHandle, WM_DATAAVAILABLE, 0, 0);

   if Value then begin
      if FRxThresholdUse then
         FRxThresholdUse := False;
      if FRxTimeout > 0 then
         FRxTimeout := 0;
   end;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SetOpen(Value: boolean);
begin
   if Value = FOpen then
      Exit;

   if Value then
      OpenPort
   else
      ClosePort;

   FOpen := Value;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SetParity(Value: TParity);
var
   DCB: TDCB;
begin
   if Value = FParity then
      Exit;

   FParity := Value;

   if (csDesigning in ComponentState) or not Open then
      Exit;

   if Assigned(FCommThread) then begin   // F.Piette
       GetCommState(FCommThread.ComHandle, DCB);
       DCB.Parity := Ord(Value);
       SetCommState(FCommThread.ComHandle, DCB);
   end;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SetRxBuffer(Value: cardinal);
var
   DCB: TDCB;
begin
   if Value = FRxBuffer then
      Exit;

   FRxBuffer  := Value;
   FXOffLimit := Value div 4 * 3;
   FXOnLimit  := Value div 2;

   if (csDesigning in ComponentState) or not Open then
      Exit;

   if Assigned(FCommThread) then begin   // F.Piette
       GetCommState(FCommThread.ComHandle, DCB);
       DCB.XoffLim := FXOffLimit;
       DCB.XonLim  := FXOnLimit;
       SetCommState(FCommThread.ComHandle, DCB);
   end;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SetRxThresholdAmount(Value: integer);
begin
   if Value = FRxThresholdAmount then
      Exit;

   { TODO : need to check if value is decremented, and eventually fire OnDataAvailable }
   { TODO : check for zero. if zero and RxThresholdUse is True then raise exception }
   FRxThresholdAmount := Value;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SetRxThresholdUse(Value: boolean);
begin
   if Value = FRxThresholdUse then
      Exit;

   { TODO : check for RxThresholdAmount = zero. if zero and Value then raise exception }
   FRxThresholduse := Value;
   if Value then begin
      if FLineMode then
         FLineMode := False;
      if FRxTimeout > 0 then
         FRxTimeout := 0;
   end;

   if FOpen and not Value and
      Assigned(FCommThread) and (FCommThread.FRxCount > 0) then  // F.Piette
      PostMessage(FHandle, WM_DATAAVAILABLE, 0, 0);
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SetRxTimeout(Value: integer);
begin
   if Value < 0 then
      raise ECiaComPort.Create('Value can not be negatieve');

   if FRxTimeout = Value then
      Exit;

   FRxTimeout := Value;
   if FRxTimeout > 0 then begin
      if FLineMode then
         FLineMode := False;
      if FRxThresholdUse then
         FRxThresholdUse := False;
      if not Assigned(FTimeoutTimer) then begin
         FTimeoutTimer := TCiaTimer.Create;
         FTimeoutTimer.Handle := FHandle;
      end;
      FTimeoutTimer.Interval := FRxTimeout;
      //FTimeoutTimer.OnTimer  := TimeoutTimerTimer;
      FTimeoutTimer.Enabled  := FOpen and (FCommThread.FRxCount > 0);
   end
   else if Assigned(FTimeoutTimer) then begin
      FTimeoutTimer.Destroy;
      FTimeoutTimer := nil;
   end;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SetStopBits(Value: TStopBits);
var
   DCB: TDCB;
begin
   if Value = FStopBits then
      Exit;

   FStopBits := Value;

   if (csDesigning in ComponentState) or not Open then
      Exit;

   if Assigned(FCommThread) then begin   // F.Piette
       GetCommState(FCommThread.ComHandle, DCB);
       DCB.StopBits := Ord(Value);
       SetCommState(FCommThread.ComHandle, DCB);
   end;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.SetVersion(Value: string);
begin
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.WMResetTimeoutTimer;
begin
   if not Assigned(FTimeoutTimer) then
      Exit;
   FTimeoutTimer.Enabled := False;
   FTimeoutTimer.Enabled := True;
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.WMTimer(var Msg: TMessage);
begin
   FTimeoutTimer.Enabled := False;
   if FOpen and Assigned(FCommThread) and (FCommThread.FRxCount > 0) then // F.Piette
      PostMessage(FHandle, WM_DATAAVAILABLE, 0, 0)
end;

//------------------------------------------------------------------------------
procedure TCiaComPort.WndProc(var MsgRec: TMessage);
begin
   try
      with MsgRec do
         case Msg of
//            WM_OPENPORT:
//               Open := True;
            WM_CLOSEPORT:
               Open := False;
            WM_RELEASEPORT:
               Destroy;
            WM_DATAAVAILABLE:
               if Assigned(FOnDataAvailable) then
                  FOnDataAvailable(Self);
            WM_DATASENT:
               InternalDataSent(MsgRec);
            WM_CTSCHANGED:
               if Assigned(FOnCtsChanged) then
                  FOnCtsChanged(Self, (WParam and MS_CTS_ON) > 0);
            WM_DSRCHANGED:
               if Assigned(FOnDsrChanged) then
                  FOnDsrChanged(Self, (WParam and MS_DSR_ON) > 0);
            WM_PORTERROR:
               if Assigned(FOnError) then
                  FOnError(Self, WParam);
            WM_RESET_RX_TIMOUT:
               WMResetTimeoutTimer;
            WM_TIMER:
               WMTimer(MsgRec);
            else
               Result := DefWindowProc(FHandle, Msg, wParam, LParam);
         end;
   except
      on E: Exception do
      try
         if Assigned(FOnBGException) then
            FOnBGException(Self, E);
      except
      end;
   end;
end;

//------------------------------------------------------------------------------
//---- TCiaCommThread ----------------------------------------------------------
//------------------------------------------------------------------------------
function TCiaCommThread.CheckLineEnd(P: PByte): boolean;
var
   n: integer;
begin
   n := 1;
   while (n <= Length(FCiaComPort.LineEnd)) and (P[n - 1] = Ord(FCiaComPort.LineEnd[n])) do
      Inc(n);
   Result := n > Length(FCiaComPort.LineEnd);
end;

//------------------------------------------------------------------------------
constructor TCiaCommThread.Create(CreateSuspended: boolean);
begin
   inherited;
   CloseEvent := CreateEvent(nil, True, False, nil);
end;

//------------------------------------------------------------------------------
destructor TCiaCommThread.Destroy;
begin
    inherited Destroy;   // F.Piette, just to be able to put a breakpoint here
end;

//------------------------------------------------------------------------------
procedure TCiaCommThread.Execute;
var
   WaitHandles: array[0..1] of THandle;
   OverLap: TOverLapped;
   WaitEvent: cardinal;
begin
   NameThreadForDebugging(AnsiString('CiaComPort COM' + IntToStr(FCiaComPort.Port)));   // F.Piette
   FillChar(OverLap, sizeof(OverLapped), 0);
   OverLap.hEvent := CreateEvent(nil, True, True, nil);
   WaitHandles[0] := CloseEvent;
   WaitHandles[1] := OverLap.hEvent;

   GetCommModemStatus(ComHandle, FModemStatus);
   if FModemStatus > 0 then begin
      if (FModemStatus and MS_CTS_ON) > 0 then
         FEventMask := FEventMask or EV_CTS;
      if (FModemStatus and MS_DSR_ON) > 0 then
         FEventMask := FEventMask or EV_DSR;
      PortEvent;
   end;

   while not Terminated do begin
      WaitCommEvent(ComHandle, FEventMask, @OverLap);
      GetCommModemStatus(ComHandle, FModemStatus);
      WaitEvent := WaitForMultipleObjects(2, @WaitHandles, False, INFINITE);
      case WaitEvent of
         WAIT_OBJECT_0:
                Terminate;
         WAIT_OBJECT_0 + 1:
                PortEvent;
            end;
   end;

   CloseHandle(OverLap.hEvent);
   CloseHandle(CloseEvent);
   CloseHandle(ComHandle);
end;

//------------------------------------------------------------------------------
procedure TCiaCommThread.InternalReceive;
var
   OverLapped: TOverLapped;
   Count: Cardinal;
begin
   if FRcvBuffer.FCapacity - cardinal(FRcvBuffer.FWritePtr) < FInternalRxCount then
      FRcvBuffer.Grow(FInternalRxCount);

   FillChar(OverLapped, SizeOf(OverLapped), 0);
   Readfile(ComHandle, FRcvBuffer.FData[FRcvBuffer.FWritePtr], FInternalRxCount, Count, @OverLapped);
   Inc(FRcvBuffer.FWritePtr, Count);
   Dec(FInternalRxCount, Count);

   // RxTimout
   if FCiaComport.RxTimeout > 0 then begin
      FRxCount := FRcvBuffer.FWritePtr - FRcvBuffer.FReadPtr;
      PostMessage(FCiaComPort.FHandle, WM_RESET_RX_TIMOUT, 0, 0);
      Exit;
   end;

   if not Assigned(FCiaComPort.OnDataAvailable) then
      Exit;

   if FCiaComPort.FLineMode then begin
      while (FRcvBuffer.FWritePtr - FRcvBuffer.FLineEndPtr) >= Length(FCiaComPort.LineEnd) do begin
         if CheckLineEnd(FRcvBuffer.FData + FRcvBuffer.FLineEndPtr) then begin
            Inc(FRcvBuffer.FLineEndPtr, Length(FCiaComPort.LineEnd));
            repeat
               FRxCount := FRcvBuffer.FLineEndPtr - FRcvBuffer.FReadPtr;
               SendMessage(FCiaComPort.FHandle, WM_DATAAVAILABLE, 0, 0);
               if not Assigned(FCiaComPort.FOnDataAvailable) then
                  Exit;
            until FRxCount = 0;
            if FRcvBuffer.FReadPtr = FRcvBuffer.FWritePtr then
               Exit;
         end
         else  // add version 1.14
            Inc(FRcvBuffer.FLineEndPtr);
      end;
      Exit;
   end;

   // RxThresholdUse
   while FRcvBuffer.FWritePtr - FRcvBuffer.FReadPtr >= FCiaComPort.RxThresholdAmount do begin
       if FCloseFlag then                                        // F.Piette
           break;                                                // F.Piette

      FRxCount := FCiaComPort.RxThresholdAmount;
      SendMessage(FCiaComPort.FHandle, WM_DATAAVAILABLE, 0, 0);
   end;

end;

//------------------------------------------------------------------------------
procedure TCiaCommThread.PortEvent;
var
   ComStat: TComStat;
   ErrorMask: cardinal;
begin
   if Debug then
      WriteLn('PortEvent ', FEventMask);
   ClearCommError(ComHandle, ErrorMask, @ComStat);

   // we have to check all the events, because more than one can happen the same time
   if (FEventMask and EV_RXCHAR) > 0 then begin
      FInternalRxCount := ComStat.cbInQue;
      if FInternalRxCount > 0 then
         if FCloseFlag then       // F.Piette
            FCiaComPort.PurgeRx   // F.Piette
         else if not Assigned(FCiaComPort.FOnDataAvailable) then // nobody wants to receive
            FCiaComPort.PurgeRx
         else begin
            if FCiaComPort.LineMode or FCiaComPort.RxThresholdUse or (FCiaComPort.FRxTimeout > 0) then begin
               InternalReceive;
            end
            else begin
               FRxCount := FInternalRxCount;
               while FRxCount > 0 do begin
                  SendMessage(FCiaComPort.FHandle, WM_DATAAVAILABLE, 0, 0);
                  if not Assigned(FCiaComPort.FOnDataAvailable) then
                     break;
               end;
            end;
         end;
   end;

   if (FEventMask and EV_TXEMPTY) > 0  then begin
      if Debug then
         WriteLn('WM_DATASENT');
      PostMessage(FCiaComPort.FHandle, WM_DATASENT, 0, 0);
   end;

   if (FEventMask and EV_CTS)  > 0 then
      PostMessage(FCiaComPort.FHandle, WM_CTSCHANGED, FModemStatus, 0);

   if (FEventMask and EV_DSR)  > 0 then
      PostMessage(FCiaComPort.FHandle, WM_DSRCHANGED, FModemStatus, 0);

   if (FEventMask and EV_ERR)  > 0 then
      PostMessage(FCiaComPort.FHandle, WM_PORTERROR, ErrorMask, 0);

   // Have to add them also in the SetCommMask
   //if (FEventMask and EV_RLSD) > 0 then
   //if (FEventMask and EV_RING) > 0 then

   FModemStatus := 0;
   FEventMask   := 0;
end;

{ TDelayQueue }

constructor TDelayQueue.Create;
begin
   BusyList := TList.Create;
   FreeList := TList.Create;
end;

destructor TDelayQueue.Destroy;
var
   DataRec: PDataRec;
   n: integer;
begin
   for n := 0 to BusyList.Count - 1 do begin
      DataRec := BusyList[n];
      FreeMem(DataRec.Data);
      Dispose(DataRec);
   end;
   BusyList.Free;
   for n := 0 to FreeList.Count - 1 do begin
      DataRec := FreeList[n];
      FreeMem(DataRec.Data);
      Dispose(DataRec);
   end;
   FreeList.Free;
   inherited;
end;

function TDelayQueue.GetCount: integer;
begin
   Result := BusyList.Count;
end;

function TDelayQueue.Pop: PDataRec;
begin
   Result := BusyList[0];
   BusyList.Delete(0);
   FreeList.Add(Result);
end;

procedure TDelayQueue.Push(Sender: TObject; Data: PByte; Len: integer);
var
   DataRec: PDataRec;
begin
   if FreeList.Count > 0 then begin
      DataRec := FreeList[0];
      FreeList.Delete(0);
   end
   else begin
      New(DataRec);
      FillChar(DataRec^, SizeOf(TDataRec), 0);
   end;
   if DataRec.Size < Len + 1 then begin
      ReAllocMem(DataRec.Data, Len + 1);
      DataRec.Size := Len + 1;
   end;
   Move(Data^, DataRec.Data^, Len);
   DataRec.Data[Len] := 0;
   DataRec.Sender := Sender;
   DataRec.Len := Len;
   BusyList.Add(DataRec);
end;

{ TCiaDelay }

constructor TCiaDelay.Create;
begin
   FBaudrate := 9600;
   FHandle := AllocateHWND(WndProc);
   FTimer := TCiaTimer.Create;
   FTimer.Handle := FHandle;
   FQueue := TDelayQueue.Create;
end;

destructor TCiaDelay.Destroy;
begin
   DeAllocateHWND(FHandle);
   FTimer.Free;
   FQueue.Free;
   inherited;
end;

procedure TCiaDelay.Receive(Sender: TObject; Data: PByte; Len: integer);
begin
   TriggerReceive(Sender, Data, Len);
   UpdateTimer(0);
end;

procedure TCiaDelay.Send(Sender: TObject; Data: PByte; Len: integer);
begin
   if FTimerRunning then
      FQueue.Push(Sender, Data, Len)
   else begin
      TriggerSend(Sender, Data, Len);
      UpdateTimer(Len);
   end;
end;

procedure TCiaDelay.StopTimer;
begin
   FTimer.Enabled := False;
   FTimerRunning := False;
end;

procedure TCiaDelay.TriggerReceive(Sender: TObject; Data: PByte; Len: integer);
begin
   if Assigned(FOnReceive) then
      FOnReceive(Sender, Data, Len);
end;

procedure TCiaDelay.TriggerSend(Sender: TObject; Data: PByte; Len: integer);
begin
   if Assigned(FOnSend) then
      FOnSend(Sender, Data, Len);
end;

procedure TCiaDelay.UpdateTimer(DataLen: integer);
begin
   FTimer.Enabled := False;
   FTimer.Interval := FDelayTime + DataLen * FBaudrate div 10000;
   FTimer.Enabled := True;
   FTimerRunning := True;
   if FDelayTime = 0 then
      PostMessage(FHandle, WM_TIMER, 0, 0);
end;

procedure TCiaDelay.WMTimer(var Msg: TMessage);
var
   DataRec: PDataRec;
begin
   if FQueue.Count = 0 then
      StopTimer
   else begin
      DataRec := FQueue.Pop;
      TriggerSend(DataRec.Sender, DataRec.Data, DataRec.Len);
      UpdateTimer(DataRec.Len);
   end;
end;

procedure TCiaDelay.WndProc(var MsgRec: TMessage);
begin
   case MsgRec.Msg of
      WM_TIMER: WMTimer(MsgRec);
      else
         MsgRec.Result := DefWindowProc(FHandle, MsgRec.Msg, MsgRec.wParam, MsgRec.LParam);
   end;
end;

end.

{  Author: Mestdagh Wilfried
   Web:    http://www.mestdagh.biz
   eMail:  wilfried@mestdagh.biz

   If you try this component then please drop me a email with your comments. Any
   comment is welcome, also negative. Please goto my web to download the latest
   version.

   Properties
      Port
         Commnumber of the port (starts at 1)

      Baudrate
         You have to fill in a valid baudreate

      ByteSize
         Default 8 bit. Note that there can be illegal combinations between
         StopBits and ByteSize.

      Parity
         TParity
          enumerated value)

      StopBits
         TStopbits enumerated value. Note that there can be illegal combinations
         between StopBits and ByteSize.

      FlowCtrl
         Set all possible flow controls. Note that there can be illegal
         combinations. See DCB in windows API help for more information.
         Note also that a lot of these handshaking are not working properly in
         some operating systems. Some of them will block the application in
         Windows9x. That's Microsoft not me ;-)

      RxBuffer
      TxBuffer
         Size of both rx and tx queue used by windows

      LineMode
         If set then the component will fire OnDataAvailable only when the
         LineEnd characters are received

      RxAmountThresholdAmount
      RxAmountThresholdUse
         If set then the component will fire OnDataAvailable only if the certain
         amount of bytes (or more) is arrived in the Rx buffer.

      LineEnd
         string to indicate the end of line.

      Open
         Open or close the port

      RxCount
         Available characters in buffer, only valid if called from within
         OnDataAvailable

   Events
      OnDataAvailable
         Fired when characters are received by the component.

      OnDataSent
         Fired when the tx queue is emply. If you have to send a large amount
         of data then this is the place to send next chunck of data.

      OnCtsChanged
      OnDsrChanged
         Fired when one of these input control lines is changed activity

      OnBGException
         Fired when an exepction occurs from the middle of nowhere. That is the
         message pump. Many events are called from the message pump. If you make
         and exception error in your code in sutch and event, then OnBGException
         will also fire.

   Methods
      procedure PurgeRx;
         Purge Rx buffer

      procedure PurgeTx;
         Purge Tx buffer and stops transmitting immediatly

      function  Send(Buffer: Pointer; Len: integer): cardinal;
         Put Buffer in the txqueue, returns char's writed

      procedure SendStr(const Tx: string);
         Call's Send. Use this one if you realy wants to use strings

         Dont call Send or SendStr in a closed loop. It is not only a bad
         programming technique but you have to remember that TCiaComPort is
         asynchronous and these methods return immediatly, long before the data
         is sended and even before the operation itself is completed. Instead
         send a block of data, and send next block of data in OnDataSent,
         because that's the signal it is realy completed.

      function  Receive(Buffer: Pointer; Len: integer): cardinal;
         Only to be called in OnDataAvailable. You have to read all available
         characters (call RxCount to know how much there is in buffer).

      function  ReceiveStr: string;
         Call's Receive. Use this if you really wants to use strings
         Be aware that unicode conversion take place and may give unexpected
         results.

      function  ReceiveRawByteStr: string;
         Call's Receive. Use this if you really wants to use strings to receive
         byte streams.

      function  GetFreeOutBuf: integer;
         Returns free space in Tx buffer.

      procedure GetAvailablePorts(PortList: TStrings);
         Returns all available ports on the machine. Check also the global
         CiaEnumPorts and CiaEnumAccessiblePorts wich does the same. The latter
         returns only the ports that are not already open by another application

      function Accessible: boolean
         Return True if the port exists and can be opened by the application

      procedure CommFunction(Value: DWord);
         Possible values are:
         CLRDTR	Clears the DTR (data-terminal-ready) signal.
         CLRRTS	Clears the RTS (request-to-send) signal.
         SETDTR	Sends the DTR (data-terminal-ready) signal.
         SETRTS	Sends the RTS (request-to-send) signal.
         SETXOFF	Causes transmission to act as if XOFF has been received.
         SETXON	Causes transmission to act as if XON has been received.
         SETBREAK	Suspends transmission and places the line in a break state.
         CLRBREAK	Restores transmission and places the line in a nonbreak.
         Note that some combinations are illegal. For example toggle the
         hardware lines when hardware handshaking is set. In some OS windows
         will not complain if you set illegal values, the port just stops
         working. That's Microsoft, not me ;-)

      procedure CloseDelayed;
         To close the port from within one of his own events. It will post a
         message to its hidden window. In the custom message handler the port
         will close.

      procedure Release;
         To Destroy the component form within one of his own events.

      function  ProcessMessage: boolean;
         Receive 1 messgae if any. Returns True if there was a message. This is
         intended for use in a thread. This is very similar to
         TApplication.ProcessMessage and can also be used if you dont have a
         TApplication object, for example in a console application.

      procedure ProcessMessages;
         Loop thru message processing until all messages are processed. This is
         intended for use in a thread. This is very similar to
         TApplication.ProcessMessage and can also be used if you dont have a
         TApplication object, for example in a console application.

      procedure MessageLoop;
         Loop thru message processing until the WM_QUIT message is received.
         MessageLoop is different from ProcessMessages because it actually
         blocks if no message is available. The loop is broken when WM_QUIT is
         retrieved. This is inteded for use in a thread.

      property  Terminated;
         Set to True when a WM_QUIT message is received, and the message loop is
         broken. This is inteded for use in a thread.

   Version information:
      1.00 18 Nov 2001
         - First version (only tested on W2K, service pack 2)
      1.01 20 Nov 2001
         - Propery editor for LineEnd
      1.02 24 Nov 2001
         - Added logic in Receive in case someone does not want to receive
           all available bytes.
         - If OnDataAvailable has no handler then we receive and trow away.
         - LineMode and LineEnd
      1.03 3 Mar 2002
         - Added GetFreeOutBuf wich returns free space in output buffer
         - Made changing port settings possible when port is already open
         - Added software and hardware handshaking
      1.04 4 Mar 2002
         - Corrected wrong calculation from XonLimit and XoffLimit
         - Added TxContinueXoff property in FlowCtrl.
      1.05 16 Mar 2002
         - Added GetAvailablePorts(PortList: TStrings), suggested by Wilson Lima
           [wsl@dglnet.com.br]
         - Added property ByteSize
         - Added CommFunction(Value: DWord).
         - Added PurgeRx to clear the Rx buffer and PurgeTx wich clears the Tx
           buffers and stop transmitting immediatly.
         - Corrected closing when there is still data in buffer (could be if
           LineMode is set) then OnDataAvailable will fire before closing.
         - Corrected if setting LineMode to False and there is still data in
           buffer then OnDataAvailable will fire immediatly.
      1.06 24 Apr 2002
         - Ported to Delphi 6 by moving the property editor to a separate file
           uCiaComportE.pas and adding conditional compilation. See notes in
           uCiaComportE.pas on what to do for Delphi6.
      1.07 15 May 2002
         - There was a bug when closing the port in Delphi 6. Serge Wagener
           [serge@wagener-consulting.lu] found offending line. Ivan Turcan
           [iturcan@drake.sk] proposed a bug fix for it. Problem was because
           the thread was created with FreeOnTerminate to True and with Delphi 6
           the thread seems to be destroyed while the WaitFor was still looping.
      1.08 1 Jul 2002
         - Added global function CiaEnumPorts.
         - Corrected spelling error in property Version (it was Verstion) found.
           Thanks to Dirk Claesen [dirk.claesen@pandora.be]. Note that you will
           have a 'property not found' error for this one, but just ignore it.
         - Bug fix in LineEnd when the byte before the first byte in LineEnd was
           exacly that same byte (eg: #13#13#10 and LineEnd #13#10). In that
           case OnDataAvailable was not fired. Fixed with a windowing function.
      1.09 4 sep 2002
         - Added OnCTSChanged and OnDSRChanged.
         - Added RxThresholdAmount and a RxThresholdUse property to fire
           OnDataAvailable when a certain amount of bytes is received.
      1.10 4 oct 2002
         - Added property PortOptions and EventCtsDsr. If EventCtsDsr is in
           PortOptions then OnCtsChanged and OnDsrChanged will be triggered if
           appropriate input line change, otherwise not. Default it is not set.
           Reason is that some Service packs on NT will crash the application
           if SetCommMask is set to fire events of this kind. Note that this
           will require change in code or in object inspector for people using
           this option (started from previous version).
         - Solved typo in source witch crashed when DSR state was changed and
           only code assigned to OnCtsChange. In that case a null pointer was
           called because OnDsrState was not assigned.
      1.11 8 nov 2002
         - upgraded for Delphi 7. Thanks to Marco Orlik <orlik@web.de> for
           testing this out.
      1.12 31 aug 2003
         - Added EventError in PortOptions and OnError event, proposed by Bruck
           Tesfaye [doyodoyodoyo@hotmail.com]. If EventError is set and there is
           a Frame, Overrun or Parity error in received data then OnError will
           fire with an error argument. Possible values are:
              CE_FRAME     The hardware detected a framing error.
              CE_OVERRUN   A character-buffer overrun has occurred.
              CE_RXPARITY  The hardware detected a parity error.
           To check the errors in the OnError event you have to code as follows:
              if (Error and CE_RXPARITY) > 0 then
                 // a parity error occured
         - The application hanged if port is just opened and closed. Reason was
           CloseEvent was created on the waiting thread but used on the main
           thread. CloseEvent creation moved to the TCiaCommThread's constructor
           thanks to Alex <alshovk@netvampire.com> who proposed the bug fix.
         - Moved OverlapTx to private member of TCiaComport, TOverlapped can not
           be released until the overlapped function is completed. Having it on
           the stack was sometimes the course of an AV. Bugfix proposed by Alex
           <alshovk@netvampire.com>.
         - If LineMode = True and OnDataAvailable is not assigned, the program
           crashed when data is received. Alex <alshovk@netvampire.com> reported
           reported this bug. Instead of a receive to a Trashcan, PurgeRx is now
           called.
         - It is now possible to recieve partial string in OnDataAvailable. If
           only partial data is received, then OnDataAvailable will be called
           again in a closed loop. This works with and without LineMode.
         - Added a hidden window, and use it for all internal synchronizing
         - Added method CloseDelayed, which make it possible to close the
           port from within one of his own events.
         - Added method Release to destroy the component from one of his event
           handlers.
         - DataAvailable will not recursive reenter when LineMode or Threshold
           is set to false and if there is still data in the buffer. Instead it
           will call a custom message handler to fire the events.
         - Added OnBGException event, called from WndProc.

      1.13 22 nov 2003
         - Added property RxTimeout (millisec). If set to a value then
           OnDataAvailable will fire when there is data received, and no data
           has arived during this interval. Usefull if there is no protocol with
           amount of bytes, or terminating character(s). The whole amount of
           data has to be received in OnDataAvailable, since it will only be
           triggered after data has received again and next timeout expires.
         - Added global procedure CiaEnumAccessiblePorts(PortList: TStrings)
           which only return accessible ports that are not opened by another
           application, thanks to Ron [ron@gogogalaxy.com]
         - Added method Accessible to check if the port can be opened.

      1.14 7 feb 2004
         - Made thread safe by own creation of hidden window without using VCS,
           therefore the Forms unit is also removed from the package.
         - Added a message pump for use in a thread, or in console application.
           Therefore 3 new funcions and a property. Function ProcessMessage and
           ProcessMessages are very similar to TApplication.ProcessMessage and
           TApplication.ProcessMessages. You can use it also if your application
           has no TApplication object.
             function  ProcessMessage: boolean;
               Receive 1 messgae if any. Returns True if there was a message.
             procedure ProcessMessages;
               Loop thru message processing until all messages are processed.
             procedure MessageLoop;
               Loop thru message processing until the WM_QUIT message is
               received. MessageLoop is different from ProcessMessages because
               it actually blocks if no message is available. The loop is broken
               when WM_QUIT is retrieved.
             property  Terminated;
               Set to True when a WM_QUIT message is received, and the message
               loop is broken.
         - Added check if port is destroyed it will not fire OnDataAvailable
           when closing.
         - Added SetByteSize thanks to Joerg Ter Veen <joerg.ter.veen@web.de>
           who provided the code for it.
         - Found bug in LineMode. If 2 empty packets received after one another,
           the second onde did not fire OnDataAvailable, and the next packet was
           concanated to it.
         - Version fully tested in D6 and D7 thanks to Ivan Turkan
           <iturcan@drake.sk>

      1.15 17 apr 2004
         - Written my own TCiaTimer to leave out ExtCtrls. This reduce a console
           application by over 220 KB. Thanks to Asseri Lintanen
           <asseri.lintanen@hedpro.fi> for make me attention to this. Note that
           this timer is different from the Delphi TTimer. It does not create
           a hidden window, but instead it used the hidden window of the port.
         - Add BCB6 package thanks to Andrzej Godawski <argut@tlen.pl> for
           providing me the code for it.
         - If data to send is larger than TxBuffer then an internal transmit
           buffer is used to buffer the data. The buffer will grow as needed and
           keep his size during lifetime of the port. His used memory can be
           cleared with the method ClearTxBuffer; By adding this buffer it is
           also possible to feed the component in a loop, however it is in many
           cases not a good programming practice.
         - New event OnSendProgress. It is fired every time TxBuffer is emtpy.
           This is usefull to feed a progressbar.

      1.16 1 feb 2006
         - Carlos Soto <carlos.soto@terra.es> found a bug in TCiaTxBuffer.Read
           where the pointer to the data was checked on Assigned which is a bug
           because it is given as var argument and can have any value.

         - Made 2 public functions: PortOpen and PortClose which return the
           success of the function. No exceptions are raised if port cannot open
           proposed by Nico <thunder_nico@hotmail.com>.

         - Add TCiaDelay which is a special delaying queue. Some pheriferals
           have trouble with fast PC's hanging up their command interpreter when
           data comes to fast after they have send something or when 2 data
           chunks come fast after another.

           TCiaDelay solve this adding a delay between each send chunck, and
           adding a delay for sending if something is received. In receive
           direction there is no delay, only in send direction. Just 'hang' the
           object between sender and receiver, and it will do it's work. Note
           that it's Sender argument is the original Sender of the data, not
           TCiaDelay.

           procedure TForm1.ComPortDataAvailable(Sender: TObject);
           var
              Len: integer;
              Port: TCiaComPort;
           begin
              Port := TCiaComPort(Sender);
              Len := Port.Receive(Buffer, SizeOf(Buffer));
              Dly.Receive(Port, Buffer, Len);
           end;

           // This set the delay and will fire immediatly OnReceive
           // Note that Sender is here the ComPort, in this example we answer to same port
           procedure TForm1.CiaDelayReceive(Sender: TObject; Data: PByte; Len: integer);
           begin
              CiaDelay.Send(Sender, NextData, NextDataLen);
           end;

           // This update the delay if any and send eventually delayed OnSend
           // Note that Sender is here the port where who did the original send
           procedure TForm1.CiaDelaySend(Sender: TObject; Data: PByte; Len: integer);
           var
              Port: TCiaComPort;
           begin
              Port := TCiaComPort(Sender);
              Port.Send(Data, Len);
           end;

           Note that you can use Sender argument for any object you wants. The
           class will pass the right object trough.

         - TDelayQueue which is internally used by TCiaDelay is a special queue
           doing a minimal reallocation of memory. It grow it's buffer if needed
           and keep it for reusing the same memory over and over again to avoid
           unnececary CPU load or fragmented memory after a while.

      1.17 27 jan 2007
         - Made compatible for Delphi 2006 thanks to Robert Kondner
           <rkondner@indexdesigns.com>

      1.18 7 nov 2009
         - Made compatible for Delphi 2010 thanks to Robert Kondner
           <rkondner@indexdesigns.com>
         - Made a DontUseInternalBuffer in PortOptions. Vodaphone PCMCIA card
           device driver dont want to generate sent events. But never used
           because I used a virtual port splitter.

      1.19 31 jul 2012
         - F. Piette: replace TThread.Resume by TThread.Start (D2010 and above)

      1.20 11 sep 2012
         - F.Piette: Added a lot of more test for Assigned(FCommThread)
         - F.Piette: Added FCloseFlag to avoid infinite loop when closing port

      1.21 25 sep 2012
         - F.Piette: Added thread naming
}
