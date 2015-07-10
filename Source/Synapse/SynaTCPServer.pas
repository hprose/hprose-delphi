unit SynaTCPServer;

interface

uses
  Windows, Classes, SysUtils, SyncObjs,
  blcksock, synsock, CfThread, CfThreadPool;

type

  // Forward declarations

  TSynaTCPServer = class;

 { TSynaTCPServer }

  TSynaEvent = procedure(Sender: TSynaTCPServer; Socket: TBlockSocket) of object;
  TSynaDataEvent = procedure(Sender: TSynaTCPServer; Socket: TBlockSocket; Data: AnsiString) of object;

  TSynaTCPServer = class(TComponent)
  private
    FActive:             Boolean;
    FDefaultPort:        Word;
    FListenSocket:       TTCPBlockSocket;
    FListenThread:       TCfThread;
    FThreadPool:         TCfThreadPool;
    FOnConnectEvent:     TSynaEvent;
    FOnDisconnect:       TSynaEvent;
    FOnDataReceive:      TSynaDataEvent;
    procedure SetActive(Value: Boolean);
  protected
    procedure DoDataReceive(Socket: TBlockSocket; Data: AnsiString); virtual;
    procedure DoConnect(Socket: TBlockSocket); virtual;
    procedure DoDisconnect(Socket: TBlockSocket); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartListening;
    procedure StopListening;
  published
    property Active: Boolean read FActive write SetActive default False;
    property DefaultPort: Word read FDefaultPort write FDefaultPort;
    property ThreadPool: TCfThreadPool read FThreadPool;
    property OnConnect: TSynaEvent read FOnConnectEvent write FOnConnectEvent;
    property OnDisConnect: TSynaEvent read FOnDisconnect write FOnDisconnect;
    property OnDataReceive: TSynaDataEvent read FOnDataReceive write FOnDataReceive;
  end;

implementation

type

  { TListenThread }

  TListenThread = class(TCfThread)
  private
    FOwner: TSynaTCPServer;
  protected
    procedure Run; override;
  public
    constructor Create(Owner: TSynaTCPServer); reintroduce;
  end;

  { TPooledEvent }

  TPooledEvent = class(TInterfacedObject, ICfThreadPoolCallback)
  private
    FOwner: TSynaTCPServer;
    FSocket: TTCPBlockSocket;
  protected
    procedure Callback(Caller: TCfThreadPool; Thread: TCfThread);
  public
    constructor Create(Owner: TSynaTCPServer; Socket: TTCPBlockSocket);
    destructor Destroy; override;
  end;

{ TServerListener }

constructor TListenThread.Create(Owner: TSynaTCPServer);
begin
  inherited Create(True, True, 'Hprose Listener');
  FOwner := Owner;
  Priority := tpHighest;
  FreeOnTerminate := True;
end;

procedure TListenThread.Run;
var
  Sock: TSocket;
  RealSock: TTCPBlockSocket;
begin
  Sock := FOwner.FListenSocket.Accept;
  if (Sock = 0) or (Sock = INVALID_SOCKET) then
  begin
    Stop;
    Exit;
  end;
  RealSock := TTCPBlockSocket.Create;
  RealSock.Socket := Sock;
  RealSock.RaiseExcept := True;
  FOwner.DoConnect(RealSock);
  FOwner.FThreadPool.QueueItem(TPooledEvent.Create(FOwner, RealSock));
end;

{ TPooledEvent }

constructor TPooledEvent.Create(Owner: TSynaTCPServer; Socket: TTCPBlockSocket);
begin
  FOwner := Owner;
  FSocket := Socket;
end;

destructor TPooledEvent.Destroy;
begin
  FSocket.CloseSocket;
  FSocket.Free;
  inherited;
end;

procedure TPooledEvent.Callback(Caller: TCfThreadPool; Thread: TCfThread);
var
  S: AnsiString;
begin
  while not Thread.Terminated do
  begin
    try
      S := FSocket.RecvPacket(3000);
      if Length(S) > 0 then
        FOwner.DoDataReceive(FSocket, S);
    except
      on E:ESynapseError do
      begin
        if E.ErrorCode = WSAECONNRESET then
        begin
          FOwner.DoDisconnect(FSocket);
          Break;
        end
        else if E.ErrorCode = WSAETIMEDOUT then
          Continue
        else
          Break;
      end;
    end;
  end;
end;

{ TSynaTCPServer }

constructor TSynaTCPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListenSocket := TTCPBlockSocket.Create;
  FListenSocket.RaiseExcept := True;
  FThreadPool := TCfThreadPool.Create(Self);
end;

destructor TSynaTCPServer.Destroy;
begin
  StopListening;
  FListenSocket.Free;
  inherited;
end;

procedure TSynaTCPServer.SetActive(Value: Boolean);
begin
  if FActive <> Value then
  begin
    if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    begin
      FActive := Value;
    end
    else
    begin
      if Value then
        StartListening
      else
        StopListening;
    end;
  end;
end;

procedure TSynaTCPServer.DoDataReceive(Socket: TBlockSocket; Data: AnsiString);
begin
  if Assigned(OnDataReceive) then OnDataReceive(Self, Socket, Data);
end;

procedure TSynaTCPServer.DoConnect(Socket: TBlockSocket);
begin
  if Assigned(OnConnect) then OnConnect(Self, Socket);
end;

procedure TSynaTCPServer.DoDisconnect(Socket: TBlockSocket);
begin
  if Assigned(OnDisconnect) then OnDisconnect(Self, Socket);
end;

procedure TSynaTCPServer.Loaded;
begin
  inherited;
  if Active then
  begin
    FActive := False;
    SetActive(True);
  end;
end;

procedure TSynaTCPServer.StartListening;
begin
  if not FActive then
  begin
    try
      FListenSocket.CreateSocket;
      FListenSocket.Bind('0.0.0.0', IntToStr(FDefaultPort));
      FListenSocket.Listen;
      FListenThread := TListenThread.Create(Self);
      FListenThread.Start;
      FActive := True;
    except
      FActive := True;
      SetActive(False);
      raise;
    end;
  end;
end;

procedure TSynaTCPServer.StopListening;
begin
  if FActive then
  begin
    if FListenThread <> nil then
      FListenThread.Terminate;
    FListenSocket.CloseSocket;
    FActive := False;
  end;
end;

end.
