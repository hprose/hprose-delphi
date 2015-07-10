unit SynaHttpServer;

interface

uses
  Windows, Classes, SysUtils, StrUtils, SyncObjs,
  SynaTCPServer, blcksock;

type

  // Forward declarations

  THTTPSessionList = class;

  THTTPCommandType = (hcUnknown, hcHEAD, hcGET, hcPOST, hcDELETE, hcPUT, hcTRACE, hcOPTION);
  THTTPCommandTypes = set of THTTPCommandType;

  { TRequestInfo }

  TRequestInfo = class
  private
    FSocket:        TBlockSocket;
    FConnection:    string;
    FContentLength: Int64;
    FContentStream: TStream;
    FCommand:       string;
    FVersion:       string;
    FRawCommand:    string;
    FRawHeaders:    TStringList;
    FParams:        TStringList;
    FCommandType:   THTTPCommandType;
  public
    constructor Create(Socket: TBlockSocket);
    destructor Destroy; override;
    procedure ParseRequest(const Request: AnsiString);
    property Connection: string read FConnection;
    property ContentStream: TStream read FContentStream;
    property Command: string read FCommand;
    property CommandType: THTTPCommandType read FCommandType;
    property Version: string read FVersion;
    property RawCommand: string read FRawCommand;
    property RawHeaders: TStringList read FRawHeaders;
    property Params: TStringList read FParams;
  end;

  { TResponseInfo }

  TResponseInfo = class
  private
    FSocket:           TBlockSocket;
    FCacheControl:     string;
    FCloseConnection:  Boolean;
    FConnection:       string;
    FContentType:      string;
    FHeaders:          AnsiString;
    FContentStream:    TMemoryStream;
    FExpires:          TDateTime;
    FResponseNo:       Integer;
    FResponseText:     string;
    FCustomHeaders:    TStringList;
  private
    procedure SetCloseConnection(const Value: Boolean);
    procedure SetResponseNo(const Value: Integer);
  public
    constructor Create(Socket: TBlockSocket);
    destructor Destroy; override;
    procedure WriteHeader;
    procedure WriteContent;
    property CacheControl: string read FCacheControl write FCacheControl;
    property CloseConnection: Boolean read FCloseConnection write SetCloseConnection;
    property ContentType: string read FContentType write FContentType;
    property ContentStream: TMemoryStream read FContentStream write FContentStream;
    property Expires: TDateTime read FExpires write FExpires;
    property ResponseNo: Integer read FResponseNo write SetResponseNo;
    property CustomHeaders: TStringList read FCustomHeaders;
  end;

  { THTTPSession }

  THTTPSession = class
  private
    FInUse:         Boolean;
    FLastTimeStamp: Cardinal;
    FLock:          TCriticalSection;
    FOwner:         THTTPSessionList;
    FSessionID:     string;
  public
    constructor Create(Owner: THTTPSessionList; const SessionID: string); virtual;
    destructor Destroy; override;
    property InUse: Boolean read FInUse write FInUse;
    property LastTimeStamp: Cardinal read FLastTimeStamp;
    property SessionID: string read FSessionID;
  end;

  { THTTPSessionList }

  THTTPSessionList = class
  private
    FSessionList: TThreadList;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSession(const SessionID: string): THTTPSession; virtual;
    procedure PurgeStaleSessions(PurgeAll: Boolean = False); virtual;
  end;

  { TSynaHTTPServer }

  THTTPCommandEvent = procedure(RequestInfo: TRequestInfo; ResponseInfo: TResponseInfo) of object;

  TSynaHTTPServer = class(TSynaTCPServer)
  private
    FKeepAlive:      Boolean;
    FSessionList:    THTTPSessionList;
    FSessionThread:  TThread;
    FOnCommandGet:   THTTPCommandEvent;
    FOnCommandOther: THTTPCommandEvent;
  protected
    function CheckHeaders(RequestInfo: TRequestInfo): Boolean;
    procedure DoCommandGet(Socket: TBlockSocket; RequestInfo: TRequestInfo;
     ResponseInfo: TResponseInfo); virtual;
    procedure DoCommandOther(Socket: TBlockSocket; RequestInfo: TRequestInfo;
     ResponseInfo: TResponseInfo); virtual;
    procedure DoDataReceive(Socket: TBlockSocket; Data: AnsiString); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive default True;
    property SessionList: THTTPSessionList read FSessionList;
    property OnCommandGet: THTTPCommandEvent read FOnCommandGet write FOnCommandGet;
    property OnCommandOther: THTTPCommandEvent read FOnCommandOther write FOnCommandOther;
  end;

function DateTimeGMTToHttpStr(const Value: TDateTime) : string;

implementation

// Utility functions

function DecodeHTTPCommand(const Cmd: string): THTTPCommandType;
const
  HTTPComandStrings: array[0..Ord(High(THTTPCommandType))] of string = (
    'UNKNOWN', 'HEAD', 'GET', 'POST', 'DELETE', 'PUT', 'TRACE', 'OPTIONS');
var
  I: Integer;
begin
  Result := hcUnknown;
  for I := Low(HTTPComandStrings) to High(HTTPComandStrings) do
  begin
    if SameText(Cmd, HTTPComandStrings[I]) then
    begin
      Result := THTTPCommandType(I);
      Exit;
    end;
  end;
end;

function DateTimeGMTToHttpStr(const Value: TDateTime) : string;
const
  WeekDays: array[1..7] of string = (
    'Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  MonthNames: array[1..12] of string = (
    'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
var
  Day, Month, Year: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := Format('%s, %.2d %s %.4d %s %s', [WeekDays[DayOfWeek(Value)], Day,
    MonthNames[Month], Year, FormatDateTime('HH":"nn":"ss',Value ), 'GMT']);
end;

type

  { THTTPSessionCleanerThread }

  THTTPSessionCleanerThread = class(TThread)
  private
    FSessionList: THTTPSessionList;
  protected
    procedure Execute; override;
  public
    constructor Create(SessionList: THTTPSessionList); reintroduce;
  end;

{ THTTPSessionCleanerThread }

constructor THTTPSessionCleanerThread.Create(SessionList: THTTPSessionList);
begin
  inherited Create(True);
  Priority := tpLowest;
  FSessionList := SessionList;
  FreeOnTerminate := False;
  Resume;
end;

procedure THTTPSessionCleanerThread.Execute;
begin
  while not Terminated do
  begin
    Sleep(1000);
    if Assigned(FSessionList) then FSessionList.PurgeStaleSessions;
  end;
  FSessionList.PurgeStaleSessions(True);
end;

{ TRequestInfo }

constructor TRequestInfo.Create(Socket: TBlockSocket);
begin
  FSocket := Socket;
  FCommandType := hcUnknown;
  FContentLength := -1;
  FContentStream := TMemoryStream.Create;
  FRawHeaders := TStringList.Create;
  FParams := TStringList.Create;
  FParams.Delimiter := '&';
end;

destructor TRequestInfo.Destroy;
begin
  FContentStream.Free;
  FParams.Free;
  FRawHeaders.Free;
  inherited;
end;

procedure TRequestInfo.ParseRequest(const Request: AnsiString);
var
  I, J, S: Integer;
  Line: string;
  Expect: string;
  Content: AnsiString;
begin
  I := Pos(CRLF, Request);
  if I = 0 then Exit;

  Line := Copy(Request, 1, I - 1);
  J := 1;
  repeat
    I := PosEx(' ', Line, J);
    if I > 0 then J := I + 1;
  until I = 0;
  if J = 1 then Exit;

  FVersion := Copy(Line, J, MaxInt);
  FRawCommand := Copy(Line, 1, J - 2);
  I := Pos('?', FRawCommand);
  if I > 0 then FParams.Add(Copy(FRawCommand, I + 1, MaxInt));
  I := Pos(' ', FRawCommand);
  if I > 0 then FCommand := Copy(FRawCommand, 1, I - 1);
  FCommandType := DecodeHTTPCommand(FCommand);

  I := -1;
  repeat
    J := I + 2;
    I := PosEx(CRLF, Request, J);
    if I > 0 then
    begin
      Line := Copy(Request, J, I - J);
      if Line = '' then Break;      
      S := Pos(': ', Line);
      if S > 0 then
      begin
        Line := Copy(Line, 1, S - 1) + '=' + Copy(Line, S + 2, MaxInt);
        FRawHeaders.Add(Line);
      end;
    end;
  until I = 0;

  FConnection := FRawHeaders.Values['Connection'];
  FContentLength := StrToInt64Def(FRawHeaders.Values['Content-Length'], 0);

  Expect := FRawHeaders.Values['Expect'];
  if Pos('100-continue', LowerCase(Expect)) > 0 then
  begin
    FSocket.SendString(AnsiString(FVersion) + ' 100 Continue' + CRLF + CRLF);
  end
  else
  begin
    if FContentLength > 0 then
    begin
      Content := AnsiString(Copy(Request, I + 2, MaxInt));
      FContentStream.Write(PAnsiChar(Content)^, Length(Content));
    end;
  end;

  while FContentLength > FContentStream.Size do
  begin
    Content := FSocket.RecvPacket(3000);
    FContentStream.Write(PAnsiChar(Content)^, Length(Content));
  end;

  if FContentStream.Size > 0 then
  begin
    FContentStream.Seek(0, soFromBeginning);
  end;
end;

{ TResponseInfo }

constructor TResponseInfo.Create(Socket: TBlockSocket);
begin
  FSocket := Socket;
  ResponseNo := 200;
  FCustomHeaders := TStringList.Create;
  FContentStream := TMemoryStream.Create;
end;

destructor TResponseInfo.Destroy;
begin
  FCustomHeaders.Free;
  FContentStream.Free;
  inherited;
end;

procedure TResponseInfo.SetCloseConnection(const Value: Boolean);
begin
  FCloseConnection := Value;
  if FCloseConnection then
    FConnection := 'close'
  else
    FConnection := 'keep-alive';
end;

procedure TResponseInfo.SetResponseNo(const Value: Integer);
begin
  FResponseNo := Value;
  case FResponseNo of
    200: FResponseText := 'OK';
  end;
end;

procedure TResponseInfo.WriteHeader;
var
  ContentLength: Integer;
  Headers: string;
  I: Integer;
begin
  ContentLength := FContentStream.Size;
  Headers := 'HTTP/1.1 ' + IntToStr(FResponseNo) + ' ' + FResponseText + CRLF;
  Headers := Headers + 'Connection: ' + FConnection + CRLF;
  Headers := Headers + 'Content-Length: ' + IntToStr(ContentLength) + CRLF;
  if Length(FContentType) > 0  then
    Headers := Headers + 'Content-Type: ' + FContentType + CRLF;
  if Length(FCacheControl) > 0  then
    Headers := Headers + 'Cache-control: ' + FCacheControl + CRLF;
  Headers := Headers + 'Expires: ' + DateTimeGMTToHttpStr(FExpires) + CRLF;

  for I := 0 to FCustomHeaders.Count - 1 do
    Headers := Headers + FCustomHeaders[I] + CRLF;

  Headers := Headers + CRLF;
  FHeaders := AnsiString(Headers);

end;

procedure TResponseInfo.WriteContent;
var
  Buffer: AnsiString;
begin
  SetLength(Buffer, FContentStream.Size);
  FContentStream.Seek(0, soFromBeginning);
  FContentStream.ReadBuffer(Buffer[1], FContentStream.Size);
  Buffer := FHeaders + Buffer;
  FSocket.SendBuffer(PAnsiChar(Buffer), Length(Buffer));

end;

{ THTTPSession }

constructor THTTPSession.Create(Owner: THTTPSessionList; const SessionID: string);
begin
  FOwner := Owner;
  FInUse := False;
  FSessionID  := SessionID;
  FLock := TCriticalSection.Create;
  FOwner.FSessionList.Add(Self);
end;

destructor THTTPSession.Destroy;
begin
  FOwner.FSessionList.Remove(Self);
  FLock.Free;
  inherited;
end;

{ THTTPSessionList }

constructor THTTPSessionList.Create;
begin
  FSessionList := TThreadList.Create;
end;

destructor THTTPSessionList.Destroy;
var
  List: TList;
  Session: THTTPSession;
begin
  List := FSessionList.LockList;
  try
    while List.Count > 0 do
    begin
      Session := THTTPSession(List.Last);
      Session.Free;
    end;
  finally
    FSessionList.UnlockList;
  end;
  FSessionList.Free;
  inherited;
end;

function THTTPSessionList.GetSession(const SessionID: string): THTTPSession;
var
  I: Integer;
  List: TList;
  Session: THTTPSession;
begin
  Result := nil;
  List := FSessionList.LockList;
  try
    for I := 0 to List.Count - 1 do
    begin
      Session := THTTPSession(List[I]);
      if SameText(Session.FSessionID, SessionID) then
      begin
        Session.FLastTimeStamp := GetTickCount;
        Result := Session;
        Break;
      end;
    end;
  finally
    FSessionList.UnlockList;
  end;
end;

procedure THTTPSessionList.PurgeStaleSessions(PurgeAll: Boolean);
var
  I: Integer;
  List: TList;
  Session: THTTPSession;
begin
  List := FSessionList.LockList;
  try
    for I := List.Count - 1 downto 0 do
    begin
      Session := THTTPSession(List[I]);
      if Assigned(Session) then
      begin
        if PurgeAll or ((GetTickCount - Session.FLastTimeStamp > 600000) and Session.InUse) then
          Session.Free;
      end;
    end;
  finally
    FSessionList.UnlockList;
  end;
end;

{ TSynaHTTPServer }

constructor TSynaHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSessionList := THTTPSessionList.Create;
  FSessionThread := THTTPSessionCleanerThread.Create(FSessionList);
end;

destructor TSynaHTTPServer.Destroy;
begin
  FSessionThread.Terminate;
  FSessionThread.WaitFor;
  FSessionThread.Free;
  FSessionList.Free;
  inherited;
end;

function TSynaHTTPServer.CheckHeaders(RequestInfo: TRequestInfo): Boolean;
begin
  Result := True;
end;

procedure TSynaHTTPServer.DoCommandGet(Socket: TBlockSocket;
  RequestInfo: TRequestInfo; ResponseInfo: TResponseInfo);
begin
  if Assigned(OnCommandGet) then
    OnCommandGet(RequestInfo, ResponseInfo);
end;

procedure TSynaHTTPServer.DoCommandOther(Socket: TBlockSocket;
  RequestInfo: TRequestInfo; ResponseInfo: TResponseInfo);
begin
  if Assigned(OnCommandOther) then
    OnCommandOther(RequestInfo, ResponseInfo);
end;

function URLDecode(const S: string): string;
var
  Idx: Integer;   // loops thru chars in string
  Hex: string;    // string of hex characters
  Code: Integer;  // hex character code (-1 on error)
begin
  // Intialise result and string index
  Result := '';
  Idx := 1;
  // Loop thru string decoding each character
  while Idx <= Length(S) do
  begin
    case S[Idx] of
      '%':
      begin
        // % should be followed by two hex digits - exception otherwise
        if Idx <= Length(S) - 2 then
        begin
          // there are sufficient digits - try to decode hex digits
          Hex := S[Idx+1] + S[Idx+2];
          Code := SysUtils.StrToIntDef('$' + Hex, -1);
          Inc(Idx, 2);
        end
        else
          // insufficient digits - error
          Code := -1;
        // check for error and raise exception if found
        if Code = -1 then
          raise SysUtils.EConvertError.Create(
            'Invalid hex digit in URL'
          );
        // decoded OK - add character to result
        Result := Result + Chr(Code);
      end;
      '+':
        // + is decoded as a space
        Result := Result + ' '
      else
        // All other characters pass thru unchanged
        Result := Result + S[Idx];
    end;
    Inc(Idx);
  end;
end;

procedure TSynaHTTPServer.DoDataReceive(Socket: TBlockSocket; Data: AnsiString);
var
  RequestInfo:  TRequestInfo;
  ResponseInfo: TResponseInfo;
begin
  RequestInfo := TRequestInfo.Create(Socket);
  RequestInfo.ParseRequest(Data);

  try
    ResponseInfo := TResponseInfo.Create(Socket);
    try
      if not CheckHeaders(RequestInfo) then Exit;

      ResponseInfo.CloseConnection :=
        not (FKeepAlive and SameText(RequestInfo.Connection, 'Keep-alive'));

      try
        if RequestInfo.CommandType in [hcGET, hcPOST, hcHEAD] then
          DoCommandGet(Socket, RequestInfo, ResponseInfo)
        else
          DoCommandOther(Socket, RequestInfo, ResponseInfo);
      except
      end;

      ResponseInfo.WriteHeader;
      ResponseInfo.WriteContent;

    finally
      ResponseInfo.Free;
    end;
  finally
    RequestInfo.Free;
  end;
end;

end.
