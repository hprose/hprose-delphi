{
/**********************************************************\
|                                                          |
|                          hprose                          |
|                                                          |
| Official WebSite: http://www.hprose.com/                 |
|                   http://www.hprose.org/                 |
|                                                          |
\**********************************************************/

/**********************************************************\
 *                                                        *
 * HproseHttpClient.pas                                   *
 *                                                        *
 * hprose synapse http client unit for delphi.            *
 *                                                        *
 * LastModified: Nov 23, 2016                             *
 * Author: Ma Bingyao <andot@hprose.com>                  *
 *                                                        *
\**********************************************************/
}
unit HproseHttpClient;

interface

uses Classes, HproseCommon, HproseClient, SysUtils{$IFDEF FPC}, LResources{$ENDIF};

type
  THeaderList = class(TStringList)
  protected
    FNameValueSeparator: string;
    FCaseSensitive: Boolean;
    FUnfoldLines: Boolean;
    FFoldLines: Boolean;
    FFoldLinesLength: Integer;
    procedure DeleteFoldedLines(Index: Integer);
    function FoldLine(AString: string): TStringList;
    procedure FoldAndInsert(AString: string; Index: Integer);
    function GetName(Index: Integer): string;
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
    function GetValueFromLine(ALine: Integer): string;
    function GetNameFromLine(ALine: Integer): string;
  public
    constructor Create;
    procedure Extract(const AName: string; ADest: TStrings);
    function IndexOfName(const Name: string): Integer; reintroduce;
    property Names[Index: Integer]: string read GetName;
    property Values[const Name: string]: string read GetValue write SetValue;
    property NameValueSeparator: string read FNameValueSeparator
    write FNameValueSeparator;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property UnfoldLines: Boolean read FUnfoldLines write FUnfoldLines;
    property FoldLines: Boolean read FFoldLines write FFoldLines;
    property FoldLength: Integer read FFoldLinesLength write FFoldLinesLength;
  end;

  { THproseHttpClient }

  THproseHttpClient = class(THproseClient)
  private
    FHttpPool: IList;
    FProtocol: string;
    FUser: string;
    FPassword: string;
    FHost: string;
    FPort: string;
    FPath: string;
    FPara: string;
    FHeaders: THeaderList;
    FKeepAlive: Boolean;
    FKeepAliveTimeout: Integer;
    FProxyHost: string;
    FProxyPort: Integer;
    FProxyUser: string;
    FProxyPass: string;
    FUserAgent: string;
    FConnectionTimeout: Integer;
  protected
    function SendAndReceive(const Data: TBytes;
      const Context: TClientContext): TBytes; override;
    procedure InitURI(const AValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {:Before HTTP operation you may define any non-standard headers for HTTP
     request, except of: 'Expect: 100-continue', 'Content-Length', 'Content-Type',
     'Connection', 'Authorization', 'Proxy-Authorization' and 'Host' headers.}
    property Headers: THeaderList read FHeaders;

    {:If @true (default value is @true), keepalives in HTTP protocol 1.1 is enabled.}
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;

    {:Define timeout for keepalives in seconds! Default value is 300.}
    property KeepAliveTimeout: Integer read FKeepAliveTimeout write FKeepAliveTimeout;

    {:Address of proxy server (IP address or domain name).}
    property ProxyHost: string read FProxyHost write FProxyHost;

    {:Port number for proxy connection. Default value is 8080.}
    property ProxyPort: Integer read FProxyPort write FProxyPort;

    {:Username for connect to proxy server.}
    property ProxyUser: string read FProxyUser write FProxyUser;

    {:Password for connect to proxy server.}
    property ProxyPass: string read FProxyPass write FProxyPass;

    {:Here you can specify custom User-Agent indentification. By default is
     used: 'Hprose Http Client for Delphi (Synapse)'}
    property UserAgent: string read FUserAgent write FUserAgent;

    {:UserName for user authorization.}
    property UserName: string read FUser write FUser;

    {:Password for user authorization.}
    property Password: string read FPassword write FPassword;

    {:Define timeout for ConnectionTimeout in milliseconds! Default value is 10000.}
    property ConnectionTimeout: Integer read FConnectionTimeout write FConnectionTimeout;
  end;

procedure Register;

implementation

uses httpsend, synautil, Variants;

const
  LWS = [#9, ' '];
  LF = #10;
  CR = #13;
  EOL = CR + LF;

function FoldWrapText(const Line, BreakStr: string; BreakChars: TSysCharSet;
  MaxCol: Integer): string;
const
  QuoteChars = ['"'];
var
  Col, Pos: Integer;
  LinePos, LineLen: Integer;
  BreakLen, BreakPos: Integer;
  QuoteChar, CurChar: Char;
  ExistingBreak: Boolean;
begin
  Col := 1;
  Pos := 1;
  LinePos := 1;
  BreakPos := 0;
  QuoteChar := ' ';
  ExistingBreak := False;
  LineLen := Length(Line);
  BreakLen := Length(BreakStr);
  Result := '';
  while Pos <= LineLen do
  begin
    CurChar := Line[Pos];
    if CurChar in LeadBytes then
    begin
      Inc(Pos);
      Inc(Col);
    end
    else
      if CurChar = BreakStr[1] then
    begin
      if QuoteChar = ' ' then
      begin
        ExistingBreak := AnsiSameText(BreakStr, Copy(Line, Pos, BreakLen));
        if ExistingBreak then
        begin
          Inc(Pos, BreakLen - 1);
          BreakPos := Pos;
        end;
      end
    end
    else
      if CurChar in BreakChars then
    begin
      if QuoteChar = ' ' then
        BreakPos := Pos
    end
    else
      if CurChar in QuoteChars then
      if CurChar = QuoteChar then
        QuoteChar := ' '
      else
        if QuoteChar = ' ' then
        QuoteChar := CurChar;
    Inc(Pos);
    Inc(Col);
    if not (QuoteChar in QuoteChars) and (ExistingBreak or
      ((Col > MaxCol) and (BreakPos > LinePos))) then
    begin
      Col := Pos - BreakPos;
      Result := Result + Copy(Line, LinePos, BreakPos - LinePos + 1);
      if not (CurChar in QuoteChars) then
        while (Pos <= LineLen) and (Line[Pos] in BreakChars + [#13, #10]) do
          Inc(Pos);
      if not ExistingBreak and (Pos < LineLen) then
        Result := Result + BreakStr;
      Inc(BreakPos);
      LinePos := BreakPos;
      ExistingBreak := False;
    end;
  end;
  Result := Result + Copy(Line, LinePos, MaxInt);
end;

{ THeaderList }

constructor THeaderList.Create;
begin
  inherited Create;
  FNameValueSeparator := ': ';
  FCaseSensitive := False;
  FUnfoldLines := True;
  FFoldLines := True;
  FFoldLinesLength := 78;
end;

procedure THeaderList.DeleteFoldedLines(Index: Integer);
begin
  Inc(Index);
  while (Index < Count) and ((Length(Get(Index)) > 0) and
    (Get(Index)[1] = ' ') or (Get(Index)[1] = #9)) do
  begin
    Delete(Index);
  end;
end;

procedure THeaderList.Extract(const AName: string; ADest: TStrings);
var
  idx: Integer;
begin
  if not Assigned(ADest) then
    Exit;
  for idx := 0 to Count - 1 do
  begin
    if AnsiSameText(AName, GetNameFromLine(idx)) then
    begin
      ADest.Add(GetValueFromLine(idx));
    end;
  end;
end;

procedure THeaderList.FoldAndInsert(AString: string; Index: Integer);
var
  strs: TStringList;
  idx: Integer;
begin
  strs := FoldLine(AString);
  try
    idx := strs.Count - 1;
    Put(Index, strs[idx]);
    Dec(idx);
    while (idx > -1) do
    begin
      Insert(Index, strs[idx]);
      Dec(idx);
    end;
  finally
    FreeAndNil(strs);
  end;
end;

function THeaderList.FoldLine(AString: string): TStringList;
var
  s: string;
begin
  Result := TStringList.Create;
  try
    s := FoldWrapText(AString, EOL + ' ', LWS, FFoldLinesLength);
    while s <> '' do
    begin
      Result.Add(TrimRight(Fetch(s, EOL)));
    end;
  finally
  end;
end;

function THeaderList.GetName(Index: Integer): string;
var
  P: Integer;
begin
  Result := Get(Index);
  P := AnsiPos(FNameValueSeparator, Result);
  if P <> 0 then
  begin
    SetLength(Result, P - 1);
  end
  else
  begin
    SetLength(Result, 0);
  end;
  Result := Result;
end;

function THeaderList.GetNameFromLine(ALine: Integer): string;
var
  p: Integer;
begin
  Result := Get(ALine);
  if not FCaseSensitive then
  begin
    Result := UpperCase(Result);
  end;
  P := AnsiPos(TrimRight(FNameValueSeparator), Result);
  Result := Copy(Result, 1, P - 1);
end;

function THeaderList.GetValue(const Name: string): string;
begin
  Result := GetValueFromLine(IndexOfName(Name));
end;

function THeaderList.GetValueFromLine(ALine: Integer): string;
var
  Name: string;
begin
  if ALine >= 0 then
  begin
    Name := GetNameFromLine(ALine);
    Result := Copy(Get(ALine), Length(Name) + 2, MaxInt);
    if FUnfoldLines then
    begin
      Inc(ALine);
      while (ALine < Count) and ((Length(Get(ALine)) > 0) and
        (Get(ALine)[1] in LWS)) do
      begin
        if (Result[Length(Result)] in LWS) then
        begin
          Result := Result + TrimLeft(Get(ALine))
        end
        else
        begin
          Result := Result + ' ' + TrimLeft(Get(ALine))
        end;
        inc(ALine);
      end;
    end;
  end
  else
  begin
    Result := '';
  end;
  Result := TrimLeft(Result);
end;

function THeaderList.IndexOfName(const Name: string): Integer;
var
  S: string;
begin
  for Result := 0 to Count - 1 do
  begin
    S := GetNameFromLine(Result);
    if (AnsiSameText(S, Name)) then
    begin
      Exit;
    end;
  end;
  Result := -1;
end;

procedure THeaderList.SetValue(const Name, Value: string);
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if Value <> '' then
  begin
    if I < 0 then
    begin
      I := Add('');
    end;
    if FFoldLines then
    begin
      DeleteFoldedLines(I);
      FoldAndInsert(Name + FNameValueSeparator + Value, I);
    end
    else
    begin
      Put(I, Name + FNameValueSeparator + Value);
    end;
  end
  else
  begin
    if I >= 0 then
    begin
      if FFoldLines then
      begin
        DeleteFoldedLines(I);
      end;
      Delete(I);
    end;
  end;
end;

///////////////////////////////////////////////////////////////

var
  cookieManager: IMap;

procedure SetCookie(Header: TStringList; const Host: string);
var
  I, Pos: Integer;
  Name, Value, CookieString, Path: string;
  Cookie: IMap;
begin
  for I := 0 to Header.Count - 1 do begin
    Value := Header.Strings[I];
    Pos := AnsiPos(':', Value);
    Name := LowerCase(Copy(Value, 1, Pos - 1));
    if (Name = 'set-cookie') or (Name = 'set-cookie2') then begin
      Value := Trim(Copy(Value, Pos + 1, MaxInt));
      Pos := AnsiPos(';', Value);
      CookieString := Copy(Value, 1, Pos - 1);
      Value := Copy(Value, Pos + 1, MaxInt);
      Cookie := TCaseInsensitiveHashMap.Split(Value, ';', '=', 0, True, False, True);
      Pos := AnsiPos('=', CookieString);
      Cookie['name'] := Copy(CookieString, 1, Pos - 1);
      Cookie['value'] := Copy(CookieString, Pos + 1, MaxInt);
      if Cookie.ContainsKey('path') then begin
        Path := Cookie['path'];
        if (Length(Path) > 0) then begin
          if (Path[1] = '"') then Delete(Path, 1, 1);
          if (Path[Length(Path)] = '"') then SetLength(Path, Length(Path) - 1);
        end;
        if (Length(Path) > 0) then
          Cookie['path'] := Path
        else
          Cookie['path'] := '/';
      end
      else
        Cookie['path'] := '/';
      if Cookie.ContainsKey('expires') then begin
        Cookie['expires'] := DecodeRfcDateTime(Cookie['expires']);
      end;
      if Cookie.ContainsKey('domain') then
        Cookie['domain'] := LowerCase(Cookie['domain'])
      else
        Cookie['domain'] := Host;
      Cookie['secure'] := Cookie.ContainsKey('secure');
      CookieManager.BeginWrite;
      try
        if not CookieManager.ContainsKey(Cookie['domain']) then
          CookieManager[Cookie['domain']] := THashMap.Create(False, True) as IMap;
        VarToMap(CookieManager[Cookie['domain']])[Cookie['name']] := Cookie;
      finally
        CookieManager.EndWrite;
      end;
    end;
  end;
end;

function GetCookie(const Host, Path: string; Secure: Boolean): string;
var
  Cookies, CookieMap, Cookie: IMap;
  Names: IList;
  Domain: string;
  I, J: Integer;
begin
  Cookies := THashMap.Create(False);
  CookieManager.BeginRead;
  try
    for I := 0 to CookieManager.Count - 1 do begin
      Domain := VarToStr(CookieManager.Keys[I]);
      if AnsiPos(Domain, Host) <> 0 then begin
        CookieMap := VarToMap(CookieManager.Values[I]);
        CookieMap.BeginRead;
        try
          Names := TArrayList.Create(False);
          for J := 0 to CookieMap.Count - 1 do begin
            Cookie := VarToMap(CookieMap.Values[J]);
            if Cookie.ContainsKey('expires') and (Cookie['expires'] < Now) then
              Names.Add(Cookie['name'])
            else if AnsiPos(Cookie['path'], Path) = 1 then begin
              if ((Secure and Cookie['secure']) or not Cookie['secure']) and
                  (Cookie['value'] <> '') then
                Cookies[Cookie['name']] := Cookie['value'];
            end;
          end;
        finally
          CookieMap.EndRead;
        end;
        if Names.Count > 0 then begin
          CookieMap.BeginWrite;
          try
            for J := 0 to Names.Count - 1 do CookieMap.Delete(Names[J]);
          finally
            CookieMap.EndWrite;
          end;
        end;
      end;
    end;
    Result := Cookies.Join('; ');
  finally
    CookieManager.EndRead;
  end;
end;

{ THproseHttpClient }

function THproseHttpClient.SendAndReceive(const Data: TBytes;
  const Context: TClientContext): TBytes;
var
  HttpSend: THttpSend;
  Cookie: string;
  Error: string;
begin
  FHttpPool.Lock;
  try
    if FHttpPool.Count > 0 then
      HttpSend := THttpSend(VarToObj(FHttpPool.Delete(FHttpPool.Count - 1)))
    else
      HttpSend := THttpSend.Create;
  finally
    FHttpPool.Unlock;
  end;
  HttpSend.Headers.Assign(FHeaders);
  HttpSend.KeepAlive := FKeepAlive;
  HttpSend.KeepAliveTimeout := FKeepAliveTimeout;
  HttpSend.UserName := FUser;
  HttpSend.Password := FPassword;
  HttpSend.ProxyHost := FProxyHost;
  if FProxyPort = 0 then
    HttpSend.ProxyPort := ''
  else
    HttpSend.ProxyPort := IntToStr(FProxyPort);
  HttpSend.ProxyUser := FProxyUser;
  HttpSend.ProxyPass := FProxyPass;
  HttpSend.UserAgent := FUserAgent;
  HttpSend.Sock.ConnectionTimeout := FConnectionTimeout;
  HttpSend.Timeout := Context.Settings.Timeout;
  HttpSend.Protocol := '1.1';
  HttpSend.MimeType := 'application/hprose';
  Cookie := GetCookie(FHost,
                      FPath,
                      LowerCase(FProtocol) = 'https');
  if Cookie <> '' then HttpSend.Headers.Add('Cookie: ' + Cookie);
  HttpSend.Document.WriteBuffer(Data[0], Length(Data));
  if (HttpSend.HTTPMethod('POST', URI)) then begin
    SetCookie(HttpSend.Headers, FHost);
    SetLength(Result, HttpSend.Document.Size);
    Move(HttpSend.Document.Memory^, Result[0], Length(Result));
    HttpSend.Clear;
    HttpSend.Cookies.Clear;
    FHttpPool.Lock;
    try
      FHttpPool.Add(ObjToVar(HttpSend));
    finally
      FHttpPool.Unlock;
    end;
  end
  else begin
    Error := IntToStr(HttpSend.Sock.LastError) + ':' + HttpSend.Sock.LastErrorDesc;
    FreeAndNil(HttpSend);
    raise Exception.Create(Error);
  end;
end;

constructor THproseHttpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpPool := TArrayList.Create(10);
  FHeaders := THeaderList.Create;
  FUser := '';
  FPassword := '';
  FKeepAlive := True;
  FKeepAliveTimeout := 300;
  FProxyHost := '';
  FProxyPort := 8080;
  FProxyUser := '';
  FProxyPass := '';
  FUserAgent := 'Hprose Http Client for Delphi (Synapse)';
  FConnectionTimeout := 10000;
end;

destructor THproseHttpClient.Destroy;
var
  I: Integer;
begin
  FHttpPool.Lock;
  try
    for I := FHttpPool.Count - 1 downto 0 do
      THTTPSend(VarToObj(FHttpPool.Delete(I))).Free;
  finally
    FHttpPool.Unlock;
  end;
  FreeAndNil(FHeaders);
  inherited;
end;

procedure THproseHttpClient.InitURI(const AValue: string);
begin
  inherited InitURI(AValue);
  ParseURL(URI, FProtocol, FUser, FPassword, FHost, FPort, FPath, FPara);
end;

procedure Register;
begin
  RegisterComponents('Hprose',[THproseHttpClient]);
end;

initialization
  CookieManager := TCaseInsensitiveHashMap.Create(False, True);
{$IFDEF FPC}
  {$I Hprose.lrs}
{$ENDIF}
end.
