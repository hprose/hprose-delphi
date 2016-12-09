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
 * LastModified: Dec 9, 2016                              *
 * Author: Ma Bingyao <andot@hprose.com>                  *
 *                                                        *
\**********************************************************/
}
unit HproseHttpClient;

interface

uses Classes, HproseCommon, HproseClient, SysUtils{$IFDEF FPC}, LResources{$ENDIF};

type

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
    FHeaders: IMap;
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
    property Headers: IMap read FHeaders;

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

var
  cookieManager: IMap;

procedure SetCookie(Header: IMap; const Host: string);
var
  I, Pos: Integer;
  Name, Value, CookieString, Path: string;
  Cookie: IMap;
begin
  for I := 0 to Header.Count - 1 do begin
    Name := LowerCase(Header.Keys[I]);
    if (Name = 'set-cookie') or (Name = 'set-cookie2') then begin
      Value := Header.Values[I];
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
  Header, HttpHeader: IMap;
  I: Integer;
  K, V: string;
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
  Header := TCaseInsensitiveHashMap.Create;
  Header.PutAll(FHeaders);
  HttpHeader := VarToMap(Context['httpHeader']);
  if (Assigned(HttpHeader)) then
    Header.PutAll(HttpHeader)
  else
    HttpHeader := TCaseInsensitiveHashMap.Create;
  for I := 0 to Header.Count - 1 do
    HttpSend.Headers.Values[Header.Keys[I]] := Header.Values[I];
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
    HttpHeader.Clear();
    for I := 0 to HttpSend.Headers.Count - 1 do begin
      K := HttpSend.Headers.Names[I];
      V := HttpSend.Headers.Values[K];
      HttpHeader.Put(K, V);
    end;
    Context['httpHeader'] := HttpHeader;
    SetCookie(HttpHeader, FHost);
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
  FHeaders := TCaseInsensitiveHashMap.Create;
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
