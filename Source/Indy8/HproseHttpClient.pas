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
 * hprose indy http client unit for delphi.               *
 *                                                        *
 * LastModified: Nov 23, 2016                             *
 * Author: Ma Bingyao <andot@hprose.com>                  *
 *                                                        *
\**********************************************************/
}
unit HproseHttpClient;

interface

uses Classes, HproseCommon, HproseClient, IdHeaderList, IdURI, SysUtils;

type

  THeaderList = TIdHeaderList;

  THproseHttpClient = class(THproseClient)
  private
    FHttpPool: IList;
    FIdURI: TIdURI;
    FUserName: string;
    FPassword: string;
    FHeaders: THeaderList;
    FProxyHost: string;
    FProxyPort: Integer;
    FProxyUser: string;
    FProxyPass: string;
    FUserAgent: string;
    FKeepAlive: Boolean;
    FKeepAliveTimeout: Integer;
    FConnectionTimeout: Integer;
  protected
    function SendAndReceive(const Data: TBytes;
      const Context: TClientContext): TBytes; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitURI(const AValue: string); override;
  published
    {:Before HTTP operation you may define any non-standard headers for HTTP
     request, except of: 'Expect: 100-continue', 'Content-Length', 'Content-Type',
     'Connection', 'Authorization', 'Proxy-Authorization' and 'Host' headers.}
    property Headers: THeaderList read FHeaders;

    {:If @true (default value is @false), keepalives in HTTP protocol 1.1 is enabled.}
    property KeepAlive: Boolean read FKeepAlive write FKeepAlive;

    {:Define timeout for keepalives in seconds! Default value is 300.}
    property KeepAliveTimeout: integer read FKeepAliveTimeout write FKeepAliveTimeout;

    {:Address of proxy server (IP address or domain name).}
    property ProxyHost: string read FProxyHost Write FProxyHost;

    {:Port number for proxy connection. Default value is 8080.}
    property ProxyPort: Integer read FProxyPort Write FProxyPort;

    {:Username for connect to proxy server.}
    property ProxyUser: string read FProxyUser Write FProxyUser;

    {:Password for connect to proxy server.}
    property ProxyPass: string read FProxyPass Write FProxyPass;

    {:Here you can specify custom User-Agent indentification. By default is
     used: 'Hprose Http Client for Delphi (Indy8)'}
    property UserAgent: string read FUserAgent Write FUserAgent;

    {:UserName for user authorization.}
    property UserName: string read FUserName write FUserName;

    {:Password for user authorization.}
    property Password: string read FPassword write FPassword;

    {:Define timeout for ConnectionTimeout in milliseconds! Can't work in Indy8}
    property ConnectionTimeout: Integer read FConnectionTimeout write FConnectionTimeout;
  end;

procedure Register;

implementation

uses IdGlobal, IdHttp, Variants;

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
        // GMTToLocalDateTime of Indy 8 can't parse cookie expires directly.
        // Use StringReplace to fix this bug.
        Value := StringReplace(Cookie['expires'], '-', ' ', [rfReplaceAll]);
        Cookie['expires'] := GMTToLocalDateTime(Value);
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

constructor THproseHttpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpPool := TArrayList.Create(10);
  FIdURI := nil;
  FHeaders := THeaderList.Create;
  FUserName := '';
  FPassword := '';
  FKeepAlive := True;
  FKeepAliveTimeout := 300;
  FProxyHost := '';
  FProxyPort := 8080;
  FProxyUser := '';
  FProxyPass := '';
  FUserAgent := 'Hprose Http Client for Delphi (Indy8)';
  FConnectionTimeout := 10000;
end;

destructor THproseHttpClient.Destroy;
var
  I: Integer;
begin
  FHttpPool.Lock;
  try
    for I := FHttpPool.Count - 1 downto 0 do
      TIdHttp(VarToObj(FHttpPool.Delete(I))).Free;
  finally
    FHttpPool.Unlock;
  end;
  FreeAndNil(FHeaders);
  FreeAndNil(FIdURI);
  inherited;
end;

procedure THproseHttpClient.InitURI(const AValue: string);

begin
  inherited InitURI(AValue);
  FreeAndNil(FIdURI);
  FIdURI := TIdURI.Create(URI);
end;

function THproseHttpClient.SendAndReceive(const Data: TBytes;
  const Context: TClientContext): TBytes;
var
  IdHttp: TIdHttp;
  Cookie: string;
  OutStream, InStream: TBytesStream;
begin
  FHttpPool.Lock;
  try
    if FHttpPool.Count > 0 then
      IdHttp := TIdHttp(VarToObj(FHttpPool.Delete(FHttpPool.Count - 1)))
    else
      IdHttp := TIdHttp.Create(nil);
  finally
    FHttpPool.Unlock;
  end;
  IdHttp.Request.UserAgent := FUserAgent;
  if FProxyHost <> '' then begin
    IdHttp.Request.ProxyServer := FProxyHost;
    IdHttp.Request.ProxyPort := FProxyPort;
    IdHttp.Request.ProxyUsername := FProxyUser;
    IdHttp.Request.ProxyPassword := FProxyPass;
  end;
  if KeepAlive then begin
    IdHttp.Request.Connection := 'keep-alive';
    FHeaders.Values['Keep-Alive'] := IntToStr(FKeepAliveTimeout);
  end
  else IdHttp.Request.Connection := 'close';
  if FUserName <> '' then begin
    IdHttp.Request.UserName := FUserName;
    IdHttp.Request.Password := FPassword;
  end;
  IdHttp.Request.ContentType := 'application/hprose';
  IdHttp.ProtocolVersion := pv1_1;
  IdHttp.Request.ExtraHeaders := FHeaders;
  Cookie := GetCookie(FIdURI.Host,
                      FIdURI.Path,
                      LowerCase(FIdURI.Protocol) = 'https');
  if Cookie <> '' then IdHttp.Request.ExtraHeaders.Values['Cookie'] := Cookie;
  OutStream := TBytesStream.Create(Data);
  InStream := TBytesStream.Create;
  try
    IdHttp.DoRequest(hmPost, URI, OutStream, InStream);
    SetCookie(IdHttp.Response.ExtraHeaders, FIdURI.Host);
    Result := InStream.Bytes;
    SetLength(Result, InStream.Size);
  finally
    OutStream.Free;
    InStream.Free;
  end;
  IdHttp.Request.Clear;
  IdHttp.Response.Clear;
  FHttpPool.Lock;
  try
    FHttpPool.Add(ObjToVar(IdHttp));
  finally
    FHttpPool.Unlock;
  end;
end;

procedure Register;
begin
  RegisterComponents('Hprose', [THproseHttpClient]);
end;

initialization
  CookieManager := TCaseInsensitiveHashMap.Create(False, True);

end.
