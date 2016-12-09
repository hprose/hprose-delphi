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
 * LastModified: Dec 9, 2016                              *
 * Author: Ma Bingyao <andot@hprose.com>                  *
 *                                                        *
\**********************************************************/
}
unit HproseHttpClient;

interface

uses Classes, HproseCommon, HproseClient, IdHeaderList, SysUtils{$IFDEF FPC}, LResources{$ENDIF};

type

  THproseHttpClient = class(THproseClient)
  private
    FHttpPool: IList;
    FUserName: string;
    FPassword: string;
    FHeaders: IMap;
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
  published
    {:Before HTTP operation you may define any non-standard headers for HTTP
     request, except of: 'Expect: 100-continue', 'Content-Length', 'Content-Type',
     'Connection', 'Authorization', 'Proxy-Authorization' and 'Host' headers.}
    property Headers: IMap read FHeaders;

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
     used: 'Hprose Http Client for Delphi (Indy10)'}
    property UserAgent: string read FUserAgent Write FUserAgent;

    {:UserName for user authorization.}
    property UserName: string read FUserName write FUserName;

    {:Password for user authorization.}
    property Password: string read FPassword write FPassword;

    {:Define timeout for ConnectionTimeout in milliseconds! Default value is 10000.}
    property ConnectionTimeout: Integer read FConnectionTimeout write FConnectionTimeout;
  end;

procedure Register;

implementation

uses IdHttp, IdGlobalProtocols, IdCookieManager;

var
  CookieManager: TIdCookieManager = nil;

{ THproseHttpClient }

constructor THproseHttpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpPool := TArrayList.Create(10);
  FHeaders := TCaseInsensitiveHashMap.Create;
  FUserName := '';
  FPassword := '';
  FKeepAlive := True;
  FKeepAliveTimeout := 300;
  FProxyHost := '';
  FProxyPort := 8080;
  FProxyUser := '';
  FProxyPass := '';
  FUserAgent := 'Hprose Http Client for Delphi (Indy10)';
  FConnectionTimeout := 10000;
end;

destructor THproseHttpClient.Destroy;
var
  I: Integer;
  IdHttp: TIdHttp;
begin
  FHttpPool.Lock;
  try
    for I := FHttpPool.Count - 1 downto 0 do begin
      IdHttp := TIdHttp(VarToObj(FHttpPool.Delete(I)));
      FreeAndNil(IdHttp);
    end;
  finally
    FHttpPool.Unlock;
  end;
  inherited;
end;

function THproseHttpClient.SendAndReceive(const Data: TBytes;
  const Context: TClientContext): TBytes;
var
  IdHttp: TIdHttp;
  OutStream, InStream: TBytesStream;
  Header, HttpHeader: IMap;
  CustomHeaders, RawHeaders: TIdHeaderList;
  I: Integer;
  Key: string;
begin
  FHttpPool.Lock;
  try
    if FHttpPool.Count > 0 then
      IdHttp := TIdHttp(VarToObj(FHttpPool.Delete(FHttpPool.Count - 1)))
    else begin
      IdHttp := TIdHttp.Create(nil);
      IdHttp.AllowCookies := True;
      IdHttp.CookieManager := CookieManager;
      IdHttp.HTTPOptions := IdHttp.HTTPOptions + [hoKeepOrigProtocol];
      IdHttp.ProtocolVersion := pv1_1;
    end;
  finally
    FHttpPool.Unlock;
  end;
  IdHttp.ConnectTimeout := FConnectionTimeout;
  IdHttp.ReadTimeout := Context.Settings.Timeout;
  IdHttp.Request.UserAgent := FUserAgent;
  if FProxyHost <> '' then begin
    IdHttp.ProxyParams.ProxyServer := FProxyHost;
    IdHttp.ProxyParams.ProxyPort := FProxyPort;
    IdHttp.ProxyParams.ProxyUsername := FProxyUser;
    IdHttp.ProxyParams.ProxyPassword := FProxyPass;
  end;
  CustomHeaders := IdHttp.Request.CustomHeaders;
  if KeepAlive then begin
    IdHttp.Request.Connection := 'keep-alive';
    CustomHeaders.Values['Keep-Alive'] := IntToStr(FKeepAliveTimeout);
  end
  else IdHttp.Request.Connection := 'close';
  if FUserName <> '' then begin
    IdHttp.Request.BasicAuthentication := True;
    IdHttp.Request.UserName := FUserName;
    IdHttp.Request.Password := FPassword;
  end;
  IdHttp.Request.ContentType := 'application/hprose';
  Header := TCaseInsensitiveHashMap.Create;
  Header.PutAll(FHeaders);
  HttpHeader := VarToMap(Context['httpHeader']);
  if (Assigned(HttpHeader)) then
    Header.PutAll(HttpHeader)
  else
    HttpHeader := TCaseInsensitiveHashMap.Create;
  for I := 0 to Header.Count - 1 do
    CustomHeaders.Values[Header.Keys[I]] := Header.Values[I];
  OutStream := TBytesStream.Create(Data);
  InStream := TBytesStream.Create;
  try
    IdHttp.Post(URI, OutStream, InStream);
    HttpHeader.Clear();
    RawHeaders := IdHttp.Response.RawHeaders;
    for I := 0 to RawHeaders.Count - 1 do begin
      Key := RawHeaders.Names[I];
      HttpHeader.Put(Key, RawHeaders.Values[Key]);
    end;
    Context['httpHeader'] := HttpHeader;
    Result := InStream.Bytes;
    SetLength(Result, InStream.Size);
  finally
    OutStream.Free;
    InStream.Free;
  end;
  IdHttp.Request.Clear;
  IdHttp.Request.CustomHeaders.Clear;
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
  CookieManager := TIdCookieManager.Create(nil);
{$IFDEF FPC}
  {$I Hprose.lrs}
{$ENDIF}
finalization
  FreeAndNil(CookieManager);
end.
