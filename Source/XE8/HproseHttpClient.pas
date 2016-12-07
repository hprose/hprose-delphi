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
 * hprose http client unit for delphi.                    *
 *                                                        *
 * LastModified: Dec 7, 2016                              *
 * Author: Ma Bingyao <andot@hprose.com>                  *
 *                                                        *
\**********************************************************/
}
unit HproseHttpClient;

interface

uses Classes, HproseCommon, HproseClient, IdHeaderList, SysUtils, System.Net.HttpClient, System.Net.URLClient;

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

var
  CookieManager: TCookieManager = nil;

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
  FUserAgent := 'Hprose Http Client for Delphi XE8+';
  FConnectionTimeout := 10000;
end;

destructor THproseHttpClient.Destroy;
var
  I: Integer;
  HttpClient: THTTPClient;
begin
  FHttpPool.Lock;
  try
    for I := FHttpPool.Count - 1 downto 0 do begin
      HttpClient := THTTPClient(VarToObj(FHttpPool.Delete(I)));
      FreeAndNil(HttpClient);
    end;
  finally
    FHttpPool.Unlock;
  end;
  inherited;
end;

procedure ValidateServerCertificateCallback(
  const Sender: TObject;
  const ARequest: TURLRequest;
  const Certificate: TCertificate;
  var Accepted: Boolean);
begin
  Accepted := True;
end;

function THproseHttpClient.SendAndReceive(const Data: TBytes;
  const Context: TClientContext): TBytes;
var
  HttpClient: THTTPClient;
  ContentStream: TBytesStream;
  Header, HttpHeader: IMap;
  Request: IHTTPRequest;
  ResponseHeader: TNetHeaders;
  I: Integer;
begin
  FHttpPool.Lock;
  try
    if FHttpPool.Count > 0 then
      HttpClient := THttpClient(VarToObj(FHttpPool.Delete(FHttpPool.Count - 1)))
    else begin
      HttpClient := THttpClient.Create();
      HttpClient.AllowCookies := True;
      HttpClient.CookieManager := CookieManager;
      HttpClient.HandleRedirects := True;
      HttpClient.ValidateServerCertificateCallback := ValidateServerCertificateCallback;
      HttpClient.ContentType := 'application/hprose';
    end;
  finally
    FHttpPool.Unlock;
  end;
  HttpClient.ConnectionTimeout := FConnectionTimeout;
  HttpClient.ResponseTimeout := Context.Settings.Timeout;
  if FProxyHost <> '' then begin
    HttpClient.ProxySettings := TProxySettings.Create(FProxyHost, FProxyPort, FProxyUser, FProxyPass);
  end;
  Request := HttpClient.GetRequest('POST', URI);
  Header := TCaseInsensitiveHashMap.Create;
  Header.PutAll(FHeaders);
  HttpHeader := VarToMap(Context['httpHeader']);
  if (Assigned(HttpHeader)) then
    Header.PutAll(HttpHeader);
  for I := 0 to Header.Count - 1 do
    Request.AddHeader(Header.Keys[I], Header.Values[I]);
  Request.UserAgent := FUserAgent;
  if KeepAlive then begin
    Request.HeaderValue['Connection'] := 'keep-alive';
    Request.HeaderValue['Keep-Alive'] := IntToStr(FKeepAliveTimeout);
  end
  else begin
    Request.HeaderValue['Connection'] := 'close';
  end;
  if FUserName <> '' then begin
    Request.SetCredential(FUserName, FPassword);
  end;
  Request.SourceStream := TBytesStream.Create(Data);
  Request.SourceStream.Position := 0;
  ContentStream := TBytesStream.Create;
  try
    ResponseHeader := HttpClient.Execute(Request, ContentStream).GetHeaders();
    HttpHeader.Clear();
    for I := 0 to Length(ResponseHeader) - 1 do begin
      HttpHeader.Put(ResponseHeader[I].Name, ResponseHeader[I].Value);
    end;
    Context['httpHeader'] := HttpHeader;
    Result := ContentStream.Bytes;
    SetLength(Result, ContentStream.Size);
  finally
    Request.SourceStream.Free;
    Request.SourceStream := nil;
    ContentStream.Free;
  end;
  FHttpPool.Lock;
  try
    FHttpPool.Add(ObjToVar(HttpClient));
  finally
    FHttpPool.Unlock;
  end;
end;

procedure Register;
begin
  RegisterComponents('Hprose', [THproseHttpClient]);
end;

initialization
  CookieManager := TCookieManager.Create();

finalization
  FreeAndNil(CookieManager);
end.
