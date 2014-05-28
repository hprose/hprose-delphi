{
/**********************************************************\
|                                                          |
|                          hprose                          |
|                                                          |
| Official WebSite: http://www.hprose.com/                 |
|                   http://www.hprose.net/                 |
|                   http://www.hprose.org/                 |
|                                                          |
\**********************************************************/

/**********************************************************\
 *                                                        *
 * HproseIdHttpClient.pas                                 *
 *                                                        *
 * hprose indy http client unit for delphi.               *
 *                                                        *
 * LastModified: May 27, 2014                             *
 * Author: Ma Bingyao <andot@hprose.com>                  *
 *                                                        *
\**********************************************************/
}
unit HproseIdHttpClient;

interface

uses Classes, HproseCommon, HproseClient, SysUtils{$IFDEF FPC}, LResources{$ENDIF};

type

  THproseIdHttpClient = class(THproseClient)
  private
    FHttpPool: IList;
    FHeaders: TStringList;
    FProxyHost: string;
    FProxyPort: Integer;
    FProxyUser: string;
    FProxyPass: string;
    FUserAgent: string;
    FTimeout: Integer;
  protected
    function SendAndReceive(Data: TBytes): TBytes; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    {:Before HTTP operation you may define any non-standard headers for HTTP
     request, except of: 'Expect: 100-continue', 'Content-Length', 'Content-Type',
     'Connection', 'Authorization', 'Proxy-Authorization' and 'Host' headers.}
    property Headers: TStringList read FHeaders;

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

    property Timeout: Integer read FTimeout Write FTimeout;
  end;

procedure Register;

implementation

uses IdHttp, IdHeaderList, IdHTTPHeaderInfo, IdGlobalProtocols, IdCookieManager;

var
  CookieManager: TIdCookieManager = nil;

{ THproseIdHttpClient }

constructor THproseIdHttpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpPool := TArrayList.Create(10);
  FHeaders := TIdHeaderList.Create(TIdHeaderQuotingType.QuotePlain);
  FProxyHost := '';
  FProxyPort := 8080;
  FProxyUser := '';
  FProxyPass := '';
  FUserAgent := 'Hprose Http Client for Delphi (Indy10.5)';
  FTimeout := 30000;
end;

destructor THproseIdHttpClient.Destroy;
var
  I: Integer;
  IdHttp: TIdHttp;
begin
  FHttpPool.Lock;
  try
    for I := FHttpPool.Count - 1 downto 0 do
      IdHttp := TIdHttp(VarToObj(FHttpPool.Delete(I)));
      FreeAndNil(IdHttp);
  finally
    FHttpPool.Unlock;
  end;
  FreeAndNil(FHeaders);
  inherited;
end;

function THproseIdHttpClient.SendAndReceive(Data: TBytes): TBytes;
var
  IdHttp: TIdHttp;
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
  IdHttp.ReadTimeout := FTimeout;
  IdHttp.Request.CustomHeaders.Assign(FHeaders);
  IdHttp.Request.UserAgent := FUserAgent;
  if FProxyHost <> '' then begin
    IdHttp.ProxyParams.ProxyServer := FProxyHost;
    IdHttp.ProxyParams.ProxyPort := FProxyPort;
    IdHttp.ProxyParams.ProxyUsername := FProxyUser;
    IdHttp.ProxyParams.ProxyPassword := FProxyPass;
  end;
  IdHttp.Request.Connection := 'close';
  IdHttp.Request.ContentType := 'application/hprose';
  IdHttp.AllowCookies := True;
  IdHttp.CookieManager := CookieManager;
  IdHttp.HTTPOptions := IdHttp.HTTPOptions + [hoKeepOrigProtocol];
  IdHttp.ProtocolVersion := pv1_1;
  OutStream := TBytesStream.Create(Data);
  InStream := TBytesStream.Create;
  try
    IdHttp.Post(FUri, OutStream, InStream);
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
  RegisterComponents('Hprose', [THproseIdHttpClient]);
end;

initialization
  CookieManager := TIdCookieManager.Create(nil);
{$IFDEF FPC}
  {$I HproseIndy.lrs}
{$ENDIF}
finalization
  FreeAndNil(CookieManager);
end.

