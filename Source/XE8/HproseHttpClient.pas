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
 * LastModified: Nov 30, 2016                             *
 * Author: Ma Bingyao <andot@hprose.com>                  *
 *                                                        *
\**********************************************************/
}
unit HproseHttpClient;

interface

uses Classes, HproseCommon, HproseClient, IdHeaderList, SysUtils, System.Net.HttpClient, System.Net.URLClient;

type
  THeaderList = class
  private
    FHeaders: TNetHeaders;
    function GetValue(const Name: string): string;
    procedure SetValue(const Name, Value: string);
  public
    constructor Create;
    property Values[const Name: string]: string read GetValue write SetValue;
    property Headers: TNetHeaders read FHeaders;
  end;

  THproseHttpClient = class(THproseClient)
  private
    FHttpPool: IList;
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

const

  Base64EncodeChars: array[0..63] of Char = (
    'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
    'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
    'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
    'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
    'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
    'w', 'x', 'y', 'z', '0', '1', '2', '3',
    '4', '5', '6', '7', '8', '9', '+', '/');

function Base64Encode(const Data: TBytes): string;
var
  R, Len, I, J, L : Longint;
  C : LongWord;
begin
  Result := '';
  Len := Length(Data);
  if Len = 0 then Exit;
  R := Len mod 3;
  Dec(Len, R);
  L := (Len div 3) * 4;
  if (R > 0) then Inc(L, 4);
  SetLength(Result, L);
  I := 0;
  J := 1;
  while (I < Len) do begin
    C := Data[I];
    Inc(I);
    C := (C shl 8) or Data[I];
    Inc(I);
    C := (C shl 8) or Data[I];
    Inc(I);
    Result[J] := Base64EncodeChars[C shr 18];
    Inc(J);
    Result[J] := Base64EncodeChars[(C shr 12) and $3F];
    Inc(J);
    Result[J] := Base64EncodeChars[(C shr 6) and $3F];
    Inc(J);
    Result[J] := Base64EncodeChars[C and $3F];
    Inc(J);
  end;
  if (R = 1) then begin
    C := Data[I];
    Result[J] := Base64EncodeChars[C shr 2];
    Inc(J);
    Result[J] := Base64EncodeChars[(C and $03) shl 4];
    Inc(J);
    Result[J] := '=';
    Inc(J);
    Result[J] := '=';
  end
  else if (R = 2) then begin
    C := Data[I];
    Inc(I);
    C := (C shl 8) or Data[I];
    Result[J] := Base64EncodeChars[C shr 10];
    Inc(J);
    Result[J] := Base64EncodeChars[(C shr 4) and $3F];
    Inc(J);
    Result[J] := Base64EncodeChars[(C and $0F) shl 2];
    Inc(J);
    Result[J] := '=';
  end;
end;

{ THeaderList }

constructor THeaderList.Create;
begin
  inherited Create;
  FHeaders := nil;
end;

function THeaderList.GetValue(const Name: string): string;
var
  I: Integer;
  Header: TNetHeader;
begin
  Result := '';
  for I := Length(FHeaders) - 1 downto 0 do begin
    Header := FHeaders[I];
    if Header.Name = Name then begin
      Result := Header.Value;
      break
    end;
  end;
end;

procedure THeaderList.SetValue(const Name, Value: string);
var
  I, J, N: Integer;
  Header: TNetHeader;
begin
  N := Length(FHeaders) - 1;
  for I := N downto 0 do begin
    Header := FHeaders[I];
    if Header.Name = Name then begin
      if Value <> '' then
        Header.Value := Value
      else begin
        for J := N downto I + 1 do begin
          FHeaders[J - 1] := FHeaders[J];
        end;
        SetLength(FHeaders, N);
      end;
      exit;
    end;
  end;
  if Value <> '' then begin
    SetLength(FHeaders, N + 2);
    FHeaders[N + 1] := TNetHeader.Create(Name, Value);
  end;
end;

var
  CookieManager: TCookieManager = nil;

{ THproseHttpClient }

constructor THproseHttpClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHttpPool := TArrayList.Create(10);
  FHeaders := THeaderList.Create;
  FUserName := '';
  FPassword := '';
  FKeepAlive := True;
  FKeepAliveTimeout := 300;
  FProxyHost := '';
  FProxyPort := 8080;
  FProxyUser := '';
  FProxyPass := '';
  FUserAgent := 'Hprose Http Client for Delphi';
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
  FreeAndNil(FHeaders);
  inherited;
end;

function THproseHttpClient.SendAndReceive(const Data: TBytes;
  const Context: TClientContext): TBytes;
var
  HttpClient: THTTPClient;
  OutStream, InStream: TBytesStream;
begin
  FHttpPool.Lock;
  try
    if FHttpPool.Count > 0 then
      HttpClient := THttpClient(VarToObj(FHttpPool.Delete(FHttpPool.Count - 1)))
    else
      HttpClient := THttpClient.Create();
  finally
    FHttpPool.Unlock;
  end;
  HttpClient.ConnectionTimeout := FConnectionTimeout;
  HttpClient.ResponseTimeout := Context.Settings.Timeout;
  HttpClient.UserAgent := FUserAgent;
  HttpClient.HandleRedirects := True;
  if FProxyHost <> '' then begin
    HttpClient.ProxySettings := TProxySettings.Create(FProxyHost, FProxyPort, FProxyUser, FProxyPass);
  end;
  if KeepAlive then begin
    HttpClient.CustomHeaders['Connection'] := 'keep-alive';
    FHeaders.Values['Keep-Alive'] := IntToStr(FKeepAliveTimeout);
  end
  else begin
    HttpClient.CustomHeaders['Connection'] := 'close';
    FHeaders.Values['Keep-Alive'] := '';
  end;
  if FUserName <> '' then begin
    FHeaders.Values['Authorization'] := Base64Encode(BytesOf(FUserName + ':' + FPassword));
  end;
  HttpClient.ContentType := 'application/hprose';
  HttpClient.AllowCookies := True;
  HttpClient.CookieManager := CookieManager;
  OutStream := TBytesStream.Create(Data);
  InStream := TBytesStream.Create;
  try
    HttpClient.Post(URI, OutStream, InStream, FHeaders.Headers);
    Result := InStream.Bytes;
    SetLength(Result, InStream.Size);
  finally
    OutStream.Free;
    InStream.Free;
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
