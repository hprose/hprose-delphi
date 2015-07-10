unit HproseSynaHTTPServer;

{$I Hprose.inc}

interface

uses
  Classes, SysUtils, HproseServer, HproseIO, SynaHTTPServer, CfThreadPool, blcksock;

type

  THproseSynaHTTPServer = class(THproseServer)
  private
    FServer: TSynaHTTPServer;
    FCrossDomain: Boolean;
    FLastModified: string;
    FETag: string;
    FCrossDomainXmlFile: string;
    FCrossDomainXmlContent: string;
    FClientAccessPolicyXmlFile: string;
    FClientAccessPolicyXmlContent: string;
    function CrossDomainXmlHandler(RequestInfo: TRequestInfo; ResponseInfo: TResponseInfo): Boolean;
    function ClientAccessPolicyXmlHandler(RequestInfo: TRequestInfo; ResponseInfo: TResponseInfo): Boolean;
    procedure SetCrossDomainXmlFile(const Value: string);
    procedure SetCrossDomainXmlContent(const Value: string);
    procedure SetClientAccessPolicyXmlFile(const Value: string);
    procedure SetClientAccessPolicyXmlContent(const Value: string);
    function  GetDefaultPort: Word;
    procedure SetDefaultPort(const Value: Word);
    function  GetThreadPool: TCfThreadPool;
  protected
    procedure HandleCommand(RequestInfo: TRequestInfo; ResponseInfo: TResponseInfo); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start;
    procedure Stop;
  published
    property CrossDomainEnabled: Boolean read FCrossDomain write FCrossDomain default False;
    property CrossDomainXmlFile: string read FCrossDomainXmlFile write SetCrossDomainXmlFile;
    property CrossDomainXmlContent: string read FCrossDomainXmlContent write SetCrossDomainXmlContent;
    property ClientAccessPolicyXmlFile: string read FClientAccessPolicyXmlFile write SetClientAccessPolicyXmlFile;
    property ClientAccessPolicyXmlContent: string read FClientAccessPolicyXmlContent write SetClientAccessPolicyXmlContent;
    property DefaultPort: Word read GetDefaultPort write SetDefaultPort;
    property ThreadPool: TCfThreadPool read GetThreadPool;
  end;

implementation

{ THproseSynaHTTPServer }

constructor THproseSynaHTTPServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FServer := TSynaHTTPServer.Create(Self);
  FServer.DefaultPort := 80;
  FServer.OnCommandGet := Self.HandleCommand;
  FServer.OnCommandOther := Self.HandleCommand;
end;

function THproseSynaHTTPServer.ClientAccessPolicyXmlHandler(
  RequestInfo: TRequestInfo; ResponseInfo: TResponseInfo): Boolean;
var
  Data: AnsiString;
begin
  Result := False;
  if LowerCase(RequestInfo.RawCommand) = 'get /clientaccesspolicy.xml' then
  begin
    if (RequestInfo.RawHeaders.Values['If-Modified-Since'] = FLastModified) and
       (RequestInfo.RawHeaders.Values['If-None-Match'] = FETag)then
        ResponseInfo.ResponseNo := 304
    else
    begin
      ResponseInfo.CustomHeaders.Add('Last-Modified: ' + FLastModified);
      ResponseInfo.CustomHeaders.Add('Etag: ' + FETag);
      ResponseInfo.ContentType := 'text/xml';
      Data := AnsiString(FClientAccessPolicyXmlContent);
      ResponseInfo.ContentStream.Write(Data[1], Length(Data));
      Result := True;
    end;
  end;
end;

function THproseSynaHTTPServer.CrossDomainXmlHandler(RequestInfo: TRequestInfo;
  ResponseInfo: TResponseInfo): Boolean;
var
  Data: AnsiString;
begin
  Result := False;
  if LowerCase(RequestInfo.RawCommand) = 'get /crossdomain.xml' then
  begin
    if (RequestInfo.RawHeaders.Values['If-Modified-Since'] = FLastModified) and
       (RequestInfo.RawHeaders.Values['If-None-Match'] = FETag) then
        ResponseInfo.ResponseNo := 304
    else
    begin
      ResponseInfo.CustomHeaders.Add('Last-Modified: ' + FLastModified);
      ResponseInfo.CustomHeaders.Add('Etag: ' + FETag);
      ResponseInfo.ContentType := 'text/xml';
      Data := AnsiString(FCrossDomainXmlContent);
      ResponseInfo.ContentStream.Write(Data[1], Length(Data));
      Result := True;
    end;
  end;
end;

procedure THproseSynaHTTPServer.HandleCommand(RequestInfo: TRequestInfo; ResponseInfo: TResponseInfo);
begin
  if (FClientAccessPolicyXmlContent <> '') and ClientAccessPolicyXmlHandler(RequestInfo, ResponseInfo) then
    Exit;
  if (FCrossDomainXmlContent <> '') and CrossDomainXmlHandler(RequestInfo, ResponseInfo) then
    Exit;
  if RequestInfo.CommandType = hcGET then
    DoFunctionList(ResponseInfo.ContentStream)
  else if RequestInfo.CommandType = hcPost then
  begin
    inherited HandleCommand(RequestInfo.ContentStream, ResponseInfo.ContentStream);
  end;
end;

procedure THproseSynaHTTPServer.SetClientAccessPolicyXmlContent(const Value: string);
begin
  FClientAccessPolicyXmlFile := '';
  FClientAccessPolicyXmlContent := Value;
end;

procedure THproseSynaHTTPServer.SetClientAccessPolicyXmlFile(const Value: string);
var
  Data: AnsiString;
begin
  FClientAccessPolicyXmlFile := Value;
  with TFileStream.Create(Value, fmOpenRead) do
  begin
    SetLength(Data, Size);
    ReadBuffer(Data[1], Size);
    Free;
  end;
  FClientAccessPolicyXmlContent := string(Data);
end;

procedure THproseSynaHTTPServer.SetCrossDomainXmlContent(const Value: string);
begin
  FCrossDomainXmlFile := '';
  FCrossDomainXmlContent := Value;
end;

procedure THproseSynaHTTPServer.SetCrossDomainXmlFile(const Value: string);
var
  Data: AnsiString;
begin
  FCrossDomainXmlFile := Value;
  with TFileStream.Create(Value, fmOpenRead) do
  begin
    SetLength(Data, Size);
    ReadBuffer(Data[1], Size);
    Free;
  end;
  FCrossDomainXmlContent := string(Data);
end;

function THproseSynaHTTPServer.GetDefaultPort: Word;
begin
  Result := FServer.DefaultPort;
end;

procedure THproseSynaHTTPServer.SetDefaultPort(const Value: Word);
begin
  FServer.DefaultPort := Value;
end;

function THproseSynaHTTPServer.GetThreadPool: TCfThreadPool;
begin
  Result := FServer.ThreadPool;
end;

procedure THproseSynaHTTPServer.Start;
begin
  if FServer.Active then Exit;
  FLastModified := DateTimeGMTToHttpStr(Now);
  Randomize;
  FETag := Format('%x:%d', [Random(MaxInt), Random(MaxInt)]);
  FServer.Active := True;
end;

procedure THproseSynaHTTPServer.Stop;
begin
  FServer.Active := False;
end;

end.
