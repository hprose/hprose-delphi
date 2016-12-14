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
 * HproseClient.pas                                       *
 *                                                        *
 * hprose client unit for delphi.                         *
 *                                                        *
 * LastModified: Dec 14, 2016                             *
 * Author: Ma Bingyao <andot@hprose.com>                  *
 *                                                        *
\**********************************************************/
}
unit HproseClient;

{$I Hprose.inc}

interface

uses HproseCommon, Classes, SysUtils, TypInfo, Variants;

type

  THproseClient = class;

{$IFDEF SUPPORTS_ANONYMOUS_METHOD}
  THproseCallback = reference to procedure(Result: Variant; var Args: TVariants);
  THproseCallback1 = reference to procedure(Result: Variant);
  TOnError = reference to procedure(const Name: string; const Error: Exception);
  TOnFailswitch = reference to procedure(const Client: THproseClient);
  THproseCallback<T> = reference to procedure(Result: T; var Args: TVariants);
  THproseCallback1<T> = reference to procedure(Result: T);
{$ELSE}
  THproseCallback = procedure(Result: Variant; var Args: TVariants) of object;
  THproseCallback1 = procedure(Result: Variant) of object;
  TOnError = procedure(const Name: string; const Error: Exception) of object;
  TOnFailswitch = procedure(const Client: THproseClient) of object;
{$ENDIF}

  { IInvokeSettings }

  IInvokeSettings = interface
    ['{3C665705-2F2D-42F1-ADC5-266967C6A257}']
    function Get(Key: string): Variant;
    function GetByRef: Boolean;
    function GetFailswitch: Boolean;
    function GetIdempotent: Boolean;
    function GetMode: TResultMode;
    function GetOneway: Boolean;
    function GetResultType: PTypeInfo;
    function GetRetry: Integer;
    function GetSimple: Boolean;
    function GetTimeout: Integer;
    function GetOnError: TOnError;
    function GetUserData: ICaseInsensitiveHashMap;
    procedure Put(Key: string; const AValue: Variant);
    procedure SetByRef(AValue: Boolean);
    procedure SetFailswitch(AValue: Boolean);
    procedure SetIdempotent(AValue: Boolean);
    procedure SetMode(AValue: TResultMode);
    procedure SetOneway(AValue: Boolean);
    procedure SetResultType(AValue: PTypeInfo);
    procedure SetRetry(AValue: Integer);
    procedure SetSimple(AValue: Boolean);
    procedure SetTimeout(AValue: Integer);
    procedure SetOnError(AValue: TOnError);
    procedure SetUserData(AValue: ICaseInsensitiveHashMap);
    property ByRef: Boolean read GetByRef write SetByRef;
    property Simple: Boolean read GetSimple write SetSimple;
    property Idempotent: Boolean read GetIdempotent write SetIdempotent;
    property Failswitch: Boolean read GetFailswitch write SetFailswitch;
    property Oneway: Boolean read GetOneway write SetOneway;
    property Retry: Integer read GetRetry write SetRetry;
    property Timeout: Integer read GetTimeout write SetTimeout;
    property Mode: TResultMode read GetMode write SetMode;
    property ResultType: PTypeInfo read GetResultType write SetResultType;
    property OnError: TOnError read GetOnError write SetOnError;
    property UserData: ICaseInsensitiveHashMap read GetUserData write SetUserData;
    property Item[Key: string]: Variant read Get write Put; default;
  end;

  { TInvokeSettings }

  TInvokeSettings = class(TInterfacedObject, IInvokeSettings)
  private
    FByRef: Boolean;
    FSimple: Boolean;
    FIdempotent: Boolean;
    FFailswitch: Boolean;
    FOneway: Boolean;
    FRetry: Integer;
    FTimeout: Integer;
    FMode: TResultMode;
    FResultType: PTypeInfo;
    FOnError: TOnError;
    FUserData: ICaseInsensitiveHashMap;
    function Get(Key: string): Variant;
    function GetByRef: Boolean;
    function GetFailswitch: Boolean;
    function GetIdempotent: Boolean;
    function GetMode: TResultMode;
    function GetOneway: Boolean;
    function GetResultType: PTypeInfo;
    function GetRetry: Integer;
    function GetSimple: Boolean;
    function GetTimeout: Integer;
    function GetOnError: TOnError;
    function GetUserData: ICaseInsensitiveHashMap;
    procedure Put(Key: string; const Value: Variant);
    procedure SetByRef(AValue: Boolean);
    procedure SetFailswitch(AValue: Boolean);
    procedure SetIdempotent(AValue: Boolean);
    procedure SetMode(AValue: TResultMode);
    procedure SetOneway(AValue: Boolean);
    procedure SetResultType(AValue: PTypeInfo);
    procedure SetRetry(AValue: Integer);
    procedure SetSimple(AValue: Boolean);
    procedure SetTimeout(AValue: Integer);
    procedure SetOnError(AValue: TOnError);
    procedure SetUserData(AValue: ICaseInsensitiveHashMap);
  public
    constructor Create(); overload;
    constructor Create(const AOnError: TOnError); overload;
    constructor Create(const ASettings: array of const); overload;
    constructor Create(const AOnError: TOnError;
      const ASettings: array of const); overload;
    property ByRef: Boolean read GetByRef write SetByRef;
    property Simple: Boolean read GetSimple write SetSimple;
    property Idempotent: Boolean read GetIdempotent write SetIdempotent;
    property Failswitch: Boolean read GetFailswitch write SetFailswitch;
    property Oneway: Boolean read GetOneway write SetOneway;
    property Retry: Integer read GetRetry write SetRetry;
    property Timeout: Integer read GetTimeout write SetTimeout;
    property Mode: TResultMode read GetMode write SetMode;
    property ResultType: PTypeInfo read GetResultType write SetResultType;
    property OnError: TOnError read GetOnError write SetOnError;
    property UserData: ICaseInsensitiveHashMap read GetUserData write SetUserData;
    property Item[Key: string]: Variant read Get write Put; default;
  end;

  { TClientContext }

  TClientContext = class(THproseContext)
  private
    FClient: THproseClient;
    FSettings: IInvokeSettings;
    FRetried: Integer;
  public
    constructor Create(const AClient: THproseClient;
      const ASettings: IInvokeSettings);
    property Client: THproseClient read FClient;
    property Settings: IInvokeSettings read FSettings;
    property Retried: Integer read FRetried write FRetried;
  end;

  { TClientTopic }

  TClientTopic = class(TThread)
  private
    FClient: THproseClient;
    FName: string;
    FId: string;
    FCallback: THproseCallback1;
    FSettings: IInvokeSettings;
  protected
    procedure Execute; override;
  public
    constructor Create(Client: THproseClient; const AName, AId: string;
      Callback: THproseCallback1; const ASettings: IInvokeSettings);
  end;

  { TTopicManager }

  TTopicManager = class
  private
    FAllTopics: IMap;
    function GetTopic(const Topic, Id: string): TClientTopic;
    procedure CreateTopic(const Topic: string);
    procedure DeleteTopic(const Topic, Id: string);
  public
    constructor Create;
    function IsSubscribed(const Topic: string): Boolean;
    function SubscribedList(): TStringArray;
  end;

  { THproseClient }

  THproseClient = class(TComponent, IInvokeableVarObject)
  private
    FNameSpace: string;
    FOnError: TOnError;
    FOnFailswitch: TOnFailswitch;
    FFilters: TFilterList;
    FHandlers: THandlerManager;
    FURI: string;
    FURIList: TStringArray;
    FIndex: Integer;
    FFailround: Integer;
    FRetry: Integer;
    FTimeout: Integer;
    FUserData: ICaseInsensitiveHashMap;
    FId: string;
    FTopicManager: TTopicManager;
    function InvokeHandler(const AName: String;
                          var Args: TVariants;
                      const Context: THproseContext;
                     Next: PNextInvokeHandler): Variant;
    function BeforeFilterHandler(var Request: TBytes;
                             const Context: THproseContext;
                            Next: PNextFilterHandler): TBytes;
    function AfterFilterHandler(var Request: TBytes;
                            const Context: THproseContext;
                          Next: PNextFilterHandler): TBytes;
    function RetrySendRequest(var Request: TBytes;
      const Context: TClientContext): TBytes;
    procedure FailSwitch;
    function Encode(const AName: string; var Args: TVariants;
      const Context: TClientContext): TBytes;
    function Decode(var Data: TBytes; var Args: TVariants;
      const Context: TClientContext): Variant;
    function GetFullName(const AName: string): string;
    procedure SetURI(const AValue: string);
    //procedure ByValue(var Arguments: TVariants);
{$IFDEF SUPPORTS_ANONYMOUS_METHOD}
    procedure VarToT(Info: PTypeInfo; const Src: Variant; out Dst);
    function VarTo<T>(const AValue: Variant): T;
{$ENDIF}
  protected
    procedure InitURI(const AValue: string); virtual;
    function SendAndReceive(const Data: TBytes;
      const Context: TClientContext): TBytes; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class function New(const AURI: string; const ANameSpace: string = ''): Variant; overload;
    class function New(const AURIList: array of string; const ANameSpace: string = ''): Variant; overload;
    function UseService(const AURI: string = ''; const ANameSpace: string = ''): Variant; overload;
    function UseService(const AURIList: array of string; const ANameSpace: string = ''): Variant; overload;
    procedure SetURIList(const AURIList: array of string);
    function AddFilter(const Filter: IFilter): THproseClient;
    function RemoveFilter(const Filter: IFilter): THproseClient;
    function AddInvokeHandler(const Handler: TInvokeHandler): THproseClient;
    function AddBeforeFilterHandler(const Handler: TFilterHandler): THproseClient;
    function AddAfterFilterHandler(const Handler: TFilterHandler): THproseClient;
    // Synchronous invoke
    function Invoke(const AName: string;
      const ASettings: IInvokeSettings = nil): Variant; overload;
    function Invoke(const AName: string; const Args: array of const;
      const ASettings: IInvokeSettings = nil): Variant; overload;
    function Invoke(const AName: string; var Args: TVariants;
      const ASettings: IInvokeSettings = nil): Variant; overload;
    // Asynchronous invoke
    procedure Invoke(const AName: string;
      Callback: THproseCallback1;
      const ASettings: IInvokeSettings = nil); overload;
    procedure Invoke(const AName: string; const Args: array of const;
      Callback: THproseCallback1;
      const ASettings: IInvokeSettings = nil); overload;
    procedure Invoke(const AName: string; const Args: array of const;
      Callback: THproseCallback;
      const ASettings: IInvokeSettings = nil); overload;
    procedure Invoke(const AName: string; var Args: TVariants;
      Callback: THproseCallback1;
      const ASettings: IInvokeSettings = nil); overload;
    procedure Invoke(const AName: string; var Args: TVariants;
      Callback: THproseCallback;
      const ASettings: IInvokeSettings = nil); overload;
    // IInvokeableVarObject method
    function Invoke(const AName: string;
      const Arguments: TVarDataArray): Variant; overload;
{$IFDEF SUPPORTS_ANONYMOUS_METHOD}
    // Synchronous invoke
    function Invoke<T>(const AName: string;
      const ASettings: IInvokeSettings = nil): T; overload;
    function Invoke<T>(const AName: string; const Args: array of const;
      const ASettings: IInvokeSettings = nil): T; overload;
    function Invoke<T>(const AName: string; var Args: TVariants;
      const ASettings: IInvokeSettings = nil): T; overload;
    // Asynchronous invoke
    procedure Invoke<T>(const AName: string;
      Callback: THproseCallback1<T>;
      const ASettings: IInvokeSettings = nil); overload;
    procedure Invoke<T>(const AName: string; const Args: array of const;
      Callback: THproseCallback1<T>;
      const ASettings: IInvokeSettings = nil); overload;
    procedure Invoke<T>(const AName: string; const Args: array of const;
      Callback: THproseCallback<T>;
      const ASettings: IInvokeSettings = nil); overload;
    procedure Invoke<T>(const AName: string; var Args: TVariants;
      Callback: THproseCallback1<T>;
      const ASettings: IInvokeSettings = nil); overload;
    procedure Invoke<T>(const AName: string; var Args: TVariants;
      Callback: THproseCallback<T>;
      const ASettings: IInvokeSettings = nil); overload;
{$ENDIF}
    function AutoId: string;
    procedure Subscribe(const AName: string; AId: string;
      Callback: THproseCallback1; ASettings: IInvokeSettings = nil); overload;
    procedure Subscribe(const AName: string;
      Callback: THproseCallback1; ASettings: IInvokeSettings = nil); overload;
{$IFDEF SUPPORTS_ANONYMOUS_METHOD}
    procedure Subscribe<T>(const AName: string; AId: string;
      Callback: THproseCallback1<T>; ASettings: IInvokeSettings = nil); overload;
    procedure Subscribe<T>(const AName: string;
      Callback: THproseCallback1<T>; ASettings: IInvokeSettings = nil); overload;
{$ENDIF}
    procedure Unsubscribe(const AName: string; AId: string = '');
    function IsSubscribed(const Topic: string): Boolean;
    function SubscribedList(): TStringArray;
  published
    property URI: string read FURI write SetURI;
    property URIList: TStringArray read FURIList;
    property Failround: Integer read FFailround;
    property Retry: Integer read FRetry write FRetry;
    property Timeout: Integer read FTimeout write FTimeout;
    property Filters: TFilterList read FFilters;
    property NameSpace: string read FNameSpace write FNameSpace;
    property UserData: ICaseInsensitiveHashMap read FUserData write FUserData;
    property Id: string read FId;
    property OnFailswitch: TOnFailswitch read FOnFailswitch write FOnFailswitch;
    // This event OnError only for asynchronous invoke
    property OnError: TOnError read FOnError write FOnError;
  end;

{$IFDEF SUPPORTS_ANONYMOUS_METHOD}
// The following classes are private classes, but they can't be moved to the
// implementation section because of E2506.

  TAsyncInvokeThread<T> = class(TThread)
  protected
    FClient: THproseClient;
    FName: string;
    FArgs: TVariants;
    FSettings: IInvokeSettings;
    FResult: T;
    FError: Exception;
    procedure Execute; override;
    procedure DoCallback; virtual; abstract;
    procedure DoError;
  end;

  TAsyncInvokeThread1<T> = class(TAsyncInvokeThread<T>)
  protected
    FCallback: THproseCallback1<T>;
    procedure DoCallback; override;
    constructor Create(Client: THproseClient; const AName: string;
      const Args: TVariants; Callback: THproseCallback1<T>;
      const ASettings: IInvokeSettings); overload;
  end;

  TAsyncInvokeThread2<T> = class(TAsyncInvokeThread<T>)
  protected
    FCallback: THproseCallback<T>;
    procedure DoCallback; override;
    constructor Create(Client: THproseClient; const AName: string;
      const Args: TVariants; Callback: THproseCallback<T>;
      const ASettings: IInvokeSettings); overload;
  end;
{$ENDIF}

function GetCallback(Callback: THproseCallback): Variant; overload;
function GetCallback(Callback: THproseCallback1): Variant; overload;

implementation

uses
  HproseIO;

{$IFNDEF FPC}
{$I InterlockedAPIs.inc}
{$ENDIF}

type

{ TCallback }

  TCallback = class
  private
    FCallback: THproseCallback;
  public
    constructor Create(Callback: THproseCallback); overload;
  end;

{ TCallback1 }

  TCallback1 = class
  private
    FCallback: THproseCallback1;
  public
    constructor Create(Callback: THproseCallback1); overload;
  end;

  { TOnewayThread }

  TOnewayThread = class(TThread)
  private
    FRequest: TBytes;
    FContext: THproseContext;
    FHandler: TFilterHandler;
    FNext: PNextFilterHandler;
  protected
    procedure Execute; override;
  public
    constructor Create(const Request: TBytes; const Context: THproseContext;
      const Handler: TFilterHandler; const Next: PNextFilterHandler);
  end;

  { TAsyncInvokeThread }

  TAsyncInvokeThread = class(TThread)
  private
    FClient: THproseClient;
    FName: string;
    FArgs: TVariants;
    FCallback1: THproseCallback1;
    FCallback: THproseCallback;
    FSettings: IInvokeSettings;
    FResult: Variant;
    FError: Exception;
  protected
    procedure Execute; override;
    procedure DoCallback;
    procedure DoError;
  public
    constructor Create(Client: THproseClient; const AName: string;
      const Args: TVariants; Callback: THproseCallback1;
      const ASettings: IInvokeSettings); overload;
    constructor Create(Client: THproseClient; const AName: string;
      const Args: TVariants; Callback: THproseCallback;
      const ASettings: IInvokeSettings); overload;
  end;

{ TClientTopic }

procedure TClientTopic.Execute;
var
  ResultType: PTypeInfo;
  Result: Variant;
begin
  ResultType := FSettings.ResultType;
  FSettings.ResultType := nil;
  while FClient.FTopicManager.GetTopic(FName, FId) = Self do begin
    try
      Result := FClient.Invoke(FName, [FId], FSettings);
      if not VarIsNull(Result) then
        FCallback(HproseUnserialize(HproseSerialize(Result), ResultType));
    except
    end;
  end;
end;

constructor TClientTopic.Create(Client: THproseClient; const AName,
  AId: string; Callback: THproseCallback1; const ASettings: IInvokeSettings);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FClient := Client;
  FName := AName;
  FId := AId;
  FCallback := Callback;
  FSettings := ASettings;
end;

{ TTopicManager }

function TTopicManager.GetTopic(const Topic, Id: string): TClientTopic;
var
  Topics: IMap;
begin
  Result := nil;
  FAllTopics.BeginRead;
  try
    Topics := VarToMap(FAllTopics[Topic]);
    if Topics <> nil then VarToObj(Topics[Id], TClientTopic, Result);
  finally
    FAllTopics.EndRead;
  end;
end;

procedure TTopicManager.CreateTopic(const Topic: string);
begin
  FAllTopics.BeginWrite;
  try
    if not FAllTopics.ContainsKey(Topic) then
      FAllTopics[Topic] := THashMap.Create as IMap;
  finally
    FAllTopics.EndWrite;
  end;
end;

procedure TTopicManager.DeleteTopic(const Topic, Id: string);
var
  Topics: IMap;
begin
  FAllTopics.BeginWrite;
  try
    if Id = '' then FAllTopics.Delete(Topic)
    else begin
      Topics := VarToMap(FAllTopics[Topic]);
      if Assigned(Topics) then begin
        Topics.Delete(Id);
        if Topics.Count = 0 then FAllTopics.Delete(Topic);
      end;
    end;
  finally
    FAllTopics.EndWrite;
  end;
end;

constructor TTopicManager.Create;
begin
  FAllTopics := THashMap.Create(False, True);
end;

function TTopicManager.IsSubscribed(const Topic: string): Boolean;
begin
  FAllTopics.BeginRead;
  try
    Result := FAllTopics.ContainsKey(Topic);
  finally
    FAllTopics.EndRead;
  end;
end;

function TTopicManager.SubscribedList: TStringArray;
var
  I, N: Integer;
begin
  FAllTopics.BeginRead;
  try
    N := FAllTopics.Count;
    SetLength(Result, N);
    for I := 0 to N - 1 do Result[I] := VarToStr(FAllTopics.Keys[I]);
  finally
    FAllTopics.EndRead;
  end;
end;

{ TOnewayThread }

procedure TOnewayThread.Execute;
begin
  try
    FHandler(FRequest, FContext, FNext);
  finally
  end;
end;

constructor TOnewayThread.Create(const Request: TBytes;
  const Context: THproseContext; const Handler: TFilterHandler;
  const Next: PNextFilterHandler);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FRequest := Request;
  FContext := Context;
  FHandler := Handler;
  FNext := Next;
end;

{ TAsyncInvokeThread }

procedure TAsyncInvokeThread.Execute;
begin
  try
    FResult := FClient.Invoke(FName, FArgs, FSettings);
    Synchronize({$IFDEF FPC}@{$ENDIF}DoCallback);
  except
    on E: Exception do begin
      FError := E;
      Synchronize({$IFDEF FPC}@{$ENDIF}DoError);
    end;
  end;
end;

procedure TAsyncInvokeThread.DoCallback;
begin
  if not Assigned(FError) then
    if Assigned(FCallback) then FCallback(FResult, FArgs)
    else if Assigned(FCallback1) then FCallback1(FResult);
end;

procedure TAsyncInvokeThread.DoError;
begin
  if Assigned(FSettings) and Assigned(FSettings.OnError) then
    FSettings.OnError(FName, FError)
  else if Assigned(FClient.FOnError) then
    FClient.FOnError(FName, FError);
end;

constructor TAsyncInvokeThread.Create(Client: THproseClient;
  const AName: string; const Args: TVariants; Callback: THproseCallback1;
  const ASettings: IInvokeSettings);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FClient := Client;
  FName := AName;
  FArgs := Args;
  //Client.ByValue(FArgs);
  FCallback1 := Callback;
  FSettings := ASettings;
  FError := nil;
end;

constructor TAsyncInvokeThread.Create(Client: THproseClient;
  const AName: string; const Args: TVariants; Callback: THproseCallback;
  const ASettings: IInvokeSettings);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FClient := Client;
  FName := AName;
  FArgs := Args;
  //Client.ByValue(FArgs);
  FCallback := Callback;
  FSettings := ASettings;
  FError := nil;
end;

{$IFDEF SUPPORTS_ANONYMOUS_METHOD}
{ TAsyncInvokeThread<T> }

procedure TAsyncInvokeThread<T>.Execute;
begin
  try
    FResult := FClient.Invoke<T>(FName, FArgs, FSettings);
    Synchronize({$IFDEF FPC}@{$ENDIF}DoCallback);
  except
    on E: Exception do begin
      FError := E;
      Synchronize({$IFDEF FPC}@{$ENDIF}DoError);
    end;
  end;
end;

procedure TAsyncInvokeThread<T>.DoError;
begin
  if Assigned(FSettings) and Assigned(FSettings.OnError) then
    FSettings.OnError(FName, FError)
  else if Assigned(FClient.FOnError) then
    FClient.FOnError(FName, FError);
end;

constructor TAsyncInvokeThread1<T>.Create(Client: THproseClient;
  const AName: string; const Args: TVariants; Callback: THproseCallback1<T>;
  const ASettings: IInvokeSettings);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FClient := Client;
  FName := AName;
  FArgs := Args;
  //Client.ByValue(FArgs);
  FCallback := Callback;
  FSettings := ASettings;
  FError := nil;
end;

procedure TAsyncInvokeThread1<T>.DoCallback;
begin
  if not Assigned(FError) and Assigned(FCallback) then
    FCallback(FResult);
end;

constructor TAsyncInvokeThread2<T>.Create(Client: THproseClient;
  const AName: string; const Args: TVariants; Callback: THproseCallback<T>;
  const ASettings: IInvokeSettings);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FClient := Client;
  FName := AName;
  FArgs := Args;
  //Client.ByValue(FArgs);
  FCallback := Callback;
  FSettings := ASettings;
  FError := nil;
end;

procedure TAsyncInvokeThread2<T>.DoCallback;
begin
  if not Assigned(FError) and Assigned(FCallback) then
    FCallback(FResult, FArgs);
end;
{$ENDIF}

{ TCallback }

constructor TCallback.Create(Callback: THproseCallback);
begin
  inherited Create;
  FCallback := Callback;
end;

function GetCallback(Callback: THproseCallback): Variant;
begin
  Result := ObjToVar(TCallback.Create(Callback));
end;

{ TCallback1 }

constructor TCallback1.Create(Callback: THproseCallback1);
begin
  inherited Create;
  FCallback := Callback;
end;

function GetCallback(Callback: THproseCallback1): Variant;
begin
  Result := ObjToVar(TCallback1.Create(Callback));
end;

{ TClientContext }

constructor TClientContext.Create(const AClient: THproseClient;
  const ASettings: IInvokeSettings);
begin
  inherited Create(AClient.UserData);
  FSettings := TInvokeSettings.Create();
  FClient := AClient;
  if Assigned(ASettings) then begin
    FSettings.ByRef := ASettings.ByRef;
    FSettings.Simple := ASettings.Simple;
    FSettings.Idempotent := ASettings.Idempotent;
    FSettings.Failswitch := ASettings.Failswitch;
    FSettings.Oneway := ASettings.Oneway;
    FSettings.Retry := ASettings.Retry;
    FSettings.Timeout := ASettings.Timeout;
    FSettings.Mode := ASettings.Mode;
    FSettings.ResultType := ASettings.ResultType;
    FSettings.OnError := ASettings.OnError;
    if Assigned(ASettings.UserData) then
      UserData.PutAll(ASettings.UserData);
  end;
  FSettings.UserData := UserData;
  if FSettings.Retry <= 0 then FSettings.Retry := AClient.Retry;
  if FSettings.Timeout <= 0 then FSettings.Timeout := AClient.Timeout;
end;

{ TInvokeSettings }

function TInvokeSettings.Get(Key: string): Variant;
begin
  Result := FUserData[Key];
end;

function TInvokeSettings.GetByRef: Boolean;
begin
  Result := FByRef;
end;

function TInvokeSettings.GetFailswitch: Boolean;
begin
  Result := FFailswitch;
end;

function TInvokeSettings.GetIdempotent: Boolean;
begin
  Result := FIdempotent;
end;

function TInvokeSettings.GetMode: TResultMode;
begin
  Result := FMode;
end;

function TInvokeSettings.GetOneway: Boolean;
begin
  Result := FOneway;
end;

function TInvokeSettings.GetResultType: PTypeInfo;
begin
  Result := FResultType;
end;

function TInvokeSettings.GetRetry: Integer;
begin
  Result := FRetry;
end;

function TInvokeSettings.GetSimple: Boolean;
begin
  Result := FSimple;
end;

function TInvokeSettings.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

function TInvokeSettings.GetOnError: TOnError;
begin
  Result := FOnError;
end;

function TInvokeSettings.GetUserData: ICaseInsensitiveHashMap;
begin
  Result := FUserData;
end;

procedure TInvokeSettings.Put(Key: string; const Value: Variant);
begin
  FUserData[Key] := Value;
end;

procedure TInvokeSettings.SetByRef(AValue: Boolean);
begin
  FByRef := AValue;
end;

procedure TInvokeSettings.SetFailswitch(AValue: Boolean);
begin
  FFailswitch := AValue;
end;

procedure TInvokeSettings.SetIdempotent(AValue: Boolean);
begin
  FIdempotent := AValue;
end;

procedure TInvokeSettings.SetMode(AValue: TResultMode);
begin
  FMode := AValue;
end;

procedure TInvokeSettings.SetOneway(AValue: Boolean);
begin
  FOneway := AValue;
end;

procedure TInvokeSettings.SetResultType(AValue: PTypeInfo);
begin
  FResultType := AValue;
end;

procedure TInvokeSettings.SetRetry(AValue: Integer);
begin
  FRetry := AValue;
end;

procedure TInvokeSettings.SetSimple(AValue: Boolean);
begin
  FSimple := AValue;
end;

procedure TInvokeSettings.SetTimeout(AValue: Integer);
begin
  FTimeout := AValue;
end;

procedure TInvokeSettings.SetOnError(AValue: TOnError);
begin
  FOnError := AValue;
end;

procedure TInvokeSettings.SetUserData(AValue: ICaseInsensitiveHashMap);
begin
  FUserData := AValue;
end;

constructor TInvokeSettings.Create;
begin
  inherited Create;
  FUserData := TCaseInsensitiveHashMap.Create(16, 0.75, False);
end;

constructor TInvokeSettings.Create(const AOnError: TOnError);
begin
    Create;
    FOnError := AOnError;
end;

constructor TInvokeSettings.Create(const ASettings: array of const);
var
  I, N: Integer;
  Key: string;
  Value: Variant;
begin
  Create;
  N := Length(ASettings);
  if Odd(N) then raise EArrayListError.Create('The ASettings length must be even.');
  I := 0;
  while I < N do begin
    Key := LowerCase(VarToStr(VarRecToVar(ASettings[I])));
    Value := VarRecToVar(ASettings[I + 1]);
    if SameStr(Key, 'byref') then FByRef := Value
    else if SameStr(Key, 'simple') then FSimple := Value
    else if SameStr(Key, 'idempotent') then FIdempotent := Value
    else if SameStr(Key, 'failswitch') then FFailswitch := Value
    else if SameStr(Key, 'oneway') then FOneway := Value
    else if SameStr(Key, 'retry') then FRetry := Value
    else if SameStr(Key, 'timeout') then FTimeout := Value
    else if SameStr(Key, 'mode') then FMode := TResultMode(Value)
    else if SameStr(Key, 'resulttype') then FResultType := PTypeInfo(NativeInt(Value))
    else if SameStr(Key, 'userdata') then FUserData.Put(Value);
    Inc(I, 2);
  end;
end;

constructor TInvokeSettings.Create(const AOnError: TOnError;
  const ASettings: array of const);
begin
  Create(ASettings);
  FOnError := AOnError;
end;

{ THproseClient }

constructor THproseClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FNameSpace := '';
  FURI := '';
  FURIList := nil;
  FIndex := 0;
  FFailround := 0;
  FOnError := nil;
  FOnFailswitch := nil;
  FRetry := 10;
  FTimeout := 30000;
  FFilters := TFilterList.Create;
  FHandlers := THandlerManager.Create({$IFDEF FPC}@{$ENDIF}InvokeHandler,
    {$IFDEF FPC}@{$ENDIF}BeforeFilterHandler,
    {$IFDEF FPC}@{$ENDIF}AfterFilterHandler);
end;

destructor THproseClient.Destroy;
begin
  FreeAndNil(FFilters);
  FreeAndNil(FHandlers);
  inherited Destroy;
end;

class function THproseClient.New(const AURI: string; const ANameSpace: string): Variant;
begin
  Result := Self.Create(nil).UseService(AURI, ANameSpace);
end;

class function THproseClient.New(const AURIList: array of string;
  const ANameSpace: string): Variant;
begin
  Result := Self.Create(nil).UseService(AURIList, ANameSpace);
end;

function THproseClient.UseService(const AURI: string; const ANameSpace: string
  ): Variant;
begin
  FNameSpace := ANameSpace;
  Self.URI := AURI;
  Result := ObjToVar(Self);
end;

function THproseClient.UseService(const AURIList: array of string;
  const ANameSpace: string): Variant;
begin
  FNameSpace := ANameSpace;
  SetURIList(AURIList);
  Result := ObjToVar(Self);
end;

procedure THproseClient.SetURIList(const AURIList: array of string);
begin
  FURIList := ShuffleStringArray(AURIList);
  FIndex := 0;
  FFailround := 0;
  InitURI(FURIList[0]);
end;

{
procedure THproseClient.ByValue(var Arguments: TVariants);
var
  I: Integer;
begin
  for I := 0 to Length(Arguments) - 1 do Arguments[I] := VarUnref(Arguments[I]);
end;
}

procedure THproseClient.SetURI(const AValue: string);
begin
  if AValue <> '' then
    SetURIList([AValue])
  else begin
    FURIList := nil;
    FIndex := 0;
    FFailround := 0;
    InitURI('');
  end;
end;

procedure THproseClient.InitURI(const AValue: string);
begin
  FURI := AValue;
end;

function THproseClient.GetFullName(const AName: string): string;
begin
  if FNameSpace <> '' then
    Result := FNameSpace + '_' + AName
  else
    Result := AName;
end;

function THproseClient.Encode(const AName: string; var Args: TVariants;
  const Context: TClientContext): TBytes;
var
  Settings: IInvokeSettings;
  OutStream: TBytesStream;
  HproseWriter: THproseWriter;
begin
  Settings := Context.Settings;
  OutStream := TBytesStream.Create;
  HproseWriter := THproseWriter.Create(OutStream, Settings.Simple);
  try
    OutStream.Write(HproseTagCall, 1);
{$IFDEF NEXTGEN}
    HproseWriter.WriteString(AName);
{$ELSE}
    HproseWriter.WriteString(WideString(AName));
{$ENDIF}
    if (Length(Args) > 0) or Settings.ByRef then begin
      HproseWriter.Reset;
      HproseWriter.WriteArray(Args);
      if Settings.ByRef then HproseWriter.WriteBoolean(True);
    end;
    OutStream.Write(HproseTagEnd, 1);
    Result := OutStream.Bytes;
    SetLength(Result, OutStream.Size);
  finally
    HproseWriter.Free;
    OutStream.Free;
  end;
end;

function THproseClient.Decode(var Data: TBytes; var Args: TVariants;
  const Context: TClientContext): Variant;
var
  Settings: IInvokeSettings;
  N: Integer;
  ATag: Byte;
  InStream: TBytesStream;
  HproseReader: THproseReader;

begin
  Settings := Context.Settings;
  if Settings.Oneway then begin
    Result := Null;
    Exit;
  end;
  if not Assigned(Data) or (Length(Data) = 0) then
    raise Exception.Create('unexpected EOF');
  N := Length(Data);
  if Data[N - 1] <> HproseTagEnd then
    raise Exception.Create('Wrong Response: ' + #13#10 + string(StringOf(Data)));
  Result := Null;
  if Settings.Mode = RawWithEndTag then Result := Data
  else if Settings.Mode = Raw then begin
    SetLength(Data, N - 1);
    Result := Data;
  end
  else begin
    InStream := TBytesStream.Create(Data);
    HproseReader := THproseReader.Create(InStream);
    try
      ATag := 0;
      InStream.ReadBuffer(ATag, 1);
      if ATag = HproseTagResult then begin
        if Settings.Mode = Serialized then
          Result := HproseReader.ReadRaw
        else
          Result := HproseReader.Unserialize(Settings.ResultType);
        InStream.ReadBuffer(ATag, 1);
        if ATag = HproseTagArgument then begin
          HproseReader.Reset;
          Args := HproseReader.ReadVariantArray;
          InStream.ReadBuffer(ATag, 1);
        end
      end
      else if ATag = HproseTagError then
        raise Exception.Create(string(HproseReader.ReadString()));
      if ATag <> HproseTagEnd then
        raise Exception.Create('Wrong Response: ' + #13#10 + string(StringOf(Data)));
    finally
      HproseReader.Free;
      InStream.Free;
    end;
  end;
end;

function THproseClient.InvokeHandler(const AName: String; var Args: TVariants;
  const Context: THproseContext; Next: PNextInvokeHandler): Variant;
var
  Request, Response: TBytes;
  Handler: TFilterHandler;
  ANext: PNextFilterHandler;
begin
  Request := Encode(AName, Args, TClientContext(Context));
  Handler := FHandlers.BeforeFilterHandler^.Handler;
  ANext := FHandlers.BeforeFilterHandler^.Next;
  Response := Handler(Request, Context, ANext);
  Result := Decode(Response, Args, TClientContext(Context));
end;

function THproseClient.BeforeFilterHandler(var Request: TBytes;
  const Context: THproseContext; Next: PNextFilterHandler): TBytes;
var
  Handler: TFilterHandler;
begin
  Request := FFilters.OutputFilter(Request, Context);
  Handler := FHandlers.AfterFilterHandler^.Handler;
  Next := FHandlers.AfterFilterHandler^.Next;
  if TClientContext(Context).Settings.Oneway then begin
    TOnewayThread.Create(Request, Context, Handler, Next);
    Result := nil;
  end
  else begin
    Result := Handler(Request, Context, Next);
    Result := FFilters.InputFilter(Result, Context);
  end;
end;

function THproseClient.AfterFilterHandler(var Request: TBytes;
  const Context: THproseContext; Next: PNextFilterHandler): TBytes;
begin
  try
    Result := SendAndReceive(Request, TClientContext(Context));
  except
    on E: Exception do begin
      Result := RetrySendRequest(Request, TClientContext(Context));
      if not Assigned(Result) then raise;
    end;
  end;
end;

function THproseClient.RetrySendRequest(var Request: TBytes;
  const Context: TClientContext): TBytes;
var
  Settings: IInvokeSettings;
  Interval: Integer;
begin
  Settings := Context.Settings;
  if Settings.Failswitch then FailSwitch;
  if Settings.Idempotent and (Context.Retried < Settings.Retry) then begin
    Context.Retried := Context.Retried + 1;
    Interval := Context.Retried * 500;
    if Settings.Failswitch then Dec(Interval, (Length(FURIList) - 1) * 500);
    if Interval > 5000 then Interval := 5000;
    if Interval > 0 then Sleep(Interval);
    Result := AfterFilterHandler(Request, Context, nil);
  end;
  Result := nil;
end;

procedure THproseClient.FailSwitch;
var
  N: Integer;
begin
  N := Length(FURIList);
  if N > 1 then begin
    if InterlockedCompareExchange(FIndex, 0, N - 1) = 0 then begin
      FURI := FURIList[0];
      InterlockedIncrement(FFailround);
    end
    else FURI := FURIList[InterlockedIncrement(FIndex)];
  end
  else InterlockedIncrement(FFailround);
  if Assigned(FOnFailswitch) then FOnFailswitch(Self);
end;

function THproseClient.AddFilter(const Filter: IFilter): THproseClient;
begin
  FFilters.Add(Filter);
  Result := Self;
end;

function THproseClient.RemoveFilter(const Filter: IFilter): THproseClient;
begin
  FFilters.Remove(Filter);
  Result := Self;
end;

function THproseClient.AddInvokeHandler(const Handler: TInvokeHandler
  ): THproseClient;
begin
  FHandlers.AddInvokeHandler(Handler);
  Result := Self;
end;

function THproseClient.AddBeforeFilterHandler(const Handler: TFilterHandler
  ): THproseClient;
begin
  FHandlers.AddBeforeFilterHandler(Handler);
  Result := Self;
end;

function THproseClient.AddAfterFilterHandler(const Handler: TFilterHandler
  ): THproseClient;
begin
  FHandlers.AddAfterFilterHandler(Handler);
  Result := Self;
end;

function THproseClient.Invoke(const AName: string; const Arguments: TVarDataArray): Variant;
var
  Callback: TCallback;
  Callback1: TCallback1;
  ASettings: IInvokeSettings;
  Args: TVariants;
  Len: Integer;
begin
  Callback := nil;
  Callback1 := nil;
  ASettings := nil;
  Args := TVariants(Arguments);
  Len := Length(Args);
  if (Len > 0) and VarToIntf(Args[Len - 1], IInvokeSettings, ASettings) and Assigned(ASettings) then begin
    Dec(Len);
    SetLength(Args, Len);
  end;
  if (Len > 0) and VarToObj(Args[Len - 1], TCallback, Callback) and Assigned(Callback) then begin
    Dec(Len);
    SetLength(Args, Len);
  end
  else if (Len > 0) and VarToObj(Args[Len - 1], TCallback1, Callback1) and Assigned(Callback1) then begin
    Dec(Len);
    SetLength(Args, Len);
  end;
  if Assigned(Callback) then begin
    Invoke(AName, Args, Callback.FCallback, ASettings);
    Callback.Free;
  end
  else if Assigned(Callback1) then begin
    Invoke(AName, Args, Callback1.FCallback, ASettings);
    Callback1.Free;
  end
  else
    Result := Invoke(AName, Args, ASettings);
end;

// Synchronous invoke
function THproseClient.Invoke(const AName: string;
  const ASettings: IInvokeSettings): Variant;
var
  Arguments: TVariants;
begin
  Arguments := nil;
  Result := Invoke(AName, Arguments, ASettings);
end;

function THproseClient.Invoke(const AName: string; const Args: array of const;
  const ASettings: IInvokeSettings): Variant;
var
  Arguments: TVariants;
begin
  Arguments := ToVariants(Args);
  Result := Invoke(AName, Arguments, ASettings);
end;

function THproseClient.Invoke(const AName: string; var Args: TVariants;
   const ASettings: IInvokeSettings): Variant;
var
  FullName: string;
  Context: TClientContext;
  Handler: TInvokeHandler;
  Next: PNextInvokeHandler;
begin
  FullName := GetFullName(AName);
  Context := TClientContext.Create(Self, ASettings);
  Handler := FHandlers.InvokeHandler^.Handler;
  Next := FHandlers.InvokeHandler^.Next;
  try
    Result := Handler(FullName, Args, Context, Next);
  finally
    FreeAndNil(Context);
  end;
end;

// Asynchronous invoke
procedure THproseClient.Invoke(const AName: string;
  Callback: THproseCallback1; const ASettings: IInvokeSettings);
var
  Arguments: TVariants;
begin
  Arguments := nil;
  Invoke(AName, Arguments, Callback, ASettings);
end;

procedure THproseClient.Invoke(const AName: string; const Args: array of const;
  Callback: THproseCallback; const ASettings: IInvokeSettings);
var
  Arguments: TVariants;
begin
  Arguments := ToVariants(Args);
  Invoke(AName, Arguments, Callback, ASettings);
end;

procedure THproseClient.Invoke(const AName: string; const Args: array of const;
  Callback: THproseCallback1; const ASettings: IInvokeSettings);
var
  Arguments: TVariants;
begin
  Arguments := ToVariants(Args);
  Invoke(AName, Arguments, Callback, ASettings);
end;

procedure THproseClient.Invoke(const AName: string; var Args: TVariants;
  Callback: THproseCallback; const ASettings: IInvokeSettings);
begin
  TAsyncInvokeThread.Create(Self, AName, Args, Callback, ASettings);
end;

procedure THproseClient.Invoke(const AName: string; var Args: TVariants;
  Callback: THproseCallback1; const ASettings: IInvokeSettings);
begin
  TAsyncInvokeThread.Create(Self, AName, Args, Callback, ASettings);
end;

{$IFDEF SUPPORTS_ANONYMOUS_METHOD}

procedure THproseClient.VarToT(Info: PTypeInfo; const Src: Variant; out Dst);
var
  TypeData: PTypeData;
  TypeName: string;
  AClass: TClass;
begin
  TypeName := GetTypeName(Info);
  if TypeName = 'Boolean' then
    Boolean(Dst) := TVarData(Src).VBoolean
  else if (TypeName = 'TDateTime') or
          (TypeName = 'TDate') or
          (TypeName = 'TTime') then
    TDateTime(Dst) := VarToDateTime(Src)
  else if TypeName = 'UInt64' then
    UInt64(Dst) := TVarData(Src).VUInt64
  else begin
    TypeData := GetTypeData(Info);
    case Info^.Kind of
      tkInteger, tkEnumeration, tkSet:
        case TypeData^.OrdType of
          otSByte:
            ShortInt(Dst) := TVarData(Src).VShortInt;
          otUByte:
            Byte(Dst) := TVarData(Src).VByte;
          otSWord:
            SmallInt(Dst) := TVarData(Src).VSmallInt;
          otUWord:
            Word(Dst) := TVarData(Src).VWord;
          otSLong:
            Integer(Dst) := TVarData(Src).VInteger;
          otULong:
            Cardinal(Dst) := TVarData(Src).VLongWord;
        end;
{$IFNDEF NEXTGEN}
      tkChar:
        AnsiChar(Dst) := AnsiChar(VarToStr(Src)[1]);
      tkWChar:
        WideChar(Dst) := WideChar(VarToWideStr(Src)[1]);
{$ENDIF}
      tkFloat:
        case TypeData^.FloatType of
          ftSingle:
            Single(Dst) := TVarData(Src).VSingle;
          ftDouble:
            Double(Dst) := TVarData(Src).VDouble;
          ftExtended:
            Extended(Dst) := Extended(Src);
          ftComp:
            Comp(Dst) := Comp(Src);
          ftCurr:
            Currency(Dst) := TVarData(Src).VCurrency;
        end;
{$IFNDEF NEXTGEN}
        tkString:
          ShortString(Dst) := ShortString(VarToStr(Src));
        tkLString:
          AnsiString(Dst) := AnsiString(VarToStr(Src));
        tkWString:
          WideString(Dst) := VarToWideStr(Src);
{$ENDIF}
{$IFDEF SUPPORTS_UNICODE}
        tkUString:
          UnicodeString(Dst) := UnicodeString(TVarData(Src).VUString);
{$ENDIF}
        tkInt64:
          Int64(Dst) := TVarData(Src).VInt64;
        tkInterface:
          VarToIntf(Src, TypeData^.Guid, Dst);
        tkDynArray:
          DynArrayFromVariant(Pointer(Dst), Src, Info);
        tkClass: begin
          AClass := TypeData^.ClassType;
          VarToObj(Src, AClass, Dst);
        end;
    end;
  end;
end;

function THproseClient.VarTo<T>(const AValue: Variant): T;
begin
  VarToT(TypeInfo(T), AValue, Result);
end;

// Synchronous invoke
function THproseClient.Invoke<T>(const AName: string;
   const ASettings: IInvokeSettings): T;
var
  Arguments: TVariants;
begin
  Arguments := nil;
  Result := Self.Invoke<T>(AName, Arguments, ASettings);
end;

function THproseClient.Invoke<T>(const AName: string;
  const Args: array of const; const ASettings: IInvokeSettings): T;
var
  Arguments: TVariants;
begin
  Arguments := ToVariants(Args);
  Result := Self.Invoke<T>(AName, Arguments, ASettings);
end;

function THproseClient.Invoke<T>(const AName: string;
  var Args: TVariants; const ASettings: IInvokeSettings): T;
var
  FullName: string;
  Context: TClientContext;
  Handler: TInvokeHandler;
  Next: PNextInvokeHandler;
begin
  FullName := GetFullName(AName);
  if ASettings = nil then
    Context := TClientContext.Create(Self, TInvokeSettings.Create(['ResultType', TypeInfo(T)]))
  else begin
    ASettings.ResultType := TypeInfo(T);
    Context := TClientContext.Create(Self, ASettings);
  end;
  Handler := FHandlers.InvokeHandler^.Handler;
  Next := FHandlers.InvokeHandler^.Next;
  try
    Result := Self.VarTo<T>(Handler(FullName, Args, Context, Next));
  finally
    FreeAndNil(Context);
  end;
end;

procedure THproseClient.Invoke<T>(const AName: string;
  Callback: THproseCallback1<T>; const ASettings: IInvokeSettings);
var
  Arguments: TVariants;
begin
  Arguments := nil;
  Self.Invoke<T>(AName, Arguments, Callback, ASettings);
end;

procedure THproseClient.Invoke<T>(const AName: string;
  const Args: array of const; Callback: THproseCallback<T>;
  const ASettings: IInvokeSettings);
var
  Arguments: TVariants;
begin
  Arguments := ToVariants(Args);
  Self.Invoke<T>(AName, Arguments, Callback, ASettings);
end;

procedure THproseClient.Invoke<T>(const AName: string;
  const Args: array of const; Callback: THproseCallback1<T>;
  const ASettings: IInvokeSettings);
var
  Arguments: TVariants;
begin
  Arguments := ToVariants(Args);
  Self.Invoke<T>(AName, Arguments, Callback, ASettings);
end;

procedure THproseClient.Invoke<T>(const AName: string; var Args: TVariants;
  Callback: THproseCallback<T>; const ASettings: IInvokeSettings);
begin
  TAsyncInvokeThread2<T>.Create(Self, AName, Args, Callback, ASettings);
end;

procedure THproseClient.Invoke<T>(const AName: string; var Args: TVariants;
  Callback: THproseCallback1<T>; const ASettings: IInvokeSettings);
begin
  TAsyncInvokeThread1<T>.Create(Self, AName, Args, Callback, ASettings);
end;

{$ENDIF}

var
  AutoIdSettings: IInvokeSettings;

function THproseClient.AutoId: string;
begin
  FTopicManager.FAllTopics.BeginRead;
  try
    if FId <> '' then begin
      Result := FId;
      Exit;
    end;
  finally
    FTopicManager.FAllTopics.EndRead;
  end;
  FTopicManager.FAllTopics.BeginWrite;
  try
    if FId = '' then
      FId := VarToStr(Invoke('#', AutoIdSettings));
    Result := FId
  finally
    FTopicManager.FAllTopics.EndWrite;
  end;
end;

procedure THproseClient.Subscribe(const AName: string; AId: string;
  Callback: THproseCallback1; ASettings: IInvokeSettings);
var
  Topics: IMap;
  Topic: TClientTopic;
begin
  if AId = '' then AId := AutoId;
  FTopicManager.CreateTopic(AName);
  Topic := FTopicManager.GetTopic(AName, AId);
  if not Assigned(Topic) then begin
    if not Assigned(ASettings) then ASettings := TInvokeSettings.Create();
    ASettings.ByRef := False;
    ASettings.Idempotent := True;
    ASettings.Mode := Normal;
    ASettings.Oneway := False;
    ASettings.Simple := true;
    Topic := TClientTopic.Create(Self, AName, AId, Callback, ASettings);
    FTopicManager.FAllTopics.BeginWrite;
    try
      Topics := VarToMap(FTopicManager.FAllTopics[AName]);
      Topics[AId] := ObjToVar(Topic);
    finally
      FTopicManager.FAllTopics.EndWrite;
    end;
{$IF DEFINED(DELPHI2010_UP) OR DEFINED(FPC) }
    Topic.Start;
{$ELSE}
    Topic.Resume;
{$IFEND}
  end;
end;

procedure THproseClient.Subscribe(const AName: string;
  Callback: THproseCallback1; ASettings: IInvokeSettings);
begin
  Subscribe(AName, AutoId(), Callback, ASettings);
end;

{$IFDEF SUPPORTS_ANONYMOUS_METHOD}

procedure THproseClient.Subscribe<T>(const AName: string; AId: string;
  Callback: THproseCallback1<T>; ASettings: IInvokeSettings = nil);
begin
  if not Assigned(ASettings) then ASettings := TInvokeSettings.Create();
  ASettings.ResultType := TypeInfo(T);
  Subscribe(AName, AId, procedure(Result: Variant) begin
    Callback(VarTo<T>(Result));
  end, ASettings);
end;

procedure THproseClient.Subscribe<T>(const AName: string;
  Callback: THproseCallback1<T>; ASettings: IInvokeSettings = nil);
begin
  Self.Subscribe<T>(AName, AutoId(), Callback, ASettings);
end;

{$ENDIF}

procedure THproseClient.Unsubscribe(const AName: string; AId: string);
begin
  if AId = '' then AId := FId;
  FTopicManager.DeleteTopic(AName, AId);
end;

function THproseClient.IsSubscribed(const Topic: string): Boolean;
begin
  Result := FTopicManager.IsSubscribed(Topic);
end;

function THproseClient.SubscribedList: TStringArray;
begin
  Result := FTopicManager.SubscribedList();
end;

initialization
  AutoIdSettings := TInvokeSettings.Create([
  'Simple',     True,
  'Idempotent', True,
  'Failswitch', True,
  'ResultType', TypeInfo(string)]);

end.
