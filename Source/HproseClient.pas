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
 * LastModified: Oct 30, 2016                             *
 * Author: Ma Bingyao <andot@hprose.com>                  *
 *                                                        *
\**********************************************************/
}
unit HproseClient;

{$I Hprose.inc}

interface

uses HproseCommon, Classes, SysUtils, TypInfo, Variants;

type
{$IFDEF Supports_Anonymous_Method}
  TCallback1 = reference to procedure(Result: Variant);
  TCallback2 = reference to procedure(Result: Variant;
    const Args: TVariants);
  TCallbackE = reference to procedure(const Name:string;
                                const Error: Exception);
{$ELSE}
  TCallback1 = procedure(Result: Variant) of object;
  TCallback2 = procedure(Result: Variant;
    const Args: TVariants) of object;

  TCallbackE = procedure(const Name:string;
                                const Error: Exception) of object;
{$ENDIF}

{$IFDEF Supports_Generics}
  TCallback1<T> = reference to procedure(Result: T);
  TCallback2<T> = reference to procedure(Result: T;
    const Args: TVariants);
{$ENDIF}

  THproseClient = class(TComponent, IInvokeableVarObject)
  private
    FNameSpace: string;
    FCallbackE: TCallbackE;
    FFilters: IList;
    function GetFilter: IFilter;
    procedure SetFilter(const Filter: IFilter);
    function DoInput(var Args: TVariants; ResultType: PTypeInfo;
      ResultMode: TResultMode; Data: TBytes): Variant; overload;
    function DoInput(ResultType: PTypeInfo; ResultMode: TResultMode;
      Data: TBytes): Variant; overload;
    function DoOutput(const AName: string;
      const Args: array of const; Simple: Boolean): TBytes; overload;
    function DoOutput(const AName: string; const Args: TVariants;
      ByRef: Boolean; Simple: Boolean): TBytes; overload;
{$IFDEF Supports_Generics}
    procedure DoInput(var Args: TVariants; ResultType: PTypeInfo;
      Data: TBytes; out Result); overload;
    procedure DoInput(ResultType: PTypeInfo;
      Data: TBytes; out Result); overload;
{$ENDIF}
    // Synchronous invoke
    function Invoke(const AName: string; const Args: array of const;
      ResultType: PTypeInfo; ResultMode: TResultMode; Simple: Boolean): Variant;
      overload;
    function Invoke(const AName: string; var Args: TVariants;
      ResultType: PTypeInfo; ByRef: Boolean;
      ResultMode: TResultMode; Simple: Boolean): Variant;
      overload;
    // Asynchronous invoke
    procedure Invoke(const AName: string; const Args: array of const;
      Callback: TCallback1;
      CallbackE: TCallbackE;
      ResultType: PTypeInfo; ResultMode: TResultMode; Simple: Boolean); overload;
    procedure Invoke(const AName: string; var Args: TVariants;
      Callback: TCallback1;
      CallbackE: TCallbackE;
      ResultType: PTypeInfo; ResultMode: TResultMode; Simple: Boolean); overload;
    procedure Invoke(const AName: string; var Args: TVariants;
      Callback: TCallback2;
      CallbackE: TCallbackE;
      ResultType: PTypeInfo;
      ByRef: Boolean; ResultMode: TResultMode; Simple: Boolean); overload;
  protected
    FUri: string;
    function SendAndReceive(Data: TBytes): TBytes; overload; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    class function New(const AUrl: string; const ANameSpace: string = ''): Variant;
    function UseService(const AUri: string = ''; const ANameSpace: string = ''): Variant; virtual;
    procedure AddFilter(const Filter: IFilter);
    function RemoveFilter(const Filter: IFilter): Boolean;
    // Synchronous invoke
    function Invoke(const AName: string;
      ResultMode: TResultMode = Normal): Variant;
      overload;
    function Invoke(const AName: string; ResultType: PTypeInfo): Variant;
      overload;
    // Synchronous invoke
    function Invoke(const AName: string; const Args: array of const;
      ResultMode: TResultMode = Normal; Simple: Boolean = False): Variant;
      overload;
    function Invoke(const AName: string; const Args: array of const;
      ResultType: PTypeInfo; Simple: Boolean = False): Variant;
      overload;
    // Synchronous invoke
    function Invoke(const AName: string; const Arguments: TVarDataArray): Variant; overload;
    function Invoke(const AName: string; var Args: TVariants; ByRef: Boolean = True;
      ResultMode: TResultMode = Normal; Simple: Boolean = False): Variant;
      overload;
    function Invoke(const AName: string; var Args: TVariants; ResultType: PTypeInfo;
      ByRef: Boolean = True; Simple: Boolean = False): Variant;
      overload;
{$IFDEF Supports_Generics}
    // Synchronous invoke
    function Invoke<T>(const AName: string): T; overload;
    function Invoke<T>(const AName: string; const Args: array of const;
      Simple: Boolean = False): T; overload;
    function Invoke<T>(const AName: string; var Args: TVariants;
      ByRef: Boolean = True; Simple: Boolean = False): T; overload;
{$ENDIF}
    // Asynchronous invoke
    procedure Invoke(const AName: string;
      Callback: TCallback1;
      ResultMode: TResultMode = Normal);
      overload;
    procedure Invoke(const AName: string;
      Callback: TCallback1;
      CallbackE: TCallbackE;
      ResultMode: TResultMode = Normal);
      overload;
    // Asynchronous invoke
    procedure Invoke(const AName: string;
      Callback: TCallback1;
      ResultType: PTypeInfo);
      overload;
    procedure Invoke(const AName: string;
      Callback: TCallback1;
      CallbackE: TCallbackE;
      ResultType: PTypeInfo);
      overload;
    // Asynchronous invoke
    procedure Invoke(const AName: string; const Args: array of const;
      Callback: TCallback1;
      ResultMode: TResultMode = Normal; Simple: Boolean = False);
      overload;
    procedure Invoke(const AName: string; const Args: array of const;
      Callback: TCallback1;
      CallbackE: TCallbackE;
      ResultMode: TResultMode = Normal; Simple: Boolean = False);
      overload;
    // Asynchronous invoke
    procedure Invoke(const AName: string; const Args: array of const;
      Callback: TCallback1;
      ResultType: PTypeInfo; Simple: Boolean = False);
      overload;
    procedure Invoke(const AName: string; const Args: array of const;
      Callback: TCallback1;
      CallbackE: TCallbackE;
      ResultType: PTypeInfo; Simple: Boolean = False);
      overload;
    // Asynchronous invoke
    procedure Invoke(const AName: string; var Args: TVariants;
      Callback: TCallback1;
      ResultMode: TResultMode = Normal; Simple: Boolean = False);
      overload;
    procedure Invoke(const AName: string; var Args: TVariants;
      Callback: TCallback1;
      CallbackE: TCallbackE;
      ResultMode: TResultMode = Normal; Simple: Boolean = False);
      overload;
    // Asynchronous invoke
    procedure Invoke(const AName: string; var Args: TVariants;
      Callback: TCallback1;
      ResultType: PTypeInfo; Simple: Boolean = False);
      overload;
    procedure Invoke(const AName: string; var Args: TVariants;
      Callback: TCallback1;
      CallbackE: TCallbackE;
      ResultType: PTypeInfo; Simple: Boolean = False);
      overload;
    // Asynchronous invoke
    procedure Invoke(const AName: string; var Args: TVariants;
      Callback: TCallback2;
      ByRef: Boolean = True;
      ResultMode: TResultMode = Normal; Simple: Boolean = False);
      overload;
    procedure Invoke(const AName: string; var Args: TVariants;
      Callback: TCallback2;
      CallbackE: TCallbackE;
      ByRef: Boolean = True;
      ResultMode: TResultMode = Normal; Simple: Boolean = False);
      overload;
    // Asynchronous invoke
    procedure Invoke(const AName: string; var Args: TVariants;
      Callback: TCallback2;
      ResultType: PTypeInfo;
      ByRef: Boolean = True; Simple: Boolean = False);
      overload;
    procedure Invoke(const AName: string; var Args: TVariants;
      Callback: TCallback2;
      CallbackE: TCallbackE;
      ResultType: PTypeInfo;
      ByRef: Boolean = True; Simple: Boolean = False);
      overload;
{$IFDEF Supports_Generics}
    // Asynchronous invoke
    procedure Invoke<T>(const AName: string;
      Callback: TCallback1<T>;
      CallbackE: TCallbackE = nil); overload;
    procedure Invoke<T>(const AName: string; const Args: array of const;
      Callback: TCallback1<T>;
      CallbackE: TCallbackE = nil; Simple: Boolean = False); overload;
    procedure Invoke<T>(const AName: string; var Args: TVariants;
      Callback: TCallback2<T>;
      ByRef: Boolean = True; Simple: Boolean = False); overload;
    procedure Invoke<T>(const AName: string; var Args: TVariants;
      Callback: TCallback2<T>;
      CallbackE: TCallbackE;
      ByRef: Boolean = True; Simple: Boolean = False); overload;
{$ENDIF}
  published
    property Uri: string read FUri;
    property Filter: IFilter read GetFilter write SetFilter;
    property NameSpace: string read FNameSpace write FNameSpace;
    // This event OnError only for asynchronous invoke
    property OnError: TCallbackE read FCallbackE write FCallbackE;
  end;

  TClientContext = class(TContext)
  private
    FClient: THproseClient;
  public
    property Client: THproseClient read FClient;
  end;

{$IFDEF Supports_Generics}
// The following two classes is private class, but they can't be moved to the
// implementation section because of E2506.
  TAsyncInvokeThread1<T> = class(TThread)
  private
    FClient: THproseClient;
    FName: string;
    FArgs: TConstArray;
    FCallback: TCallback1<T>;
    FCallbackE: TCallbackE;
    FSimple: Boolean;
    FResult: T;
    FError: Exception;
    constructor Create(Client: THproseClient; const AName: string;
      const Args: array of const; Callback: TCallback1<T>;
      CallbackE: TCallbackE; Simple: Boolean);
  protected
    procedure Execute; override;
    procedure DoCallback;
    procedure DoError;
  end;

  TAsyncInvokeThread2<T> = class(TThread)
  private
    FClient: THproseClient;
    FName: string;
    FArgs: TVariants;
    FCallback: TCallback2<T>;
    FCallbackE: TCallbackE;
    FByRef: Boolean;
    FSimple: Boolean;
    FResult: T;
    FError: Exception;
    constructor Create(Client: THproseClient; const AName: string;
      const Args: TVariants; Callback: TCallback2<T>;
      CallbackE: TCallbackE; ByRef: Boolean; Simple: Boolean);
  protected
    procedure Execute; override;
    procedure DoCallback;
    procedure DoError;
  end;
{$ENDIF}

  TType = class
  private
    FTypeInfo: PTypeInfo;
  public
{$IFDEF Supports_Generics}
    class function New<T>: Variant;
{$ENDIF}
    constructor Create(Info: PTypeInfo);
  end;

function TypeToVar(Info: PTypeInfo): Variant;

function CallbackToVar: Variant; overload;
function CallbackToVar(Callback: TCallback1): Variant; overload;
function CallbackToVar(Callback: TCallback2): Variant; overload;
function CallbackToVar(CallbackE: TCallbackE): Variant; overload;

implementation

uses
  HproseIO;

type

  TCallback = class
  private
    FCallback1: TCallback1;
    FCallback2: TCallback2;
    FCallbackE: TCallbackE;
  public
    constructor Create; overload;
    constructor Create(Callback: TCallback1); overload;
    constructor Create(Callback: TCallback2); overload;
    constructor Create(CallbackE: TCallbackE); overload;
  end;

  TAsyncInvokeThread1 = class(TThread)
  private
    FClient: THproseClient;
    FName: string;
    FArgs: TConstArray;
    FCallback: TCallback1;
    FCallbackE: TCallbackE;
    FResultType: PTypeInfo;
    FResultMode: TResultMode;
    FSimple: Boolean;
    FResult: Variant;
    FError: Exception;
  protected
    procedure Execute; override;
    procedure DoCallback;
    procedure DoError;
  public
    constructor Create(Client: THproseClient; const AName: string;
      const Args: array of const; Callback: TCallback1;
      CallbackE: TCallbackE; ResultType: PTypeInfo;
      ResultMode: TResultMode; Simple: Boolean);
  end;

  TAsyncInvokeThread2 = class(TThread)
  private
    FClient: THproseClient;
    FName: string;
    FArgs: TVariants;
    FCallback1: TCallback1;
    FCallback2: TCallback2;
    FCallbackE: TCallbackE;
    FResultType: PTypeInfo;
    FByRef: Boolean;
    FResultMode: TResultMode;
    FSimple: Boolean;
    FResult: Variant;
    FError: Exception;
  protected
    procedure Execute; override;
    procedure DoCallback;
    procedure DoError;
  public
    constructor Create(Client: THproseClient; const AName: string;
      const Args: TVariants; Callback: TCallback1;
      CallbackE: TCallbackE; ResultType: PTypeInfo;
      ByRef: Boolean; ResultMode: TResultMode; Simple: Boolean); overload;
    constructor Create(Client: THproseClient; const AName: string;
      const Args: TVariants; Callback: TCallback2;
      CallbackE: TCallbackE; ResultType: PTypeInfo;
      ByRef: Boolean; ResultMode: TResultMode; Simple: Boolean); overload;
  end;

{ TType }

{$IFDEF Supports_Generics}
class function TType.New<T>: Variant;
begin
  Result := ObjToVar(Self.Create(TypeInfo(T)));
end;
{$ENDIF}

constructor TType.Create(Info: PTypeInfo);
begin
  FTypeInfo := Info;
end;

function TypeToVar(Info: PTypeInfo): Variant;
begin
  Result := ObjToVar(TType.Create(Info));
end;

function CallbackToVar: Variant;
begin
  Result := ObjToVar(TCallback.Create);
end;

function CallbackToVar(Callback: TCallback1): Variant;
begin
  Result := ObjToVar(TCallback.Create(Callback));
end;

function CallbackToVar(Callback: TCallback2): Variant;
begin
  Result := ObjToVar(TCallback.Create(Callback));
end;

function CallbackToVar(CallbackE: TCallbackE): Variant;
begin
  Result := ObjToVar(TCallback.Create(CallbackE));
end;

{ TCallback }

constructor TCallback.Create;
begin
  inherited Create;
  FCallback1 := nil;
  FCallback2 := nil;
  FCallbackE := nil;
end;

constructor TCallback.Create(Callback: TCallback1);
begin
  inherited Create;
  FCallback1 := Callback;
  FCallback2 := nil;
  FCallbackE := nil;
end;

constructor TCallback.Create(Callback: TCallback2);
begin
  inherited Create;
  FCallback1 := nil;
  FCallback2 := Callback;
  FCallbackE := nil;
end;

constructor TCallback.Create(CallbackE: TCallbackE);
begin
  inherited Create;
  FCallback1 := nil;
  FCallback2 := nil;
  FCallbackE := CallbackE;
end;

{ THproseClient }

constructor THproseClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUri := '';
  FNameSpace := '';
  FCallbackE := nil;
  FFilters := THashedList.Create;
end;

class function THproseClient.New(const AUrl: string; const ANameSpace: string): Variant;
begin
  Result := Self.Create(nil).UseService(AUrl, ANameSpace);
end;

function THproseClient.GetFilter: IFilter;
begin
  if FFilters.Count = 0 then
    Result := nil
  else
    VarToIntf(FFilters[0], IFilter, Result);
end;

procedure THproseClient.SetFilter(const Filter: IFilter);
begin
  if FFilters.Count > 0 then FFilters.Clear;
  if Filter <> nil then FFilters.Add(Filter);
end;

procedure THproseClient.AddFilter(const Filter: IFilter);
begin
  if Filter <> nil then FFilters.Add(Filter);
end;

function THproseClient.RemoveFilter(const Filter: IFilter): Boolean;
begin
  Result := (FFilters.Remove(Filter) >= 0);
end;

function THproseClient.DoInput(var Args: TVariants; ResultType: PTypeInfo;
  ResultMode: TResultMode; Data: TBytes): Variant;
var
  I: Integer;
  AFilter: IFilter;
  ATag: Byte;
  InStream: TBytesStream;
  HproseReader: THproseReader;
begin
  for I := FFilters.Count - 1 downto 0 do begin
    VarToIntf(FFilters[I], IFilter, AFilter);
    Data := AFilter.InputFilter(Data, nil);
  end;
  if Data[Length(Data) - 1] <> HproseTagEnd then
    raise Exception.Create('Wrong Response: ' + #13#10 + StringOf(Data));
  Result := Null;
  if (ResultMode = RawWithEndTag) or
    (ResultMode = Raw) then begin
    if ResultMode = Raw then SetLength(Data, Length(Data) - 1);
    Result := Data;
  end
  else begin
    InStream := TBytesStream.Create(Data);
    HproseReader := THproseReader.Create(InStream);
    try
      ATag := 0;
      while Data[Instream.Position] <> HproseTagEnd do begin
        InStream.ReadBuffer(ATag, 1);
        if ATag = HproseTagResult then begin
          if ResultMode = Serialized then
            Result := HproseReader.ReadRaw
          else begin
            HproseReader.Reset;
            Result := HproseReader.Unserialize(ResultType)
          end
        end
        else if ATag = HproseTagArgument then begin
          HproseReader.Reset;
          Args := HproseReader.ReadVariantArray;
        end
        else if ATag = HproseTagError then begin
          HproseReader.Reset;
          raise Exception.Create(HproseReader.ReadString());
        end
        else raise Exception.Create('Wrong Response: ' + #13#10 + StringOf(Data));
      end;
    finally
      HproseReader.Free;
      InStream.Free;
    end;
  end;
end;

function THproseClient.DoInput(ResultType: PTypeInfo;
  ResultMode: TResultMode; Data: TBytes): Variant;
var
  Args: TVariants;
begin
  SetLength(Args, 0);
  Result := DoInput(Args, ResultType, ResultMode, Data);
end;

{$IFDEF Supports_Generics}
procedure THproseClient.DoInput(var Args: TVariants; ResultType: PTypeInfo;
      Data: TBytes; out Result);
var
  I: Integer;
  Filter: IFilter;
  Tag: Byte;
  InStream: TBytesStream;
  HproseReader: THproseReader;
begin
  for I := FFilters.Count - 1 downto 0 do begin
    VarToIntf(FFilters[I], IFilter, Filter);
    Data := Filter.InputFilter(Data, nil);
  end;
  if Data[Length(Data) - 1] <> HproseTagEnd then
    raise Exception.Create('Wrong Response: ' + #13#10 + StringOf(Data));
  InStream := TBytesStream.Create(Data);
  HproseReader := THproseReader.Create(InStream);
  try
    while Data[Instream.Position] <> HproseTagEnd do begin
      InStream.ReadBuffer(Tag, 1);
      if Tag = HproseTagResult then begin
        HproseReader.Reset;
        HproseReader.Unserialize(ResultType, Result);
      end
      else if Tag = HproseTagArgument then begin
        HproseReader.Reset;
        Args := HproseReader.ReadVariantArray;
      end
      else if Tag = HproseTagError then begin
        HproseReader.Reset;
        raise Exception.Create(HproseReader.ReadString());
      end
      else raise Exception.Create('Wrong Response: ' + #13#10 + StringOf(Data));
    end;
  finally
    HproseReader.Free;
    InStream.Free;
  end;
end;

procedure THproseClient.DoInput(ResultType: PTypeInfo;
  Data: TBytes; out Result);
var
  Args: TVariants;
begin
  DoInput(Args, ResultType, Data, Result);
end;
{$ENDIF}

function THproseClient.DoOutput(const AName: string;
  const Args: array of const; Simple: Boolean): TBytes;
var
  I: Integer;
  AFilter: IFilter;
  OutStream: TBytesStream;
  HproseWriter: THproseWriter;
begin
  OutStream := TBytesStream.Create;
  HproseWriter := THproseWriter.Create(OutStream, Simple);
  try
    OutStream.Write(HproseTagCall, 1);
    HproseWriter.WriteString(AName);
    if Length(Args) > 0 then begin
      HproseWriter.Reset;
      HproseWriter.WriteArray(Args);
    end;
    OutStream.Write(HproseTagEnd, 1);
    Result := OutStream.Bytes;
    SetLength(Result, OutStream.Size);
  finally
    HproseWriter.Free;
    OutStream.Free;
  end;
  for I := 0 to FFilters.Count - 1 do begin
    VarToIntf(FFilters[I], IFilter, AFilter);
    Result := AFilter.OutputFilter(Result, nil);
  end;
end;

function THproseClient.DoOutput(const AName: string;
  const Args: TVariants; ByRef: Boolean; Simple: Boolean): TBytes;
var
  I: Integer;
  AFilter: IFilter;
  OutStream: TBytesStream;
  HproseWriter: THproseWriter;
begin
  OutStream := TBytesStream.Create;
  HproseWriter := THproseWriter.Create(OutStream, Simple);
  try
    OutStream.Write(HproseTagCall, 1);
    HproseWriter.WriteString(AName);
    if (Length(Args) > 0) or ByRef then begin
      HproseWriter.Reset;
      HproseWriter.WriteArray(Args);
      if ByRef then HproseWriter.WriteBoolean(True);
    end;
    OutStream.Write(HproseTagEnd, 1);
    Result := OutStream.Bytes;
    SetLength(Result, OutStream.Size);
  finally
    HproseWriter.Free;
    OutStream.Free;
  end;
  for I := 0 to FFilters.Count - 1 do begin
    VarToIntf(FFilters[I], IFilter, AFilter);
    Result := AFilter.OutputFilter(Result, nil);
  end;
end;

function THproseClient.Invoke(const AName: string; const Arguments: TVarDataArray): Variant;
var
  Async: Boolean;
  Callback: TCallback;
  Callback1: TCallback1;
  Callback2: TCallback2;
  CallbackE: TCallbackE;
  HType: TType;
  Info: PTypeInfo;
  Args: TVariants;
  Len: Integer;
begin
  Async := False;
  Callback := nil;
  Callback1 := nil;
  Callback2 := nil;
  CallbackE := nil;
  HType := nil;
  Info := nil;
  Args := TVariants(Arguments);
  Len := Length(Args);
  while (Len > 0) and VarToObj(Args[Len - 1], TType, HType) and Assigned(HType) do begin
    if Assigned(HType.FTypeInfo) then Info := HType.FTypeInfo;
    HType.Free;
    Dec(Len);
    SetLength(Args, Len);
  end;
  while (Len > 0) and VarToObj(Args[Len - 1], TCallback, Callback) and Assigned(Callback) do begin
    Async := True;
    if Assigned(Callback.FCallback1) then Callback1 := Callback.FCallback1;
    if Assigned(Callback.FCallback2) then Callback2 := Callback.FCallback2;
    if Assigned(Callback.FCallbackE) then CallbackE := Callback.FCallbackE;
    Callback.Free;
    Dec(Len);
    SetLength(Args, Len);
  end;
  if Async then
      if Assigned(Callback1) then
        Invoke(AName, Args, Callback1, CallbackE, Info, Normal, False)
      else
        Invoke(AName, Args, Callback2, CallbackE, Info, True, Normal, False)
  else
    Result := Invoke(AName, Args, Info, False, Normal, False);
end;

// Synchronous invoke
function THproseClient.Invoke(const AName: string;
  ResultMode: TResultMode): Variant;
begin
  Result := Invoke(AName, [], PTypeInfo(nil), ResultMode, True);
end;

function THproseClient.Invoke(const AName: string;
  ResultType: PTypeInfo): Variant;
begin
  Result := Invoke(AName, [], ResultType, Normal, True);
end;

function THproseClient.Invoke(const AName: string;
  const Args: array of const; ResultMode: TResultMode; Simple: Boolean): Variant;
begin
  Result := Invoke(AName, Args, PTypeInfo(nil), ResultMode, Simple);
end;

function THproseClient.Invoke(const AName: string;
  const Args: array of const; ResultType: PTypeInfo; Simple: Boolean): Variant;
begin
  Result := Invoke(AName, Args, ResultType, Normal, Simple);
end;

function THproseClient.Invoke(const AName: string;
  const Args: array of const; ResultType: PTypeInfo;
  ResultMode: TResultMode; Simple: Boolean): Variant;
begin
  Result := DoInput(ResultType, ResultMode, SendAndReceive(DoOutput(AName, Args, Simple)));
end;

// Synchronous invoke

function THproseClient.Invoke(const AName: string; var Args: TVariants;
  ByRef: Boolean; ResultMode: TResultMode; Simple: Boolean): Variant;
begin
  Result := Invoke(AName, Args, PTypeInfo(nil), ByRef, ResultMode, Simple);
end;

function THproseClient.Invoke(const AName: string; var Args: TVariants;
  ResultType: PTypeInfo; ByRef: Boolean; Simple: Boolean): Variant;
begin
  Result := Invoke(AName, Args, ResultType, ByRef, Normal, Simple);
end;

function THproseClient.Invoke(const AName: string; var Args: TVariants;
  ResultType: PTypeInfo; ByRef: Boolean;
  ResultMode: TResultMode; Simple: Boolean): Variant;
var
  FullName: string;
begin
  if FNameSpace <> '' then
    FullName := FNameSpace + '_' + AName
  else
    FullName := AName;
  Result := DoInput(Args, ResultType, ResultMode, SendAndReceive(DoOutput(FullName, Args, ByRef, Simple)));
end;

{$IFDEF Supports_Generics}
// Synchronous invoke
function THproseClient.Invoke<T>(const AName: string): T;
var
  Args: array of TVarRec;
begin
  SetLength(Args, 0);
  Result := Self.Invoke<T>(AName, Args, True);
end;

function THproseClient.Invoke<T>(const AName: string;
  const Args: array of const; Simple: Boolean): T;
var
  InStream, OutStream: TStream;
  FullName: string;
begin
  if FNameSpace <> '' then
    FullName := FNameSpace + '_' + AName
  else
    FullName := AName;
  Result := Default(T);
  DoInput(TypeInfo(T), SendAndReceive(DoOutput(FullName, Args, Simple)), Result);
end;

function THproseClient.Invoke<T>(const AName: string; var Args: TVariants;
  ByRef: Boolean; Simple: Boolean): T;
var
  InStream, OutStream: TStream;
  FullName: string;
begin
  if FNameSpace <> '' then
    FullName := FNameSpace + '_' + AName
  else
    FullName := AName;
  Result := Default(T);
  DoInput(Args, TypeInfo(T), SendAndReceive(DoOutput(FullName, Args, ByRef, Simple)), Result);
end;
{$ENDIF}

// Asynchronous invoke
procedure THproseClient.Invoke(const AName: string;
  Callback: TCallback1;
  ResultMode: TResultMode);
var
  Args: array of TVarRec;
  CallbackE: TCallbackE;
begin
  SetLength(Args, 0);
  CallbackE := nil;
  Invoke(AName, Args, Callback, CallbackE, nil, ResultMode, True);
end;

procedure THproseClient.Invoke(const AName: string;
  Callback: TCallback1;
  CallbackE: TCallbackE;
  ResultMode: TResultMode);
var
  Args: array of TVarRec;
begin
  SetLength(Args, 0);
  Invoke(AName, Args, Callback, CallbackE, nil, ResultMode, True);
end;

// Asynchronous invoke
procedure THproseClient.Invoke(const AName: string;
  Callback: TCallback1;
  ResultType: PTypeInfo);
var
  Args: array of TVarRec;
  CallbackE: TCallbackE;
begin
  SetLength(Args, 0);
  CallbackE := nil;
  Invoke(AName, Args, Callback, CallbackE, ResultType, Normal, True);
end;

procedure THproseClient.Invoke(const AName: string;
  Callback: TCallback1;
  CallbackE: TCallbackE;
  ResultType: PTypeInfo);
var
  Args: array of TVarRec;
begin
  SetLength(Args, 0);
  Invoke(AName, Args, Callback, CallbackE, ResultType, Normal, True);
end;

// Asynchronous invoke
procedure THproseClient.Invoke(const AName: string; const Args: array of const;
  Callback: TCallback1;
  ResultMode: TResultMode; Simple: Boolean);
var
  CallbackE: TCallbackE;
begin
  CallbackE := nil;
  Invoke(AName, Args, Callback, CallbackE, nil, ResultMode, Simple);
end;

procedure THproseClient.Invoke(const AName: string; const Args: array of const;
  Callback: TCallback1;
  CallbackE: TCallbackE;
  ResultMode: TResultMode; Simple: Boolean);
begin
  Invoke(AName, Args, Callback, CallbackE, nil, ResultMode, Simple);
end;

// Asynchronous invoke
procedure THproseClient.Invoke(const AName: string; const Args: array of const;
  Callback: TCallback1;
  ResultType: PTypeInfo; Simple: Boolean);
var
  CallbackE: TCallbackE;
begin
  CallbackE := nil;
  Invoke(AName, Args, Callback, CallbackE, ResultType, Normal, Simple);
end;

procedure THproseClient.Invoke(const AName: string; const Args: array of const;
  Callback: TCallback1;
  CallbackE: TCallbackE;
  ResultType: PTypeInfo; Simple: Boolean);
begin
  Invoke(AName, Args, Callback, CallbackE, ResultType, Normal, Simple);
end;

procedure THproseClient.Invoke(const AName: string; const Args: array of const;
  Callback: TCallback1;
  CallbackE: TCallbackE;
  ResultType: PTypeInfo; ResultMode: TResultMode; Simple: Boolean);
var
  FullName: string;
begin
  if FNameSpace <> '' then
    FullName := FNameSpace + '_' + AName
  else
    FullName := AName;
  TAsyncInvokeThread1.Create(Self, FullName, Args, Callback, CallbackE, ResultType, ResultMode, Simple)
end;

// Asynchronous invoke
procedure THproseClient.Invoke(const AName: string; var Args: TVariants;
  Callback: TCallback1;
  ResultMode: TResultMode; Simple: Boolean);
var
  CallbackE: TCallbackE;
begin
  CallbackE := nil;
  Invoke(AName, Args, Callback, CallbackE, nil, ResultMode, Simple);
end;

procedure THproseClient.Invoke(const AName: string; var Args: TVariants;
  Callback: TCallback1;
  CallbackE: TCallbackE;
  ResultMode: TResultMode; Simple: Boolean);
begin
  Invoke(AName, Args, Callback, CallbackE, nil, ResultMode, Simple);
end;

// Asynchronous invoke
procedure THproseClient.Invoke(const AName: string; var Args: TVariants;
  Callback: TCallback1;
  ResultType: PTypeInfo; Simple: Boolean);
var
  CallbackE: TCallbackE;
begin
  CallbackE := nil;
  Invoke(AName, Args, Callback, CallbackE, ResultType, Normal, Simple);
end;

procedure THproseClient.Invoke(const AName: string; var Args: TVariants;
  Callback: TCallback1;
  CallbackE: TCallbackE;
  ResultType: PTypeInfo; Simple: Boolean);
begin
  Invoke(AName, Args, Callback, CallbackE, ResultType, Normal, Simple);
end;

procedure THproseClient.Invoke(const AName: string; var Args: TVariants;
  Callback: TCallback1;
  CallbackE: TCallbackE;
  ResultType: PTypeInfo;
  ResultMode: TResultMode; Simple: Boolean);
var
  FullName: string;
begin
  if FNameSpace <> '' then
    FullName := FNameSpace + '_' + AName
  else
    FullName := AName;
  TAsyncInvokeThread2.Create(Self, FullName, Args, Callback, CallbackE, ResultType, False, ResultMode, Simple)
end;

// Asynchronous invoke
procedure THproseClient.Invoke(const AName: string; var Args: TVariants;
  Callback: TCallback2;
  ByRef: Boolean;
  ResultMode: TResultMode; Simple: Boolean);
var
  CallbackE: TCallbackE;
begin
  CallbackE := nil;
  Invoke(AName, Args, Callback, CallbackE, nil, ByRef, ResultMode, Simple);
end;

procedure THproseClient.Invoke(const AName: string; var Args: TVariants;
  Callback: TCallback2;
  CallbackE: TCallbackE;
  ByRef: Boolean;
  ResultMode: TResultMode; Simple: Boolean);
begin
  Invoke(AName, Args, Callback, CallbackE, nil, ByRef, ResultMode, Simple);
end;

// Asynchronous invoke
procedure THproseClient.Invoke(const AName: string; var Args: TVariants;
  Callback: TCallback2;
  ResultType: PTypeInfo;
  ByRef: Boolean; Simple: Boolean);
var
  CallbackE: TCallbackE;
begin
  CallbackE := nil;
  Invoke(AName, Args, Callback, CallbackE, ResultType, ByRef, Normal, Simple);
end;

procedure THproseClient.Invoke(const AName: string; var Args: TVariants;
  Callback: TCallback2;
  CallbackE: TCallbackE;
  ResultType: PTypeInfo;
  ByRef: Boolean; Simple: Boolean);
begin
  Invoke(AName, Args, Callback, CallbackE, ResultType, ByRef, Normal, Simple);
end;

procedure THproseClient.Invoke(const AName: string; var Args: TVariants;
  Callback: TCallback2;
  CallbackE: TCallbackE;
  ResultType: PTypeInfo;
  ByRef: Boolean; ResultMode: TResultMode; Simple: Boolean);
var
  FullName: string;
begin
  if FNameSpace <> '' then
    FullName := FNameSpace + '_' + AName
  else
    FullName := AName;
  TAsyncInvokeThread2.Create(Self, FullName, Args, Callback, CallbackE, ResultType, ByRef, ResultMode, Simple)
end;

{$IFDEF Supports_Generics}
procedure THproseClient.Invoke<T>(const AName: string;
  Callback: TCallback1<T>;
  CallbackE: TCallbackE);
begin
  Self.Invoke<T>(AName, [], Callback, CallbackE, True);
end;

procedure THproseClient.Invoke<T>(const AName: string; const Args: array of const;
  Callback: TCallback1<T>;
  CallbackE: TCallbackE; Simple: Boolean);
var
  FullName: string;
begin
  if FNameSpace <> '' then
    FullName := FNameSpace + '_' + AName
  else
    FullName := AName;
  TAsyncInvokeThread1<T>.Create(Self, FullName, Args, Callback, CallbackE, Simple)
end;

procedure THproseClient.Invoke<T>(const AName: string; var Args: TVariants;
  Callback: TCallback2<T>;
  ByRef: Boolean; Simple: Boolean);
begin
  Self.Invoke<T>(AName, Args, Callback, nil, ByRef, Simple);
end;

procedure THproseClient.Invoke<T>(const AName: string; var Args: TVariants;
  Callback: TCallback2<T>;
  CallbackE: TCallbackE;
  ByRef: Boolean; Simple: Boolean);
var
  FullName: string;
begin
  if FNameSpace <> '' then
    FullName := FNameSpace + '_' + AName
  else
    FullName := AName;
  TAsyncInvokeThread2<T>.Create(Self, FullName, Args, Callback, CallbackE, ByRef, Simple)
end;
{$ENDIF}

function THproseClient.UseService(const AUri: string; const ANameSpace: string): Variant;
begin
  if AUri <> '' then FUri := AUri;
  FNameSpace := ANameSpace;
  Result := ObjToVar(Self);
end;

{ TAsyncInvokeThread1 }

constructor TAsyncInvokeThread1.Create(Client: THproseClient;
  const AName: string; const Args: array of const;
  Callback: TCallback1; CallbackE: TCallbackE;
  ResultType: PTypeInfo; ResultMode: TResultMode; Simple: Boolean);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FClient := Client;
  FName := AName;
  FArgs := CreateConstArray(Args);
  FCallback := Callback;
  FCallbackE := CallbackE;
  FResultType := ResultType;
  FResultMode := ResultMode;
  FSimple := Simple;
  FError := nil;
end;

procedure TAsyncInvokeThread1.DoCallback;
begin
  if not Assigned(FError) and Assigned(FCallback) then FCallback(FResult);
end;

procedure TAsyncInvokeThread1.DoError;
begin
  if Assigned(FCallbackE) then
    FCallbackE(FName, FError)
  else if Assigned(FClient.FCallbackE) then
    FClient.FCallbackE(FName, FError);
end;

procedure TAsyncInvokeThread1.Execute;
begin
  try
    try
      FResult := FClient.Invoke(FName,
                                FArgs,
                                FResultType,
                                FResultMode,
                                FSimple);
    except
      on E: Exception do begin
        FError := E;
        Synchronize({$IFDEF FPC}@{$ENDIF}DoError);
      end;
    end;
  finally
    FinalizeConstArray(FArgs);
  end;
  Synchronize({$IFDEF FPC}@{$ENDIF}DoCallback);
end;

{ TAsyncInvokeThread2 }

constructor TAsyncInvokeThread2.Create(Client: THproseClient;
  const AName: string; const Args: TVariants;
  Callback: TCallback1; CallbackE: TCallbackE;
  ResultType: PTypeInfo; ByRef: Boolean;
  ResultMode: TResultMode; Simple: Boolean);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FClient := Client;
  FName := AName;
  FArgs := Args;
  FCallback1 := Callback;
  FCallback2 := nil;
  FCallbackE := CallbackE;
  FResultType := ResultType;
  FByRef := ByRef;
  FResultMode := ResultMode;
  FSimple := Simple;
  FError := nil;
end;

constructor TAsyncInvokeThread2.Create(Client: THproseClient;
  const AName: string; const Args: TVariants;
  Callback: TCallback2; CallbackE: TCallbackE;
  ResultType: PTypeInfo; ByRef: Boolean;
  ResultMode: TResultMode; Simple: Boolean);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FClient := Client;
  FName := AName;
  FArgs := Args;
  FCallback1 := nil;
  FCallback2 := Callback;
  FCallbackE := CallbackE;
  FResultType := ResultType;
  FByRef := ByRef;
  FResultMode := ResultMode;
  FSimple := Simple;
  FError := nil;
end;

procedure TAsyncInvokeThread2.DoCallback;
begin
  if not Assigned(FError) then
    if Assigned(FCallback1) then FCallback1(FResult)
    else if Assigned(FCallback2) then FCallback2(FResult, FArgs);
end;

procedure TAsyncInvokeThread2.DoError;
begin
  if Assigned(FCallbackE) then
    FCallbackE(FName, FError)
  else if Assigned(FClient.FCallbackE) then
    FClient.FCallbackE(FName, FError);
end;

procedure TAsyncInvokeThread2.Execute;
begin
  try
    FResult := FClient.Invoke(FName,
                              FArgs,
                              FResultType,
                              FByRef,
                              FResultMode,
                              FSimple);
  except
    on E: Exception do begin
      FError := E;
      Synchronize({$IFDEF FPC}@{$ENDIF}DoError);
    end;
  end;
  Synchronize({$IFDEF FPC}@{$ENDIF}DoCallback);
end;

{$IFDEF Supports_Generics}
{ TAsyncInvokeThread1<T> }

constructor TAsyncInvokeThread1<T>.Create(Client: THproseClient;
  const AName: string; const Args: array of const;
  Callback: TCallback1<T>; CallbackE: TCallbackE; Simple: Boolean);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FClient := Client;
  FName := AName;
  FArgs := CreateConstArray(Args);
  FCallback := Callback;
  FCallbackE := CallbackE;
  FSimple := Simple;
  FError := nil;
end;

procedure TAsyncInvokeThread1<T>.DoCallback;
begin
  if not Assigned(FError) and Assigned(FCallback) then FCallback(FResult);
end;

procedure TAsyncInvokeThread1<T>.DoError;
begin
  if Assigned(FCallbackE) then
    FCallbackE(FName, FError)
  else if Assigned(FClient.FCallbackE) then
    FClient.FCallbackE(FName, FError);
end;

procedure TAsyncInvokeThread1<T>.Execute;
begin
  try
    try
      FResult := FClient.Invoke<T>(FName, FArgs, FSimple);
    except
      on E: Exception do begin
        FError := E;
        Synchronize(DoError);
      end;
    end;
  finally
    FinalizeConstArray(FArgs);
  end;
  Synchronize(DoCallback);
end;

{ TAsyncInvokeThread2<T> }

constructor TAsyncInvokeThread2<T>.Create(Client: THproseClient;
  const AName: string; const Args: TVariants;
  Callback: TCallback2<T>; CallbackE: TCallbackE;
  ByRef: Boolean; Simple: Boolean);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FClient := Client;
  FName := AName;
  FArgs := Args;
  FCallback := Callback;
  FCallbackE := CallbackE;
  FByRef := ByRef;
  FSimple := Simple;
  FError := nil;
end;

procedure TAsyncInvokeThread2<T>.DoCallback;
begin
  if not Assigned(FError) and Assigned(FCallback) then FCallback(FResult, FArgs);
end;

procedure TAsyncInvokeThread2<T>.DoError;
begin
  if Assigned(FCallbackE) then
    FCallbackE(FName, FError)
  else if Assigned(FClient.FCallbackE) then
    FClient.FCallbackE(FName, FError);
end;

procedure TAsyncInvokeThread2<T>.Execute;
begin
  try
    FResult := FClient.Invoke<T>(FName, FArgs, FByRef, FSimple);
  except
    on E: Exception do begin
      FError := E;
      Synchronize(DoError);
    end;
  end;
  Synchronize(DoCallback);
end;

{$ENDIF}

end.
