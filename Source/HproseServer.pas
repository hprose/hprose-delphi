unit HproseServer;

{$I Hprose.inc}

interface

uses
  Classes, SysUtils, Variants, HproseCommon, HproseIO;

type

  THproseServer = class;

  TResultMode = (rmNormal, rmSerialized, rmRaw, rmRawWithEndTag);
  TMethodType = (mtProc, mtFunc, mtProcArgs, mtFuncArgs);

  TInvokeFunction00 = procedure;
  TInvokeFunction01 = function: Variant;
  TInvokeFunction10 = procedure(Args: Variant);
  TInvokeFunction11 = function(Args: Variant): Variant;

  TInvokeMethod00 = procedure of object;
  TInvokeMethod01 = function: Variant of object;
  TInvokeMethod10 = procedure(Args: Variant) of object;
  TInvokeMethod11 = function(Args: Variant): Variant of object;

  TBeforeInvokeEvent = procedure(Name: string; Args: Variant; ByRef: Boolean) of object;
  TAfterInvokeEvent = procedure(Name: string; Args: Variant; ByRef: Boolean; Result: Variant) of object;

  TErrorEvent = procedure(Error: Exception) of object;

  THproseMethod = class
  private
    FMethod:     TMethod;
    FMethodType: TMethodType;
  public
    constructor Create(Method: TMethod; MethodType: TMethodType);
    function Invoke(Args: Variant): Variant;
  end;

  THproseServer = class(TComponent)
  private
    FMethods: THashMap;
    FMethodNames: THashMap;
    FResultMode: THashMap;
    FDebug: Boolean;
    FOnBeforeInvoke: TBeforeInvokeEvent;
    FOnAfterInvoke: TAfterInvokeEvent;
    FOnSendError: TErrorEvent;
    procedure AddMethod(Method: TMethod; MethodType: TMethodType; AliasName: string; ResultMode: TResultMode);overload;
  protected
    procedure DoFunctionList(OutStream: TStream);
    procedure DoInvoke(InStream: TStream; OutStream: TStream);
    procedure HandleCommand(InStream: TStream; OutStream: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddFunction(Func: Pointer; MethodType: TMethodType; FuncName: string; ResultMode: TResultMode = rmNormal);
    procedure AddMethod(Cls: TClass; MethodName: string; MethodType: TMethodType; AliasName: string = ''; ResultMode: TResultMode = rmNormal); overload;
    procedure AddMethod(Obj: TObject; MethodName: string; MethodType: TMethodType; AliasName: string = ''; ResultMode: TResultMode = rmNormal); overload;
  published
    property DebugEnabled: Boolean read FDebug write FDebug default False;
    property OnBeforeInvoke: TBeforeInvokeEvent read FOnBeforeInvoke write FOnBeforeInvoke;
    property OnAfterInvoke: TAfterInvokeEvent read FOnAfterInvoke write FOnAfterInvoke;
    property OnSendError: TErrorEvent read FOnSendError write FOnSendError;
  end;

implementation

uses
  TypInfo;

{ THproseMethod }

constructor THproseMethod.Create(Method: TMethod; MethodType: TMethodType);
begin
  FMethod := Method;
  FMethodType := MethodType;
end;

function THproseMethod.Invoke(Args: Variant): Variant;
begin
  if FMethod.Data = nil then
    case FMethodType of
      mtProc:
        TInvokeFunction00(FMethod.Code)();
      mtFunc:
        Result := TInvokeFunction01(FMethod.Code)();
      mtProcArgs:
        TInvokeFunction10(FMethod.Code)(Args);
      mtFuncArgs:
        Result := TInvokeFunction11(FMethod.Code)(Args);
    end
  else
    case FMethodType of
      mtProc:
        TInvokeMethod00(FMethod)();
      mtFunc:
        Result := TInvokeMethod01(FMethod)();
      mtProcArgs:
        TInvokeMethod10(FMethod)(Args);
      mtFuncArgs:
        Result := TInvokeMethod11(FMethod)(Args);
    end;
end;

{ THproseServer }

constructor THproseServer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMethods := TCaseInsensitiveHashedMap.Create;
  FMethodNames := TCaseInsensitiveHashedMap.Create;
  FResultMode := TCaseInsensitiveHashedMap.Create;
end;

destructor THproseServer.Destroy;
var
  Enum: IListEnumerator;
begin
  Enum := FMethods.Values.GetEnumerator;
  while Enum.MoveNext do
    THproseMethod(Integer(Enum.Current)).Free;
  FMethods.Free;
  FMethodNames.Free;
  FResultMode.Free;
  inherited;
end;

procedure THproseServer.AddMethod(Method: TMethod; MethodType: TMethodType;
  AliasName: string; ResultMode: TResultMode);
begin
  FMethods[AliasName] := Integer(THproseMethod.Create(Method, MethodType));
  //FMethodNames[AliasName] := MethodName;
  FResultMode[AliasName] := ResultMode;
end;

procedure THproseServer.AddFunction(Func: Pointer; MethodType: TMethodType;
  FuncName: string; ResultMode: TResultMode);
var
  Method: TMethod;
begin
  if Func <> nil then
  begin
    Method.Code := Func;
    Method.Data := nil;
    AddMethod(Method, MethodType, FuncName, ResultMode);
  end;
end;

procedure THproseServer.AddMethod(Cls: TClass; MethodName: string;
  MethodType: TMethodType; AliasName: string; ResultMode: TResultMode);
var
  Method: TMethod;
begin
  Method.Code := Cls.MethodAddress(MethodName);
  if Method.Code <> nil then
  begin
    Method.Data := Cls;
    if AliasName = '' then
      AliasName := MethodName;
    AddMethod(Method, MethodType, AliasName, ResultMode);
  end;

end;

procedure THproseServer.AddMethod(Obj: TObject; MethodName: string;
  MethodType: TMethodType; AliasName: string; ResultMode: TResultMode);
var
  Method: TMethod;
begin
  Method.Code := Obj.MethodAddress(MethodName);
  if Method.Code <> nil then
  begin
    Method.Data := Obj;
    if AliasName = '' then
      AliasName := MethodName;
    AddMethod(Method, MethodType, AliasName, ResultMode);
  end;
end;

procedure THproseServer.DoFunctionList(OutStream: TStream);
var
  Writer: THproseWriter;
begin
  Writer := THproseWriter.Create(OutStream);
  try
    OutStream.Write(HproseTagFunctions, 1);
    Writer.Serialize(FMethodS.Keys);
    OutStream.Write(HproseTagEnd, 1);
  finally
    Writer.Free;
  end;
end;

procedure THproseServer.DoInvoke(InStream: TStream; OutStream: TStream);
var
  Reader: THproseReader;
  Writer: THproseWriter;
  Tag: AnsiChar;
  MethodName: string;
  Method: THproseMethod;
  ResultMode: TResultMode;
  Args, Result: Variant;
  ByRef: Boolean;
begin
  Reader := THproseReader.Create(InStream);
  Writer := THproseWriter.Create(OutStream);
  try
    repeat
      Reader.Reset;
      ByRef := False;
      MethodName := Reader.ReadString;
      Tag := Reader.CheckTags(HproseTagList + HproseTagEnd + HproseTagCall);
      if Tag = HproseTagList then
      begin
        Reader.Reset;
        Args := Reader.ReadList(varVariant, nil, False);
        Tag := Reader.CheckTags(HproseTagTrue + HproseTagEnd + HproseTagCall);
        if Tag = HproseTagTrue then
        begin
          ByRef := True;
          Tag := Reader.CheckTags(HproseTagEnd + HproseTagCall);
        end;
      end;
      if FMethods.ContainsKey(MethodName) then
      begin
        Method := THproseMethod(Integer(FMethods[MethodName]));
        ResultMode := TResultMode(FResultMode[MethodName]);
        if Assigned(FOnBeforeInvoke) then
          FOnBeforeInvoke(Name, Args, ByRef);
        Result := Method.Invoke(Args);
        if Assigned(FOnAfterInvoke) then
          FOnAfterInvoke(Name, Args, ByRef, Result);
        if ResultMode = rmRawWithEndTag then
        begin
          OutStream.Write(AnsiString(Result)[1], Length(AnsiString(Result)));
          Exit;
        end
        else if ResultMode = rmRaw then
          OutStream.Write(AnsiString(Result)[1], Length(AnsiString(Result)))
        else
        begin
          OutStream.Write(HproseTagResult, 1);
          if ResultMode = rmSerialized then
            OutStream.Write(AnsiString(Result)[1], Length(AnsiString(Result)))
          else
          begin
            Writer.Reset;
            Writer.Serialize(Result);
          end;
          if ByRef then
          begin
            OutStream.write(HproseTagArgument, 1);
            Writer.Reset;
            Writer.Serialize(Args);
          end;
        end;
        OutStream.Write(HproseTagEnd, 1);
      end
      else
        raise EHproseException.Create(Format('Can''''t find this function %s.', [MethodName]));
    until (Tag <> HproseTagCall);
  except
    On E: Exception do
    begin
      if Assigned(OnSendError) then
        OnSendError(E);
      OutStream.Write(HproseTagError, 1);
      if FDebug then
        Writer.WriteString(E.ToString, False)
      else
        Writer.WriteString(E.Message, False);
      OutStream.Write(HproseTagEnd, 1)
    end;
  end;
  Reader.Free;
  Writer.Free;
end;

procedure THproseServer.HandleCommand(InStream: TStream; OutStream: TStream);
var
  Reader: THproseReader;
  Tag: AnsiChar;
begin
  Reader := THproseReader.Create(InStream);
  Tag := Reader.CheckTags(HproseTagCall + HproseTagEnd);
  if Tag = HproseTagEnd then
    DoFunctionList(OutStream)
  else
    DoInvoke(InStream, OutStream);
  Reader.Free;
end;

end.

