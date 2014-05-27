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
 * HproseIO.pas                                           *
 *                                                        *
 * hprose io unit for delphi.                             *
 *                                                        *
 * LastModified: May 26, 2014                             *
 * Author: Ma Bingyao <andot@hprose.com>                  *
 *                                                        *
\**********************************************************/
}
unit HproseIO;

{$I Hprose.inc}

interface

uses Classes, HproseCommon
{$IFDEF Supports_Generics}, Generics.Collections {$ENDIF}, TypInfo, SysUtils;

const
  { Hprose Serialize Tags }
  HproseTagInteger    :Byte = Byte('i');
  HproseTagLong       :Byte = Byte('l');
  HproseTagDouble     :Byte = Byte('d');
  HproseTagNull       :Byte = Byte('n');
  HproseTagEmpty      :Byte = Byte('e');
  HproseTagTrue       :Byte = Byte('t');
  HproseTagFalse      :Byte = Byte('f');
  HproseTagNaN        :Byte = Byte('N');
  HproseTagInfinity   :Byte = Byte('I');
  HproseTagDate       :Byte = Byte('D');
  HproseTagTime       :Byte = Byte('T');
  HproseTagUTC        :Byte = Byte('Z');
  HproseTagBytes      :Byte = Byte('b');
  HproseTagUTF8Char   :Byte = Byte('u');
  HproseTagString     :Byte = Byte('s');
  HproseTagGuid       :Byte = Byte('g');
  HproseTagList       :Byte = Byte('a');
  HproseTagMap        :Byte = Byte('m');
  HproseTagClass      :Byte = Byte('c');
  HproseTagObject     :Byte = Byte('o');
  HproseTagRef        :Byte = Byte('r');
  { Hprose Serialize Marks }
  HproseTagPos        :Byte = Byte('+');
  HproseTagNeg        :Byte = Byte('-');
  HproseTagSemicolon  :Byte = Byte(';');
  HproseTagOpenbrace  :Byte = Byte('{');
  HproseTagClosebrace :Byte = Byte('}');
  HproseTagQuote      :Byte = Byte('"');
  HproseTagPoint      :Byte = Byte('.');
  { Hprose Protocol Tags }
  HproseTagFunctions  :Byte = Byte('F');
  HproseTagCall       :Byte = Byte('C');
  HproseTagResult     :Byte = Byte('R');
  HproseTagArgument   :Byte = Byte('A');
  HproseTagError      :Byte = Byte('E');
  HproseTagEnd        :Byte = Byte('z');

type

  IReaderRefer = interface
  ['{22C0727C-A00B-4283-B4E1-BF1629116526}']
    function SetRef(const V: Variant): Integer; overload;
    procedure SetRef(I: Integer; const V: Variant); overload;
    function ReadRef(I: Integer): Variant;
    procedure Reset;
  end;

  THproseReader = class
  private
    FStream: TStream;
    FRefer: IReaderRefer;
    FClassRefList: IList;
    FAttrRefMap: IMap;
    function UnexpectedTag(Tag: Byte;
      const ExpectTags: string = ''): EHproseException;
    function TagToString(Tag: Byte): string;
    function ReadByte: Byte;
    function ReadInt64(Tag: Byte): Int64; overload;
{$IFDEF Supports_UInt64}
    function ReadUInt64(Tag: Byte): UInt64; overload;
{$ENDIF}
{$IFNDEF NEXTGEN}
    function ReadStringAsWideString: WideString;
{$ELSE}
    function ReadStringAsWideString: string;
{$ENDIF}
    function ReadBooleanArray(Count: Integer): Variant;
    function ReadShortIntArray(Count: Integer): Variant;
    function ReadByteArray(Count: Integer): Variant;
    function ReadSmallIntArray(Count: Integer): Variant;
    function ReadWordArray(Count: Integer): Variant;
    function ReadIntegerArray(Count: Integer): Variant;
    function ReadLongWordArray(Count: Integer): Variant;
    function ReadSingleArray(Count: Integer): Variant;
    function ReadDoubleArray(Count: Integer): Variant;
    function ReadCurrencyArray(Count: Integer): Variant;
    function ReadDateTimeArray(Count: Integer): Variant;
    function ReadWideStringArray(Count: Integer): Variant;
    function ReadVariantArray(Count: Integer): Variant; overload;
    function ReadInterfaceArray(Count: Integer): Variant;
    function ReadDynArrayWithoutTag(varType: Integer): Variant;
    function ReadIList(AClass: TClass): IList;
    function ReadList(AClass: TClass): TAbstractList;
    function ReadListAsIMap(AClass: TClass): IMap;
    function ReadListAsMap(AClass: TClass): TAbstractMap;
    function ReadIMap(AClass: TClass): IMap;
    function ReadMap(AClass: TClass): TAbstractMap;
    function ReadMapAsInterface(AClass: TClass; const IID: TGUID): IInterface;
    function ReadMapAsObject(AClass: TClass): TObject;
    function ReadObjectAsIMap(AClass: TMapClass): IMap;
    function ReadObjectAsMap(AClass: TMapClass): TAbstractMap;
    function ReadObjectAsInterface(AClass: TClass; const IID: TGUID): IInterface;
    function ReadObjectWithoutTag(AClass: TClass): TObject; overload;
    procedure ReadClass;
    function ReadRef: Variant;
{$IFDEF Supports_Generics}
    procedure ReadArray<T>(var DynArray: TArray<T>; Info: PTypeInfo); overload;
    procedure ReadArray(Info: PTypeInfo; out DynArray); overload;
    procedure ReadDynArray(Info: PTypeInfo; out DynArray); overload;
{$IFDEF Supports_Rtti}
    function ReadTList<T>(Info, ElementInfo: PTypeInfo): TList<T>; overload;
    function ReadTList(Info: PTypeInfo): TObject; overload;
    function ReadTQueue<T>(Info, ElementInfo: PTypeInfo): TQueue<T>; overload;
    function ReadTQueue(Info: PTypeInfo): TObject; overload;
    function ReadTStack<T>(Info, ElementInfo: PTypeInfo): TStack<T>; overload;
    function ReadTStack(Info: PTypeInfo): TObject; overload;
    function ReadTDictionary2<TKey, TValue>(
      Info, KeyInfo, ValueInfo: PTypeInfo): TDictionary<TKey, TValue>;
    function ReadTDictionary1<TKey>(Info, KeyInfo,
      ValueInfo: PTypeInfo): TObject;
    function ReadTDictionary(Info: PTypeInfo): TObject;
    function UnserializeTypeAsT<T>(Info: PTypeInfo): T;
{$ENDIF}
    function ReadSmartObject(Info: PTypeInfo): ISmartObject;
{$ENDIF}
    procedure ReadRaw(const OStream: TStream; Tag: Byte); overload;
    procedure ReadInfinityRaw(const OStream: TStream);
    procedure ReadNumberRaw(const OStream: TStream);
    procedure ReadDateTimeRaw(const OStream: TStream);
    procedure ReadUTF8CharRaw(const OStream: TStream);
    procedure ReadStringRaw(const OStream: TStream);
    procedure ReadBytesRaw(const OStream: TStream);
    procedure ReadGuidRaw(const OStream: TStream);
    procedure ReadComplexRaw(const OStream: TStream);
  public
    constructor Create(AStream: TStream; Simple: Boolean = False);
{$IFDEF BCB}
    constructor Create1(AStream: TStream);
{$ENDIF}
    procedure CheckTag(expectTag: Byte);
    function CheckTags(const expectTags: TBytes): Byte;
    function ReadUntil(Tag: Byte): string;
    function ReadInt(Tag: Byte): Integer;
    function ReadIntegerWithoutTag: Integer;
    function ReadLongWithoutTag: string;
    function ReadInfinityWithoutTag: Extended;
    function ReadDoubleWithoutTag: Extended;
    function ReadDateWithoutTag: TDateTime;
    function ReadTimeWithoutTag: TDateTime;
    function ReadUTF8CharWithoutTag: WideChar;
{$IFNDEF NEXTGEN}
    function ReadStringWithoutTag: WideString;
{$ELSE}
    function ReadStringWithoutTag: string;
{$ENDIF}
    function ReadBytesWithoutTag: Variant;
    function ReadGuidWithoutTag: string;
    function ReadListWithoutTag: Variant;
    function ReadMapWithoutTag: Variant;
    function ReadObjectWithoutTag: Variant; overload;
    function ReadInteger: Integer;
    function ReadInt64: Int64; overload;
{$IFDEF Supports_UInt64}
    function ReadUInt64: UInt64; overload;
{$ENDIF}
    function ReadExtended: Extended;
    function ReadCurrency: Currency;
    function ReadBoolean: Boolean;
    function ReadDateTime: TDateTime;
    function ReadUTF8Char: WideChar;
{$IFNDEF NEXTGEN}
    function ReadString: WideString;
{$ELSE}
    function ReadString: string;
{$ENDIF}
    function ReadBytes: Variant;
    function ReadGuid: string;
    function ReadDynArray(varType: Integer): Variant; overload;
    function ReadVariantArray: TVariants; overload;
    function ReadInterface(AClass: TClass; const IID: TGUID): IInterface;
    function ReadObject(AClass: TClass): TObject;
{$IFDEF Supports_Generics}
    procedure Unserialize(Info: PTypeInfo; out Value); overload;
    function Unserialize<T>: T; overload;
{$ENDIF}
    function Unserialize: Variant; overload;
    function Unserialize(Info: PTypeInfo): Variant; overload;
    function ReadRaw: TBytes; overload;
    procedure ReadRaw(const OStream: TStream); overload;
    procedure Reset;
    property Stream: TStream read FStream;
  end;

  IWriterRefer = interface
  ['{71734211-0FB5-483A-BF61-992CE616482C}']
    procedure AddCount(Count: Integer);
    procedure SetRef(const V: Variant);
    function WriteRef(const V:Variant): Boolean;
    procedure Reset;
  end;

  THproseWriter = class
  private
    FStream: TStream;
    FRefer: IWriterRefer;
    FClassRefList: IList;
    function WriteClass(const Instance: TObject): Integer;
    procedure WriteRawBytes(const Bytes: TBytes);
    procedure WriteShortIntArray(var P; Count: Integer);
    procedure WriteSmallIntArray(var P; Count: Integer);
    procedure WriteWordArray(var P; Count: Integer);
    procedure WriteIntegerArray(var P; Count: Integer);
    procedure WriteCurrencyArray(var P; Count: Integer);
    procedure WriteLongWordArray(var P; Count: Integer);
    procedure WriteSingleArray(var P; Count: Integer);
    procedure WriteDoubleArray(var P; Count: Integer);
    procedure WriteBooleanArray(var P; Count: Integer);
    procedure WriteWideStringArray(var P; Count: Integer);
    procedure WriteDateTimeArray(var P; Count: Integer);
    procedure WriteVariantArray(var P; Count: Integer);
{$IFNDEF NEXTGEN}
    procedure WriteWideString(const Str: WideString);
{$ELSE}
    procedure WriteWideString(const Str: string);
{$ENDIF}
    procedure WriteStrings(const SS: TStrings);
    procedure WriteList(const AList: TAbstractList); overload;
    procedure WriteMap(const AMap: TAbstractMap); overload;
{$IFDEF Supports_Generics}
    procedure Serialize(const Value; Info: PTypeInfo); overload;
    procedure WriteArray(const DynArray; const Name: string); overload;
    procedure WriteArrayWithRef(const DynArray; Info: PTypeInfo); overload;
    procedure WriteList(const AList: TObject); overload;
    procedure WriteObjectList(const AList: TObject);
    procedure WriteQueue(const AQueue: TObject);
    procedure WriteObjectQueue(const AQueue: TObject);
    procedure WriteStack(const AStack: TObject);
    procedure WriteObjectStack(const AStack: TObject);
    procedure WriteDictionary(const ADict: TObject);
    procedure WriteObjectDictionary(const ADict: TObject);
    procedure WriteTDictionary1<TKey>(const ADict: TObject;
              KeyInfo, ValueInfo: PTypeInfo);
    procedure WriteTDictionary2<TKey, TValue>(const ADict: TDictionary<TKey, TValue>;
              KeyInfo, ValueInfo: PTypeInfo);
{$ENDIF}
  public
    constructor Create(AStream: TStream; Simple: Boolean = False);
{$IFDEF BCB}
    constructor Create1(AStream: TStream);
{$ENDIF}
    procedure Serialize(const Value: Variant); overload;
    procedure Serialize(const Value: array of const); overload;
    procedure WriteInteger(I: Integer);
    procedure WriteLong(L: Int64); overload;
{$IFDEF DELPHI2009_UP}
    procedure WriteLong(L: UInt64); overload;
{$ENDIF}
{$IFDEF FPC}
    procedure WriteLong(L: QWord); overload;
{$ENDIF}
    procedure WriteLong(const L: string); overload;
    procedure WriteDouble(D: Extended);
    procedure WriteCurrency(C: Currency);
    procedure WriteNull();
    procedure WriteEmpty();
    procedure WriteBoolean(B: Boolean);
    procedure WriteNaN();
    procedure WriteInfinity(Positive: Boolean);
    procedure WriteUTF8Char(C: WideChar);
    procedure WriteDateTime(const ADateTime: TDateTime);
    procedure WriteDateTimeWithRef(const ADateTime: TDateTime);
    procedure WriteBytes(const Bytes: Variant);
    procedure WriteBytesWithRef(const Bytes: Variant);
{$IFNDEF NEXTGEN}
    procedure WriteString(const S: WideString);
    procedure WriteStringWithRef(const S: WideString);
{$ELSE}
    procedure WriteString(const S: string);
    procedure WriteStringWithRef(const S: string);
{$ENDIF}
    procedure WriteArray(const Value: Variant); overload;
    procedure WriteArrayWithRef(const Value: Variant); overload;
    procedure WriteArray(const Value: array of const); overload;
    procedure WriteList(const AList: IList); overload;
    procedure WriteListWithRef(const AList: IList); overload;
    procedure WriteMap(const AMap: IMap); overload;
    procedure WriteMapWithRef(const AMap: IMap);
{$IFDEF Supports_Generics}
    procedure Serialize<T>(const Value: T); overload;
    procedure WriteArray<T>(const DynArray: array of T); overload;
    procedure WriteDynArray<T>(const DynArray: TArray<T>);
    procedure WriteDynArrayWithRef<T>(const DynArray: TArray<T>); overload;
    procedure WriteTList<T>(const AList: TList<T>); overload;
    procedure WriteTListWithRef<T>(const AList: TList<T>); overload;
    procedure WriteTQueue<T>(const AQueue: TQueue<T>); overload;
    procedure WriteTQueueWithRef<T>(const AQueue: TQueue<T>);
    procedure WriteTStack<T>(const AStack: TStack<T>); overload;
    procedure WriteTStackWithRef<T>(const AStack: TStack<T>);
    procedure WriteTDictionary<TKey, TValue>(const ADict: TDictionary<TKey, TValue>); overload;
    procedure WriteTDictionaryWithRef<TKey, TValue>(const ADict: TDictionary<TKey, TValue>);
{$ELSE}
    procedure Serialize(const Value: TObject); overload;
{$ENDIF}
    procedure WriteObject(const AObject: TObject);
    procedure WriteObjectWithRef(const AObject: TObject);
    procedure WriteInterface(const Intf: IInterface);
    procedure WriteInterfaceWithRef(const Intf: IInterface);
    procedure WriteSmartObject(const SmartObject: ISmartObject);
    procedure WriteSmartObjectWithRef(const SmartObject: ISmartObject);
    procedure Reset;
    property Stream: TStream read FStream;
  end;

  THproseFormatter = class
  public
    class function Serialize(const Value: Variant; Simple: Boolean = False): TBytes; overload;
    class function Serialize(const Value: array of const; Simple: Boolean = False): TBytes; overload;
{$IFDEF Supports_Generics}
    class function Serialize<T>(const Value: T; Simple: Boolean = False): TBytes; overload;
    class function Unserialize<T>(const Data:TBytes; Simple: Boolean = False): T; overload;
{$ELSE}
    class function Serialize(const Value: TObject; Simple: Boolean = False): TBytes; overload;
{$ENDIF}
    class function Unserialize(const Data:TBytes; Simple: Boolean = False): Variant; overload;
    class function Unserialize(const Data:TBytes; Info: PTypeInfo; Simple: Boolean = False): Variant; overload;
  end;

function HproseSerialize(const Value: TObject; Simple: Boolean = False): TBytes; overload;
function HproseSerialize(const Value: Variant; Simple: Boolean = False): TBytes; overload;
function HproseSerialize(const Value: array of const; Simple: Boolean = False): TBytes; overload;
function HproseUnserialize(const Data:TBytes; Info: PTypeInfo; Simple: Boolean = False): Variant; overload;
function HproseUnserialize(const Data:TBytes; Simple: Boolean = True): Variant; overload;

implementation

{$IFDEF NEXTGEN}
{$ZEROBASEDSTRINGS OFF}
{$ENDIF}

uses DateUtils, Math, RTLConsts, StrUtils,
{$IFNDEF FPC}SysConst, {$ENDIF}
{$IFDEF Supports_Rtti}Rtti, {$ENDIF}
     Variants;
type

  PSmallIntArray = ^TSmallIntArray;
  TSmallIntArray = array[0..MaxInt div Sizeof(SmallInt) - 1] of SmallInt;

  PShortIntArray = ^TShortIntArray;
  TShortIntArray = array[0..MaxInt div Sizeof(ShortInt) - 1] of ShortInt;

  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array[0..MaxInt div Sizeof(LongWord) - 1] of LongWord;

  PSingleArray = ^TSingleArray;
  TSingleArray = array[0..MaxInt div Sizeof(Single) - 1] of Single;

  PDoubleArray = ^TDoubleArray;
  TDoubleArray = array[0..MaxInt div Sizeof(Double) - 1] of Double;

  PCurrencyArray = ^TCurrencyArray;
  TCurrencyArray = array[0..MaxInt div Sizeof(Currency) - 1] of Currency;

  PWordBoolArray = ^TWordBoolArray;
  TWordBoolArray = array[0..MaxInt div Sizeof(WordBool) - 1] of WordBool;

  PWideStringArray = ^TWideStringArray;
{$IFNDEF NEXTGEN}
  TWideStringArray = array[0..MaxInt div Sizeof(WideString) - 1] of WideString;
{$ELSE}
  TWideStringArray = array[0..MaxInt div Sizeof(string) - 1] of string;
{$ENDIF}

  PDateTimeArray = ^TDateTimeArray;
  TDateTimeArray = array[0..MaxInt div Sizeof(TDateTime) - 1] of TDateTime;

  PVariantArray = ^TVariantArray;
  TVariantArray = array[0..MaxInt div Sizeof(Variant) - 1] of Variant;

  PInterfaceArray = ^TInterfaceArray;
  TInterfaceArray = array[0..MaxInt div Sizeof(Variant) - 1] of IInterface;

  SerializeCache = record
    RefCount: Integer;
    Data: TBytes;
  end;
  PSerializeCache = ^SerializeCache;

var
  PropertiesCache: IMap;

const
  htInteger  = Byte('i');
  htLong     = Byte('l');
  htDouble   = Byte('d');
  htNull     = Byte('n');
  htEmpty    = Byte('e');
  htTrue     = Byte('t');
  htFalse    = Byte('f');
  htNaN      = Byte('N');
  htInfinity = Byte('I');
  htDate     = Byte('D');
  htTime     = Byte('T');
  htBytes    = Byte('b');
  htUTF8Char = Byte('u');
  htString   = Byte('s');
  htGuid     = Byte('g');
  htList     = Byte('a');
  htMap      = Byte('m');
  htClass    = Byte('c');
  htObject   = Byte('o');
  htRef      = Byte('r');
  htError    = Byte('E');

  HproseTagBoolean   :array[Boolean] of Byte = (Byte('f'), Byte('t'));
  HproseTagSign      :array[Boolean] of Byte = (Byte('-'), Byte('+'));

{$IFDEF Supports_Generics}
type
  TB1 = Byte;
  TB2 = Word;
  TB4 = LongWord;
  TB8 = UInt64;

function IsSmartObject(const Name: string): Boolean; inline;
begin
  Result := AnsiStartsText('ISmartObject<', Name) or
            AnsiStartsText('HproseCommon.ISmartObject<', Name);
end;

function GetElementName(const Name: string): string;
var
  I, L: Integer;
begin
  L := Length(Name);
  for I := 1 to L do if Name[I] = '<' then begin
    Result := AnsiMidStr(Name, I + 1, L - I - 1);
    Break;
  end;
end;

procedure SplitKeyValueTypeName(const Name: string;
  var KeyName, ValueName: string);
var
  I, N, P, L: Integer;
begin
  L := Length(Name);
  N := 0;
  P := 0;
  for I := 1 to L do begin
    case Name[I] of
      '<': Inc(N);
      '>': Dec(N);
      ',': if N = 0 then begin
        P := I;
        Break;
      end;
    end;
  end;
  if P > 0 then begin
    KeyName := AnsiMidStr(Name, 1, P - 1);
    ValueName := AnsiMidStr(Name, P + 1, L - P);
  end;
end;
{$ENDIF}

function GetStoredPropList(Instance: TObject; out PropList: PPropList): Integer;
var
  I, Count: Integer;
  TempList: PPropList;
begin
  Count := GetPropList(PTypeInfo(Instance.ClassInfo), TempList);
  PropList := nil;
  Result := 0;
  if Count > 0 then
    try
      for I := 0 to Count - 1 do
        if IsStoredProp(Instance, TempList^[I]) then
          Inc(Result);
      GetMem(PropList, Result * SizeOf(Pointer));
      for I := 0 to Result - 1 do
        if IsStoredProp(Instance, TempList^[I]) then
          PropList^[I] := TempList^[I];
    finally
      FreeMem(TempList);
    end;
end;

type
  TFakeReaderRefer = class(TInterfacedObject, IReaderRefer)
  public
    function SetRef(const V: Variant): Integer; overload;
    procedure SetRef(I: Integer; const V: Variant); overload;
    function ReadRef(I: Integer): Variant;
    procedure Reset;
  end;

{ TFakeReaderRefer }

function TFakeReaderRefer.ReadRef(I: Integer): Variant;
begin
  raise EHproseException.Create('Unexpected serialize tag "r" in stream');
end;

procedure TFakeReaderRefer.Reset;
begin
end;

function TFakeReaderRefer.SetRef(const V: Variant): Integer;
begin
  Result := 0;
end;

procedure TFakeReaderRefer.SetRef(I: Integer; const V: Variant);
begin
end;

type
  TRealReaderRefer = class(TInterfacedObject, IReaderRefer)
  private
    FRefList: IList;
  public
    function SetRef(const V: Variant): Integer; overload;
    procedure SetRef(I: Integer; const V: Variant); overload;
    function ReadRef(I: Integer): Variant;
    procedure Reset;
  end;

{ TRealReaderRefer }

function TRealReaderRefer.ReadRef(I: Integer): Variant;
begin
  Result := FRefList[I];
end;

procedure TRealReaderRefer.Reset;
begin
  if FRefList <> nil then FRefList.Clear;
end;

function TRealReaderRefer.SetRef(const V: Variant): Integer;
begin
  if FRefList = nil then FRefList := TArrayList.Create(False);
  Result := FRefList.Add(V);
end;

procedure TRealReaderRefer.SetRef(I: Integer; const V: Variant);
begin
  FRefList[I] := V;
end;

{ THproseReader }

constructor THproseReader.Create(AStream: TStream; Simple: Boolean);
begin
  FStream := AStream;
  if Simple then FRefer := TFakeReaderRefer.Create
  else FRefer := TRealReaderRefer.Create;
  FClassRefList := TArrayList.Create(False);
  FAttrRefMap := THashMap.Create(False);
end;

{$IFDEF BCB}
constructor THproseReader.Create1(AStream: TStream);
begin
  Create(AStream);
end;
{$ENDIF}

function THproseReader.UnexpectedTag(Tag: Byte;
  const ExpectTags: string): EHproseException;
begin
  if ExpectTags = '' then
    Result := EHproseException.Create('Unexpected serialize tag "' +
                                       Char(Tag) + '" in stream')
  else
    Result := EHproseException.Create('Tag "' + ExpectTags +
                                       '" expected, but "' + Char(Tag) +
                                       '" found in stream');
end;

function THproseReader.TagToString(Tag: Byte): string;
begin
  case Tag of
    Byte('0')..Byte('9'), htInteger: Result := 'Integer';
    htLong: Result := 'BigInteger';
    htDouble: Result := 'Double';
    htNull: Result := 'Null';
    htEmpty: Result := 'Empty String';
    htTrue: Result := 'Boolean True';
    htFalse: Result := 'Boolean False';
    htNaN: Result := 'NaN';
    htInfinity: Result := 'Infinity';
    htDate: Result := 'DateTime';
    htTime: Result := 'DateTime';
    htBytes: Result := 'Binary Data';
    htUTF8Char: Result := 'Char';
    htString: Result := 'String';
    htGuid: Result := 'Guid';
    htList: Result := 'List';
    htMap: Result := 'Map';
    htClass: Result := 'Class';
    htObject: Result := 'Object';
    htRef: Result := 'Object Reference';
    htError: raise EHproseException.Create(ReadString());
  else
    raise UnexpectedTag(Tag);
  end;
end;

procedure THproseReader.CheckTag(ExpectTag: Byte);
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  if Tag <> expectTag then raise UnexpectedTag(Tag, String(Char(ExpectTag)));
end;

function THproseReader.CheckTags(const ExpectTags: TBytes): Byte;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  if Pos(Char(Tag), StringOf(ExpectTags)) = 0 then raise UnexpectedTag(Tag, StringOf(ExpectTags));
  Result := Tag;
end;

function THproseReader.ReadByte: Byte;
begin
  FStream.ReadBuffer(Result, 1);
end;

function THproseReader.ReadUntil(Tag: Byte): string;
var
  BS: TBytesStream;
  Bytes: TBytes;
  C: Byte;
begin
  BS := TBytesStream.Create;
  try
    while (FStream.Read(C, 1) = 1) and (C <> Tag) do BS.Write(C, 1);
    Bytes := BS.Bytes;
    SetLength(Bytes, BS.Size);
    Result := StringOf(Bytes);
  finally
    BS.Free;
  end;
end;

function THproseReader.ReadInt(Tag: Byte): Integer;
var
  S: Integer;
  I: Integer;
  C: Byte;
begin
  Result := 0;
  S := 1;
  I := FStream.Read(C, 1);
  if I = 1 then
    if C = Byte('+') then
      I := FStream.Read(C, 1)
    else if C = Byte('-') then begin
      S := -1;
      I := FStream.Read(C, 1);
    end;
  while (I = 1) and (C <> Tag) do begin
    Result := Result * 10 + (C - Byte('0')) * S;
    I := FStream.Read(C, 1);
  end;
end;

function THproseReader.ReadInt64(Tag: Byte): Int64;
var
  S: Int64;
  I: Integer;
  C: Byte;
begin
  Result := 0;
  S := 1;
  I := FStream.Read(C, 1);
  if I = 1 then
    if C = Byte('+') then
      I := FStream.Read(C, 1)
    else if C = Byte('-') then begin
      S := -1;
      I := FStream.Read(C, 1);
    end;
  while (I = 1) and (C <> Tag) do begin
    Result := Result * 10 + Int64(C - Byte('0')) * S;
    I := FStream.Read(C, 1);
  end;
end;

{$IFDEF Supports_UInt64}
function THproseReader.ReadUInt64(Tag: Byte): UInt64;
var
  I: Integer;
  C: Byte;
begin
  Result := 0;
  I := FStream.Read(C, 1);
  if (I = 1) and (C = Byte('+')) then I := FStream.Read(C, 1);
  while (I = 1) and (C <> Tag) do begin
    Result := Result * 10 + UInt64(C - Byte('0'));
    I := FStream.Read(C, 1);
  end;
end;
{$ENDIF}

function THproseReader.ReadIntegerWithoutTag: Integer;
begin
  Result := ReadInt(HproseTagSemicolon);
end;

function THproseReader.ReadLongWithoutTag: string;
begin
  Result := ReadUntil(HproseTagSemicolon);
end;

function THproseReader.ReadInfinityWithoutTag: Extended;
begin
  if ReadByte = HproseTagNeg then
    Result := NegInfinity
  else
    Result := Infinity;
end;

function THproseReader.ReadDoubleWithoutTag: Extended;
begin
  Result := StrToFloat(ReadUntil(HproseTagSemicolon));
end;

function THproseReader.ReadDateWithoutTag: TDateTime;
var
  Tag, Year, Month, Day, Hour, Minute, Second, Millisecond: Integer;
begin
  Year := ReadByte - Byte('0');
  Year := Year * 10 + ReadByte - Byte('0');
  Year := Year * 10 + ReadByte - Byte('0');
  Year := Year * 10 + ReadByte - Byte('0');
  Month := ReadByte - Byte('0');
  Month := Month * 10 + ReadByte - Byte('0');
  Day := ReadByte - Byte('0');
  Day := Day * 10 + ReadByte - Byte('0');
  Tag := ReadByte;
  if Tag = HproseTagTime then begin
    Hour := ReadByte - Byte('0');
    Hour := Hour * 10 + ReadByte - Byte('0');
    Minute := ReadByte - Byte('0');
    Minute := Minute * 10 + ReadByte - Byte('0');
    Second := ReadByte - Byte('0');
    Second := Second * 10 + ReadByte - Byte('0');
    Millisecond := 0;
    if ReadByte = HproseTagPoint then begin
      Millisecond := ReadByte - Byte('0');
      Millisecond := Millisecond * 10 + ReadByte - Byte('0');
      Millisecond := Millisecond * 10 + ReadByte - Byte('0');
      Tag := ReadByte;
      if Tag in [Byte('0')..Byte('9')] then begin
        ReadByte;
        ReadByte;
        Tag := ReadByte;
        if Tag in [Byte('0')..Byte('9')] then begin
          ReadByte;
          ReadByte;
          ReadByte;
        end;
      end;
    end;
    Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, Millisecond);
  end
  else
    Result := EncodeDate(Year, Month, Day);
  FRefer.SetRef(Result);
end;

function THproseReader.ReadTimeWithoutTag: TDateTime;
var
  Tag, Hour, Minute, Second, Millisecond: Integer;
begin
  Hour := ReadByte - Byte('0');
  Hour := Hour * 10 + ReadByte - Byte('0');
  Minute := ReadByte - Byte('0');
  Minute := Minute * 10 + ReadByte - Byte('0');
  Second := ReadByte - Byte('0');
  Second := Second * 10 + ReadByte - Byte('0');
  Millisecond := 0;
  if ReadByte = HproseTagPoint then begin
    Millisecond := ReadByte - Byte('0');
    Millisecond := Millisecond * 10 + ReadByte - Byte('0');
    Millisecond := Millisecond * 10 + ReadByte - Byte('0');
    Tag := ReadByte;
    if Tag in [Byte('0')..Byte('9')] then begin
      ReadByte;
      ReadByte;
      Tag := ReadByte;
      if Tag in [Byte('0')..Byte('9')] then begin
        ReadByte;
        ReadByte;
        ReadByte;
      end;
    end;
  end;
  Result := EncodeTime(Hour, Minute, Second, Millisecond);
  FRefer.SetRef(Result);
end;

function THproseReader.ReadUTF8CharWithoutTag: WideChar;
var
  C, C2, C3: LongWord;
begin
  C := ReadByte;
  case C shr 4 of
    0..7: { 0xxx xxxx } Result := WideChar(C);
    12,13: begin
      { 110x xxxx   10xx xxxx }
      C2 := ReadByte;
      Result := WideChar(((C and $1F) shl 6) or
                          (C2 and $3F));
    end;
    14: begin
      { 1110 xxxx  10xx xxxx  10xx xxxx }
      C2 := ReadByte;
      C3 := ReadByte;
      Result := WideChar(((C and $0F) shl 12) or
                         ((C2 and $3F) shl 6) or
                          (C3 and $3F));
    end;
  else
    raise EHproseException.Create('bad unicode encoding at $' + IntToHex(C, 4));
  end;
end;

{$IFNDEF NEXTGEN}
function THproseReader.ReadStringAsWideString: WideString;
var
  Count, I: Integer;
  C, C2, C3, C4: LongWord;
begin
  Count := ReadInt(HproseTagQuote);
  SetLength(Result, Count);
  I := 0;
  while I < Count do begin
    Inc(I);
    C := ReadByte;
    case C shr 4 of
      0..7: { 0xxx xxxx } Result[I] := WideChar(C);
      12,13: begin
        { 110x xxxx   10xx xxxx }
        C2 := ReadByte;
        Result[I] := WideChar(((C and $1F) shl 6) or
                              (C2 and $3F));
      end;
      14: begin
        { 1110 xxxx  10xx xxxx  10xx xxxx }
        C2 := ReadByte;
        C3 := ReadByte;
        Result[I] := WideChar(((C and $0F) shl 12) or
                             ((C2 and $3F) shl 6)  or
                              (C3 and $3F));
      end;
      15: begin
        { 1111 0xxx  10xx xxxx  10xx xxxx  10xx xxxx }
        if (C and $F) <= 4 then begin
          C2 := ReadByte;
          C3 := ReadByte;
          C4 := ReadByte;
          C := ((C and $07) shl 18) or
              ((C2 and $3F) shl 12) or
              ((C3 and $3F) shl 6)  or
               (C4 and $3F) - $10000;
          if C <= $FFFFF then begin
            Result[I] := WideChar(((C shr 10) and $03FF) or $D800);
            Inc(I);
            Result[I] := WideChar((C and $03FF) or $DC00);
            Continue;
          end;
        end;
        raise EHproseException.Create('bad unicode encoding at $' + IntToHex(C, 4));
      end;
    else
      raise EHproseException.Create('bad unicode encoding at $' + IntToHex(C, 4));
    end;
  end;
  CheckTag(HproseTagQuote);
end;
{$ELSE}
function THproseReader.ReadStringAsWideString: string;
var
  Count, I: Integer;
  C, C2, C3, C4: LongWord;
  Chars: TCharArray;
begin
  Count := ReadInt(HproseTagQuote);
  SetLength(Chars, Count);
  I := 0;
  while I < Count do begin
    C := ReadByte;
    case C shr 4 of
      0..7: { 0xxx xxxx } Chars[I] := WideChar(C);
      12,13: begin
        { 110x xxxx   10xx xxxx }
        C2 := ReadByte;
        Chars[I] := WideChar(((C and $1F) shl 6) or
                              (C2 and $3F));
      end;
      14: begin
        { 1110 xxxx  10xx xxxx  10xx xxxx }
        C2 := ReadByte;
        C3 := ReadByte;
        Chars[I] := WideChar(((C and $0F) shl 12) or
                             ((C2 and $3F) shl 6)  or
                              (C3 and $3F));
      end;
      15: begin
        { 1111 0xxx  10xx xxxx  10xx xxxx  10xx xxxx }
        if (C and $F) <= 4 then begin
          C2 := ReadByte;
          C3 := ReadByte;
          C4 := ReadByte;
          C := ((C and $07) shl 18) or
              ((C2 and $3F) shl 12) or
              ((C3 and $3F) shl 6)  or
               (C4 and $3F) - $10000;
          if C <= $FFFFF then begin
            Chars[I] := WideChar(((C shr 10) and $03FF) or $D800);
            Inc(I);
            Chars[I] := WideChar((C and $03FF) or $DC00);
            Continue;
          end;
        end;
        raise EHproseException.Create('bad unicode encoding at $' + IntToHex(C, 4));
      end;
    else
      raise EHproseException.Create('bad unicode encoding at $' + IntToHex(C, 4));
    end;
    Inc(I);
  end;
  CheckTag(HproseTagQuote);
  SetString(Result, PChar(Chars), Count);
end;
{$ENDIF}

{$IFNDEF NEXTGEN}
function THproseReader.ReadStringWithoutTag: WideString;
{$ELSE}
function THproseReader.ReadStringWithoutTag: string;
{$ENDIF}
begin
  Result := ReadStringAsWideString;
  FRefer.SetRef(Result);
end;

function THproseReader.ReadBytesWithoutTag: Variant;
var
  Len: Integer;
  P: PByteArray;
begin
  Len := ReadInt(HproseTagQuote);
  Result := VarArrayCreate([0, Len - 1], varByte);
  P := VarArrayLock(Result);
  FStream.ReadBuffer(P^[0], Len);
  VarArrayUnLock(Result);
  CheckTag(HproseTagQuote);
  FRefer.SetRef(VarArrayRef(Result));
end;

function THproseReader.ReadGuidWithoutTag: string;
var
  Bytes: TBytes;
begin
  SetLength(Bytes, 38);
  FStream.ReadBuffer(Bytes[0], 38);
  Result := StringOf(Bytes);
  FRefer.SetRef(Result);
end;

function THproseReader.ReadBooleanArray(Count: Integer): Variant;
var
  P: PWordBoolArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varBoolean);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := ReadBoolean;
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadShortIntArray(Count: Integer): Variant;
var
  P: PShortIntArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varShortInt);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := ShortInt(ReadInteger);
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadByteArray(Count: Integer): Variant;
var
  P: PShortIntArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varByte);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := Byte(ReadInteger);
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadSmallIntArray(Count: Integer): Variant;
var
  P: PSmallIntArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varSmallInt);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := SmallInt(ReadInteger);
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadWordArray(Count: Integer): Variant;
var
  P: PWordArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varWord);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := Word(ReadInteger);
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadIntegerArray(Count: Integer): Variant;
var
  P: PIntegerArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varInteger);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := ReadInteger;
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadLongWordArray(Count: Integer): Variant;
var
  P: PLongWordArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varLongWord);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := LongWord(ReadInt64);
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadSingleArray(Count: Integer): Variant;
var
  P: PSingleArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varSingle);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := ReadExtended;
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadDoubleArray(Count: Integer): Variant;
var
  P: PDoubleArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varDouble);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := ReadExtended;
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadCurrencyArray(Count: Integer): Variant;
var
  P: PCurrencyArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varCurrency);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := ReadCurrency;
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadDateTimeArray(Count: Integer): Variant;
var
  P: PDateTimeArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varDate);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := ReadDateTime;
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadWideStringArray(Count: Integer): Variant;
var
  P: PWideStringArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varOleStr);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := ReadString;
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadVariantArray(Count: Integer): Variant;
var
  P: PVariantArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varVariant);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := Unserialize;
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadInterfaceArray(Count: Integer): Variant;
var
  P: PInterfaceArray;
  I, N: Integer;
begin
  Result := VarArrayCreate([0, Count - 1], varVariant);
  N := FRefer.SetRef(Null);
  P := VarArrayLock(Result);
  for I := 0 to Count - 1 do P^[I] := Unserialize;
  VarArrayUnlock(Result);
  FRefer.SetRef(N, VarArrayRef(Result));
end;

function THproseReader.ReadDynArrayWithoutTag(varType: Integer): Variant;
var
  Count: Integer;
begin
  Count := ReadInt(HproseTagOpenbrace);
  case varType of
    varBoolean:  Result := ReadBooleanArray(Count);
    varShortInt: Result := ReadShortIntArray(Count);
    varByte:     Result := ReadByteArray(Count);
    varSmallint: Result := ReadSmallintArray(Count);
    varWord:     Result := ReadWordArray(Count);
    varInteger:  Result := ReadIntegerArray(Count);
    varLongWord: Result := ReadLongWordArray(Count);
    varSingle:   Result := ReadSingleArray(Count);
    varDouble:   Result := ReadDoubleArray(Count);
    varCurrency: Result := ReadCurrencyArray(Count);
    varOleStr:   Result := ReadWideStringArray(Count);
    varDate:     Result := ReadDateTimeArray(Count);
    varVariant:  Result := ReadVariantArray(Count);
    varUnknown:  Result := ReadInterfaceArray(Count);
  end;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadIList(AClass: TClass): IList;
var
  Count, I: Integer;
begin
  Count := ReadInt(HproseTagOpenbrace);
  Result := TListClass(AClass).Create(Count) as IList;
  FRefer.SetRef(Result);
  for I := 0 to Count - 1 do Result[I] := Unserialize;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadList(AClass: TClass): TAbstractList;
var
  Count, I: Integer;
begin
  Count := ReadInt(HproseTagOpenbrace);
  Result := TListClass(AClass).Create(Count);
  FRefer.SetRef(ObjToVar(Result));
  for I := 0 to Count - 1 do Result[I] := Unserialize;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadListWithoutTag: Variant;
begin
  Result := ReadIList(TArrayList);
end;

function THproseReader.ReadListAsIMap(AClass: TClass): IMap;
var
  Count, I: Integer;
begin
  Count := ReadInt(HproseTagOpenbrace);
  Result := TMapClass(AClass).Create(Count) as IMap;
  FRefer.SetRef(Result);
  for I := 0 to Count - 1 do Result[I] := Unserialize;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadListAsMap(AClass: TClass): TAbstractMap;
var
  Count, I: Integer;
begin
  Count := ReadInt(HproseTagOpenbrace);
  Result := TMapClass(AClass).Create(Count);
  FRefer.SetRef(ObjToVar(Result));
  for I := 0 to Count - 1 do Result[I] := Unserialize;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadIMap(AClass: TClass): IMap;
var
  Count, I: Integer;
  Key: Variant;
begin
  Count := ReadInt(HproseTagOpenbrace);
  Result := TMapClass(AClass).Create(Count) as IMap;
  FRefer.SetRef(Result);
  for I := 0 to Count - 1 do begin
    Key := Unserialize;
    Result[Key] := Unserialize;
  end;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadMap(AClass: TClass): TAbstractMap;
var
  Count, I: Integer;
  Key: Variant;
begin
  Count := ReadInt(HproseTagOpenbrace);
  Result := TMapClass(AClass).Create(Count);
  FRefer.SetRef(ObjToVar(Result));
  for I := 0 to Count - 1 do begin
    Key := Unserialize;
    Result[Key] := Unserialize;
  end;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadMapAsInterface(AClass: TClass; const IID: TGUID): IInterface;
var
  Count, I: Integer;
  Instance: TObject;
  PropInfo: PPropInfo;
begin
  Count := ReadInt(HproseTagOpenbrace);
  Instance := AClass.Create;
  Supports(Instance, IID, Result);
  FRefer.SetRef(Result);
  for I := 0 to Count - 1 do begin
    PropInfo := GetPropInfo(AClass, ReadString);
    if (PropInfo <> nil) then
      HproseCommon.SetPropValue(Instance, PropInfo,
                   Unserialize(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF}))
    else Unserialize;
  end;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadMapAsObject(AClass: TClass): TObject;
var
  Count, I: Integer;
  PropInfo: PPropInfo;
begin
  Count := ReadInt(HproseTagOpenbrace);
  Result := AClass.Create;
  FRefer.SetRef(ObjToVar(Result));
  for I := 0 to Count - 1 do begin
    PropInfo := GetPropInfo(AClass, ReadString);
    if (PropInfo <> nil) then
      HproseCommon.SetPropValue(Result, PropInfo,
                   Unserialize(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF}))
    else Unserialize;
  end;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadMapWithoutTag: Variant;
begin
  Result := ReadIMap(THashMap);
end;

procedure THproseReader.ReadClass;
var
  ClassName: string;
  I, Count: Integer;
  AttrNames: IList;
  AClass: TClass;
  Key: Variant;
begin
  ClassName := ReadStringAsWideString;
  Count := ReadInt(HproseTagOpenbrace);
  AttrNames := TArrayList.Create(Count, False) as IList;
  for I := 0 to Count - 1 do AttrNames[I] := ReadString;
  CheckTag(HproseTagClosebrace);
  AClass := GetClassByAlias(ClassName);
  if AClass = nil then begin
    Key := IInterface(TInterfacedObject.Create);
    FClassRefList.Add(Key);
    FAttrRefMap[Key] := AttrNames;
  end
  else begin
    Key := NativeInt(AClass);
    FClassRefList.Add(Key);
    FAttrRefMap[Key] := AttrNames;
  end;
end;

function THproseReader.ReadObjectAsIMap(AClass: TMapClass): IMap;
var
  C: Variant;
  AttrNames: IList;
  I, Count: Integer;
begin
  C := FClassRefList[ReadInt(HproseTagOpenbrace)];
  AttrNames := VarToList(FAttrRefMap[C]);
  Count := AttrNames.Count;
  Result := AClass.Create(Count) as IMap;
  FRefer.SetRef(Result);
  for I := 0 to Count - 1 do Result[AttrNames[I]] := Unserialize;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadObjectAsMap(AClass: TMapClass): TAbstractMap;
var
  C: Variant;
  AttrNames: IList;
  I, Count: Integer;
begin
  C := FClassRefList[ReadInt(HproseTagOpenbrace)];
  AttrNames := VarToList(FAttrRefMap[C]);
  Count := AttrNames.Count;
  Result := AClass.Create(Count);
  FRefer.SetRef(ObjToVar(Result));
  for I := 0 to Count - 1 do Result[AttrNames[I]] := Unserialize;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadObjectAsInterface(AClass: TClass; const IID: TGUID): IInterface;
var
  C: Variant;
  RegisteredClass: TClass;
  AttrNames: IList;
  I, Count: Integer;
  Instance: TObject;
  PropInfo: PPropInfo;
begin
  C := FClassRefList[ReadInt(HproseTagOpenbrace)];
  if VarType(C) = varNativeInt then begin
    RegisteredClass := TClass(NativeInt(C));
    if (AClass = nil) or
       RegisteredClass.InheritsFrom(AClass) then AClass := RegisteredClass;
  end;
  AttrNames := VarToList(FAttrRefMap[C]);
  Count := AttrNames.Count;
  Instance := AClass.Create;
  Supports(Instance, IID, Result);
  FRefer.SetRef(Result);
  for I := 0 to Count - 1 do begin
    PropInfo := GetPropInfo(Instance, AttrNames[I]);
    if (PropInfo <> nil) then
      HproseCommon.SetPropValue(Instance, PropInfo,
                   Unserialize(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF}))
    else Unserialize;
  end;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadObjectWithoutTag(AClass: TClass): TObject;
var
  C: Variant;
  RegisteredClass: TClass;
  AttrNames: IList;
  I, Count: Integer;
  PropInfo: PPropInfo;
begin
  C := FClassRefList[ReadInt(HproseTagOpenbrace)];
  if VarType(C) = varNativeInt then begin
    RegisteredClass := TClass(NativeInt(C));
    if (AClass = nil) or
       RegisteredClass.InheritsFrom(AClass) then AClass := RegisteredClass;
  end;
  AttrNames := VarToList(FAttrRefMap[C]);
  Count := AttrNames.Count;
  Result := AClass.Create;
  FRefer.SetRef(ObjToVar(Result));
  for I := 0 to Count - 1 do begin
    PropInfo := GetPropInfo(Result, AttrNames[I]);
    if (PropInfo <> nil) then
      HproseCommon.SetPropValue(Result, PropInfo,
                   Unserialize(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF}))
    else Unserialize;
  end;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadObjectWithoutTag: Variant;
var
  C: Variant;
  AttrNames: IList;
  I, Count: Integer;
  AClass: TClass;
  AMap: IMap;
  Intf: IInterface;
  Instance: TObject;
  Map: TAbstractMap;
  PropInfo: PPropInfo;
begin
  C := FClassRefList[ReadInt(HproseTagOpenbrace)];
  AttrNames := VarToList(FAttrRefMap[C]);
  Count := AttrNames.Count;
  if VarType(C) = varNativeInt then begin
    AClass := TClass(NativeInt(C));
    if AClass.InheritsFrom(TInterfacedObject) and
       HasRegisterWithInterface(TInterfacedClass(AClass)) then begin
      Instance := AClass.Create;
      Supports(Instance, GetInterfaceByClass(TInterfacedClass(AClass)), Intf);
      Result := Intf;
    end
    else begin
      Instance := AClass.Create;
      Result := ObjToVar(Instance);
    end;
    FRefer.SetRef(Result);
    if Instance is TAbstractMap then begin
      Map := TAbstractMap(Instance);
      for I := 0 to Count - 1 do Map[AttrNames[I]] := Unserialize;
    end
    else for I := 0 to Count - 1 do begin
      PropInfo := GetPropInfo(Instance, AttrNames[I]);
      if (PropInfo <> nil) then
        HproseCommon.SetPropValue(Instance, PropInfo,
                     Unserialize(PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF}))
      else Unserialize;
    end;
  end
  else begin
    AMap := TCaseInsensitiveHashMap.Create(Count) as IMap;
    Result := AMap;
    FRefer.SetRef(Result);
    for I := 0 to Count - 1 do AMap[AttrNames[I]] := Unserialize;
  end;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadRef: Variant;
begin
  Result := FRefer.ReadRef(ReadInt(HproseTagSemicolon));
end;

function CastError(const SrcType, DestType: string): EHproseException;
begin
  Result := EHproseException.Create(SrcType + ' can''t change to ' + DestType);
end;

function THproseReader.ReadInteger: Integer;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    Byte('0'): Result := 0;
    Byte('1'): Result := 1;
    Byte('2'): Result := 2;
    Byte('3'): Result := 3;
    Byte('4'): Result := 4;
    Byte('5'): Result := 5;
    Byte('6'): Result := 6;
    Byte('7'): Result := 7;
    Byte('8'): Result := 8;
    Byte('9'): Result := 9;
    htInteger,
    htLong: Result := ReadIntegerWithoutTag;
    htDouble: Result := Integer(Trunc(ReadDoubleWithoutTag));
    htNull,
    htEmpty,
    htFalse: Result := 0;
    htTrue: Result := 1;
    htUTF8Char: Result := StrToInt(string(ReadUTF8CharWithoutTag));
    htString: Result := StrToInt(ReadStringWithoutTag);
  else
    raise CastError(TagToString(Tag), 'Integer');
  end;
end;

function THproseReader.ReadInt64: Int64;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    Byte('0'): Result := 0;
    Byte('1'): Result := 1;
    Byte('2'): Result := 2;
    Byte('3'): Result := 3;
    Byte('4'): Result := 4;
    Byte('5'): Result := 5;
    Byte('6'): Result := 6;
    Byte('7'): Result := 7;
    Byte('8'): Result := 8;
    Byte('9'): Result := 9;
    htInteger,
    htLong: Result := ReadInt64(HproseTagSemicolon);
    htDouble: Result := Trunc(ReadDoubleWithoutTag);
    htNull,
    htEmpty,
    htFalse: Result := 0;
    htTrue: Result := 1;
    htUTF8Char: Result := StrToInt64(string(ReadUTF8CharWithoutTag));
    htString: Result := StrToInt64(ReadStringWithoutTag);
  else
    raise CastError(TagToString(Tag), 'Int64');
  end;
end;

{$IFDEF Supports_UInt64}
function THproseReader.ReadUInt64: UInt64;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    Byte('0'): Result := 0;
    Byte('1'): Result := 1;
    Byte('2'): Result := 2;
    Byte('3'): Result := 3;
    Byte('4'): Result := 4;
    Byte('5'): Result := 5;
    Byte('6'): Result := 6;
    Byte('7'): Result := 7;
    Byte('8'): Result := 8;
    Byte('9'): Result := 9;
    htInteger,
    htLong: Result := ReadUInt64(HproseTagSemicolon);
    htDouble: Result := Trunc(ReadDoubleWithoutTag);
    htNull,
    htEmpty,
    htFalse: Result := 0;
    htTrue: Result := 1;
{$IFDEF FPC}
    htUTF8Char: Result := StrToQWord(string(ReadUTF8CharWithoutTag));
    htString: Result := StrToQWord(ReadStringWithoutTag);
{$ELSE}
    htUTF8Char: Result := StrToInt64(string(ReadUTF8CharWithoutTag));
    htString: Result := StrToInt64(ReadStringWithoutTag);
{$ENDIF}
  else
    raise CastError(TagToString(Tag), 'UInt64');
  end;
end;
{$ENDIF}

function THproseReader.ReadExtended: Extended;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    Byte('0'): Result := 0;
    Byte('1'): Result := 1;
    Byte('2'): Result := 2;
    Byte('3'): Result := 3;
    Byte('4'): Result := 4;
    Byte('5'): Result := 5;
    Byte('6'): Result := 6;
    Byte('7'): Result := 7;
    Byte('8'): Result := 8;
    Byte('9'): Result := 9;
    htInteger,
    htLong,
    htDouble: Result := ReadDoubleWithoutTag;
    htNull,
    htEmpty,
    htFalse: Result := 0;
    htTrue: Result := 1;
    htNaN: Result := NaN;
    htInfinity: Result := ReadInfinityWithoutTag;
    htUTF8Char: Result := StrToFloat(string(ReadUTF8CharWithoutTag));
    htString: Result := StrToFloat(ReadStringWithoutTag);
  else
    raise CastError(TagToString(Tag), 'Extended');
  end;
end;

function THproseReader.ReadCurrency: Currency;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    Byte('0'): Result := 0;
    Byte('1'): Result := 1;
    Byte('2'): Result := 2;
    Byte('3'): Result := 3;
    Byte('4'): Result := 4;
    Byte('5'): Result := 5;
    Byte('6'): Result := 6;
    Byte('7'): Result := 7;
    Byte('8'): Result := 8;
    Byte('9'): Result := 9;
    htInteger,
    htLong,
    htDouble: Result := StrToCurr(ReadUntil(HproseTagSemicolon));
    htNull,
    htEmpty,
    htFalse: Result := 0;
    htTrue: Result := 1;
    htUTF8Char: Result := StrToCurr(string(ReadUTF8CharWithoutTag));
    htString: Result := StrToCurr(ReadStringWithoutTag);
  else
    raise CastError(TagToString(Tag), 'Currency');
  end;
end;

function THproseReader.ReadBoolean: Boolean;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    Byte('0'): Result := False;
    Byte('1')..Byte('9'): Result := True;
    htInteger,
    htLong,
    htDouble: Result := ReadDoubleWithoutTag <> 0;
    htNull,
    htEmpty,
    htFalse: Result := False;
    htTrue: Result := True;
    htUTF8Char: Result := StrToBool(string(ReadUTF8CharWithoutTag));
    htString: Result := StrToBool(ReadStringWithoutTag);
  else
    raise CastError(TagToString(Tag), 'Boolean');
  end;
end;

function THproseReader.ReadDateTime: TDateTime;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    Byte('0')..Byte('9'): Result := TimeStampToDateTime(MSecsToTimeStamp(Tag - Byte('0')));
    htInteger: Result := TimeStampToDateTime(MSecsToTimeStamp(ReadIntegerWithoutTag));
    htLong: Result := TimeStampToDateTime(MSecsToTimeStamp(ReadInt64(HproseTagSemicolon)));
    htDouble: Result := TimeStampToDateTime(MSecsToTimeStamp(Trunc(ReadDoubleWithoutTag)));
    htDate: Result := ReadDateWithoutTag;
    htTime: Result := ReadTimeWithoutTag;
    htString: Result := StrToDateTime(ReadStringWithoutTag);
    htRef: Result := ReadRef;
  else
    raise CastError(TagToString(Tag), 'TDateTime');
  end;
end;

function THproseReader.ReadUTF8Char: WideChar;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    Byte('0')..Byte('9'): Result := WideChar(Tag);
    htInteger,
    htLong: Result := WideChar(ReadIntegerWithoutTag);
    htDouble: Result := WideChar(Trunc(ReadDoubleWithoutTag));
    htNull: Result := #0;
    htUTF8Char: Result := ReadUTF8CharWithoutTag;
    htString: Result := ReadStringWithoutTag[1];
  else
    raise CastError(TagToString(Tag), 'WideChar');
  end;
end;

{$IFNDEF NEXTGEN}
function THproseReader.ReadString: WideString;
{$ELSE}
function THproseReader.ReadString: string;
{$ENDIF}
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    Byte('0')..Byte('9'): Result := string(Char(Tag));
    htInteger,
    htLong,
    htDouble: Result := ReadUntil(HproseTagSemicolon);
    htNull,
    htEmpty: Result := '';
    htFalse: Result := 'False';
    htTrue: Result := 'True';
    htNaN: Result := FloatToStr(NaN);
    htInfinity: Result := FloatToStr(ReadInfinityWithoutTag);
    htDate: Result := DateTimeToStr(ReadDateWithoutTag);
    htTime: Result := DateTimeToStr(ReadTimeWithoutTag);
    htUTF8Char: Result := string(ReadUTF8CharWithoutTag);
    htString: Result := ReadStringWithoutTag;
    htGuid: Result := ReadGuidWithoutTag;
    htRef: Result := ReadRef;
  else
    raise CastError(TagToString(Tag), 'String');
  end;
end;

function THproseReader.ReadBytes: Variant;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    htNull,
    htEmpty: Result := Null;
    htBytes: Result := ReadBytesWithoutTag;
    htList: Result := ReadDynArrayWithoutTag(varByte);
    htRef: Result := ReadRef;
  else
    raise CastError(TagToString(Tag), 'Byte');
  end;
end;

function THproseReader.ReadGuid: string;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    htNull,
    htEmpty: Result := '';
    htString: Result := GuidToString(StringToGuid(ReadStringWithoutTag));
    htGuid: Result := ReadGuidWithoutTag;
    htRef: Result := ReadRef;
  else
    raise CastError(TagToString(Tag), 'TGUID');
  end;
end;

function THproseReader.ReadDynArray(varType: Integer): Variant;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    htNull,
    htEmpty: Result := Null;
    htList: Result := ReadDynArrayWithoutTag(varType);
    htRef: Result := ReadRef;
  else
    raise CastError(TagToString(Tag), 'DynArray');
  end;
end;

function THproseReader.ReadVariantArray: TVariants;
var
  Tag: Byte;
  I, Count: Integer;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    htNull,
    htEmpty: Result := Null;
    htList: begin
      Count := ReadInt(HproseTagOpenbrace);
      SetLength(Result, Count);
      FRefer.SetRef(Null);
      for I := 0 to Count - 1 do Result[I] := Unserialize;
      CheckTag(HproseTagClosebrace);
    end;
  else
    raise CastError(TagToString(Tag), 'TVariants');
  end;
end;

function THproseReader.ReadInterface(AClass: TClass; const IID: TGUID): IInterface;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    htNull,
    htEmpty: Result := nil;
    htList:
      if AClass.InheritsFrom(TAbstractList) then
        Result := ReadIList(AClass)
      else if AClass.InheritsFrom(TAbstractMap) then
        Result := ReadListAsIMap(AClass)
      else
        Result := nil;
    htMap:
      if AClass.InheritsFrom(TAbstractMap) then
        Result := ReadIMap(AClass)
      else
        Result := ReadMapAsInterface(AClass, IID);
    htClass: begin
      ReadClass;
      Result := ReadInterface(AClass, IID);
    end;
    htObject: begin
      if AClass.InheritsFrom(TAbstractMap) then
        Result := ReadObjectAsIMap(TMapClass(AClass))
      else
        Result := ReadObjectAsInterface(AClass, IID);
    end;
    htRef: Result := ReadRef;
  else
    raise CastError(TagToString(Tag), 'Interface');
  end;
end;

function THproseReader.ReadObject(AClass: TClass): TObject;
var
{$IFDEF Supports_Rtti}
  ClassName: string;
  Info: PTypeInfo;
{$ENDIF}
  Tag: Byte;
begin
{$IFDEF Supports_Rtti}
  ClassName := AClass.ClassName;
  Info := PTypeInfo(AClass.ClassInfo);
{$ENDIF}
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    htNull,
    htEmpty: Result := nil;
    htList:
      if AClass.InheritsFrom(TAbstractList) then
        Result := ReadList(AClass)
      else if AClass.InheritsFrom(TAbstractMap) then
        Result := ReadListAsMap(AClass)
{$IFDEF Supports_Rtti}
      else if AnsiStartsText('TList<', ClassName) or
        AnsiStartsText('TObjectList<', ClassName) then
        Result := ReadTList(Info)
      else if AnsiStartsText('TQueue<', ClassName) or
        AnsiStartsText('TObjectQueue<', ClassName) then
        Result := ReadTQueue(Info)
      else if AnsiStartsText('TStack<', ClassName) or
        AnsiStartsText('TObjectStack<', ClassName) then
        Result := ReadTStack(Info)
{$ENDIF}
      else
        Result := nil;
    htMap:
      if AClass.InheritsFrom(TAbstractMap) then
        Result := ReadMap(AClass)
{$IFDEF Supports_Rtti}
      else if AnsiStartsText('TDictionary<', ClassName) or
        AnsiStartsText('TObjectDictionary<', ClassName) then
        Result := ReadTDictionary(Info)
{$ENDIF}
      else
        Result := ReadMapAsObject(AClass);
    htClass: begin
      ReadClass;
      Result := ReadObject(AClass);
    end;
    htObject: begin
      if AClass.InheritsFrom(TAbstractMap) then
        Result := ReadObjectAsMap(TMapClass(AClass))
      else
        Result := ReadObjectWithoutTag(AClass);
    end;
    htRef: Result := VarToObj(ReadRef);
  else
    raise CastError(TagToString(Tag), 'Object');
  end;
end;

{$IFDEF Supports_Generics}
procedure THproseReader.ReadArray<T>(var DynArray: TArray<T>; Info: PTypeInfo);
var
  Count, I: Integer;
begin
  Count := ReadInt(HproseTagOpenbrace);
  SetLength(DynArray, Count);
  FRefer.SetRef(NativeInt(Pointer(DynArray)));
  for I := 0 to Count - 1 do Unserialize(Info, DynArray[I]);
  CheckTag(HproseTagClosebrace);
end;

procedure THproseReader.ReadArray(Info: PTypeInfo; out DynArray);
var
  Name, ElementName: string;
  ElementInfo: PTypeInfo;
begin
  Name := GetTypeName(Info);
  ElementName := GetElementName(Name);
  ElementInfo := THproseClassManager.TypeInfo(ElementName);
  if ElementInfo = nil then
    raise EHproseException.Create(ElementName + 'is not registered');
  case ElementInfo^.Kind of
{$IFNDEF NEXTGEN}
    tkString: ReadArray<ShortString>(TArray<ShortString>(DynArray), ElementInfo);
    tkLString: ReadArray<AnsiString>(TArray<AnsiString>(DynArray), ElementInfo);
    tkWString: ReadArray<WideString>(TArray<WideString>(DynArray), ElementInfo);
{$ELSE}
    tkWString: ReadArray<string>(TArray<string>(DynArray), ElementInfo);
{$ENDIF}
    tkUString: ReadArray<UnicodeString>(TArray<UnicodeString>(DynArray), ElementInfo);
    tkVariant: ReadArray<Variant>(TArray<Variant>(DynArray), ElementInfo);
    tkDynArray: ReadArray<TArray<Pointer>>(TArray<TArray<Pointer>>(DynArray), ElementInfo);
    tkInterface: ReadArray<IInterface>(TArray<IInterface>(DynArray), ElementInfo);
    tkClass: ReadArray<TObject>(TArray<TObject>(DynArray), ElementInfo);
  else
    case GetTypeSize(ElementInfo) of
      1: ReadArray<TB1>(TArray<TB1>(DynArray), ElementInfo);
      2: ReadArray<TB2>(TArray<TB2>(DynArray), ElementInfo);
      4: ReadArray<TB4>(TArray<TB4>(DynArray), ElementInfo);
      8: ReadArray<TB8>(TArray<TB8>(DynArray), ElementInfo);
    else if GetTypeName(Info) = 'Extended' then
      ReadArray<Extended>(TArray<Extended>(DynArray), ElementInfo)
    else
      raise EHproseException.Create('Can not unserialize ' + Name);
    end;
  end;
end;

type
  PDynArrayRec = ^TDynArrayRec;
  TDynArrayRec = packed record
  {$IFDEF CPUX64}
    _Padding: LongInt; // Make 16 byte align for payload..
  {$ENDIF}
    RefCnt: LongInt;
    Length: NativeInt;
  end;

procedure DynArrayAddRef(P: Pointer);
begin
  if P <> nil then
    Inc(PDynArrayRec(PByte(P) - SizeOf(TDynArrayRec))^.RefCnt);
end;

procedure THproseReader.ReadDynArray(Info: PTypeInfo; out DynArray);
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    htNull,
    htEmpty: Pointer(DynArray) := nil;
    htList: ReadArray(Info, DynArray);
    htRef: begin
      Pointer(DynArray) := Pointer(NativeInt(ReadRef));
      DynArrayAddRef(Pointer(DynArray));
    end;
  else
    raise CastError(TagToString(Tag), 'DynArray');
  end;
end;

{$IFDEF Supports_Rtti}
function THproseReader.ReadTList<T>(Info, ElementInfo: PTypeInfo): TList<T>;
var
  Count, I: Integer;
  AClass: TClass;
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
begin
  Count := ReadInt(HproseTagOpenbrace);
  AClass := GetTypeData(Info)^.ClassType;
  Context := TRttiContext.Create;
  RttiType := Context.GetType(AClass);
  RttiMethod := RttiType.GetMethod('Create');
  Result := TList<T>(RttiMethod.Invoke(AClass, []).AsObject);
  RttiMethod.Free;
  RttiType.Free;
  Context.Free;
  Result.Count := Count;
  FRefer.SetRef(ObjToVar(Result));
  for I := 0 to Count - 1 do Result[I] := UnserializeTypeAsT<T>(ElementInfo);
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadTList(Info: PTypeInfo): TObject;
var
  Name, ElementName: string;
  ElementInfo: PTypeInfo;
begin
  Name := GetTypeName(Info);
  ElementName := GetElementName(Name);
  ElementInfo := THproseClassManager.TypeInfo(ElementName);
  if ElementInfo = nil then
    raise EHproseException.Create(ElementName + 'is not registered');
  case ElementInfo^.Kind of
{$IFNDEF NEXTGEN}
    tkString: Result := ReadTList<ShortString>(Info, ElementInfo);
    tkLString: Result := ReadTList<AnsiString>(Info, ElementInfo);
    tkWString: Result := ReadTList<WideString>(Info, ElementInfo);
{$ELSE}
    tkWString: Result := ReadTList<string>(Info, ElementInfo);
{$ENDIF}
    tkUString: Result := ReadTList<UnicodeString>(Info, ElementInfo);
    tkVariant: Result := ReadTList<Variant>(Info, ElementInfo);
    tkDynArray: Result := ReadTList<TArray<Pointer>>(Info, ElementInfo);
    tkInterface: Result := ReadTList<IInterface>(Info, ElementInfo);
    tkClass: Result := ReadTList<TObject>(Info, ElementInfo);
  else
    case GetTypeSize(ElementInfo) of
      1: Result := ReadTList<TB1>(Info, ElementInfo);
      2: Result := ReadTList<TB2>(Info, ElementInfo);
      4: Result := ReadTList<TB4>(Info, ElementInfo);
      8: Result := ReadTList<TB8>(Info, ElementInfo);
    else if GetTypeName(Info) = 'Extended' then
      Result := ReadTList<Extended>(Info, ElementInfo)
    else
      raise EHproseException.Create('Can not unserialize ' + Name);
    end;
  end;
end;

function THproseReader.ReadTQueue<T>(Info, ElementInfo: PTypeInfo): TQueue<T>;
var
  Count, I: Integer;
  AClass: TClass;
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
begin
  Count := ReadInt(HproseTagOpenbrace);
  AClass := GetTypeData(Info)^.ClassType;
  Context := TRttiContext.Create;
  RttiType := Context.GetType(AClass);
  RttiMethod := RttiType.GetMethod('Create');
  Result := TQueue<T>(RttiMethod.Invoke(AClass, []).AsObject);
  RttiMethod.Free;
  RttiType.Free;
  Context.Free;
  FRefer.SetRef(ObjToVar(Result));
  for I := 1 to Count do Result.Enqueue(UnserializeTypeAsT<T>(ElementInfo));
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadTQueue(Info: PTypeInfo): TObject;
var
  Name, ElementName: string;
  ElementInfo: PTypeInfo;
begin
  Name := GetTypeName(Info);
  ElementName := GetElementName(Name);
  ElementInfo := THproseClassManager.TypeInfo(ElementName);
  if ElementInfo = nil then
    raise EHproseException.Create(ElementName + 'is not registered');
  case ElementInfo^.Kind of
{$IFNDEF NEXTGEN}
    tkString: Result := ReadTQueue<ShortString>(Info, ElementInfo);
    tkLString: Result := ReadTQueue<AnsiString>(Info, ElementInfo);
    tkWString: Result := ReadTQueue<WideString>(Info, ElementInfo);
{$ELSE}
    tkWString: Result := ReadTQueue<string>(Info, ElementInfo);
{$ENDIF}
    tkUString: Result := ReadTQueue<UnicodeString>(Info, ElementInfo);
    tkVariant: Result := ReadTQueue<Variant>(Info, ElementInfo);
    tkDynArray: Result := ReadTQueue<TArray<Pointer>>(Info, ElementInfo);
    tkInterface: Result := ReadTQueue<IInterface>(Info, ElementInfo);
    tkClass: Result := ReadTQueue<TObject>(Info, ElementInfo);
  else
    case GetTypeSize(ElementInfo) of
      1: Result := ReadTQueue<TB1>(Info, ElementInfo);
      2: Result := ReadTQueue<TB2>(Info, ElementInfo);
      4: Result := ReadTQueue<TB4>(Info, ElementInfo);
      8: Result := ReadTQueue<TB8>(Info, ElementInfo);
    else if GetTypeName(Info) = 'Extended' then
      Result := ReadTQueue<Extended>(Info, ElementInfo)
    else
      raise EHproseException.Create('Can not unserialize ' + Name);
    end;
  end;
end;

function THproseReader.ReadTStack<T>(Info, ElementInfo: PTypeInfo): TStack<T>;
var
  Count, I: Integer;
  AClass: TClass;
begin
  Count := ReadInt(HproseTagOpenbrace);
  AClass := GetTypeData(Info)^.ClassType;
  Result := TStack<T>(AClass.Create);
  FRefer.SetRef(ObjToVar(Result));
  for I := 1 to Count do Result.Push(UnserializeTypeAsT<T>(ElementInfo));
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadTStack(Info: PTypeInfo): TObject;
var
  Name, ElementName: string;
  ElementInfo: PTypeInfo;
begin
  Name := GetTypeName(Info);
  ElementName := GetElementName(Name);
  ElementInfo := THproseClassManager.TypeInfo(ElementName);
  if ElementInfo = nil then
    raise EHproseException.Create(ElementName + 'is not registered');
  case ElementInfo^.Kind of
{$IFNDEF NEXTGEN}
    tkString: Result := ReadTStack<ShortString>(Info, ElementInfo);
    tkLString: Result := ReadTStack<AnsiString>(Info, ElementInfo);
    tkWString: Result := ReadTStack<WideString>(Info, ElementInfo);
{$ELSE}
    tkWString: Result := ReadTStack<string>(Info, ElementInfo);
{$ENDIF}
    tkUString: Result := ReadTStack<UnicodeString>(Info, ElementInfo);
    tkVariant: Result := ReadTStack<Variant>(Info, ElementInfo);
    tkDynArray: Result := ReadTStack<TArray<Pointer>>(Info, ElementInfo);
    tkInterface: Result := ReadTStack<IInterface>(Info, ElementInfo);
    tkClass: Result := ReadTStack<TObject>(Info, ElementInfo);
  else
    case GetTypeSize(ElementInfo) of
      1: Result := ReadTStack<TB1>(Info, ElementInfo);
      2: Result := ReadTStack<TB2>(Info, ElementInfo);
      4: Result := ReadTStack<TB4>(Info, ElementInfo);
      8: Result := ReadTStack<TB8>(Info, ElementInfo);
    else if GetTypeName(Info) = 'Extended' then
      Result := ReadTStack<Extended>(Info, ElementInfo)
    else
      raise EHproseException.Create('Can not unserialize ' + Name);
    end;
  end;
end;

function THproseReader.ReadTDictionary2<TKey, TValue>(
  Info, KeyInfo, ValueInfo: PTypeInfo): TDictionary<TKey, TValue>;
var
  Count, I: Integer;
  Key: TKey;
  Value: TValue;
  AClass: TClass;
  Context: TRttiContext;
  RttiType: TRttiType;
  RttiMethod: TRttiMethod;
begin
  Count := ReadInt(HproseTagOpenbrace);
  AClass := GetTypeData(Info)^.ClassType;
  Context := TRttiContext.Create;
  RttiType := Context.GetType(AClass);
  RttiMethod := RttiType.GetMethod('Create');
  Result := TDictionary<TKey, TValue>(RttiMethod.Invoke(AClass, [Count]).AsObject);
  RttiMethod.Free;
  RttiType.Free;
  Context.Free;
  FRefer.SetRef(ObjToVar(Result));
  for I := 1 to Count do begin
    Unserialize(KeyInfo, Key);
    Unserialize(ValueInfo, Value);
    Result.Add(Key, Value);
  end;
  CheckTag(HproseTagClosebrace);
end;

function THproseReader.ReadTDictionary1<TKey>(Info, KeyInfo,
  ValueInfo: PTypeInfo): TObject;
begin
  case ValueInfo^.Kind of
{$IFNDEF NEXTGEN}
    tkString: Result := ReadTDictionary2<TKey, ShortString>(
                Info, KeyInfo, ValueInfo);
    tkLString: Result := ReadTDictionary2<TKey, AnsiString>(
                 Info, KeyInfo, ValueInfo);
    tkWString: Result := ReadTDictionary2<TKey, WideString>(
                 Info, KeyInfo, ValueInfo);
{$ELSE}
    tkWString: Result := ReadTDictionary2<TKey, string>(
                 Info, KeyInfo, ValueInfo);
{$ENDIF}
    tkUString: Result := ReadTDictionary2<TKey, UnicodeString>(
                 Info, KeyInfo, ValueInfo);
    tkVariant: Result := ReadTDictionary2<TKey, Variant>(
                 Info, KeyInfo, ValueInfo);
    tkDynArray: Result := ReadTDictionary2<TKey, TArray<Pointer>>(
                  Info, KeyInfo, ValueInfo);
    tkInterface: Result := ReadTDictionary2<TKey, IInterface>(
                   Info, KeyInfo, ValueInfo);
    tkClass: Result := ReadTDictionary2<TKey, TObject>(
               Info, KeyInfo, ValueInfo);
  else
    case GetTypeSize(ValueInfo) of
      1: Result := ReadTDictionary2<TKey, TB1>(
           Info, KeyInfo, ValueInfo);
      2: Result := ReadTDictionary2<TKey, TB2>(
           Info, KeyInfo, ValueInfo);
      4: Result := ReadTDictionary2<TKey, TB4>(
           Info, KeyInfo, ValueInfo);
      8: Result := ReadTDictionary2<TKey, TB8>(
           Info, KeyInfo, ValueInfo);
    else if GetTypeName(ValueInfo) = 'Extended' then
      Result := ReadTDictionary2<TKey, Extended>(
        Info, KeyInfo, ValueInfo)
    else
      raise EHproseException.Create('Can not unserialize ' + GetTypeName(Info));
    end;
  end;
end;

function THproseReader.ReadTDictionary(Info: PTypeInfo): TObject;
var
  Name, KeyName, ValueName: string;
  KeyInfo, ValueInfo: PTypeInfo;
begin
  Name := GetTypeName(Info);
  SplitKeyValueTypeName(GetElementName(Name), KeyName, ValueName);
  KeyInfo := THproseClassManager.TypeInfo(KeyName);
  ValueInfo := THproseClassManager.TypeInfo(ValueName);
  if KeyInfo = nil then
    raise EHproseException.Create(KeyName + 'is not registered');
  if ValueInfo = nil then
    raise EHproseException.Create(ValueName + 'is not registered');
  case KeyInfo^.Kind of
{$IFNDEF NEXTGEN}
    tkString: Result := ReadTDictionary1<ShortString>(Info, KeyInfo, ValueInfo);
    tkLString: Result := ReadTDictionary1<AnsiString>(Info, KeyInfo, ValueInfo);
    tkWString: Result := ReadTDictionary1<WideString>(Info, KeyInfo, ValueInfo);
{$ELSE}
    tkWString: Result := ReadTDictionary1<string>(Info, KeyInfo, ValueInfo);
{$ENDIF}
    tkUString: Result := ReadTDictionary1<UnicodeString>(Info, KeyInfo, ValueInfo);
    tkVariant: Result := ReadTDictionary1<Variant>(Info, KeyInfo, ValueInfo);
    tkDynArray: Result := ReadTDictionary1<TArray<Pointer>>(Info, KeyInfo, ValueInfo);
    tkInterface: Result := ReadTDictionary1<IInterface>(Info, KeyInfo, ValueInfo);
    tkClass: Result := ReadTDictionary1<TObject>(Info, KeyInfo, ValueInfo);
  else
    case GetTypeSize(KeyInfo) of
      1: Result := ReadTDictionary1<TB1>(Info, KeyInfo, ValueInfo);
      2: Result := ReadTDictionary1<TB2>(Info, KeyInfo, ValueInfo);
      4: Result := ReadTDictionary1<TB4>(Info, KeyInfo, ValueInfo);
      8: Result := ReadTDictionary1<TB8>(Info, KeyInfo, ValueInfo);
    else if GetTypeName(KeyInfo) = 'Extended' then
      Result := ReadTDictionary1<Extended>(Info, KeyInfo, ValueInfo)
    else
      raise EHproseException.Create('Can not unserialize ' + Name);
    end;
  end;
end;

function THproseReader.UnserializeTypeAsT<T>(Info: PTypeInfo): T;
begin
  Unserialize(Info, Result);
end;
{$ENDIF}

function THproseReader.ReadSmartObject(Info: PTypeInfo): ISmartObject;
var
  Name, ElementName: string;
  ElementInfo: PTypeInfo;
  AObject: TObject;
begin
  Name := GetTypeName(Info);
  if not IsSmartObject(Name) then
    raise EHproseException.Create(Name + ' is not a ISmartObject interface');
  ElementName := GetElementName(Name);
  Name := 'TSmartObject<' + ElementName + '>';
  Info := THproseClassManager.TypeInfo(Name);
  ElementInfo := THproseClassManager.TypeInfo(ElementName);
  if (Info = nil) or (ElementInfo = nil) then
    raise EHproseException.Create(ElementName + 'is not registered');
  if ElementInfo^.Kind <> tkClass then
    raise EHproseException.Create(ElementName + 'is not a Class');
  AObject := ReadObject(GetTypeData(ElementInfo)^.ClassType);
  Result := TSmartClass(GetTypeData(Info)^.ClassType).Create(AObject) as ISmartObject;
end;

procedure THproseReader.Unserialize(Info: PTypeInfo; out Value);
var
  TypeData: PTypeData;
  TypeName: string;
  AClass: TClass;
begin
  TypeName := GetTypeName(Info);
  if TypeName = 'Boolean' then
    Boolean(Value) := ReadBoolean
  else if (TypeName = 'TDateTime') or
          (TypeName = 'TDate') or
          (TypeName = 'TTime') then
    TDateTime(Value) := ReadDateTime
{$IFDEF DELPHI2009_UP}
  else if TypeName = 'UInt64' then
    UInt64(Value) := ReadUInt64
{$ENDIF}
  else begin
    TypeData := GetTypeData(Info);
    case Info^.Kind of
      tkInteger, tkEnumeration, tkSet:
        case TypeData^.OrdType of
          otSByte:
            ShortInt(Value) := ShortInt(ReadInteger);
          otUByte:
            Byte(Value) := Byte(ReadInteger);
          otSWord:
            SmallInt(Value) := SmallInt(ReadInteger);
          otUWord:
            Word(Value) := Word(ReadInteger);
          otSLong:
            Integer(Value) := ReadInteger;
          otULong:
            LongWord(Value) := LongWord(ReadInt64);
        end;
{$IFNDEF NEXTGEN}
      tkChar:
        AnsiChar(Value) := AnsiChar(ReadUTF8Char);
{$ENDIF}
      tkWChar:
        WideChar(Value) := ReadUTF8Char;
{$IFDEF FPC}
      tkBool:
        Boolean(Value) := ReadBoolean;
      tkQWord:
        QWord(Value) := ReadUInt64;
{$ENDIF}
      tkFloat:
        case TypeData^.FloatType of
          ftSingle:
            Single(Value) := ReadExtended;
          ftDouble:
            Double(Value) := ReadExtended;
          ftExtended:
            Extended(Value) := ReadExtended;
          ftComp:
            Comp(Value) := ReadInt64;
          ftCurr:
            Currency(Value) := ReadCurrency;
        end;
{$IFNDEF NEXTGEN}
      tkString:
        ShortString(Value) := ShortString(ReadString());
      tkLString{$IFDEF FPC}, tkAString{$ENDIF}:
        AnsiString(Value) := AnsiString(ReadString);
      tkWString:
        WideString(Value) := ReadString;
{$ELSE}
      tkWString:
        string(Value) := ReadString;
{$ENDIF}
{$IFDEF DELPHI2009_UP}
      tkUString:
        UnicodeString(Value) := UnicodeString(ReadString);
{$ENDIF}
      tkInt64:
        Int64(Value) := ReadInt64;
      tkInterface: begin
        AClass := GetClassByInterface(TypeData^.Guid);
        if AClass = nil then
          raise EHproseException.Create(GetTypeName(Info) + ' is not registered')
        else if Supports(AClass, ISmartObject) then
          ISmartObject(Value) := ReadSmartObject(Info)
        else
          IInterface(Value) := ReadInterface(AClass, TypeData^.Guid);
      end;
      tkDynArray:
        ReadDynArray(Info, Value);
      tkClass: begin
        AClass := TypeData^.ClassType;
        TObject(Value) := ReadObject(AClass);
      end;
    end;
  end;
end;

function THproseReader.Unserialize<T>: T;
begin
  Unserialize(TypeInfo(T), Result);
end;

{$ENDIF}

function THproseReader.Unserialize: Variant;
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag of
    Byte('0')..Byte('9'): Result := Tag - Byte('0');
    htInteger: Result := ReadIntegerWithoutTag;
    htLong: Result := ReadLongWithoutTag;
    htDouble: Result := ReadDoubleWithoutTag;
    htNaN: Result := NaN;
    htInfinity: Result := ReadInfinityWithoutTag;
    htTrue: Result := True;
    htFalse: Result := False;
    htNull: Result := Null;
    htEmpty: Result := '';
    htDate: Result := ReadDateWithoutTag;
    htTime: Result := ReadTimeWithoutTag;
    htBytes: Result := ReadBytesWithoutTag;
{$IFNDEF NEXTGEN}
    htUTF8Char: Result := WideString(ReadUTF8CharWithoutTag);
{$ELSE}
    htUTF8Char: Result := string(ReadUTF8CharWithoutTag);
{$ENDIF}
    htString: Result := ReadStringWithoutTag;
    htGuid: Result := ReadGuidWithoutTag;
    htList: Result := ReadListWithoutTag;
    htMap: Result := ReadMapWithoutTag;
    htClass: begin
      ReadClass;
      Result := Unserialize;
    end;
    htObject: Result := ReadObjectWithoutTag;
    htRef: Result := ReadRef;
  else
    raise EHproseException.Create(TagToString(Tag) + 'can''t unserialize');
  end;
end;

function THproseReader.Unserialize(Info: PTypeInfo): Variant;
var
  TypeData: PTypeData;
  TypeName: string;
  AClass: TClass;
begin
  if Info = nil then Result := Unserialize
  else begin
    Result := Unassigned;
    TypeName := GetTypeName(Info);
    if TypeName = 'Boolean' then
      Result := ReadBoolean
    else if (TypeName = 'TDateTime') or
            (TypeName = 'TDate') or
            (TypeName = 'TTime') then
      Result := ReadDateTime
{$IFDEF DELPHI2009_UP}
    else if TypeName = 'UInt64' then
      Result := ReadUInt64
{$ENDIF}
    else begin
      TypeData := GetTypeData(Info);
      case Info^.Kind of
        tkInteger, tkEnumeration, tkSet:
          case TypeData^.OrdType of
            otSByte:
              Result := ShortInt(ReadInteger);
            otUByte:
              Result := Byte(ReadInteger);
            otSWord:
              Result := SmallInt(ReadInteger);
            otUWord:
              Result := Word(ReadInteger);
            otSLong:
              Result := ReadInteger;
            otULong:
              Result := LongWord(ReadInt64);
          end;
{$IFNDEF NEXTGEN}
        tkChar:
          Result := AnsiChar(ReadUTF8Char);
{$ENDIF}
        tkWChar:
          Result := ReadUTF8Char;
{$IFDEF FPC}
        tkBool:
          Result := ReadBoolean;
        tkQWord:
          Result := ReadUInt64;
{$ENDIF}
        tkFloat:
          case TypeData^.FloatType of
            ftSingle:
              Result := VarAsType(ReadExtended, varSingle);
            ftDouble, ftExtended:
              Result := ReadExtended;
            ftComp:
              Result := ReadInt64;
            ftCurr:
              Result := ReadCurrency;
          end;
{$IFNDEF NEXTGEN}
        tkString:
          Result := ShortString(ReadString());
        tkLString{$IFDEF FPC}, tkAString{$ENDIF}:
          Result := AnsiString(ReadString);
        tkWString:
          Result := ReadString;
{$ENDIF}
{$IFDEF Supports_Unicode}
        tkUString:
          Result := UnicodeString(ReadString);
{$ENDIF}
        tkInt64:
          Result := ReadInt64;
        tkInterface: begin
          AClass := GetClassByInterface(TypeData^.Guid);
          if AClass = nil then
            raise EHproseException.Create(GetTypeName(Info) + ' is not registered');
          Result := ReadInterface(AClass, TypeData^.Guid);
        end;
        tkDynArray:
          Result := ReadDynArray(TypeData^.varType and not varArray);
        tkClass: begin
          AClass := TypeData^.ClassType;
          Result := ObjToVar(ReadObject(AClass));
        end;
      end;
    end;
  end;
end;

function THproseReader.ReadRaw: TBytes;
var
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create;
  try
    ReadRaw(Stream);
    Result := Stream.Bytes;
    SetLength(Result, Stream.Size);
  finally
    Stream.Free;
  end
end;

procedure THproseReader.ReadRaw(const OStream: TStream);
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  ReadRaw(OStream, Tag);
end;

procedure THproseReader.ReadRaw(const OStream: TStream; Tag: Byte);
begin
  OStream.WriteBuffer(Tag, 1);
  case Tag of
    Byte('0')..Byte('9'),
    htNull,
    htEmpty,
    htTrue,
    htFalse,
    htNaN: begin end;
    htInfinity: ReadInfinityRaw(OStream);
    htInteger,
    htLong,
    htDouble,
    htRef: ReadNumberRaw(OStream);
    htDate,
    htTime: ReadDateTimeRaw(OStream);
    htUTF8Char: ReadUTF8CharRaw(OStream);
    htBytes: ReadBytesRaw(OStream);
    htString: ReadStringRaw(OStream);
    htGuid: ReadGuidRaw(OStream);
    htList,
    htMap,
    htObject: ReadComplexRaw(OStream);
    htClass: begin
      ReadComplexRaw(OStream);
      ReadRaw(OStream);
    end;
    htError: begin
      ReadRaw(OStream);
    end;
  else
    raise EHproseException.Create('Unexpected serialize tag "' +
                                  Char(Tag) + '" in stream');
  end;
end;

procedure THproseReader.ReadInfinityRaw(const OStream: TStream);
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  OStream.WriteBuffer(Tag, 1);
end;

procedure THproseReader.ReadNumberRaw(const OStream: TStream);
var
  Tag: Byte;
begin
  repeat
    FStream.ReadBuffer(Tag, 1);
    OStream.WriteBuffer(Tag, 1);
  until (Tag = HproseTagSemicolon);
end;

procedure THproseReader.ReadDateTimeRaw(const OStream: TStream);
var
  Tag: Byte;
begin
  repeat
    FStream.ReadBuffer(Tag, 1);
    OStream.WriteBuffer(Tag, 1);
  until (Tag = HproseTagSemicolon) or
        (Tag = HproseTagUTC);
end;

procedure THproseReader.ReadUTF8CharRaw(const OStream: TStream);
var
  Tag: Byte;
begin
  FStream.ReadBuffer(Tag, 1);
  case Tag shr 4 of
    0..7: OStream.WriteBuffer(Tag, 1);
    12,13: begin
      OStream.WriteBuffer(Tag, 1);
      FStream.ReadBuffer(Tag, 1);
      OStream.WriteBuffer(Tag, 1);
    end;
    14: begin
      OStream.WriteBuffer(Tag, 1);
      FStream.ReadBuffer(Tag, 1);
      OStream.WriteBuffer(Tag, 1);
      FStream.ReadBuffer(Tag, 1);
      OStream.WriteBuffer(Tag, 1);
    end;
  else
    raise EHproseException.Create('bad unicode encoding at $' +
                                  IntToHex(Tag, 2));
  end;
end;

procedure THproseReader.ReadBytesRaw(const OStream: TStream);
var
  Tag: Byte;
  Len: Integer;
begin
  Len := 0;
  Tag := Byte('0');
  repeat
    Len := Len * 10 + (Tag - Byte('0'));
    FStream.ReadBuffer(Tag, 1);
    OStream.WriteBuffer(Tag, 1);
  until (Tag = HproseTagQuote);
  OStream.CopyFrom(FStream, Len + 1);
end;

procedure THproseReader.ReadStringRaw(const OStream: TStream);
var
  Tag: Byte;
  Len, I: Integer;
begin
  Len := 0;
  Tag := Byte('0');
  repeat
    Len := Len * 10 + (Tag - Byte('0'));
    FStream.ReadBuffer(Tag, 1);
    OStream.WriteBuffer(Tag, 1);
  until (Tag = HproseTagQuote);
  { When I = Len, Read & Write HproseTagQuote }
  for I := 0 to Len do begin
    FStream.ReadBuffer(Tag, 1);
    case Tag shr 4 of
      0..7: OStream.WriteBuffer(Tag, 1);
      12,13: begin
        OStream.WriteBuffer(Tag, 1);
        FStream.ReadBuffer(Tag, 1);
        OStream.WriteBuffer(Tag, 1);
      end;
      14: begin
        OStream.WriteBuffer(Tag, 1);
        FStream.ReadBuffer(Tag, 1);
        OStream.WriteBuffer(Tag, 1);
        FStream.ReadBuffer(Tag, 1);
        OStream.WriteBuffer(Tag, 1);
      end;
      15: begin
        if (Tag and $F) <= 4 then begin
          OStream.WriteBuffer(Tag, 1);
          FStream.ReadBuffer(Tag, 1);
          OStream.WriteBuffer(Tag, 1);
          FStream.ReadBuffer(Tag, 1);
          OStream.WriteBuffer(Tag, 1);
          FStream.ReadBuffer(Tag, 1);
          OStream.WriteBuffer(Tag, 1);
          Continue;
        end;
        raise EHproseException.Create('bad unicode encoding at $' +
                                      IntToHex(Tag, 2));
      end;
    else
      raise EHproseException.Create('bad unicode encoding at $' +
                                    IntToHex(Tag, 2));
    end;
  end;
end;

procedure THproseReader.ReadGuidRaw(const OStream: TStream);
begin
  OStream.CopyFrom(FStream, 38);
end;

procedure THproseReader.ReadComplexRaw(const OStream: TStream);
var
  Tag: Byte;
begin
  repeat
    FStream.ReadBuffer(Tag, 1);
    OStream.WriteBuffer(Tag, 1);
  until (Tag = HproseTagOpenbrace);
  FStream.ReadBuffer(Tag, 1);
  while (Tag <> HproseTagClosebrace) do begin
    ReadRaw(OStream, Tag);
    FStream.ReadBuffer(Tag, 1);
  end;
  OStream.WriteBuffer(Tag, 1);
end;

procedure THproseReader.Reset;
begin
  FRefer.Reset;
  FClassRefList.Clear;
  FAttrRefMap.Clear;
end;

type
  TFakeWriterRefer = class(TInterfacedObject, IWriterRefer)
  public
    procedure AddCount(Count: Integer);
    procedure SetRef(const V: Variant);
    function WriteRef(const V:Variant): Boolean;
    procedure Reset;
  end;

{ TFakeWriterRefer }

procedure TFakeWriterRefer.AddCount(Count: Integer);
begin
end;

procedure TFakeWriterRefer.SetRef(const V: Variant);
begin
end;

function TFakeWriterRefer.WriteRef(const V:Variant): Boolean;
begin
  Result := False;
end;

procedure TFakeWriterRefer.Reset;
begin
end;

type
  TRealWriterRefer = class(TInterfacedObject, IWriterRefer)
  private
    FRefList: IList;
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    procedure AddCount(Count: Integer);
    procedure SetRef(const V: Variant);
    function WriteRef(const V:Variant): Boolean;
    procedure Reset;
  end;

{ TRealWriterRefer }

constructor TRealWriterRefer.Create(AStream: TStream);
begin
  FStream := AStream;
  FRefList := THashedList.Create(False);
end;

procedure TRealWriterRefer.AddCount(Count: Integer);
begin
  FRefList.Count := FRefList.Count + Count;
end;

procedure TRealWriterRefer.SetRef(const V: Variant);
begin
  FRefList.Add(V);
end;

function TRealWriterRefer.WriteRef(const V:Variant): Boolean;
var
  Ref: Integer;
  Bytes: TBytes;
begin
  Ref := FRefList.IndexOf(V);
  Bytes := nil;
  if Ref > -1 then begin
    FStream.WriteBuffer(HproseTagRef, 1);
    Bytes := BytesOf(IntToStr(Ref));
    FStream.WriteBuffer(Bytes[0], Length(Bytes));
    FStream.WriteBuffer(HproseTagSemicolon, 1);
    Result := True;
  end
  else
    Result := False;
end;

procedure TRealWriterRefer.Reset;
begin
  FRefList.Clear;
end;

{ THproseWriter }

constructor THproseWriter.Create(AStream: TStream; Simple: Boolean);
begin
  FStream := AStream;
  if Simple then FRefer := TFakeWriterRefer.Create
  else FRefer := TRealWriterRefer.Create(AStream);
  FClassRefList := THashedList.Create(False);
end;

{$IFDEF BCB}
constructor THproseWriter.Create1(AStream: TStream);
begin
  Create(AStream);
end;
{$ENDIF}

procedure THproseWriter.Serialize(const Value: Variant);
var
  AList: IList;
  AMap: IMap;
  ASmartObject: ISmartObject;
  Obj: TObject;
  P: PVarData;
begin
  P := FindVarData(Value);
  case P^.VType and not varByRef of
    varEmpty, varNull :
      WriteNull;
    varBoolean :
      WriteBoolean(Value);
    varByte, varWord, varShortInt, varSmallint, varInteger:
      WriteInteger(Value);
{$IFDEF DELPHI2009_UP}
    varUInt64:
      WriteLong(UIntToStr(Value));
{$ENDIF}
    {$IFDEF FPC}varQWord, {$ENDIF}
    varLongWord, varInt64:
      WriteLong(VarToStr(Value));
    varSingle, varDouble:
      WriteDouble(Value);
    varCurrency:
      WriteCurrency(Value);
    varString, {$IFDEF Supports_Unicode}varUString, {$ENDIF}varOleStr:
      WriteWideString(Value);
    varDate:
      WriteDateTimeWithRef(Value);
    varUnknown:
      if (IInterface(Value) = nil) then
        WriteNull
      else if Supports(IInterface(Value), IList, AList) then
        WriteListWithRef(AList)
      else if Supports(IInterface(Value), IMap, AMap) then
        WriteMapWithRef(AMap)
      else if Supports(IInterface(Value), ISmartObject, ASmartObject) then
        WriteSmartObjectWithRef(ASmartObject)
      else
        WriteInterfaceWithRef(IInterface(Value));
  else
    if P^.VType and varArray = varArray then
      if (P^.VType and varTypeMask = varByte) and
         (VarArrayDimCount(Value) = 1) then
        WriteBytesWithRef(Value)
      else
        WriteArrayWithRef(Value)
    else if P^.VType and not varByRef = varObject then begin
      Obj := VarToObj(Value);
      if Obj = nil then WriteNull else WriteObjectWithRef(Obj);
    end
  end;
end;

procedure THproseWriter.Serialize(const Value: array of const);
begin
  WriteArray(Value);
end;

procedure THproseWriter.WriteRawBytes(const Bytes: TBytes);
begin
  FStream.WriteBuffer(Bytes[0], Length(Bytes));
end;

procedure THproseWriter.WriteArray(const Value: array of const);
var
  I, N: Integer;
  AList: IList;
  AMap: IMap;
  ASmartObject: ISmartObject;
  V: TVarRec;
begin
  FRefer.SetRef(Null);
  N := Length(Value);
  FStream.WriteBuffer(HproseTagList, 1);
  if N > 0 then WriteRawBytes(BytesOf(IntToStr(N)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  for I := 0 to N - 1 do begin
    V := Value[I];
    case V.VType of
      vtInteger:       WriteInteger(V.VInteger);
      vtBoolean:       WriteBoolean(V.VBoolean);
      vtExtended:      WriteDouble(V.VExtended^);
{$IFNDEF NEXTGEN}
      vtChar:          WriteUTF8Char(WideString(V.VChar)[1]);
      vtString:        WriteStringWithRef(WideString(V.VString^));
      vtPChar:         WriteStringWithRef(WideString(AnsiString(V.VPChar)));
      vtAnsiString:    WriteStringWithRef(WideString(AnsiString(V.VAnsiString)));
      vtPWideChar:     WriteStringWithRef(WideString(V.VPWideChar));
      vtWideString:    WriteStringWithRef(WideString(V.VWideString));
{$ELSE}
      vtPWideChar:     WriteStringWithRef(string(V.VPWideChar));
      vtWideString:    WriteStringWithRef(string(V.VWideString));
{$ENDIF}
      vtObject:
        if V.VObject = nil then WriteNull else WriteObjectWithRef(V.VObject);
      vtWideChar:      WriteUTF8Char(V.VWideChar);
      vtCurrency:      WriteCurrency(V.VCurrency^);
      vtVariant:       Serialize(V.VVariant^);
      vtInterface:
        if IInterface(V.VInterface) = nil then
          WriteNull
        else if Supports(IInterface(V.VInterface), IList, AList) then
          WriteListWithRef(AList)
        else if Supports(IInterface(V.VInterface), IMap, AMap) then
          WriteMapWithRef(AMap)
        else if Supports(IInterface(V.VInterface), ISmartObject, ASmartObject) then
          WriteSmartObjectWithRef(ASmartObject)
        else
          WriteInterfaceWithRef(IInterface(V.VInterface));
      vtInt64:         WriteLong(V.VInt64^);
{$IFDEF FPC}
      vtQWord:         WriteLong(V.VQWord^);
{$ENDIF}
{$IFDEF Supports_Unicode}
      vtUnicodeString: WriteStringWithRef(UnicodeString(V.VUnicodeString));
{$ENDIF}
    else
      WriteNull;
    end;
  end;
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteArray(const Value: Variant);
var
  PVar: PVarData;
  P: Pointer;
  Rank, Count, MaxRank, I, N: Integer;
  Des: array of array[0..1] of Integer;
  Loc, Len: array of Integer;
begin
  FRefer.SetRef(Value);
  PVar := FindVarData(Value);
  Rank := VarArrayDimCount(Value);
  if Rank = 1 then begin
    Count := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 1;
    FStream.WriteBuffer(HproseTagList, 1);
    if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
    FStream.WriteBuffer(HproseTagOpenbrace, 1);
    P := VarArrayLock(Value);
    case PVar.VType and varTypeMask of
      varInteger: WriteIntegerArray(P, Count);
      varShortInt: WriteShortIntArray(P, Count);
      varWord: WriteWordArray(P, Count);
      varSmallint: WriteSmallintArray(P, Count);
      varLongWord: WriteLongWordArray(P, Count);
      varSingle: WriteSingleArray(P, Count);
      varDouble: WriteDoubleArray(P, Count);
      varCurrency: WriteCurrencyArray(P, Count);
      varOleStr: WriteWideStringArray(P, Count);
      varBoolean: WriteBooleanArray(P, Count);
      varDate: WriteDateTimeArray(P, Count);
      varVariant: WriteVariantArray(P, Count);
    end;
    VarArrayUnLock(Value);
    FStream.WriteBuffer(HproseTagClosebrace, 1);
  end
  else begin
    SetLength(Des, Rank);
    SetLength(Loc, Rank);
    SetLength(Len, Rank);
    MaxRank := Rank - 1;
    for I := 0 to MaxRank do begin
      Des[I, 0] := VarArrayLowBound(Value, I + 1);
      Des[I, 1] := VarArrayHighBound(Value, I + 1);
      Loc[I] := Des[I, 0];
      Len[I] := Des[I, 1] - Des[I, 0] + 1;
    end;
    FStream.WriteBuffer(HproseTagList, 1);
    if Len[0] > 0 then WriteRawBytes(BytesOf(IntToStr(Len[0])));
    FStream.WriteBuffer(HproseTagOpenbrace, 1);
    while Loc[0] <= Des[0, 1] do begin
      N := 0;
      for I := Maxrank downto 1 do
        if Loc[I] = Des[I, 0] then Inc(N) else Break;
      for I := Rank - N to MaxRank do begin
        FRefer.SetRef(Null);
        FStream.WriteBuffer(HproseTagList, 1);
        if Len[I] > 0 then WriteRawBytes(BytesOf(IntToStr(Len[I])));
        FStream.WriteBuffer(HproseTagOpenbrace, 1);
      end;
      for I := Des[MaxRank, 0] to Des[MaxRank, 1] do begin
        Loc[MaxRank] := I;
        Serialize(VarArrayGet(Value, Loc));
      end;
      Inc(Loc[MaxRank]);
      for I := MaxRank downto 1 do
        if Loc[I] > Des[I, 1] then begin
          Loc[I] := Des[I, 0];
          Inc(Loc[I - 1]);
          FStream.WriteBuffer(HproseTagClosebrace, 1);
        end;
    end;
    FStream.WriteBuffer(HproseTagClosebrace, 1);
  end;
end;

procedure THproseWriter.WriteArrayWithRef(const Value: Variant);
begin
  if not FRefer.WriteRef(Value) then WriteArray(Value);
end;

procedure THproseWriter.WriteBoolean(B: Boolean);
begin
  FStream.WriteBuffer(HproseTagBoolean[B], 1);
end;

procedure THproseWriter.WriteBooleanArray(var P; Count: Integer);
var
  AP: PWordBoolArray absolute P;
  I: Integer;
begin
  for I := 0 to Count - 1 do WriteBoolean(AP^[I]);
end;

procedure THproseWriter.WriteBytes(const Bytes: Variant);
var
  N: Integer;
begin
  FRefer.SetRef(Bytes);
  N := VarArrayHighBound(Bytes, 1) - VarArrayLowBound(Bytes, 1) + 1;
  FStream.WriteBuffer(HproseTagBytes, 1);
  WriteRawBytes(BytesOf(IntToStr(N)));
  FStream.WriteBuffer(HproseTagQuote, 1);
  FStream.WriteBuffer(VarArrayLock(Bytes)^, N);
  VarArrayUnLock(Bytes);
  FStream.WriteBuffer(HproseTagQuote, 1);
end;

procedure THproseWriter.WriteBytesWithRef(const Bytes: Variant);
begin
  if not FRefer.WriteRef(Bytes) then WriteBytes(Bytes);
end;

function THproseWriter.WriteClass(const Instance: TObject): Integer;
var
  ClassAlias: string;
  PropName: string;
  PropList: PPropList;
  PropCount, I: Integer;
  CachePointer: PSerializeCache;
  CacheStream: TBytesStream;
  TempData: TBytes;
{$IFNDEF NEXTGEN}
  TempStr: WideString;
{$ENDIF}
begin
  ClassAlias := GetClassAlias(Instance.ClassType);
  if ClassAlias = '' then
    raise EHproseException.Create(Instance.ClassName + ' has not registered');
  PropertiesCache.Lock;
  try
    TempData := nil;
    CachePointer := PSerializeCache(NativeInt(PropertiesCache[ClassAlias]));
    if CachePointer = nil then begin
      New(CachePointer);
      try
        CachePointer^.RefCount := 0;
        CachePointer^.Data := nil;
        CacheStream := TBytesStream.Create;
        try
          PropCount := GetStoredPropList(Instance, PropList);
          try
            CacheStream.WriteBuffer(HproseTagClass, 1);
            TempData := BytesOf(IntToStr(Length(ClassAlias)));
            CacheStream.WriteBuffer(TempData[0], Length(TempData));
            CacheStream.WriteBuffer(HproseTagQuote, 1);
            Tempdata := BytesOf(ClassAlias);
            CacheStream.WriteBuffer(TempData[0], Length(TempData));
            CacheStream.WriteBuffer(HproseTagQuote, 1);
            if PropCount > 0 then begin
              Tempdata := BytesOf(IntToStr(PropCount));
              CacheStream.WriteBuffer(TempData[0], Length(TempData));
            end;
            CacheStream.WriteBuffer(HproseTagOpenbrace, 1);
            for I := 0 to PropCount - 1 do begin
{$IFDEF NEXTGEN}
              PropName := GetPropName(PropList^[I]);
{$ELSE}
              PropName := string(PropList^[I]^.Name);
{$ENDIF}
              if Byte(PropName[1]) in [Byte('A')..Byte('Z')] then
                PropName := Char(Byte(PropName[1]) + 32) + RightStr(PropName, Length(propName) - 1);
{$IFNDEF NEXTGEN}
              TempStr := WideString(PropName);
{$ENDIF}
              CacheStream.WriteBuffer(HproseTagString, 1);
{$IFNDEF NEXTGEN}
              Tempdata := BytesOf(IntToStr(Length(TempStr)));
{$ELSE}
              Tempdata := BytesOf(IntToStr(Length(PropName)));
{$ENDIF}
              CacheStream.WriteBuffer(TempData[0], Length(TempData));
              CacheStream.WriteBuffer(HproseTagQuote, 1);
{$IFDEF NEXTGEN}
              Tempdata := BytesOf(PropName);
{$ELSE}
              Tempdata := BytesOf(UTF8Encode(TempStr));
{$ENDIF}
              CacheStream.WriteBuffer(TempData[0], Length(TempData));
              CacheStream.WriteBuffer(HproseTagQuote, 1);
              Inc(CachePointer^.RefCount);
            end;
            CacheStream.WriteBuffer(HproseTagClosebrace, 1);
          finally
            FreeMem(PropList);
          end;
          CachePointer^.Data := CacheStream.Bytes;
          SetLength(CachePointer^.Data, CacheStream.Size);
        finally
          CacheStream.Free;
        end;
      except
        Dispose(CachePointer);
      end;
      PropertiesCache[ClassAlias] := NativeInt(CachePointer);
    end;
  finally
    PropertiesCache.UnLock;
  end;
  FStream.WriteBuffer(CachePointer^.Data[0], Length(CachePointer^.Data));
  if CachePointer^.RefCount > 0 then
    FRefer.AddCount(CachePointer^.RefCount);
  Result := FClassRefList.Add(Instance.ClassName);
end;

procedure THproseWriter.WriteCurrency(C: Currency);
begin
  stream.WriteBuffer(HproseTagDouble, 1);
  WriteRawBytes(BytesOf(CurrToStr(C)));
  stream.WriteBuffer(HproseTagSemicolon, 1);
end;

procedure THproseWriter.WriteCurrencyArray(var P; Count: Integer);
var
  AP: PCurrencyArray absolute P;
  I: Integer;
begin
  for I := 0 to Count - 1 do WriteCurrency(AP^[I]);
end;

procedure THproseWriter.WriteDateTimeArray(var P; Count: Integer);
var
  AP: PDateTimeArray absolute P;
  I: Integer;
begin
  for I := 0 to Count - 1 do WriteDateTimeWithRef(AP^[I]);
end;

procedure THproseWriter.WriteDateTime(const ADateTime: TDateTime);
var
  ADate, ATime, AMillisecond: string;
begin
  FRefer.SetRef(ADateTime);
  ADate := FormatDateTime('yyyymmdd', ADateTime);
  ATime := FormatDateTime('hhnnss', ADateTime);
  AMillisecond := FormatDateTime('zzz', ADateTime);
  if (ATime = '000000') and (AMillisecond = '000') then begin
    FStream.WriteBuffer(HproseTagDate, 1);
    WriteRawBytes(BytesOf(ADate));
  end
  else if ADate = '18991230' then begin
    FStream.WriteBuffer(HproseTagTime, 1);
    WriteRawBytes(BytesOf(ATime));
    if AMillisecond <> '000' then begin
      FStream.WriteBuffer(HproseTagPoint, 1);
      WriteRawBytes(BytesOf(AMillisecond));
    end;
  end
  else begin
    FStream.WriteBuffer(HproseTagDate, 1);
    WriteRawBytes(BytesOf(ADate));
    FStream.WriteBuffer(HproseTagTime, 1);
    WriteRawBytes(BytesOf(ATime));
    if AMillisecond <> '000' then begin
      FStream.WriteBuffer(HproseTagPoint, 1);
      WriteRawBytes(BytesOf(AMillisecond));
    end;
  end;
  FStream.WriteBuffer(HproseTagSemicolon, 1);
end;

procedure THproseWriter.WriteDateTimeWithRef(const ADateTime: TDateTime);
begin
  if not FRefer.WriteRef(ADateTime) then WriteDateTime(ADateTime);
end;

procedure THproseWriter.WriteDouble(D: Extended);
begin
  if IsNaN(D) then
    WriteNaN
  else if IsInfinite(D) then
    WriteInfinity(Sign(D) = 1)
  else begin
    stream.WriteBuffer(HproseTagDouble, 1);
    WriteRawBytes(BytesOf(FloatToStr(D)));
    stream.WriteBuffer(HproseTagSemicolon, 1);
  end;
end;

procedure THproseWriter.WriteDoubleArray(var P; Count: Integer);
var
  AP: PDoubleArray absolute P;
  I: Integer;
begin
  for I := 0 to Count - 1 do WriteDouble(AP^[I]);
end;

procedure THproseWriter.WriteInfinity(Positive: Boolean);
begin
  FStream.WriteBuffer(HproseTagInfinity, 1);
  FStream.WriteBuffer(HproseTagSign[Positive], 1);
end;

procedure THproseWriter.WriteInteger(I: Integer);
var
  C: Byte;
begin
  if (I >= 0) and (I <= 9) then begin
    C := Byte(I + Byte('0'));
    FStream.WriteBuffer(C, 1);
  end
  else begin
    FStream.WriteBuffer(HproseTagInteger, 1);
    WriteRawBytes(BytesOf(IntToStr(I)));
    FStream.WriteBuffer(HproseTagSemicolon, 1);
  end;
end;

procedure THproseWriter.WriteIntegerArray(var P; Count: Integer);
var
  AP: PIntegerArray absolute P;
  I: Integer;
begin
  for I := 0 to Count - 1 do WriteInteger(AP^[I]);
end;

procedure THproseWriter.WriteList(const AList: IList);
var
  Count, I: Integer;
begin
  FRefer.SetRef(AList);
  Count := AList.Count;
  FStream.WriteBuffer(HproseTagList, 1);
  if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  for I := 0 to Count - 1 do Serialize(AList[I]);
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteListWithRef(const AList: IList);
begin
  if not FRefer.WriteRef(AList) then WriteList(AList);
end;

procedure THproseWriter.WriteList(const AList: TAbstractList);
var
  Count, I: Integer;
begin
  FRefer.SetRef(ObjToVar(AList));
  Count := AList.Count;
  FStream.WriteBuffer(HproseTagList, 1);
  if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  for I := 0 to Count - 1 do Serialize(AList[I]);
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteLong(const L: string);
var
  C: Byte;
begin
  if (Length(L) = 1) and (Byte(L[1]) in [Byte('0')..Byte('9')]) then begin
    C := Byte(L[1]);
    FStream.WriteBuffer(C, 1)
  end
  else begin
    FStream.WriteBuffer(HproseTagLong, 1);
    WriteRawBytes(BytesOf(L));
    FStream.WriteBuffer(HproseTagSemicolon, 1);
  end;
end;

procedure THproseWriter.WriteLong(L: Int64);
var
  C: Byte;
begin
  if (L >= 0) and (L <= 9) then begin
    C := Byte(L + Byte('0'));
    FStream.WriteBuffer(C, 1);
  end
  else begin
    FStream.WriteBuffer(HproseTagLong, 1);
    WriteRawBytes(BytesOf(IntToStr(L)));
    FStream.WriteBuffer(HproseTagSemicolon, 1);
  end;
end;

{$IFDEF DELPHI2009_UP}
procedure THproseWriter.WriteLong(L: UInt64);
var
  C: Byte;
begin
  if L <= 9 then begin
    C := Byte(L + Byte('0'));
    FStream.WriteBuffer(C, 1);
  end
  else begin
    FStream.WriteBuffer(HproseTagLong, 1);
    WriteRawBytes(BytesOf(UIntToStr(L)));
    FStream.WriteBuffer(HproseTagSemicolon, 1);
  end;
end;
{$ENDIF}

{$IFDEF FPC}
procedure THproseWriter.WriteLong(L: QWord);
var
  C: Byte;
begin
  if L <= 9 then begin
    C := Byte(L + Byte('0'));
    FStream.WriteBuffer(C, 1);
  end
  else begin
    FStream.WriteBuffer(HproseTagLong, 1);
    WriteRawBytes(BytesOf(IntToStr(L)));
    FStream.WriteBuffer(HproseTagSemicolon, 1);
  end;
end;
{$ENDIF}

procedure THproseWriter.WriteLongWordArray(var P; Count: Integer);
var
  AP: PLongWordArray absolute P;
  I: Integer;
begin
  for I := 0 to Count - 1 do WriteLong(AP^[I]);
end;

procedure THproseWriter.WriteMap(const AMap: IMap);
var
  Count, I: Integer;
begin
  FRefer.SetRef(AMap);
  Count := AMap.Count;
  FStream.WriteBuffer(HproseTagMap, 1);
  if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  for I := 0 to Count - 1 do begin
    Serialize(AMap.Keys[I]);
    Serialize(AMap.Values[I]);
  end;
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteMapWithRef(const AMap: IMap);
begin
  if not FRefer.WriteRef(AMap) then WriteMap(AMap);
end;

procedure THproseWriter.WriteMap(const AMap: TAbstractMap);
var
  Count, I: Integer;
begin
  FRefer.SetRef(ObjToVar(AMap));
  Count := AMap.Count;
  FStream.WriteBuffer(HproseTagMap, 1);
  if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  for I := 0 to Count - 1 do begin
    Serialize(AMap.Keys[I]);
    Serialize(AMap.Values[I]);
  end;
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteNaN;
begin
  FStream.WriteBuffer(HproseTagNaN, 1);
end;

procedure THproseWriter.WriteNull;
begin
  FStream.WriteBuffer(HproseTagNull, 1);
end;

procedure THproseWriter.WriteEmpty;
begin
  FStream.WriteBuffer(HproseTagEmpty, 1);
end;

procedure THproseWriter.WriteObject(const AObject: TObject);
var
  ClassRef: Integer;
  Value: Variant;
  PropList: PPropList;
  PropCount, I: Integer;
  ClassName: string;
begin
  ClassName := AObject.ClassName;
  if AObject is TAbstractList then WriteList(TAbstractList(AObject))
  else if AObject is TAbstractMap then WriteMap(TAbstractMap(AObject))
  else if AObject is TStrings then WriteStrings(TStrings(AObject))
  else
{$IFDEF Supports_Generics}
  if AnsiStartsText('TList<', ClassName) then
    WriteList(AObject)
  else if AnsiStartsText('TQueue<', ClassName) then
    WriteQueue(AObject)
  else if AnsiStartsText('TStack<', ClassName) then
    WriteStack(AObject)
  else if AnsiStartsText('TDictionary<', ClassName) then
    WriteDictionary(AObject)
  else if AnsiStartsText('TObjectList<', ClassName) then
    WriteObjectList(AObject)
  else if AnsiStartsText('TObjectQueue<', ClassName) then
    WriteObjectQueue(AObject)
  else if AnsiStartsText('TObjectStack<', ClassName) then
    WriteObjectStack(AObject)
  else if AnsiStartsText('TObjectDictionary<', ClassName) then
    WriteObjectDictionary(AObject)
  else
{$ENDIF}
  begin
    Value := ObjToVar(AObject);
    ClassRef := FClassRefList.IndexOf(ClassName);
    if ClassRef < 0 then ClassRef := WriteClass(AObject);
    FRefer.SetRef(Value);
    FStream.WriteBuffer(HproseTagObject, 1);
    WriteRawBytes(BytesOf(IntToStr(ClassRef)));
    FStream.WriteBuffer(HproseTagOpenbrace, 1);
    PropCount := GetStoredPropList(AObject, PropList);
    try
      for I := 0 to PropCount - 1 do
        Serialize(HproseCommon.GetPropValue(AObject, PropList^[I]));
    finally
      FreeMem(PropList);
    end;
    FStream.WriteBuffer(HproseTagClosebrace, 1);
  end;
end;

procedure THproseWriter.WriteObjectWithRef(const AObject: TObject);
begin
  if not FRefer.WriteRef(ObjToVar(AObject)) then WriteObject(AObject);
end;

procedure THproseWriter.WriteInterface(const Intf: IInterface);
var
  ClassRef: Integer;
  AObject: TObject;
  PropList: PPropList;
  PropCount, I: Integer;
begin
  AObject := IntfToObj(Intf);
  ClassRef := FClassRefList.IndexOf(AObject.ClassName);
  if ClassRef < 0 then ClassRef := WriteClass(AObject);
  FRefer.SetRef(Intf);
  FStream.WriteBuffer(HproseTagObject, 1);
  WriteRawBytes(BytesOf(IntToStr(ClassRef)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  PropCount := GetStoredPropList(AObject, PropList);
  try
    for I := 0 to PropCount - 1 do
      Serialize(HproseCommon.GetPropValue(AObject, PropList^[I]));
  finally
    FreeMem(PropList);
  end;
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteInterfaceWithRef(const Intf: IInterface);
begin
  if not FRefer.WriteRef(Intf) then WriteInterface(Intf);
end;

procedure THproseWriter.WriteSmartObject(const SmartObject: ISmartObject);
begin
  WriteObject(SmartObject.Value);
end;

procedure THproseWriter.WriteSmartObjectWithRef(const SmartObject: ISmartObject);
begin
  if not FRefer.WriteRef(ObjToVar(SmartObject.Value)) then begin
    if SmartObject.Value is TStrings then
      WriteStrings(TStrings(SmartObject.Value))
    else
      WriteObject(SmartObject.Value);
  end;
end;

procedure THproseWriter.WriteShortIntArray(var P; Count: Integer);
var
  AP: PShortIntArray absolute P;
  I: Integer;
begin
  for I := 0 to Count - 1 do WriteInteger(AP^[I]);
end;

procedure THproseWriter.WriteSingleArray(var P; Count: Integer);
var
  AP: PSingleArray absolute P;
  I: Integer;
begin
  for I := 0 to Count - 1 do WriteDouble(AP^[I]);
end;

procedure THproseWriter.WriteSmallIntArray(var P; Count: Integer);
var
  AP: PSmallIntArray absolute P;
  I: Integer;
begin
  for I := 0 to Count - 1 do WriteInteger(AP^[I]);
end;

procedure THproseWriter.WriteUTF8Char(C: WideChar);
begin
  FStream.WriteBuffer(HproseTagUTF8Char, 1);
{$IFDEF NEXTGEN}
  WriteRawBytes(BytesOf(string(C)));
{$ELSE}
  WriteRawBytes(BytesOf(UTF8Encode(WideString(C))));
{$ENDIF}
end;

{$IFDEF NEXTGEN}
procedure THproseWriter.WriteString(const S: string);
{$ELSE}
procedure THproseWriter.WriteString(const S: WideString);
{$ENDIF}
begin
  FRefer.SetRef(S);
  FStream.WriteBuffer(HproseTagString, 1);
  WriteRawBytes(BytesOf(IntToStr(Length(S))));
  FStream.WriteBuffer(HproseTagQuote, 1);
{$IFDEF NEXTGEN}
  WriteRawBytes(BytesOf(S));
{$ELSE}
  WriteRawBytes(BytesOf(UTF8Encode(S)));
{$ENDIF}
  FStream.WriteBuffer(HproseTagQuote, 1);
end;

{$IFDEF NEXTGEN}
procedure THproseWriter.WriteStringWithRef(const S: string);
{$ELSE}
procedure THproseWriter.WriteStringWithRef(const S: WideString);
{$ENDIF}
begin
  if not FRefer.WriteRef(S) then WriteString(S);
end;

procedure THproseWriter.WriteStrings(const SS: TStrings);
var
  Count, I: Integer;
begin
  FRefer.SetRef(ObjToVar(SS));
  Count := SS.Count;
  FStream.WriteBuffer(HproseTagList, 1);
  if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  for I := 0 to Count - 1 do WriteStringWithRef(SS[I]);
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteVariantArray(var P; Count: Integer);
var
  AP: PVariantArray absolute P;
  I: Integer;
begin
  for I := 0 to Count - 1 do Serialize(AP^[I]);
end;

{$IFDEF NEXTGEN}
procedure THproseWriter.WriteWideString(const Str: string);
{$ELSE}
procedure THproseWriter.WriteWideString(const Str: WideString);
{$ENDIF}
begin
  case Length(Str) of
    0: WriteEmpty;
    1: WriteUTF8Char(Str[1]);
  else
    WriteStringWithRef(Str);
  end;
end;

procedure THproseWriter.WriteWideStringArray(var P; Count: Integer);
var
  AP: PWideStringArray absolute P;
  I: Integer;
begin
  for I := 0 to Count - 1 do WriteWideString(AP^[I]);
end;

procedure THproseWriter.WriteWordArray(var P; Count: Integer);
var
  AP: PWordArray absolute P;
  I: Integer;
begin
  for I := 0 to Count - 1 do WriteInteger(AP^[I]);
end;

procedure THproseWriter.Reset;
begin
  FRefer.Reset;
  FClassRefList.Clear;
end;

{$IFDEF Supports_Generics}

procedure THproseWriter.Serialize(const Value; Info: PTypeInfo);
var
  TypeData: PTypeData;
  TypeName: string;
  AList: IList;
  AMap: IMap;
  ASmartObject: ISmartObject;
  Obj: TObject;
begin
  TypeName := GetTypeName(Info);
  if TypeName = 'Boolean' then
    WriteBoolean(Boolean(Value))
  else if (TypeName = 'TDateTime') or
          (TypeName = 'TDate') or
          (TypeName = 'TTime') then
    WriteDateTimeWithRef(TDateTime(Value))
  else if TypeName = 'UInt64' then
    WriteLong(UIntToStr(UInt64(Value)))
  else begin
    TypeData := GetTypeData(Info);
    case Info^.Kind of
      tkVariant: Serialize(Variant(Value));
      tkInteger, tkEnumeration, tkSet:
        case TypeData^.OrdType of
          otSByte: WriteInteger(ShortInt(Value));
          otUByte: WriteInteger(Byte(Value));
          otSWord: WriteInteger(SmallInt(Value));
          otUWord: WriteInteger(Word(Value));
          otSLong: WriteInteger(Integer(Value));
          otULong: WriteLong(UIntToStr(LongWord(Value)));
        end;
      tkWChar: WriteUTF8Char(WideChar(Value));
      tkFloat:
        case TypeData^.FloatType of
          ftSingle: WriteDouble(Single(Value));
          ftDouble: WriteDouble(Double(Value));
          ftExtended: WriteDouble(Extended(Value));
          ftComp: WriteLong(IntToStr(Int64(Value)));
          ftCurr: WriteCurrency(Currency(Value));
        end;
{$IFNDEF NEXTGEN}
      tkChar: WriteUTF8Char(WideString(Char(Value))[1]);
      tkString: WriteWideString(WideString(ShortString(Value)));
      tkLString: WriteWideString(WideString(AnsiString(Value)));
      tkWString: WriteWideString(WideString(Value));
      tkUString: WriteWideString(WideString(UnicodeString(Value)));
{$ELSE}
      tkWString: WriteWideString(string(Value));
      tkUString: WriteWideString(string(UnicodeString(Value)));
{$ENDIF}
      tkInt64: WriteLong(IntToStr(Int64(Value)));
      tkDynArray: WriteArrayWithRef(Value, Info);
      tkInterface: begin
        if IInterface(Value) = nil then
          WriteNull
        else if Supports(IInterface(Value), IList, AList) then
          WriteListWithRef(AList)
        else if Supports(IInterface(Value), IMap, AMap) then
          WriteMapWithRef(AMap)
        else if Supports(IInterface(Value), ISmartObject, ASmartObject) then
          WriteSmartObjectWithRef(ASmartObject)
        else
          WriteInterfaceWithRef(IInterface(Value));
      end;
      tkClass: begin
        Obj := TObject(Value);
        if Obj = nil then WriteNull else WriteObjectWithRef(Obj);
      end;
    end;
  end;
end;

procedure THproseWriter.WriteArray(const DynArray; const Name: string);
var
  ElementName: string;
  Info: PTypeInfo;
  B1Array: TArray<TB1> absolute DynArray;
  B2Array: TArray<TB2> absolute DynArray;
  B4Array: TArray<TB4> absolute DynArray;
  B8Array: TArray<TB8> absolute DynArray;
  EArray: TArray<Extended> absolute DynArray;
{$IFNDEF NEXTGEN}
  SArray: TArray<ShortString> absolute DynArray;
  LArray: TArray<AnsiString> absolute DynArray;
  WArray: TArray<WideString> absolute DynArray;
{$ELSE}
  WArray: TArray<string> absolute DynArray;
{$ENDIF}
  UArray: TArray<UnicodeString> absolute DynArray;
  VArray: TArray<Variant> absolute DynArray;
  DArray: TArray<TArray<Pointer>> absolute DynArray;
  IArray: TArray<IInterface> absolute DynArray;
  OArray: TArray<TObject> absolute DynArray;
  Count, I: Integer;
begin
  ElementName := GetElementName(Name);
  if IsSmartObject(ElementName) then ElementName := 'ISmartObject';
  Info := THproseClassManager.TypeInfo(ElementName);
  if Info = nil then
    raise EHproseException.Create('Can not serialize ' + Name)
  else begin
    FRefer.SetRef(NativeInt(Pointer(DynArray)));
    Count := Length(B1Array);
    FStream.WriteBuffer(HproseTagList, 1);
    if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
    FStream.WriteBuffer(HproseTagOpenbrace, 1);
    case Info^.Kind of
{$IFNDEF NEXTGEN}
      tkString: for I := 0 to Count - 1 do WriteWideString(WideString(SArray[I]));
      tkLString: for I := 0 to Count - 1 do WriteWideString(WideString(LArray[I]));
      tkUString: for I := 0 to Count - 1 do WriteWideString(WideString(UArray[I]));
{$ELSE}
      tkUString: for I := 0 to Count - 1 do WriteWideString(string(UArray[I]));
{$ENDIF}
      tkWString: for I := 0 to Count - 1 do WriteWideString(WArray[I]);
      tkVariant: for I := 0 to Count - 1 do Serialize(VArray[I]);
      tkDynArray: for I := 0 to Count - 1 do WriteArrayWithRef(DArray[I], Info);
      tkInterface: for I := 0 to Count - 1 do Serialize(IArray[I], Info);
      tkClass: for I := 0 to Count - 1 do Serialize(OArray[I], Info);
    else
      case GetTypeSize(Info) of
        1: for I := 0 to Count - 1 do Serialize(B1Array[I], Info);
        2: for I := 0 to Count - 1 do Serialize(B2Array[I], Info);
        4: for I := 0 to Count - 1 do Serialize(B4Array[I], Info);
        8: for I := 0 to Count - 1 do Serialize(B8Array[I], Info);
      else if GetTypeName(Info) = 'Extended' then
        for I := 0 to Count - 1 do WriteDouble(EArray[I])
      else
        raise EHproseException.Create('Can not serialize ' + Name);
      end;
    end;
    FStream.WriteBuffer(HproseTagClosebrace, 1);
  end;
end;

procedure THproseWriter.WriteArrayWithRef(const DynArray; Info: PTypeInfo);
var
  Name: string;
  Value: Variant;
  TypeData: PTypeData;
begin
  Name := GetTypeName(Info);
  if AnsiStartsText('TArray<', Name) then begin
    if not FRefer.WriteRef(NativeInt(Pointer(DynArray))) then WriteArray(DynArray, Name);
  end
  else begin
    DynArrayToVariant(Value, Pointer(DynArray), Info);
    TypeData := GetTypeData(Info);
    if (TypeData^.varType and varTypeMask = varByte) and
       (VarArrayDimCount(Value) = 1) then
      WriteBytesWithRef(Value)
    else
      WriteArrayWithRef(Value);
  end;
end;

procedure THproseWriter.WriteList(const AList: TObject);
var
  ClassName: string;
  ElementName: string;
  Info: PTypeInfo;
  B1List: TList<TB1> absolute AList;
  B2List: TList<TB2> absolute AList;
  B4List: TList<TB4> absolute AList;
  B8List: TList<TB8> absolute AList;
  EList: TList<Extended> absolute AList;
{$IFNDEF NEXTGEN}
  SList: TList<ShortString> absolute AList;
  LList: TList<AnsiString> absolute AList;
  WList: TList<WideString> absolute AList;
{$ELSE}
  WList: TList<string> absolute AList;
{$ENDIF}
  UList: TList<UnicodeString> absolute AList;
  VList: TList<Variant> absolute AList;
  DList: TList<TArray<Pointer>> absolute AList;
  IList: TList<IInterface> absolute AList;
  OList: TList<TObject> absolute AList;
  B1: TB1;
  B2: TB2;
  B4: TB4;
  B8: TB8;
{$IFNDEF NEXTGEN}
  SS: ShortString;
  LS: AnsiString;
  WS: WideString;
{$ELSE}
  WS: string;
{$ENDIF}
  US: UnicodeString;
  V: Variant;
  E: Extended;
  D: TArray<Pointer>;
  I: IInterface;
  O: TObject;
  Count: Integer;
begin
  ClassName := AList.ClassName;
  ElementName := GetElementName(ClassName);
  if IsSmartObject(ElementName) then ElementName := 'ISmartObject';
  Info := THproseClassManager.TypeInfo(ElementName);
  if Info = nil then
    raise EHproseException.Create('Can not serialize ' + ClassName)
  else begin
    FRefer.SetRef(ObjToVar(AList));
    Count := B1List.Count;
    FStream.WriteBuffer(HproseTagList, 1);
    if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
    FStream.WriteBuffer(HproseTagOpenbrace, 1);
    case Info^.Kind of
{$IFNDEF NEXTGEN}
      tkString: for SS in SList do WriteWideString(WideString(SS));
      tkLString: for LS in LList do WriteWideString(WideString(LS));
      tkUString: for US in UList do WriteWideString(WideString(US));
{$ELSE}
      tkUString: for US in UList do WriteWideString(string(US));
{$ENDIF}
      tkWString: for WS in WList do WriteWideString(WS);
      tkVariant: for V in VList do Serialize(V);
      tkDynArray: for D in DList do WriteArrayWithRef(D, Info);
      tkInterface: for I in IList do Serialize(I, Info);
      tkClass: for O in OList do Serialize(O, Info);
    else
      case GetTypeSize(Info) of
        1: for B1 in B1List do Serialize(B1, Info);
        2: for B2 in B2List do Serialize(B2, Info);
        4: for B4 in B4List do Serialize(B4, Info);
        8: for B8 in B8List do Serialize(B8, Info);
      else if GetTypeName(Info) = 'Extended' then
        for E in EList do WriteDouble(E)
      else
        raise EHproseException.Create('Can not serialize ' + ClassName);
      end;
    end;
    FStream.WriteBuffer(HproseTagClosebrace, 1);
  end;
end;

procedure THproseWriter.WriteObjectList(const AList: TObject);
var
  ClassName: string;
  Info: PTypeInfo;
  OList: TObjectList<TObject> absolute AList;
  O: TObject;
  Count: Integer;
begin
  ClassName := AList.ClassName;
  Info := THproseClassManager.TypeInfo(GetElementName(ClassName));
  if Info = nil then
    raise EHproseException.Create('Can not serialize ' + ClassName)
  else begin
    FRefer.SetRef(ObjToVar(AList));
    Count := OList.Count;
    FStream.WriteBuffer(HproseTagList, 1);
    if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
    FStream.WriteBuffer(HproseTagOpenbrace, 1);
    for O in OList do Serialize(O, Info);
    FStream.WriteBuffer(HproseTagClosebrace, 1);
  end;
end;

procedure THproseWriter.WriteQueue(const AQueue: TObject);
var
  ClassName: string;
  ElementName: string;
  Info: PTypeInfo;
  B1Queue: TQueue<TB1> absolute AQueue;
  B2Queue: TQueue<TB2> absolute AQueue;
  B4Queue: TQueue<TB4> absolute AQueue;
  B8Queue: TQueue<TB8> absolute AQueue;
  EQueue: TQueue<Extended> absolute AQueue;
{$IFNDEF NEXTGEN}
  SQueue: TQueue<ShortString> absolute AQueue;
  LQueue: TQueue<AnsiString> absolute AQueue;
  WQueue: TQueue<WideString> absolute AQueue;
{$ELSE}
  WQueue: TQueue<string> absolute AQueue;
{$ENDIF}
  UQueue: TQueue<UnicodeString> absolute AQueue;
  VQueue: TQueue<Variant> absolute AQueue;
  DQueue: TQueue<TArray<Pointer>> absolute AQueue;
  IQueue: TQueue<IInterface> absolute AQueue;
  OQueue: TQueue<TObject> absolute AQueue;
  B1: TB1;
  B2: TB2;
  B4: TB4;
  B8: TB8;
{$IFNDEF NEXTGEN}
  SS: ShortString;
  LS: AnsiString;
  WS: WideString;
{$ELSE}
  WS: string;
{$ENDIF}
  US: UnicodeString;
  V: Variant;
  E: Extended;
  D: TArray<Pointer>;
  I: IInterface;
  O: TObject;
  Count: Integer;
begin
  ClassName := AQueue.ClassName;
  ElementName := GetElementName(ClassName);
  if IsSmartObject(ElementName) then ElementName := 'ISmartObject';
  Info := THproseClassManager.TypeInfo(ElementName);
  if Info = nil then
    raise EHproseException.Create('Can not serialize ' + ClassName)
  else begin
    FRefer.SetRef(ObjToVar(AQueue));
    Count := B1Queue.Count;
    FStream.WriteBuffer(HproseTagList, 1);
    if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
    FStream.WriteBuffer(HproseTagOpenbrace, 1);
    case Info^.Kind of
{$IFNDEF NEXTGEN}
      tkString: for SS in SQueue do WriteWideString(WideString(SS));
      tkLString: for LS in LQueue do WriteWideString(WideString(LS));
      tkUString: for US in UQueue do WriteWideString(WideString(US));
{$ELSE}
      tkUString: for US in UQueue do WriteWideString(string(US));
{$ENDIF}
      tkWString: for WS in WQueue do WriteWideString(WS);
      tkVariant: for V in VQueue do Serialize(V);
      tkDynArray: for D in DQueue do WriteArrayWithRef(D, Info);
      tkInterface: for I in IQueue do Serialize(I, Info);
      tkClass: for O in OQueue do Serialize(O, Info);
    else
      case GetTypeSize(Info) of
        1: for B1 in B1Queue do Serialize(B1, Info);
        2: for B2 in B2Queue do Serialize(B2, Info);
        4: for B4 in B4Queue do Serialize(B4, Info);
        8: for B8 in B8Queue do Serialize(B8, Info);
      else if GetTypeName(Info) = 'Extended' then
        for E in EQueue do WriteDouble(E)
      else
        raise EHproseException.Create('Can not serialize ' + ClassName);
      end;
    end;
    FStream.WriteBuffer(HproseTagClosebrace, 1);
  end;
end;

procedure THproseWriter.WriteObjectQueue(const AQueue: TObject);
var
  ClassName: string;
  Info: PTypeInfo;
  OQueue: TObjectQueue<TObject> absolute AQueue;
  O: TObject;
  Count: Integer;
begin
  ClassName := AQueue.ClassName;
  Info := THproseClassManager.TypeInfo(GetElementName(ClassName));
  if Info = nil then
    raise EHproseException.Create('Can not serialize ' + ClassName)
  else begin
    FRefer.SetRef(ObjToVar(AQueue));
    Count := OQueue.Count;
    FStream.WriteBuffer(HproseTagList, 1);
    if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
    FStream.WriteBuffer(HproseTagOpenbrace, 1);
    for O in OQueue do Serialize(O, Info);
    FStream.WriteBuffer(HproseTagClosebrace, 1);
  end;
end;

procedure THproseWriter.WriteStack(const AStack: TObject);
var
  ClassName: string;
  ElementName: string;
  Info: PTypeInfo;
  B1Stack: TStack<TB1> absolute AStack;
  B2Stack: TStack<TB2> absolute AStack;
  B4Stack: TStack<TB4> absolute AStack;
  B8Stack: TStack<TB8> absolute AStack;
  EStack: TStack<Extended> absolute AStack;
{$IFNDEF NEXTGEN}
  SStack: TStack<ShortString> absolute AStack;
  LStack: TStack<AnsiString> absolute AStack;
  WStack: TStack<WideString> absolute AStack;
{$ELSE}
  WStack: TStack<string> absolute AStack;
{$ENDIF}
  UStack: TStack<UnicodeString> absolute AStack;
  VStack: TStack<Variant> absolute AStack;
  DStack: TStack<TArray<Pointer>> absolute AStack;
  IStack: TStack<IInterface> absolute AStack;
  OStack: TStack<TObject> absolute AStack;
  B1: TB1;
  B2: TB2;
  B4: TB4;
  B8: TB8;
{$IFNDEF NEXTGEN}
  SS: ShortString;
  LS: AnsiString;
  WS: WideString;
{$ELSE}
  WS: string;
{$ENDIF}
  US: UnicodeString;
  V: Variant;
  E: Extended;
  D: TArray<Pointer>;
  I: IInterface;
  O: TObject;
  Count: Integer;
begin
  ClassName := AStack.ClassName;
  ElementName := GetElementName(ClassName);
  if IsSmartObject(ElementName) then ElementName := 'ISmartObject';
  Info := THproseClassManager.TypeInfo(ElementName);
  if Info = nil then
    raise EHproseException.Create('Can not serialize ' + ClassName)
  else begin
    FRefer.SetRef(ObjToVar(AStack));
    Count := B1Stack.Count;
    FStream.WriteBuffer(HproseTagList, 1);
    if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
    FStream.WriteBuffer(HproseTagOpenbrace, 1);
    case Info^.Kind of
{$IFNDEF NEXTGEN}
      tkString: for SS in SStack do WriteWideString(WideString(SS));
      tkLString: for LS in LStack do WriteWideString(WideString(LS));
      tkUString: for US in UStack do WriteWideString(WideString(US));
{$ELSE}
      tkUString: for US in UStack do WriteWideString(string(US));
{$ENDIF}
      tkWString: for WS in WStack do WriteWideString(WS);
      tkVariant: for V in VStack do Serialize(V);
      tkDynArray: for D in DStack do WriteArrayWithRef(D, Info);
      tkInterface: for I in IStack do Serialize(I, Info);
      tkClass: for O in OStack do Serialize(O, Info);
    else
      case GetTypeSize(Info) of
        1: for B1 in B1Stack do Serialize(B1, Info);
        2: for B2 in B2Stack do Serialize(B2, Info);
        4: for B4 in B4Stack do Serialize(B4, Info);
        8: for B8 in B8Stack do Serialize(B8, Info);
      else if GetTypeName(Info) = 'Extended' then
        for E in EStack do WriteDouble(E)
      else
        raise EHproseException.Create('Can not serialize ' + ClassName);
      end;
    end;
    FStream.WriteBuffer(HproseTagClosebrace, 1);
  end;
end;

procedure THproseWriter.WriteObjectStack(const AStack: TObject);
var
  ClassName: string;
  Info: PTypeInfo;
  OStack: TObjectStack<TObject> absolute AStack;
  O: TObject;
  Count: Integer;
begin
  ClassName := AStack.ClassName;
  Info := THproseClassManager.TypeInfo(GetElementName(ClassName));
  if Info = nil then
    raise EHproseException.Create('Can not serialize ' + ClassName)
  else begin
    FRefer.SetRef(ObjToVar(AStack));
    Count := OStack.Count;
    FStream.WriteBuffer(HproseTagList, 1);
    if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
    FStream.WriteBuffer(HproseTagOpenbrace, 1);
    for O in OStack do Serialize(O, Info);
    FStream.WriteBuffer(HproseTagClosebrace, 1);
  end;
end;

procedure THproseWriter.WriteTDictionary1<TKey>(const ADict: TObject;
  KeyInfo, ValueInfo: PTypeInfo);
begin
    case ValueInfo^.Kind of
{$IFNDEF NEXTGEN}
      tkString: WriteTDictionary2<TKey, ShortString>(
        TDictionary<TKey, ShortString>(ADict), KeyInfo, ValueInfo);
      tkLString: WriteTDictionary2<TKey, AnsiString>(
        TDictionary<TKey, AnsiString>(ADict), KeyInfo, ValueInfo);
      tkWString: WriteTDictionary2<TKey, WideString>(
        TDictionary<TKey, WideString>(ADict), KeyInfo, ValueInfo);
{$ELSE}
      tkWString: WriteTDictionary2<TKey, string>(
        TDictionary<TKey, string>(ADict), KeyInfo, ValueInfo);
{$ENDIF}
      tkUString: WriteTDictionary2<TKey, UnicodeString>(
        TDictionary<TKey, UnicodeString>(ADict), KeyInfo, ValueInfo);
      tkVariant: WriteTDictionary2<TKey, Variant>(
        TDictionary<TKey, Variant>(ADict), KeyInfo, ValueInfo);
      tkDynArray: WriteTDictionary2<TKey, TArray<Pointer>>(
        TDictionary<TKey, TArray<Pointer>>(ADict), KeyInfo, ValueInfo);
      tkInterface: WriteTDictionary2<TKey, IInterface>(
        TDictionary<TKey, IInterface>(ADict), KeyInfo, ValueInfo);
      tkClass: WriteTDictionary2<TKey, TObject>(
        TDictionary<TKey, TObject>(ADict), KeyInfo, ValueInfo);
    else
      case GetTypeSize(ValueInfo) of
        1: WriteTDictionary2<TKey, TB1>(
          TDictionary<TKey, TB1>(ADict), KeyInfo, ValueInfo);
        2: WriteTDictionary2<TKey, TB2>(
          TDictionary<TKey, TB2>(ADict), KeyInfo, ValueInfo);
        4: WriteTDictionary2<TKey, TB4>(
          TDictionary<TKey, TB4>(ADict), KeyInfo, ValueInfo);
        8: WriteTDictionary2<TKey, TB8>(
          TDictionary<TKey, TB8>(ADict), KeyInfo, ValueInfo);
      else if GetTypeName(ValueInfo) = 'Extended' then
        WriteTDictionary2<TKey, Extended>(
          TDictionary<TKey, Extended>(ADict), KeyInfo, ValueInfo)
      else
        raise EHproseException.Create('Can not serialize ' + ClassName);
      end;
    end;
end;


procedure THproseWriter.WriteDictionary(const ADict: TObject);
var
  ClassName: string;
  KeyName, ValueName: string;
  KeyInfo, ValueInfo: PTypeInfo;
begin
  ClassName := ADict.ClassName;
  SplitKeyValueTypeName(GetElementName(ClassName), KeyName, ValueName);
  if IsSmartObject(KeyName) then KeyName := 'ISmartObject';
  if IsSmartObject(ValueName) then ValueName := 'ISmartObject';
  KeyInfo := THproseClassManager.TypeInfo(KeyName);
  ValueInfo := THproseClassManager.TypeInfo(ValueName);
  if (KeyInfo = nil) or (ValueInfo = nil) then
    raise EHproseException.Create('Can not serialize ' + ClassName)
  else begin
    case KeyInfo^.Kind of
{$IFNDEF NEXTGEN}
      tkString: WriteTDictionary1<ShortString>(ADict, KeyInfo, ValueInfo);
      tkLString: WriteTDictionary1<AnsiString>(ADict, KeyInfo, ValueInfo);
      tkWString: WriteTDictionary1<WideString>(ADict, KeyInfo, ValueInfo);
{$ELSE}
      tkWString: WriteTDictionary1<string>(ADict, KeyInfo, ValueInfo);
{$ENDIF}
      tkUString: WriteTDictionary1<UnicodeString>(ADict, KeyInfo, ValueInfo);
      tkVariant: WriteTDictionary1<Variant>(ADict, KeyInfo, ValueInfo);
      tkDynArray: WriteTDictionary1<TArray<Pointer>>(ADict, KeyInfo, ValueInfo);
      tkInterface: WriteTDictionary1<IInterface>(ADict, KeyInfo, ValueInfo);
      tkClass: WriteTDictionary1<TObject>(ADict, KeyInfo, ValueInfo);
    else
      case GetTypeSize(KeyInfo) of
        1: WriteTDictionary1<TB1>(ADict, KeyInfo, ValueInfo);
        2: WriteTDictionary1<TB2>(ADict, KeyInfo, ValueInfo);
        4: WriteTDictionary1<TB4>(ADict, KeyInfo, ValueInfo);
        8: WriteTDictionary1<TB8>(ADict, KeyInfo, ValueInfo);
      else if GetTypeName(KeyInfo) = 'Extended' then
         WriteTDictionary1<Extended>(ADict, KeyInfo, ValueInfo)
      else
        raise EHproseException.Create('Can not serialize ' + ClassName);
      end;
    end;
  end;
end;

procedure THproseWriter.WriteObjectDictionary(const ADict: TObject);
var
  ClassName, KeyName, ValueName: string;
  KeyInfo, ValueInfo: PTypeInfo;
  ODict: TObjectDictionary<TObject, TObject> absolute ADict;
  O: TPair<TObject, TObject>;
  Count: Integer;
begin
  ClassName := ADict.ClassName;
  SplitKeyValueTypeName(GetElementName(ClassName), KeyName, ValueName);
  KeyInfo := THproseClassManager.TypeInfo(KeyName);
  ValueInfo := THproseClassManager.TypeInfo(ValueName);
  if (KeyInfo = nil) or (ValueInfo = nil) then
    raise EHproseException.Create('Can not serialize ' + ClassName)
  else begin
    FRefer.SetRef(ObjToVar(ADict));
    Count := ODict.Count;
    FStream.WriteBuffer(HproseTagMap, 1);
    if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
    FStream.WriteBuffer(HproseTagOpenbrace, 1);
    for O in ODict do begin
      Serialize(O.Key, KeyInfo);
      Serialize(O.Value, ValueInfo);
    end;
    FStream.WriteBuffer(HproseTagClosebrace, 1);
  end;
end;

procedure THproseWriter.Serialize<T>(const Value: T);
begin
  Serialize(Value, TypeInfo(T));
end;

procedure THproseWriter.WriteArray<T>(const DynArray: array of T);
var
  Count, I: Integer;
begin
  FRefer.SetRef(Null);
  Count := Length(DynArray);
  FStream.WriteBuffer(HproseTagList, 1);
  if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  for I := 0 to Count - 1 do Serialize(DynArray[I], TypeInfo(T));
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteDynArray<T>(const DynArray: TArray<T>);
var
  Count, I: Integer;
begin
  FRefer.SetRef(NativeInt(Pointer(DynArray)));
  Count := Length(DynArray);
  FStream.WriteBuffer(HproseTagList, 1);
  if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  for I := 0 to Count - 1 do Serialize(DynArray[I], TypeInfo(T));
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteDynArrayWithRef<T>(const DynArray: TArray<T>);
begin
  if not FRefer.WriteRef(NativeInt(Pointer(DynArray))) then WriteDynArray<T>(DynArray);
end;

procedure THproseWriter.WriteTList<T>(const AList: TList<T>);
var
  Count, I: Integer;
  Element: T;
begin
  FRefer.SetRef(ObjToVar(AList));
  Count := AList.Count;
  FStream.WriteBuffer(HproseTagList, 1);
  if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  for I := 0 to Count - 1 do begin
    Element := AList[I];
    Serialize(Element, TypeInfo(T));
  end;
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteTListWithRef<T>(const AList: TList<T>);
begin
  if not FRefer.WriteRef(ObjToVar(AList)) then WriteTList<T>(AList);
end;

procedure THproseWriter.WriteTQueue<T>(const AQueue: TQueue<T>);
var
  Count, I: Integer;
  Element: T;
begin
  FRefer.SetRef(ObjToVar(AQueue));
  Count := AQueue.Count;
  FStream.WriteBuffer(HproseTagList, 1);
  if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  for Element in AQueue do Serialize(Element, TypeInfo(T));
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteTQueueWithRef<T>(const AQueue: TQueue<T>);
begin
  if not FRefer.WriteRef(ObjToVar(AQueue)) then WriteTQueue<T>(AQueue);
end;

procedure THproseWriter.WriteTStack<T>(const AStack: TStack<T>);
var
  Count, I: Integer;
  Element: T;
begin
  FRefer.SetRef(ObjToVar(AStack));
  Count := AStack.Count;
  FStream.WriteBuffer(HproseTagList, 1);
  if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  for Element in AStack do Serialize(Element, TypeInfo(T));
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteTStackWithRef<T>(const AStack: TStack<T>);
begin
  if not FRefer.WriteRef(ObjToVar(AStack)) then WriteTStack<T>(AStack);
end;

procedure THproseWriter.WriteTDictionary2<TKey, TValue>(
  const ADict: TDictionary<TKey, TValue>; KeyInfo, ValueInfo: PTypeInfo);
var
  Count, I: Integer;
  Pair: TPair<TKey, TValue>;
begin
  FRefer.SetRef(ObjToVar(ADict));
  Count := ADict.Count;
  FStream.WriteBuffer(HproseTagMap, 1);
  if Count > 0 then WriteRawBytes(BytesOf(IntToStr(Count)));
  FStream.WriteBuffer(HproseTagOpenbrace, 1);
  for Pair in ADict do begin
    Serialize(Pair.Key, KeyInfo);
    Serialize(Pair.Value, ValueInfo);
  end;
  FStream.WriteBuffer(HproseTagClosebrace, 1);
end;

procedure THproseWriter.WriteTDictionary<TKey, TValue>(
  const ADict: TDictionary<TKey, TValue>);
begin
  WriteTDictionary2<TKey, TValue>(ADict, TypeInfo(TKey), TypeInfo(TValue));
end;

procedure THproseWriter.WriteTDictionaryWithRef<TKey, TValue>(
  const ADict: TDictionary<TKey, TValue>);
begin
  if not FRefer.WriteRef(ObjToVar(ADict)) then WriteTDictionary<TKey, TValue>(ADict);
end;

{$ELSE}

procedure THproseWriter.Serialize(const Value: TObject);
begin
  Serialize(ObjToVar(Value));
end;

{$ENDIF}

{ HproseSerialize }

function HproseSerialize(const Value: Variant; Simple: Boolean): TBytes;
var
  Writer: THproseWriter;
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create;
  try
    Writer := THproseWriter.Create(Stream, Simple);
    try
      Writer.Serialize(Value);
      Result := Stream.Bytes;
      SetLength(Result, Stream.Size);
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function HproseSerialize(const Value: TObject; Simple: Boolean): TBytes;
begin
  Result := HproseSerialize(ObjToVar(Value), Simple);
end;

function HproseSerialize(const Value: array of const; Simple: Boolean): TBytes;
var
  Writer: THproseWriter;
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create;
  try
    Writer := THproseWriter.Create(Stream, Simple);
    try
      Writer.Serialize(Value);
      Result := Stream.Bytes;
      SetLength(Result, Stream.Size);
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

{ HproseUnserialize }

function HproseUnserialize(const Data: TBytes; Info: PTypeInfo; Simple: Boolean): Variant;
var
  Reader: THproseReader;
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create(Data);
  try
    Reader := THproseReader.Create(Stream, Simple);
    try
      Result := Reader.Unserialize(Info);
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function HproseUnserialize(const Data:TBytes; Simple: Boolean): Variant;
begin
  Result := HproseUnserialize(Data, nil, Simple);
end;

{ THproseFormatter }

class function THproseFormatter.Serialize(
  const Value: Variant; Simple: Boolean): TBytes;
begin
  Result := HproseSerialize(Value, Simple);
end;

class function THproseFormatter.Serialize(
  const Value: array of const; Simple: Boolean): TBytes;
begin
  Result := HproseSerialize(Value, Simple);
end;

{$IFDEF Supports_Generics}

class function THproseFormatter.Serialize<T>(const Value: T; Simple: Boolean): TBytes;
var
  Writer: THproseWriter;
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create;
  try
    Writer := THproseWriter.Create(Stream, Simple);
    try
      Writer.Serialize<T>(Value);
      Result := Stream.Bytes;
      SetLength(Result, Stream.Size);
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
  end;
end;

class function THproseFormatter.Unserialize<T>(const Data:TBytes; Simple: Boolean): T;
var
  Reader: THproseReader;
  Stream: TBytesStream;
begin
  Stream := TBytesStream.Create(Data);
  try
    Reader := THproseReader.Create(Stream, Simple);
    try
      Result := Reader.Unserialize<T>;
    finally
      Reader.Free;
    end;
  finally
    Stream.Free;
  end;
end;

{$ELSE}

class function THproseFormatter.Serialize(const Value: TObject; Simple: Boolean): TBytes;
begin
  Result := HproseSerialize(Value, Simple);
end;

{$ENDIF}

class function THproseFormatter.Unserialize(const Data: TBytes;
  Info: PTypeInfo; Simple: Boolean): Variant;
begin
  Result := HproseUnserialize(Data, Info, Simple);
end;

class function THproseFormatter.Unserialize(const Data:TBytes; Simple: Boolean): Variant;
begin
  Result := HproseUnserialize(Data, nil, Simple);
end;

procedure FreePropertiesCache;
var
  I: Integer;
  CacheValues: IList;
  CachePointer: PSerializeCache;
begin
  PropertiesCache.Lock;
  try
    CacheValues := PropertiesCache.Values;
    for I := 0 to CacheValues.Count - 1 do begin
      CachePointer := PSerializeCache(NativeInt(CacheValues[I]));
      Dispose(CachePointer);
    end;
  finally
    PropertiesCache.Unlock;
  end;
end;

initialization
  PropertiesCache := THashMap.Create;
finalization
  FreePropertiesCache;

end.
