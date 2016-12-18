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
 * HproseCommon.pas                                       *
 *                                                        *
 * hprose common unit for delphi.                         *
 *                                                        *
 * LastModified: Dec 19, 2016                             *
 * Author: Ma Bingyao <andot@hprose.com>                  *
 *                                                        *
\**********************************************************/
}
unit HproseCommon;

{$I Hprose.inc}

{$IFDEF DELPHIXE3_UP}
  {$LEGACYIFEND ON}
{$ENDIF}
 
interface

uses Classes, SyncObjs, SysUtils, TypInfo, Variants;

type

{$IFDEF FPC}
  UInt64 = QWord;
{$ENDIF}

{$IFDEF DELPHI2009}
  TArray<T> = array of T;
{$ENDIF}

{$IFNDEF DELPHI2009_UP}
  RawByteString = type AnsiString;
{$IFDEF CPU64}
  NativeInt = Int64;
  NativeUInt = UInt64;
{$ELSE}
  NativeInt = Integer;
  NativeUInt = Cardinal;
{$ENDIF}
{$ENDIF}

  TResultMode = (Normal, Serialized, Raw, RawWithEndTag);

{$IF NOT DEFINED(DELPHI2007_UP) AND NOT DEFINED(FPC)}
  TBytes = array of Byte;
{$IFEND}

  TVariants = array of Variant;
  PVariants = ^TVariants;

  TConstArray = array of TVarRec;
  TStringArray = array of string;

  ETypeError = class(Exception);
  EHashBucketError = class(Exception);
  EArrayListError = class(Exception);

  TDirection = (FromBeginning, FromEnd);

  IInvokeableVarObject = interface
  ['{FDC126C2-EF9F-4898-BF97-87C01B050F88}']
    function Invoke(const Name: string; const Arguments: TVarDataArray): Variant;
  end;

  IListEnumerator = interface
  ['{767477EC-A143-4DC6-9962-A6837A7AEC01}']
    function GetCurrent: Variant;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: Variant read GetCurrent;
  end;

  IImmutableList = interface(IReadWriteSync)
  ['{1D116D24-E964-E019-FC27-AFE4BF2A181D}']
    function Get(Index: Integer): Variant;
    function GetCapacity: Integer;
    function GetCount: Integer;
    function Contains(const Value: Variant): Boolean;
    function GetEnumerator: IListEnumerator;
    function IndexOf(const Value: Variant): Integer; overload;
    function IndexOf(const Value: Variant; StartIndex: Integer): Integer; overload;
    function IndexOf(const Value: Variant; StartIndex, ACount: Integer): Integer; overload;
    function LastIndexOf(const Value: Variant): Integer; overload;
    function LastIndexOf(const Value: Variant; StartIndex: Integer): Integer; overload;
    function LastIndexOf(const Value: Variant; StartIndex, ACount: Integer): Integer; overload;
    function Join(const Glue: string = ',';
                  const LeftPad: string = '';
                  const RightPad: string = ''): string;
    procedure Lock;
    procedure Unlock;
    function ToArray: TVariants; overload;
    function ToArray(VarType: TVarType): Variant; overload;
    function First: Variant;
    function Last: Variant;
    property Item[Index: Integer]: Variant read Get; default;
    property Capacity: Integer read GetCapacity;
    property Count: Integer read GetCount;
  end;

{$IFDEF SUPPORTS_ANONYMOUS_METHOD}
  TListCompareMethod = reference to function (const Value1, Value2: Variant): Integer;
{$ELSE}
  TListCompareMethod = function (const Value1, Value2: Variant): Integer of object;
{$ENDIF}

  IList = interface(IImmutableList)
  ['{DE925411-42B8-4DB3-A00C-B585C087EC4C}']
    procedure Put(Index: Integer; const Value: Variant);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    function Add(const Value: Variant): Integer;
    procedure AddAll(const AList: IImmutableList); overload;
    procedure AddAll(const Container: Variant); overload;
    procedure AddAll(const ConstArray: array of const); overload;
    procedure Assign(const Source: IImmutableList);
    procedure Clear;
    function Compare(const Value1, Value2: Variant): Integer;
    function Delete(Index: Integer): Variant;
    procedure DeleteRange(Index, ACount: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Insert(Index: Integer; const Value: Variant);
    procedure InsertRange(Index: Integer; const AList: IImmutableList); overload;
    procedure InsertRange(Index: Integer; const Container: Variant); overload;
    procedure InsertRange(Index: Integer; const ConstArray: array of const); overload;
    procedure InitLock;
    procedure InitReadWriteLock;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(const Value: Variant): Integer; overload;
    function Remove(const Value: Variant; Direction: TDirection): Integer; overload;
    procedure Pack;
    procedure Reverse;
    procedure Sort; overload;
    procedure Sort(CompareProc: TListCompareMethod); overload;
    procedure Shuffle;
    procedure TrimExcess;
    property Item[Index: Integer]: Variant read Get write Put; default;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
  end;

  { TAbstractList }

{$M+}
  TAbstractList = class(TInterfacedObject, IList, IInvokeableVarObject)
  private
    FLock: TCriticalSection;
    FReadWriteLock: TMultiReadExclusiveWriteSynchronizer;
  protected
    procedure Grow; overload; virtual; abstract;
    procedure Grow(N: Integer); overload; virtual; abstract;
    function GetCapacity: Integer; virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    procedure SetCapacity(NewCapacity: Integer); virtual; abstract;
    procedure SetCount(NewCount: Integer); virtual; abstract;
    function Compare(const Value1, Value2: Variant): Integer; virtual;
    function Invoke(const Name: string; const Arguments: TVarDataArray): Variant; virtual;
  public
    constructor Create(ACapacity: Integer = 4; Sync: Boolean = True;
      ReadWriteSync: Boolean = False); overload; virtual; abstract;
    constructor Create(Sync: Boolean;
      ReadWriteSync: Boolean = False); overload; virtual;
    constructor Create(const AList: IImmutableList; Sync: Boolean = True;
      ReadWriteSync: Boolean = False); overload; virtual;
    constructor Create(const Container: Variant; Sync: Boolean = True;
      ReadWriteSync: Boolean = False); overload; virtual;
    constructor Create(const ConstArray: array of const; Sync: Boolean = True;
      ReadWriteSync: Boolean = False); overload; virtual;
    {$IFDEF BCB}
        constructor Create0; virtual; // for C++ Builder
        constructor Create1(ACapacity: Integer); virtual; // for C++ Builder
        constructor Create2(ACapacity: Integer; Sync: Boolean); virtual; // for C++ Builder
        constructor CreateS(Sync: Boolean); virtual; // for C++ Builder
        constructor CreateL1(const AList: IImmutableList); virtual; // for C++ Builder
        constructor CreateA1(const Container: Variant); virtual; // for C++ Builder
        constructor CreateCA1(const ConstArray: array of const); virtual; // for C++ Builder
        constructor CreateL2(const AList: IImmutableList; Sync: Boolean); virtual; // for C++ Builder
        constructor CreateA2(const Container: Variant; Sync: Boolean); virtual; // for C++ Builder
        constructor CreateCA2(const ConstArray: array of const; Sync: Boolean); virtual; // for C++ Builder
    {$ENDIF}
    destructor Destroy; override;
    function Add(const Value: Variant): Integer; virtual; abstract;
    procedure AddAll(const AList: IImmutableList); overload; virtual;
    procedure AddAll(const Container: Variant); overload; virtual;
    procedure AddAll(const ConstArray: array of const); overload; virtual;
    procedure Assign(const Source: IImmutableList); virtual;
    procedure Clear; virtual; abstract;
    function Contains(const Value: Variant): Boolean; virtual;
    function Delete(Index: Integer): Variant; virtual; abstract;
    procedure DeleteRange(Index, Count: Integer); virtual; abstract;
    procedure Exchange(Index1, Index2: Integer); virtual; abstract;
    function Get(Index: Integer): Variant; virtual; abstract;
    procedure Put(Index: Integer; const Value: Variant); virtual; abstract;
    function GetEnumerator: IListEnumerator; virtual;
    function IndexOf(const Value: Variant): Integer; overload; virtual;
    function IndexOf(const Value: Variant; StartIndex: Integer): Integer; overload; virtual;
    function IndexOf(const Value: Variant; StartIndex, ACount: Integer): Integer; overload; virtual; abstract;
    function LastIndexOf(const Value: Variant): Integer; overload; virtual;
    function LastIndexOf(const Value: Variant; StartIndex: Integer): Integer; overload; virtual;
    function LastIndexOf(const Value: Variant; StartIndex, ACount: Integer): Integer; overload; virtual; abstract;
    procedure Insert(Index: Integer; const Value: Variant); virtual; abstract;
    procedure InsertRange(Index: Integer; const AList: IImmutableList); overload; virtual; abstract;
    procedure InsertRange(Index: Integer; const Container: Variant); overload; virtual; abstract;
    procedure InsertRange(Index: Integer; const ConstArray: array of const); overload; virtual; abstract;
    function Join(const Glue: string = ',';
                  const LeftPad: string = '';
                  const RightPad: string = ''): string;
    class function Split(Str: string; const Separator: string = ',';
      Limit: Integer = 0; TrimItem: Boolean = False;
      SkipEmptyItem: Boolean = False; Sync: Boolean = True;
      ReadWriteSync: Boolean = False): IList;
    procedure InitLock;
    procedure InitReadWriteLock;
    procedure Lock;
    procedure Unlock;
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
    procedure Move(CurIndex, NewIndex: Integer); virtual; abstract;
    function Remove(const Value: Variant): Integer; overload;
    function Remove(const Value: Variant; Direction: TDirection): Integer; overload;
    function ToArray: TVariants; overload; virtual; abstract;
    function ToArray(VarType: TVarType): Variant; overload; virtual; abstract;
    function First: Variant;
    function Last: Variant;
    procedure Pack;
    procedure Reverse;
    procedure Sort; overload;
    procedure Sort(CompareProc: TListCompareMethod); overload;
    procedure Shuffle;
    procedure TrimExcess;
    property Item[Index: Integer]: Variant read Get write Put; default;
  published
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
  end;
{$M-}

  TListClass = class of TAbstractList;

  IArrayList = interface(IList)
  ['{0D12803C-6B0B-476B-A9E3-C219BF651BD1}']
  end;

  { TArrayList }

  TArrayList = class(TAbstractList, IArrayList)
  private
    FCount: Integer;
    FCapacity: Integer;
    FList: TVariants;
  protected
    procedure Grow; overload; override;
    procedure Grow(N: Integer); overload; override;
    procedure ShiftRight(Index, N: Integer);
    procedure ShiftLeft(Index, N: Integer);
    function GetCapacity: Integer; override;
    function GetCount: Integer; override;
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetCount(NewCount: Integer); override;
    function IndexCompare(Index: Integer; const Value: Variant):
      Boolean; virtual;
  public
    constructor Create(ACapacity: Integer = 4; Sync: Boolean = True;
      ReadWriteSync: Boolean = False); overload; override;
    function Add(const Value: Variant): Integer; override;
    procedure Clear; override;
    function Delete(Index: Integer): Variant; override;
    procedure DeleteRange(Index, ACount: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function Get(Index: Integer): Variant; override;
    procedure Put(Index: Integer; const Value: Variant); override;
    function IndexOf(const Value: Variant; StartIndex, ACount: Integer): Integer; overload; override;
    function LastIndexOf(const Value: Variant; StartIndex, ACount: Integer): Integer; overload; override;
    procedure Insert(Index: Integer; const Value: Variant); override;
    procedure InsertRange(Index: Integer; const AList: IImmutableList); overload; override;
    procedure InsertRange(Index: Integer; const Container: Variant); overload; override;
    procedure InsertRange(Index: Integer; const ConstArray: array of const); overload; override;
    procedure Move(CurIndex, NewIndex: Integer); override;
    function ToArray: TVariants; overload; override;
    function ToArray(VarType: TVarType): Variant; overload; override;
  end;

  PHashItem = ^THashItem;

  THashItem = record
    Prev: PHashItem;
    Next: PHashItem;
    Index: Integer;
    HashCode: Integer;
  end;

  THashItemDynArray = array of PHashItem;

  TIndexCompareMethod = function (Index: Integer;
    const Value: Variant): Boolean of object;

  { THashBucket }

  THashBucket = class(TObject)
  private
    FCount: Integer;
    FFactor: Single;
    FCapacity: Integer;
    FIndices: THashItemDynArray;
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer);
    function GetHashIndex(HashCode: Integer): Integer;
    procedure Insert(Item: PHashItem);
    function Remove(HashCode, Index: Integer): PHashItem;
  public
    constructor Create(ACapacity: Integer = 16; Factor: Single = 0.75);
    destructor Destroy; override;
    function Add(HashCode, Index: Integer): PHashItem;
    procedure Clear;
    procedure Delete(HashCode, Index: Integer);
    function IndexOf(HashCode: Integer; const Value: Variant;
      CompareProc: TIndexCompareMethod; StartIndex, ACount: Integer): Integer;
    function LastIndexOf(HashCode: Integer; const Value: Variant;
      CompareProc: TIndexCompareMethod; StartIndex, ACount: Integer): Integer;
    function Modify(OldHashCode, NewHashCode, Index: Integer): PHashItem;
    property Count: Integer read FCount;
    property Capacity: Integer read FCapacity write SetCapacity;
  end;

  IHashedList = interface(IArrayList)
  ['{D2392014-7451-40EF-809E-D25BFB0FA661}']
  end;

  { THashedList }

  THashedList = class(TArrayList, IHashedList)
  private
    FHashBucket: THashBucket;
  protected
    procedure DeleteHash(Index, N: Integer);
    procedure InsertHash(Index, N: Integer);
    function HashOf(const Value: Variant): Integer; virtual;
  public
    constructor Create(ACapacity: Integer = 4; Sync: Boolean = True;
      ReadWriteSync: Boolean = False); overload; override;
    constructor Create(ACapacity: Integer; Factor: Single; Sync: Boolean = True;
      ReadWriteSync: Boolean = False); reintroduce; overload; virtual;
{$IFDEF BCB}
    constructor Create3(ACapacity: Integer; Factor: Single;
      Sync: Boolean); virtual; // for C++ Builder
{$ENDIF}
    destructor Destroy; override;
    function Add(const Value: Variant): Integer; override;
    procedure Clear; override;
    function Delete(Index: Integer): Variant; override;
    procedure DeleteRange(Index, ACount: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    procedure Put(Index: Integer; const Value: Variant); override;
    function IndexOf(const Value: Variant; StartIndex, ACount: Integer): Integer; overload; override;
    function LastIndexOf(const Value: Variant; StartIndex, ACount: Integer): Integer; overload; override;
    procedure Insert(Index: Integer; const Value: Variant); override;
    procedure InsertRange(Index: Integer; const AList: IImmutableList); overload; override;
    procedure InsertRange(Index: Integer; const Container: Variant); overload; override;
    procedure InsertRange(Index: Integer; const ConstArray: array of const); overload; override;
    procedure Move(CurIndex, NewIndex: Integer); override;
  end;

  ICaseInsensitiveArrayList = interface(IArrayList)
    ['{DAF8EA96-E2CC-F49C-8617-D999CA8A2EB3}']
  end;

  { TCaseInsensitiveArrayList }

  TCaseInsensitiveArrayList = class(TArrayList, ICaseInsensitiveArrayList)
  protected
    function Compare(const Value1, Value2: Variant): Integer; override;
    function IndexCompare(Index: Integer; const Value: Variant):
      Boolean; override;
  end;

  ICaseInsensitiveHashedList = interface(IHashedList)
  ['{9ECA15EC-9486-4BF6-AADD-BBD88890FAF8}']
  end;

  { TCaseInsensitiveHashedList }

  TCaseInsensitiveHashedList = class(THashedList, ICaseInsensitiveHashedList)
  protected
    function Compare(const Value1, Value2: Variant): Integer; override;
    function HashOf(const Value: Variant): Integer; override;
    function IndexCompare(Index: Integer; const Value: Variant):
      Boolean; override;
{$IFDEF BCB}
  public
    constructor Create4(ACapacity: Integer; Factor: Single; Sync,
      ReadWriteSync: Boolean); virtual; // for C++ Builder
{$ENDIF}
  end;

  TMapEntry = record
    Key: Variant;
    Value: Variant;
  end;

  IMapEnumerator = interface
  ['{5DE7A194-4476-42A6-A1E7-CB1D20AA7B0A}']
    function GetCurrent: TMapEntry;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TMapEntry read GetCurrent;
  end;

{$IFDEF SUPPORTS_ANONYMOUS_METHOD}
  TMapCompareMethod = reference to function (const Entry1, Entry2: TMapEntry): Integer;
{$ELSE}
  TMapCompareMethod = function (const Entry1, Entry2: TMapEntry): Integer of object;
{$ENDIF}

  IMap = interface(IReadWriteSync)
  ['{28B78387-CB07-4C28-B642-09716DAA2170}']
    procedure Assign(const Source: IMap);
    function GetCount: Integer;
    function GetKeys: IImmutableList;
    function GetValues: IImmutableList;
    function GetKey(const AValue: Variant): Variant;
    function GetValue(const AKey: Variant): Variant;
    procedure PutValue(const AKey: Variant; AValue: Variant);
    function Get(const AKey: Variant): Variant; overload;
    function Get(const AKey: Variant; out AValue: Variant): Boolean; overload;
    procedure Put(const AKey, AValue: Variant); overload;
    procedure Put(const AList: IImmutableList); overload;
    procedure Put(const AMap: IMap); overload;
    procedure Put(const Container: Variant); overload;
    procedure Put(const ConstArray: array of const); overload;
    function Add(const AKey, AValue: Variant): Boolean;
    procedure Clear;
    function CompareKey(const Entry1, Entry2: TMapEntry): Integer;
    function CompareValue(const Entry1, Entry2: TMapEntry): Integer;
    function ContainsKey(const AKey: Variant): Boolean;
    function ContainsValue(const AValue: Variant): Boolean;
    function Delete(const AKey: Variant): Variant;
    function GetEnumerator: IMapEnumerator;
    function Join(const ItemGlue: string = ';';
                  const KeyValueGlue: string = '=';
                  const LeftPad: string = '';
                  const RightPad: string = ''): string;
    procedure InitLock;
    procedure InitReadWriteLock;
    procedure Lock;
    procedure Unlock;
    procedure PutAll(const AList: IImmutableList); overload;
    procedure PutAll(const AMap: IMap); overload;
    procedure PutAll(const Container: Variant); overload;
    procedure PutAll(const ConstArray: array of const); overload;
    function ToList(ListClass: TListClass; Sync: Boolean = True;
      ReadWriteSync: Boolean = False): IList;
    function ToArrayList(Sync: Boolean = True;
      ReadWriteSync: Boolean = False): IArrayList;
    procedure Sort; overload;
    procedure Sort(CompareProc: TMapCompareMethod); overload;
    procedure SortByValue;
    procedure TrimExcess;
    property Count: Integer read GetCount;
    property Key[const AValue: Variant]: Variant read GetKey;
    property Value[const AKey: Variant]: Variant read GetValue write PutValue; default;
    property Keys: IImmutableList read GetKeys;
    property Values: IImmutableList read GetValues;
  end;

  { TAbstractMap }

{$M+}
  TAbstractMap = class(TInterfacedObject, IMap, IInvokeableVarObject)
  private
    FLock: TCriticalSection;
    FReadWriteLock: TMultiReadExclusiveWriteSynchronizer;
    function GetValue(const AKey: Variant): Variant;
    procedure PutValue(const AKey: Variant; AValue: Variant);
  protected
    procedure Assign(const Source: IMap); virtual; abstract;
    function GetCount: Integer; virtual; abstract;
    function GetKeys: IImmutableList; virtual; abstract;
    function GetValues: IImmutableList; virtual; abstract;
    function CompareKey(const Entry1, Entry2: TMapEntry): Integer; virtual; abstract;
    function CompareValue(const Entry1, Entry2: TMapEntry): Integer; virtual; abstract;
    function Invoke(const Name: string; const Arguments: TVarDataArray): Variant; virtual;
  public
    constructor Create(ACapacity: Integer = 16; Factor: Single = 0.75;
      Sync: Boolean = True; ReadWriteSync: Boolean = False); overload; virtual; abstract;
    constructor Create(ACapacity: Integer; Sync: Boolean;
      ReadWriteSync: Boolean = False); overload; virtual;
    constructor Create(Sync: Boolean;
      ReadWriteSync: Boolean = False); overload; virtual;
    constructor Create(const AList: IImmutableList; Sync: Boolean = True;
      ReadWriteSync: Boolean = False); overload; virtual;
    constructor Create(const AMap: IMap; Sync: Boolean = True;
      ReadWriteSync: Boolean = False); overload; virtual;
    constructor Create(const Container: Variant; Sync: Boolean = True;
      ReadWriteSync: Boolean = False); overload; virtual;
    constructor Create(const ConstArray: array of const; Sync: Boolean = True;
      ReadWriteSync: Boolean = False); overload; virtual;
{$IFDEF BCB}
    constructor Create0; virtual;
    constructor Create1(ACapacity: Integer); virtual;
    constructor Create2(ACapacity: Integer; Factor: Single); virtual;
    constructor Create3(ACapacity: Integer; Factor: Single; Sync: Boolean); virtual;
    constructor CreateS(Sync: Boolean); virtual;
    constructor CreateL1(const AList: IImmutableList); virtual; // for C++ Builder
    constructor CreateM1(const AMap: IMap); virtual; // for C++ Builder
    constructor CreateA1(const Container: Variant); virtual; // for C++ Builder
    constructor CreateCA1(const ConstArray: array of const); virtual; // for C++ Builder
    constructor CreateL2(const AList: IImmutableList; Sync: Boolean); virtual; // for C++ Builder
    constructor CreateA2(const Container: Variant; Sync: Boolean); virtual; // for C++ Builder
    constructor CreateCA2(const ConstArray: array of const; Sync: Boolean); virtual; // for C++ Builder
{$ENDIF}
    destructor Destroy; override;
    function Get(const AKey: Variant): Variant; overload; virtual; abstract;
    function Get(const AKey: Variant; out AValue: Variant): Boolean; overload; virtual; abstract;
    function GetKey(const AValue: Variant): Variant; virtual; abstract;
    procedure Put(const AKey, AValue: Variant); overload; virtual; abstract;
    procedure Put(const AList: IImmutableList); overload; virtual;
    procedure Put(const AMap: IMap); overload; virtual;
    procedure Put(const Container: Variant); overload; virtual;
    procedure Put(const ConstArray: array of const); overload; virtual;
    function Add(const AKey, AValue: Variant): Boolean; virtual; abstract;
    procedure Clear; virtual; abstract;
    function ContainsKey(const AKey: Variant): Boolean; virtual; abstract;
    function ContainsValue(const AValue: Variant): Boolean; virtual; abstract;
    function Delete(const AKey: Variant): Variant; virtual; abstract;
    function GetEnumerator: IMapEnumerator; virtual;
    function Join(const ItemGlue: string = ';';
                  const KeyValueGlue: string = '=';
                  const LeftPad: string = '';
                  const RightPad: string = ''): string;
    class function Split(Str: string; const ItemSeparator: string = ';';
      const KeyValueSeparator: string = '='; Limit: Integer = 0;
      TrimKey: Boolean = False; TrimValue: Boolean = False;
      SkipEmptyKey: Boolean = False; SkipEmptyValue: Boolean = False;
      Sync: Boolean = True; ReadWriteSync: Boolean = False): IMap;
    procedure InitLock;
    procedure InitReadWriteLock;
    procedure Lock;
    procedure Unlock;
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
    procedure PutAll(const AList: IImmutableList); overload; virtual;
    procedure PutAll(const AMap: IMap); overload; virtual;
    procedure PutAll(const Container: Variant); overload; virtual;
    procedure PutAll(const ConstArray: array of const); overload; virtual;
    function ToList(ListClass: TListClass; Sync: Boolean = True;
      ReadWriteSync: Boolean = False): IList; virtual; abstract;
    function ToArrayList(Sync: Boolean = True;
      ReadWriteSync: Boolean = False): IArrayList; virtual; abstract;
    procedure Sort; overload; virtual; abstract;
    procedure Sort(CompareProc: TMapCompareMethod); overload; virtual; abstract;
    procedure SortByValue; virtual; abstract;
    procedure TrimExcess; virtual; abstract;
    property Key[const AValue: Variant]: Variant read GetKey;
    property Value[const AKey: Variant]: Variant read GetValue write PutValue; default;
  published
    property Count: Integer read GetCount;
    property Keys: IImmutableList read GetKeys;
    property Values: IImmutableList read GetValues;
  end;
{$M-}

  TMapClass = class of TAbstractMap;
  { function ContainsValue is an O(n) operation in THashMap,
    and property Key is also an O(n) operation. They perform
    a linear search. THashedMap is faster than THashMap when
    do those operations. But THashMap needs less memory than
    THashedMap. }

  IHashMap = interface(IMap)
  ['{B66C3C4F-3FBB-41FF-B0FA-5E73D87CBE56}']
  end;

  { THashMap }

  THashMap = class(TAbstractMap, IHashMap)
  private
    FKeys: IList;
    FValues: IList;
  protected
    function GetCount: Integer; override;
    function GetKeys: IImmutableList; override;
    function GetValues: IImmutableList; override;
    procedure InitData(AKeys, AValues: IList);
    function CompareKey(const Entry1, Entry2: TMapEntry): Integer; override;
    function CompareValue(const Entry1, Entry2: TMapEntry): Integer; override;
  public
    constructor Create(ACapacity: Integer = 16; Factor: Single = 0.75;
      Sync: Boolean = True; ReadWriteSync: Boolean = False); overload; override;
    procedure Assign(const Source: IMap); override;
    function Get(const AKey: Variant): Variant; overload; override;
    function Get(const AKey: Variant; out AValue: Variant): Boolean; overload; override;
    function GetKey(const AValue: Variant): Variant; override;
    procedure Put(const AKey, AValue: Variant); overload; override;
    function Add(const AKey, AValue: Variant): Boolean; override;
    procedure Clear; override;
    function ContainsKey(const AKey: Variant): Boolean; override;
    function ContainsValue(const AValue: Variant): Boolean; override;
    function Delete(const AKey: Variant): Variant; override;
    function ToList(ListClass: TListClass; Sync: Boolean = True;
      ReadWriteSync: Boolean = False): IList; override;
    function ToArrayList(Sync: Boolean = True;
      ReadWriteSync: Boolean = False): IArrayList; override;
    procedure Sort; overload; override;
    procedure Sort(CompareProc: TMapCompareMethod); overload; override;
    procedure SortByValue; override;
    procedure TrimExcess; override;
  end;

  { function ContainsValue is an O(1) operation in THashedMap,
    and property Key is also an O(1) operation. }

  IHashedMap = interface(IHashMap)
  ['{D2598919-07DA-401A-A971-7DB8624E2660}']
  end;

  THashedMap = class(THashMap, IHashedMap)
  public
    constructor Create(ACapacity: Integer = 16; Factor: Single = 0.75;
      Sync: Boolean = True; ReadWriteSync: Boolean = False); override;
  end;

  ICaseInsensitiveHashMap = interface(IHashMap)
  ['{B8F8E5E7-53ED-48BE-B171-2EA2548FCAC7}']
  end;

  TCaseInsensitiveHashMap = class(THashMap, ICaseInsensitiveHashMap)
  public
    constructor Create(ACapacity: Integer = 16; Factor: Single = 0.75;
      Sync: Boolean = True; ReadWriteSync: Boolean = False); override;
  end;

  ICaseInsensitiveHashedMap = interface(IHashMap)
  ['{839DCE08-95DE-462F-B59D-16BA89D3DC6B}']
  end;

  TCaseInsensitiveHashedMap = class(THashMap, ICaseInsensitiveHashedMap)
  public
    constructor Create(ACapacity: Integer = 16; Factor: Single = 0.75;
      Sync: Boolean = True; ReadWriteSync: Boolean = False); override;
  end;

{$IF NOT DEFINED(DELPHI2009_UP) AND NOT DEFINED(FPC)}
  TBytesStream = class(TMemoryStream)
  private
    FBytes: TBytes;
  protected
    function Realloc(var NewCapacity: Integer): Pointer; override;
  public
    constructor Create(const ABytes: TBytes); overload;
    property Bytes: TBytes read FBytes;
  end;
{$IFEND}

  TChars = array of Char;

  TStringBuffer = class(TObject)
  private
    FData: TChars;
    FPosition: Integer;
    FCapacity: Integer;
    FLength: Integer;
    procedure Grow;
    procedure SetPosition(NewPosition: Integer);
    procedure SetCapacity(NewCapacity: Integer);
  public
    constructor Create(ACapacity: Integer = 255); overload;
    constructor Create(const AString: string); overload;
    function ReadString(Count: Integer): string;
    procedure WriteString(const AString: string);
    procedure InsertString(const AString: string);
    function Seek(Offset: Integer; Origin: Word): Integer;
    function ToString: string; {$IFDEF DELPHI2009_UP}override;{$ENDIF}{$IFDEF FPC}override;{$ENDIF}
    property Position: Integer read FPosition write SetPosition;
    property Length: Integer read FLength;
    property Capacity: Integer read FCapacity write SetCapacity;
  end;

  ISmartObject = interface
  ['{496CD091-9C33-423A-BC4A-61AF16C74A75}']
    function Value: TObject;
  end;

  TSmartObject = class(TInterfacedObject, ISmartObject)
  private
    FValue: TObject;
  protected
    function Value: TObject;
  public
    constructor Create(AObject: TObject); virtual;
    destructor Destroy; override;
  end;

  TSmartClass = class of TSmartObject;

{$IFDEF SUPPORTS_GENERICS}
  ISmartObject<T> = reference to function: T;

{$M+}
  TSmartObject<T: class, constructor> = class(TSmartObject, ISmartObject<T>)
  public
    constructor Create; reintroduce;
    function Invoke: T;
  end;
{$M-}
{$ENDIF}

const

{$IFDEF CPU64}
  varNativeInt = varInt64;
{$ELSE}
  varNativeInt = varInteger;
{$ENDIF}

var
  varObject: TVarType;

{$IFNDEF DELPHI2009_UP}
function GetTypeName(const Info: PTypeInfo): string;
{$ENDIF}
function GetTypeSize(const Info: PTypeInfo): Integer;
{$IFDEF DELPHI6}
function FindVarData(const Value: Variant): PVarData;
{$ENDIF}
{$IF DEFINED(FPC) OR DEFINED(DELPHI6)}
function VarIsType(const V: Variant; AVarType: TVarType): Boolean; overload;
function VarIsType(const V: Variant; const AVarTypes: array of TVarType):
  Boolean; overload;
function VarIsCustom(const V: Variant): Boolean;
function VarIsOrdinal(const V: Variant): Boolean;
function VarIsFloat(const V: Variant): Boolean;
function VarIsNumeric(const V: Variant): Boolean;
function VarIsStr(const V: Variant): Boolean;
function VarIsEmpty(const V: Variant): Boolean;
function VarIsNull(const V: Variant): Boolean;
{$IFEND}
function VarIsObj(const Value: Variant): Boolean; overload;
function VarIsObj(const Value: Variant; AClass: TClass): Boolean; overload;
function VarToObj(const Value: Variant): TObject; overload;
function VarToObj(const Value: Variant; AClass: TClass):
  TObject; overload;
function VarToObj(const Value: Variant; AClass: TClass; out AObject):
  Boolean; overload;
function ObjToVar(const Value: TObject): Variant;
function VarIsList(const Value: Variant): Boolean;
function VarIsMap(const Value: Variant): Boolean;
function VarToList(const Value: Variant): IList;
function VarToMap(const Value: Variant): IMap;
function VarIsIntf(const Value: Variant): Boolean; overload;
function VarIsIntf(const Value: Variant; const IID: TGUID): Boolean; overload;
function VarToIntf(const Value: Variant; const IID: TGUID; out AIntf): Boolean;
function IntfToObj(const Intf: IInterface): TInterfacedObject;
function VarToBytes(const Value: Variant): TBytes;
function BytesToVar(const Value: TBytes): Variant;

{$IFDEF FPC}
operator :=(const Source : Variant) Dest : TObject; inline;
operator :=(const Source : TObject) Dest : Variant; inline;
operator :=(const Source : TBytes) Dest : Variant; inline;
{$ENDIF}

function VarEquals(const Left, Right: Variant): Boolean;
function VarRef(const Value: Variant): Variant;
function VarDeref(const Value: Variant): Variant;

function GetPropValue(Instance: TObject; PropInfo: PPropInfo): Variant; overload;
function GetPropValue(Instance: TObject; const Name: string): Variant; overload;
procedure SetPropValue(Instance: TObject; PropInfo: PPropInfo;
  const Value: Variant); overload;
procedure SetPropValue(Instance: TObject; const Name: string;
  const Value: Variant); overload;

function GetStoredPropList(Instance: TObject; out PropList: PPropList): Integer;

function GetProperties(Instance: TObject): IMap;
function GetStoredProperties(Instance: TObject): IMap;

procedure SetProperties(Instance: TObject; const Properties: IMap);

function CopyVarRec(const Item: TVarRec): TVarRec;
function CreateConstArray(const Elements: array of const): TConstArray;
procedure FinalizeVarRec(var Item: TVarRec);
procedure FinalizeConstArray(var Arr: TConstArray);
function VarRecToVar(const V: TVarRec): Variant;
function ToVariants(const Elements: array of const): TVariants;

procedure RegisterClass(const AClass: TClass; const Alias: string); overload;
procedure RegisterClass(const AClass: TInterfacedClass; const IID: TGUID; const Alias: string); overload;
function GetClassByAlias(const Alias: string): TClass;
function GetClassAlias(const AClass: TClass): string;
function GetClassByInterface(const IID: TGUID): TInterfacedClass;
function HasRegisterWithInterface(const AClass: TInterfacedClass): Boolean;
function GetInterfaceByClass(const AClass: TInterfacedClass): TGUID;

function ArrayList(const Elements: array of const): IArrayList;
function HashedList(const Elements: array of const): IHashedList;
function CaseInsensitiveArrayList(const Elements: array of const): ICaseInsensitiveArrayList;
function CaseInsensitiveHashedList(const Elements: array of const): ICaseInsensitiveHashedList;

function HashMap(const Elements: array of const): IHashMap;
function HashedMap(const Elements: array of const): IHashedMap;
function CaseInsensitiveHashMap(const Elements: array of const): ICaseInsensitiveHashMap;
function CaseInsensitiveHashedMap(const Elements: array of const): ICaseInsensitiveHashedMap;

type
  TClassManager = class
{$IFDEF SUPPORTS_GENERICS}
  private
    class procedure RegisterSmartObject<T, I>(const Alias: string);
    class procedure Register(const TypeInfo: PTypeInfo; TypeName: string); overload;
  public
    class procedure Register(const TypeInfo: PTypeInfo); overload;
    class procedure Register<T: class, constructor>; overload;
    class procedure Register<T: class, constructor>(const Alias: string); overload;
    class procedure Register<T: class, constructor; I: IInterface>(const Alias: string); overload;
    class function GetAlias<T: class>: string; overload;
    class function GetInterface<T: TInterfacedObject>: TGUID; overload;
    class function GetClass<I>: TInterfacedClass; overload;
    class function TypeInfo(const Name: string): PTypeInfo; overload;
{$ENDIF}
    class procedure Register(const AClass: TClass; const Alias: string); overload;
    class procedure Register(const AClass: TInterfacedClass; const IID: TGUID; const Alias: string); overload;
    class function GetAlias(const AClass: TClass): string; overload;
    class function GetClass(const Alias: string): TClass; overload;
    class function GetClass(const IID: TGUID): TInterfacedClass; overload;
    class function GetInterface(const AClass: TInterfacedClass): TGUID; overload;
  end;

function ListSplit(ListClass: TListClass; Str: string;
  const Separator: string = ','; Limit: Integer = 0; TrimItem: Boolean = False;
  SkipEmptyItem: Boolean = False): IList;
function MapSplit(MapClass: TMapClass; Str: string;
  const ItemSeparator: string = ';'; const KeyValueSeparator: string = '=';
  Limit: Integer = 0; TrimKey: Boolean = False; TrimValue: Boolean = False;
  SkipEmptyKey: Boolean = False; SkipEmptyValue: Boolean = False): IMap;

{$IFNDEF DELPHI2009_UP}
  // TBytes/string conversion routines
function BytesOf(const Val: RawByteString): TBytes; overload;
function BytesOf(const Val: WideChar): TBytes; overload;
function BytesOf(const Val: AnsiChar): TBytes; overload;
function BytesOf(const Val: WideString): TBytes; overload;
function StringOf(const Bytes: TBytes): RawByteString;
function WideStringOf(const Value: TBytes): WideString;
function WideBytesOf(const Value: WideString): TBytes;
{$ENDIF}

{$IFNDEF DELPHIXE3_UP}
function BytesOf(const Val: Pointer; const Len: integer): TBytes; overload;
{$ENDIF}

type

  { THproseContext }

  THproseContext = class
  private
    FUserData: ICaseInsensitiveHashMap;
    function Get(Key: string): Variant;
    procedure Put(Key: string; const Value: Variant);
  public
    constructor Create(const AUserData: IMap);
    property UserData: ICaseInsensitiveHashMap read FUserData write FUserData;
    property Item[Key: string]: Variant read Get write Put; default;
  end;

  IFilter = interface
  ['{4AD7CCF2-1121-4CA4-92A7-5704C5956BA4}']
    function InputFilter(var Data: TBytes; const Context: THproseContext): TBytes;
    function OutputFilter(var Data: TBytes; const Context: THproseContext): TBytes;
  end;

  TFilter = class(TInterfacedObject, IFilter)
  public
    function InputFilter(var Data: TBytes; const Context: THproseContext): TBytes;  virtual; abstract;
    function OutputFilter(var Data: TBytes; const Context: THproseContext): TBytes; virtual; abstract;
  end;

  { TFilterList }

  TFilterList = class
  private
    FFilters: IList;
  public
    constructor Create;
    function Get(Index: Integer): IFilter;
    function GetCount: Integer;
    function Add(const Filter: IFilter): TFilterList;
    function Remove(const Filter: IFilter): TFilterList;
    function InputFilter(var Data: TBytes; const Context: THproseContext): TBytes;
    function OutputFilter(var Data: TBytes; const Context: THproseContext): TBytes;
    property Filter[Index: Integer]: IFilter read Get; default;
    property Count: Integer read GetCount;
  end;


  PNextInvokeHandler = ^TNextInvokeHandler;
  PNextFilterHandler = ^TNextFilterHandler;

{$IFDEF SUPPORTS_ANONYMOUS_METHOD}
  TInvokeHandler = reference to function(const AName: String;
                                         var Args: TVariants;
                                     const Context: THproseContext;
                                    Next: PNextInvokeHandler): Variant;
  TFilterHandler = reference to function(var Request: TBytes;
                                     const Context: THproseContext;
                                    Next: PNextFilterHandler): TBytes;
{$ELSE}
  TInvokeHandler = function(const AName: String;
                            var Args: TVariants;
                        const Context: THproseContext;
                       Next: PNextInvokeHandler): Variant of object;
  TFilterHandler = function(var Request: TBytes;
                        const Context: THproseContext;
                       Next: PNextFilterHandler): TBytes of object;
{$ENDIF}

  TNextInvokeHandler = record
    Handler: TInvokeHandler;
    Next: PNextInvokeHandler;
  end;

  TNextFilterHandler = record
    Handler: TFilterHandler;
    Next: PNextFilterHandler;
  end;

  THandlerManager = class
  private
    FInvokeHandler: PNextInvokeHandler;
    FLastInvokeHandler: PNextInvokeHandler;
    FDefaultInvokeHandler: PNextInvokeHandler;
    FBeforeFilterHandler: PNextFilterHandler;
    FLastBeforeFilterHandler: PNextFilterHandler;
    FDefaultBeforeFilterHandler: PNextFilterHandler;
    FAfterFilterHandler: PNextFilterHandler;
    FLastAfterFilterHandler: PNextFilterHandler;
    FDefaultAfterFilterHandler: PNextFilterHandler;
    function GetNextInvokeHandler(const Next: PNextInvokeHandler;
      const Handler: TInvokeHandler): PNextInvokeHandler;
    function GetNextFilterHandler(const Next: PNextFilterHandler;
      const Handler: TFilterHandler): PNextFilterHandler;
  public
    constructor Create(const AInvokeHandler: TInvokeHandler;
      const ABeforeFilterHandler: TFilterHandler;
      const AAfterFilterHandler: TFilterHandler);
    destructor Destroy; override;
    procedure AddInvokeHandler(const Handler: TInvokeHandler);
    procedure AddBeforeFilterHandler(const Handler: TFilterHandler);
    procedure AddAfterFilterHandler(const Handler: TFilterHandler);
    property InvokeHandler: PNextInvokeHandler read FInvokeHandler;
    property BeforeFilterHandler: PNextFilterHandler read FBeforeFilterHandler;
    property AfterFilterHandler: PNextFilterHandler read FAfterFilterHandler;
  end;

implementation

{$IFDEF NEXTGEN}
{$ZEROBASEDSTRINGS OFF}
{$ENDIF}

uses RTLConsts
{$IFNDEF FPC}, StrUtils{$ENDIF}
{$IFDEF DELPHIXE4_UP}{$IFNDEF NEXTGEN}, AnsiStrings{$ENDIF}{$ENDIF}
{$IFDEF SUPPORTS_RTTI}, Rtti{$ENDIF}{$IFDEF DELPHIXE2_UP}, ObjAuto{$ENDIF}
{$IFDEF DELPHI7_UP}, ObjAutoX{$ENDIF};

type

  TVarObjectType = class(TInvokeableVariantType, IVarInstanceReference)
  protected
{$IF DEFINED(FPC) AND (FPC_VERSION < 3.0)}
    procedure DispInvoke(Dest: PVarData; const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); override;
{$IFEND}
    { IVarInstanceReference }
    function GetInstance(const V: TVarData): TObject;
  public
    { IVarInvokeable }
    function DoFunction(var Dest: TVarData; const V: TVarData;
      const Name: string; const Arguments: TVarDataArray): Boolean; override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty({$IF DEFINED(FPC) AND (FPC_VERSION >= 3.0)}var{$ELSE}const{$IFEND} V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;

    procedure CastTo(var Dest: TVarData; const Source: TVarData;
      const AVarType: TVarType); override;
    procedure Clear(var V: TVarData); override;
    function CompareOp(const Left, Right: TVarData;
      const Operation: TVarOp): Boolean; override;
    procedure Copy(var Dest: TVarData; const Source: TVarData;
      const Indirect: Boolean); override;
    function IsClear(const V: TVarData): Boolean; override;
  end;

var
  VarObjectType: TVarObjectType;

{$IFDEF DELPHIXE2_UP}
const
{ Maximum TList size }
  MaxListSize = Maxint div 16;
{$ENDIF}

{$IFNDEF DELPHI2009_UP}
function GetTypeName(const Info: PTypeInfo): string;
begin
  Result := string(Info^.Name);
end;
{$ENDIF}

function GetTypeSize(const Info: PTypeInfo): Integer;
{$IFNDEF SUPPORTS_RTTI}
var
  TypeData: PTypeData;
begin
  Result := 4;
  TypeData := GetTypeData(Info);
  case Info^.Kind of
    tkInteger:
      case TypeData^.OrdType of
        otSByte,
        otUByte:
            Result := SizeOf(Byte);
        otSWord,
        otUWord:
          begin
            Result := SizeOf(Word);
          end;
        otSLong,
        otULong:
          ;
      end;
    tkFloat:
      case TypeData^.FloatType of
        ftSingle:
          Result := SizeOf(Single);
        ftDouble:
          Result := SizeOf(Double);
        ftComp:
          Result := SizeOf(Comp);
        ftCurr:
          Result := SizeOf(Currency);
        ftExtended:
          Result := SizeOf(Extended);
      end;
    tkChar:
      Result := 1;
    tkWChar:
      Result := 2;
    tkInt64:
      Result := SizeOf(Int64);
    tkVariant:
      Result := SizeOf(TVarData);
    tkEnumeration:
      Result := 1;
  end;
end;
{$ELSE}
var
  Context: TRttiContext;
  Typ: TRttiType;
begin
  if (Info = TypeInfo(Variant)) or (Info = TypeInfo(OleVariant)) then
    Exit(SizeOf(TVarData));
  Result := SizeOf(Pointer);
  Typ := Context.GetType(Info);
  if Assigned(Typ) then Result := Typ.TypeSize;
end;
{$ENDIF}

{$IFDEF DELPHI6}
function FindVarData(const Value: Variant): PVarData;
begin
  Result := @TVarData(Value);
  while Result.VType = varByRef or varVariant do
    Result := PVarData(Result.VPointer);
end;
{$ENDIF}

{$IF DEFINED(FPC) OR DEFINED(DELPHI6)}
function VarIsType(const V: Variant; AVarType: TVarType): Boolean;
begin
  Result := FindVarData(V)^.VType = AVarType;
end;

function VarIsType(const V: Variant; const AVarTypes: array of TVarType): Boolean;
var
  I: Integer;
  P: PVarData;
begin
  Result := False;
  P := FindVarData(V);
  for I := Low(AVarTypes) to High(AVarTypes) do
    if P^.VType = AVarTypes[I] then
    begin
      Result := True;
      Break;
    end;
end;

function VarTypeIsCustom(const AVarType: TVarType): Boolean;
var
  LHandler: TCustomVariantType;
begin
  Result := FindCustomVariantType(AVarType, LHandler);
end;

function VarIsCustom(const V: Variant): Boolean;
begin
  Result := VarTypeIsCustom(FindVarData(V)^.VType);
end;

function VarTypeIsOrdinal(const AVarType: TVarType): Boolean;
begin
  Result := AVarType in [varSmallInt, varInteger, varBoolean, varShortInt,
                         varByte, varWord, varLongWord, varInt64];
end;

function VarIsOrdinal(const V: Variant): Boolean;
begin
  Result := VarTypeIsOrdinal(FindVarData(V)^.VType);
end;

function VarTypeIsFloat(const AVarType: TVarType): Boolean;
begin
  Result := AVarType in [varSingle, varDouble, varCurrency];
end;

function VarIsFloat(const V: Variant): Boolean;
begin
  Result := VarTypeIsFloat(FindVarData(V)^.VType);
end;

function VarTypeIsNumeric(const AVarType: TVarType): Boolean;
begin
  Result := VarTypeIsOrdinal(AVarType) or VarTypeIsFloat(AVarType);
end;

function VarIsNumeric(const V: Variant): Boolean;
begin
  Result := VarTypeIsNumeric(FindVarData(V)^.VType);
end;

function VarTypeIsStr(const AVarType: TVarType): Boolean;
begin
  Result := (AVarType = varOleStr) or (AVarType = varString);
end;

function VarIsStr(const V: Variant): Boolean;
begin
  Result := VarTypeIsStr(FindVarData(V)^.VType);
end;

function VarIsEmpty(const V: Variant): Boolean;
begin
  Result := FindVarData(V)^.VType = varEmpty;
end;

function VarIsNull(const V: Variant): Boolean;
begin
  Result := FindVarData(V)^.VType = varNull;
end;

{$IFEND}

function VarToObj(const Value: Variant): TObject;
var
  P: PVarData;
  VType: TVarType;
begin
  P := FindVarData(Value);
  VType := P^.VType;
  Result := nil;
  try
    if VType = varObject then
      Result := TObject(P^.VPointer)
    else if (VType <> varNull) and (VType <> varEmpty) then
      Error(reInvalidCast);
  except
    Error(reInvalidCast);
  end;
end;

function VarToObj(const Value: Variant; AClass: TClass): TObject;
var
  P: PVarData;
  VType: TVarType;
begin
  P := FindVarData(Value);
  VType := P^.VType;
  Result := nil;
  try
    if VType = varObject then begin
      Result := TObject(P^.VPointer);
      if not (Result is AClass) then Error(reInvalidCast);
    end
    else if (VType <> varNull) and (VType <> varEmpty) then
      Error(reInvalidCast);
  except
    Error(reInvalidCast);
  end;
end;

function VarToObj(const Value: Variant; AClass: TClass; out AObject):
  Boolean;
var
  Obj: TObject absolute AObject;
  P: PVarData;
  VType: TVarType;
begin
  P := FindVarData(Value);
  VType := P^.VType;
  Obj := nil;
  Result := True;
  try
    if (VType = varObject) and (TObject(P^.VPointer) is AClass) then begin
      Obj := TObject(P^.VPointer) as AClass;
      Result := Assigned(Obj) or not Assigned(P^.VPointer);
    end
    else if (VType <> varNull) and (VType <> varEmpty) then
      Result := False;
  except
    Result := False;
  end;
end;

function ObjToVar(const Value: TObject): Variant;
begin
  VarClear(Result);
  TObject(TVarData(Result).VPointer) := Value;
  TVarData(Result).VType := varObject;
end;

function VarIsObj(const Value: Variant): Boolean;
begin
  Result := VarIsObj(Value, TObject);
end;

function VarIsObj(const Value: Variant; AClass: TClass): Boolean;
var
  P: PVarData;
  VType: TVarType;
begin
  P := FindVarData(Value);
  VType := P^.VType;
  Result := True;
  try
    if VType = varObject then
      Result := TObject(P^.VPointer) is AClass
    else if (VType <> varNull) and (VType <> varEmpty) then
      Result := False;
  except
    Result := False;
  end;
end;

function VarIsList(const Value: Variant): Boolean;
var
  P: PVarData;
begin
  P := FindVarData(Value);
  Result := (P^.VType = varUnknown) and
            Supports(IInterface(P^.VUnknown), IList) or
            (P^.VType = (varUnknown or varByRef)) and
            Supports(IInterface(P^.VPointer^), IList) or
            VarIsObj(Value, TAbstractList);
end;

function VarToList(const Value: Variant): IList;
var
  P: PVarData;
  VType: TVarType;
begin
  P := FindVarData(Value);
  VType := P^.VType;
  if (VType = varEmpty) or (VType = varNull) then
    Result := nil
  else if VType = varUnknown then
    Supports(IInterface(P^.VUnknown), IList, Result)
  else if VType = (varUnknown or varByRef) then
    Supports(IInterface(P^.VPointer^), IList, Result)
  else if VarIsObj(Value, TAbstractList) then
    VarToObj(Value, TAbstractList, Result)
  else
    Error(reInvalidCast);
end;

function VarIsMap(const Value: Variant): Boolean;
var
  P: PVarData;
begin
  P := FindVarData(Value);
  Result := (P^.VType = varUnknown) and
            Supports(IInterface(P^.VUnknown), IMap) or
            (P^.VType = (varUnknown or varByRef)) and
            Supports(IInterface(P^.VPointer^), IMap) or
            VarIsObj(Value, TAbstractMap);
end;

function VarToMap(const Value: Variant): IMap;
var
  P: PVarData;
  VType: TVarType;
begin
  P := FindVarData(Value);
  VType := P^.VType;
  if (VType = varEmpty) or (VType = varNull) then
    Result := nil
  else if VType = varUnknown then
    Supports(IInterface(P^.VUnknown), IMap, Result)
  else if VType = (varUnknown or varByRef) then
    Supports(IInterface(P^.VPointer^), IMap, Result)
  else if VarIsObj(Value, TAbstractMap) then
    VarToObj(Value, TAbstractMap, Result)
  else
    Error(reInvalidCast);
end;

function VarIsIntf(const Value: Variant): Boolean;
var
  VType: TVarType;
begin
  VType := FindVarData(Value)^.VType;
  Result := (VType = varUnknown) or (VType = (varUnknown or varByRef)) or
            (VType = varNull) or (VType = varEmpty);
end;

function VarIsIntf(const Value: Variant; const IID: TGUID): Boolean;
var
  P: PVarData;
  VType: TVarType;
begin
  P := FindVarData(Value);
  VType := P^.VType;
  if (VType = varNull) or (VType = varEmpty) then
    Result := True
  else if VType = varUnknown then
    Result := Supports(IInterface(P^.VUnknown), IID)
  else if VType = (varUnknown or varByRef) then
    Result := Supports(IInterface(P^.VPointer^), IID)
  else
    Result := False;
end;

function VarToIntf(const Value: Variant; const IID: TGUID; out AIntf): Boolean;
var
  P: PVarData;
  VType: TVarType;
begin
  P := FindVarData(Value);
  VType := P^.VType;
  if (VType = varNull) or (VType = varEmpty) then
    Result := Supports(nil, IID, AIntf)
  else if VType = varUnknown then
    Result := Supports(IInterface(P^.VUnknown), IID, AIntf)
  else if VType = (varUnknown or varByRef) then begin
    Result := Supports(IInterface(P^.VPointer^), IID, AIntf)
  end
  else
    Result := False;
end;

{$IFNDEF DELPHI2010_UP}
type
  TObjectFromInterfaceStub = packed record
    Stub: cardinal;
    case integer of
    0: (ShortJmp: ShortInt);
    1: (LongJmp: LongInt)
  end;
  PObjectFromInterfaceStub = ^TObjectFromInterfaceStub;
{$ENDIF}

function IntfToObj(const Intf: IInterface): TInterfacedObject; {$IFDEF DELPHI2010_UP}inline;{$ENDIF}
begin
  if not Assigned(Intf) then
    result := nil
  else begin
{$IFDEF DELPHI2010_UP}
    result := Intf as TInterfacedObject; // slower but always working
{$ELSE}
    with PObjectFromInterfaceStub(PPointer(PPointer(Intf)^)^)^ do
    case Stub of
      $04244483: result := TInterfacedObject(Pointer(NativeInt(Intf) + ShortJmp));
      $04244481: result := TInterfacedObject(Pointer(NativeInt(Intf) + LongJmp));
      else       result := nil;
    end;
{$ENDIF}
  end;
end;

function VarToBytes(const Value: Variant): TBytes;
var
  Count: Integer;
begin
  Count := VarArrayHighBound(Value, 1) - VarArrayLowBound(Value, 1) + 1;
  SetLength(Result, Count);
  Move(VarArrayLock(Value)^, Result[0], Count);
  VarArrayUnLock(Value);
end;

function BytesToVar(const Value: TBytes): Variant;
var
  Count: Integer;
begin
  Count := Length(Value);
  Result := VarArrayCreate([0, Count - 1], varByte);
  Move(Value[0], VarArrayLock(Result)^, Count);
  VarArrayUnlock(Result);
end;

{$IFDEF FPC}
operator :=(const Source : Variant) Dest : TObject; inline;
begin
  Dest := VarToObj(Source);
end;

operator :=(const Source : TObject) Dest : Variant; inline;
begin
  Dest := ObjToVar(Source);
end;

operator :=(const Source : TBytes) Dest : Variant; inline;
begin
  Dest := BytesToVar(Source);
end;
{$ENDIF}

{ GetPropValue/SetPropValue }

procedure PropertyNotFound(const Name: string);
begin
  raise EPropertyError.CreateResFmt(@SUnknownProperty, [Name]);
end;

procedure PropertyConvertError(const Name: string);
begin
  raise EPropertyConvertError.CreateResFmt(@SInvalidPropertyType, [Name]);
end;

{$IFNDEF FPC}
{$IFNDEF DELPHI2007_UP}
type
  TAccessStyle = (asFieldData, asAccessor, asIndexedAccessor);

function GetAccessToProperty(Instance: TObject; PropInfo: PPropInfo;
  AccessorProc: Longint; out FieldData: Pointer;
  out Accessor: TMethod): TAccessStyle;
begin
  if (AccessorProc and $FF000000) = $FF000000 then
  begin  // field - Getter is the field's offset in the instance data
    FieldData := Pointer(Integer(Instance) + (AccessorProc and $00FFFFFF));
    Result := asFieldData;
  end
  else
  begin
    if (AccessorProc and $FF000000) = $FE000000 then
      // virtual method  - Getter is a signed 2 byte integer VMT offset
      Accessor.Code := Pointer(PInteger(PInteger(Instance)^ + SmallInt(AccessorProc))^)
    else
      // static method - Getter is the actual address
      Accessor.Code := Pointer(AccessorProc);

    Accessor.Data := Instance;
    if PropInfo^.Index = Integer($80000000) then  // no index
      Result := asAccessor
    else
      Result := asIndexedAccessor;
  end;
end;

function GetDynArrayProp(Instance: TObject; PropInfo: PPropInfo): Pointer;
type
  { Need a(ny) dynamic array type to force correct call setup.
    (Address of result passed in EDX) }
  TDynamicArray = array of Byte;
type
  TDynArrayGetProc = function: TDynamicArray of object;
  TDynArrayIndexedGetProc = function (Index: Integer): TDynamicArray of object;
var
  M: TMethod;
begin
  case GetAccessToProperty(Instance, PropInfo, Longint(PropInfo^.GetProc),
    Result, M) of
    asFieldData:
      Result := PPointer(Result)^;
    asAccessor:
      Result := Pointer(TDynArrayGetProc(M)());
    asIndexedAccessor:
      Result := Pointer(TDynArrayIndexedGetProc(M)(PropInfo^.Index));
  end;
end;

procedure SetDynArrayProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: Pointer);
type
  TDynArraySetProc = procedure (const Value: Pointer) of object;
  TDynArrayIndexedSetProc = procedure (Index: Integer;
                                       const Value: Pointer) of object;
var
  P: Pointer;
  M: TMethod;
begin
  case GetAccessToProperty(Instance, PropInfo, Longint(PropInfo^.SetProc),
    P, M) of
    asFieldData:
      asm
        MOV    ECX, PropInfo
        MOV    ECX, [ECX].TPropInfo.PropType
        MOV    ECX, [ECX]

        MOV    EAX, [P]
        MOV    EDX, Value
        CALL   System.@DynArrayAsg
      end;
    asAccessor:
      TDynArraySetProc(M)(Value);
    asIndexedAccessor:
      TDynArrayIndexedSetProc(M)(PropInfo^.Index, Value);
  end;
end;
{$ENDIF}
{$ELSE}
function GetDynArrayProp(Instance: TObject; PropInfo: PPropInfo): Pointer;
type
  { Need a(ny) dynamic array type to force correct call setup.
    (Address of result passed in EDX) }
  TDynamicArray = array of Byte;
type
  TDynArrayGetProc = function: TDynamicArray of object;
  TDynArrayIndexedGetProc = function (Index: Integer): TDynamicArray of object;
var
  AMethod: TMethod;
begin
  case (PropInfo^.PropProcs) and 3 of
    ptfield:
      Result := PPointer(Pointer(Instance) + PtrUInt(PropInfo^.GetProc))^;
    ptstatic,
    ptvirtual:
    begin
      if (PropInfo^.PropProcs and 3) = ptStatic then
        AMethod.Code := PropInfo^.GetProc
      else
        AMethod.Code := PPointer(Pointer(Instance.ClassType) + PtrUInt(PropInfo^.GetProc))^;
      AMethod.Data := Instance;
      if ((PropInfo^.PropProcs shr 6) and 1) <> 0 then
        Result := Pointer(TDynArrayIndexedGetProc(AMethod)(PropInfo^.Index))
      else
        Result := Pointer(TDynArrayGetProc(AMethod)());
    end;
  end;
end;

procedure SetDynArrayProp(Instance: TObject; PropInfo: PPropInfo;
  const Value: Pointer);
type
  TDynArraySetProc = procedure (const Value: Pointer) of object;
  TDynArrayIndexedSetProc = procedure (Index: Integer;
                                       const Value: Pointer) of object;
var
  AMethod: TMethod;
begin
  case (PropInfo^.PropProcs shr 2) and 3 of
    ptfield:
      PPointer(Pointer(Instance) + PtrUInt(PropInfo^.SetProc))^ := Value;
    ptstatic,
    ptvirtual:
    begin
      if ((PropInfo^.PropProcs shr 2) and 3) = ptStatic then
        AMethod.Code := PropInfo^.SetProc
      else
        AMethod.Code := PPointer(Pointer(Instance.ClassType) + PtrUInt(PropInfo^.SetProc))^;
      AMethod.Data := Instance;
      if ((PropInfo^.PropProcs shr 6) and 1) <> 0 then
        TDynArrayIndexedSetProc(AMethod)(PropInfo^.Index, Value)
      else
        TDynArraySetProc(AMethod)(Value);
    end;
  end;
end;
{$ENDIF}

function GetPropValue(Instance: TObject; PropInfo: PPropInfo): Variant;
var
  PropType: PTypeInfo;
  DynArray: Pointer;
begin
  // assume failure
  Result := Null;
  PropType := PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF};
  case PropType^.Kind of
    tkInteger:
      Result := GetOrdProp(Instance, PropInfo);
    tkWChar:
{$IFNDEF NEXTGEN}
      Result := WideString(WideChar(GetOrdProp(Instance, PropInfo)));
{$ELSE}
      Result := string(WideChar(GetOrdProp(Instance, PropInfo)));
{$ENDIF}
    tkEnumeration:
      if GetTypeData(PropType)^.BaseType{$IFNDEF FPC}^{$ENDIF} = TypeInfo(Boolean) then
        Result := Boolean(GetOrdProp(Instance, PropInfo))
      else
        Result := GetOrdProp(Instance, PropInfo);
    tkSet:
      Result := GetOrdProp(Instance, PropInfo);
    tkFloat:
      if ((GetTypeName(PropType) = 'TDateTime') or
          (GetTypeName(PropType) = 'TDate') or
          (GetTypeName(PropType) = 'TTime')) then
        Result := VarAsType(GetFloatProp(Instance, PropInfo), varDate)
      else
        Result := GetFloatProp(Instance, PropInfo);
{$IFNDEF NEXTGEN}
    tkString, {$IFDEF FPC}tkAString, {$ENDIF}tkLString:
      Result := GetStrProp(Instance, PropInfo);
    tkChar:
      Result := Char(GetOrdProp(Instance, PropInfo));
    tkWString:
      Result := GetWideStrProp(Instance, PropInfo);
{$IFDEF SUPPORTS_UNICODE}
    tkUString:
      Result := GetUnicodeStrProp(Instance, PropInfo);
{$ENDIF}
{$ELSE}
    tkUString:
      Result := GetStrProp(Instance, PropInfo);
{$ENDIF}
    tkVariant:
      Result := GetVariantProp(Instance, PropInfo);
    tkInt64:
{$IFDEF DELPHI2009_UP}
    if (GetTypeName(PropType) = 'UInt64') then
      Result := UInt64(GetInt64Prop(Instance, PropInfo))
    else
{$ENDIF}
      Result := GetInt64Prop(Instance, PropInfo);
{$IFDEF FPC}
    tkBool:
      Result := Boolean(GetOrdProp(Instance, PropInfo));
    tkQWord:
      Result := QWord(GetInt64Prop(Instance, PropInfo));
{$ENDIF}
    tkInterface:
      Result := GetInterfaceProp(Instance, PropInfo);
    tkDynArray:
      begin
        DynArray := GetDynArrayProp(Instance, PropInfo);
        DynArrayToVariant(Result, DynArray, PropType);
      end;
    tkClass:
      Result := ObjToVar(GetObjectProp(Instance, PropInfo));
  else
    PropertyConvertError(GetTypeName(PropType));
  end;
end;

function GetPropValue(Instance: TObject; const Name: string): Variant;
begin
  Result := HproseCommon.GetPropValue(Instance, GetPropInfo(Instance, Name));
end;

procedure SetPropValue(Instance: TObject; PropInfo: PPropInfo;
  const Value: Variant);
var
  PropType: PTypeInfo;
  TypeData: PTypeData;
  Obj: TObject;
  DynArray: Pointer;
begin
  PropType := PropInfo^.PropType{$IFNDEF FPC}^{$ENDIF};
  TypeData := GetTypeData(PropType);
  // set the right type
  case PropType^.Kind of
    tkInteger, {$IFNDEF NEXTGEN}tkChar, {$ENDIF}tkWChar, tkEnumeration, tkSet:
      SetOrdProp(Instance, PropInfo, Value);
{$IFDEF FPC}
    tkBool:
      SetOrdProp(Instance, PropInfo, Value);
    tkQWord:
      SetInt64Prop(Instance, PropInfo, QWord(Value));
{$ENDIF}
    tkFloat:
      SetFloatProp(Instance, PropInfo, Value);
{$IFNDEF NEXTGEN}
    tkString, {$IFDEF FPC}tkAString, {$ENDIF}tkLString:
      SetStrProp(Instance, PropInfo, VarToStr(Value));
    tkWString:
      SetWideStrProp(Instance, PropInfo, VarToWideStr(Value));
{$IFDEF SUPPORTS_UNICODE}
    tkUString:
  {$IFDEF FPC}
      SetUnicodeStrProp(Instance, PropInfo, VarToUnicodeStr(Value)); //SB: ??
  {$ELSE}
      SetUnicodeStrProp(Instance, PropInfo, VarToStr(Value)); //SB: ??
  {$ENDIF}
{$ENDIF}
{$ELSE}
    tkUString:
      SetStrProp(Instance, PropInfo, VarToStr(Value)); //SB: ??
{$ENDIF}
{$IFDEF DELPHI2009_UP}
    tkInt64:
      SetInt64Prop(Instance, PropInfo, Value);
{$ELSE}
    tkInt64:
      SetInt64Prop(Instance, PropInfo, TVarData(VarAsType(Value, varInt64)).VInt64);
{$ENDIF}
    tkVariant:
      SetVariantProp(Instance, PropInfo, Value);
    tkInterface:
        SetInterfaceProp(Instance, PropInfo, Value);
    tkDynArray:
      begin
        DynArray := nil; // "nil array"
        if VarIsNull(Value) or (VarArrayHighBound(Value, 1) >= 0) then begin
          DynArrayFromVariant(DynArray, Value, PropType);
        end;
        SetDynArrayProp(Instance, PropInfo, DynArray);
{$IFNDEF FPC}
        DynArrayClear(DynArray, PropType);
{$ENDIF}
      end;
    tkClass:
      if VarIsNull(Value) then
        SetOrdProp(Instance, PropInfo, 0)
      else if VarIsObj(Value) then begin
        Obj := VarToObj(Value);
        if (Obj.ClassType.InheritsFrom(TypeData^.ClassType)) then
          SetObjectProp(Instance, PropInfo, Obj)
        else
          PropertyConvertError(GetTypeName(PropType));
      end
      else
        PropertyConvertError(GetTypeName(PropType));
  else
    PropertyConvertError(GetTypeName(PropType));
  end;
end;

procedure SetPropValue(Instance: TObject; const Name: string;
  const Value: Variant);
begin
  HproseCommon.SetPropValue(Instance, GetPropInfo(Instance, Name), Value);
end;

function GetStoredPropList(Instance: TObject; out PropList: PPropList): Integer;
var
  I, J, Count: Integer;
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
      J := 0;
      for I := 0 to Count - 1 do
        if IsStoredProp(Instance, TempList^[I]) then begin
          PropList^[J] := TempList^[I];
          Inc(J);
        end;
    finally
      FreeMem(TempList);
    end;
end;

function GetProperties(Instance: TObject): IMap;
var
  I, Count: Integer;
  TempList: PPropList;
  PropInfo: PPropInfo;
begin
  Count := GetPropList(PTypeInfo(Instance.ClassInfo), TempList);
  Result := THashMap.create;
  if Count > 0 then
    try
      for I := 0 to Count - 1 do begin
        PropInfo := TempList^[I];
        Result[PropInfo^.Name] := HproseCommon.GetPropValue(Instance, PropInfo);
      end;
    finally
      FreeMem(TempList);
    end;
end;

function GetStoredProperties(Instance: TObject): IMap;
var
  I, Count: Integer;
  TempList: PPropList;
  PropInfo: PPropInfo;
begin
  Count := GetPropList(PTypeInfo(Instance.ClassInfo), TempList);
  Result := THashMap.create;
  if Count > 0 then
    try
      for I := 0 to Count - 1 do begin
        PropInfo := TempList^[I];
        if IsStoredProp(Instance, PropInfo) then begin
          Result[PropInfo^.Name] := HproseCommon.GetPropValue(Instance, PropInfo);
        end;
      end;
    finally
      FreeMem(TempList);
    end;
end;

procedure SetProperties(Instance: TObject; const Properties: IMap);
var
  I, Count: Integer;
begin
  Count := Properties.Count;
  if (Count > 0) then
    for I := 0 to Count - 1 do
      HproseCommon.SetPropValue(Instance, VarToStr(Properties.Keys[I]), Properties.Values[I]);
end;

const
  htNull    = $00000000;
  htBoolean = $10000000;
  htInteger = $20000000;
  htInt64   = $30000000;
  htDouble  = $40000000;
  htOleStr  = $50000000;
  htDate    = $60000000;
  htObject  = $70000000;
  htArray   = $80000000;

function GetHashType(VType: Word): Integer;
begin
  case VType of
    varEmpty:    Result := htNull;
    varNull:     Result := htNull;
    varBoolean:  Result := htBoolean;
    varByte:     Result := htInteger;
    varWord:     Result := htInteger;
    varShortInt: Result := htInteger;
    varSmallint: Result := htInteger;
    varInteger:  Result := htInteger;
    varLongWord: Result := htInt64;
    varInt64:    Result := htInt64;
{$IFDEF DELPHI2009_UP}
    varUInt64:   Result := htInt64;
{$ENDIF}
{$IFDEF FPC}
    varQWord:    Result := htInt64;
{$ENDIF}
    varSingle:   Result := htDouble;
    varDouble:   Result := htDouble;
    varCurrency: Result := htDouble;
    varOleStr:   Result := htOleStr;
    varString:   Result := htOleStr;
{$IFDEF SUPPORTS_UNICODE}
    varUString:  Result := htOleStr;
{$ENDIF}
    varDate:     Result := htDate;
    varUnknown:  Result := htObject;
    varVariant:  Result := htObject;
  else
    if VType = varObject then
      Result := htObject
    else
      Result := htNull;
  end;
end;

function VarEquals(const Left, Right: Variant): Boolean;
var
  L, R: PVarData;
  LA, RA: PVarArray;
begin
  Result := False;
  L := FindVarData(Left);
  R := FindVarData(Right);
  if VarIsArray(Left) and VarIsArray(Right) then begin
    if (L^.VType and varByRef) <> 0 then
      LA := PVarArray(L^.VPointer^)
    else
      LA := L^.VArray;
    if (R^.VType and varByRef) <> 0 then
      RA := PVarArray(R^.VPointer^)
    else
      RA := R^.VArray;
    if LA = RA then Result := True;
  end
  else begin
    if (L^.VType = varUnknown) and
       (R^.VType = varUnknown) then
      Result := L^.VUnknown = R^.VUnknown
    else if (L^.VType = varUnknown or varByRef) and
            (R^.VType = varUnknown) then
      Result := Pointer(L^.VPointer^) = R^.VUnknown
    else if (L^.VType = varUnknown) and
            (R^.VType = varUnknown or varByRef) then
      Result := L^.VUnknown = Pointer(R^.VPointer^)
    else if (L^.VType = varUnknown or varByRef) and
            (R^.VType = varUnknown or varByRef) then
      Result := Pointer(L^.VPointer^) = Pointer(R^.VPointer^)
    else if GetHashType(L^.VType and varTypeMask) = GetHashType(R^.VType and varTypeMask) then
      try
        Result := Left = Right;
      except
        Result := False;
      end
    else
      Result := False;
  end;
end;

function VarRef(const Value: Variant): Variant;
var
  VType: TVarType;
begin
  if VarIsByRef(Value) then
    Result := Value
  else if VarIsArray(Value, False) then
    Result := VarArrayRef(Value)
  else begin
    VarClear(Result);
    VType := VarType(Value);
    if VType in [varSmallint, varInteger, varSingle, varDouble,
                 varCurrency, varDate, varOleStr, varDispatch,
                 varError, varBoolean, varUnknown, varShortInt,
                 varByte ,varWord, varLongWord, varInt64
                 {$IFDEF DELPHI2009_UP}, varUInt64{$ENDIF}
                 {$IFDEF FPC}, varQWord{$ENDIF}] then begin
      TVarData(Result).VType := VType or varByRef;
      TVarData(Result).VPointer := @TVarData(Value).VPointer;
    end
{$IFDEF DELPHI6}
    else if VType <> varVariant then begin
      TVarData(Result).VType := VType or varByRef;
      TVarData(Result).VPointer := @TVarData(Value).VPointer;
    end
{$ENDIF}
    else begin
      TVarData(Result).VType := varByRef or varVariant;
      TVarData(Result).VPointer := @TVarData(Value);
    end;
  end;
end;

function VarDataDeref(const Value: TVarData): TVarData;
begin
  if (Value.VType and varByRef) = 0 then
    Result := Value
  else begin
    Result.VType := Value.VType and (not varByRef);
    case Result.VType of
      varVariant: Result := PVarData(Value.VPointer)^;
      varSmallint: Result.VSmallInt := PSmallInt(Value.VPointer)^;
      varInteger: Result.VInteger := PInteger(Value.VPointer)^;
      varSingle: Result.VSingle := PSingle(Value.VPointer)^;
      varDouble: Result.VDouble := PDouble(Value.VPointer)^;
      varCurrency: Result.VCurrency := PCurrency(Value.VPointer)^;
      varDate: Result.VDate := PDateTime(Value.VPointer)^;
      varOleStr: Result.VOleStr := PPWideChar(Value.VPointer)^;
      varError: Result.VError := PError(Value.VPointer)^;
      varBoolean: Result.VBoolean := PWordBool(Value.VPointer)^;
      varShortInt: Result.VShortInt := PShortInt(Value.VPointer)^;
      varByte: Result.VByte := PByte(Value.VPointer)^;
      varWord: Result.VWord := PWord(Value.VPointer)^;
      varLongWord: Result.VLongWord := PCardinal(Value.VPointer)^;
      varInt64: Result.VInt64 := PInt64(Value.VPointer)^;
{$IFDEF DELPHI2009_UP}
      varUInt64: Result.VUInt64 := PUint64(Value.VPointer)^;
{$ENDIF}
{$IFDEF FPC}
      varQWord: Result.VQWord := PQWord(Value.VPointer)^;
{$ENDIF}
      else
        Result.VPointer := PPointer(Value.VPointer)^;
    end;
  end;
end;

function VarDeref(const Value: Variant): Variant;
begin
  if not VarIsByRef(Value) then
    Result := Value
  else begin
    TVarData(Result) := VarDataDeref(TVarData(Value));
  end;
end;

{$IFDEF NEXTGEN}
function HashOfString(const Value: string): Integer;
{$ELSE}
function HashOfString(const Value: WideString): Integer;
{$ENDIF}
var
  I, N: Integer;
begin
  N := Length(Value);
  Result := 0;
  for I := 1 to N do
    Result := ((Result shl 2) or (Result shr 30)) xor Ord(Value[I]);
  Result := htOleStr or (Result and $0FFFFFFF);
end;

function HashOfDouble(const Value: Double): Integer;
var
  I: Int64 absolute Value;
begin
  Result := htDouble or ((I xor (I shr 32)) and $0FFFFFFF);
end;

function HashOfVariant(const Value: Variant): Integer;
var
  P: PVarData;
begin
  P := FindVarData(Value);
  case P^.VType of
    varEmpty:    Result := 0;
    varNull:     Result := 1;
    varBoolean:  Result := htBoolean or Abs(Integer(P^.VBoolean));
    varByte:     Result := htInteger or P^.VByte;
    varWord:     Result := htInteger or P^.VWord;
    varShortInt: Result := htInteger or (P^.VShortInt and $FF);
    varSmallint: Result := htInteger or (P^.VSmallInt and $FFFF);
    varInteger:  Result := htInteger or (P^.VInteger and $0FFFFFFF);
    varLongWord: Result := htInt64 or (P^.VLongWord and $0FFFFFFF)
                           xor (not (P^.VLongWord shr 3) and $10000000);
    varInt64:    Result := htInt64 or (P^.VInt64 and $0FFFFFFF)
                           xor (not (P^.VInt64 shr 3) and $10000000);
{$IFDEF DELPHI2009_UP}
    varUInt64:   Result := htInt64 or (P^.VUInt64 and $0FFFFFFF)
                           xor (not (P^.VUInt64 shr 3) and $10000000);
{$ENDIF}
{$IFDEF FPC}
     varQWord:   Result := htInt64 or (P^.VQWord and $0FFFFFFF)
                       xor (not (P^.VQWord shr 3) and $10000000);
{$ENDIF}
    varSingle:   Result := HashOfDouble(P^.VSingle);
    varDouble:   Result := HashOfDouble(P^.VDouble);
    varCurrency: Result := HashOfDouble(P^.VCurrency);
    varDate:     Result := htDate or ((P^.VInteger xor (P^.VInt64 shr 32))
                           and $0FFFFFFF);
    varUnknown:  Result := htObject or (P^.VInteger and $0FFFFFFF);
    varVariant:  Result := htObject or (P^.VInteger and $0FFFFFFF);
  else
    if  P^.VType and varByRef <> 0 then
      case P^.VType and not varByRef of
        varBoolean:  Result := htBoolean
                               or Abs(Integer(PWordBool(P^.VPointer)^));
        varByte:     Result := htInteger or PByte(P^.VPointer)^;
        varWord:     Result := htInteger or PWord(P^.VPointer)^;
        varShortInt: Result := htInteger or (PShortInt(P^.VPointer)^ and $FF);
        varSmallInt: Result := htInteger or (PSmallInt(P^.VPointer)^ and $FFFF);
        varInteger:  Result := htInteger or (PInteger(P^.VPointer)^
                               and $0FFFFFFF);
        varLongWord: Result := htInt64 or (PCardinal(P^.VPointer)^ and $0FFFFFFF)
                               xor (not (PCardinal(P^.VPointer)^ shr 3)
                               and $10000000);
        varInt64:    Result := htInt64 or (PInt64(P^.VPointer)^ and $0FFFFFFF)
                               xor (not (PInt64(P^.VPointer)^ shr 3)
                               and $10000000);
{$IFDEF DELPHI2009_UP}
        varUInt64:   Result := htInt64 or (PUInt64(P^.VPointer)^ and $0FFFFFFF)
                               xor (not (PUInt64(P^.VPointer)^ shr 3)
                               and $10000000);
{$ENDIF}
{$IFDEF FPC}
         varQWord:   Result := htInt64 or (PQWord(P^.VPointer)^ and $0FFFFFFF)
                           xor (not (PQWord(P^.VPointer)^ shr 3) and $10000000);
{$ENDIF}
        varSingle:   Result := htDouble or (PInteger(P^.VPointer)^
                               and $0FFFFFFF);
        varDouble:   Result := htDouble or ((PInteger(P^.VPointer)^
                               xor (PInt64(P^.VPointer)^ shr 32)) and $0FFFFFFF);
        varCurrency: Result := htDouble or ((PInteger(P^.VPointer)^
                               xor (PInt64(P^.VPointer)^ shr 32)) and $0FFFFFFF);
        varDate:     Result := htDate or ((PInteger(P^.VPointer)^
                               xor (PInt64(P^.VPointer)^ shr 32)) and $0FFFFFFF);
        varUnknown:  Result := htObject or (PInteger(P^.VPointer)^
                               and $0FFFFFFF);
      else
        if VarIsArray(Value) then
          Result := Integer(htArray) or GetHashType(P^.VType and varTypeMask)
                    or (PInteger(P^.VPointer)^ and $0FFFFFFF)
        else
          Result := 0;
      end
    else if VarIsArray(Value) then
      Result := Integer(htArray) or GetHashType(P^.VType and varTypeMask)
                or (P^.VInteger and $0FFFFFFF)
    else if P^.VType = varObject then
      Result := htObject or (P^.VInteger and $0FFFFFFF)
    else
      Result := (P^.VInteger xor (P^.VInt64 shr 32)) and $0FFFFFFF;
  end;
end;

// Copies a TVarRec and its contents. If the content is referenced
// the value will be copied to a new location and the reference
// updated.
function CopyVarRec(const Item: TVarRec): TVarRec;
var
  S: {$IFNDEF NEXTGEN}WideString{$ELSE}string{$ENDIF};
begin
  // Copy entire TVarRec first
  Result := Item;

  // Now handle special cases
  case Item.VType of
    vtExtended:
      begin
        New(Result.VExtended);
        Result.VExtended^ := Item.VExtended^;
      end;
{$IFNDEF NEXTGEN}
    vtString:
      begin
        New(Result.VString);
        Result.VString^ := Item.VString^;
      end;
    vtPChar:
      Result.VPChar := {$IFDEF DELPHIXE4_UP}AnsiStrings.{$ENDIF}StrNew(Item.VPChar);
    // a little trickier: casting to AnsiString will ensure
    // reference counting is done properly
    vtAnsiString:
      begin
        // nil out first, so no attempt to decrement
        // reference count
        Result.VAnsiString := nil;
        AnsiString(Result.VAnsiString) := AnsiString(Item.VAnsiString);
      end;
{$ENDIF}
    // there is no StrNew for PWideChar
    vtPWideChar:
      begin
        S := Item.VPWideChar;
        GetMem(Result.VPWideChar,
               (Length(S) + 1) * SizeOf(WideChar));
        Move(PWideChar(S)^, Result.VPWideChar^,
             (Length(S) + 1) * SizeOf(WideChar));
      end;
    vtCurrency:
      begin
        New(Result.VCurrency);
        Result.VCurrency^ := Item.VCurrency^;
      end;
    vtVariant:
      begin
        New(Result.VVariant);
        Result.VVariant^ := Item.VVariant^;
      end;
    // casting ensures proper reference counting
    vtInterface:
      begin
        Result.VInterface := nil;
        IInterface(Result.VInterface) := IInterface(Item.VInterface);
      end;
    // casting ensures a proper copy is created
    vtWideString:
      begin
        Result.VWideString := nil;
{$IFNDEF NEXTGEN}
        WideString(Result.VWideString) := WideString(Item.VWideString);
{$ELSE}
        string(Result.VWideString) := string(Item.VWideString);
{$ENDIF}
      end;
    vtInt64:
      begin
        New(Result.VInt64);
        Result.VInt64^ := Item.VInt64^;
      end;
{$IFDEF DELPHI2009_UP}
    vtUnicodeString:
      begin
        // nil out first, so no attempt to decrement
        // reference count
        Result.VUnicodeString := nil;
        UnicodeString(Result.VUnicodeString) := UnicodeString(Item.VUnicodeString);
      end;
{$ENDIF}
{$IFDEF FPC}
    vtQWord:
      begin
        New(Result.VQWord);
        Result.VQWord^ := Item.VQWord^;
      end;
{$ENDIF}
    // VPointer and VObject don't have proper copy semantics so it
    // is impossible to write generic code that copies the contents
  end;
end;

// Creates a TConstArray out of the values given. Uses CopyVarRec
// to make copies of the original elements.
function CreateConstArray(const Elements: array of const): TConstArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Elements));
  for I := Low(Elements) to High(Elements) do
    Result[I] := CopyVarRec(Elements[I]);
end;


// TVarRecs created by CopyVarRec must be finalized with this function.
// You should not use it on other TVarRecs.
// use this function on copied TVarRecs only!
procedure FinalizeVarRec(var Item: TVarRec);
begin
  case Item.VType of
    vtExtended: Dispose(Item.VExtended);
{$IFNDEF NEXTGEN}
    vtString: Dispose(Item.VString);
    vtPChar: {$IFDEF DELPHIXE4_UP}AnsiStrings.{$ENDIF}StrDispose(Item.VPChar);
    vtAnsiString: AnsiString(Item.VAnsiString) := '';
{$ENDIF}
    vtPWideChar: FreeMem(Item.VPWideChar);
    vtCurrency: Dispose(Item.VCurrency);
    vtVariant: Dispose(Item.VVariant);
    vtInterface: IInterface(Item.VInterface) := nil;
{$IFNDEF NEXTGEN}
    vtWideString: WideString(Item.VWideString) := '';
{$ELSE}
    vtWideString: string(Item.VWideString) := '';
{$ENDIF}
{$IFDEF SUPPORTS_UNICODE}
    vtUnicodeString: UnicodeString(Item.VUnicodeString) := '';
{$ENDIF}
    vtInt64: Dispose(Item.VInt64);
{$IFDEF FPC}
    vtQWord: Dispose(Item.VQWord);
{$ENDIF}
  end;
  Item.VPointer := nil;
end;

// A TConstArray contains TVarRecs that must be finalized. This function
// does that for all items in the array.
procedure FinalizeConstArray(var Arr: TConstArray);
var
  I: Integer;
begin
  for I := Low(Arr) to High(Arr) do
    FinalizeVarRec(Arr[I]);
  Finalize(Arr);
  Arr := nil;
end;

function VarRecToVar(const V: TVarRec): Variant;
begin
  case V.VType of
    vtInteger:       Result := V.VInteger;
    vtBoolean:       Result := V.VBoolean;
    vtExtended:      Result := V.VExtended^;
{$IFNDEF NEXTGEN}
    vtChar:          Result := WideString(V.VChar);
    vtString:        Result := AnsiString(V.VString^);
    vtPChar:         Result := AnsiString(V.VPChar);
    vtAnsiString:    Result := AnsiString(V.VAnsiString);
    vtPWideChar:     Result := WideString(V.VPWideChar);
    vtWideString:    Result := WideString(V.VWideString);
    vtWideChar:      Result := WideString(V.VWideChar);
{$ELSE}
    vtPWideChar:     Result := string(V.VPWideChar);
    vtWideString:    Result := string(V.VWideString);
    vtWideChar:      Result := string(V.VWideChar);
{$ENDIF}
    vtCurrency:      Result := V.VCurrency^;
    vtVariant:       Result := V.VVariant^;
    vtInt64:         Result := V.VInt64^;
{$IFDEF FPC}
    vtQWord:         Result := V.VQWord^;
{$ENDIF}
{$IFDEF SUPPORTS_UNICODE}
    vtUnicodeString: Result := UnicodeString(V.VUnicodeString);
{$ENDIF}
    vtPointer:       Result := NativeInt(V.VPointer);
    vtClass:         Result := NativeInt(V.VClass);
    vtObject:
      if not Assigned(V.VObject) then
        Result := Null
      else
        Result := ObjToVar(V.VObject);
    vtInterface:
      if not Assigned(IInterface(V.VInterface)) then
        Result := Null
      else
        Result := IInterface(V.VInterface);
  end;
end;

function ToVariants(const Elements: array of const): TVariants;
var
  I: Integer;
begin
  SetLength(Result, Length(Elements));
  for I := Low(Elements) to High(Elements) do
    Result[I] := VarRecToVar(Elements[I]);
end;

type

  TListEnumerator = class(TInterfacedObject, IListEnumerator)
  private
    FList: TAbstractList;
    FIndex: Integer;
    function GetCurrent: Variant;
  public
    constructor Create(AList: TAbstractList);
    function MoveNext: Boolean;
    procedure Reset;
    property Current: Variant read GetCurrent;
  end;

{ TListEnumerator }

constructor TListEnumerator.Create(AList: TAbstractList);
begin
  FList := AList;
  FIndex := -1;
end;

function TListEnumerator.GetCurrent: Variant;
begin
  Result := FList[FIndex];
end;

function TListEnumerator.MoveNext: Boolean;
begin
  if FIndex < FList.Count - 1 then begin
    Inc(FIndex);
    Result := True;
  end
  else
    Result := False;
end;

procedure TListEnumerator.Reset;
begin
  FIndex := -1;
end;

{ TAbstractList }

constructor TAbstractList.Create(Sync: Boolean; ReadWriteSync: Boolean);
begin
  Create(4, Sync, ReadWriteSync);
end;

constructor TAbstractList.Create(const AList: IImmutableList; Sync: Boolean;
  ReadWriteSync: Boolean);
begin
  Create(AList.Count, Sync, ReadWriteSync);
  AddAll(AList);
end;

constructor TAbstractList.Create(const Container: Variant; Sync: Boolean;
  ReadWriteSync: Boolean);
var
  AList: IList;
  N: Integer;
begin
  if VarIsList(Container) then begin
    AList := VarToList(Container);
    Create(AList, Sync, ReadWriteSync);
  end
  else if VarIsArray(Container) then begin
    N := VarArrayHighBound(Container, 1) - VarArrayLowBound(Container, 1) + 1;
    Create(N, Sync, ReadWriteSync);
    AddAll(Container);
  end;
end;

constructor TAbstractList.Create(const ConstArray: array of const;
  Sync: Boolean; ReadWriteSync: Boolean);
begin
  Create(Length(ConstArray), Sync, ReadWriteSync);
  AddAll(ConstArray);
end;

{$IFDEF BCB}
constructor TAbstractList.Create0;
begin
  Create;
end;
constructor TAbstractList.Create1(ACapacity: Integer);
begin
  Create(ACapacity);
end;
constructor TAbstractList.Create2(ACapacity: Integer; Sync: Boolean);
begin
  Create(ACapacity, Sync);
end;

constructor TAbstractList.CreateS(Sync: Boolean);
begin
  Create(Sync);
end;

constructor TAbstractList.CreateL1(const AList: IImmutableList);
begin
  Create(AList);
end;

constructor TAbstractList.CreateA1(const Container: Variant);
begin
  Create(Container);
end;

constructor TAbstractList.CreateCA1(const ConstArray: array of const);
begin
  Create(ConstArray);
end;

constructor TAbstractList.CreateL2(const AList: IImmutableList; Sync: Boolean);
begin
  Create(AList, Sync);
end;

constructor TAbstractList.CreateA2(const Container: Variant; Sync: Boolean);
begin
  Create(Container, Sync);
end;

constructor TAbstractList.CreateCA2(const ConstArray: array of const; Sync: Boolean);
begin
  Create(ConstArray, Sync);
end;

{$ENDIF}

destructor TAbstractList.Destroy;
begin
  Clear;
  FreeAndNil(FLock);
  FreeAndNil(FReadWriteLock);
  inherited Destroy;
end;

function TAbstractList.IndexOf(const Value: Variant): Integer;
begin
  Result := IndexOf(Value, 0, Count);
end;

function TAbstractList.IndexOf(const Value: Variant;
  StartIndex: Integer): Integer;
begin
  Result := IndexOf(Value, StartIndex, Count - StartIndex);
end;

procedure TAbstractList.InitLock;
begin
  if not Assigned(FLock) then
    FLock := TCriticalSection.Create;
end;

procedure TAbstractList.InitReadWriteLock;
begin
  if not Assigned(FReadWriteLock) then
    FReadWriteLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

function TAbstractList.Invoke(const Name: string;
  const Arguments: TVarDataArray): Variant;
var
  LList: IList;
  Args: TVariants;
  AName: string;
begin
  Result := Unassigned;
  Args := TVariants(Arguments);
  AName := UpperCase(Name);
  if AName = 'ADD' then
    Result := Add(Args[0])
  else if AName = 'ADDALL' then
    AddAll(Args[0])
  else if AName = 'ASSIGN' then begin
    Result := VarToIntf(Args[0], IList, LList);
    if Result then Assign(LList);
  end
  else if AName = 'CLEAR' then
    Clear
  else if AName = 'CONTAINS' then
    Result := Contains(Args[0])
  else if AName = 'DELETE' then
    Result := Delete(Args[0])
  else if AName = 'DELETERANGE' then
    DeleteRange(Args[0], Args[1])
  else if AName = 'EXCHANGE' then
    Exchange(Args[0], Args[1])
  else if AName = 'GET' then
    Result := Get(Args[0])
  else if AName = 'PUT' then
    Put(Args[0], Args[1])
  else if AName = 'GETENUMERATOR' then
    Result := GetEnumerator()
  else if AName = 'INDEXOF' then
    Result := IndexOf(Args[0])
  else if AName = 'LASTINDEXOF' then
    Result := LastIndexOf(Args[0])
  else if AName = 'INSERT' then
    Insert(Args[0], Args[1])
  else if AName = 'INSERTRANGE' then
    InsertRange(Args[0], Args[1])
  else if AName = 'JOIN' then
    case Length(Args) of
      0: Result := Join();
      1: Result := Join(Args[0]);
      2: Result := Join(Args[0], Args[1]);
      3: Result := Join(Args[0], Args[1], Args[2]);
    end
  else if AName = 'INITLOCK' then
    InitLock
  else if AName = 'INITREADWRITELOCK' then
    InitReadWriteLock
  else if AName = 'LOCK' then
    Lock
  else if AName = 'UNLOCK' then
    Unlock
  else if AName = 'BEGINREAD' then
    BeginRead
  else if AName = 'ENDREAD' then
    EndRead
  else if AName = 'BEGINWRITE' then
    Result := BeginWrite()
  else if AName = 'ENDWRITE' then
    EndWrite
  else if AName = 'MOVE' then
    Move(Args[0], Args[1])
  else if AName = 'REMOVE' then
    case Length(Args) of
      1: Result := Remove(Args[0]);
      2: Result := Remove(Args[0], TDirection(Args[1]));
    end
  else if AName = 'TOARRAY' then
    case Length(Args) of
      0: Result := ToArray();
      1: Result := ToArray(TVarType(Args[0]));
    end
  else if AName = 'FIRST' then
    Result := First()
  else if AName = 'LAST' then
    Result := Last()
  else if AName = 'PACK' then
    Pack
  else if AName = 'REVERSE' then
    Reverse
  else if AName = 'SORT' then
    Sort
  else if AName = 'SHUFFLE' then
    Shuffle
  else if AName = 'TRIMEXCESS' then
    TrimExcess
  else
    raise Exception.Create('Variant method "' + Name + '" has not found');
end;

procedure TAbstractList.Lock;
begin
  FLock.Acquire;
end;

procedure TAbstractList.Unlock;
begin
  FLock.Release;
end;

procedure TAbstractList.BeginRead;
begin
  FReadWriteLock.BeginRead;
end;

function TAbstractList.BeginWrite: Boolean;
begin
  Result := FReadWriteLock.BeginWrite;
end;

procedure TAbstractList.EndRead;
begin
  FReadWriteLock.EndRead;
end;

procedure TAbstractList.EndWrite;
begin
  FReadWriteLock.EndWrite;
end;

procedure TAbstractList.AddAll(const AList: IImmutableList);
var
  I: Integer;
begin
  Grow(AList.Count);
  for I := 0 to AList.Count - 1 do Add(AList[I]);
end;

procedure TAbstractList.AddAll(const Container: Variant);
var
  I, N, Low, High: Integer;
begin
  if VarIsList(Container) then begin
    AddAll(VarToList(Container));
  end
  else if VarIsArray(Container) then begin
    Low := VarArrayLowBound(Container, 1);
    High := VarArrayHighBound(Container, 1);
    N := High - Low + 1;
    Grow(N);
    for I := Low to High do Add(Container[I]);
  end;
end;

procedure TAbstractList.AddAll(const ConstArray: array of const);
var
  I, N: Integer;
begin
  N := Length(ConstArray);
  Grow(N);
  for I := 0 to N - 1 do Add(VarRecToVar(ConstArray[I]));
end;

procedure TAbstractList.Assign(const Source: IImmutableList);
var
  I: Integer;
begin
  Clear;
  Capacity := Source.Capacity;
  for I := 0 to Source.Count - 1 do Add(Source[I]);
end;

function TAbstractList.Contains(const Value: Variant): Boolean;
begin
  Result := IndexOf(Value) > -1;
end;

function TAbstractList.GetEnumerator: IListEnumerator;
begin
  Result := TListEnumerator.Create(Self);
end;

function TAbstractList.Join(const Glue: string; const LeftPad: string;
  const RightPad: string): string;
var
  Buffer: TStringBuffer;
  E: IListEnumerator;
begin
  if Count = 0 then begin
    Result := LeftPad + RightPad;
    Exit;
  end;
  E := GetEnumerator;
  Buffer := TStringBuffer.Create(LeftPad);
  E.MoveNext;
  while True do begin
    Buffer.WriteString(VarToStr(E.Current));
    if not E.MoveNext then Break;
    Buffer.WriteString(Glue);
  end;
  Buffer.WriteString(RightPad);
  Result := Buffer.ToString;
  Buffer.Free;
end;

class function TAbstractList.Split(Str: string; const Separator: string;
  Limit: Integer; TrimItem: Boolean; SkipEmptyItem: Boolean; Sync: Boolean;
      ReadWriteSync: Boolean): IList;
var
  I, N, L: Integer;
  S: string;
begin
  if Str = '' then begin
    Result := nil;
    Exit;
  end;
  Result := Self.Create(Sync, ReadWriteSync);
  L := Length(Separator);
  N := 0;
  I := L;
  while (I > 0) and ((Limit = 0) or (N < Limit - 1)) do begin
    I := AnsiPos(Separator, Str);
    if I > 0 then begin
      S := Copy(Str, 1, I - 1);
      if TrimItem then S := Trim(S);
      if not SkipEmptyItem or (S <> '') then Result.Add(S);
      Str := Copy(Str, I + L, MaxInt);
      Inc(N);
    end
  end;
  if TrimItem then Str := Trim(Str);
  if not SkipEmptyItem or (Str <> '') then Result.Add(Str);
end;


function TAbstractList.Remove(const Value: Variant): Integer;
begin
  Result := IndexOf(Value);
  if Result >= 0 then Delete(Result);
end;

function TAbstractList.Remove(const Value: Variant; Direction: TDirection
  ): Integer;
begin
  if Direction = FromBeginning then
    Result := IndexOf(Value)
  else
    Result := LastIndexOf(Value);
  if Result >= 0 then Delete(Result);
end;

function TAbstractList.First: Variant;
begin
  Result := Get(0);
end;

function TAbstractList.Last: Variant;
begin
  Result := Get(Count - 1);
end;

function TAbstractList.LastIndexOf(const Value: Variant): Integer;
begin
  Result := LastIndexOf(Value, Count - 1, Count);
end;

function TAbstractList.LastIndexOf(const Value: Variant;
  StartIndex: Integer): Integer;
begin
  Result := LastIndexOf(Value, StartIndex, StartIndex + 1);
end;

procedure TAbstractList.Pack;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do if VarIsClear(Get(I)) then Delete(I);
end;

procedure TAbstractList.Reverse;
var
  I, J: Integer;
begin
  if Count < 2 then Exit;
  J := Count - 1;
  for I := 0 to J shr 1 do Exchange(I, J - I);
end;

function TAbstractList.Compare(const Value1, Value2: Variant): Integer;
begin
  case VarCompareValue(Value1, Value2) of
    vrLessThan: Result := -1;
    vrGreaterThan: Result := 1;
  else
    Result := 0
  end;
end;

procedure TAbstractList.Sort;
begin
  Sort({$IFDEF FPC}@{$ENDIF}Compare);
end;

procedure TAbstractList.Sort(CompareProc: TListCompareMethod);
  function Partition(L, H: Integer): Integer;
  var
    Elem: Variant;
  begin
    Elem := Get(L);
    while L < H do begin
      while (L < H) and (CompareProc(Elem, Get(H)) <= 0) do Dec(H);
      Exchange(L, H);
      while (L < H) and (CompareProc(Elem, Get(L)) >= 0) do Inc(L);
      Exchange(L, H);
    end;
    Result := L;
  end;
  procedure QuickSort(Low, High: Integer);
  var
    P: Integer;
  begin
    if High - Low > 8 then begin
      P := Partition(Low, High);
      QuickSort(Low, P - 1);
      QuickSort(P + 1, High);
    end;
  end;
var
  I, J, N: Integer;
  Elem: Variant;
begin
  N := Count - 1;
  QuickSort(0, N);
  for I := 1 to N do begin
    Elem := Get(I);
    J := I - 1;
    while (J >= 0) and (CompareProc(Elem, Get(J)) < 0) do begin
      Put(J + 1, Get(J));
      Dec(J);
    end;
    Put(J + 1, Elem);
  end;
end;

procedure TAbstractList.Shuffle;
var
  I: Integer;
{$J+}
const
  RandomInited: Boolean = False;
{$J-}
begin
  if Count > 1 then begin
    if not RandomInited then begin
      Randomize;
      RandomInited := True;
    end;
    for I := Count - 1 downto 0 do Exchange(I, Random(I + 1));
  end;
end;

procedure TAbstractList.TrimExcess;
begin
  SetCapacity(Count);
end;

{ TArrayList }

function TArrayList.Add(const Value: Variant): Integer;
begin
  Result := FCount;
  if FCount = FCapacity then Grow;
  FList[Result] := Value;
  Inc(FCount);
end;

procedure TArrayList.Clear;
begin
  SetLength(FList, 0);
  FCount := 0;
  FCapacity := 0;
end;

constructor TArrayList.Create(ACapacity: Integer; Sync: Boolean;
  ReadWriteSync: Boolean);
begin
  if Sync then InitLock;
  if ReadWriteSync then InitReadWriteLock;
  FCapacity := ACapacity;
  FCount := 0;
  SetLength(FList, FCapacity);
end;

procedure TArrayList.ShiftLeft(Index, N: Integer);
var
  I: Integer;
begin
  Dec(FCount, N);
  for I := Index to Index + N - 1 do VarClear(FList[I]);
  if Index < FCount then begin
    System.Move(FList[Index + N], FList[Index],
      (FCount - Index) * SizeOf(Variant));
    FillChar(FList[FCount], N * SizeOf(Variant), 0);
  end;
end;

function TArrayList.Delete(Index: Integer): Variant;
begin
  if (Index >= 0) and (Index < FCount) then begin
    Result := FList[Index];
    ShiftLeft(Index, 1);
  end;
end;

procedure TArrayList.DeleteRange(Index, ACount: Integer);
begin
  if (Index >= 0) and (Index < FCount) then begin
    if ACount > FCount - Index then ACount := Fcount - Index;
    ShiftLeft(Index, ACount);
  end;
end;

procedure TArrayList.Exchange(Index1, Index2: Integer);
var
  Elem: Variant;
begin
  if Index1 = Index2 then exit;
  if (Index1 < 0) or (Index1 >= FCount) then
    raise EArrayListError.CreateResFmt(@SListIndexError, [Index1]);
  if (Index2 < 0) or (Index2 >= FCount) then
    raise EArrayListError.CreateResFmt(@SListIndexError, [Index2]);

  Elem := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Elem;
end;

function TArrayList.Get(Index: Integer): Variant;
begin
  if (Index >= 0) and (Index < FCount) then
    Result := FList[Index]
  else
    Result := Unassigned;
end;

function TArrayList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TArrayList.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TArrayList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TArrayList.Grow(N: Integer);
var
  TotalCount: Integer;
begin
  TotalCount := FCount + N;
  if TotalCount > FCapacity then begin
    FCapacity := TotalCount;
    Grow;
  end;
end;

procedure TArrayList.ShiftRight(Index, N: Integer);
begin
  if Index < FCount then begin
    System.Move(FList[Index], FList[Index + N],
      (FCount - Index) * SizeOf(Variant));
    FillChar(FList[Index], N * SizeOf(Variant), 0);
  end;
end;

function TArrayList.IndexCompare(Index: Integer;
  const Value: Variant): Boolean;
begin
  Result := VarEquals(FList[Index], Value);
end;

function TArrayList.IndexOf(const Value: Variant; StartIndex,
  ACount: Integer): Integer;
var
  I, EndIndex: Integer;
begin
  if StartIndex < 0 then StartIndex := 0;
  EndIndex := StartIndex + ACount - 1;
  if EndIndex >= FCount then EndIndex := FCount - 1;
  for I := StartIndex to EndIndex do
    if IndexCompare(I, Value) then begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TArrayList.Insert(Index: Integer; const Value: Variant);
begin
  if (Index < 0) or (Index > FCount) then
    raise EArrayListError.CreateResFmt(@SListIndexError, [Index]);
  if FCount = FCapacity then Grow;
  ShiftRight(Index, 1);
  FList[Index] := Value;
  Inc(FCount);
end;

procedure TArrayList.InsertRange(Index: Integer; const AList: IImmutableList);
var
  I, N: Integer;
begin
  if (Index < 0) or (Index > FCount) then
    raise EArrayListError.CreateResFmt(@SListIndexError, [Index]);
  N := AList.Count;
  Grow(N);
  ShiftRight(Index, N);
  for I := 0 to N - 1 do FList[Index + I] := AList[I];
  Inc(FCount, N);
end;

procedure TArrayList.InsertRange(Index: Integer; const Container: Variant);
var
  I, N, Low, High: Integer;
begin
  if VarIsList(Container) then begin
    InsertRange(Index, VarToList(Container));
  end
  else if VarIsArray(Container) then begin
    if (Index < 0) or (Index > FCount) then
      raise EArrayListError.CreateResFmt(@SListIndexError, [Index]);
    N := Length(Container);
    Grow(N);
    ShiftRight(Index, N);
    Low := VarArrayLowBound(Container, 1);
    High := Low + N - 1;
    for I := Low to High do FList[Index + I] := Container[I];
  end;
end;

procedure TArrayList.InsertRange(Index: Integer; const ConstArray: array of const);
var
  I, N: Integer;
begin
  if (Index < 0) or (Index > FCount) then
    raise EArrayListError.CreateResFmt(@SListIndexError, [Index]);
  N := Length(ConstArray);
  Grow(N);
  ShiftRight(Index, N);
  for I := 0 to N - 1 do FList[Index + I] := VarRecToVar(ConstArray[I]);
  Inc(FCount, N);
end;

function TArrayList.LastIndexOf(const Value: Variant; StartIndex,
  ACount: Integer): Integer;
var
  I, EndIndex: Integer;
begin
  if StartIndex >= FCount then StartIndex := FCount - 1;
  EndIndex := StartIndex - ACount + 1;
  if EndIndex < 0 then EndIndex := 0;
  for I := StartIndex downto EndIndex do
    if IndexCompare(I, Value) then begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

procedure TArrayList.Move(CurIndex, NewIndex: Integer);
var
  Elem: Variant;
begin
  if CurIndex <> NewIndex then begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      raise EArrayListError.CreateResFmt(@SListIndexError, [NewIndex]);
    Elem := FList[CurIndex];
    if CurIndex < NewIndex then
      System.Move(FList[CurIndex + 1], FList[CurIndex],
      (NewIndex - CurIndex) * SizeOf(Variant))
    else
      System.Move(FList[NewIndex], FList[NewIndex + 1],
      (CurIndex - NewIndex) * SizeOf(Variant));
    FList[NewIndex] := Elem;
  end;
end;

procedure TArrayList.Put(Index: Integer; const Value: Variant);
begin
  if (Index < 0) or (Index > MaxListSize) then
    raise EArrayListError.CreateResFmt(@SListIndexError, [Index]);

  if Index >= FCapacity then begin
    FCapacity := Index;
    Grow;
  end;
  if Index >= FCount then FCount := Index + 1;

  FList[Index] := Value;
end;

function TArrayList.ToArray: TVariants;
begin
  Result := Copy(FList, 0, FCount);
end;

function TArrayList.ToArray(VarType: TVarType): Variant;
var
  I: Integer;
begin
  Result := VarArrayCreate([0, FCount - 1], VarType);
  for I := 0 to FCount - 1 do Result[I] := FList[I];
end;

procedure TArrayList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    raise EArrayListError.CreateResFmt(@SListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TArrayList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    raise EArrayListError.CreateResFmt(@SListCountError, [NewCount]);

  if NewCount > FCapacity then begin
    FCapacity := NewCount;
    Grow;
  end
  else if NewCount < FCount then
    for I := FCount - 1 downto NewCount do
      Delete(I);

  FCount := NewCount;
end;

{ THashBucket }

constructor THashBucket.Create(ACapacity: Integer; Factor: Single);
begin
  FCount := 0;
  FFactor := Factor;
  FCapacity := ACapacity;
  SetLength(FIndices, FCapacity);
end;

function THashBucket.GetHashIndex(HashCode: Integer): Integer;
begin
  Result := (HashCode and $7FFFFFFF) mod FCapacity;
end;

procedure THashBucket.Insert(Item: PHashItem);
var
  HashIndex: Integer;
  TempItem: PHashItem;
begin
  HashIndex := GetHashIndex(Item^.HashCode);
  if not Assigned(FIndices[HashIndex]) then begin
    Item^.Prev := Item;
    Item^.Next := nil;
    FIndices[HashIndex] := Item;
  end
  else begin
    TempItem := FIndices[HashIndex]^.Prev;
    repeat
      if Item^.Index >= TempItem^.Index then begin
        Item^.Prev := TempItem;
        Item^.Next := TempItem^.Next;
        TempItem^.Next := Item;
        if not Assigned(Item^.Next) then
          FIndices[HashIndex]^.Prev := Item
        else
          Item^.Next^.Prev := Item;
        Exit;
      end;
      TempItem := TempItem^.Prev;
    until not Assigned(TempItem^.Next);
    Item^.Prev := Findices[HashIndex]^.Prev;
    Item^.Next := Findices[HashIndex];
    Item^.Next^.Prev := Item;
    FIndices[HashIndex] := Item;
  end;
end;

function THashBucket.Add(HashCode, Index: Integer): PHashItem;
begin
  if FCount * FFactor >= FCapacity then Grow;
  System.New(Result);
  Result^.HashCode := HashCode;
  Result^.Index := Index;
  Insert(Result);
  Inc(FCount);
end;

procedure THashBucket.Clear;
var
  I: Integer;
  Item: PHashItem;
begin
  for I := 0 to FCapacity - 1 do begin
    while Assigned(FIndices[I]) do begin
      Item := FIndices[I]^.Next;
      Dispose(FIndices[I]);
      FIndices[I] := Item;
    end;
  end;
  FCount := 0;
end;

function THashBucket.Remove(HashCode, Index: Integer): PHashItem;
var
  HashIndex: Integer;
begin
  HashIndex := GetHashIndex(HashCode);
  Result := FIndices[HashIndex];
  while Assigned(Result) do begin
    if Result^.Index = Index then begin
      if Result^.Prev = Result then
        FIndices[HashIndex] := nil
      else if not Assigned(Result^.Prev^.Next) then begin
        Findices[HashIndex] := Result^.Next;
        Result^.Next^.Prev := Result^.Prev;
      end
      else if not Assigned(Result^.Next) then begin
        Result^.Prev^.Next := Result^.Next;
        Findices[HashIndex]^.Prev := Result^.Prev;
      end
      else begin
        Result^.Prev^.Next := Result^.Next;
        Result^.Next^.Prev := Result^.Prev;
      end;
      Exit;
    end;
    Result := Result^.Next;
  end;
end;

procedure THashBucket.Delete(HashCode, Index: Integer);
var
  Item: PHashItem;
begin
  Item := Remove(HashCode, Index);
  if Assigned(Item) then begin
    Dispose(Item);
    Dec(FCount);
  end;
end;

destructor THashBucket.Destroy;
begin
  Clear;
  inherited;
end;

procedure THashBucket.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function THashBucket.IndexOf(HashCode: Integer; const Value: Variant;
  CompareProc: TIndexCompareMethod; StartIndex, ACount: Integer): Integer;
var
  EndIndex: Integer;
  HashIndex: Integer;
  Item: PHashItem;
begin
  if StartIndex < 0 then StartIndex := 0;
  EndIndex := StartIndex + ACount - 1;
  if EndIndex >= FCount then EndIndex := FCount - 1;
  Result := -1;
  HashIndex := GetHashIndex(HashCode);
  Item := FIndices[HashIndex];
  while Assigned(Item) do begin
    if Item^.Index > EndIndex then Exit;
    if (Item^.Index >= StartIndex) and (Item^.HashCode = HashCode) and CompareProc(Item^.Index, Value) then begin
      Result := Item^.Index;
      Exit;
    end;
    Item := Item^.Next;
  end;
end;

function THashBucket.LastIndexOf(HashCode: Integer; const Value: Variant;
  CompareProc: TIndexCompareMethod; StartIndex, ACount: Integer): Integer;
var
  EndIndex: Integer;
  HashIndex: Integer;
  Item: PHashItem;
begin
  if StartIndex >= FCount then StartIndex := FCount - 1;
  EndIndex := StartIndex - ACount + 1;
  if EndIndex < 0 then EndIndex := 0;
  Result := -1;
  HashIndex := GetHashIndex(HashCode);
  Item := FIndices[HashIndex];
  if not Assigned(Item) then Exit;
  Item := Item^.Prev;
  repeat
    if Item^.Index < EndIndex then Exit;
    if (Item^.Index <= StartIndex) and (Item^.HashCode = HashCode) and CompareProc(Item^.Index, Value) then begin
      Result := Item^.Index;
      Exit;
    end;
    Item := Item^.Prev;
  until not Assigned(Item^.Next);
end;

function THashBucket.Modify(OldHashCode, NewHashCode,
  Index: Integer): PHashItem;
begin
  if OldHashCode = NewHashCode then
    Result := nil
  else begin
    Result := Remove(OldHashCode, Index);
    Result^.HashCode := NewHashCode;
    Insert(Result);
  end;
end;

procedure THashBucket.SetCapacity(NewCapacity: Integer);
var
  OldCapacity, I: Integer;
  OldIndices, NewIndices: THashItemDynArray;
  Item, Next: PHashItem;
begin
  OldIndices := nil;
  if (NewCapacity < 0) or (NewCapacity > MaxListSize) then
    raise EHashBucketError.CreateResFmt(@SListCapacityError, [NewCapacity]);
  if FCapacity = NewCapacity then Exit;
  if NewCapacity = 0 then begin
    Clear;
    SetLength(FIndices, 0);
    FCapacity := 0;
  end
  else begin
    SetLength(NewIndices, NewCapacity);
    OldIndices := FIndices;
    OldCapacity := FCapacity;
    FIndices := NewIndices;
    FCapacity := NewCapacity;
    for I := 0 to OldCapacity - 1 do begin
      Item := OldIndices[I];
      while Assigned(Item) do begin
        Next := Item^.Next;
        Insert(Item);
        Item := Next;
      end;
    end;
  end;
end;

{ THashedList }

function THashedList.Add(const Value: Variant): Integer;
begin
  Result := inherited Add(Value);
  FHashBucket.Add(HashOf(Value), Result);
end;

procedure THashedList.Clear;
begin
  inherited;
  if Assigned(FHashBucket) then FHashBucket.Clear;
end;

constructor THashedList.Create(ACapacity: Integer; Sync, ReadWriteSync: Boolean);
begin
  Create(ACapacity, 0.75, Sync, ReadWriteSync);
end;

constructor THashedList.Create(ACapacity: Integer; Factor: Single; Sync,
  ReadWriteSync: Boolean);
begin
  inherited Create(ACapacity, Sync, ReadWriteSync);
  FHashBucket := THashBucket.Create(ACapacity, Factor);
end;

{$IFDEF BCB}
constructor THashedList.Create3(ACapacity: Integer; Factor: Single;
  Sync: Boolean);
begin
  Create(ACapacity, Factor, Sync);
end;
{$ENDIF}

procedure THashedList.DeleteHash(Index, N: Integer);
var
  I, HashCode, NewHashCode, NewCount: Integer;
begin
  NewCount := FCount - N;
  if Index < NewCount then begin
    for I := Index to NewCount - 1 do begin
      HashCode := HashOf(FList[I]);
      NewHashCode := HashOf(FList[I + N]);
      FHashBucket.Modify(HashCode, NewHashCode, I);
    end;
  end;
  for I := NewCount to FCount - 1 do
    FHashBucket.Delete(HashOf(FList[I]), I);
end;

function THashedList.Delete(Index: Integer): Variant;
begin
  if (Index >= 0) and (Index < FCount) then begin
    Result := FList[Index];
    DeleteHash(Index, 1);
    ShiftLeft(Index, 1);
  end;
end;

procedure THashedList.DeleteRange(Index, ACount: Integer);
begin
  if (Index >= 0) and (Index < FCount) then begin
    if ACount > FCount - Index then ACount := FCount - Index;
    DeleteHash(Index, ACount);
    ShiftLeft(Index, ACount);
  end;
end;

destructor THashedList.Destroy;
begin
  FreeAndNil(FHashBucket);
  inherited;
end;

procedure THashedList.Exchange(Index1, Index2: Integer);
var
  HashCode1, HashCode2: Integer;
begin
  if Index1 = Index2 then exit;
  inherited Exchange(Index1, Index2);
  HashCode1 := HashOf(FList[Index1]);
  HashCode2 := HashOf(FList[Index2]);
  if HashCode1 <> HashCode2 then begin
    FHashBucket.Modify(HashCode1, HashCode2, Index2);
    FHashBucket.Modify(HashCode2, HashCode1, Index1);
  end;
end;

function THashedList.HashOf(const Value: Variant): Integer;
begin
  if VarIsStr(Value) then
    Result := HashOfString(Value)
  else
    Result := HashOfVariant(Value);
end;

procedure THashedList.InsertHash(Index, N: Integer);
var
  HashCode, NewHashCode, I: Integer;
begin
  if Index < FCount then begin
    for I := Index to FCount - N - 1 do begin
      NewHashCode := HashOf(FList[I]);
      HashCode := HashOf(FList[I + N]);
      FHashBucket.Modify(HashCode, NewHashCode, I);
    end;
    for I := FCount - N to FCount - 1 do
      FHashBucket.Add(HashOf(FList[I]), I);
  end;
end;

function THashedList.IndexOf(const Value: Variant; StartIndex,
  ACount: Integer): Integer;
begin
  Result := FHashBucket.IndexOf(HashOf(Value), Value,
    {$IFDEF FPC}@{$ENDIF}IndexCompare, StartIndex, ACount);

end;

procedure THashedList.Insert(Index: Integer; const Value: Variant);
begin
  inherited Insert(Index, Value);
  InsertHash(Index, 1);
end;

procedure THashedList.InsertRange(Index: Integer; const AList: IImmutableList);
begin
  inherited InsertRange(Index, AList);
  InsertHash(Index, AList.Count);
end;

procedure THashedList.InsertRange(Index: Integer; const Container: Variant);
begin
  if VarIsList(Container) then begin
    InsertRange(Index, VarToList(Container));
  end
  else if VarIsArray(Container) then begin
    inherited InsertRange(Index, Container);
    InsertHash(Index, Length(Container));
  end;
end;

procedure THashedList.InsertRange(Index: Integer; const ConstArray: array of const);
begin
  inherited InsertRange(Index, ConstArray);
  InsertHash(Index, Length(ConstArray));
end;

function THashedList.LastIndexOf(const Value: Variant; StartIndex,
  ACount: Integer): Integer;
begin
  Result := FHashBucket.LastIndexOf(HashOf(Value), Value,
    {$IFDEF FPC}@{$ENDIF}IndexCompare, StartIndex, ACount);
end;

procedure THashedList.Put(Index: Integer; const Value: Variant);
var
  HashCode, I: Integer;
begin
  HashCode := HashOf(Value);
  if Index < FCount then
    FHashBucket.Modify(HashOf(FList[Index]), HashCode, Index)
  else if Index = FCount then
    FHashBucket.Add(HashCode, Index)
  else begin
    FHashBucket.Add(HashCode, Index);
    HashCode := HashOf(Unassigned);
    for I := FCount to Index - 1 do FHashBucket.Add(HashCode, I);
  end;

  inherited Put(Index, Value);
end;

procedure THashedList.Move(CurIndex, NewIndex: Integer);
var
  I, HashCode, NewHashCode: Integer;
begin
  if CurIndex = NewIndex then Exit;
  inherited Move(CurIndex, NewIndex);
  HashCode := HashOf(FList[NewIndex]);
  if CurIndex < NewIndex then
    for I := CurIndex to NewIndex - 1 do begin
      NewHashCode := HashOf(FList[I]);
      FHashBucket.Modify(HashCode, NewHashCode, I);
      HashCode := NewHashCode;
    end
  else
    for I := CurIndex downto NewIndex + 1 do begin
      NewHashCode := HashOf(FList[I]);
      FHashBucket.Modify(HashCode, NewHashCode, I);
      HashCode := NewHashCode;
    end;
  FHashBucket.Modify(HashCode, HashOf(FList[NewIndex]), NewIndex);
end;

{ TCaseInsensitiveArrayList }

function TCaseInsensitiveArrayList.Compare(const Value1, Value2: Variant
  ): Integer;
begin
  if VarIsStr(Value1) and VarIsStr(Value2) then
{$IFNDEF NEXTGEN}
    Result := WideCompareText(Value1, Value2)
{$ELSE}
    Result := CompareText(Value1, Value2)
{$ENDIF}
  else
    Result:=inherited Compare(Value1, Value2);
end;

function TCaseInsensitiveArrayList.IndexCompare(Index: Integer;
  const Value: Variant): Boolean;
var
  Elem: Variant;
begin
  Elem := Get(Index);
  if VarIsStr(Elem) and VarIsStr(Value) then
{$IFNDEF NEXTGEN}
    Result := WideCompareText(Elem, Value) = 0
{$ELSE}
    Result := CompareText(Elem, Value) = 0
{$ENDIF}
  else
    Result := VarEquals(Elem, Value)
end;

{ TCaseInsensitiveHashedList }
{$IFDEF BCB}
constructor TCaseInsensitiveHashedList.Create4(ACapacity: Integer;
  Factor: Single; Sync, ReadWriteSync: Boolean);
begin
  Create(ACapacity, Factor, Sync, ReadWriteSync);
end;
{$ENDIF}

function TCaseInsensitiveHashedList.Compare(const Value1, Value2: Variant
  ): Integer;
begin
  if VarIsStr(Value1) and VarIsStr(Value2) then
{$IFNDEF NEXTGEN}
    Result := WideCompareText(Value1, Value2)
{$ELSE}
    Result := CompareText(Value1, Value2)
{$ENDIF}
  else
    Result := inherited Compare(Value1, Value2);
end;

function TCaseInsensitiveHashedList.HashOf(const Value: Variant): Integer;
begin
  if VarIsStr(Value) then
{$IFNDEF NEXTGEN}
    Result := HashOfString(WideLowerCase(Value))
{$ELSE}
    Result := HashOfString(LowerCase(Value))
{$ENDIF}
  else
    Result := HashOfVariant(Value);
end;

function TCaseInsensitiveHashedList.IndexCompare(Index: Integer;
  const Value: Variant): Boolean;
var
  Elem: Variant;
begin
  Elem := Get(Index);
  if VarIsStr(Elem) and VarIsStr(Value) then
{$IFNDEF NEXTGEN}
    Result := WideCompareText(Elem, Value) = 0
{$ELSE}
    Result := CompareText(Elem, Value) = 0
{$ENDIF}
  else
    Result := VarEquals(Elem, Value)
end;

type

  TMapEnumerator = class(TInterfacedObject, IMapEnumerator)
  private
    FMap: TAbstractMap;
    FIndex: Integer;
    function GetCurrent: TMapEntry;
  public
    constructor Create(AMap: TAbstractMap);
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TMapEntry read GetCurrent;
  end;

{ TMapEnumerator }

constructor TMapEnumerator.Create(AMap: TAbstractMap);
begin
  FMap := AMap;
  FIndex := -1;
end;

function TMapEnumerator.GetCurrent: TMapEntry;
begin
  Result.Key := FMap.Keys[FIndex];
  Result.Value := FMap.Values[FIndex];
end;

function TMapEnumerator.MoveNext: Boolean;
begin
    if FIndex < FMap.Count - 1 then begin
    Inc(FIndex);
    Result := True;
  end
  else
    Result := False;
end;

procedure TMapEnumerator.Reset;
begin
  FIndex := -1;
end;

{ TAbstractMap }

constructor TAbstractMap.Create(ACapacity: Integer; Sync: Boolean;
  ReadWriteSync: Boolean = False);
begin
  Create(ACapacity, 0.75, Sync, ReadWriteSync);
end;

constructor TAbstractMap.Create(Sync: Boolean; ReadWriteSync: Boolean);
begin
  Create(16, 0.75, Sync, ReadWriteSync);
end;

constructor TAbstractMap.Create(const AList: IImmutableList;
  Sync: Boolean = True; ReadWriteSync: Boolean = False);
begin
  Create(AList.Count shr 1, Sync, ReadWriteSync);
  Put(AList);
end;

constructor TAbstractMap.Create(const AMap: IMap; Sync: Boolean = True;
  ReadWriteSync: Boolean = False);
begin
  Create(AMap.Count, Sync, ReadWriteSync);
  Put(AMap);
end;

constructor TAbstractMap.Create(const Container: Variant; Sync: Boolean = True;
  ReadWriteSync: Boolean = False);
begin
  if VarIsList(Container) then begin
    Create(VarToList(Container), Sync, ReadWriteSync);
  end
  else if VarIsMap(Container) then begin
    Create(VarToMap(Container), Sync, ReadWriteSync);
  end
  else if VarIsArray(Container) then begin
    Create(Length(Container) shr 1, Sync, ReadWriteSync);
    Put(Container);
  end;
end;

constructor TAbstractMap.Create(const ConstArray: array of const;
  Sync: Boolean = True; ReadWriteSync: Boolean = False);
begin
  Create(Length(ConstArray) shr 1, Sync, ReadWriteSync);
  Put(ConstArray);
end;

{$IFDEF BCB}
constructor TAbstractMap.Create0;
begin
  Create;
end;

constructor TAbstractMap.Create1(ACapacity: Integer);
begin
  Create(ACapacity);
end;

constructor TAbstractMap.Create2(ACapacity: Integer; Factor: Single);
begin
  Create(ACapacity, Factor);
end;

constructor TAbstractMap.Create3(ACapacity: Integer; Factor: Single; Sync: Boolean);
begin
  Create(ACapacity, Factor, Sync);
end;

constructor TAbstractMap.CreateS(Sync: Boolean);
begin
  Create(Sync);
end;

constructor TAbstractMap.CreateL1(const AList: IImmutableList);
begin
  Create(AList);
end;

constructor TAbstractMap.CreateM1(const AMap: IMap);
begin
  Create(AMap);
end;

constructor TAbstractMap.CreateA1(const Container: Variant);
begin
  Create(Container);
end;

constructor TAbstractMap.CreateCA1(const ConstArray: array of const);
begin
  Create(ConstArray);
end;

constructor TAbstractMap.CreateL2(const AList: IImmutableList; Sync: Boolean);
begin
  Create(AList, Sync);
end;

constructor TAbstractMap.CreateM2(const AMap: IMap; Sync: Boolean);
begin
  Create(AMap, Sync);
end;

constructor TAbstractMap.CreateA2(const Container: Variant; Sync: Boolean);
begin
  Create(Container, Sync);
end;

constructor TAbstractMap.CreateCA2(const ConstArray: array of const; Sync: Boolean);
begin
  Create(ConstArray, Sync);
end;

{$ENDIF}

destructor TAbstractMap.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FReadWriteLock);
  inherited Destroy;
end;

function TAbstractMap.GetValue(const AKey: Variant): Variant;
begin
  Result := Get(AKey);
end;

procedure TAbstractMap.PutValue(const AKey: Variant; AValue: Variant);
begin
  Put(AKey, AValue);
end;

function TAbstractMap.GetEnumerator: IMapEnumerator;
begin
  Result := TMapEnumerator.Create(Self);
end;

procedure TAbstractMap.InitLock;
begin
  if not Assigned(FLock) then
    FLock := TCriticalSection.Create;
end;

procedure TAbstractMap.InitReadWriteLock;
begin
  if not Assigned(FReadWriteLock) then
    FReadWriteLock := TMultiReadExclusiveWriteSynchronizer.Create;
end;

function TAbstractMap.Invoke(const Name: string;
  const Arguments: TVarDataArray): Variant;
var
  LMap: IMap;
  Args: TVariants;
  AName: string;
begin
  Result := Unassigned;
  Args := TVariants(Arguments);
  AName := UpperCase(Name);
  if AName = 'ADD' then
    Result := Add(Args[0], Args[1])
  else if AName = 'ASSIGN' then begin
    Result := VarToIntf(Args[0], IMap, LMap);
    if Result then Assign(LMap);
  end
  else if AName = 'CLEAR' then
    Clear
  else if AName = 'CONTAINSKEY' then
    Result := ContainsKey(Args[0])
  else if AName = 'CONTAINSVALUE' then
    Result := ContainsValue(Args[0])
  else if AName = 'DELETE' then
    Result := Delete(Args[0])
  else if AName = 'GET' then
    case Length(Args) of
      1: Result := Get(Args[0]);
      2: Result := Get(Args[0], Args[1]);
    end
  else if AName = 'GETKEY' then
    Result := GetKey(Args[0])
  else if AName = 'PUT' then
    case Length(Args) of
      1: Put(Args[0]);
      2: Put(Args[0], Args[1]);
    end
  else if AName = 'GETENUMERATOR' then
    Result := GetEnumerator()
  else if AName = 'JOIN' then
    case Length(Args) of
      0: Result := Join();
      1: Result := Join(Args[0]);
      2: Result := Join(Args[0], Args[1]);
      3: Result := Join(Args[0], Args[1], Args[2]);
      4: Result := Join(Args[0], Args[1], Args[2], Args[3]);
    end
  else if AName = 'INITLOCK' then
    InitLock
  else if AName = 'INITREADWRITELOCK' then
    InitReadWriteLock
  else if AName = 'LOCK' then
    Lock
  else if AName = 'UNLOCK' then
    Unlock
  else if AName = 'BEGINREAD' then
    BeginRead
  else if AName = 'ENDREAD' then
    EndRead
  else if AName = 'BEGINWRITE' then
    Result := BeginWrite()
  else if AName = 'ENDWRITE' then
    EndWrite
  else if AName = 'PUTALL' then
    PutAll(Args[0])
  else if AName = 'TOLIST' then
    case Length(Args) of
      0: Result := ToArrayList();
      1: Result := ToList(TListClass(NativeInt(Args[0])));
      2: Result := ToList(TListClass(NativeInt(Args[0])), Args[1]);
      3: Result := ToList(TListClass(NativeInt(Args[0])), Args[1], Args[2]);
    end
  else if AName = 'ToArrayList' then
    case Length(Args) of
      0: Result := ToArrayList();
      1: Result := ToArrayList(Args[1]);
      2: Result := ToArrayList(Args[1], Args[2]);
    end
  else if AName = 'SORT' then
    Sort
  else if AName = 'SORTBYVALUE' then
    SortByValue
  else if AName = 'TRIMEXCESS' then
    TrimExcess
  else
    raise Exception.Create('Variant method "' + Name + '" has not found');
end;

procedure TAbstractMap.Lock;
begin
  FLock.Acquire;
end;

procedure TAbstractMap.Unlock;
begin
  FLock.Release;
end;

procedure TAbstractMap.BeginRead;
begin
  FReadWriteLock.BeginRead;
end;

function TAbstractMap.BeginWrite: Boolean;
begin
  Result := FReadWriteLock.BeginWrite;
end;

procedure TAbstractMap.EndRead;
begin
  FReadWriteLock.EndRead;
end;

procedure TAbstractMap.EndWrite;
begin
  FReadWriteLock.EndWrite;
end;


procedure TAbstractMap.Put(const AList: IImmutableList);
var
  I, N: Integer;
begin
  N := AList.Count;
  if Odd(N) then raise EArrayListError.Create('The Count of Alist must be even.');
  I := 0;
  while I < N do begin
      Put(AList[I], AList[I + 1]);
      Inc(I, 2);
  end;
end;

procedure TAbstractMap.Put(const AMap: IMap);
begin
  PutAll(AMap);
end;

procedure TAbstractMap.Put(const Container: Variant);
var
  I, N: Integer;
begin
  if VarIsList(Container) then
    Put(VarToList(Container))
  else if VarIsMap(Container) then
    PutAll(VarToMap(Container))
  else if VarIsArray(Container) then begin
    if Odd(Length(Container)) then raise EArrayListError.Create('The array length must be even.');
    I := VarArrayLowBound(Container, 1);
    N := VarArrayHighBound(Container, 1);
    while I < N do begin
      Put(Container[I], Container[I + 1]);
      Inc(I, 2);
    end;
  end;
end;

procedure TAbstractMap.Put(const ConstArray: array of const);
var
  I, N: Integer;
begin
  N := Length(ConstArray);
  if Odd(N) then raise EArrayListError.Create('The array length must be even.');
  I := 0;
  while I < N do begin
      Put(VarRecToVar(ConstArray[I]), VarRecToVar(ConstArray[I + 1]));
      Inc(I, 2);
  end;
end;


procedure TAbstractMap.PutAll(const AList: IImmutableList);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    Put(I, AList[I]);
end;

procedure TAbstractMap.PutAll(const AMap: IMap);
var
  I: Integer;
  K, V: IImmutableList;
begin
  K := AMap.Keys;
  V := AMap.Values;
  for I := 0 to AMap.Count - 1 do
    Put(K[I], V[I]);
end;

procedure TAbstractMap.PutAll(const Container: Variant);
var
  I: Integer;
begin
  if VarIsList(Container) then
    PutAll(VarToList(Container))
  else if VarIsMap(Container) then
    PutAll(VarToMap(Container))
  else if VarIsArray(Container) then begin
    for I := VarArrayLowBound(Container, 1) to
             VarArrayHighBound(Container, 1) do
      Put(I, Container[I]);
  end;
end;

procedure TAbstractMap.PutAll(const ConstArray: array of const);
var
  I: Integer;
begin
  for I := 0 to Length(ConstArray) - 1 do
      Put(I, VarRecToVar(ConstArray[I]));
end;

function TAbstractMap.Join(const ItemGlue: string; const KeyValueGlue: string;
  const LeftPad: string; const RightPad: string): string;
var
  Buffer: TStringBuffer;
  E: IMapEnumerator;
  Entry: TMapEntry;
begin
  if Count = 0 then begin
    Result := LeftPad + RightPad;
    Exit;
  end;
  E := GetEnumerator;
  Buffer := TStringBuffer.Create(LeftPad);
  E.MoveNext;
  while True do begin
    Entry := E.Current;
    Buffer.WriteString(VarToStr(Entry.Key));
    Buffer.WriteString(KeyValueGlue);
    Buffer.WriteString(VarToStr(Entry.Value));
    if not E.MoveNext then Break;
    Buffer.WriteString(ItemGlue);
  end;
  Buffer.WriteString(RightPad);
  Result := Buffer.ToString;
  Buffer.Free;
end;

class function TAbstractMap.Split(Str: string; const ItemSeparator: string; 
  const KeyValueSeparator: string; Limit: Integer; TrimKey: Boolean; 
  TrimValue: Boolean; SkipEmptyKey: Boolean; SkipEmptyValue: Boolean; 
  Sync: Boolean; ReadWriteSync: Boolean): IMap;

var
  I, L, L2, N: Integer;

procedure SetKeyValue(const AMap: IMap; const S: string);
var
  J: Integer;
  Key, Value: string;
begin
  if (SkipEmptyKey or SkipEmptyValue) and (S = '') then Exit;
  J := AnsiPos(KeyValueSeparator, S);
  if J > 0 then begin
    Key := Copy(S, 1, J - 1);
    if TrimKey then Key := Trim(Key);
    Value := Copy(S, J + L2, MaxInt);
    if TrimValue then Value := Trim(Value);
    if SkipEmptyKey and (Key = '') then Exit;
    if SkipEmptyValue and (Value = '') then Exit;
    AMap[Key] := Value;
  end
  else if SkipEmptyValue then
    Exit
  else if TrimKey then begin
    Key := Trim(S);
    if SkipEmptyKey and (Key = '') then Exit;
    AMap[Key] := '';
  end
  else
    AMap[S] := '';
end;

begin
  if Str = '' then begin
    Result := nil;
    Exit;
  end;
  Result := Self.Create(Sync, ReadWriteSync);
  L := Length(ItemSeparator);
  L2 := Length(KeyValueSeparator);
  N := 0;
  I := L;
  while (I > 0) and ((Limit = 0) or (N < Limit - 1)) do begin
    I := AnsiPos(ItemSeparator, Str);
    if I > 0 then begin
      SetKeyValue(Result, Copy(Str, 1, I - 1));
      Str := Copy(Str, I + L, MaxInt);
      Inc(N);
    end
  end;
  SetKeyValue(Result, Str);
end;

{ THashMap }

procedure THashMap.Assign(const Source: IMap);
begin
  FKeys.Assign(Source.Keys);
  FValues.Assign(Source.Values);
end;

function THashMap.Get(const AKey: Variant; out AValue: Variant): Boolean;
var
  Index: Integer;
begin
  Index := FKeys.IndexOf(AKey);
  AValue := FValues[Index];
  Result := Index > -1;
end;

function THashMap.Add(const AKey, AValue: Variant): Boolean;
var
  Index: Integer;
begin
  Index := FKeys.IndexOf(AKey);
  if Index < 0 then FValues[FKeys.Add(AKey)] := AValue;
  Result := Index < 0;
end;

procedure THashMap.Clear;
begin
  FKeys.Clear;
  FValues.Clear;
end;

function THashMap.ContainsKey(const AKey: Variant): Boolean;
begin
  Result := FKeys.Contains(AKey);
end;

function THashMap.ContainsValue(const AValue: Variant): Boolean;
begin
  Result := FValues.Contains(AValue);
end;

constructor THashMap.Create(ACapacity: Integer; Factor: Single; Sync: Boolean;
  ReadWriteSync: Boolean);
begin
  if Sync then InitLock;
  if ReadWriteSync then InitReadWriteLock;
  InitData(THashedList.Create(ACapacity, Factor, False),
           TArrayList.Create(ACapacity, False));
end;

function THashMap.Delete(const AKey: Variant): Variant;
begin
  Result := FValues.Delete(FKeys.Remove(AKey));
end;

function THashMap.GetCount: Integer;
begin
  Result := FKeys.Count;
end;

function THashMap.Get(const AKey: Variant): Variant;
begin
  Result := FValues[FKeys.IndexOf(AKey)];
end;

function THashMap.GetKey(const AValue: Variant): Variant;
begin
  Result := FKeys[FValues.IndexOf(AValue)];
end;

procedure THashMap.InitData(AKeys, AValues: IList);
begin
  FKeys := AKeys;
  FValues := AValues;
end;

procedure THashMap.Put(const AKey, AValue: Variant);
var
  Index: Integer;
begin
  Index := FKeys.IndexOf(AKey);
  if Index > -1 then
    FValues[Index] := AValue
  else
    FValues[FKeys.Add(AKey)] := AValue;
end;

function THashMap.ToList(ListClass: TListClass; Sync: Boolean;
  ReadWriteSync: Boolean): IList;
var
  I: Integer;
begin
  Result := ListClass.Create(Count, Sync, ReadWriteSync) as IList;
  for I := 0 to Count - 1 do
    if (VarIsOrdinal(FKeys[I])) and (FKeys[I] >= 0)
      and (FKeys[I] <= MaxListSize) then Result.Put(FKeys[I], FValues[I]);
end;

function THashMap.ToArrayList(Sync: Boolean; ReadWriteSync: Boolean
  ): IArrayList;
var
  I: Integer;
begin
  Result := TArrayList.Create(Count, Sync, ReadWriteSync);
  for I := 0 to Count - 1 do
    if (VarIsOrdinal(FKeys[I])) and (FKeys[I] >= 0)
      and (FKeys[I] <= MaxListSize) then Result.Put(FKeys[I], FValues[I]);
end;

function THashMap.CompareKey(const Entry1, Entry2: TMapEntry): Integer;
begin
  Result := FKeys.Compare(Entry1.Key, Entry2.Key);
end;

function THashMap.CompareValue(const Entry1, Entry2: TMapEntry): Integer;
begin
  Result := FValues.Compare(Entry1.Value, Entry2.Value);
end;

procedure THashMap.Sort;
begin
  Sort({$IFDEF FPC}@{$ENDIF}CompareKey);
end;

procedure THashMap.SortByValue;
begin
  Sort({$IFDEF FPC}@{$ENDIF}CompareValue);
end;

procedure THashMap.Sort(CompareProc: TMapCompareMethod);
  function GetEntry(I: Integer): TMapEntry;
  begin
    Result.Key := FKeys.Get(I);
    Result.Value := FValues.Get(I);
  end;
  procedure PutEntry(I: Integer; const Entry: TMapEntry);
  begin
    FKeys.Put(I, Entry.Key);
    FValues.Put(I, Entry.Value);
  end;
  procedure ExchangeEntry(Index1, Index2: Integer);
  begin
    FKeys.Exchange(Index1, Index2);
    FValues.Exchange(Index1, Index2);
  end;
  function Partition(L, H: Integer): Integer;
  var
    Entry: TMapEntry;
  begin
    Entry := GetEntry(L);
    while L < H do begin
      while (L < H) and (CompareProc(Entry, GetEntry(H)) <= 0) do Dec(H);
      ExchangeEntry(L, H);
      while (L < H) and (CompareProc(Entry, GetEntry(L)) >= 0) do Inc(L);
      ExchangeEntry(L, H);
    end;
    Result := L;
  end;
  procedure QuickSort(Low, High: Integer);
  var
    P: Integer;
  begin
    if High - Low > 8 then begin
      P := Partition(Low, High);
      QuickSort(Low, P - 1);
      QuickSort(P + 1, High);
    end;
  end;
var
  I, J, N: Integer;
  Entry: TMapEntry;
begin
  N := Count - 1;
  QuickSort(0, N);
  for I := 1 to N do begin
    Entry := GetEntry(I);
    J := I - 1;
    while (J >= 0) and (CompareProc(Entry, GetEntry(J)) < 0) do begin
      PutEntry(J + 1, GetEntry(J));
      Dec(J);
    end;
    PutEntry(J + 1, Entry);
  end;
end;

procedure THashMap.TrimExcess;
begin
  FKeys.TrimExcess;
  FValues.TrimExcess;
end;

function THashMap.GetKeys: IImmutableList;
begin
  Result := FKeys;
end;

function THashMap.GetValues: IImmutableList;
begin
  Result := FValues;
end;

{ THashedMap }

constructor THashedMap.Create(ACapacity: Integer; Factor: Single;
      Sync, ReadWriteSync: Boolean);
begin
  if Sync then InitLock;
  if ReadWriteSync then InitReadWriteLock;
  InitData(THashedList.Create(ACapacity, Factor, False),
           THashedList.Create(ACapacity, Factor, False));
end;

{ TCaseInsensitiveHashMap }

constructor TCaseInsensitiveHashMap.Create(ACapacity: Integer; Factor: Single;
      Sync, ReadWriteSync: Boolean);
begin
  if Sync then InitLock;
  if ReadWriteSync then InitReadWriteLock;
  InitData(TCaseInsensitiveHashedList.Create(ACapacity, Factor, False),
           TArrayList.Create(ACapacity, False));
end;

{ TCaseInsensitiveHashedMap }

constructor TCaseInsensitiveHashedMap.Create(ACapacity: Integer; Factor: Single;
      Sync, ReadWriteSync: Boolean);
begin
  if Sync then InitLock;
  if ReadWriteSync then InitReadWriteLock;
  InitData(TCaseInsensitiveHashedList.Create(ACapacity, Factor, False),
           THashedList.Create(ACapacity, Factor, False));
end;

{$IFNDEF DELPHI2009_UP}

{ TBytes/string conversion routines }

function BytesOf(const Val: RawByteString): TBytes;
var
  Len: Integer;
begin
  Len := Length(Val);
  SetLength(Result, Len);
  if Len > 0 then Move(Val[1], Result[0], Len);
end;

function BytesOf(const Val: AnsiChar): TBytes;
begin
  SetLength(Result, 1);
  Result[0] := Byte(Val);
end;

function BytesOf(const Val: WideChar): TBytes;
begin
  Result := BytesOf(WideString(Val));
end;

function BytesOf(const Val: WideString): TBytes;
begin
  Result := BytesOf(RawByteString(Val));
end;

function StringOf(const Bytes: TBytes): RawByteString;
var
  Len: Integer;
begin
  if Assigned(Bytes) then begin
    Len := Length(Bytes);
    SetLength(Result, Len);
    if Len > 0 then Move(Bytes[0], Result[1], Len);
  end
  else
    Result := '';
end;

function WideStringOf(const Value: TBytes): WideString;
var
  Len: Integer;
begin
  if Assigned(Value) then begin
    Len := Length(Value);
    SetLength(Result, Len div 2);
    if Len > 0 then Move(Value[0], Result[1], Len);
  end
  else
    Result := '';
end;

function WideBytesOf(const Value: WideString): TBytes;
var
  Len: Integer;
begin
  Len := Length(Value) * 2;
  SetLength(Result, Len);
  if Len > 0 then Move(Value[1], Result[0], Len);
end;

{$ENDIF}

{$IFNDEF DELPHIXE3_UP}
function BytesOf(const Val: Pointer; const Len: integer): TBytes;
begin
  SetLength(Result, Len);
  if Len > 0 then Move(PByte(Val)^, Result[0], Len);
end;
{$ENDIF}

{$IF NOT DEFINED(DELPHI2009_UP) AND NOT DEFINED(FPC)}
const
  MemoryDelta = $2000; { Must be a power of 2 }

{ TBytesStream }

constructor TBytesStream.Create(const ABytes: TBytes);
begin
  inherited Create;
  FBytes := ABytes;
  SetPointer(FBytes, Length(FBytes));
  Capacity := Size;
end;

function TBytesStream.Realloc(var NewCapacity: Integer): Pointer;
begin
  if (NewCapacity > 0) and (NewCapacity <> Size) then
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
  Result := Pointer(FBytes);
  if NewCapacity <> Capacity then
  begin
    SetLength(FBytes, NewCapacity);
    Result := Pointer(FBytes);
    if NewCapacity = 0 then
      Exit;
    if Result = nil then raise EStreamError.CreateRes(@SMemoryStreamError);
  end;
end;
{$IFEND}

{ TStringBuffer }

constructor TStringBuffer.Create(const AString: string);
begin
  FLength := System.Length(AString);
  FCapacity := FLength;
  FPosition := FLength;
  SetLength(FData, FLength);
  Move(PChar(AString)^, FData[0], FLength * SizeOf(Char));
end;

constructor TStringBuffer.Create(ACapacity: Integer);
begin
  FLength := 0;
  FPosition := 0;
  FCapacity := ACapacity;
  SetLength(FData, ACapacity);
end;

procedure TStringBuffer.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

procedure TStringBuffer.InsertString(const AString: string);
var
  Count: Integer;
begin
  if FPosition = FLength then
    WriteString(AString)
  else begin
    Count := System.Length(AString);
    if (FLength + Count > FCapacity) then begin
      FCapacity := FLength + Count;
      Grow;
    end;
    Move(FData[FPosition], FData[FPosition + Count], (FLength - FPosition) * SizeOf(Char));
    Move(PChar(AString)^, FData[FPosition], Count * SizeOf(Char));
    Inc(FPosition, Count);
    Inc(FLength, Count);
  end;
end;


function TStringBuffer.ReadString(Count: Integer): string;
var
  Len: Integer;
begin
  Len := FLength - FPosition;
  if Len > Count then Len := Count;
  if Len > 0 then begin
    SetString(Result, PChar(@FData[FPosition]), Len);
    Inc(FPosition, Len);
  end;
end;

function TStringBuffer.Seek(Offset: Integer; Origin: Word): Integer;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := FLength - Offset;
  end;
  if FPosition > FLength then
    FPosition := FLength
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

procedure TStringBuffer.SetCapacity(NewCapacity: Integer);
begin
  FCapacity := NewCapacity;
  if FLength > NewCapacity then FLength := NewCapacity;
  if FPosition > NewCapacity then FPosition := NewCapacity;
  SetLength(FData, NewCapacity);
end;

procedure TStringBuffer.SetPosition(NewPosition: Integer);
begin
  if NewPosition < 0 then FPosition := 0
  else if NewPosition > FLength then FPosition := FLength
  else FPosition := NewPosition;
end;

function TStringBuffer.ToString: string;
begin
  SetString(Result, PChar(FData), FLength);
end;

procedure TStringBuffer.WriteString(const AString: string);
var
  Count: Integer;
begin
  Count := System.Length(AString);
  if (FPosition + Count > FCapacity) then begin
    FCapacity := FPosition + Count;
    Grow;
  end;
  Move(PChar(AString)^, FData[FPosition], Count * SizeOf(Char));
  Inc(FPosition, Count);
  if FPosition > FLength then FLength := FPosition;
end;

function ArrayList(const Elements: array of const): IArrayList;
begin
  Result := TArrayList.Create(Elements);
end;

function HashedList(const Elements: array of const): IHashedList;
begin
  Result := THashedList.Create(Elements);
end;

function CaseInsensitiveArrayList(const Elements: array of const): ICaseInsensitiveArrayList;
begin
  Result := TCaseInsensitiveArrayList.Create(Elements);
end;

function CaseInsensitiveHashedList(const Elements: array of const): ICaseInsensitiveHashedList;
begin
  Result := TCaseInsensitiveHashedList.Create(Elements);
end;

function HashMap(const Elements: array of const): IHashMap;
begin
  Result := THashMap.Create(Elements);
end;

function HashedMap(const Elements: array of const): IHashedMap;
begin
  Result := THashedMap.Create(Elements);
end;

function CaseInsensitiveHashMap(const Elements: array of const): ICaseInsensitiveHashMap;
begin
  Result := TCaseInsensitiveHashMap.Create(Elements);
end;

function CaseInsensitiveHashedMap(const Elements: array of const): ICaseInsensitiveHashedMap;
begin
  Result := TCaseInsensitiveHashedMap.Create(Elements);
end;

{ TVarObjectType }

{$IF DEFINED(FPC) AND (FPC_VERSION < 3.0)}
const
  VAR_PARAMNOTFOUND = HRESULT($80020004);

procedure TVarObjectType.DispInvoke(Dest: PVarData; const Source: TVarData;
  CallDesc: PCallDesc; Params: Pointer);
var
  method_name: ansistring;
  arg_count: byte;
  args: TVarDataArray;
  arg_idx: byte;
  arg_type: byte;
  arg_byref, has_result: boolean;
  arg_ptr: pointer;
  arg_data: PVarData;
  dummy_data: TVarData;
  arg_advanced: boolean;

const
  argtype_mask = $7F;
  argref_mask = $80;
begin
  arg_count := CallDesc^.ArgCount;
  method_name := ansistring(pchar(@CallDesc^.ArgTypes[arg_count]));
  setLength(args, arg_count);
  if arg_count > 0 then
  begin
    arg_ptr := Params;
    for arg_idx := 0 to arg_count - 1 do
    begin
      arg_type := CallDesc^.ArgTypes[arg_idx] and argtype_mask;
      arg_byref := (CallDesc^.ArgTypes[arg_idx] and argref_mask) <> 0;
      arg_data := @args[arg_count - arg_idx - 1];
      case arg_type of
        varUStrArg: arg_data^.vType := varUString;
        varStrArg: arg_data^.vType := varString;
      else
        arg_data^.vType := arg_type
      end;
      if arg_byref then
      begin
        arg_data^.vType := arg_data^.vType or varByRef;
        arg_data^.vPointer := PPointer(arg_ptr)^;
        Inc(arg_ptr,sizeof(Pointer));
      end
      else
        begin
          arg_advanced:=false;
          case arg_type of
            varError:
              begin
                arg_data^.vError:=VAR_PARAMNOTFOUND;
                arg_advanced := true;
              end;
            varVariant:
              arg_data^ := PVarData(PPointer(arg_ptr)^)^;
            varDouble, varCurrency, varDate, varInt64, varQWord:
              begin
                arg_data^.vQWord := PQWord(arg_ptr)^; // 64bit on all platforms
                inc(arg_ptr,sizeof(QWord));
                arg_advanced := true;
              end;
            { values potentially smaller than sizeof(pointer) must be handled
              explicitly to guarantee endian safety and to prevent copying/
              skipping data (they are always copied into a 4 byte element
              by the compiler, although it will still skip sizeof(pointer)
              bytes afterwards) }
            varSingle:
              arg_data^.vSingle := PSingle(arg_ptr)^;
            varSmallint:
              arg_data^.vSmallInt := PLongint(arg_ptr)^;
            varInteger:
              arg_data^.vInteger := PLongint(arg_ptr)^;
            varBoolean:
              arg_data^.vBoolean := WordBool(PLongint(arg_ptr)^);
            varShortInt:
              arg_data^.vShortInt := PLongint(arg_ptr)^;
            varByte:
              arg_data^.vByte := PLongint(arg_ptr)^;
            varWord:
              arg_data^.vWord := PLongint(arg_ptr)^;
            else
              arg_data^.vAny := PPointer(arg_ptr)^; // 32 or 64bit
          end;
          if not arg_advanced then
            inc(arg_ptr,sizeof(pointer));
        end;
    end;
  end;
  has_result := Assigned(Dest);
  if has_result then
    variant(Dest^) := Unassigned;
  case CallDesc^.CallType of

    1:     { DISPATCH_METHOD }
      if has_result then
      begin
        if arg_count = 0 then
        begin
          // no args -- try GetProperty first, then DoFunction
          if not (GetProperty(Dest^,Source,method_name) or
            DoFunction(Dest^,Source,method_name,args)) then
            RaiseDispError
        end
        else
          if not DoFunction(Dest^,Source,method_name,args) then
            RaiseDispError;
      end
      else
      begin
        // may be procedure?
        if not DoProcedure(Source,method_name,args) then
        // may be function?
        try
          variant(dummy_data) := Unassigned;
          if not DoFunction(dummy_data,Source,method_name,args) then
            RaiseDispError;
        finally
          VarDataClear(dummy_data)
        end;
      end;

    2:     { DISPATCH_PROPERTYGET -- currently never generated by compiler for Variant Dispatch }
      if has_result then
      begin
        // must be property...
        if not GetProperty(Dest^,Source,method_name) then
          // may be function?
          if not DoFunction(Dest^,Source,method_name,args) then
            RaiseDispError
      end
      else
        RaiseDispError;

    4:    { DISPATCH_PROPERTYPUT }
      if has_result or (arg_count<>1) or  // must be no result and a single arg
        (not SetProperty(Source,method_name,args[0])) then
        RaiseDispError;
  else
    RaiseDispError;
  end;
end;
{$IFEND}

function TVarObjectType.GetInstance(const V: TVarData): TObject;
begin
  Result := nil;
  try
    if V.VType = varObject then
      Result := TObject(V.VPointer)
    else if V.VType = varUnknown then
      Result := IntfToObj(IInterface(V.VUnknown))   
    else if V.VType <> varNull then Error(reInvalidCast);
  except
    Error(reInvalidCast);
  end;
end;

function TVarObjectType.DoFunction(var Dest: TVarData; const V: TVarData;
  const Name: string; const Arguments: TVarDataArray): Boolean;
var
  Obj: TObject;
  Intf: IInvokeableVarObject;
  Args: TVarDataArray;
  I, N: Integer;
begin
  N := Length(Arguments);
  if (N = 1) and
     (Arguments[0].VType = varError) and
     (HRESULT(Arguments[0].VError) = -2147352572) // Parameter not found.
  then begin
    SetLength(Args, 0);
    Result := DoFunction(Dest, V, Name, Args);
    Exit;
  end;
  for I := 0 to N - 1 do Arguments[I] := VarDataDeref(Arguments[I]);
  Obj := GetInstance(V);
  Result := True;
  if SameText(Name, 'Free') and (Length(Arguments) = 0) then
    Obj.Free
  else if Supports(Obj.ClassType, IInvokeableVarObject) then begin
    Supports(Obj, IInvokeableVarObject, Intf);
    Variant(Dest) := Intf.Invoke(Name, Arguments);
  end
  else begin
{$IFDEF DELPHI7_UP}
    Result := Assigned(GetMethodInfo(Obj, Name));
    if Result then Variant(Dest) := ObjectInvoke(Obj, Name, TVariants(Arguments));
{$ELSE}
    Result := False;
{$ENDIF}
  end;
end;

function TVarObjectType.GetProperty(var Dest: TVarData;
  const V: TVarData; const Name: string): Boolean;
var
  Obj: TObject;
  Info: PPropInfo;
begin
  Obj := GetInstance(V);
  Info := GetPropInfo(PTypeInfo(Obj.ClassInfo), Name);
  Result := Assigned(Info);
  if Result then Variant(Dest) := HproseCommon.GetPropValue(Obj, Info);
end;

function TVarObjectType.SetProperty({$IF DEFINED(FPC) AND (FPC_VERSION >= 3.0)}var{$ELSE}const{$IFEND} V: TVarData;
  const Name: string; const Value: TVarData): Boolean;
var
  Obj: TObject;
  Info: PPropInfo;
begin
  Obj := GetInstance(V);
  Info := GetPropInfo(PTypeInfo(Obj.ClassInfo), Name);
  Result := Assigned(Info);
  if Result then HproseCommon.SetPropValue(Obj, Info, Variant(VarDataDeref(Value)));
end;

procedure TVarObjectType.CastTo(var Dest: TVarData; const Source: TVarData;
  const AVarType: TVarType);
begin
  if (AVarType = varNull) and IsClear(Source) then
    Variant(Dest) := Null
  else if AVarType = varInteger then
    Variant(Dest) := FindVarData(Variant(Source))^.VInteger
  else if AVarType = varInt64 then
    Variant(Dest) := FindVarData(Variant(Source))^.VInt64
  else if AVarType = varString then
    Variant(Dest) := string(TObject(FindVarData(Variant(Source))^.VPointer).ClassName)
{$IFDEF DELPHI2009_UP}
  else if AVarType = varUString then
    Variant(Dest) := UnicodeString(TObject(FindVarData(Variant(Source))^.VPointer).ClassName)
{$ENDIF}
  else if AVarType = varOleStr then
{$IFNDEF NEXTGEN}
    Variant(Dest) := WideString(TObject(FindVarData(Variant(Source))^.VPointer).ClassName)
{$ELSE}
    Variant(Dest) := TObject(FindVarData(Variant(Source))^.VPointer).ClassName
{$ENDIF}
  else
    RaiseCastError;
end;

procedure TVarObjectType.Clear(var V: TVarData);
begin
  V.VType := varEmpty;
  V.VPointer := nil;
end;

function TVarObjectType.CompareOp(const Left, Right: TVarData;
  const Operation: TVarOp): Boolean;
var
  L, R: TVarData;
begin
  L := FindVarData(Variant(Left))^;
  R := FindVarData(Variant(Right))^;
  Result := False;
  if (L.VType = varObject) and (R.VType = varObject) then
    case Operation of
      opCmpEQ:
        Result := L.VPointer = R.VPointer;
      opCmpNE:
        Result := L.VPointer <> R.VPointer;
    else
      RaiseInvalidOp;
    end
  else
    case Operation of
      opCmpEQ:
        Result := False;
      opCmpNE:
        Result := True;
    else
      RaiseInvalidOp;
    end
end;

procedure TVarObjectType.Copy(var Dest: TVarData; const Source: TVarData;
  const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else
    VarDataClear(Dest);
    Dest.VType := Source.VType;
    Dest.VPointer := Source.VPointer;
end;

function TVarObjectType.IsClear(const V: TVarData): Boolean;
begin
  Result := not Assigned(V.VPointer);
end;

var
  OldVarDispProc: TVarDispProc;

procedure IntfDispInvoke(Dest: PVariant; const Source: Variant;
      CallDesc: PCallDesc; Params: Pointer); cdecl;
begin
  if TVarData(Source).VType=varUnknown then
{$IFDEF DELPHI6}
    VarObjectType.DispInvoke(PVarData(Dest)^, TVarData(Source), CallDesc, Params)
{$ELSE}
    VarObjectType.DispInvoke(PVarData(Dest), TVarData(Source), CallDesc, Params)
{$ENDIF}
  else if Assigned(OldVarDispProc) then
    OldVarDispProc(Dest, Source, CallDesc, Params)
  else
    raise Exception.Create('Variant method calls not supported');
end;

function ListSplit(ListClass: TListClass; Str: string;
  const Separator: string; Limit: Integer; TrimItem: Boolean;
  SkipEmptyItem: Boolean): IList;
begin
  Result := ListClass.Split(Str, Separator, Limit, TrimItem, SkipEmptyItem);
end;

function MapSplit(MapClass: TMapClass; Str: string;
  const ItemSeparator: string; const KeyValueSeparator: string;
  Limit: Integer; TrimKey: Boolean; TrimValue: Boolean;
  SkipEmptyKey: Boolean; SkipEmptyValue: Boolean): IMap;
begin
  Result := MapClass.Split(Str, ItemSeparator, KeyValueSeparator, Limit,
    TrimKey, TrimValue, SkipEmptyKey, SkipEmptyValue);
end;

var
  HproseClassMap: IMap;
  HproseInterfaceMap: IMap;

procedure RegisterClass(const AClass: TClass; const Alias: string);
begin
  HproseClassMap.BeginWrite;
  try
    HproseClassMap[Alias] := NativeInt(AClass);
  finally
    HproseClassMap.EndWrite;
  end;
end;

procedure RegisterClass(const AClass: TInterfacedClass; const IID: TGUID; const Alias: string);
begin
  HproseInterfaceMap.BeginWrite;
  RegisterClass(AClass, Alias);
  try
    HproseInterfaceMap[Alias] := GuidToString(IID);
  finally
    HproseInterfaceMap.EndWrite;
  end;
end;

function GetClassByAlias(const Alias: string): TClass;
begin
  HproseClassMap.BeginRead;
  try
    Result := TClass(NativeInt(HproseClassMap[Alias]));
  finally
    HproseClassMap.EndRead;
  end;
end;

function GetClassAlias(const AClass: TClass): string;
begin
  HproseClassMap.BeginRead;
  try
    Result := HproseClassMap.Key[NativeInt(AClass)];
  finally
    HproseClassMap.EndRead;
  end;
end;

function GetClassByInterface(const IID: TGUID): TInterfacedClass;
begin
  HproseInterfaceMap.BeginRead;
  try
    Result := TInterfacedClass(GetClassByAlias(HproseInterfaceMap.Key[GuidToString(IID)]));
  finally
    HproseInterfaceMap.EndRead;
  end;
end;

function HasRegisterWithInterface(const AClass: TInterfacedClass): Boolean;
begin
  HproseInterfaceMap.BeginRead;
  try
    Result := HproseInterfaceMap.ContainsKey(GetClassAlias(AClass));
  finally
    HproseInterfaceMap.EndRead;
  end;
end;

function GetInterfaceByClass(const AClass: TInterfacedClass): TGUID;
begin
  HproseInterfaceMap.BeginRead;
  try
    Result := StringToGuid(HproseInterfaceMap[GetClassAlias(AClass)]);
  finally
    HproseInterfaceMap.EndRead;
  end;
end;

{ TSmartObject }

function TSmartObject.Value: TObject;
begin
  Result := FValue;
end;

constructor TSmartObject.Create(AObject: TObject);
begin
  inherited Create;
  FValue := AObject;
end;

destructor TSmartObject.Destroy;
begin
  FValue.Free;
  inherited;
end;

{$IFDEF SUPPORTS_GENERICS}

{ TSmartObject<T> }

constructor TSmartObject<T>.Create;
begin
  inherited Create(T.Create());
end;

function TSmartObject<T>.Invoke: T;
begin
  Result := T(Pointer(@FValue)^);
end;

var
  HproseTypeMap: IMap;

procedure RegisterType(const TypeName: string; const TypeInfo: PTypeInfo);
begin
  HproseTypeMap.BeginWrite;
  try
    HproseTypeMap[TypeName] := NativeInt(TypeInfo);
  finally
    HproseTypeMap.EndWrite;
  end;
end;

{ TClassManager }

class procedure TClassManager.Register(const TypeInfo: PTypeInfo; TypeName: string);
var
  UnitName: string;
  TypeData: PTypeData;
begin
  RegisterType(TypeName, TypeInfo);
  TypeData := GetTypeData(TypeInfo);
  case TypeInfo^.Kind of
  tkEnumeration:
{$IFDEF SUPPORTS_RTTI}
    TypeName := TRttiContext.Create.GetType(TypeInfo).QualifiedName;
{$ELSE}
    UnitName := string(TypeData^.EnumUnitName);
{$ENDIF}
{$IFNDEF DELPHIXE3_UP}
  tkClass:       UnitName := string(TypeData^.UnitName);
  tkInterface:   UnitName := string(TypeData^.IntfUnit);
  tkDynArray:    UnitName := string(TypeData^.DynUnitName);
{$ELSE}
  tkClass:       UnitName := TypeData^.UnitNameFld.ToString;
  tkInterface:   UnitName := TypeData^.IntfUnitFld.ToString;
  tkDynArray:    UnitName := TypeData^.DynUnitNameFld.ToString;
{$ENDIF}
  else
    raise ETypeError.Create('Can not register this type: ' + TypeName);
  end;
  if UnitName <> '' then TypeName := UnitName + '.' + TypeName;
  RegisterType(TypeName, TypeInfo);
end;

class procedure TClassManager.Register(const TypeInfo: PTypeInfo);
begin
  Register(TypeInfo, GetTypeName(TypeInfo));
end;

class procedure TClassManager.RegisterSmartObject<T, I>(const Alias: string);
var
  TTI, ITI: PTypeInfo;
begin
  TTI := System.TypeInfo(T);
  ITI := System.TypeInfo(I);
  RegisterClass(TInterfacedClass(GetTypeData(TTI)^.ClassType), GetTypeData(ITI)^.Guid, Alias);
{$IFDEF DELPHIXE3_UP}
  Register(TTI);
{$ELSE}
// Delphi 2010 - XE2 have a bug for GetTypeName from TTI, so I hack it like this
  Register(TTI, string('T' + RightStr(GetTypeName(ITI), Length(GetTypeName(ITI)) - 1)));
{$ENDIF}
  Register(ITI);
end;

class procedure TClassManager.Register<T>;
var
  TI: PTypeInfo;
  TypeName: string;
begin
  TI := System.TypeInfo(T);
  Register(TI);
  TypeName := GetTypeName(TI);
  if StrUtils.AnsiStartsText('TSmartObject<', TypeName) then
    Self.RegisterSmartObject<TSmartObject<T>, ISmartObject<T>>(TypeName);
end;

class procedure TClassManager.Register<T>(const Alias: string);
var
  TI: PTypeInfo;
begin
  TI := System.TypeInfo(T);
  RegisterClass(GetTypeData(TI)^.ClassType, Alias);
  Self.Register<T>;
end;

class procedure TClassManager.Register<T, I>(const Alias: string);
var
  TTI, ITI: PTypeInfo;
begin
  TTI := System.TypeInfo(T);
  ITI := System.TypeInfo(I);
  if ITI^.Kind <> tkInterface then
    raise ETypeError.Create(GetTypeName(ITI) + ' must be a interface');
  RegisterClass(TInterfacedClass(GetTypeData(TTI)^.ClassType), GetTypeData(ITI)^.Guid, Alias);
  Self.Register<T>;
  Self.Register(ITI);
end;

class function TClassManager.GetAlias<T>: string;
var
  TI: PTypeInfo;
begin
  Result := '';
  TI := System.TypeInfo(T);
  if PTypeInfo(TI)^.Kind = tkClass then Result := GetClassAlias(GetTypeData(TI)^.ClassType);
end;

class function TClassManager.GetInterface<T>: TGUID;
begin
  Result := GetInterfaceByClass(TInterfacedClass(GetTypeData(System.TypeInfo(T))^.ClassType));
end;

class function TClassManager.GetClass<I>: TInterfacedClass;
var
  ITI: PTypeInfo;
begin
  ITI := System.TypeInfo(I);
  if ITI^.Kind <> tkInterface then
    raise ETypeError.Create('I must be a interface');
  Result := GetClassByInterface(GetTypeData(ITI)^.Guid);
end;

class function TClassManager.TypeInfo(const Name: string): PTypeInfo;
begin
  HproseTypeMap.BeginRead;
  try
    Result := PTypeInfo(NativeInt(HproseTypeMap[Name]));
  finally
    HproseTypeMap.EndRead;
  end;
end;

{$ENDIF}

class function TClassManager.GetClass(const IID: TGUID): TInterfacedClass;
begin
  Result := GetClassByInterface(IID);
end;

class function TClassManager.GetClass(const Alias: string): TClass;
begin
  Result := GetClassByAlias(Alias);
end;

class function TClassManager.GetInterface(const AClass: TInterfacedClass): TGUID;
begin
  Result := GetInterfaceByClass(AClass);
end;

class function TClassManager.GetAlias(const AClass: TClass): string;
begin
  Result := GetClassAlias(AClass);
end;

class procedure TClassManager.Register(const AClass: TInterfacedClass;
  const IID: TGUID; const Alias: string);
begin
  RegisterClass(AClass, IID, Alias);
end;

class procedure TClassManager.Register(const AClass: TClass;
  const Alias: string);
begin
  RegisterClass(AClass, Alias);
end;


{ THproseContext }

function THproseContext.Get(Key: string): Variant;
begin
  Result := FUserData[Key];
end;

procedure THproseContext.Put(Key: string; const Value: Variant);
begin
  FUserData[Key] := Value;
end;

constructor THproseContext.Create(const AUserData: IMap);
begin
  FUserData := TCaseInsensitiveHashMap.Create(16, 0.75, False);
  if Assigned(AUserData) then FUserData.PutAll(AUserData);
end;

{ TFilterList }

constructor TFilterList.Create();
begin
  inherited Create;
  FFilters := THashedList.Create();
end;

function TFilterList.Get(Index: Integer): IFilter;
var
  F: Variant;
begin
  Result := nil;
  F := FFilters[Index];
  if (not VarIsNull(F)) and (not VarIsEmpty(F)) then
    VarToIntf(F, IFilter, Result);
end;

function TFilterList.GetCount: Integer;
begin
  Result := FFilters.Count;
end;

function TFilterList.Add(const Filter: IFilter): TFilterList;
begin
  if Assigned(Filter) then FFilters.Add(Filter);
  Result := Self;
end;

function TFilterList.Remove(const Filter: IFilter): TFilterList;
begin
  FFilters.Remove(Filter);
  Result := Self;
end;

function TFilterList.InputFilter(var Data: TBytes; const Context: THproseContext
  ): TBytes;
var
  I: Integer;
  AFilter: IFilter;
begin
  Result := Data;
  for I := FFilters.Count - 1 downto 0 do begin
    VarToIntf(FFilters[I], IFilter, AFilter);
    Result := AFilter.InputFilter(Result, Context);
  end;
end;

function TFilterList.OutputFilter(var Data: TBytes; const Context: THproseContext
  ): TBytes;
var
  I: Integer;
  AFilter: IFilter;
begin
  Result := Data;
  for I := 0 to FFilters.Count - 1 do begin
    VarToIntf(FFilters[I], IFilter, AFilter);
    Result := AFilter.OutputFilter(Result, Context);
  end;
end;

{ THandlerManager }

procedure THandlerManager.AddAfterFilterHandler(const Handler: TFilterHandler);
var
  Next: PNextFilterHandler;
begin
  Next := GetNextFilterHandler(FDefaultAfterFilterHandler, Handler);
  if not Assigned(FLastAfterFilterHandler) then begin
    FLastAfterFilterHandler := Next;
    FAfterFilterHandler := Next;
  end
  else begin
    FLastAfterFilterHandler^.Next := Next;
    FLastAfterFilterHandler := Next;
  end
end;

procedure THandlerManager.AddBeforeFilterHandler(const Handler: TFilterHandler);
var
  Next: PNextFilterHandler;
begin
  Next := GetNextFilterHandler(FDefaultBeforeFilterHandler, Handler);
  if not Assigned(FLastBeforeFilterHandler) then begin
    FLastBeforeFilterHandler := Next;
    FBeforeFilterHandler := Next;
  end
  else begin
    FLastBeforeFilterHandler^.Next := Next;
    FLastBeforeFilterHandler := Next;
  end
end;

procedure THandlerManager.AddInvokeHandler(const Handler: TInvokeHandler);
var
  Next: PNextInvokeHandler;
begin
  Next := GetNextInvokeHandler(FDefaultInvokeHandler, Handler);
  if not Assigned(FLastInvokeHandler) then begin
    FLastInvokeHandler := Next;
    FInvokeHandler := Next;
  end
  else begin
    FLastInvokeHandler^.Next := Next;
    FLastInvokeHandler := Next;
  end
end;

constructor THandlerManager.Create(const AInvokeHandler: TInvokeHandler;
  const ABeforeFilterHandler: TFilterHandler;
  const AAfterFilterHandler: TFilterHandler);
begin
  inherited Create;
  FDefaultInvokeHandler := GetNextInvokeHandler(nil, AInvokeHandler);
  FDefaultBeforeFilterHandler := GetNextFilterHandler(nil, ABeforeFilterHandler);
  FDefaultAfterFilterHandler := GetNextFilterHandler(nil, AAfterFilterHandler);
  FInvokeHandler := FDefaultInvokeHandler;
  FBeforeFilterHandler := FDefaultBeforeFilterHandler;
  FAfterFilterHandler := FDefaultAfterFilterHandler;
  FLastInvokeHandler := nil;
  FLastBeforeFilterHandler := nil;
  FLastAfterFilterHandler := nil;
end;

destructor THandlerManager.Destroy;
var
  ANextInvokeHandler: PNextInvokeHandler;
  ANextFilterHandler: PNextFilterHandler;
begin
  While Assigned(FInvokeHandler) do begin
    ANextInvokeHandler := FInvokeHandler;
    FInvokeHandler := FInvokeHandler^.Next;
    Dispose(ANextInvokeHandler);
  end;
  While Assigned(FBeforeFilterHandler) do begin
    ANextFilterHandler := FBeforeFilterHandler;
    FBeforeFilterHandler := FBeforeFilterHandler^.Next;
    Dispose(ANextFilterHandler);
  end;
  While Assigned(FAfterFilterHandler) do begin
    ANextFilterHandler := FAfterFilterHandler;
    FAfterFilterHandler := FAfterFilterHandler^.Next;
    Dispose(ANextFilterHandler);
  end;
  inherited Destroy;
end;

function THandlerManager.GetNextInvokeHandler(const Next: PNextInvokeHandler;
  const Handler: TInvokeHandler): PNextInvokeHandler;
begin
  New(Result);
  Result^.Handler := Handler;
  Result^.Next := Next;
end;

function THandlerManager.GetNextFilterHandler(const Next: PNextFilterHandler;
  const Handler: TFilterHandler): PNextFilterHandler;
begin
  New(Result);
  Result^.Handler := Handler;
  Result^.Next := Next;
end;

initialization
  OldVarDispProc := Variants.VarDispProc;
  Variants.VarDispProc := @IntfDispInvoke;
  VarObjectType := TVarObjectType.Create;
  varObject := VarObjectType.VarType;

  HproseClassMap := TCaseInsensitiveHashedMap.Create(False, True);
  HproseInterfaceMap := TCaseInsensitiveHashedMap.Create(False, True);

{$IFDEF SUPPORTS_GENERICS}
  HproseTypeMap := TCaseInsensitiveHashMap.Create(64, 0.75, False, True);
  HproseTypeMap.BeginWrite;
  try
    HproseTypeMap['System.NativeInt'] := NativeInt(TypeInfo(NativeInt));
    HproseTypeMap['System.NativeUInt'] := NativeInt(TypeInfo(NativeUInt));
    HproseTypeMap['System.ShortInt'] := NativeInt(TypeInfo(ShortInt));
    HproseTypeMap['System.SmallInt'] := NativeInt(TypeInfo(SmallInt));
    HproseTypeMap['System.Integer'] := NativeInt(TypeInfo(Integer));
    HproseTypeMap['System.Int64'] := NativeInt(TypeInfo(Int64));
    HproseTypeMap['System.Byte'] := NativeInt(TypeInfo(Byte));
    HproseTypeMap['System.Word'] := NativeInt(TypeInfo(Word));
    HproseTypeMap['System.Cardinal'] := NativeInt(TypeInfo(Cardinal));
    HproseTypeMap['System.UInt64'] := NativeInt(TypeInfo(UInt64));
    HproseTypeMap['System.Char'] := NativeInt(TypeInfo(Char));
    HproseTypeMap['System.WideChar'] := NativeInt(TypeInfo(WideChar));
    HproseTypeMap['System.UCS4Char'] := NativeInt(TypeInfo(UCS4Char));
    HproseTypeMap['System.Boolean'] := NativeInt(TypeInfo(Boolean));
    HproseTypeMap['System.ByteBool'] := NativeInt(TypeInfo(ByteBool));
    HproseTypeMap['System.WordBool'] := NativeInt(TypeInfo(WordBool));
    HproseTypeMap['System.LongBool'] := NativeInt(TypeInfo(LongBool));
    HproseTypeMap['System.Single'] := NativeInt(TypeInfo(Single));
    HproseTypeMap['System.Double'] := NativeInt(TypeInfo(Double));
    HproseTypeMap['System.Real'] := NativeInt(TypeInfo(Real));
    HproseTypeMap['System.Extended'] := NativeInt(TypeInfo(Extended));
    HproseTypeMap['System.Comp'] := NativeInt(TypeInfo(Comp));
    HproseTypeMap['System.Currency'] := NativeInt(TypeInfo(Currency));
    HproseTypeMap['System.string'] := NativeInt(TypeInfo(string));
    HproseTypeMap['System.UnicodeString'] := NativeInt(TypeInfo(UnicodeString));
{$IFNDEF NEXTGEN}
    HproseTypeMap['System.AnsiChar'] := NativeInt(TypeInfo(AnsiChar));
    HproseTypeMap['System.AnsiString'] := NativeInt(TypeInfo(AnsiString));
    HproseTypeMap['System.RawByteString'] := NativeInt(TypeInfo(RawByteString));
    HproseTypeMap['System.ShortString'] := NativeInt(TypeInfo(ShortString));
    HproseTypeMap['System.UTF8String'] := NativeInt(TypeInfo(UTF8String));
    HproseTypeMap['System.WideString'] := NativeInt(TypeInfo(WideString));
{$ENDIF}
    HproseTypeMap['System.UCS4String'] := NativeInt(TypeInfo(UCS4String));
    HproseTypeMap['System.TDateTime'] := NativeInt(TypeInfo(TDateTime));
    HproseTypeMap['System.TDate'] := NativeInt(TypeInfo(TDate));
    HproseTypeMap['System.TTime'] := NativeInt(TypeInfo(TTime));
    HproseTypeMap['System.Variant'] := NativeInt(TypeInfo(Variant));
    HproseTypeMap['System.OleVariant'] := NativeInt(TypeInfo(OleVariant));
  finally
    HproseTypeMap.EndWrite;
  end;

  TClassManager.Register(TypeInfo(ISmartObject));

{$ENDIF}

{$IFDEF SUPPORTS_GENERICS}
  TClassManager.Register<TArrayList, IImmutableList>('!IImmutableList');
  TClassManager.Register<TArrayList, IList>('!List');
  TClassManager.Register<TArrayList, IArrayList>('!ArrayList');
  TClassManager.Register<THashedList, IHashedList>('!HashedList');
  TClassManager.Register<TCaseInsensitiveArrayList, ICaseInsensitiveArrayList>('!CaseInsensitiveArrayList');
  TClassManager.Register<TCaseInsensitiveHashedList, ICaseInsensitiveHashedList>('!CaseInsensitiveHashedList');
  TClassManager.Register<THashMap, IMap>('!Map');
  TClassManager.Register<THashMap, IHashMap>('!HashMap');
  TClassManager.Register<THashedMap, IHashedMap>('!HashedMap');
  TClassManager.Register<TCaseInsensitiveHashMap, ICaseInsensitiveHashMap>('!CaseInsensitiveHashMap');
  TClassManager.Register<TCaseInsensitiveHashedMap, ICaseInsensitiveHashedMap>('!CaseInsensitiveHashedMap');
{$ELSE}
  RegisterClass(TArrayList, IImmutableList, '!IImmutableList');
  RegisterClass(TArrayList, IList, '!List');
  RegisterClass(TArrayList, IArrayList, '!ArrayList');
  RegisterClass(THashedList, IHashedList, '!HashedList');
RegisterClass(TCaseInsensitiveArrayList, ICaseInsensitiveArrayList, '!CaseInsensitiveArrayList');
  RegisterClass(TCaseInsensitiveHashedList, ICaseInsensitiveHashedList, '!CaseInsensitiveHashedList');
  RegisterClass(THashMap, IMap, '!Map');
  RegisterClass(THashMap, IHashMap, '!HashMap');
  RegisterClass(THashedMap, IHashedMap, '!HashedMap');
  RegisterClass(TCaseInsensitiveHashMap, ICaseInsensitiveHashMap, '!CaseInsensitiveHashMap');
  RegisterClass(TCaseInsensitiveHashedMap, ICaseInsensitiveHashedMap, '!CaseInsensitiveHashedMap');
{$ENDIF}

finalization
  Variants.VarDispProc := OldVarDispProc;
  FreeAndNil(VarObjectType);

end.
