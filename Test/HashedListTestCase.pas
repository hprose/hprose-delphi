unit HashedListTestCase;

interface

uses
  TestFrameWork, HproseCommon;

type

  TTestCaseHashedList = class(TTestCase)
  private
    procedure CheckEqualsList(Expected: IList; Actual: IList; Msg: string = '');
  published
    procedure TestCreate;
    procedure TestAdd;
    procedure TestAddAll;
    procedure TestInsert;
    procedure TestInsertRange;
    procedure TestMove;
    procedure TestExchange;
    procedure TestContains;
    procedure TestIndexOf;
    procedure TestLastIndexOf;
    procedure TestDelete;
    procedure TestRemove;
    procedure TestClear;
    procedure TestAssign;
    procedure TestToArray;
{$IF RTLVersion >= 17.00}  // Delphi 2005 or later
    procedure TestForIn;
{$IFEND}
    procedure TestSplit;
    procedure TestJoin;
    procedure TestItem;
    procedure TestPack;
    procedure TestReverse;
    procedure TestSort;
    procedure TestShuffle;
    procedure TestVariantPut;
  end;

implementation

uses Variants;

{ TTestCaseHashedList }

procedure TTestCaseHashedList.CheckEqualsList(Expected: IList; Actual: IList; Msg: string);
var
  I: Integer;
begin
  Check(Expected.Count = Actual.Count, Msg);
  for I := 0 to 6 do Check(Expected[I] = Actual[I], Msg);
end;

procedure TTestCaseHashedList.TestCreate;
var
  L: IHashedList;
begin
  L := HashedList([1, 2, 3]);
  Check(L[0] = 1);
  Check(L[1] = 2);
  Check(L[2] = 3);
  L := THashedList.Create(L);
  Check(L[0] = 1);
  Check(L[1] = 2);
  Check(L[2] = 3);
  L := THashedList.Create(L.ToArray);
  Check(L[0] = 1);
  Check(L[1] = 2);
  Check(L[2] = 3);
  L := THashedList.Create();
  Check(L.Count = 0);
  L := THashedList.Create(False, True);
  L.BeginRead;
  L.EndRead;
  L.BeginWrite;
  L.EndWrite;
end;

procedure TTestCaseHashedList.TestAdd;
var
  L: IHashedList;
begin
  L := HashedList([1, 2, 3]);
  L.Add('Hello');
  L.Add(3.14);
  L.Add(True);
  L.Add(HashedList(['a', 'b', 'c']));
  Check(L.Count = 7);
  Check(L[3] = 'Hello');
  Check(L[4] = 3.14);
  Check(L[5] = True);
  Check(L[6].Get(0) = 'a');
  Check(L[6].Get(1) = 'b');
  Check(L[6].Get(2) = 'c');
end;

procedure TTestCaseHashedList.TestAddAll;
var
  L: Variant;
begin
  L := HashedList([1, 2, 3]);
  Check(L.Count = 3);
  L.AddAll(HashedList(['a', 'b', 'c']));
  Check(L.Count = 6);
  L.AddAll(L);
  Check(L.Count = 12);
  Check(L.Get(3) = 'a');
  Check(L.Get(4) = 'b');
  Check(L.Get(5) = 'c');
  Check(L.Get(6) = 1);
  Check(L.Get(7) = 2);
  Check(L.Get(8) = 3);
end;

procedure TTestCaseHashedList.TestInsert;
var
  L: Variant;
  L2: IHashedList;
  I: Integer;
begin
  L := HashedList([1, 'abc', 3.14, True]);
  L.Insert(0, 'top');
  L.Insert(3, 'middle');
  L.Insert(6, 'bottom');
  L2 := HashedList([1, 'abc', 3.14, True]);
  L2.Insert(0, 'top');
  L2.Insert(3, 'middle');
  L2.Insert(6, 'bottom');
  for I := 0 to 6 do Check(L.Get(I) = L2[I]);
end;

procedure TTestCaseHashedList.TestInsertRange;
var
  L: Variant;
  L2: IHashedList;
  I: Integer;
begin
  L := HashedList([1, 'abc', 3.14, True]);
  L.InsertRange(2, HashedList([1, 2, 3]));
  L2 := HashedList([1, 'abc', 3.14, True]);
  L2.InsertRange(2, [1, 2, 3]);
  for I := 0 to 6 do Check(L.Get(I) = L2[I]);
end;

procedure TTestCaseHashedList.TestMove;
var
  L: Variant;
  L2: IHashedList;
  I: Integer;
begin
  L := HashedList([1, 'abc', 3.14, True]);
  L.Move(2, 0);
  L2 := HashedList([3.14, 1, 'abc', True]);
  for I := 0 to 6 do Check(L.Get(I) = L2[I]);
end;

procedure TTestCaseHashedList.TestExchange;
var
  L: Variant;
  L2: IHashedList;
  I: Integer;
begin
  L := HashedList([1, 'abc', 3.14, True]);
  L.Exchange(2, 0);
  L2 := HashedList([3.14, 'abc', 1, True]);
  for I := 0 to 6 do Check(L.Get(I) = L2[I]);
end;

procedure TTestCaseHashedList.TestContains;
var
  L: IList;
begin
  L := HashedList([1, 'abc', 3.14, True, 'abc']);
  Check(L.Contains('hello') = False);
  Check(L.Contains('abc') = True);
end;

procedure TTestCaseHashedList.TestIndexOf;
var
  L: IList;

begin
  L := HashedList([1, 'abc', Variant(3.14), True, 'abc']);
  Check(L.IndexOf(3.14) = 2);
  Check(L.IndexOf(False) = -1);
end;

procedure TTestCaseHashedList.TestLastIndexOf;
var
  L: IList;
begin
  L := HashedList([1, 'abc', 3.14, True, 'abc']);
  Check(L.LastIndexOf('abc') = 4);
  Check(L.LastIndexOf(1) = 0);
  Check(L.LastIndexOf('hello') = -1);
end;

procedure TTestCaseHashedList.TestDelete;
var
  L: IList;
begin
  L := HashedList([1, 'abc', 3.14, True]);
  Check(L.Delete(2) = 3.14);
  Check(L.Delete(2) = True);
end;

procedure TTestCaseHashedList.TestRemove;
var
  L: IList;
begin
  L := HashedList([1, 'abc', 3.14, True, 'abc']);
  Check(L.Remove('abc', FromEnd) = 4);
  Check(L.Remove('abc', FromBeginning) = 1);
  Check(L.Remove(True) = 2);
end;

procedure TTestCaseHashedList.TestClear;
var
  L: IList;
begin
  L := HashedList([1, 'abc', 3.14, True]);
  L.Clear;
  Check(L.Count = 0);
end;

procedure TTestCaseHashedList.TestAssign;
var
  L: IList;
  L2: Variant;
  I: Integer;
begin
  L := HashedList([1, 'abc', 3.14, True]);
  L2 := HashedList([1, 2, 3]);
  Check(L2.Assign(L) = True);
  for I := 0 to 3 do Check(L[I] = L2.Get(I));
end;

procedure TTestCaseHashedList.TestToArray;
var
  L: IList;
  A: array of Integer;
  I: Integer;
begin
  L := HashedList([1, 3, 4, 5, 9]);
  A := L.ToArray(varInteger);
  for I := 0 to Length(A) - 1 do Check(L[I] = A[I]);
end;

{$IF RTLVersion >= 17.00}  // Delphi 2005 or later
procedure TTestCaseHashedList.TestForIn;
var
  L, L2: IList;
  V: Variant;
begin
  L := HashedList([1, 'abc', 3.14, True]);
  L2 := THashedList.Create;
  for V in L do L2.Add(V);
  CheckEqualsList(L, L2);
end;
{$IFEND}


procedure TTestCaseHashedList.TestSplit;
var
  S: String;
begin
  S := 'Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday,';
  CheckEqualsList(
    THashedList.Split(S),
    HashedList(['Monday', ' Tuesday', ' Wednesday', ' Thursday', ' Friday', ' Saturday', ' Sunday', '']),
    'Split 1 failed'
  );
  CheckEqualsList(
    THashedList.Split(S, ', '),
    HashedList(['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday,']),
    'Split 2 failed'
  );
  CheckEqualsList(
    THashedList.Split(S, ',', 4),
    HashedList(['Monday', ' Tuesday', ' Wednesday', ' Thursday, Friday, Saturday, Sunday,']),
    'Split 3 failed'
  );
  CheckEqualsList(
    THashedList.Split(S, ',', 0, true),
    HashedList(['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday', '']),
    'Split 4 failed'
  );
  CheckEqualsList(
    THashedList.Split(S, ',', 0, true, true),
    HashedList(['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']),
    'Split 5 failed'
  );
end;

procedure TTestCaseHashedList.TestJoin;
var
  S: String;
  L: IList;
begin
  S := 'Monday,Tuesday,Wednesday,Thursday,Friday,Saturday,Sunday';
  L := THashedList.Split(S);
  Check(L.Join('; ') = 'Monday; Tuesday; Wednesday; Thursday; Friday; Saturday; Sunday');
  Check(L.Join('", "', '["', '"]') = '["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]');
end;

procedure TTestCaseHashedList.TestItem;
var
  L: IList;
begin
  L := HashedList([1, 'abc', 3.14, True]);
  L[10] := 'test';
  Check(L[9] = Unassigned);
  Check(L[10] = 'test');
  L.Put(12, 'hello');
  Check(L.Get(11) = Unassigned);
  Check(L.Get(12) = 'hello');
  Check(L.Get(-1) = Unassigned);
end;

procedure TTestCaseHashedList.TestPack;
var
  L: IList;
  N: Integer;
begin
  L := HashedList([1, 'abc', 3.14, True]);
  L[10] := 'test';
  Check(L.Count = 11);
  Check(L[10] = 'test');
  N := L.Capacity;
  L.Pack;
  Check(L.Count = 5);
  Check(L[4] = 'test');
  Check(L.Capacity = N);
  L.TrimExcess;
  Check(L.Capacity <> N);
  Check(L.Count = L.Capacity);
end;

procedure TTestCaseHashedList.TestReverse;
var
  L: IList;
begin
  L := HashedList([1, 'abc', 3.14, True]);
  L.Reverse;
  CheckEqualsList(L, HashedList([True, 3.14, 'abc', 1]));
end;

procedure TTestCaseHashedList.TestSort;
var
  L: IList;
begin
  L := HashedList([3, 5, 1, 2, 4, 9, 7, 6, 8]);
  L.Sort;
  CheckEqualsList(L, HashedList([1, 2, 3, 4, 5, 6, 7, 8, 9]));
end;

procedure TTestCaseHashedList.TestShuffle;
var
  Src, Dest: IList;
  Statistics: IList;
  M: IMap;
  I, J, N: Integer;
begin
  Src := HashedList([0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
  Statistics := THashedList.Create(Src.Count);
  for I := 0 to Src.Count - 1 do
    Statistics[I] := HashMap([0, 0, 1, 0, 2, 0, 3, 0, 4, 0, 5, 0, 6, 0, 7, 0, 8, 0, 9, 0]);
  for I := 0 to 99999 do begin
    Dest := THashedList.Create(Src);
    Dest.Shuffle;
    for J := 0 to Statistics.Count - 1 do begin
      M := VarToMap(Statistics[J]);
      N := M.Get(Dest[J]);
      Inc(N);
      M.Put(Dest[J], N);
    end;
  end;
  for I := 0 to Statistics.Count - 1 do begin
    for J := 0 to Statistics[I].Count - 1 do begin
      N := Statistics[I].Values.Get(J);
      Check((9500 < N) and (N < 10500), Variant(N));
    end;
  end;
end;

procedure TTestCaseHashedList.TestVariantPut;
var
  L: IList;
  N: Integer;
  V: Variant;
begin
  L := HashedList([1, 'abc', 3.14, True]);
  N := 10;
  L.Put(3, N);
  Check(L.Get(3) = 10);
  N := 11;
  Check(L.Get(3) = 10);

  N := 10;
  L.Put(3, Variant(N));
  Check(L.Get(3) = 10);
  N := 11;
  Check(L.Get(3) = 10);

  V := 10;
  L.Put(3, V);
  Check(L.Get(3) = 10);
  V := 11;
  Check(L.Get(3) = 10);

  V := 10;
  L.Put(3, VarRef(V));
  Check(L.Get(3) = 10);
  V := 11;
  Check(L.Get(3) = 11);
end;

initialization
  TestFramework.RegisterTest(TTestCaseHashedList.Suite);

end.
