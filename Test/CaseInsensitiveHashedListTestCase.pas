unit CaseInsensitiveHashedListTestCase;

interface

uses
  TestFrameWork, HproseCommon;

type

  TCaseInsensitiveHashedListTestCase = class(TTestCase)
  published
    procedure TestContains;
    procedure TestIndexOf;
    procedure TestLastIndexOf;
    procedure TestRemove;
  end;

implementation

uses Variants;

{ TCaseInsensitiveHashedListTestCase }

procedure TCaseInsensitiveHashedListTestCase.TestContains;
var
  L: IList;
begin
  L := CaseInsensitiveHashedList([1, 'Abc', 3.14, True, 'ABC']);
  Check(L.Contains('hello') = False);
  Check(L.Contains('abc') = True);
end;

procedure TCaseInsensitiveHashedListTestCase.TestIndexOf;
var
  L: IList;
begin
  L := CaseInsensitiveHashedList([1, 'Abc', 3.14, True, 'ABC']);
  Check(L.IndexOf(1.0) = -1);
  Check(L.IndexOf(1) = 0);
  Check(L.IndexOf(3.14) = 2);
  Check(L.IndexOf(False) = -1);
  Check(L.IndexOf('abc') = 1);
  Check(L.IndexOf('abc', 1) = 1);
  Check(L.IndexOf('abc', 2) = 4);
  Check(L.IndexOf('abc', 2, 2) = -1);
end;

procedure TCaseInsensitiveHashedListTestCase.TestLastIndexOf;
var
  L: IList;
begin
  L := CaseInsensitiveHashedList([1, 'Abc', 3.14, True, 'ABC']);
  Check(L.LastIndexOf(1) = 0);
  Check(L.LastIndexOf('hello') = -1);
  Check(L.LastIndexOf('abc') = 4);
  Check(L.LastIndexOf('abc', 3) = 1);
  Check(L.LastIndexOf('abc', 4) = 4);
  Check(L.LastIndexOf('abc', 3, 2) = -1);
end;

procedure TCaseInsensitiveHashedListTestCase.TestRemove;
var
  L: IList;
begin
  L := CaseInsensitiveHashedList([1, 'Abc', 3.14, True, 'ABC']);
  Check(L.Remove('abc', FromEnd) = 4);
  Check(L.Remove('abc', FromBeginning) = 1);
  Check(L.Remove(True) = 2);
end;

initialization
  TestFramework.RegisterTest(TCaseInsensitiveHashedListTestCase.Suite);

end.
