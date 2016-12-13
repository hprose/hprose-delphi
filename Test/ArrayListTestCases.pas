unit ArrayListTestCases;

interface

uses
  TestFrameWork, HproseCommon;

type
  {$M+}
  TTestCaseArrayList = class(TTestCase)
  published
    procedure TestCreate;
    procedure TestAdd;
  end;

implementation

{ TTestCaseArrayList }

procedure TTestCaseArrayList.TestCreate;
var
  L: IArrayList;
begin
  L := TArrayList.Create([1, 2, 3]);
  Check(L[0] = 1);
  Check(L[1] = 2);
  Check(L[2] = 3);
  L := TArrayList.Create(L);
  Check(L[0] = 1);
  Check(L[1] = 2);
  Check(L[2] = 3);
  L := TArrayList.Create(L.ToArray);
  Check(L[0] = 1);
  Check(L[1] = 2);
  Check(L[2] = 3);
  L := TArrayList.Create();
  Check(L.Count = 0);
  L := TArrayList.Create(False, True);
  L.BeginRead;
  L.EndRead;
  L.BeginWrite;
  L.EndWrite;
end;

procedure TTestCaseArrayList.TestAdd;
var
  L: IArrayList;
begin
  L := TArrayList.Create([1, 2, 3]);
  L.Add('Hello');
  L.Add(3.14);
  L.Add(True);
  L.Add(ArrayList(['a', 'b', 'c']));
  Check(L.Count = 7);
  Check(L[3] = 'Hello');
  Check(L[4] = 3.14);
  Check(L[5] = True);
  Check(L[6].Get(0) = 'a');
  Check(L[6].Get(1) = 'b');
  Check(L[6].Get(2) = 'c');
end;

initialization
  TestFramework.RegisterTest(TTestCaseArrayList.Suite);
end.
