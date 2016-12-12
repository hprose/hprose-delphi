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
  Check(L.Count = 4);
  Check(L[3] = 'Hello');
end;

initialization
  TestFramework.RegisterTest(TTestCaseArrayList.Suite);
end.
