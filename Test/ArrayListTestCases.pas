unit ArrayListTestCases;

interface

uses
  TestFrameWork, HproseCommon;

type
  TTestCaseArrayList = class(TTestCase)
  published
    procedure TestCreate1;
  end;

implementation

{ TTestCaseArrayList }

procedure TTestCaseArrayList.TestCreate1;
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
  //Check(L[2] = 3);
end;

initialization
  TestFramework.RegisterTest(TTestCaseArrayList.Suite);
end.
