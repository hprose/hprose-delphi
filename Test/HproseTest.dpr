program HproseTest;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options
  to use the console test runner.  Otherwise the GUI test runner will be used by
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  SysUtils,
  TextTestRunner,
{$IFDEF FASTMM}
  FastMM4,
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$IFNDEF NEXTGEN}
  Forms,
  GuiTestRunner,
  {$ENDIF NEXTGEN}
{$ENDIF}
  TestFramework,
  ArrayListTestCase in 'ArrayListTestCase.pas',
  HashedListTestCase in 'HashedListTestCase.pas',
  CaseInsensitiveArrayListTestCase in 'CaseInsensitiveArrayListTestCase.pas',
  HproseClient in '..\Source\HproseClient.pas',
  HproseCommon in '..\Source\HproseCommon.pas',
  HproseIO in '..\Source\HproseIO.pas',
  ObjAutoX in '..\Source\ObjAutoX.pas';

{$R *.RES}

begin
  {$IFDEF MSWINDOWS}
    {$IFNDEF NEXTGEN}
  Application.Initialize;
    {$ENDIF NEXTGEN}
  {$ENDIF}
  if IsConsole then
  begin
    {$IFNDEF NEXTGEN}
    with TextTestRunner.RunRegisteredTests do
      Free;
    {$ELSE NEXTGEN}
      TextTestRunner.RunRegisteredTests
    {$ENDIF NEXTGEN}
  end
  else
  begin
    {$IFDEF MSWINDOWS}
    {$IFNDEF NEXTGEN}
    GuiTestRunner.RunRegisteredTests;
    {$ENDIF NEXTGEN}
    {$ENDIF}
  end;
end.

