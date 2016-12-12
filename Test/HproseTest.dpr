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
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  ArrayListTestCases in 'ArrayListTestCases.pas',
  HproseClient in '..\Source\HproseClient.pas',
  HproseCommon in '..\Source\HproseCommon.pas',
  HproseIO in '..\Source\HproseIO.pas',
  ObjAutoX in '..\Source\ObjAutoX.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

