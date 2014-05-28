{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit HproseIndy;

interface

uses
  HproseClient, HproseCommon, HproseIO, ObjAutoX, HproseIdHttpClient, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('HproseIdHttpClient', @HproseIdHttpClient.Register);
end;

initialization
  RegisterPackage('HproseIndy', @Register);
end.
