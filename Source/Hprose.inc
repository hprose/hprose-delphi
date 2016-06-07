// Delphi version

{$ifdef VER140}
  {$define DELPHI6}
  {$define DELPHI6_UP}
{$endif}

{$ifdef VER150}
  {$define DELPHI7}
  {$define DELPHI7_UP}
{$endif}

{$ifdef VER160}
  {$define DELPHI8}
  {$define DELPHI8_UP}
{$endif}

{$ifdef VER170}
  {$define DELPHI2005}
  {$define DELPHI2005_UP}
{$endif}

{$ifdef VER180}
  {$define DELPHI2006}
  {$define DELPHI2006_UP}
{$endif}

{$ifdef VER185}
  {$define DELPHI2007}
  {$define DELPHI2007_UP}
{$endif}

{$ifdef VER200}
  {$define DELPHI2009}
  {$define DELPHI2009_UP}
{$endif}

{$ifdef VER210}
  {$define DELPHI2010}
  {$define DELPHI2010_UP}
{$endif}

{$ifdef VER220}
  {$define DELPHIXE}
  {$define DELPHIXE_UP}
{$endif}

{$ifdef VER230}
  {$define DELPHIXE2}
  {$define DELPHIXE2_UP}
{$endif}

{$ifdef VER240}
  {$define DELPHIXE3}
  {$define DELPHIXE3_UP}
{$endif}

{$ifdef VER250}
  {$define DELPHIXE4}
  {$define DELPHIXE4_UP}
{$endif}

{$ifdef VER260}
  {$define DELPHIXE5}
  {$define DELPHIXE5_UP}
{$endif}

{$ifdef VER270}
  {$define DELPHIXE6}
  {$define DELPHIXE6_UP}
{$endif}

{$ifdef VER280}
  {$define DELPHIXE7}
  {$define DELPHIXE7_UP}
{$endif}

{$ifdef VER290}
  {$define DELPHIXE8}
  {$define DELPHIXE8_UP}
{$endif}

{$ifdef VER300}
  {$define DELPHIXE9}
  {$define DELPHIXE9_UP}
{$endif}

{$ifdef DELPHIXE9_UP}
  {$define DELPHIXE8_UP}
{$endif}

{$ifdef DELPHIXE8_UP}
  {$define DELPHIXE7_UP}
{$endif}

{$ifdef DELPHIXE7_UP}
  {$define DELPHIXE6_UP}
{$endif}

{$ifdef DELPHIXE6_UP}
  {$define DELPHIXE5_UP}
{$endif}

{$ifdef DELPHIXE5_UP}
  {$define DELPHIXE4_UP}
{$endif}

{$ifdef DELPHIXE4_UP}
  {$define DELPHIXE3_UP}
{$endif}

{$ifdef DELPHIXE3_UP}
  {$define DELPHIXE2_UP}
{$endif}

{$ifdef DELPHIXE2_UP}
  {$define DELPHIXE_UP}
{$endif}

{$ifdef DELPHIXE_UP}
  {$define DELPHI2010_UP}
{$endif}

{$ifdef DELPHI2010_UP}
  {$define DELPHI2009_UP}
{$endif}

{$ifdef DELPHI2009_UP}
  {$define DELPHI2007_UP}
{$endif}

{$ifdef DELPHI2007_UP}
  {$define DELPHI2006_UP}
{$endif}

{$ifdef DELPHI2006_UP}
  {$define DELPHI2005_UP}
{$endif}

{$ifdef DELPHI2005_UP}
  {$define DELPHI8_UP}
{$endif}

{$ifdef DELPHI8_UP}
  {$define DELPHI7_UP}
{$endif}

{$ifdef DELPHI7_UP}
  {$define DELPHI6_UP}
{$endif}

// Delphi features

{$ifdef DELPHI2005_UP}
  {$define Supports_For_In}
  {$define Supports_Inline}
  {$define Supports_Nested_Constants}
  {$define Supports_Nested_Types}
{$endif}

{$ifdef DELPHI2006_UP}
  {$define Supports_Static}
{$endif}

{$ifdef DELPHI2009_UP}
  {$define Supports_Unicode}
  {$define Supports_Generics}
  {$define Supports_Anonymous_Method}
  {$define Supports_UInt64}
{$endif}

{$ifdef DELPHI2010_UP}
  {$define Supports_Rtti}
{$endif}

// Free Pascal

{$ifdef FPC}
  {$define Supports_UInt64}
  {$mode DELPHI}
{$endif}

{$if defined(FPC) and (FPC_FULLVERSION > 20400)}
  {$define Supports_Unicode}
{$ifend}

{$ifdef CPUX64}
  {$define CPU64}
{$endif}

{$H+}
