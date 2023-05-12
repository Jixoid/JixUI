Unit JixComponent;

{$mode Delphi}{$Interfaces Corba}

Interface

Uses
  Classes, Sysutils;

Type
  jBGRAComponent = Interface
      Procedure SetThemeName(Value: String);
      Function GetThemeName: String;

      Property ThemeName: String Read GetThemeName Write SetThemeName;
  End;

Implementation

End.

