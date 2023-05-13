Unit JixComponent;

{$mode Delphi}{$Interfaces Corba}

Interface

Uses
  Classes, Sysutils, JixTemplate;

Type
  jJixComponent = Interface
      Procedure SetTemplate(Temp: jTemplateCollectionItem);

      Procedure SetThemeName(Value: String);
      Function GetThemeName: String;

      Property ThemeName: String Read GetThemeName Write SetThemeName;
  End;

Implementation

End.

