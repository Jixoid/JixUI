unit JixButton;

{$mode ObjFPC}{$H+}{$Interfaces Corba}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BCButton,
  JixComponent, JixTemplate, JForms;

type
  TJixButton = class(TBCButton, jJixComponent)
  Private
    FThemeName: String;

    Procedure SetThemeName(Value: String);
    Function GetThemeName: String;

  Protected

  Public
    Constructor Create(Aowner: Tcomponent); Override;
    Procedure SetTemplate(Temp: jTemplateCollectionItem);

  Published
    Property ThemeName: String Read GetThemeName Write SetThemeName;

  End;

procedure Register;

implementation

Constructor TJixButton.Create(Aowner: Tcomponent);
Begin
  Inherited;

  ThemeName:= 'Default';
  SetTemplate(FormManager.Templates.Items[0].Templates.Items[0] as jTemplateCollectionItem);
End;

Procedure TJixButton.SetThemeName(Value: String);
Begin
  FThemeName:= Value;
End;

Function TJixButton.GetThemeName: String;
Begin
  Result:= FThemeName;
End;

Procedure TJixButton.SetTemplate(Temp: jTemplateCollectionItem);
Begin
  if not Assigned(Temp) then
    {$ifopt D-}
      CustomErrorSend(
        'Access Violation',
        'JixAPI Error!',
        CustomErrorFlag.Close_Halt
      );
    {$Else}
       Raise Exception.Create('Template is Nil') at Self;
    {$Endif}

  Self.StateNormal.Assign(Temp.StateNormal);
  Self.StateHover.Assign(Temp.StateHover);
  Self.StateClicked.Assign(Temp.StateClick);

  if not Temp.IgnoreRound then
  Begin
    Self.Rounding.RoundX:= Temp.CornerRound;
    Self.Rounding.RoundY:= Temp.CornerRound;
  End;
end;

procedure Register;
begin
  {$I jixbutton_icon.lrs}
  RegisterComponents('JixUI',[TJixButton]);
end;

end.
