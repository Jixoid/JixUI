unit JixComboBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BCComboBox,
  JixComponent, JixTemplate, JForms, MiniJixoid_JCL, JixPas;

type
  TJixComboBox = class(TBCComboBox, jJixComponent)
  private
    FThemeName: String;

  protected
    Procedure SetThemeName(Value: String);
    Function GetThemeName: String;

  public
    Constructor Create(Aowner: Tcomponent); Override;
    Procedure SetTemplate(Temp: jTemplateCollectionItem);

  published
    Property ThemeName: String Read GetThemeName Write SetThemeName;

  end;

procedure Register;

implementation

Constructor TJixComboBox.Create(Aowner: Tcomponent);
Begin
  Inherited;

  ThemeName:= 'Default';
  SetTemplate(FormManager.Templates.Items[0].Templates.Items[0] as jTemplateCollectionItem);

  Width:= 123;
  Height:= 33;
End;

Procedure TJixComboBox.SetThemeName(Value: String);
Begin
  FThemeName:= Value;
End;

Function TJixComboBox.GetThemeName: String;
Begin
  Result:= FThemeName;
End;

Procedure TJixComboBox.SetTemplate(Temp: jTemplateCollectionItem);
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

  DropDownColor:= Temp.BackgroundColor;
  DropDownFontColor:= FindFontColor(DropDownColor);
  DropDownBorderColor:= DropDownFontColor;

  DropDownHighlight:= Temp.StateNormal.Background.Color;
  DropDownFontHighlight:= FindFontColor(DropDownHighlight);

  FocusBorderColor:= DropDownHighlight;
  FocusBorderOpacity:= 125;
end;

procedure Register;
begin
  {$I jixcombobox_icon.lrs}
  RegisterComponents('JixUI',[TJixComboBox]);
end;

end.
