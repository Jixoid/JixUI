unit JixComboBox;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, BCComboBox,
  JixComponent, JForms, MiniJixoid_JCL, JixPas;

type
  TJixComboBox = class(TBCComboBox, iJixComponent)
  Private
    Var FThemeName: String;

    Procedure SetThemeName(Value: String);
    Function GetThemeName: String;

    Procedure SetStatic(Value: Boolean);
    Function GetStatic: Boolean;

    Property StaticButton;

  Protected

  Public
    Constructor Create(Aowner: Tcomponent); Override;
    Procedure SetTemplate(Temp: jTemplateCollectionItem);

  Published
    Property ThemeName: String Read GetThemeName Write SetThemeName;
    Property Static: Boolean   Read GetStatic    Write SetStatic    Default False;

    Property OnClick;
    Property OnDblClick;
    Property OnMouseDown;
    Property OnMouseEnter;
    Property OnMouseLeave;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnMouseWheel;
    Property OnMouseWheelDown;
    Property OnMouseWheelUp;
    Property ParentColor;
    Property PopupMenu;
  End;

procedure Register;

implementation

Constructor TJixComboBox.Create(Aowner: Tcomponent);
Begin
  Inherited;

  Static:= False;

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
  {
  if not Assigned(Temp) then
    {$ifopt D-}
      if
        MessageDlg(
          'Access Violation', '',
          mtError, [mbIgnore, mbAbort], 0
        ) = mrAbort
      then
        Halt;
    {$Else}
       if
        MessageDlg(
          'Access Violation',
          'Temp parameter isn''t assigned' +#13+#13+
          'Class: '+Self.ClassName +#13+
          'Method: '+'SetTemplate' +#13+
          'Parameter: '+'Temp' +#13,
          mtError, [mbIgnore, mbAbort], 0
        ) = mrAbort
      then
        Raise EAccessViolation.Create('Temp parameter isn''t assigned');
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
  }
end;

Procedure TJixComboBox.SetStatic(Value: Boolean);
Begin
  StaticButton:= Value;
End;

Function TJixComboBox.GetStatic: Boolean;
Begin
  Result:= StaticButton;
End;

procedure Register;
begin
  {$I jixcombobox_icon.lrs}
  RegisterComponents('JixUI',[TJixComboBox]);
end;

end.
