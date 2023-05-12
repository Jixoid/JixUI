Unit JixAPI;

{$Mode ObjFpc}{$H+}
//{$Define JRO}

Interface

Uses
  Classes, SysUtils, Forms, Controls, JixPas, MiniJixoid_JCL, Jedab, Graphics,
  BCButton, BCComboBox, BCTypes, BGRABitmapTypes, BGRAFlashProgressBar,
  BCMaterialProgressBarMarquee, BCPanel, JDialogs, JForms, JixTemplate, JixButton;

{$Include Term.inc}

{$Ifdef JRO}{$O+}{$Endif}

Type
  rTheme = Packed Record
    ActiveColor: jARGB;
    InActiveColor: jARGB;
    DarkTheme: Boolean;
  End;

Const
  JixUIVersion: String = '4.0.0.3 (Beta)';

Var
  AnimateSpeed: Int8U = 100;

Function FindComponentForm(bComp: TControl): TForm;

Procedure SetTheme(State: TBCButton; Template: jTemplateCollectionItem);

Procedure SetTheme(State: TBCPanel; Template: TBCPanel);
Procedure SetTheme(State: TBGRAFlashProgressBar; Template: rTheme);
Procedure SetTheme(State: TBCMaterialProgressBarMarquee; Template: rTheme);

Implementation

Function FindComponentForm(bComp: TControl): TForm;

  Function GetParent(bComp2: TControl): TWinControl;
  Begin
    if Assigned(bComp2.Parent) then
      if bComp2.Parent is TForm then
        Result:= bComp2.Parent
      else
        Result:= GetParent(bComp2.Parent)
    else
      Result:= Nil;
  end;

Begin
  Result:= GetParent(bComp) as TForm;
End;

Procedure SetTheme(State: TBCButton; Template: jTemplateCollectionItem);
Begin
  if not (Assigned(State) and Assigned(Template)) then
    CustomErrorSend(
      'Access Violation',
      'JixAPI Error!',
      CustomErrorFlag.Close_Halt
    );

  State.StateNormal.Assign(Template.StateNormal);
  State.StateHover.Assign(Template.StateHover);
  State.StateClicked.Assign(Template.StateClick);

  State.Rounding.RoundX:= Template.CornerRound;
  State.Rounding.RoundY:= Template.CornerRound;
end;

Procedure SetTheme(State: TBCPanel; Template: TBCPanel);
Begin
  if not (Assigned(State) and Assigned(Template)) then
    CustomErrorSend(
      'Access Violation',
      'JixAPI Error!',
      CustomErrorFlag.Close_Halt
    );

  State.Background.Assign(Template.Background);
  State.BorderBCStyle:= Template.BorderBCStyle;
  State.Border.Assign(Template.Border);
  State.FontEx.Assign(Template.FontEx);

  State.Rounding.RoundX:= Template.Rounding.RoundX;
  State.Rounding.RoundY:= Template.Rounding.RoundY;
end;

Procedure SetTheme(State: TBGRAFlashProgressBar; Template: rTheme);
Begin
  if not Assigned(State) then
    CustomErrorSend(
      'Access Violation',
      'JixAPI Error!',
      CustomErrorFlag.Close_Halt
    );

  State.BarColor:= Template.ActiveColor;

  if Template.DarkTheme then
    State.BackgroundColor:= Colors.Black
  else
    State.BackgroundColor:= Colors.White;

end;

Procedure SetTheme(State: TBCMaterialProgressBarMarquee; Template: rTheme);
Begin
  if not Assigned(State) then
    CustomErrorSend(
      'Access Violation',
      'JixAPI Error!',
      CustomErrorFlag.Close_Halt
    );

  State.BarColor:= Template.ActiveColor;

  if Template.DarkTheme then
    State.Color:= Colors.Black
  else
    State.Color:= Colors.White;

end;

{
Procedure SetTheme(bComp: jJixForm);
Var
  ValidColor: jARGB;

  Procedure ReSet(bCont: TControl);
  Var
    i: Integer;
  Begin
    if bCont is TWinControl then
      for i:= 0 to TWinControl(bCont).ControlCount-1 do
        ReSet(TWinControl(bCont).Controls[i]);

    if
      (bCont = bComp.Title.Custom_Caption) or
      (bCont = bComp.Title.Custom_Halt) or
      (bCont = bComp.Title.Custom_Maximize) or
      (bCont = bComp.Title.Custom_Minimize) or
      (bCont = bComp.Title.Custom_Help)
    then
      Exit;

    if bCont is TBCButton then
      SetTheme(bCont as TBCButton);

    if bCont is TBCPanel then
      SetTheme(bCont as TBCPanel);

    if bCont is TBGRAFlashProgressBar then
      SetTheme(bCont as TBGRAFlashProgressBar);

    if bCont is TBCMaterialProgressBarMarquee then
      SetTheme(bCont as TBCMaterialProgressBarMarquee);
  end;

Begin
  //ValidColor:= GetFormThemeColor(bComp);
  ValidColor:= ActiveColor;

  ReSet(bComp);
end;
}

End.
