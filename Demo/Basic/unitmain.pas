Unit UnitMain;

{$Mode Delphi}

Interface

Uses
  Classes, Sysutils, Forms, Controls, Graphics, Dialogs, RTTIGrids, JixButton,
  JixComboBox, JixTemplate, JForms, JixPas, StdCtrls, ExtCtrls, ComCtrls, Menus,
  MiniJixoid_JCL, JixRadioButton, JixCheckBox;

Type

  { TFormMain }

  TFormMain = Class(jJixForm)
    Jixbutton1: Tjixbutton;
    Jixbutton2: Tjixbutton;
    Jixbutton3: Tjixbutton;
    Jixcheckbox1: Tjixcheckbox;
    Jixcheckbox2: Tjixcheckbox;
    Jixcheckbox3: Tjixcheckbox;
    Jixcombobox1: Tjixcombobox;
    Jixradiobutton1: Tjixradiobutton;
    Jixradiobutton2: Tjixradiobutton;
    Jixradiobutton3: Tjixradiobutton;
    Jixradiobutton4: Tjixradiobutton;
    Jixradiobutton5: Tjixradiobutton;
    Jixradiobutton6: Tjixradiobutton;
    Jixtemplate1: Tjixtemplate;
    Procedure Formcreate(Sender: Tobject);
    Procedure Jixbutton1click(Sender: Tobject);
    Procedure Jixbutton2click(Sender: Tobject);
    Procedure Jixbutton3click(Sender: Tobject);
    Procedure Jixradiobutton4change(Sender: Tobject);
  Private
    Procedure OnFocus(Sender: TObject);
    Procedure OnDeFocus(Sender: TObject);
    Function GetterActive: jARGB;
    Function GetterInActive: jARGB;

  Public

  End;

Var
  FormMain: TFormMain;

Implementation

{$R *.lfm}

Procedure Tformmain.Formcreate(Sender: Tobject);
Var
  AForm: TForm;
  Prop: TTIPropertyGrid;
Begin
  ThemeName:= 'Test';

  WhenFocus:= OnFocus;
  WhenDeFocus:= OnDeFocus;

  Title.TextPosition:= haCenter;

  Application.CreateForm(TForm, AForm);
  Prop:= TTIPropertyGrid.Create(Nil);
  Prop.Parent:= AForm;
  Prop.Align:= alClient;
  Prop.TIObject:= Jixtemplate1.Templates.Items[0];
  Prop.Filter:= Prop.Filter -[tkMethod];
  Prop.CheckboxForBoolean:= True;
  AForm.Height:= Screen.Height;

  AForm.Caption:= 'Editing: ' +TComponent(Prop.TIObject).Name;
  AForm.Show;
End;

Procedure Tformmain.Jixbutton1click(Sender: Tobject);
Begin
  FormManager.GetterActiveColor:= Nil;
  FormManager.GetterInActiveColor:= Nil;
End;

Var
  MyActive: jARGB;
  MyInActive: jARGB;

Function Tformmain.GetterActive: jARGB;
Begin
  Result:= MyActive;
End;

Function Tformmain.GetterInActive: jARGB;
Begin
  Result:= MyInActive;
End;

Procedure Tformmain.Jixbutton2click(Sender: Tobject);
Var
  Cac: TColorDialog;
Begin
  Cac:= TColorDialog.Create(Nil);

  if Cac.Execute then
  Begin
    MyActive:= Cac.Color;

    FormManager.GetterActiveColor:= GetterActive;
  End;

  Cac.Free;
End;

Procedure Tformmain.Jixbutton3click(Sender: Tobject);
Var
  Cac: TColorDialog;
Begin
  Cac:= TColorDialog.Create(Nil);

  if Cac.Execute then
  Begin
    MyInActive:= Cac.Color;

    FormManager.GetterInActiveColor:= GetterInActive;
  End;

  Cac.Free;
End;

Procedure Tformmain.Jixradiobutton4change(Sender: Tobject);
Begin
  if TJixRadioButton(Sender).Checked then
    Case TControl(Sender).Tag of
      1: jTemplateCollectionItem(Jixtemplate1.Templates.Items[0]).DarkTheme:= dtAuto;

      2: jTemplateCollectionItem(Jixtemplate1.Templates.Items[0]).DarkTheme:= dtDark;

      3: jTemplateCollectionItem(Jixtemplate1.Templates.Items[0]).DarkTheme:= dtLight;
    End;

  FormManager.ForceReSet(Self);
End;

Procedure TFormMain.OnFocus(Sender: TObject);
Begin
  Title.Caption:= 'Focus';
End;

Procedure TFormMain.OnDeFocus(Sender: TObject);
Begin
  Title.Caption:= 'DeFocus';
End;

End.

