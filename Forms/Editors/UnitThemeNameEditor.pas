Unit UnitThemeNameEditor;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, Sysutils, Forms, Controls, Graphics, Dialogs, PropEdits,
  JCL.Utils, JCL.Forms, JCL.Controls, JCL.Comps.Panel, JCL.Comps.List,
  JCL.Comps.Template, JCL.Theme, JCL.Comps.Button,
  Windows, JCL.Dialogs;

Type

  { TFormThemeNameEditor }

  TThemeNamePropertyEditor = class(TStringPropertyEditor)
  public
    procedure Edit; Override;
    function  GetAttributes: TPropertyAttributes; Override;
  end;

  TFormThemeNameEditor = Class(jJixForm)
    Jixbutton2: Tjixbutton;
    Jixbutton3: Tjixbutton;
    Jixlist1: Tjixlist;
    Jixpanel1: Tjixpanel;
    JixPanelMain: Tjixpanel;
    Procedure Formcreate(Sender: Tobject);
    Procedure Formshow(Sender: Tobject);
    Procedure Jixbutton2click(Sender: Tobject);
    Procedure Jixbutton3click(Sender: Tobject);
    Procedure Jixlist1change(Sender: Tobject);
  Private

  Public
    Var EditN: TThemeNamePropertyEditor;
    Var TargetObject: TControl;

  End;

Implementation
End.
