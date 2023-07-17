Unit UnitBorderEditor;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, Sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, PropEdits,
  JCL.CompTypes, JCL.Comps.Panel, JCL.Comps.Button, JCL.Forms, JCL.Utils,
  JCL.Comps.FillBar, JCL.Comps.RadioButton, JCL.Comps.Edit, Windows,
  JCL.Comps.StaticLabel, mbOfficeColorDialog, JCL.Dialogs;

Type

  { TFormBorderEditor }

  TFormBorderEditor = Class(jJixForm)
    Jixbutcolorp1: Tjixbutton;
    Jixbutcolorp2: Tjixbutton;
    Jixbutton1: Tjixbutton;
    Jixbutton2: Tjixbutton;
    Jixbutton3: Tjixbutton;
    Jixedit1: Tjixedit;
    Jixedit2: Tjixedit;
    Jixeditcolorp1: Tjixedit;
    Jixeditcolorp2: Tjixedit;
    Jixfillbarp1: Tjixfillbar;
    Jixfillbarp2: Tjixfillbar;
    Jixlabel2: Tjixlabel;
    Jixlabel3: Tjixlabel;
    Jixpanel1: Tjixpanel;
    Jixpanel4: Tjixpanel;
    Jixpanel5: Tjixpanel;
    JixPanelMain: Tjixpanel;
    Jixradiobutton1: Tjixradiobutton;
    Jixradiobutton2: Tjixradiobutton;
    Label1: Tlabel;
    Label2: Tlabel;
    Procedure Formcreate(Sender: Tobject);
    Procedure Formshow(Sender: Tobject);
    Procedure Jixbutcolorp1click(Sender: Tobject);
    Procedure Jixbutcolorp2click(Sender: Tobject);
    Procedure Jixbutton2click(Sender: Tobject);
    Procedure Jixbutton3click(Sender: Tobject);
    Procedure Jixfillbarp1change(Sender: Tobject);
    Procedure Jixfillbarp2change(Sender: Tobject);
    Procedure Jixradiobutton1change(Sender: Tobject);
  Private

  Public
    Procedure PLoad(Source: jJixBorder);
    Procedure PUpload(Dest: jJixBorder);
    Var Border: jJixBorder;

  End;

  TBorderPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; Override;
    function  GetAttributes: TPropertyAttributes; Override;
  end;

Implementation
End.
