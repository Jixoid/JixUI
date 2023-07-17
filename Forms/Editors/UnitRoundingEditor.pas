Unit UnitRoundingEditor;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, Sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  JCL.CompTypes, PropEdits, JCL.Utils, JCL.Comps.Panel, JCL.Comps.Button,
  JCL.Forms, JCL.Comps.Edit, JCL.Comps.RadioButton, JCL.Comps.FillBar,
  BGRABitmapTypes, Windows, JCL.Dialogs;

Type

  { TFormRoundingEditor }

  TFormRoundingEditor = Class(jJixForm)
    Jixbutton1: Tjixbutton;
    Jixbutton3: Tjixbutton;
    Jixbuttonsave: Tjixbutton;
    Jixfillbar1: Tjixfillbar;
    Jixfillbar2: Tjixfillbar;
    Jixlbb: Tjixradiobutton;
    Jixlbs: Tjixradiobutton;
    Jixltb: Tjixradiobutton;
    Jixlts: Tjixradiobutton;
    JixeditX: Tjixedit;
    JixeditY: Tjixedit;
    Jixrbb: Tjixradiobutton;
    Jixrbs: Tjixradiobutton;
    Jixrtb: Tjixradiobutton;
    Jixrts: Tjixradiobutton;
    Label1: TLabel;
    Label2: TLabel;
    Jixpanel1: Tjixpanel;
    JixPanelMain: Tjixpanel;
    Procedure Formcreate(Sender: Tobject);
    Procedure Formshow(Sender: Tobject);
    Procedure Jixbutton3click(Sender: Tobject);
    Procedure Jixbuttonsaveclick(Sender: Tobject);
    Procedure Jixfillbar1change(Sender: Tobject);
    Procedure Jixltschange(Sender: Tobject);
  Private

  Public
    Procedure PLoad(Source: jJixRounding);
    Procedure PUpload(Dest: jJixRounding);
    Var Rounding: jJixRounding;

  End;

 TRoundingPropertyEditor = class(TClassPropertyEditor)
 public
   procedure Edit; Override;
   function  GetAttributes: TPropertyAttributes; Override;
 end;

Implementation
End.
