Unit UnitBackgroundEditor;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, Sysutils, Forms, Controls, Graphics, Dialogs, PropEdits,
  JCL.CompTypes, JCL.Comps.Panel, JCL.Comps.Button, JCL.Utils, JCL.Forms,
  JCL.Comps.RadioButton, JCL.Comps.FillBar, JCL.Comps.Edit, Windows,
  JCL.Comps.StaticLabel, JCL.Comps.CheckBox, JCL.Comps.ComboBox,
  mbOfficeColorDialog, BGRABitmapTypes, JCL.Dialogs;

Type

  { TFormBackgroundEditor }

  TFormBackgroundEditor = Class(jJixForm)
    Jixbutcolor1: Tjixbutton;
    JixButColorP1: Tjixbutton;
    Jixbutcolorp2: Tjixbutton;
    Jixbutsave: Tjixbutton;
    Jixbutton1: Tjixbutton;
    Jixbutton3: Tjixbutton;
    Jixchecksinus: Tjixcheckbox;
    Jixcombobox1: Tjixcombobox;
    Jixeditcolor1: Tjixedit;
    JixEditColorP1: Tjixedit;
    Jixeditcolorp2: Tjixedit;
    Jixfillbar1: Tjixfillbar;
    JixFillBarP1: Tjixfillbar;
    Jixfillbarp2: Tjixfillbar;
    Jixlabel1: Tjixlabel;
    Jixlabel2: Tjixlabel;
    Jixlabel3: Tjixlabel;
    JixP1: Tjixlabel;
    JixP2: Tjixlabel;
    Jixpanel1: Tjixpanel;
    Jixpanel2: Tjixpanel;
    Jixpanel3: Tjixpanel;
    Jixpanel4: Tjixpanel;
    Jixpanel5: Tjixpanel;
    Jixradiobutton1: Tjixradiobutton;
    Jixradiobutton2: Tjixradiobutton;
    Jixradiobutton3: Tjixradiobutton;
    Jixradiobutton4: Tjixradiobutton;
    Procedure Formcreate(Sender: Tobject);
    Procedure Formshow(Sender: Tobject);
    Procedure Jixbutcolor1click(Sender: Tobject);
    Procedure Jixbutcolorp1click(Sender: Tobject);
    Procedure JixButColorP2Click(Sender: Tobject);
    Procedure Jixbutsaveclick(Sender: Tobject);
    Procedure Jixbutton3click(Sender: Tobject);
    Procedure Jixfillbar1change(Sender: Tobject);
    Procedure Jixfillbarp1change(Sender: Tobject);
    Procedure JixFillBarP2Change(Sender: Tobject);
    Procedure JixP1MouseDown(Sender: Tobject; Button: Tmousebutton;
      {%H-}Shift: Tshiftstate; {%H-}X, {%H-}Y: Integer);
    Procedure JixP1MouseMove(Sender: Tobject; Shift: Tshiftstate; {%H-}X,
      {%H-}Y: Integer);
    Procedure Jixradiobutton1change(Sender: Tobject);
  Private

  Public
    Procedure PLoad(Source: jJixBackground);
    Procedure PUpload(Dest: jJixBackground);
    Var Background: jJixBackground;

  End;

  TBackgroundPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; Override;
    function  GetAttributes: TPropertyAttributes; Override;
  end;

Implementation
End.
