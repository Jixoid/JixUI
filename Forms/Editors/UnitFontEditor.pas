Unit UnitFontEditor;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, Sysutils, Forms, Controls, Graphics, Dialogs, StdCtrls, JCL.Forms,
  PropEdits, JCL.CompTypes, JCL.Comps.Panel, JCL.Comps.Button, JCL.Dialogs,
  JCL.Comps.StaticLabel, JCL.Utils, JCL.Comps.Edit, JCL.Comps.CheckBox, Windows;

Type

  { TFormFontEditor }

  TFormFontEditor = Class(jJixForm)
    Jixbutcolorp1: Tjixbutton;
    Jixbutcolorp2: Tjixbutton;
    Jixbutton1: Tjixbutton;
    Jixbutton2: Tjixbutton;
    Jixbutton3: Tjixbutton;
    Jixcheckbox1: Tjixcheckbox;
    Jixcheckbox2: Tjixcheckbox;
    Jixcheckbox3: Tjixcheckbox;
    Jixcheckbox4: Tjixcheckbox;
    JixEditT: Tjixedit;
    JixEditB: Tjixedit;
    JixEditL: Tjixedit;
    JixEditR: Tjixedit;
    Jixeditcolorp1: Tjixedit;
    Jixeditcolorp2: Tjixedit;
    Jixeditx: Tjixedit;
    Jixeditx1: Tjixedit;
    JixLL: Tjixlabel;
    JixLC: Tjixlabel;
    JixLR: Tjixlabel;
    Jixpanel3: Tjixpanel;
    Jixpanel4: Tjixpanel;
    JixTT: Tjixlabel;
    JixTC: Tjixlabel;
    JixTB: Tjixlabel;
    Jixpanel1: Tjixpanel;
    Jixpanel2: Tjixpanel;
    Label1: Tlabel;
    Label2: Tlabel;
    Label3: Tlabel;
    Label4: Tlabel;
    Procedure Formcreate(Sender: Tobject);
    Procedure Formshow(Sender: Tobject);
    Procedure Jixbutton2click(Sender: Tobject);
    Procedure Jixbutton3click(Sender: Tobject);
    Procedure Jixcheckbox1change(Sender: Tobject);
    Procedure Jixllclick(Sender: Tobject);
  Private

  Public
    Procedure PLoad(Source: jJixFontEx);
    Procedure PUpload(Dest: jJixFontEx);
    Var FontEx: jJixFontEx;

  End;

  TFontPropertyEditor = class(TClassPropertyEditor)
  Public
    procedure Edit; Override;
    function  GetAttributes: TPropertyAttributes; Override;
  End;

Implementation
End.
