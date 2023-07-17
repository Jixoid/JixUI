Unit UnitIntAsk;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  JCL.Auxiliary, Windows, JCL.Forms, JCL.Utils, JCL.Comps.Button,
  JCL.Comps.FillBar, JCL.Comps.Edit;

Type

  { TFormIntAsk }

  TFormIntAsk = Class(jJixForm)
    JixButCancel: Tjixbutton;
    JixButOk: Tjixbutton;
    Jixedit1: Tjixedit;
    Jixfillbar1: Tjixfillbar;
    Labelsubcapion: Tlabel;
    Procedure Formshow(Sender: Tobject);
    Procedure Jixbutcancelclick(Sender: Tobject);
    Procedure Jixbutokclick(Sender: Tobject);
    Procedure Jixedit1change(Sender: Tobject);
    Procedure Jixfillbar1change(Sender: Tobject);
  Private

  Public

  End;

Var
  FormIntAsk: TFormIntAsk;
  Used: Boolean;

Implementation
End.
