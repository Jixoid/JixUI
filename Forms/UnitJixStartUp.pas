Unit UnitJixStartUp;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, Sysutils, Forms, Controls, Graphics, Dialogs, JCL.Comps.Panel,
  JCL.Forms, JCL.Auxiliary, JCL.Comps.ProgressBar, JCL.Comps.StaticLabel,
  JixUIManager;

Type

  { TFormJixStartUp }

  TFormJixStartUp = Class(jJixForm)
    Jixlabel1: Tjixlabel;
    Jixlabel2: Tjixlabel;
    Jixpanel1: Tjixpanel;
    Jixprogressbar1: Tjixprogressbar;
    Procedure Formactivate(Sender: Tobject);
    Procedure Formcreate(Sender: Tobject);
    Procedure Formresize(Sender: Tobject);
  Private

  Public

  End;

Var
  FormJixStartUp: TFormJixStartUp;

Implementation
End.
