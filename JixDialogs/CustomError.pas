unit CustomError;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  JCL.Utils, JCL.Forms, JCL.Comps.Button, JCL.Comps.Memo, Windows;

type

  { TFormCustomError }

  TFormCustomError = class(jJixForm)
    JixButLog: Tjixbutton;
    Memolog: Tjixmemo;
    Procedure Formactivate(Sender: Tobject);
    Procedure Formcreate(Sender: Tobject);
    Procedure Jixbutlogclick(Sender: Tobject);
  private

  public

  end;

var
  FormCustomError: TFormCustomError;

Implementation
End.
