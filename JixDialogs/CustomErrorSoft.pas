unit CustomErrorSoft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  JCL.Utils, JCL.Comps.Button, JCL.Comps.Panel;

type

  { TFormCustomErrorSoft }

  TFormCustomErrorSoft = class(TForm)
    Jixbutton1: Tjixbutton;
    Jixpanel1: Tjixpanel;
    Label1: Tlabel;
    Label2: Tlabel;
    procedure BCButton1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    Procedure Up;
    Procedure Down;
  private

  public

  end;

var
  FormCustomErrorSoft: TFormCustomErrorSoft;
  First: Boolean = True;

Implementation
End.
