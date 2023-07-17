unit UnitDialogTime;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, windows,
  JCL.Auxiliary, JCL.Utils, JCL.Forms, JCL.Comps.Button;

type

  { TFormDialogTime }

  TFormDialogTime = class(jJixForm)
    Edit1: TEdit;
    Edit2: TEdit;
    JixButOk: Tjixbutton;
    JixButMPlus: Tjixbutton;
    JixButMMinus: Tjixbutton;
    JixButSPlus: Tjixbutton;
    JixButSMinus: Tjixbutton;
    procedure BCButMAddClick(Sender: TObject);
    procedure BCButMMinusClick(Sender: TObject);
    procedure BCButOKClick(Sender: TObject);
    procedure BCButSAddClick(Sender: TObject);
    procedure BCButSMinusClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  FormDialogTime: TFormDialogTime;
  Used: Boolean;

Implementation
End.
