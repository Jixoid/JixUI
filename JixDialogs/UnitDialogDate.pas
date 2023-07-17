unit UnitDialogDate;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, windows,
  JCL.Auxiliary, JCL.Utils, JCL.Forms, JCL.Comps.Button, JCL.Comps.ComboBox;

type

  { TFormDialogDate }

  TFormDialogDate = class(jJixForm)
    Edit1: TEdit;
    Edit2: TEdit;
    JixButMMinus: Tjixbutton;
    JixButSMinus: Tjixbutton;
    JixButSPlus: Tjixbutton;
    JixButOk: Tjixbutton;
    JixButMPlus: Tjixbutton;
    JixComboMount: Tjixcombobox;
    procedure BCButMAddClick(Sender: TObject);
    procedure BCButMMinusClick(Sender: TObject);
    procedure BCButOKClick(Sender: TObject);
    procedure BCButSAddClick(Sender: TObject);
    procedure BCButSMinusClick(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure Edit2Change(Sender: TObject);
    procedure FormShow(Sender: TObject);
    Procedure Jixcombomountchange(Sender: Tobject);
  private

  public

  end;

var
  FormDialogDate: TFormDialogDate;
  Used: Boolean;

Implementation
End.
