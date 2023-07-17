unit UnitTextAsk;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  JCL.Auxiliary, Windows, JCL.Forms, JCL.Utils, JCL.Comps.Button, JCL.Comps.Edit;

type

  { TFormTextAsk }

  TFormTextAsk = class(jJixForm)
    JixButCancel: Tjixbutton;
    JixButOk: Tjixbutton;
    JixEditText: Tjixedit;
    LabelSubCapion: TLabel;
    procedure FormShow(Sender: TObject);
    Procedure Jixbutcancelclick(Sender: Tobject);
    Procedure Jixbutokclick(Sender: Tobject);
  private

  public

  end;

var
  FormTextAsk: TFormTextAsk;
  Used: Boolean;

Implementation
End.
