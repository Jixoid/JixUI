unit UnitQuestAsk;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Windows, JCL.Forms, JCL.Utils, JCL.Auxiliary, JCL.Comps.Button;

type

  { TFormQuestAsk }

  TFormQuestAsk = class(jJixForm)
    JixButCancel: Tjixbutton;
    JixButOk: Tjixbutton;
    LabelSubCapion: TLabel;
    procedure FormShow(Sender: TObject);
    Procedure Jixbutcancelclick(Sender: Tobject);
    Procedure Jixbutokclick(Sender: Tobject);
  private

  public

  end;

var
  FormQuestAsk: TFormQuestAsk;
  Used: Boolean;

Implementation
End.
