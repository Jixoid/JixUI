unit BCMaterialEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls;

type

  { TBCMaterialEdit }

  TBCMaterialEdit = class(TCustomPanel)
  private
    FAccentColor: TColor;
    FDisabledColor: TColor;
    Flbl: TLabel;
    Fedt: TEdit;
    Ffocused: boolean;
    FOnChange: TNotifyEvent;
    FTexto: string;
    procedure ChangeEdit(Sender: TObject);
    procedure EnterEdit(Sender: TObject);
    procedure ExitEdit(Sender: TObject);
    procedure SetTexto(AValue: string);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color;
    property Text: string read FTexto write SetTexto;
    property Edit: TEdit read Fedt;
    property Title: TLabel read Flbl;
    property DisabledColor: TColor read FDisabledColor write FDisabledColor;
    property AccentColor: TColor read FAccentColor write FAccentColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

Implementation
End.
