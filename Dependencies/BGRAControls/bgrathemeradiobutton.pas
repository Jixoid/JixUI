// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAThemeRadioButton;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, Types;

type

  { TBGRAThemeRadioButton }

  TBGRAThemeRadioButton = class(TBGRAThemeControl)
  private
    FChecked: boolean;
    FOnChange: TNotifyEvent;
    FState: TBGRAThemeButtonState;
    procedure SetChecked(AValue: boolean);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure Click; override;
    procedure SetEnabled(Value: boolean); override;
    procedure TextChanged; override;
    procedure Paint; override;
    procedure Resize; override;
    procedure UncheckOthers;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property Checked: boolean read FChecked write SetChecked;
    property Font;
    property Enabled;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

procedure Register;

Implementation
End.
