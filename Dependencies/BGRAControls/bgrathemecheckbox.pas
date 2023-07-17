// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAThemeCheckBox;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, Types, LMessages, LCLType;

type

  { TBGRAThemeCheckBox }

  TBGRAThemeCheckBox = class(TBGRAThemeControl)
  private
    FChecked: boolean;
    FOnChange: TNotifyEvent;
    FState: TBGRAThemeButtonState;
    procedure SetChecked(AValue: boolean);
  protected
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    procedure WMSetFocus(var Message: {$IFDEF FPC}TLMSetFocus{$ELSE}TWMSetFocus{$ENDIF}); message {$IFDEF FPC}LM_SETFOCUS{$ELSE}WM_SETFOCUS{$ENDIF};
    procedure WMKillFocus(var Message: {$IFDEF FPC}TLMKillFocus{$ELSE}TWMKillFocus{$ENDIF}); message {$IFDEF FPC}LM_KILLFOCUS{$ELSE}WM_KILLFOCUS{$ENDIF};
    procedure UpdateFocus(AFocused: boolean);
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
    property TabStop;
    property TabOrder;
  end;

procedure Register;

Implementation
End.
