// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAThemeButton;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, Types, ExtCtrls, BGRASVGImageList;

type

  { TBGRAThemeButton }

  TBGRAThemeButton = class(TBGRAThemeControl)
  private
    FImageIndex: integer;
    FImageList: TBGRASVGImageList;
    FModalResult: TModalResult;
    FState: TBGRAThemeButtonState;
    FTimerHover: TTimer;
    procedure SetImageIndex(AValue: integer);
    procedure SetImageList(AValue: TBGRASVGImageList);
    procedure SetState(AValue: TBGRAThemeButtonState);
    procedure TimerHoverElapse(Sender: TObject);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: Boolean); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
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
    procedure UpdateHoverState;
    property State: TBGRAThemeButtonState read FState write SetState;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Action;
    property ModalResult: TModalResult
      read FModalResult write FModalResult default mrNone;
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property Enabled;
    property Font;
    property ImageList: TBGRASVGImageList read FImageList write SetImageList;
    property ImageIndex: integer read FImageIndex write SetImageIndex;
    property OnClick;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

procedure Register;

Implementation
End.
