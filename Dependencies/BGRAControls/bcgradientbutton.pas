// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BCGradientButton;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes, BCTypes;

type

  { TBCGradientButton }

  TBCGradientButton = class(TGraphicControl)
  private
    FBorderColor: TBCPixel;
    FBorderSize: integer;
    FColor1: TBCPixel;
    FColor2: TBCPixel;
    FDimColor: TBCPixel;
    FLockHorizontal: boolean;
    FLockVertical: boolean;
    FOnAfterRedraw: TBGRARedrawEvent;
    FOnBeforeRedraw: TBGRARedrawEvent;
    Fx: integer;
    Fy: integer;
    Fdraw: boolean;
    Fupdating: boolean;
    Fdown: boolean;
    procedure ColorInvalidate({%H-}ASender: TObject; {%H-}AData: PtrInt);
    procedure SetBorderColor(AValue: TBCPixel);
    procedure SetBorderSize(AValue: integer);
    procedure SetColor1(AValue: TBCPixel);
    procedure SetColor2(AValue: TBCPixel);
    procedure SetDimColor(AValue: TBCPixel);
    procedure SetLockHorizontal(AValue: boolean);
    procedure SetLockVertical(AValue: boolean);
  protected
    procedure Paint; override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseLeave; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure BeginUpdate;
    procedure EndUpdate;
  published
    property LockHorizontal: boolean read FLockHorizontal
      write SetLockHorizontal default False;
    property LockVertical: boolean
      read FLockVertical write SetLockVertical default False;
    property DimColor: TBCPixel read FDimColor write SetDimColor;
    property Color1: TBCPixel read FColor1 write SetColor1;
    property Color2: TBCPixel read FColor2 write SetColor2;
    property BorderColor: TBCPixel read FBorderColor write SetBorderColor;
    property BorderSize: integer read FBorderSize write SetBorderSize;
    property OnBeforeRedraw: TBGRARedrawEvent read FOnBeforeRedraw write FOnBeforeRedraw;
    property OnAfterRedraw: TBGRARedrawEvent read FOnAfterRedraw write FOnAfterRedraw;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Caption;
    property Enabled;
    property ShowHint;
  end;

procedure Register;

Implementation
End.
