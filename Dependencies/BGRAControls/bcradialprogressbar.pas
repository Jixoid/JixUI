// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCRadialProgressBar;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF} Forms, Controls, Graphics, Dialogs, BCBaseCtrls,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmap, BGRABitmapTypes, BGRATextFX;

type

  { TBCRadialProgressBar }

  TBCRadialProgressBar = class(TBCGraphicControl)
  private
    FDrawText: boolean;
    { Private declarations }
    FMaxValue: integer;
    FMinValue: integer;
    FRotation: single;
    FValue: integer;
    FBitmap: TBGRABitmap;
    FLineColor: TColor;
    FLineBkgColor: TColor;
    FFontShadowColor: TColor;
    FFontShadowOffsetX: integer;
    FFontShadowOffsetY: integer;
    FFontShadowRadius: integer;
    FLineWidth: single;
    procedure SetDrawText(AValue: boolean);
    procedure SetFFontShadowColor(AValue: TColor);
    procedure SetFFontShadowOffsetX(AValue: integer);
    procedure SetFFontShadowOffsetY(AValue: integer);
    procedure SetFFontShadowRadius(AValue: integer);
    procedure SetFLineBkgColor(AValue: TColor);
    procedure SetFLineColor(AValue: TColor);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetRotation(AValue: single);
    procedure SetValue(AValue: integer);
    procedure SetLineWidth(AValue: single);
  protected
    { Protected declarations }
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: boolean); override;
    procedure DrawControl; override;
    procedure RenderControl; override;
    procedure SetColor(Value: TColor); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Rotation: single read FRotation write SetRotation default 0;
    property DrawText: boolean read FDrawText write SetDrawText default true;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property MinValue: integer read FMinValue write SetMinValue default 0;
    property MaxValue: integer read FMaxValue write SetMaxValue default 100;
    property Value: integer read FValue write SetValue default 0;
    property OnClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelUp;
    property OnMouseWheelDown;
    property Color default clWhite;
    property LineColor: TColor read FLineColor write SetFLineColor default clBlack;
    property LineBkgColor: TColor read FLineBkgColor write SetFLineBkgColor default
      clSilver;
    property LineWidth: single read FLineWidth write SetLineWidth {$IFDEF FPC}default 4{$ENDIF};
    property FontShadowColor: TColor read FFontShadowColor
      write SetFFontShadowColor default clBlack;
    property FontShadowOffsetX: integer read FFontShadowOffsetX
      write SetFFontShadowOffsetX default 2;
    property FontShadowOffsetY: integer read FFontShadowOffsetY
      write SetFFontShadowOffsetY default 2;
    property FontShadowRadius: integer read FFontSHadowRadius
      write SetFFontShadowRadius default 4;
    property Font;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.
