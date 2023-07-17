// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCMaterialDesignButton;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, {$IFDEF FPC}LResources,{$ENDIF}
  Types, Controls, Graphics, ExtCtrls, BCBaseCtrls, BGRABitmap, BGRABitmapTypes;

type

  { TBCMaterialDesignButton }

  TBCMaterialDesignButton = class(TBGRAGraphicCtrl)
  private
    FNormalColor: TColor;
    FNormalColorEffect: TColor;
    FRoundBorders: single;
    FShadow: boolean;
    FShadowColor: TColor;
    FShadowSize: integer;
    FTextColor: TColor;
    FTextFont: string;
    FTextQuality: TBGRAFontQuality;
    FTextShadow: boolean;
    FTextShadowColor: TColor;
    FTextShadowOffsetX: integer;
    FTextShadowOffsetY: integer;
    FTextShadowSize: integer;
    FTextSize: integer;
    FTextStyle: TFontStyles;
    FTimer: TTimer;
    FBGRA: TBGRABitmap;
    FBGRAShadow: TBGRABitmap;
    FMousePos: TPoint;
    FCircleSize: single;
    FCircleAlpha: byte;
    procedure SetFNormalColor(AValue: TColor);
    procedure SetFNormalColorEffect(AValue: TColor);
    procedure SetFRoundBorders(AValue: single);
    procedure SetFShadow(AValue: boolean);
    procedure SetFShadowColor(AValue: TColor);
    procedure SetFShadowSize(AValue: integer);
    procedure SetFTextColor(AValue: TColor);
    procedure SetFTextFont(AValue: string);
    procedure SetFTextQuality(AValue: TBGRAFontQuality);
    procedure SetFTextShadow(AValue: boolean);
    procedure SetFTextShadowColor(AValue: TColor);
    procedure SetFTextShadowOffsetX(AValue: integer);
    procedure SetFTextShadowOffsetY(AValue: integer);
    procedure SetFTextShadowSize(AValue: integer);
    procedure SetFTextSize(AValue: integer);
    procedure SetFTextStyle(AValue: TFontStyles);
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
    {%H-}WithThemeSpace: boolean); override;
    procedure OnStartTimer({%H-}Sender: TObject);
    procedure OnTimer({%H-}Sender: TObject);
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure TextChanged; override;
    procedure UpdateShadow;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RoundBorders: single read FRoundBorders write SetFRoundBorders {$IFDEF FPC}default 5{$ENDIF};
    property NormalColor: TColor read FNormalColor write SetFNormalColor default clWhite;
    property NormalColorEffect: TColor read FNormalColorEffect
      write SetFNormalColorEffect default clSilver;
    property Shadow: boolean read FShadow write SetFShadow default True;
    property ShadowColor: TColor read FShadowColor write SetFShadowColor default clGray;
    property ShadowSize: integer read FShadowSize write SetFShadowSize default 5;
    property TextColor: TColor read FTextColor write SetFTextColor default clBlack;
    property TextSize: integer read FTextSize write SetFTextSize default 16;
    property TextShadow: boolean read FTextShadow write SetFTextShadow default True;
    property TextShadowColor: TColor read FTextShadowColor
      write SetFTextShadowColor default clBlack;
    property TextShadowSize: integer read FTextShadowSize
      write SetFTextShadowSize default 2;
    property TextShadowOffsetX: integer read FTextShadowOffsetX
      write SetFTextShadowOffsetX default 0;
    property TextShadowOffsetY: integer read FTextShadowOffsetY
      write SetFTextShadowOffsetY default 0;
    property TextStyle: TFontStyles read FTextStyle write SetFTextStyle default [];
    property TextFont: string read FTextFont write SetFTextFont;
    property TextQuality: TBGRAFontQuality read FTextQuality
      write SetFTextQuality default fqFineAntialiasing;
  published
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    {$IFDEF FPC} //#
    property OnChangeBounds;
    {$ENDIF}
    property Caption;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDrag;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.
