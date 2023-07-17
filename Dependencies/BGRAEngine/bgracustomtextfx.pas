// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRACustomTextFX;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, BGRAPhongTypes, BGRAGrayscaleMask;

const DefaultOutlineWidth = 3;

type
  { TBGRACustomTextEffect }

  TBGRACustomTextEffect = class
  private
    function GetBounds: TRect;
    function GetMaskHeight: integer;
    class function GetOutlineWidth: integer; static;
    function GetShadowBounds(ARadius: integer): TRect;
    function GetMaskWidth: integer;
    function GetTextHeight: integer;
    function GetTextWidth: integer;
    procedure SetShadowQuality(AValue: TRadialBlurType);
  protected
    FShadowQuality: TRadialBlurType;
    FTextMask: TGrayscaleMask;
    FShadowRadius: integer;
    FOutlineMask, FShadowMask : TGrayscaleMask;
    FShadingMask: TBGRACustomBitmap;
    FShadingAltitude: integer;
    FShadingRounded: boolean;
    FTextSize: TSize;
    FOffset: TPoint;
    function DrawMaskMulticolored(ADest: TBGRACustomBitmap; AMask: TCustomUniversalBitmap; X,Y: Integer; const AColors: array of TBGRAPixel): TRect;
    function DrawMask(ADest: TBGRACustomBitmap; AMask: TCustomUniversalBitmap; X,Y: Integer; AColor: TBGRAPixel): TRect; overload;
    function DrawMask(ADest: TBGRACustomBitmap; AMask: TCustomUniversalBitmap; X,Y: Integer; ATexture: IBGRAScanner): TRect; overload;
    function InternalDrawShaded(ADest: TBGRACustomBitmap; X,Y: integer; Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel; ATexture: IBGRAScanner; ARounded: Boolean): TRect;
    procedure Init(AMask: TGrayscaleMask; AMaskOwner: boolean; AWidth,AHeight: integer; AOffset: TPoint);
  public
    constructor Create(AMask: TBGRACustomBitmap; AMaskOwner: boolean; AWidth,AHeight: integer; AOffset: TPoint);
    constructor Create(AMask: TGrayscaleMask; AMaskOwner: boolean; AWidth,AHeight: integer; AOffset: TPoint);
    procedure ApplySphere;
    procedure ApplyVerticalCylinder;
    procedure ApplyHorizontalCylinder;
    function Draw(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel): TRect; overload;
    function Draw(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner): TRect; overload;
    function Draw(ADest: TBGRACustomBitmap; X, Y: integer; AColor: TBGRAPixel; AAlign: TAlignment): TRect; overload;
    function Draw(ADest: TBGRACustomBitmap; X, Y: integer; ATexture: IBGRAScanner; AAlign: TAlignment): TRect; overload;

    function DrawShaded(ADest: TBGRACustomBitmap; X,Y: integer; Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel; ARounded: Boolean = true): TRect; overload;
    function DrawShaded(ADest: TBGRACustomBitmap; X,Y: integer; Shader: TCustomPhongShading; Altitude: integer; ATexture: IBGRAScanner; ARounded: Boolean = true): TRect; overload;
    function DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer; Shader: TCustomPhongShading; Altitude: integer; AColor: TBGRAPixel; AAlign: TAlignment; ARounded: Boolean = true): TRect; overload;
    function DrawShaded(ADest: TBGRACustomBitmap; X, Y: integer; Shader: TCustomPhongShading; Altitude: integer; ATexture: IBGRAScanner; AAlign: TAlignment; ARounded: Boolean = true): TRect; overload;

    function DrawMulticolored(ADest: TBGRACustomBitmap; X,Y: integer; const AColors: array of TBGRAPixel): TRect; overload;
    function DrawMulticolored(ADest: TBGRACustomBitmap; X,Y: integer; const AColors: array of TBGRAPixel; AAlign: TAlignment): TRect; overload;
    function DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel): TRect; overload;
    function DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner): TRect; overload;
    function DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; AColor: TBGRAPixel; AAlign: TAlignment): TRect; overload;
    function DrawOutline(ADest: TBGRACustomBitmap; X,Y: integer; ATexture: IBGRAScanner; AAlign: TAlignment): TRect; overload;
    function DrawShadow(ADest: TBGRACustomBitmap; X,Y,Radius: integer; AColor: TBGRAPixel): TRect; overload;
    function DrawShadow(ADest: TBGRACustomBitmap; X,Y,Radius: integer; AColor: TBGRAPixel; AAlign: TAlignment): TRect; overload;
    destructor Destroy; override;
    property TextMask: TGrayscaleMask read FTextMask;
    property TextMaskOffset: TPoint read FOffset;
    property Width: integer read GetTextWidth; deprecated;
    property Height: integer read GetTextHeight; deprecated;
    property MaskWidth: integer read GetMaskWidth;
    property MaskHeight: integer read GetMaskHeight;
    property TextSize: TSize read FTextSize;
    property TextWidth: integer read GetTextWidth;
    property TextHeight: integer read GetTextHeight;
    property Bounds: TRect read GetBounds;
    property ShadowBounds[ARadius: integer]: TRect read GetShadowBounds;
    property ShadowQuality: TRadialBlurType read FShadowQuality write SetShadowQuality;
    class property OutlineWidth: integer read GetOutlineWidth;
  end;

Implementation
End.
