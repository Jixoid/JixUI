// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRATextFX;

{$mode objfpc}{$H+}

{
  Font rendering units : BGRAText, BGRATextFX, BGRAVectorize, BGRAFreeType

  This unit provide text effects. The simplest way to render effects is to use TBGRATextEffectFontRenderer class.
  To do this, create an instance of this class and assign it to a TBGRABitmap.FontRenderer property. Now functions
  to draw text like TBGRABitmap.TextOut will use the chosen renderer. To set the effects, keep a variable containing
  the TBGRATextEffectFontRenderer class and modify ShadowVisible and other effects parameters.

  The TBGRATextEffectFontRenderer class makes use of other classes depending on the situation. For example,
  TBGRATextEffect, which is also in this unit, provides effects on a text mask. But the renderer also uses
  BGRAVectorize unit in order to have big texts or to rotate them at will.

  Note that you may need TBGRATextEffect if you want to have more control over text effects, especially
  if you always draw the same text. Keeping the same TBGRATextEffect object will avoid creating the text
  mask over and over again.

  TextShadow function is a simple function to compute an image containing a text with shadow.

}
{$WARN 6018 off : unreachable code}
{$WARN 5025 off : Local variable "$1" not used}
interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmapTypes, BGRAPhongTypes, BGRAText,
  BGRACustomTextFX, BGRAVectorize;

type
  TBGRATextEffect = class;

  { TBGRATextEffectFontRenderer }

  TBGRATextEffectFontRenderer = class(TBGRASystemFontRenderer)
  private
    function GetShaderLightPosition: TPoint;
    function GetShaderLightPositionF: TPointF;
    function GetVectorizedRenderer: TBGRAVectorizedFontRenderer;
    procedure SetShaderLightPosition(const AValue: TPoint);
    procedure SetShaderLightPositionF(const AValue: TPointF);
  protected
    FShaderOwner: boolean;
    FShader: TCustomPhongShading;
    FVectorizedRenderer: TBGRAVectorizedFontRenderer;
    function ShadowActuallyVisible :boolean;
    function ShaderActuallyActive: boolean;
    function OutlineActuallyVisible: boolean;
    procedure Init;
    function VectorizedFontNeeded(AOrientation: integer): boolean;
    procedure InternalTextOutAngle(ADest: TBGRACustomBitmap; x, y: single; AOrientation: integer; sUTF8: string; c: TBGRAPixel; texture: IBGRAScanner;
                              align: TAlignment; AShowPrefix: boolean = false; ARightToLeft: boolean = false); override;
  public
    ShaderActive: boolean;

    ShadowVisible: boolean;
    ShadowColor: TBGRAPixel;
    ShadowRadius: integer;
    ShadowOffset: TPoint;
    ShadowQuality: TRadialBlurType;

    OutlineColor: TBGRAPixel;
    OutlineWidth: single;
    OutlineVisible,OuterOutlineOnly: boolean;
    OutlineJoin: TPenJoinStyle;
    OutlineTexture: IBGRAScanner;
    constructor Create; overload;
    constructor Create(AShader: TCustomPhongShading; AShaderOwner: boolean); overload;
    destructor Destroy; override;
    function TextVisible(const AColor: TBGRAPixel): boolean; override;
    function TextSize(sUTF8: string): TSize; overload; override;
    function TextSizeAngle(sUTF8: string; orientationTenthDegCCW: integer): TSize; override;
    function TextSize(sUTF8: string; AMaxWidth: integer; {%H-}ARightToLeft: boolean): TSize; overload; override;
    function TextFitInfo(sUTF8: string; AMaxWidth: integer): integer; override;
    property Shader: TCustomPhongShading read FShader;
    property ShaderLightPosition: TPoint read GetShaderLightPosition write SetShaderLightPosition;
    property ShaderLightPositionF: TPointF read GetShaderLightPositionF write SetShaderLightPositionF;
    property VectorizedFontRenderer: TBGRAVectorizedFontRenderer read GetVectorizedRenderer;
  end;

  { TBGRATextEffect }

  TBGRATextEffect = class(TBGRACustomTextEffect)
  protected
    procedure InitImproveReadability(AText: string; Font: TFont; SubOffsetX,SubOffsetY: single);
    procedure Init(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single; GrainX, GrainY: Integer);
    procedure InitWithFontName(AText: string; AFontName: string; AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean; SubOffsetX,SubOffsetY: single);
  public
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean); overload;
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single); overload;
    constructor Create(AText: string; Font: TFont; Antialiasing: boolean; SubOffsetX,SubOffsetY: single; GrainX, GrainY: Integer); overload;
    constructor Create(AText: string; AFontName: string; AFullHeight: integer; Antialiasing: boolean); overload;
    constructor Create(AText: string; AFontName: string; AFullHeight: integer; Antialiasing: boolean; SubOffsetX,SubOffsetY: single); overload;
    constructor Create(AText: string; AFontName: string; AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean); overload;
    constructor Create(AText: string; AFontName: string; AFullHeight: integer; AStyle: TFontStyles; Antialiasing: boolean; SubOffsetX,SubOffsetY: single); overload;
  end;

function TextShadow(AWidth,AHeight: Integer; AText: String; AFontHeight: Integer; ATextColor,AShadowColor: TBGRAPixel;
    AOffSetX,AOffSetY: Integer; ARadius: Integer = 0; AFontStyle: TFontStyles = []; AFontName: String = 'Default'; AShowText: Boolean = True; AFontQuality: TBGRAFontQuality = fqFineAntialiasing): TBGRACustomBitmap;

procedure BGRATextOutImproveReadability(bmp: TBGRACustomBitmap; AFont: TFont; xf,yf: single; text: string; color: TBGRAPixel; tex: IBGRAScanner; align: TAlignment; mode : TBGRATextOutImproveReadabilityMode);

Implementation
End.
