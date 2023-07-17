// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ Common types for BGRA Controls package

  originally written in 2011 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCTypes;
{$WARN 5024 off : Parameter "$1" not used}
{$I bgracontrols.map.inc}

interface

uses
  Classes, Controls, {$IFNDEF FPC}Types, Windows, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmap, BGRABitmapTypes, Graphics, BCBasectrls;

type

  {$IFDEF FPC}
    {$IFDEF CPU64}
    BGRAPtrInt      = PtrInt;           //    Cardinal;//PtrInt;
    BGRAPtrUInt     = PtrUInt;          //    Cardinal;//PtrUInt;
    {$ELSE}
    BGRAPtrInt      = PtrInt;           //    LongInt;//PtrInt;
    BGRAPtrUInt     = PtrUInt;          //    Cardinal;//PtrUInt;
    BGRAQWord       = Int64;            //    Cardinal;//QWord;
    {$ENDIF}
    BGRAWord        = Word;                        //    Word;
    PBGRAWord       = PWord;                       //    PWord;
    BGRADWord       = DWord;          //    Cardinal;  //DWord;
    BGRALongWord    = LongWord;       //    Cardinal;  //LongWord;
    PBGRAQWord      = PQWord;         //    PCardinal; //PQWord;
    PBGRADWord      = PDWord;         //    PCardinal; //PDWord;
    PBGRALongWord   = PLongWord;      //    PCardinal; //PLongWord;
    BGRANativeInt   = NativeInt;      //    NativeInt;  //NativeInt;
    BGRANativeUInt  = NativeUInt;     //    Cardinal;  //NativeUInt;
    BGRALongInt     = LongInt;        //    Cardinal;  //LongInt;
    BGRAInt64       = Int64;                       //    Int64;
    BGRAUInt64      = Int64;                      //    UInt64;
    BGRACardinal    = Cardinal;                    //    Cardinal;
    PBGRACardinal   = PCardinal;                   //    PCardinal;

    HDC             = {$IFDEF BGRABITMAP_USE_LCL}LCLType.HDC{$ELSE}BGRAPtrUInt{$ENDIF};
    PPTrint         = ^PtrInt;
  {$ELSE}
    ValReal         = Extended;
    {$IFDEF CPU64}
    BGRAPtrInt      = Int64;
    BGRAPtrUInt     = QWord;
    {$ELSE}
    BGRAPtrInt      = LongInt;        //      LongInt;//LongInt;
    BGRAPtrUInt     = LongWord;       //      Cardinal;//LongWord;
    BGRAQWord       = Int64;          //      Cardinal;//LongWord;
    {$ENDIF}
    BGRAWord        = Word;           //                 Word;
    PBGRAWord       = PWord;          //                 PWord;
    BGRADWord       = DWord;          //      Cardinal;//DWord;
    BGRALongWord    = LongWord;       //      Cardinal;//LongWord;
    PBGRAPtrInt     = ^BGRAPtrInt;    //     PCardinal;//^BGRAPtrInt;
    PBGRAPtrUInt    = ^BGRAPtrUInt;   //     PCardinal;//^BGRAPtrUInt;
    PBGRAQWord      = ^BGRAQWord;     //     PCardinal;//^BGRAQWord;
    PBGRADWord      = PDWord;         //     PCardinal;//PDWord;
    PBGRALongWord   = PLongWord;      //     PCardinal;//PLongWord;
    BGRANativeInt   = NativeInt;      //     NativeInt;//NativeInt;
    BGRANativeUInt  = NativeUInt;     //      Cardinal;//NativeUInt;
    BGRALongInt     = LongInt;        //                 LongInt;
    BGRAInt64       = Int64;          //                 Int64;
    BGRAUInt64      = Int64;          //                 UInt64;
    BGRACardinal    = Cardinal;       //                 Cardinal;
    PBGRACardinal   = PCardinal;      //                 PCardinal;

    HDC             = Windows.HDC;    //
    PUnicodeChar    = Windows.PWChar; //
    UnicodeChar     = Windows.WCHAR;  //
(*    ValReal         = FPImage.ValReal;
    {$IFDEF CPU64}     //WORD = 2 bytes = 4 nybbles = 16 bits    for 32bits
    BGRAPtrInt      = FPImage.BGRAPtrInt;
    BGRAPtrUInt     = FPImage.BGRAPtrUInt;  //QWORD = 2 DWORDs = 4 WORDs = ….. = 64 bits       for 32bits
    {$ELSE}               //BGRADWord = 2 WORDs = 4 bytes = 8 nybbles = 32 bits   for 32bits
    BGRAPtrInt      = FPImage.BGRAPtrInt;
    BGRAPtrUInt     = FPImage.BGRAPtrUInt;
    BGRAQWord       = FPImage.BGRAQWord;
    {$ENDIF}
    BGRADWord       = FPImage.BGRADWord;
    BGRALongWord    = FPImage.BGRALongWord;
    PBGRAPtrInt     = FPImage.PBGRAPtrInt;
    PBGRAPtrUInt    = FPImage.PBGRAPtrUInt;
    PBGRAQWord      = FPImage.PBGRAQWord;
    PBGRADWord      = FPImage.PBGRADWord;
    HDC             = FPImage.HDC;
    BGRANativeInt   = FPImage.BGRANativeInt;
    PBGRALongWord   = FPImage.PBGRALongWord;

    PUnicodeChar    = FPImage.PUnicodeChar;
    UnicodeChar     = FPImage.UnicodeChar;    *)
  {$ENDIF}


  TBCMouseState = (msNone, msHover, msClicked);
  TBCAlignment = (bcaLeftTop, bcaLeftCenter, bcaLeftBottom,
    bcaCenterTop, bcaCenter, bcaCenterBottom, bcaRightTop, bcaRightCenter,
    bcaRightBottom);
  TBCBackgroundStyle = (bbsClear, bbsColor, bbsGradient);
  TBCBorderStyle = (bboNone, bboSolid);
  TBCArrowDirection = (badLeft, badRight, badUp, badDown);
  TBCStretchMode = (smNone, smShrink, smStretch, smCover);
  TBCCanvasScaleMode = (csmAuto, csmScaleBitmap, csmFullResolution);
  TBGRATextAlign = (btaLeft, btaCenter, btaRight); // deprecated
  TBGRATextVAlign = (btvaTop, btvaCenter, btvaBottom); // deprecated
  TBGRARedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap) of object;

type
  { TBCGradient }

  TBCGradient = class(TBCProperty)
  private
    FColorCorrection: boolean;
    FDrawMode: TDrawMode;
    FGradientType: TGradientType;
    FEndColor: TColor;
    FEndColorOpacity: byte;
    FPoint1XPercent: single;
    FPoint1YPercent: single;
    FPoint2XPercent: single;
    FPoint2YPercent: single;
    FSinus: boolean;
    FStartColor: TColor;
    FStartColorOpacity: byte;
    procedure SetColorCorrection(const AValue: boolean);
    procedure SetDrawMode(const AValue: TDrawMode);
    procedure SetEndColor(const AValue: TColor);
    procedure SetEndColorOpacity(const AValue: byte);
    procedure SetGradientType(const AValue: TGradientType);
    procedure SetPoint1XPercent(const AValue: single);
    procedure SetPoint1YPercent(const AValue: single);
    procedure SetPoint2XPercent(const AValue: single);
    procedure SetPoint2YPercent(const AValue: single);
    procedure SetSinus(const AValue: boolean);
    procedure SetStartColor(const AValue: TColor);
    procedure SetStartColorOpacity(const AValue: byte);
  public
    constructor Create(AControl: TControl); override;

    procedure Assign(Source: TPersistent); override;
    procedure Scale(AScale: single);
  published
    property StartColor: TColor read FStartColor write SetStartColor;
    property StartColorOpacity: byte read FStartColorOpacity write SetStartColorOpacity default 255;
    property DrawMode: TDrawMode read FDrawMode write SetDrawMode default dmSet;
    property EndColor: TColor read FEndColor write SetEndColor;
    property EndColorOpacity: byte read FEndColorOpacity write SetEndColorOpacity default 255;
    property ColorCorrection: boolean read FColorCorrection write SetColorCorrection default true;
    property GradientType: TGradientType read FGradientType write SetGradientType;
    property Point1XPercent: single read FPoint1XPercent write SetPoint1XPercent default EmptySingle;
    property Point1YPercent: single read FPoint1YPercent write SetPoint1YPercent default EmptySingle;
    property Point2XPercent: single read FPoint2XPercent write SetPoint2XPercent default EmptySingle;
    property Point2YPercent: single read FPoint2YPercent write SetPoint2YPercent default EmptySingle;
    property Sinus: boolean read FSinus write SetSinus default false;
  end;

  { TBCFont }

  TBCFont = class(TBCProperty)
  private
    FColor, FDisabledColor: TColor;
    FEndEllipsis: boolean;
    FFontQuality: TBGRAFontQuality;
    FHeight: integer;
    FName: string;
    FPaddingBottom: integer;
    FPaddingLeft: integer;
    FPaddingRight: integer;
    FPaddingTop: integer;
    FShadow: boolean;
    FShadowColor: TColor;
    FShadowColorOpacity: byte;
    FShadowOffsetX: shortint;
    FShadowOffsetY: shortint;
    FShadowRadius: byte;
    FSingleLine: boolean;
    FStyle: TFontStyles;
    FTextAlignment: TBCAlignment;
    FWordBreak: boolean;
    function IsNameStored: boolean;
    procedure SetColor(AValue: TColor);
    procedure SetDisabledColor(AValue: TColor);
    procedure SetEndEllipsis(AValue: boolean);
    procedure SetFontQuality(AValue: TBGRAFontQuality);
    procedure SetHeight(AValue: integer);
    procedure SetName(AValue: string);
    procedure SetPaddingBottom(AValue: integer);
    procedure SetPaddingLeft(AValue: integer);
    procedure SetPaddingRight(AValue: integer);
    procedure SetPaddingTop(AValue: integer);
    procedure SetShadow(AValue: boolean);
    procedure SetShadowColor(AValue: TColor);
    procedure SetShadowColorOpacity(AValue: byte);
    procedure SetShadowOffsetX(AValue: shortint);
    procedure SetShadowOffsetY(AValue: shortint);
    procedure SetShadowRadius(AValue: byte);
    procedure SetSingleLine(AValue: boolean);
    procedure SetStyle(AValue: TFontStyles);
    procedure SetTextAlignment(AValue: TBCAlignment);
    procedure SetWordBreak(AValue: boolean);
  public
    constructor Create(AControl: TControl); override;
    procedure Assign(Source: TPersistent); override;
    procedure Scale(AScale: single; APreserveDefaultHeight: boolean = true);
  published
    property Color: TColor read FColor write SetColor;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clNone;
    property EndEllipsis: boolean read FEndEllipsis write SetEndEllipsis default false;
    property FontQuality: TBGRAFontQuality read FFontQuality write SetFontQuality;
    property Height: integer read FHeight write SetHeight default 0;
    property Name: string read FName write SetName stored IsNameStored;
    property SingleLine: boolean read FSingleLine write SetSingleLine default true;
    property Shadow: boolean read FShadow write SetShadow;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBlack;
    property ShadowColorOpacity: byte read FShadowColorOpacity
      write SetShadowColorOpacity default 255;
    property ShadowRadius: byte read FShadowRadius write SetShadowRadius;
    property ShadowOffsetX: shortint read FShadowOffsetX write SetShadowOffsetX;
    property ShadowOffsetY: shortint read FShadowOffsetY write SetShadowOffsetY;
    property Style: TFontStyles read FStyle write SetStyle;
    property TextAlignment: TBCAlignment read FTextAlignment write SetTextAlignment default bcaCenter;
    property WordBreak: boolean read FWordBreak write SetWordBreak default false;
    property PaddingLeft: integer read FPaddingLeft write SetPaddingLeft default 0;
    property PaddingRight: integer read FPaddingRight write SetPaddingRight default 0;
    property PaddingTop: integer read FPaddingTop write SetPaddingTop default 0;
    property PaddingBottom: integer read FPaddingBottom write SetPaddingBottom default 0;
  end;

  { TBCBackground }

  TBCBackground = class(TBCProperty)
  private
    FColor: TColor;
    FColorOpacity: byte;
    FGradient1: TBCGradient;
    FGradient1EndPercent: single;
    FGradient2: TBCGradient;
    FStyle: TBCBackgroundStyle;
    procedure OnChangeChildProperty({%H-}Sender: TObject; AData: PtrInt);
    procedure SetColor(AValue: TColor);
    procedure SetColorOpacity(AValue: byte);
    procedure SetGradient1(AValue: TBCGradient);
    procedure SetGradient1EndPercent(AValue: single);
    procedure SetGradient2(AValue: TBCGradient);
    procedure SetStyle(AValue: TBCBackgroundStyle);
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Scale(AScale: single);
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property ColorOpacity: byte read FColorOpacity write SetColorOpacity default 255;
    property Gradient1: TBCGradient read FGradient1 write SetGradient1;
    property Gradient2: TBCGradient read FGradient2 write SetGradient2;
    property Gradient1EndPercent: single read FGradient1EndPercent write SetGradient1EndPercent;
    property Style: TBCBackgroundStyle read FStyle write SetStyle;
  end;

  { TBCBorder }

  TBCBorder = class(TBCProperty)
  private
    FColor: TColor;
    FColorOpacity: byte;
    FLightColor: TColor;
    FLightOpacity: byte;
    FLightWidth: integer;
    FStyle: TBCBorderStyle;
    FWidth: integer;
    procedure SetColor(AValue: TColor);
    procedure SetColorOpacity(AValue: byte);
    procedure SetLightColor(AValue: TColor);
    procedure SetLightOpacity(AValue: byte);
    procedure SetLightWidth(AValue: integer);
    procedure SetStyle(AValue: TBCBorderStyle);
    procedure SetWidth(AValue: integer);
  public
    constructor Create(AControl: TControl); override;
    procedure Assign(Source: TPersistent); override;
    procedure Scale(AScale: single);
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property ColorOpacity: byte read FColorOpacity write SetColorOpacity default 255;
    property LightColor: TColor read FLightColor write SetLightColor default clWhite;
    property LightOpacity: byte read FLightOpacity write SetLightOpacity default 255;
    property LightWidth: integer read FLightWidth write SetLightWidth default 0;
    property Style: TBCBorderStyle read FStyle write SetStyle;
    property Width: integer read FWidth write SetWidth default 1;
  end;

  { TBCRounding }

  TBCRounding = class(TBCProperty)
  private
    FRoundOptions: TRoundRectangleOptions;
    FRoundX: byte;
    FRoundY: byte;
    procedure SetRoundOptions(AValue: TRoundRectangleOptions);
    procedure SetRoundX(AValue: byte);
    procedure SetRoundY(AValue: byte);
  public
    constructor Create(AControl: TControl); override;
    procedure Assign(Source: TPersistent); override;
    procedure Scale(AScale: single);
  published
    property RoundX: byte read FRoundX write SetRoundX;
    property RoundY: byte read FRoundY write SetRoundY;
    property RoundOptions: TRoundRectangleOptions
      read FRoundOptions write SetRoundOptions default [];
  end;

  { TBCPixel }

  TBCPixel = class(TBCProperty)
  private
    FPixel: TBGRAPixel;
  public
    { Constructor }
    constructor Create(AControl: TControl); overload; override;
    constructor Create(AControl: TControl; APixel: TBGRAPixel); overload;
    constructor Create(AControl: TControl; AColor: TColor); overload;
    { Assign values to Pixel }
    procedure Assign(Source: TPersistent); overload;  override;
    procedure Assign(Source: TBGRAPixel); overload;
    procedure Assign(Source: TColor; Opacity: byte = 255);overload;
    procedure Assign(Source: string); overload;
    { Read values }
    property Pixel: TBGRAPixel read FPixel write FPixel;
    function Color: TColor;
    function Hex: string;
    { Color functions }
    procedure ApplyLightness(lightness: word);
    procedure ApplyIntensity(lightness: longword);
    procedure ToGrayscale;
  published
    { Streaming }
    property Red: byte read FPixel.red write FPixel.red;
    property Green: byte read FPixel.green write FPixel.green;
    property Blue: byte read FPixel.blue write FPixel.blue;
    property Alpha: byte read FPixel.alpha write FPixel.alpha;
  end;

{const
  DEF_START_COL      = $00EFE6D2;
  DEF_END_COL        = $00C87511;
  DEF_BORD_COL       = $00AB713B;
  DEF_BORD_COL_HOVER = $00D7B697;
  DEF_FONT_COLOR     = $0072412A; }

Implementation
End.
