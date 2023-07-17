// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAFontGL;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRAOpenGLType, BGRABitmapTypes,
  Avl_Tree;

type
  { TRenderedGlyph }

  TRenderedGlyph = class
  private
    FIdentifier: UTF8String;
    FTexture: IBGLTexture;
    FHorizontalOverflowPx, FVerticalOverflowPx, FAdvancePx: integer;
  public
    constructor Create(AIdentifier: UTF8String; ATexture: IBGLTexture;
      AHorizontalOverflowPx, AVerticalOverflowPx: integer);
    procedure Draw(x,y,Scale: single; AColor: TBGRAPixel); overload;
    procedure Draw(x,y,Scale: single; AGradTopLeft, AGradTopRight, AGradBottomRight, AGradBottomLeft: TBGRAPixel); overload;
    property Identifier: UTF8String read FIdentifier;
    property AdvancePx: integer read FAdvancePx;
  end;

  { IBGLRenderedFont }

  IBGLRenderedFont = interface(IBGLFont)
    function GetBackgroundColor: TBGRAPixel;
    function GetColor: TBGRAPixel;
    function GetFontEmHeight: integer;
    function GetFontFullHeight: integer;
    function GetHorizontalOverflow: single;
    function GetName: string;
    function GetQuality: TBGRAFontQuality;
    function GetStyle: TFontStyles;
    function GetVerticalOverflow: single;
    procedure SetBackgroundColor(AValue: TBGRAPixel);
    procedure SetColor(AValue: TBGRAPixel);
    procedure SetFontEmHeight(AValue: integer);
    procedure SetFontFullHeight(AValue: integer);
    procedure SetHorizontalOverflow(AValue: single);
    procedure SetName(AValue: string);
    procedure SetQuality(AValue: TBGRAFontQuality);
    procedure SetStyle(AValue: TFontStyles);
    procedure SetVerticalOverflow(AValue: single);

    property Name: string read GetName write SetName;
    property Style: TFontStyles read GetStyle write SetStyle;
    property Quality: TBGRAFontQuality read GetQuality write SetQuality;
    property EmHeight: integer read GetFontEmHeight write SetFontEmHeight;
    property FullHeight: integer read GetFontFullHeight write SetFontFullHeight;
    property Color: TBGRAPixel read GetColor write SetColor;
    property HorizontalOverflow: single read GetHorizontalOverflow write SetHorizontalOverflow;
    property VerticalOverflow: single read GetVerticalOverflow write SetVerticalOverflow;
    property BackgroundColor: TBGRAPixel read GetBackgroundColor write SetBackgroundColor;
  end;

  { TBGLRenderedFont }

  TBGLRenderedFont = class(TBGLCustomFont,IBGLRenderedFont)
  private
    FGlyphs: TAVLTree;

    FName: string;
    FColor: TBGRAPixel;
    FBackgroundColor: TBGRAPixel;
    FEmHeight: integer;
    FHorizontalOverflow: single;
    FVerticalOverflow: single;
    FQuality: TBGRAFontQuality;
    FStyle: TFontStyles;
    FGradTopLeft, FGradTopRight, FGradBottomRight, FGradBottomLeft: TBGRAPixel;
    FUseGradientColor: boolean;
    FClipped: boolean;
    FWordBreakHandler: TWordBreakHandler;

    function FindGlyph(AIdentifier: string): TAVLTreeNode;
    function GetBackgroundColor: TBGRAPixel;
    function GetColor: TBGRAPixel;
    function GetFontEmHeight: integer;
    function GetGlyph(AIdentifier: string): TRenderedGlyph;
    function GetHorizontalOverflow: single;
    function GetName: string;
    function GetQuality: TBGRAFontQuality;
    function GetStyle: TFontStyles;
    function GetVerticalOverflow: single;
    procedure SetGlyph(AIdentifier: string; AValue: TRenderedGlyph);
    function GetFontFullHeight: integer;
    procedure SetBackgroundColor(AValue: TBGRAPixel);
    procedure SetColor(AValue: TBGRAPixel);
    procedure SetFontEmHeight(AValue: integer);
    procedure SetFontFullHeight(AValue: integer);
    procedure SetHorizontalOverflow(AValue: single);
    procedure SetName(AValue: string);
    procedure SetQuality(AValue: TBGRAFontQuality);
    procedure SetStyle(AValue: TFontStyles);
    procedure SetVerticalOverflow(AValue: single);
  protected
    FRenderer: TBGRACustomFontRenderer;
    FRendererOwned: boolean;
    function LoadFromFile({%H-}AFilename: UTF8String): boolean; override;
    procedure FreeMemoryOnDestroy; override;
    function CreateGlyph(AIdentifier: string): TRenderedGlyph; virtual;
    procedure CopyFontToRenderer; virtual;
    procedure DoTextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel; AHorizontalAlign: TAlignment; AVerticalAlign: TTextLayout); overload; virtual;
    procedure DoTextOut(X, Y: Single; const Text : UTF8String; AColor: TBGRAPixel); overload; override;
    procedure DoTextRect(X, Y, Width, Height: Single; const Text : UTF8String; AColor: TBGRAPixel); override;
    function GetClipped: boolean; override;
    function GetUseGradientColors: boolean; override;
    procedure SetClipped(AValue: boolean); override;
    procedure SetUseGradientColors(AValue: boolean); override;
    procedure DiscardGlyphs; virtual;
    procedure DefaultWordBreakHandler(var ABefore, AAfter: string);
    procedure SplitText(var ATextUTF8: string; AMaxWidth: single; out ARemainsUTF8: string);
    function GetWrappedLines(ATextUTF8: string; AWidth: single): TStringList;
  public
    constructor Create(ARenderer: TBGRACustomFontRenderer; ARendererOwned: boolean = true);
    procedure FreeMemory; override;
    function TextWidth(const Text: UTF8String): single; override;
    function TextHeight(const {%H-}Text: UTF8String): single; override;
    function TextHeight(const Text: UTF8String; AWidth: single): single; override;
    procedure SetGradientColors(ATopLeft, ATopRight, ABottomRight, ABottomLeft: TBGRAPixel); override;
    property Name: string read GetName write SetName;
    property Style: TFontStyles read GetStyle write SetStyle;
    property Quality: TBGRAFontQuality read GetQuality write SetQuality;
    property EmHeight: integer read GetFontEmHeight write SetFontEmHeight;
    property FullHeight: integer read GetFontFullHeight write SetFontFullHeight;
    property Color: TBGRAPixel read GetColor write SetColor;
    property HorizontalOverflow: single read GetHorizontalOverflow write SetHorizontalOverflow;
    property VerticalOverflow: single read GetVerticalOverflow write SetVerticalOverflow;
    property BackgroundColor: TBGRAPixel read GetBackgroundColor write SetBackgroundColor;
    property WordBreakHandler: TWordBreakHandler read FWordBreakHandler write FWordBreakHandler;
    property Glyph[AIdentifier: string]: TRenderedGlyph read GetGlyph;
  end;

Implementation
End.
