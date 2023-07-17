// SPDX-License-Identifier: LGPL-3.0-linking-exception
{ General framework methods for rendering background, borders, text, etc.

  originally written in 2012 by Krzysztof Dibowski dibowski at interia.pl
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCTools;
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, Types, Graphics,
  {$IFDEF FPC}LCLType, LCLIntf,{$ENDIF} {$IFNDEF FPC}BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmap, BGRABitmapTypes, bctypes, Controls, BGRAGradientScanner;

function ScaleRect(ARect: TRect; AScale: Single): TRect;
// This method prepare BGRABitmap for rendering BCFont type
procedure AssignBCFont(AFont: TBCFont; var ATargetBGRA: TBGRABitmap);
// Calculate text height and width (doesn't include wordwrap - just single line)
procedure CalculateTextSize(const AText: String; AFont: TBCFont; out ANewWidth, ANewHeight: integer;
  AShadowMargin: boolean = true);
// Calculate text height and width (handles wordwrap and end ellipsis)
procedure CalculateTextSizeEx(const AText: String; AFont: TBCFont; out ANewWidth, ANewHeight: integer;
  AAvailableWidth: integer; AShadowMargin: boolean = false);
// Determines the layout of the glyph
procedure GetGlyphActualLayout(ACaption: string; AFont: TBCFont;
  AGlyphAlignment: TBCAlignment; AGlyphMargin: integer; out AHorizAlign: TAlignment;
  out AVertAlign: TTextLayout; out AGlyphRelativeHorizAlign: TAlignment;
  out AGlyphRelativeVertAlign: TTextLayout; out AGlyphHorizMargin: integer;
  out AGlyphVertMargin: integer);
// Computes the position the glyph and update rAvail with the space dedicated to text.
// Specify the flag AOldPlacement to have the old (buggy) version
function ComputeGlyphPosition(var rAvail: TRect;
  AGlyph: TBitmap; AGlyphAlignment: TBCAlignment; AGlyphMargin: integer;
  ACaption: string; AFont: TBCFont; AOldPlacement: boolean = false;
  AGlyphScale: Single = 1): TRect; overload;
function ComputeGlyphPosition(var rAvail: TRect;
  gw, gh: integer; AGlyphAlignment: TBCAlignment; AGlyphMargin: integer;
  ACaption: string; AFont: TBCFont; AOldPlacement: boolean = false): TRect; overload;
// This method correct TRect to border width. As far as border width is bigger,
// BGRA drawing rectangle with offset (half border width)
procedure CalculateBorderRect(ABorder: TBCBorder; var ARect: TRect);
// This returns a rectangle that is inside the border outline
procedure CalculateInnerRect(ABorder: TBCBorder; var ARect: TRect);
// Create BGRA Gradient Scanner based on BCGradient properties
function CreateGradient(AGradient: TBCGradient; ARect: TRect): TBGRAGradientScanner;
// Render arrow (used by BCButton with DropDownMenu style)
procedure RenderArrow(ATargetBGRA: TBGRABitmap; const ARect: TRect;
  ASize: Integer; ADirection: TBCArrowDirection; AColor: TColor = clBlack;
  AOpacity: Byte = 255);
// Render customizable backgroud (used e.g. by TBCButton, TBCPanel, TBCLabel)
procedure RenderBackground(const ARect: TRect; ABackground: TBCBackground;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding = nil; AHasNoBorder: boolean = false);
procedure RenderBackgroundF(x1,y1,x2,y2: single; ABackground: TBCBackground;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding = nil);
procedure RenderBackgroundAndBorder(const ARect: TRect; ABackground: TBCBackground;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding; ABorder: TBCBorder; AInnerMargin: single = 0);
// Render customizable border (used e.g. by TBCButton, TBCPanel, TBCLabel)
procedure RenderBorder(const ARect: TRect; ABorder: TBCBorder;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding = nil);
procedure RenderBorderF(x1,y1,x2,y2: single; ABorder: TBCBorder;
  ATargetBGRA: TBGRABitmap; ARounding: TBCRounding = nil);
// Render BCFont (used e.g. by TBCButton, TBCPanel, TBCLabel)
procedure RenderText(const ARect: TRect; AFont: TBCFont;
  const AText: String; ATargetBGRA: TBGRABitmap; AEnabled: boolean);
// Return LCL horizontal equivalent for BCAlignment
function BCAlign2HAlign(AAlign: TBCAlignment): TAlignment;
// Return LCL vertical equivalent for BCAlignment
function BCAlign2VAlign(AAlign: TBCAlignment): TTextLayout;

Implementation
End.
