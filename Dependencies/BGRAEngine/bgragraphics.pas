// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAGraphics;
{=== Types imported from Graphics ===}
{$mode objfpc}{$H+}
{$I bgrabitmap.map.inc}

interface

{$IFDEF BGRABITMAP_USE_LCL}
uses Graphics, GraphType, FPImage, FPCanvas;

type
  PColor = Graphics.PColor;
  TColor = Graphics.TColor;
  TAntialiasingMode = Graphics.TAntialiasingMode;
  TGradientDirection = Graphics.TGradientDirection;
  TPenEndCap = Graphics.TPenEndCap;
  TPenJoinStyle = Graphics.TPenJoinStyle;
  TPenStyle = Graphics.TPenStyle;
  TPenMode = Graphics.TPenMode;

const
  amDontCare = Graphics.amDontCare;
  amOn = Graphics.amOn;
  amOff = Graphics.amOff;

  gdVertical = Graphics.gdVertical;
  gdHorizontal = Graphics.gdHorizontal;

  pecRound = Graphics.pecRound;
  pecSquare = Graphics.pecSquare;
  pecFlat = Graphics.pecFlat;

  pjsRound = Graphics.pjsRound;
  pjsBevel = Graphics.pjsBevel;
  pjsMiter = Graphics.pjsMiter;

  psSolid = Graphics.psSolid;
  psDash = Graphics.psDash;
  psDot = Graphics.psDot;
  psDashDot = Graphics.psDashDot;
  psDashDotDot = Graphics.psDashDotDot;
  psClear = Graphics.psClear;
  psInsideframe = Graphics.psInsideframe;
  psPattern = Graphics.psPattern;

  pmBlack = Graphics.pmBlack;
  pmWhite = Graphics.pmWhite;
  pmNop = Graphics.pmNop;
  pmNot = Graphics.pmNot;
  pmCopy = Graphics.pmCopy;
  pmNotCopy = Graphics.pmNotCopy;
  pmMergePenNot = Graphics.pmMergePenNot;
  pmMaskPenNot = Graphics.pmMaskPenNot;
  pmMergeNotPen = Graphics.pmMergeNotPen;
  pmMaskNotPen = Graphics.pmMaskNotPen;
  pmMerge = Graphics.pmMerge;
  pmNotMerge = Graphics.pmNotMerge;
  pmMask = Graphics.pmMask;
  pmNotMask = Graphics.pmNotMask;
  pmXor = Graphics.pmXor;
  pmNotXor = Graphics.pmNotXor;

  tmAuto = Graphics.tmAuto;
  tmFixed = Graphics.tmFixed;

type
  TPen = Graphics.TPen;
  TTextLayout = Graphics.TTextLayout;
  TTextStyle = Graphics.TTextStyle;

  TFillStyle = Graphics.TFillStyle;
  TFillMode = Graphics.TFillMode;
  TBrushStyle = Graphics.TBrushStyle;

const
  tlTop = Graphics.tlTop;
  tlCenter = Graphics.tlCenter;
  tlBottom = Graphics.tlBottom;

  fsSurface = GraphType.fsSurface;
  fsBorder = GraphType.fsBorder;

  fmAlternate = Graphics.fmAlternate;
  fmWinding = Graphics.fmWinding;

  bsSolid = Graphics.bsSolid;
  bsClear = Graphics.bsClear;
  bsHorizontal = Graphics.bsHorizontal;
  bsVertical = Graphics.bsVertical;
  bsFDiagonal = Graphics.bsFDiagonal;
  bsBDiagonal = Graphics.bsBDiagonal;
  bsCross = Graphics.bsCross;
  bsDiagCross = Graphics.bsDiagCross;
  bsImage = FPCanvas.bsImage;

type
  TBrush = Graphics.TBrush;
  TCanvas = Graphics.TCanvas;
  TGraphic = Graphics.TGraphic;
  TRawImage = GraphType.TRawImage;
  TBitmap = Graphics.TBitmap;

  TRasterImage = Graphics.TRasterImage;

  TFontStyle = Graphics.TFontStyle;
  TFontStyles = Graphics.TFontStyles;
  TFontQuality = Graphics.TFontQuality;

type
  TFont = Graphics.TFont;

const
  fsBold = Graphics.fsBold;
  fsItalic = Graphics.fsItalic;
  fsStrikeOut = Graphics.fsStrikeOut;
  fsUnderline = Graphics.fsUnderline;

  fqDefault = Graphics.fqDefault;
  fqDraft = Graphics.fqDraft;
  fqProof = Graphics.fqProof;
  fqNonAntialiased = Graphics.fqNonAntialiased;
  fqAntialiased = Graphics.fqAntialiased;
  fqCleartype = Graphics.fqCleartype;
  fqCleartypeNatural = Graphics.fqCleartypeNatural;

  clNone = Graphics.clNone;

  clBlack   = Graphics.clBlack;
  clMaroon  = Graphics.clMaroon;
  clGreen   = Graphics.clGreen;
  clOlive   = Graphics.clOlive;
  clNavy    = Graphics.clNavy;
  clPurple  = Graphics.clPurple;
  clTeal    = Graphics.clTeal;
  clGray    = Graphics.clGray;
  clSilver  = Graphics.clSilver;
  clRed     = Graphics.clRed;
  clLime    = Graphics.clLime;
  clYellow  = Graphics.clYellow;
  clBlue    = Graphics.clBlue;
  clFuchsia = Graphics.clFuchsia;
  clAqua    = Graphics.clAqua;
  clLtGray  = Graphics.clLtGray; // clSilver alias
  clDkGray  = Graphics.clDkGray; // clGray alias
  clWhite   = Graphics.clWhite;

function FPColorToTColor(const FPColor: TFPColor): TColor; inline;
function TColorToFPColor(const c: TColor): TFPColor; inline;
function ColorToRGB(c: TColor): TColor; inline;
function RGBToColor(R, G, B: Byte): TColor; inline;
procedure RedGreenBlue(rgb: TColor; out Red, Green, Blue: Byte); inline;// does not work on system color
function clRgbBtnHighlight: TColor;
function clRgbBtnShadow: TColor;

Implementation
End.
