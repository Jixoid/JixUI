unit PalUtils;

interface

uses
  LCLType, LCLIntf, SysUtils, Classes, Graphics,
  //RGBHSVUtils, RGBHSLUtils,
  RGBCIEUtils, RGBCMYKUtils,
  HTMLColors;

const
  clCustom = $2FFFFFFF;
  clTransparent = $3FFFFFFF;

type
  TSortOrder = (soAscending, soDescending);
  TSortMode = (smRed, smGreen, smBlue, smHue, smSaturation, smLuminance, smValue, smNone, smCyan, smMagenta, smYellow, smBlacK, smCIEx, smCIEy, smCIEz, smCIEl, smCIEa, smCIEb);

  AcoColors = record
    Colors: array of TColor;
    Names: array of WideString;
    HasNames: boolean;
  end;

//replaces passed strings with passed value
function ReplaceFlags(s: string; flags: array of string; value: integer): string;

//replaces the appropriate tags with values in a hint format string
function FormatHint(fmt: string; c: TColor): string;

//converts a string value to TColor including clCustom and clTransparent
function mbStringToColor(s: string): TColor;

//converts a TColor to a string value including clCustom and clTransparent
function mbColorToString(c: TColor): string;

//blends two colors together in proportion C1 : C2 = W1 : 100 - W1, where 0 <= W1 <= 100
function Blend(C1, C2: TColor; W1: Integer): TColor;

// Inverts a color
function InvertedColor(C: TColor): TColor;

//generates a white-color-black or a black-color-white gradient palette
function MakePalette(BaseColor: TColor; SortOrder: TSortOrder): string;

//generates a gradient palette from the given colors
function MakeGradientPalette(Colors: array of TColor): string;

//sorts colors in a string list
procedure SortPalColors(Colors: TStrings; SortMode: TSortMode; SortOrder: TSortOrder);

//reads JASC .pal file
function ReadJASCPal(PalFile: TFileName): string;

//saves a string list to a JASC .pal file
procedure SaveJASCPal(pal: TStrings; FileName: TFileName);

//reads Photoshop .aco file into an Aco record
function ReadPhotoshopAco(PalFile: TFileName): AcoColors;

//reads Photoshop .act file
function ReadPhotoshopAct(PalFile: TFileName): string;


Implementation
End.
