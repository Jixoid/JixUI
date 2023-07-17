// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Material Design color pallete from
  https://material.google.com/style/color.html#color-color-palette
}

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit MaterialColors;

{$I bgracontrols.map.inc}

{$IFDEF FPC}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  Classes, SysUtils, BGRABitmap, BGRABitmapTypes, {$IFDEF FPC}FGL{$ELSE}Generics.Collections{$ENDIF};

type

  { TMaterialColors }

  TMaterialColors = record
    M50: TBGRAPixel;
    M100: TBGRAPixel;
    M200: TBGRAPixel;
    M300: TBGRAPixel;
    M400: TBGRAPixel;
    M500: TBGRAPixel;
    M600: TBGRAPixel;
    M700: TBGRAPixel;
    M800: TBGRAPixel;
    M900: TBGRAPixel;
    A100: TBGRAPixel;
    A200: TBGRAPixel;
    A400: TBGRAPixel;
    A700: TBGRAPixel;
    procedure Create(aM50, aM100, aM200, aM300, aM400, aM500, aM600,
        aM700, aM800, aM900, aA100, aA200, aA400, aA700: string);
  end;

  {$IFDEF FPC}
  TMaterialColorsList = TFPGMap<string, TMaterialColors>;
  {$ELSE}
  TMaterialColorsList = TDictionary<string, TMaterialColors>;
  {$ENDIF}

procedure MaterialColorsListStr(AList: TStrings);

var
  MaterialBlack: TBGRAPixel;
  MaterialWhite: TBGRAPixel;
  MaterialRed, MaterialPink, MaterialPurple, MaterialDeepPurple,
  MaterialIndigo, MaterialBlue, MaterialLightBlue, MaterialCyan,
  MaterialTeal, MaterialGreen, MaterialLightGreen, MaterialLime,
  MaterialYellow, MaterialAmber, MaterialOrange, MaterialDeepOrange,
  MaterialBrown, MaterialGrey, MaterialBlueGrey: TMaterialColors;
  MaterialColorsList: TMaterialColorsList;

Implementation
End.
