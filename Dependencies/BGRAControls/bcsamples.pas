// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCSamples;

{$I bgracontrols.map.inc}

interface

uses
  Classes, Controls, Graphics,
  {$IFDEF FPC}LCLProc, LCLType, LazUTF8,{$ENDIF}
  StdCtrls, BCButton, BCButtonFocus, BCTypes,
  {$IFNDEF FPC}Types, Windows, Sysutils, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BGRABitmap, BGRABitmapTypes, BGRAGradients, MaterialColors, BCBrightAndContrast;

{$IFNDEF FPC}
type
  TWinControlHack = class(TWinControl);
{$ENDIF}

const
  {Accent Colors}
  acMagenta = TColor($009700FF);
  acPurple = TColor($00FF00A2);
  acTeal = TColor($00A9AB00);
  acLime = TColor($0026BF8C);
  acBrown = TColor($000050A0);
  acPink = TColor($00B871E6);
  acOrange = TColor($000996F0);
  acBlue = TColor($00E2A11B);
  acRed = TColor($000014E5);
  acGreen = TColor($00339933);
  {Facebook Colors}
  fbBlue = TColor($00AA785F);
  fbGreen = TColor($004BA567);
  fbGray = TColor($00F8F8F8);
  {Windows 8 Color Scheme - Background}
  clScheme1_Background = TColor($00252525);
  clScheme2_Background = TColor($00252525);
  clScheme3_Background = TColor($00252525);
  clScheme4_Background = TColor($00252525);
  clScheme5_Background = TColor($0000172E);
  clScheme6_Background = TColor($0000004E);
  clScheme7_Background = TColor($0038004E);
  clScheme8_Background = TColor($004E002D);
  clScheme9_Background = TColor($0068001F);
  clScheme10_Background = TColor($004E1E00);
  clScheme11_Background = TColor($00604D00);
  clScheme12_Background = TColor($00004A00);
  clScheme13_Background = TColor($002A9915);
  clScheme14_Background = TColor($00196CE5);
  clScheme15_Background = TColor($001B1BB8);
  clScheme16_Background = TColor($006C1BB8);
  clScheme17_Background = TColor($00B81B69);
  clScheme18_Background = TColor($00B8581B);
  clScheme19_Background = TColor($00E39C56);
  clScheme20_Background = TColor($00AAAA00);
  clScheme21_Background = TColor($001FBA83);
  clScheme22_Background = TColor($00099DD3);
  clScheme23_Background = TColor($00B764E0);
  clScheme24_Background = TColor($00696969);
  clScheme25_Background = TColor($00696969);
  {Windows 8 Color Scheme - Selection}
  clScheme1_Selection = TColor($0000B3F4);
  clScheme2_Selection = TColor($0000BA78);
  clScheme3_Selection = TColor($00EC7326);
  clScheme4_Selection = TColor($003D11AE);
  clScheme5_Selection = TColor($00002F63);
  clScheme6_Selection = TColor($00001EB0);
  clScheme7_Selection = TColor($004F00C1);
  clScheme8_Selection = TColor($00AC0072);
  clScheme9_Selection = TColor($00B41746);
  clScheme10_Selection = TColor($00C16A00);
  clScheme11_Selection = TColor($00878200);
  clScheme12_Selection = TColor($00009919);
  clScheme13_Selection = TColor($003FC100);
  clScheme14_Selection = TColor($001D98FF);
  clScheme15_Selection = TColor($00122EFF);
  clScheme16_Selection = TColor($00771DFF);
  clScheme17_Selection = TColor($00FF40AA);
  clScheme18_Selection = TColor($00FFAE1F);
  clScheme19_Selection = TColor($00FFC556);
  clScheme20_Selection = TColor($00CCD800);
  clScheme21_Selection = TColor($0000D191);
  clScheme22_Selection = TColor($0000B7E1);
  clScheme23_Selection = TColor($00BC76FF);
  clScheme24_Selection = TColor($00A4A400);
  clScheme25_Selection = TColor($00237DFF);

type
  TBCSampleStyle = (ssDefault, ssWindows7, ssWindows7ToolBar, ssOffice2010,
    ssFlashPlayer, ssMacOSXLion, ssWindows8_1, ssWindows8_2, ssWindows8_3,
    ssWindows8_4, ssWindows8_5, ssWindows8_6, ssWindows8_7, ssWindows8_8,
    ssWindows8_9, ssWindows8_10, ssWindows8_11, ssWindows8_12, ssWindows8_13,
    ssWindows8_14, ssWindows8_15, ssWindows8_16, ssWindows8_17, ssWindows8_18,
    ssWindows8_19, ssWindows8_20, ssWindows8_21, ssWindows8_22, ssWindows8_23,
    ssWindows8_24, ssWindows8_25, ssMaterialRed, ssMaterialPink, ssMaterialPurple,
    ssMaterialDeepPurple, ssMaterialIndigo, ssMaterialBlue, ssMaterialLightBlue,
    ssMaterialCyan, ssMaterialTeal, ssMaterialGreen, ssMaterialLightGreen,
    ssMaterialLime, ssMaterialYellow, ssMaterialAmber, ssMaterialOrange,
    ssMaterialDeepOrange, ssMaterialBrown, ssMaterialGrey, ssMaterialBlueGrey);

  TBCSampleDrawing = (sdDefault, sdFlashPlayerBody, sdFlashPlayerButtonPanel,
    sdWindows7Toolbar, sdiOSBar, sdiOSToolBar, sdiOSBackground,
    sdWindows8_1, sdWindows8_2, sdWindows8_3,
    sdWindows8_4, sdWindows8_5, sdWindows8_6, sdWindows8_7, sdWindows8_8,
    sdWindows8_9, sdWindows8_10, sdWindows8_11, sdWindows8_12, sdWindows8_13,
    sdWindows8_14, sdWindows8_15, sdWindows8_16, sdWindows8_17, sdWindows8_18,
    sdWindows8_19, sdWindows8_20, sdWindows8_21, sdWindows8_22, sdWindows8_23,
    sdWindows8_24, sdWindows8_25);

const
  BCSampleStyleStr: array[TBCSampleStyle] of string =
    ('Default', 'Windows 7', 'Windows 7 ToolBar', 'Office 2010',
    'Flash Player', 'Mac OSX Lion', 'Windows 8 Scheme 1', 'Windows 8 Scheme 2',
    'Windows 8 Scheme 3', 'Windows 8 Scheme 4', 'Windows 8 Scheme 5'
    , 'Windows 8 Scheme 6', 'Windows 8 Scheme 7', 'Windows 8 Scheme 8'
    , 'Windows 8 Scheme 9', 'Windows 8 Scheme 10', 'Windows 8 Scheme 11'
    , 'Windows 8 Scheme 12', 'Windows 8 Scheme 13', 'Windows 8 Scheme 14'
    , 'Windows 8 Scheme 15', 'Windows 8 Scheme 16', 'Windows 8 Scheme 17'
    , 'Windows 8 Scheme 18', 'Windows 8 Scheme 19', 'Windows 8 Scheme 20'
    , 'Windows 8 Scheme 21', 'Windows 8 Scheme 22', 'Windows 8 Scheme 23'
    , 'Windows 8 Scheme 24', 'Windows 8 Scheme 25', 'Material Red', 'Material Pink',
    'Material Purple', 'Material Deep Purple', 'Material Indigo', 'Material Blue',
    'Material Light Blue', 'Material Cyan', 'Material Teal', 'Material Green',
    'Material Light Green', 'Material Lime', 'Material Yellow', 'Material Amber',
    'Material Orange', 'Material Deep Orange', 'Material Brown', 'Material Grey',
    'Material Blue Grey');

  BCSampleDrawingStr: array[TBCSampleDrawing] of string =
    ('Default', 'Flash Player Body', 'Flash Player Button Panel',
    'Windows 7 ToolBar', 'iOS Bar', 'iOS ToolBar', 'iOS Background',
    'Windows 8 Scheme 1', 'Windows 8 Scheme 2',
    'Windows 8 Scheme 3', 'Windows 8 Scheme 4', 'Windows 8 Scheme 5'
    , 'Windows 8 Scheme 6', 'Windows 8 Scheme 7', 'Windows 8 Scheme 8'
    , 'Windows 8 Scheme 9', 'Windows 8 Scheme 10', 'Windows 8 Scheme 11'
    , 'Windows 8 Scheme 12', 'Windows 8 Scheme 13', 'Windows 8 Scheme 14'
    , 'Windows 8 Scheme 15', 'Windows 8 Scheme 16', 'Windows 8 Scheme 17'
    , 'Windows 8 Scheme 18', 'Windows 8 Scheme 19', 'Windows 8 Scheme 20'
    , 'Windows 8 Scheme 21', 'Windows 8 Scheme 22', 'Windows 8 Scheme 23'
    , 'Windows 8 Scheme 24', 'Windows 8 Scheme 25');

function StrToTBCSampleStyle(const s: ansistring): TBCSampleStyle;
procedure BCSampleStyleStrList(s: TStrings);

function StrToTBCSampleDrawing(const s: ansistring): TBCSampleDrawing;
procedure BCSampleDrawingStrList(s: TStrings);

procedure StyleButtons(AControl: TControl; AButton: TBCButton); overload;
procedure StyleButtons(AControl: TControl; AButton: TBCButtonFocus); overload;
procedure StyleButtonsSample(AControl: TControl; AStyle: TBCSampleStyle);
procedure StyleButtonsFocusSample(AControl: TControl; AStyle: TBCSampleStyle);

procedure DrawSample(ABitmap: TBGRABitmap; Element: TBCSampleDrawing;
  Align: TAlign = alNone); overload;
function DrawSample(AWidth, AHeight: integer; Element: TBCSampleDrawing;
  Align: TAlign = alNone): TBGRABitmap; overload;
procedure DrawItem(Control: TWinControl; Index: integer; ARect: TRect;
  State: TOwnerDrawState; Style: TBCSampleDrawing);

{ Buttons }
procedure BCButtonWindows7(AButton: TBCButton); overload;
procedure BCButtonWindows7ToolBar(AButton: TBCButton); overload;
procedure BCButtonOffice2010(AButton: TBCButton); overload;
procedure BCButtonFlashPlayer(AButton: TBCButton); overload;
procedure BCButtonMacOSXLion(AButton: TBCButton); overload;
procedure BCButtonWindows8(AButton: TBCButton; cl1, cl2: TColor; rounding: integer = 1); overload;

procedure BCButtonWindows7(AButton: TBCButtonFocus); overload;
procedure BCButtonWindows7ToolBar(AButton: TBCButtonFocus);overload;
procedure BCButtonOffice2010(AButton: TBCButtonFocus);overload;
procedure BCButtonFlashPlayer(AButton: TBCButtonFocus);overload;
procedure BCButtonMacOSXLion(AButton: TBCButtonFocus);overload;
procedure BCButtonWindows8(AButton: TBCButtonFocus; cl1, cl2: TColor;
  rounding: integer = 1); overload;

{ Drawings }
procedure DrawFlashPlayerBody(ABitmap: TBGRABitmap);
procedure DrawFlashPlayerButtonPanel(ABitmap: TBGRABitmap);
procedure DrawWindows7ToolBar(ABitmap: TBGRABitmap; ADir: TAlign;
  Smooth: boolean = False);
procedure DrawiOSBar(ABitmap: TBGRABitmap);
procedure DrawiOSToolBar(ABitmap: TBGRABitmap; Shadow: boolean = True);
procedure DrawiOSBackground(ABitmap: TBGRABitmap);
procedure DrawWindows8Background(ABitmap: TBGRABitmap; {%H-}cl1, cl2: TColor);

Implementation
End.
