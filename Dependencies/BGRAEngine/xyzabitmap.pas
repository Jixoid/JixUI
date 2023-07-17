// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit XYZABitmap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, UniversalDrawer;

type

  { TXYZABitmap }

  TXYZABitmap = class(specialize TGenericUniversalBitmap<TXYZA,TXYZAColorspace>)
  protected
    function InternalNew: TCustomUniversalBitmap; override;
    procedure AssignTransparentPixel(out ADest); override;
  public
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TXYZA; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure MaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
                              AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure EraseBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    class procedure AlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    procedure ReplaceImaginary(const AAfter: TXYZA);
  end;

const
  XYZATransparent : TXYZA = (X:0; Y:0; Z:0; alpha:0);

operator = (const c1, c2: TXYZA): boolean; inline;
function IsRealColor(xyza: TXYZA): boolean;

Implementation
End.
