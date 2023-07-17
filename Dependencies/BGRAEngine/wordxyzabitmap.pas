// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit WordXYZABitmap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, UniversalDrawer;

type

  { TWordXYZABitmap }

  TWordXYZABitmap = class(specialize TGenericUniversalBitmap<TWordXYZA,TWordXYZAColorspace>)
  protected
    function InternalNew: TCustomUniversalBitmap; override;
    procedure AssignTransparentPixel(out ADest); override;
  public
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TWordXYZA; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure MaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
                              AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure EraseBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    class procedure AlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    procedure ReplaceImaginary(const AAfter: TWordXYZA);
  end;

const
  WordXYZATransparent : TWordXYZA = (X:0; Y:0; Z:0; alpha:0);

operator = (const c1, c2: TWordXYZA): boolean; inline;

Implementation
End.
