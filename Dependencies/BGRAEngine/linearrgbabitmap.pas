// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit LinearRGBABitmap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, UniversalDrawer;

type

  { TLinearRGBABitmap }

  TLinearRGBABitmap = class(specialize TGenericUniversalBitmap<TLinearRGBA,TLinearRGBAColorspace>)
  protected
    function InternalNew: TCustomUniversalBitmap; override;
    procedure AssignTransparentPixel(out ADest); override;
  public
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TLinearRGBA; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure MaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
                              AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure EraseBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    class procedure AlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
  end;

const
  LinearRGBATransparent : TLinearRGBA = (red:0; green:0; blue:0; alpha:0);

operator = (const c1, c2: TLinearRGBA): boolean; inline;

Implementation
End.
