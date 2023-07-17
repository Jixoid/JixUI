// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit ExpandedBitmap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, UniversalDrawer;

type

  { TExpandedBitmap }

  TExpandedBitmap = class(specialize TGenericUniversalBitmap<TExpandedPixel,TExpandedPixelColorspace>)
  protected
    function InternalNew: TCustomUniversalBitmap; override;
    procedure AssignTransparentPixel(out ADest); override;
  public
    class procedure SolidBrush(out ABrush: TUniversalBrush; const AColor: TExpandedPixel; ADrawMode: TDrawMode = dmDrawWithTransparency); override;
    class procedure ScannerBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner; ADrawMode: TDrawMode = dmDrawWithTransparency;
                                 AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure MaskBrush(out ABrush: TUniversalBrush; AScanner: IBGRAScanner;
                              AOffsetX: integer = 0; AOffsetY: integer = 0); override;
    class procedure EraseBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
    class procedure AlphaBrush(out ABrush: TUniversalBrush; AAlpha: Word); override;
  end;

const
  ExpandedPixelTransparent : TExpandedPixel = (red:0; green:0; blue:0; alpha:0);

operator = (const c1, c2: TExpandedPixel): boolean; inline;

Implementation
End.
