// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAMacBitmap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRALCLBitmap, BGRAGraphics, BGRABitmapTypes,
  BGRADefaultBitmap;

type

  { TBGRAMacBitmap }

  TBGRAMacBitmap = class(TBGRALCLBitmap)
    procedure DataDrawOpaque(ACanvas: TCanvas; Rect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    function MakeBitmapCopy(BackgroundColor: TColor; AMasked: boolean=False): TBitmap; override;
    procedure TakeScreenshotOfPrimaryMonitor; override;
    procedure TakeScreenshot(ARect: TRect); override;
  end;

Implementation
End.
