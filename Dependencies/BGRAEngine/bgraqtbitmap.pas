// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
 /**************************************************************************\
                             bgraqtbitmap.pas
                             -----------------
                 This unit should NOT be added to the 'uses' clause.
                 It contains patches for Qt.
}

unit BGRAQtBitmap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRALCLBitmap, Graphics,
  GraphType, BGRABitmapTypes;

type
  { TBGRAQtBitmap }

  TBGRAQtBitmap = class(TBGRALCLBitmap)
  private
    procedure SlowDrawTransparent(ABitmap: TBGRACustomBitmap;
      ACanvas: TCanvas; ARect: TRect);
  public
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
      AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; ARect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean = True); override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); override;
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
  end;

Implementation
End.
