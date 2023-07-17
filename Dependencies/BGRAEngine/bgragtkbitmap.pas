// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
 /**************************************************************************\
                             bgragtkbitmap.pas
                             -----------------
                 This unit should NOT be added to the 'uses' clause.
                 It contains patches for Gtk.
}

unit BGRAGtkBitmap;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRALCLBitmap, Graphics,
  GraphType;

type
  { TBGRAGtkBitmap }

  TBGRAGtkBitmap = class(TBGRALCLBitmap)
  private
    FPixBuf: Pointer;
    procedure DrawTransparent(ACanvas: TCanvas; Rect: TRect);
    procedure DrawOpaque(ACanvas: TCanvas; ARect: TRect);
  protected
    procedure ReallocData; override;
    procedure FreeData; override;
  public
    procedure DataDrawTransparent(ACanvas: TCanvas; Rect: TRect;
      AData: Pointer; ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer);
      override;
    procedure DrawPart(ARect: TRect; ACanvas: TCanvas; x, y: integer; Opaque: boolean); override;
    procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean = True); override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); override;
    procedure DataDrawOpaque(ACanvas: TCanvas; ARect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); overload; override;
    procedure DataDrawOpaque(ACanvas: TCanvas; ARect: TRect; ADataFirstRow: Pointer;
      ARowStride: integer; AWidth, AHeight: integer); overload;
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
  end;

Implementation
End.
