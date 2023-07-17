// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
 /**************************************************************************\
                             bgrawinbitmap.pas
                             -----------------
                 This unit should NOT be added to the 'uses' clause.
                 It contains accelerations for Windows. Notably, it
                 provides direct access to bitmap data.
}

unit BGRAWinBitmap;

{$mode objfpc}{$H+}
{$WARN 6018 off : unreachable code}
interface

uses
  BGRAClasses, SysUtils, BGRALCLBitmap, Windows, Graphics, GraphType;

type
  { TBGRAWinBitmap }

  TBGRAWinBitmap = class(TBGRALCLBitmap)
  private
    procedure AlphaCorrectionNeeded;
  protected
    DIB_SectionHandle: HBITMAP;
    FReversed: boolean;
    function DIBitmapInfo(AWidth, AHeight: integer): TBitmapInfo;

    procedure ReallocData; override;
    procedure FreeData; override;

    procedure RebuildBitmap; override;
    procedure FreeBitmap; override;

    procedure Init; override;
    function GetBitmap: TBitmap; override;

  public
    procedure LoadFromBitmapIfNeeded; override;
    procedure Draw(ACanvas: TCanvas; x, y: integer; Opaque: boolean=True); overload; override;
    procedure Draw(ACanvas: TCanvas; Rect: TRect; Opaque: boolean = True); overload; override;
    procedure DataDrawOpaque(ACanvas: TCanvas; ARect: TRect; AData: Pointer;
      ALineOrder: TRawImageLineOrder; AWidth, AHeight: integer); override;
    procedure GetImageFromCanvas(CanvasSource: TCanvas; x, y: integer); override;
  end;

Implementation
End.
