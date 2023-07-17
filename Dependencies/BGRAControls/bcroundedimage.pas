// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  BCRoundedImage
  by Lainz

  Last modified: 2020-09-06 19:16 GMT-3

  Changelog:
  - 2020-09-06: Initial version supporting circle, rounded rectangle and square.
                Changing the quality of the resample, setting the rounding.
                OnPaintEvent to customize the final drawing.
}
unit BCRoundedImage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes;

type
  TBCRoundedImage = class;

  // Event to draw before the image is sent to canvas
  TBCRoundedImagePaintEvent = procedure (const Sender: TBCRoundedImage; const Bitmap: TBGRABitmap) of object;
  // Supported styles are circle, rounded rectangle and square
  TBCRoundedImageStyle = (isCircle, isRoundedRectangle, isSquare);

  // Control that draws an image within a rounded border

  { TBCRoundedImage }

  TBCRoundedImage = class(TGraphicControl)
  private
    FBorderStyle: TRoundRectangleOptions;
    FOnPaintEvent: TBCRoundedImagePaintEvent;
    FPicture: TPicture;
    FQuality: TResampleFilter;
    FStyle: TBCRoundedImageStyle;
    FRounding: single;
    procedure SetBorderStyle(AValue: TRoundRectangleOptions);
    procedure SetPicture(AValue: TPicture);
    procedure SetQuality(AValue: TResampleFilter);
    procedure SetStyle(AValue: TBCRoundedImageStyle);
    procedure SetRounding(AValue: single);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    // The image that's used as background
    property Picture: TPicture read FPicture write SetPicture;
    // The style can be circle, rounded rectangle or square
    property Style: TBCRoundedImageStyle read FStyle write SetStyle;
    // The style of the rounded rectangle
    property BorderStyle: TRoundRectangleOptions read FBorderStyle write SetBorderStyle;
    // Rounding is used when you choose the rounded rectangle style
    property Rounding: single read FRounding write SetRounding;
    // The quality when resizing the image
    property Quality: TResampleFilter read FQuality write SetQuality;
    // You can paint before the bitmap is drawn on canvas
    property OnPaintEvent: TBCRoundedImagePaintEvent read FOnPaintEvent write FOnPaintEvent;
  published
    property Anchors;
    property Align;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnClick;
  end;

procedure Register;

Implementation
End.
