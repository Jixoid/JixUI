unit BGRADrawerFlashProgressBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, {$IFDEF BGRABITMAP_USE_MSEGUI} mclasses, {$ENDIF} SysUtils, Types, BGRABitmap, BGRABitmapTypes, BGRAGraphics, BGRAGradients,
  Math;

type

  TBGRAProgressBarRedrawEvent = procedure(Sender: TObject; Bitmap: TBGRABitmap; xpos: integer) of object;

  { TBGRADrawerFlashProgressBar }

  TBGRADrawerFlashProgressBar = class(TPersistent)
  private
    FBackgroundColor: TColor;
    FBackgroundRandomize: boolean;
    FBackgroundRandomizeMaxIntensity: word;
    FBackgroundRandomizeMinIntensity: word;
    FBarColor: TColor;
    FMaxValue: integer;
    FMinValue: integer;
    FOnChange: TNotifyEvent;
    FRandSeed: integer;
    FValue: integer;
    xpos: integer;
    procedure SetBackgroundRandomize(AValue: boolean);
    procedure SetBackgroundRandomizeMaxIntensity(AValue: word);
    procedure SetBackgroundRandomizeMinIntensity(AValue: word);
    procedure SetBarColor(AValue: TColor);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetMaxValue(AValue: integer);
    procedure SetMinValue(AValue: integer);
    procedure SetRandSeed(AValue: integer);
    procedure SetValue(AValue: integer);
  public
    procedure Draw(ABitmap: TBGRABitmap);
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property RandSeed: integer read FRandSeed write SetRandSeed;
    property BarColor: TColor read FBarColor write SetBarColor;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property BackgroundRandomizeMinIntensity: word
      read FBackgroundRandomizeMinIntensity write SetBackgroundRandomizeMinIntensity;
    property BackgroundRandomizeMaxIntensity: word
      read FBackgroundRandomizeMaxIntensity write SetBackgroundRandomizeMaxIntensity;
    property BackgroundRandomize: boolean read FBackgroundRandomize
      write SetBackgroundRandomize;
    property XPosition: integer read xpos;
  public
    property MinValue: integer read FMinValue write SetMinValue;
    property MaxValue: integer read FMaxValue write SetMaxValue;
    property Value: integer read FValue write SetValue;
  end;

Implementation
End.
