// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAFilterScanner;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, BGRABitmapTypes, BGRAFilterType;

type
  { TBGRAFilterScannerGrayscale }
  { Grayscale converts colored pixel into grayscale with same luminosity }
  TBGRAFilterScannerGrayscale = class(TBGRAFilterScannerPixelwise)
    class procedure ComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
      ACount: integer; AGammaCorrection: boolean); override;
  end;

  { TBGRAFilterScannerNegative }

  TBGRAFilterScannerNegative = class(TBGRAFilterScannerPixelwise)
    class procedure ComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
      ACount: integer; AGammaCorrection: boolean); override;
  end;

  { TBGRAFilterScannerSwapRedBlue }

  TBGRAFilterScannerSwapRedBlue = class(TBGRAFilterScannerPixelwise)
    class procedure ComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
      ACount: integer; {%H-}AGammaCorrection: boolean); override;
  end;

  { TBGRAFilterScannerNormalize }
  { Normalize compute min-max of specified channel and apply an affine transformation
    to make it use the full range of values }
  TBGRAFilterScannerNormalize = class(TBGRAFilterScannerPixelwise)
  private
    minValRed, maxValRed, minValGreen, maxValGreen,
    minValBlue, maxValBlue, minAlpha, maxAlpha: word;
    addValRed, addValGreen, addValBlue, addAlpha: word;
    factorValRed, factorValGreen, factorValBlue, factorAlpha: int32or64;
    procedure DetermineNormalizationFactors(ABounds: TRect; AEachChannel: boolean);
  protected
    procedure DoComputeFilterAt(ASource: PBGRAPixel; ADest: PBGRAPixel;
            ACount: integer; {%H-}AGammaCorrection: boolean); override;
  public
    constructor Create(ASource: IBGRAScanner; AOffset: TPoint; ABounds: TRect;
            AEachChannel: boolean);
    class procedure ComputeFilterAt({%H-}ASource: PBGRAPixel; {%H-}ADest: PBGRAPixel;
            {%H-}ACount: integer; {%H-}AGammaCorrection: boolean); override;
  end;

  { TBGRA3X3FilterScanner }

  TBGRA3X3FilterScanner = class(TBGRAFilterScannerMultipixel)
  protected
    FSourceBorderColor,FDestinationBorderColor: TBGRAPixel;
    FAutoSourceBorderColor: boolean;
    function DoFilter3X3(PTop,PMiddle,PBottom: PBGRAPixel): TBGRAPixel; virtual; abstract;
    procedure DoComputeFilter(BufferX: Integer;
      const Buffers: array of PBGRAPixel; BufferWidth: integer;
      ADest: PBGRAPixel; ACount: integer); override;
  public
    constructor Create(ASource: IBGRAScanner; ABounds: TRect); overload;
    constructor Create(ASource: TBGRACustomBitmap); overload;
    property SourceBorderColor: TBGRAPixel read FSourceBorderColor write FSourceBorderColor;
    property DestinationBorderColor: TBGRAPixel read FDestinationBorderColor write FDestinationBorderColor;
    property AutoSourceBorderColor: boolean read FAutoSourceBorderColor write FAutoSourceBorderColor;
  end;

  { TBGRAContourScanner }
  { Filter contour compute a grayscale image, then for each pixel
    calculates the difference with surrounding pixels (in intensity and alpha)
    and draw black pixels when there is a difference }
  TBGRAContourScanner = class(TBGRA3X3FilterScanner)
  protected
    FGammaCorrection: boolean;
    FOpacity: byte;
    function DoFilter3X3(PTop,PMiddle,PBottom: PBGRAPixel): TBGRAPixel; override;
  public
    constructor Create(ASource: IBGRAScanner; ABounds: TRect;
                       AGammaCorrection: boolean = False); overload;
    constructor Create(ASource: TBGRACustomBitmap;
                       AGammaCorrection: boolean = False); overload;
    property Opacity: Byte read FOpacity write FOpacity;
  end;

  { TBGRASharpenScanner }

  TBGRASharpenScanner = class(TBGRA3X3FilterScanner)
  protected
    FAmount: integer;
    function DoFilter3X3(PTop,PMiddle,PBottom: PBGRAPixel): TBGRAPixel; override;
  public
    constructor Create(ASource: IBGRAScanner; ABounds: TRect;
                       AAmount: integer = 256); overload;
    constructor Create(ASource: TBGRACustomBitmap;
                       AAmount: integer = 256); overload;
  end;

  { TBGRAEmbossHightlightScanner }

  TBGRAEmbossHightlightScanner = class(TBGRA3X3FilterScanner)
  protected
    FFillSelection: boolean;
    FSourceChannel: TChannel;
    FChannelOffset: Byte;
    function DoFilter3X3(PTop,PMiddle,PBottom: PBGRAPixel): TBGRAPixel; override;
    procedure SetSourceChannel(AValue: TChannel);
  public
    constructor Create(ASource: IBGRAScanner; ABounds: TRect; ABoundsVisible: Boolean); overload;
    constructor Create(ASource: TBGRACustomBitmap; ABoundsVisible: Boolean); overload;
    property FillSelection: boolean read FFillSelection write FFillSelection;
    property SourceChannel: TChannel read FSourceChannel write SetSourceChannel;
  end;

Implementation
End.
