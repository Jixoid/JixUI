// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
 /**************************************************************************\
                                bgrabitmaptypes.pas
                                -------------------
                   This unit defines basic types and it must be
                   included in the 'uses' clause.

       --> Include BGRABitmap and BGRABitmapTypes in the 'uses' clause.
	       If you are using LCL types, add also BGRAGraphics unit.
}

unit BGRABitmapTypes;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 5093 off : function result variable of a managed type does not seem to be initialized}
{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}
{$WARN 5025 off : Local variable "$1" not used}
{$i bgrabitmap.map.inc}

interface

uses
  BGRAClasses, BGRAGraphics, BGRAUnicode,
  FPImage{$IFDEF BGRABITMAP_USE_FPCANVAS}, FPImgCanv{$ENDIF}
  {$IFDEF BGRABITMAP_USE_LCL}, LCLType, GraphType, LResources{$ENDIF},
  BGRAMultiFileType;


const
  BGRABitmapVersion = 11050400;

  function BGRABitmapVersionStr: string;

type
  TMultiFileContainer = BGRAMultiFileType.TMultiFileContainer;
  Int32or64 = BGRAClasses.Int32or64;
  UInt32or64 = BGRAClasses.UInt32or64;
  HDC = {$IFDEF BGRABITMAP_USE_LCL}LCLType.HDC{$ELSE}PtrUInt{$ENDIF};

{=== Miscellaneous types ===}

type
  {* Options when doing a floodfill (also called bucket fill) }
  TFloodfillMode = (
    {** Pixels that are filled are replaced }
    fmSet,
    {** Pixels that are filled are drawn upon with the fill color }
    fmDrawWithTransparency,
    {** Pixels that are filled are drawn without gamma correction upon with the fill color }
    fmLinearBlend,
    {** Pixels that are XORed with the fill color}
    fmXor,
    {** Pixels that are filled are drawn upon to the extent that the color underneath is similar to
        the start color. The more different the different is, the less it is drawn upon }
    fmProgressive);

  {* Specifies how much smoothing is applied to the computation of the median }
  TMedianOption = (moNone, moLowSmooth, moMediumSmooth, moHighSmooth);
  {* Specifies the shape of a predefined blur }
  TRadialBlurType = (
    {** Gaussian-like, pixel importance decreases progressively }
    rbNormal,
    {** Disk blur, pixel importance does not decrease progressively }
    rbDisk,
    {** Pixel are considered when they are at a certain distance }
    rbCorona,
    {** Gaussian-like, but 10 times smaller than ''rbNormal'' }
    rbPrecise,
    {** Gaussian-like but simplified to be computed faster }
    rbFast,
    {** Box blur, pixel importance does not decrease progressively
        and the pixels are included when they are in a square.
        This is much faster than ''rbFast'' however you may get
        square shapes in the resulting image }
    rbBox);

  TEmbossOption = (eoTransparent, eoPreserveHue);
  TEmbossOptions = set of TEmbossOption;

  {* List of image formats }
  TBGRAImageFormat = (
    {** Unknown format }
    ifUnknown,
    {** JPEG format, opaque, lossy compression }
    ifJpeg,
    {** PNG format, transparency, lossless compression }
    ifPng,
    {** GIF format, single transparent color, lossless in theory but only low number of colors allowed }
    ifGif,
    {** BMP format, transparency, no compression. Note that transparency is
        not supported by all BMP readers so it is recommended to avoid
        storing images with transparency in this format }
    ifBmp,
    {** iGO BMP (16-bit, rudimentary lossless compression) }
    ifBmpMioMap,
    {** ICO format, contains different sizes of the same image }
    ifIco,
    {** CUR format, has hotspot, contains different sizes of the same image }
    ifCur,
    {** PCX format, opaque, rudimentary lossless compression }
    ifPcx,
    {** Paint.NET format, layers, lossless compression }
    ifPaintDotNet,
    {** LazPaint format, layers, lossless compression }
    ifLazPaint,
    {** OpenRaster format, layers, lossless compression }
    ifOpenRaster,
    {** Phoxo format, layers }
    ifPhoxo,
    {** Photoshop format, layers, rudimentary lossless compression }
    ifPsd,
    {** Targa format (TGA), transparency, rudimentary lossless compression }
    ifTarga,
    {** TIFF format, limited support }
    ifTiff,
    {** X-Window capture, limited support }
    ifXwd,
    {** X-Pixmap, text encoded image, limited support }
    ifXPixMap,
    {** text or binary encoded image, no compression, extension PBM, PGM, PPM }
    ifPortableAnyMap,
    {** Scalable Vector Graphic, vectorial, read-only as raster }
    ifSvg,
    {** Lossless or lossy compression using V8 algorithm (need libwebp library) }
    ifWebP,
    {** Lossless or lossy compression using Avif algorithm (need libavif library) }
    ifAvif
    );


  {* Options when loading an image }
  TBGRALoadingOption = (
     {** Do not clear RGB channels when alpha is zero (not recommended) }
     loKeepTransparentRGB,
     {** Consider BMP to be opaque if no alpha value is provided (for compatibility) }
     loBmpAutoOpaque,
     {** Load JPEG quickly however with a lower quality }
     loJpegQuick);
  TBGRALoadingOptions = set of TBGRALoadingOption;

  TTextLayout = BGRAGraphics.TTextLayout;
  TFontBidiMode = BGRAUnicode.TFontBidiMode;
  TBidiTextAlignment = (btaNatural, btaOpposite, btaLeftJustify, btaRightJustify, btaCenter);

const
  fbmAuto = BGRAUnicode.fbmAuto;
  fbmLeftToRight = BGRAUnicode.fbmLeftToRight;
  fbmRightToLeft = BGRAUnicode.fbmRightToLeft;

  function AlignmentToBidiTextAlignment(AAlign: TAlignment; ARightToLeft: boolean): TBidiTextAlignment; overload;
  function AlignmentToBidiTextAlignment(AAlign: TAlignment): TBidiTextAlignment; overload;
  function BidiTextAlignmentToAlignment(ABidiAlign: TBidiTextAlignment; ARightToLeft: boolean): TAlignment;

const
  RadialBlurTypeToStr: array[TRadialBlurType] of string =
  ('Normal','Disk','Corona','Precise','Fast','Box');


  tlTop = BGRAGraphics.tlTop;
  tlCenter = BGRAGraphics.tlCenter;
  tlBottom = BGRAGraphics.tlBottom;

  // checks the bounds of an image in the given clipping rectangle
  function CheckPutImageBounds(x, y, tx, ty: integer; out minxb, minyb, maxxb, maxyb, ignoreleft: integer; const cliprect: TRect): boolean;

{==== Imported from GraphType ====}
//if this unit is defined, otherwise
//define here the types used by the library.
{$IFDEF BGRABITMAP_USE_LCL}
  type
    { Order of the lines in an image }
    TRawImageLineOrder = GraphType.TRawImageLineOrder;
    { Order of the bits in a byte containing pixel values }
    TRawImageBitOrder = GraphType.TRawImageBitOrder;
    { Order of the bytes in a group of byte containing pixel values }
    TRawImageByteOrder = GraphType.TRawImageByteOrder;
    { Definition of a single line 3D bevel }
    TGraphicsBevelCut = GraphType.TGraphicsBevelCut;

  const
    riloTopToBottom = GraphType.riloTopToBottom;   // The first line (line 0) is the top line
    riloBottomToTop = GraphType.riloBottomToTop;   // The first line (line 0) is the bottom line

    riboBitsInOrder = GraphType.riboBitsInOrder;   // Bit 0 is pixel 0
    riboReversedBits = GraphType.riboReversedBits; // Bit 0 is pixel 7 (Bit 1 is pixel 6, ...)

    riboLSBFirst = GraphType.riboLSBFirst; // least significant byte first (little endian)
    riboMSBFirst = GraphType.riboMSBFirst; // most significant byte first (big endian)

    fsSurface = GraphType.fsSurface; //type is defined as Graphics.TFillStyle
    fsBorder = GraphType.fsBorder;

    bvNone = GraphType.bvNone;
    bvLowered = GraphType.bvLowered;
    bvRaised = GraphType.bvRaised;
    bvSpace = GraphType.bvSpace;
{$ELSE}
  type
    {* Order of the lines in an image }
    TRawImageLineOrder = (
      {** The first line in memory (line 0) is the top line }
      riloTopToBottom,
      {** The first line in memory (line 0) is the bottom line }
      riloBottomToTop);

    {* Order of the bits in a byte containing pixel values }
    TRawImageBitOrder = (
      {** The lowest bit is on the left. So with a monochrome picture, bit 0 would be pixel 0 }
      riboBitsInOrder,
      {** The lowest bit is on the right. So with a momochrome picture, bit 0 would be pixel 7 (bit 1 would be pixel 6, ...) }
      riboReversedBits);

    {* Order of the bytes in a group of byte containing pixel values }
    TRawImageByteOrder = (
      {** Least significant byte first (little endian) }
      riboLSBFirst,
      {** most significant byte first (big endian) }
      riboMSBFirst);

    {* Definition of a single line 3D bevel }
    TGraphicsBevelCut =
    (
      {** No bevel }
      bvNone,
      {** Shape is lowered, light is on the bottom-right corner }
      bvLowered,
      {** Shape is raised, light is on the top-left corner }
      bvRaised,
      {** Shape is at the same level, there is no particular lighting }
      bvSpace);
{$ENDIF}

{$DEFINE INCLUDE_INTERFACE}
{$I bgrapixel.map.inc}

{$DEFINE INCLUDE_INTERFACE}
{$I geometrytypes.map.inc}

{$DEFINE INCLUDE_INTERFACE}
{$i csscolorconst.map.inc}

{$DEFINE INCLUDE_INTERFACE}
{$I bgrascanner.map.inc}

{$DEFINE INCLUDE_INTERFACE}
{$I unibitmap.map.inc}

{$DEFINE INCLUDE_INTERFACE}
{$I unibitmapgeneric.map.inc}

{==== Integer math ====}

  {* Computes the value modulo cycle, and if the ''value'' is negative, the result
     is still positive }
  function PositiveMod(value, cycle: Int32or64): Int32or64; inline; overload;

  { Sin65536 and Cos65536 are fast routines to compute sine and cosine as integer values.
    They use a table to store already computed values. The return value is an integer
    ranging from 0 to 65536, so the mean value is 32768 and the half amplitude is
    32768 instead of 1. The input has a period of 65536, so you can supply any integer
    without applying a modulo. }

  { Compute all values now }
  procedure PrecalcSin65536;

  {* Returns an integer approximation of the sine. Value ranges from 0 to 65535,
     where 65536 corresponds to the next cycle }
  function Sin65536(value: word): Int32or64; inline;
  {* Returns an integer approximation of the cosine. Value ranges from 0 to 65535,
     where 65536 corresponds to the next cycle }
  function Cos65536(value: word): Int32or64; inline;

  {* Returns the square root of the given byte, considering that
     255 is equal to unity }
  function ByteSqrt(value: byte): byte; inline;

{==== Types provided for fonts ====}
type
  {* Quality to be used to render text }
  TBGRAFontQuality = (
    {** Use the system capabilities. It is rather fast however it may be
        not be smoothed. }
    fqSystem,
    {** Use the system capabilities to render with ClearType. This quality is
        of course better than fqSystem however it may not be perfect.}
    fqSystemClearType,
    {** Garanties a high quality antialiasing. }
    fqFineAntialiasing,
    {** Fine antialiasing with ClearType assuming an LCD display in red/green/blue order }
    fqFineClearTypeRGB,
    {** Fine antialiasing with ClearType assuming an LCD display in blue/green/red order }
    fqFineClearTypeBGR);

  TGetFineClearTypeAutoFunc = function(): TBGRAFontQuality;
var
  fqFineClearType : TGetFineClearTypeAutoFunc;

type
  {* Measurements of a font }
  TFontPixelMetric = record
    {** The values have been computed }
    Defined: boolean;
    {** Position of the baseline, where most letters lie }
    Baseline,
    {** Position of the top of the small letters (x being one of them) }
    xLine,
    {** Position of the top of the UPPERCASE letters }
    CapLine,
    {** Position of the bottom of letters like g and p }
    DescentLine,
    {** Total line height including line spacing defined by the font }
    Lineheight: integer;
  end;

  {* Measurements of a font in floating point values }
  TFontPixelMetricF = record
    {** The values have been computed }
    Defined: boolean;
    {** Position of the baseline, where most letters lie }
    Baseline,
    {** Position of the top of the small letters (x being one of them) }
    xLine,
    {** Position of the top of the UPPERCASE letters }
    CapLine,
    {** Position of the bottom of letters like g and p }
    DescentLine,
    {** Total line height including line spacing defined by the font }
    Lineheight: single;
  end;

  {* Vertical anchoring of the font. When text is drawn, a start coordinate
      is necessary. Text can be positioned in different ways. This enum
      defines what position it is regarding the font }
  TFontVerticalAnchor = (
    {** The top of the font. Everything will be drawn below the start coordinate. }
    fvaTop,
    {** The center of the font }
    fvaCenter,
    {** The top of capital letters }
    fvaCapLine,
    {** The center of capital letters }
    fvaCapCenter,
    {** The top of small letters }
    fvaXLine,
    {** The center of small letters }
    fvaXCenter,
    {** The baseline, the bottom of most letters }
    fvaBaseline,
    {** The bottom of letters that go below the baseline }
    fvaDescentLine,
    {** The bottom of the font. Everything will be drawn above the start coordinate }
    fvaBottom);

  {* Definition of a function that handles work-break }
  TWordBreakHandler = procedure(var ABeforeUTF8, AAfterUTF8: string) of object;

  {* Alignment for a typewriter, that does not have any more information
     than a square shape containing glyphs }
  TBGRATypeWriterAlignment = (twaTopLeft, twaTop, twaTopRight, twaLeft, twaMiddle, twaRight, twaBottomLeft, twaBottom, twaBottomRight);
  {* How a typewriter must render its content on a Canvas2d }
  TBGRATypeWriterOutlineMode = (twoPath, twoFill, twoStroke, twoFillOverStroke, twoStrokeOverFill, twoFillThenStroke, twoStrokeThenFill);

  { TBGRACustomFontRenderer }
  {* Abstract class for all font renderers }
  TBGRACustomFontRenderer = class
  protected
    {** Specifies the height of the font without taking into account additional line spacing.
        A negative value means that it is the full height instead }
    FFontEmHeightF: single;
    function GetFontEmHeight: integer;
    procedure SetFontEmHeight(AValue: integer);
  public
    {** Specifies the font to use. Unless the font renderer accept otherwise,
        the name is in human readable form, like 'Arial', 'Times New Roman', ...  }
    FontName: string;

    {** Specifies the set of styles to be applied to the font.
        These can be fsBold, fsItalic, fsStrikeOut, fsUnderline.
        So the value [fsBold,fsItalic] means that the font must be bold and italic }
    FontStyle: TFontStyles;

    {** Specifies the quality of rendering. Default value is fqSystem }
    FontQuality : TBGRAFontQuality;

    {** Specifies the rotation of the text, for functions that support text rotation.
        It is expressed in tenth of degrees, positive values going counter-clockwise }
    FontOrientation: integer;

    {** Returns measurement for the current font in pixels }
    function GetFontPixelMetric: TFontPixelMetric; virtual; abstract;
    function GetFontPixelMetricF: TFontPixelMetricF; virtual;
    function FontExists(AName: string): boolean; virtual; abstract;

    {** Checks if any text would be visible using the specified color }
    function TextVisible(const AColor: TBGRAPixel): boolean; virtual;

    {** Returns the total size of the string provided using the current font.
        Orientation is not taken into account, so that the width is along the text }
    function TextSize(sUTF8: string): TSize; overload; virtual; abstract;
    function TextSizeF(sUTF8: string): TPointF; overload; virtual;
    function TextSize(sUTF8: string; AMaxWidth: integer; ARightToLeft: boolean): TSize; overload; virtual; abstract;
    function TextSizeF(sUTF8: string; AMaxWidthF: single; ARightToLeft: boolean): TPointF; overload; virtual;
    function TextSizeAngle(sUTF8: string; {%H-}orientationTenthDegCCW: integer): TSize; virtual;
    function TextSizeAngleF(sUTF8: string; {%H-}orientationTenthDegCCW: integer): TPointF; virtual;

    {** Returns the number of Unicode characters that fit into the specified size }
    function TextFitInfo(sUTF8: string; AMaxWidth: integer): integer; virtual; abstract;
    function TextFitInfoF(sUTF8: string; AMaxWidthF: single): integer; virtual;

    {** Draws the UTF8 encoded string, with color ''c''.
        If align is taLeftJustify, (''x'',''y'') is the top-left corner.
        If align is taCenter, (''x'',''y'') is at the top and middle of the text.
        If align is taRightJustify, (''x'',''y'') is the top-right corner.
        The value of ''FontOrientation'' is taken into account, so that the text may be rotated }
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment); overload; virtual; abstract;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; c: TBGRAPixel; align: TAlignment; {%H-}ARightToLeft: boolean); overload; virtual;

    {** Same as above functions, except that the text is filled using texture.
        The value of ''FontOrientation'' is taken into account, so that the text may be rotated }
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment); overload; virtual; abstract;
    procedure TextOut(ADest: TBGRACustomBitmap; x, y: single; sUTF8: string; texture: IBGRAScanner; align: TAlignment; {%H-}ARightToLeft: boolean); overload; virtual;

    {** Same as above, except that the orientation is specified, overriding the value of the property ''FontOrientation'' }
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment); overload; virtual; abstract;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; c: TBGRAPixel; align: TAlignment; {%H-}ARightToLeft: boolean); overload; virtual;
    {** Same as above, except that the orientation is specified, overriding the value of the property ''FontOrientation'' }
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment); overload; virtual; abstract;
    procedure TextOutAngle(ADest: TBGRACustomBitmap; x, y: single; orientationTenthDegCCW: integer; sUTF8: string; texture: IBGRAScanner; align: TAlignment; {%H-}ARightToLeft: boolean); overload; virtual;

    {** Draw the UTF8 encoded string at the coordinate (''x'',''y''), clipped inside the rectangle ''ARect''.
        Additional style information is provided by the style parameter.
        The color ''c'' is used to fill the text. No rotation is applied. }
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; c: TBGRAPixel); overload; virtual; abstract;

    {** Same as above except a ''texture'' is used to fill the text }
    procedure TextRect(ADest: TBGRACustomBitmap; ARect: TRect; x, y: integer; sUTF8: string; style: TTextStyle; texture: IBGRAScanner); overload; virtual; abstract;

    {** Copy the path for the UTF8 encoded string into ''ADest''.
        If ''align'' is ''taLeftJustify'', (''x'',''y'') is the top-left corner.
        If ''align'' is ''taCenter'', (''x'',''y'') is at the top and middle of the text.
        If ''align'' is ''taRightJustify'', (''x'',''y'') is the top-right corner. }
    procedure CopyTextPathTo({%H-}ADest: IBGRAPath; {%H-}x, {%H-}y: single; {%H-}s: string; {%H-}align: TAlignment); virtual; //optional
    procedure CopyTextPathTo({%H-}ADest: IBGRAPath; {%H-}x, {%H-}y: single; {%H-}s: string; {%H-}align: TAlignment; {%H-}ARightToLeft: boolean); virtual; //optional
    function HandlesTextPath: boolean; virtual;

    property FontEmHeight: integer read GetFontEmHeight write SetFontEmHeight;
    property FontEmHeightF: single read FFontEmHeightF write FFontEmHeightF;
  end;

  {* Output mode for the improved renderer for readability. This is used by the font renderer based on LCL in ''BGRAText'' }
  TBGRATextOutImproveReadabilityMode = (irMask, irNormal, irClearTypeRGB, irClearTypeBGR);

{** Removes line ending and tab characters from a string (for a function
    like ''TextOut'' that does not handle this). this works with UTF8 strings
    as well }
function CleanTextOutString(const s: string): string;
{** Remove the line ending at the specified position or return False.
    This works with UTF8 strings however the index is the byte index }
function RemoveLineEnding(var s: string; indexByte: integer): boolean;
{** Remove the line ending at the specified position or return False.
    The index is the character index, that may be different from the
    byte index }
function RemoveLineEndingUTF8(var sUTF8: string; indexUTF8: integer): boolean;
{** Default word break handler }
procedure BGRADefaultWordBreakHandler(var ABefore, AAfter: string);

{==== Images and resampling ====}

type
  {* How the resample is to be computed }
  TResampleMode = (
    {** Low quality resample by repeating pixels, stretching them }
    rmSimpleStretch,
    {** Use resample filters. This gives high
        quality resampling however this the proportion changes slightly because
        the first and last pixel are considered to occupy only half a unit as
        they are considered as the border of the picture
        (pixel-centered coordinates) }
    rmFineResample);

  {* List of resample filter to be used with ''rmFineResample'' }
  TResampleFilter = (
    {** Equivalent of simple stretch with high quality and pixel-centered coordinates }
    rfBox,
    {** Linear interpolation giving slow transition between pixels }
    rfLinear,
    {** Mix of ''rfLinear'' and ''rfCosine'' giving medium speed stransition between pixels }
    rfHalfCosine,
    {** Cosine-like interpolation giving fast transition between pixels }
    rfCosine,
    {** Simple bi-cubic filter (blurry) }
    rfBicubic,
    {** Mitchell filter, good for downsizing interpolation }
    rfMitchell,
    {** Spline filter, good for upsizing interpolation, however slightly blurry }
    rfSpline,
    {** Lanczos with radius 2, blur is corrected }
    rfLanczos2,
    {** Lanczos with radius 3, high contrast }
    rfLanczos3,
    {** Lanczos with radius 4, high contrast }
    rfLanczos4,
    {** Best quality using rfMitchell or rfSpline }
    rfBestQuality);

const
  {** List of strings to represent resample filters }
  ResampleFilterStr : array[TResampleFilter] of string =
   ('Box','Linear','HalfCosine','Cosine','Bicubic','Mitchell','Spline',
    'Lanczos2','Lanczos3','Lanczos4','BestQuality');

  {** Gives the sample filter represented by a string }
  function StrToResampleFilter(str: string): TResampleFilter;

type
  {* Image information from superficial analysis }
  TQuickImageInfo = record
    {** Width in pixels }
    Width,
    {** Height in pixels }
    Height,
    {** Bitdepth for colors (1, 2, 4, 8 for images with palette/grayscale, 16, 24 or 48 if each channel is present) }
    ColorDepth,
    {** Bitdepth for alpha (0 if no alpha channel, 1 if bit mask, 8 or 16 if alpha channel) }
    AlphaDepth: integer;
  end;

  {* Bitmap reader with additional features }
  TBGRAImageReader = class(TFPCustomImageReader)
    {** Return bitmap information (size, bit depth) }
    function GetQuickInfo(AStream: TStream): TQuickImageInfo; virtual; abstract;
    {** Return a draft of the bitmap, the ratio may change compared to the original width and height (useful to make thumbnails) }
    function GetBitmapDraft(AStream: TStream; AMaxWidth, AMaxHeight: integer; out AOriginalWidth,AOriginalHeight: integer): TBGRACustomBitmap; virtual; abstract;
  end;

  { TBGRACustomWriterPNG }

  TBGRACustomWriterPNG = class(TFPCustomImageWriter)
  protected
    function GetUseAlpha: boolean; virtual; abstract;
    procedure SetUseAlpha(AValue: boolean); virtual; abstract;
  public
    property UseAlpha : boolean read GetUseAlpha write SetUseAlpha;
  end;

var
  {** List of stream readers for images }
  DefaultBGRAImageReader: array[TBGRAImageFormat] of TFPCustomImageReaderClass;
  {** List of stream writers for images }
  DefaultBGRAImageWriter: array[TBGRAImageFormat] of TFPCustomImageWriterClass;

  {** Detect the file format of a given file }
  function DetectFileFormat(AFilenameUTF8: string): TBGRAImageFormat;
  {** Detect the file format of a given stream. ''ASuggestedExtensionUTF8'' can
      be provided to guess the format }
  function DetectFileFormat(AStream: TStream; ASuggestedExtensionUTF8: string = ''): TBGRAImageFormat;
  {** Returns the file format that is most likely to be stored in the
      given filename (according to its extension) }
  function SuggestImageFormat(AFilenameOrExtensionUTF8: string): TBGRAImageFormat;
  {** Returns a likely image extension for the format }
  function SuggestImageExtension(AFormat: TBGRAImageFormat): string;
  {** Create an image reader for the given format }
  function CreateBGRAImageReader(AFormat: TBGRAImageFormat): TFPCustomImageReader;
  {** Create an image writer for the given format. ''AHasTransparentPixels''
      specifies if alpha channel must be supported }
  function CreateBGRAImageWriter(AFormat: TBGRAImageFormat; AHasTransparentPixels: boolean): TFPCustomImageWriter;

{$DEFINE INCLUDE_INTERFACE}
{$I bgracustombitmap.map.inc}

operator =(const AGuid1, AGuid2: TGuid): boolean;

type
  { TBGRAResourceManager }

  TBGRAResourceManager = class
  protected
    function GetWinResourceType(AExtension: string): pchar;
  public
    function GetResourceStream(AFilename: string): TStream; virtual;
    function IsWinResource(AFilename: string): boolean; virtual;
  end;

var
  BGRAResource : TBGRAResourceManager;

Implementation
End.
