// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAPolygonAliased;

{$mode objfpc}{$H+}

{$i bgrasse.map.inc}

interface

{ This unit provides fast aliased polygon routines.

  To do aliased drawing, only one line is intersected with polygons for each output scanline.
  Along with intersection coordinates, color and texture coordinates are computed using
  linear interpolation. Inverse values are used for projective transform. }

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, BGRAFillInfo, BGRASSE;

type
  //segment information for linear color
  TLinearColorInfo = record
    Color, ColorSlopes: TColorF;
  end;
  PLinearColorInfo = ^TLinearColorInfo;
  ArrayOfTColorF = array of TColorF;

  //add a color information to intersection info
  TLinearColorGradientIntersectionInfo = class(TIntersectionInfo)
    Color: TColorF;
  end;

  { TPolygonLinearColorGradientInfo }

  TPolygonLinearColorGradientInfo = class(TOnePassFillPolyInfo)
  protected
    FColors: array of TColorF;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding,
      ANumSegment: integer; dy: single; AData: pointer); override;
  public
    constructor Create(const points: array of TPointF; const Colors: array of TBGRAPixel);
    function CreateSegmentData(numPt, nextPt: integer; ASeg: PCustomPointRecord): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
  end;

procedure PolygonLinearColorGradientAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonLinearColorGradientInfo;
  NonZeroWinding: boolean); overload;
procedure PolygonLinearColorGradientAliased(bmp: TBGRACustomBitmap; const points: array of TPointF;
  const Colors: array of TBGRAPixel; NonZeroWinding: boolean); overload;

type
  //segment information for linear color
  TPerspectiveColorInfo = record
    ColorDivZ, ColorSlopesDivZ: TColorF;
    InvZ, InvZSlope: single;
  end;
  PPerspectiveColorInfo = ^TPerspectiveColorInfo;

  //add a color information to intersection info
  TPerspectiveColorGradientIntersectionInfo = class(TIntersectionInfo)
    ColorDivZ: TColorF;
    coordInvZ: single;
  end;

  { TPolygonPerspectiveColorGradientInfo }

  TPolygonPerspectiveColorGradientInfo = class(TOnePassFillPolyInfo)
  protected
    FColors: array of TColorF;
    FPointsZ: array of single;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding,
      ANumSegment: integer; dy: single; AData: pointer); override;
  public
    constructor Create(const points: array of TPointF; const pointsZ: array of single; const Colors: array of TBGRAPixel);
    function CreateSegmentData(numPt, nextPt: integer; ASeg: PCustomPointRecord): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
  end;

procedure PolygonPerspectiveColorGradientAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonPerspectiveColorGradientInfo;
  NonZeroWinding: boolean; zbuffer: psingle = nil); overload;
procedure PolygonPerspectiveColorGradientAliased(bmp: TBGRACustomBitmap; const points: array of TPointF;
  const pointsZ: array of single; const Colors: array of TBGRAPixel; NonZeroWinding: boolean; zbuffer: psingle = nil); overload;

type
  //segment information for linear texture
  TLinearTextureInfo = record
    TexCoord: TPointF;
    TexCoordSlopes: TPointF;
    lightness: single;
    lightnessSlope: single;
  end;
  PLinearTextureInfo = ^TLinearTextureInfo;

  //add a texture coordinate to intersection info
  TLinearTextureMappingIntersectionInfo = class(TIntersectionInfo)
    texCoord: TPointF;
    lightness: word;
  end;

  { TPolygonLinearTextureMappingInfo }

  TPolygonLinearTextureMappingInfo = class(TOnePassFillPolyInfo)
  protected
    FTexCoords: array of TPointF;
    FLightnesses: array of Word;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding,
      ANumSegment: integer; dy: single; AData: pointer); override;
  public
    constructor Create(const points: array of TPointF; const texCoords: array of TPointF); overload;
    constructor Create(const points: array of TPointF; const texCoords: array of TPointF; const lightnesses: array of word); overload;
    function CreateSegmentData(numPt, nextPt: integer; ASeg: PCustomPointRecord): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
  end;

procedure PolygonLinearTextureMappingAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonLinearTextureMappingInfo;
  texture: IBGRAScanner; TextureInterpolation: Boolean; NonZeroWinding: boolean); overload;

procedure PolygonLinearTextureMappingAliased(bmp: TBGRACustomBitmap; const points: array of TPointF; texture: IBGRAScanner;
  const texCoords: array of TPointF; TextureInterpolation: Boolean; NonZeroWinding: boolean); overload;
procedure PolygonLinearTextureMappingAliasedWithLightness(bmp: TBGRACustomBitmap; const points: array of TPointF; texture: IBGRAScanner;
  const texCoords: array of TPointF; TextureInterpolation: Boolean; lightnesses: array of word; NonZeroWinding: boolean); overload;

type
  //segment information for perspective texture. Use inverse Z and slopes.
  TPerspectiveTextureInfo = record
    InvZ,InvZSlope: Single;
    TexCoordDivByZ: TPointF;
    TexCoordDivByZSlopes: TPointF;
    lightness: single;
    lightnessSlope: single;
    Position3D, Normal3D: TPoint3D_128;
    Position3DSlope, Normal3DSlope: TPoint3D_128;
  end;
  PPerspectiveTextureInfo = ^TPerspectiveTextureInfo;

  //add a texture coordinate and depth to intersection info (stored as inverse)
  TPerspectiveTextureMappingIntersectionInfo = class(TIntersectionInfo)
    texCoordDivByZ: TPointF;
    coordInvZ: single;
    lightness: word;
    Position3D, Normal3D: TPoint3D_128;
  end;

  { TPolygonPerspectiveTextureMappingInfo }

  TPolygonPerspectiveTextureMappingInfo = class(TOnePassFillPolyInfo)
  protected
    FTexCoords: array of TPointF;
    FPointsZ: array of single;
    FLightnesses: array of Word;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding,
      ANumSegment: integer; dy: single; AData: pointer); override;
  public
    constructor Create(const points: array of TPointF; const pointsZ: array of single; const texCoords: array of TPointF); overload;
    constructor Create(const points: array of TPointF; const pointsZ: array of single; const texCoords: array of TPointF; const lightnesses: array of word); overload;
    function CreateSegmentData(numPt, nextPt: integer; ASeg: PCustomPointRecord): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
  end;

  { TPolygonPerspectiveMappingShaderInfo }

  TPolygonPerspectiveMappingShaderInfo = class(TOnePassFillPolyInfo)
  protected
    FTexCoords: array of TPointF;
    FPositions3D, FNormals3D: array of TPoint3D_128;
    procedure SetIntersectionValues(AInter: TIntersectionInfo; AInterX: Single; AWinding,
      ANumSegment: integer; dy: single; AData: pointer); override;
  public
    constructor Create(const points: array of TPointF; const points3D: array of TPoint3D; const normals: array of TPoint3D; const texCoords: array of TPointF); overload;
    constructor Create(const points: array of TPointF; const points3D: array of TPoint3D_128; const normals: array of TPoint3D_128; const texCoords: array of TPointF); overload;
    function CreateSegmentData(numPt, nextPt: integer; ASeg: PCustomPointRecord): pointer; override;
    function CreateIntersectionInfo: TIntersectionInfo; override;
  end;

  TShaderFunction3D = function (Context: PBasicLightingContext; Color: TBGRAPixel): TBGRAPixel of object;

procedure PolygonPerspectiveTextureMappingAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonPerspectiveTextureMappingInfo;
         texture: IBGRAScanner; TextureInterpolation: Boolean; NonZeroWinding: boolean; zbuffer: psingle = nil); overload;
procedure PolygonPerspectiveTextureMappingAliased(bmp: TBGRACustomBitmap; const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner;
           const texCoords: array of TPointF; TextureInterpolation: Boolean; NonZeroWinding: boolean; zbuffer: psingle = nil); overload;
procedure PolygonPerspectiveTextureMappingAliasedWithLightness(bmp: TBGRACustomBitmap; const points: array of TPointF; const pointsZ: array of single; texture: IBGRAScanner;
           const texCoords: array of TPointF; TextureInterpolation: Boolean; lightnesses: array of word; NonZeroWinding: boolean; zbuffer: psingle = nil); overload;

procedure PolygonPerspectiveMappingShaderAliased(bmp: TBGRACustomBitmap; polyInfo: TPolygonPerspectiveMappingShaderInfo;
         texture: IBGRAScanner; TextureInterpolation: Boolean; ShaderFunction: TShaderFunction3D; NonZeroWinding: boolean;
         solidColor: TBGRAPixel; zbuffer: psingle = nil; ShaderContext: PBasicLightingContext= nil); overload;
procedure PolygonPerspectiveMappingShaderAliased(bmp: TBGRACustomBitmap; const points: array of TPointF; const points3D: array of TPoint3D;
           const normals: array of TPoint3D; texture: IBGRAScanner; const texCoords: array of TPointF;
           TextureInterpolation: Boolean; ShaderFunction: TShaderFunction3D; NonZeroWinding: boolean;
           solidColor: TBGRAPixel; zbuffer: psingle = nil; ShaderContext: PBasicLightingContext= nil); overload;
procedure PolygonPerspectiveMappingShaderAliased(bmp: TBGRACustomBitmap; const points: array of TPointF; const points3D: array of TPoint3D_128;
           const normals: array of TPoint3D_128; texture: IBGRAScanner; const texCoords: array of TPointF;
           TextureInterpolation: Boolean; ShaderFunction: TShaderFunction3D; NonZeroWinding: boolean;
           solidColor: TBGRAPixel; zbuffer: psingle = nil; ShaderContext: PBasicLightingContext= nil); overload;

{ Aliased round rectangle }
procedure BGRARoundRectAliased(dest: TBGRACustomBitmap; X1, Y1, X2, Y2: integer;
  DX, DY: integer; BorderColor, FillColor: TBGRAPixel; FillTexture: IBGRAScanner = nil; ADrawMode: TDrawMode = dmDrawWithTransparency;
  skipFill: boolean = false); overload;
procedure BGRARoundRectAliased(dest: TCustomUniversalBitmap; X1, Y1, X2, Y2: integer;
  DX, DY: integer; const BorderColor, FillColor: TUniversalBrush; AAlpha: Word; skipBorder: boolean = false; skipFill: boolean = false); overload;
procedure BGRAFillRoundRectAliased(dest: TBGRACustomBitmap; X1, Y1, X2, Y2: integer;
  DX, DY: integer; FillColor: TBGRAPixel; FillTexture: IBGRAScanner = nil; ADrawMode: TDrawMode = dmDrawWithTransparency);

Implementation
End.
