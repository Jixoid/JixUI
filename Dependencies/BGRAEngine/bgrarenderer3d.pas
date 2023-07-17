// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRARenderer3D;

{$mode objfpc}{$H+}

interface

uses BGRABitmapTypes,
  BGRASceneTypes, BGRASSE,
  BGRAPolygon, BGRAColorInt,
  BGRAClasses, BGRAMatrix3D,
  BGRAPolygonAliased;

type
  TInt65536ShaderFunction3D = function (Context: PBasicLightingContext; Color: TBGRAPixel): TColorInt65536 of object;

  { TBGRAShader3D }

  TBGRAShader3D = class
  protected
    FAmbiantLightness: integer;
    FAmbiantLightColor: TColorInt65536;
    FUseAmbiantColor: boolean;
    FLights: TList;
    FContextBlock: TMemoryBlockAlign128;
    FShaderFunc: TShaderFunction3D;
    FInt65536ShaderFunc: TInt65536ShaderFunction3D;
    FContext: PBasicLightingContext;
    FOnlyDirectionalLights: boolean;
    FWhiteMaterial: boolean;

    procedure ComputeDiffuseLightness(Context: PSceneLightingContext); inline;
    procedure ComputeDiffuseLight(Context: PSceneLightingContext); inline;
    procedure ComputeDiffuseAndSpecularLight(Context: PSceneLightingContext); inline;

    function ApplyNoLighting(Context: PSceneLightingContext; Color: TBGRAPixel): TBGRAPixel;
    function ApplyLightingWithAmbiantLightnessOnly(Context: PSceneLightingContext; Color: TBGRAPixel): TBGRAPixel;
    function ApplyLightingWithLightness(Context: PSceneLightingContext; Color: TBGRAPixel): TBGRAPixel;
    function ApplyLightingWithDiffuseColor(Context: PSceneLightingContext; Color: TBGRAPixel): TBGRAPixel;
    function ApplyLightingWithDiffuseAndSpecularColor(Context: PSceneLightingContext; Color: TBGRAPixel): TBGRAPixel;

    function Int65536ApplyNoLighting(Context: PSceneLightingContext; Color: TBGRAPixel): TColorInt65536;
    function Int65536ApplyLightingWithAmbiantLightnessOnly(Context: PSceneLightingContext; Color: TBGRAPixel): TColorInt65536;
    function Int65536ApplyLightingWithLightness(Context: PSceneLightingContext; Color: TBGRAPixel): TColorInt65536;
    function Int65536ApplyLightingWithDiffuseColor(Context: PSceneLightingContext; Color: TBGRAPixel): TColorInt65536;
    function Int65536ApplyLightingWithDiffuseAndSpecularColor(Context: PSceneLightingContext; Color: TBGRAPixel): TColorInt65536;
  public
    constructor Create(const AAmbiantLightColorF: TColorF; ALights: TList);
    destructor Destroy; override;
    function Apply(APosition: TPoint3D_128; ANormal: TPoint3D_128; AColor: TBGRAPixel): TBGRAPixel;
    function Int65536Apply(APosition: TPoint3D_128; ANormal: TPoint3D_128; AColor: TBGRAPixel): TColorInt65536;
    procedure Prepare(constref ADescription: TFaceRenderingDescription);
    property ShaderFunction: TShaderFunction3D read FShaderFunc;
    property Int65536ShaderFunction: TInt65536ShaderFunction3D read FInt65536ShaderFunc;
    property Context: PBasicLightingContext read FContext;
    property OnlyDirectionalLights: boolean read FOnlyDirectionalLights;
  end;

  { TBGRARenderer3D }

  TBGRARenderer3D = class(TCustomRenderer3D)
  protected
    FColorGradientTempBmp: TBGRACustomBitmap;
    FZBuffer: PSingle;
    FOutputSurface, FRenderSurface: TBGRACustomBitmap;
    FRenderSurfaceMultisample: Integer;
    FMultishapeFiller: TBGRAMultishapeFiller;
    FOptions: TRenderingOptions;
    FShader: TBGRAShader3D;
    FDepths: array of single;
    FLightings: array of word;

    FShadedColors: array of TBGRAPixel;
    FSameShadedColors: boolean;

    FCenter: record
      proj: TPointF;
      pos3D,normal3D: TPoint3D_128;
      color: TBGRAPixel;
    end;

    function GetHasZBuffer: boolean; override;
    function GetGlobalScale: single; override;
    function GetSurfaceWidth: integer; override;
    function GetSurfaceHeight: integer; override;
    function GetHandlesNearClipping: boolean; override;
    function GetHandlesFaceCulling: boolean; override;
  public
    constructor Create(AOutputSurface: TBGRACustomBitmap;
      ARenderingOptions: TRenderingOptions;
      AAmbiantLightColorF: TColorF;
      ALights: TList);
    function RenderFace(var ADescription: TFaceRenderingDescription;
      AComputeCoordinate: TComputeProjectionFunc): boolean; override;
    destructor Destroy; override;
  end;

Implementation
End.
