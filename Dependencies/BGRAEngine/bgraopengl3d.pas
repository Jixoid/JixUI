// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAOpenGL3D;

{$mode objfpc}{$H+}

interface

uses BGRABitmapTypes,
  BGRASceneTypes, BGRASSE,
  BGRAClasses, BGRAMatrix3D,
  BGRACanvasGL,
  BGRAScene3D,
  BGRAOpenGLType,
  BGRATransform,
  BGRARenderer3D;

type
  TAttributeVariable = BGRACanvasGL.TAttributeVariable;

  TBGLShader3D = class;

  { TBGLLighting3D }

  TBGLLighting3D = class
  private
    procedure SetUseBuiltIn(AValue: boolean);
  protected
    FCanvas: TBGLCustomCanvas;
    FLights: TList;
    FAmbiantLight: TColorF;
    FShaderLightingCode: string;
    FUseBuiltIn: boolean;
    procedure Init;
  public
    constructor Create(ACanvas: TBGLCustomCanvas; AAmbiantLight: TColorF; ALights: TList);
    procedure SetSpecularIndex(AIndex: Integer);
    destructor Destroy; override;
    property ShaderLightingCode: string read FShaderLightingCode;
    property UseOpenGLBuiltInLighting: boolean read FUseBuiltIn write SetUseBuiltIn;
  end;

  { TBGLRenderer3D }

  TBGLRenderer3D = class(TCustomRenderer3D)
  protected
    FCanvas: TBGLCustomCanvas;
    FHasZBuffer: Boolean;
    FGlobalScale: single;
    FOptions: TRenderingOptions;
    FFactorZ, FAddZ: single;
    FLightingGL: TBGLLighting3D;
    FLights: TList;
    FAmbiantLight: TColorF;
    FFar: single;
    FOldCulling: TFaceCulling;
    FOldMatrix: TAffineMatrix;
    FOldProjection, FProjectionMatrix: TMatrix4D;
    FShader, FShaderWithTexture: TBGLCustomShader;
    FBGRAShader: TBGRAShader3D;
    FShadedColorsF: array of TColorF;
    FShadedColors: array of TBGRAPixel;
    function GetHasZBuffer: boolean; override;
    function GetGlobalScale: single; override;
    function GetSurfaceWidth: integer; override;
    function GetSurfaceHeight: integer; override;
    procedure SetProjection(const AValue: TProjection3D); override;
    function GetHandlesNearClipping: boolean; override;
    function GetHandlesFaceCulling: boolean; override;
    procedure InitLighting(AUseOpenGLBuiltInLighting: boolean);
  public
    constructor Create(ACanvas: TBGLCustomCanvas;
      AScene: TBGRAScene3D; AFar: single);
    function RenderFace(var ADescription: TFaceRenderingDescription;
      {%H-}AComputeCoordinate: TComputeProjectionFunc): boolean; override;
    destructor Destroy; override;
    property Canvas: TBGLCustomCanvas read FCanvas;
  end;

  { TBGLScene3D }

  TBGLScene3D = class(TBGRAScene3D)
  protected
    function LoadBitmapFromFileUTF8(AFilenameUTF8: string): TBGRACustomBitmap; override;
  public
    procedure RenderGL(ACanvas: TBGLCustomCanvas; AMaxZ: single = 1000); virtual;
  end;

  { TUniformVariable }

  TUniformVariable = object
  private
    FProgram: TBGLShader3D;
    FVariable: LongWord;
    procedure Init(AProgram: TBGLShader3D; AVariable: LongWord);
  end;

  { TUniformVariableSingle }

  TUniformVariableSingle = object(TUniformVariable)
  private
    FValue: single;
    procedure SetValue(const AValue: single);
  public
    procedure Update;
    property Value: single read FValue write SetValue;
  end;

  { TUniformVariablePointF }

  TUniformVariablePointF = object(TUniformVariable)
  private
    FValue: TPointF;
    procedure SetValue(const AValue: TPointF);
  public
    procedure Update;
    property Value: TPointF read FValue write SetValue;
  end;

  { TUniformVariablePoint3D }

  TUniformVariablePoint3D = object(TUniformVariable)
  private
    FValue: TPoint3D;
    procedure SetValue(const AValue: TPoint3D);
  public
    procedure Update;
    property Value: TPoint3D read FValue write SetValue;
  end;

  { TUniformVariableInteger }

  TUniformVariableInteger = object(TUniformVariable)
  private
    FValue: Integer;
    procedure SetValue(const AValue: Integer);
  public
    procedure Update;
    property Value: Integer read FValue write SetValue;
  end;

  { TUniformVariablePoint }

  TUniformVariablePoint = object(TUniformVariable)
  private
    FValue: TPoint;
    procedure SetValue(const AValue: TPoint);
  public
    procedure Update;
    property Value: TPoint read FValue write SetValue;
  end;

  { TUniformVariableMatrix4D }

  TUniformVariableMatrix4D = object(TUniformVariable)
  private
    FValue: TMatrix4D;
    procedure SetValue(const AValue: TMatrix4D);
  public
    procedure Update;
    property Value: TMatrix4D read FValue write SetValue;
  end;

  { TAttributeVariableSingle }

  TAttributeVariableSingle = object(TAttributeVariable)
  protected
    procedure Init(AProgram: TObject; AAttribute: LongWord);
  end;

  { TAttributeVariablePointF }

  TAttributeVariablePointF = object(TAttributeVariable)
  protected
    procedure Init(AProgram: TObject; AAttribute: LongWord);
  end;

  { TAttributeVariablePoint3D }

  TAttributeVariablePoint3D = object(TAttributeVariable)
  protected
    procedure Init(AProgram: TObject; AAttribute: LongWord);
  end;

  { TAttributeVariableInteger }

  TAttributeVariableInteger = object(TAttributeVariable)
  protected
    procedure Init(AProgram: TObject; AAttribute: LongWord);
  end;

  { TAttributeVariablePoint }

  TAttributeVariablePoint = object(TAttributeVariable)
  protected
    procedure Init(AProgram: TObject; AAttribute: LongWord);
  end;

  { TBGLShader3D }

  TBGLShader3D = class(TBGLCustomShader)
  protected
    FUsed: boolean;
    FCanvas: TBGLCustomCanvas;
    FLighting: TBGLCustomLighting;
    FVertexShaderSource,
    FFragmentShaderSource: string;
    FVertexShader,
    FFragmentShader,
    FProgram: LongWord;
    function GetUniformVariableSingle(AName: string): TUniformVariableSingle;
    function GetUniformVariablePointF(AName: string): TUniformVariablePointF;
    function GetUniformVariablePoint3D(AName: string): TUniformVariablePoint3D;
    function GetUniformVariableInteger(AName: string): TUniformVariableInteger;
    function GetUniformVariablePoint(AName: string): TUniformVariablePoint;
    function GetUniformVariableMatrix4D(AName: string): TUniformVariableMatrix4D;
    function GetAttributeVariableInteger(AName: string): TAttributeVariableInteger;
    function GetAttributeVariablePoint(AName: string): TAttributeVariablePoint;
    function GetAttributeVariableSingle(AName: string): TAttributeVariableSingle;
    function GetAttributeVariablePointF(AName: string): TAttributeVariablePointF;
    function GetAttributeVariablePoint3D(AName: string): TAttributeVariablePoint3D;
    procedure SetUniformSingle(AVariable: LongWord; const AValue; AElementCount: integer; AComponentCount: integer);
    procedure SetUniformInteger(AVariable: LongWord; const AValue; AElementCount: integer; AComponentCount: integer);
    procedure CheckUsage(AUsing: boolean);
    procedure StartUse; override;
    procedure EndUse; override;
    property Canvas: TBGLCustomCanvas read FCanvas;
  public
    constructor Create(ACanvas: TBGLCustomCanvas; AVertexShaderSource: string;
        AFragmentShaderSource: string; AVaryingVariables: string = '';
        AVersion: string = '120');
    destructor Destroy; override;
    property UniformSingle[AName: string]: TUniformVariableSingle read GetUniformVariableSingle;
    property UniformPointF[AName: string]: TUniformVariablePointF read GetUniformVariablePointF;
    property UniformPoint3D[AName: string]: TUniformVariablePoint3D read GetUniformVariablePoint3D;
    property UniformInteger[AName: string]: TUniformVariableInteger read GetUniformVariableInteger;
    property UniformPoint[AName: string]: TUniformVariablePoint read GetUniformVariablePoint;
    property UniformMatrix4D[AName: string]: TUniformVariableMatrix4D read GetUniformVariableMatrix4D;
    property AttributeSingle[AName: string]: TAttributeVariableSingle read GetAttributeVariableSingle;
    property AttributePointF[AName: string]: TAttributeVariablePointF read GetAttributeVariablePointF;
    property AttributePoint3D[AName: string]: TAttributeVariablePoint3D read GetAttributeVariablePoint3D;
    property AttributeInteger[AName: string]: TAttributeVariableInteger read GetAttributeVariableInteger;
    property AttributePoint[AName: string]: TAttributeVariablePoint read GetAttributeVariablePoint;
    property IsUsed: boolean read FUsed;
  end;

function ProjectionToOpenGL(AProj: TProjection3D; ANear, AFar: Single): TMatrix4D;

Implementation
End.
