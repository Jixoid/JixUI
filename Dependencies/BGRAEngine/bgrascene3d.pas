// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAScene3D;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRABitmapTypes, BGRAColorInt,
  BGRASSE, BGRAMatrix3D,
  BGRASceneTypes, BGRARenderer3D;

type
  TProjection3D = BGRAMatrix3D.TProjection3D;
  TLightingNormal3D = BGRASceneTypes.TLightingNormal3D;
  TLightingInterpolation3D = BGRASceneTypes.TLightingInterpolation3D;
  TAntialiasingMode3D = BGRASceneTypes.TAntialiasingMode3D;
  TPerspectiveMode3D = BGRASceneTypes.TPerspectiveMode3D;
  TRenderingOptions = BGRASceneTypes.TRenderingOptions;

  IBGRAVertex3D = BGRASceneTypes.IBGRAVertex3D;
  IBGRANormal3D = BGRASceneTypes.IBGRANormal3D;
  IBGRALight3D = BGRASceneTypes.IBGRALight3D;
  IBGRADirectionalLight3D = BGRASceneTypes.IBGRADirectionalLight3D;
  IBGRAPointLight3D = BGRASceneTypes.IBGRAPointLight3D;
  IBGRAMaterial3D = BGRASceneTypes.IBGRAMaterial3D;
  IBGRAFace3D = BGRASceneTypes.IBGRAFace3D;
  IBGRAPart3D = BGRASceneTypes.IBGRAPart3D;
  IBGRAObject3D = BGRASceneTypes.IBGRAObject3D;

  arrayOfIBGRAVertex3D = BGRASceneTypes.arrayOfIBGRAVertex3D;

const
  lnNone = BGRASceneTypes.lnNone;
  lnFace = BGRASceneTypes.lnFace;
  lnVertex = BGRASceneTypes.lnVertex;
  lnFaceVertexMix = BGRASceneTypes.lnFaceVertexMix;

  liLowQuality = BGRASceneTypes.liLowQuality;
  liSpecularHighQuality = BGRASceneTypes.liSpecularHighQuality;
  liAlwaysHighQuality = BGRASceneTypes.liAlwaysHighQuality;

  am3dNone = BGRASceneTypes.am3dNone;
  am3dMultishape = BGRASceneTypes.am3dMultishape;
  am3dResample = BGRASceneTypes.am3dResample;

  pmLinearMapping = BGRASceneTypes.pmLinearMapping;
  pmPerspectiveMapping = BGRASceneTypes.pmPerspectiveMapping;
  pmZBuffer = BGRASceneTypes.pmZBuffer;

type

  { TCamera3D }

  TCamera3D = class
  private
    procedure ComputeMatrix;
    function GetLookWhere: TPoint3D;
    function GetMatrix: TMatrix3D;
    function GetViewPoint: TPoint3D;
    procedure SetMatrix(AValue: TMatrix3D);
    procedure SetViewPoint(AValue: TPoint3D);
  protected
    FMatrix: TMatrix3D;
    FMatrixComputed: boolean;
    FViewPoint: TPoint3D_128;
    FLookWhere, FTopDir: TPoint3D_128;
  public
    procedure LookAt(AWhere: TPoint3D; ATopDir: TPoint3D);
    procedure LookDown(angleDeg: single);
    procedure LookLeft(angleDeg: single);
    procedure LookRight(angleDeg: single);
    procedure LookUp(angleDeg: single);
    property ViewPoint: TPoint3D read GetViewPoint write SetViewPoint;
    property LookWhere: TPoint3D read GetLookWhere;
    property Matrix: TMatrix3D read GetMatrix write SetMatrix;
  end;

  { TBGRAScene3D }

  TBGRAScene3D = class
  private
    FSurface: TBGRACustomBitmap; //destination of software renderer
    FViewCenter: TPointF;        //where origin is drawn
    FAutoViewCenter: boolean;    //use middle of the screen
    FZoom: TPointF;              //how much the drawing is zoomed
    FAutoZoom: Boolean;          //display 1 as 80% of surface size
    FProjection: TProjection3D;  //current projection
    FRenderedFaceCount: integer; //current counter of rendered faces

    FCamera: TCamera3D;

    FObjects: array of IBGRAObject3D;
    FObjectCount: integer;
    FMaterials: array of IBGRAMaterial3D;
    FMaterialCount: integer;
    FDefaultMaterial : IBGRAMaterial3D;

    FAmbiantLightColorF: TColorF;        //lightness without light sources
    FLights: TList;                      //individual light sources

    function GetAmbiantLightColorF: TColorF;
    function GetAmbiantLightness: single;
    function GetAmbiantLightColor: TBGRAPixel;
    function GetFaceCount: integer;
    function GetLight(AIndex: integer): IBGRALight3D;
    function GetLightCount: integer;
    function GetMaterial(AIndex: integer): IBGRAMaterial3D;
    function GetNormalCount: integer;
    function GetObject(AIndex: integer): IBGRAObject3D;
    function GetVertexCount: integer;
    function GetViewCenter: TPointF;
    function GetViewPoint: TPoint3D;
    function GetZoom: TPointF;
    procedure SetAmbiantLightColorF(const AValue: TColorF);
    procedure SetAmbiantLightness(const AValue: single);
    procedure SetAmbiantLightColor(const AValue: TBGRAPixel);
    procedure SetAutoViewCenter(const AValue: boolean);
    procedure SetAutoZoom(const AValue: boolean);
    procedure SetViewCenter(const AValue: TPointF);
    procedure SetViewPoint(const AValue: TPoint3D);
    procedure ComputeView(ScaleX,ScaleY: single);
    function ComputeCoordinate(AViewCoord: TPoint3D_128): TPointF;
    procedure AddObject(AObj: IBGRAObject3D);
    procedure AddLight(ALight: TObject);
    procedure AddMaterial(AMaterial: IBGRAMaterial3D);
    procedure Init;

  protected
    FRenderer: TCustomRenderer3D;
    FMaterialLibrariesFetched: array of string;
    FTexturesFetched: array of record
        Name: string;
        Bitmap: TBGRACustomBitmap;
      end;
    procedure UseMaterial(AMaterialName: string; AFace: IBGRAFace3D); virtual;
    function LoadBitmapFromFileUTF8(AFilenameUTF8: string): TBGRACustomBitmap; virtual;
    function FetchTexture(AName: string; out texSize: TPointF): IBGRAScanner; virtual;
    procedure HandleFetchException(AException: Exception); virtual;
    procedure DoRender; virtual;
    procedure DoClear; virtual;
    function GetRenderWidth: integer;
    function GetRenderHeight: integer;
    procedure OnMaterialTextureChanged({%H-}ASender: TObject); virtual;
    procedure SetDefaultMaterial(AValue: IBGRAMaterial3D);
    procedure InvalidateMaterial;

  public
    DefaultLightingNormal: TLightingNormal3D;
    RenderingOptions: TRenderingOptions;
    UnknownColor: TBGRAPixel;
    FetchDirectory: string;
    FetchThrowsException: boolean;

    constructor Create; overload;
    constructor Create(ASurface: TBGRACustomBitmap); overload;
    destructor Destroy; override;
    procedure Clear; virtual;
    function FetchObject(AName: string; SwapFacesOrientation: boolean = true): IBGRAObject3D;
    procedure FetchMaterials(ALibraryName: string); virtual;
    function LoadObjectFromFile(AFilename: string; SwapFacesOrientation: boolean = true): IBGRAObject3D;
    function LoadObjectFromFileUTF8(AFilename: string; SwapFacesOrientation: boolean = true): IBGRAObject3D;
    function LoadObjectFromStream(AStream: TStream; SwapFacesOrientation: boolean = true): IBGRAObject3D;
    procedure LoadMaterialsFromFile(AFilename: string);
    procedure LoadMaterialsFromFileUTF8(AFilename: string);
    procedure LoadMaterialsFromStream(AStream: TStream);
    procedure LookAt(AWhere: TPoint3D; ATopDir: TPoint3D);
    procedure LookLeft(angleDeg: single);
    procedure LookRight(angleDeg: single);
    procedure LookUp(angleDeg: single);
    procedure LookDown(angleDeg: single);
    procedure Render;  overload; virtual;
    procedure Render(ARenderer: TCustomRenderer3D); overload;
    function CreateObject: IBGRAObject3D; overload;
    function CreateObject(ATexture: IBGRAScanner): IBGRAObject3D; overload;
    function CreateObject(AColor: TBGRAPixel): IBGRAObject3D; overload;
    function CreateSphere(ARadius: Single; AHorizPrecision: integer = 8; AVerticalPrecision : integer = 6): IBGRAObject3D; overload;
    function CreateSphere(ARadius: Single; AColor: TBGRAPixel; AHorizPrecision: integer = 8; AVerticalPrecision : integer = 6): IBGRAObject3D; overload;
    function CreateHalfSphere(ARadius: Single; AHorizPrecision: integer = 6; AVerticalPrecision : integer = 6): IBGRAObject3D; overload;
    function CreateHalfSphere(ARadius: Single; AColor: TBGRAPixel; AHorizPrecision: integer = 6; AVerticalPrecision : integer = 6): IBGRAObject3D; overload;
    procedure RemoveObject(AObject: IBGRAObject3D);
    function AddDirectionalLight(ADirection: TPoint3D; ALightness: single = 1; AMinIntensity : single = 0): IBGRADirectionalLight3D; overload;
    function AddDirectionalLight(ADirection: TPoint3D; AColor: TBGRAPixel; AMinIntensity: single = 0): IBGRADirectionalLight3D; overload;
    function AddPointLight(AVertex: IBGRAVertex3D; AOptimalDistance: single; ALightness: single = 1; AMinIntensity : single = 0): IBGRAPointLight3D; overload;
    function AddPointLight(AVertex: IBGRAVertex3D; AOptimalDistance: single; AColor: TBGRAPixel; AMinIntensity: single = 0): IBGRAPointLight3D; overload;
    procedure RemoveLight(ALight: IBGRALight3D);
    procedure SetZoom(value: Single); overload;
    procedure SetZoom(value: TPointF); overload;
    function CreateMaterial: IBGRAMaterial3D; overload;
    function CreateMaterial(ASpecularIndex: integer): IBGRAMaterial3D; overload;
    function GetMaterialByName(AName: string): IBGRAMaterial3D;
    procedure UpdateMaterials; virtual;
    procedure UpdateMaterial(AMaterialName: string); virtual;
    procedure ForEachVertex(ACallback: TVertex3DCallback);
    procedure ForEachFace(ACallback: TFace3DCallback);
    function MakeLightList: TList;

    property ViewCenter: TPointF read GetViewCenter write SetViewCenter;
    property AutoViewCenter: boolean read FAutoViewCenter write SetAutoViewCenter;
    property AutoZoom: boolean read FAutoZoom write SetAutoZoom;
    property Surface: TBGRACustomBitmap read FSurface write FSurface;
    property Object3D[AIndex: integer]: IBGRAObject3D read GetObject;
    property Object3DCount: integer read FObjectCount;
    property VertexCount: integer read GetVertexCount;
    property NormalCount: integer read GetNormalCount;
    property FaceCount: integer read GetFaceCount;
    property Zoom: TPointF read GetZoom write SetZoom;
    property AmbiantLightness: single read GetAmbiantLightness write SetAmbiantLightness;
    property AmbiantLightColor: TBGRAPixel read GetAmbiantLightColor write SetAmbiantLightColor;
    property AmbiantLightColorF: TColorF read GetAmbiantLightColorF write SetAmbiantLightColorF;
    property LightCount: integer read GetLightCount;
    property Light[AIndex: integer]: IBGRALight3D read GetLight;
    property ViewPoint: TPoint3D read GetViewPoint write SetViewPoint;
    property RenderedFaceCount : integer read FRenderedFaceCount;
    property Material[AIndex: integer] : IBGRAMaterial3D read GetMaterial;
    property MaterialCount: integer read FMaterialCount;
    property Camera: TCamera3D read FCamera;
    property DefaultMaterial: IBGRAMaterial3D read FDefaultMaterial write SetDefaultMaterial;
  end;

Implementation
End.
