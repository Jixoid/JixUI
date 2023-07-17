// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRACanvasGL;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRAGraphics, BGRABitmapTypes,
  BGRAOpenGLType, BGRATransform, BGRAPath,
  BGRASSE, BGRAMatrix3D;

type
  TBGLPath = class;
  TBGLCustomCanvas = class;

  TBGLCustomShader = class
  protected
    procedure StartUse; virtual; abstract;
    procedure EndUse; virtual; abstract;
  end;

  TBGLCustomArray = class
  protected
    FBuffer: LongWord;
    function GetCount: integer; virtual; abstract;
    function GetRecordSize: integer; virtual; abstract;
  public
    constructor Create(ABufferAddress: pointer; ACount: integer; ARecordSize: integer); virtual; abstract;
    property Count: integer read GetCount;
    property RecordSize: integer read GetRecordSize;
    property Handle: LongWord read FBuffer;
  end;

  { TAttributeVariable }

  TAttributeVariable = object
  protected
    FOwner: TObject;
    FAttribute: LongWord;
    FVectorSize: integer;
    FArray: TBGLCustomArray;
    FRecordOffset: integer;
    FFloat: boolean;
    procedure Init(AOwner: TObject; AAttribute: LongWord; AVectorSize: integer;
              AFloat: boolean);
  public
    property Source: TBGLCustomArray read FArray write FArray;
    property RecordOffset: integer read FRecordOffset write FRecordOffset;
    property Handle: LongWord read FAttribute;
    property VectorSize: integer read FVectorSize;
    property IsFloat: boolean read FFloat;
    property Owner: TObject read FOwner;
  end;

  TBGLCustomElementArray = class
  protected
    function GetCount: integer; virtual; abstract;
  public
    constructor Create(const AElements: array of integer); virtual; abstract;
    procedure Draw(ACanvas: TBGLCustomCanvas; APrimitive: TOpenGLPrimitive; AAttributes: array of TAttributeVariable); virtual; abstract;
    property Count: integer read GetCount;
  end;

  { TBGLCustomLighting }

  TBGLCustomLighting = class
  private
    FCurrentShader: TBGLCustomShader;
    function GetActiveShader: TBGLCustomShader;
    procedure SetActiveShader(AValue: TBGLCustomShader);
  protected
    function GetSupportShaders: boolean; virtual;
    function GetShader(AName: string): TBGLCustomShader;
    procedure SetShader(AName: string; AValue: TBGLCustomShader);
    procedure SetAmbiantLightF(AAmbiantLight: TColorF); virtual; abstract;
    function GetAmbiantLightF: TColorF; virtual; abstract;
    function GetBuiltInLightingEnabled: boolean; virtual; abstract;
    procedure SetBuiltInLightingEnabled(AValue: boolean); virtual; abstract;
  public
    ShaderList: TStringList;
    destructor Destroy; override;
    function AddDirectionalLight(AColor: TColorF; ADirection: TPoint3D): integer; virtual; abstract;
    function AddPointLight(AColor: TColorF; APosition: TPoint3D; ALinearAttenuation, AQuadraticAttenuation: single): integer; virtual; abstract;
    procedure ClearLights; virtual; abstract;
    function RemoveLight(AIndex: integer): boolean; virtual; abstract;
    procedure SetSpecularIndex(AIndex: integer); virtual; abstract;

    function MakeVertexShader(ASource: string): LongWord; virtual; abstract;
    function MakeFragmentShader(ASource: string): LongWord; virtual; abstract;
    function MakeShaderProgram(AVertexShader, AFragmentShader: LongWord): LongWord; virtual; abstract;
    procedure DeleteShaderObject(AShader: LongWord); virtual; abstract;
    procedure DeleteShaderProgram(AProgram: LongWord); virtual; abstract;
    procedure UseProgram(AProgram: LongWord); virtual; abstract;
    function GetUniformVariable(AProgram: LongWord; AName: string): LongWord; virtual; abstract;
    function GetAttribVariable(AProgram: LongWord; AName: string): LongWord; virtual; abstract;
    procedure SetUniformSingle(AVariable: LongWord; const AValue; AElementCount, AComponentCount: integer); virtual; abstract;
    procedure SetUniformInteger(AVariable: LongWord; const AValue; AElementCount, AComponentCount: integer); virtual; abstract;
    procedure BindAttribute(AAttribute: TAttributeVariable); virtual; abstract;
    procedure UnbindAttribute(AAttribute: TAttributeVariable); virtual; abstract;
    procedure FreeShaders;
    property ActiveShader: TBGLCustomShader read GetActiveShader write SetActiveShader;
    property Shader[AName: string]: TBGLCustomShader read GetShader write SetShader;
    property SupportShaders: boolean read GetSupportShaders;
    property AmbiantLightF: TColorF read GetAmbiantLightF write SetAmbiantLightF;
    property BuiltInLightingEnabled: boolean read GetBuiltInLightingEnabled write SetBuiltInLightingEnabled;
  end;

  { TBGLCustomCanvas }

  TBGLCustomCanvas = class
  private
    FActiveFrameBuffer: TBGLCustomFrameBuffer;
    FHeight: integer;
    FWidth: integer;
    FNoClip: boolean;
    FClipRect: TRect;
  protected
    procedure SwapRect(var r: TRect); overload;
    procedure SwapRect(var x1,y1,x2,y2: single); overload;
    procedure InternalArc(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; ABorderColor,AOuterFillColor,ACenterFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false); overload;
    procedure InternalArc(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; ABorderColor,AOuterFillColor,ACenterFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false); overload;
    procedure InternalArcInRect(r: TRect; StartAngleRad,EndAngleRad: Single; ABorderColor,AOuterFillColor,ACenterFillColor: TBGRAPixel; AOptions: TArcOptions; ADrawChord: boolean = false); overload;
    function ComputeEllipseC(r: TRect; AHasBorder: boolean; out cx,cy,rx,ry: single): boolean;
    function GetHeight: integer; virtual;
    function GetWidth: integer; virtual;
    procedure SetWidth(AValue: integer); virtual;
    procedure SetHeight(AValue: integer); virtual;
    function GetClipRect: TRect;
    procedure SetClipRect(AValue: TRect);
    procedure EnableScissor(AValue: TRect); virtual; abstract;
    procedure DisableScissor; virtual; abstract;
    function GetMatrix: TAffineMatrix; virtual; abstract;
    procedure SetMatrix(const AValue: TAffineMatrix); virtual; abstract;
    function GetProjectionMatrix: TMatrix4D; virtual;
    procedure SetProjectionMatrix(const {%H-}AValue: TMatrix4D); virtual;
    procedure SetBlendMode(AValue: TOpenGLBlendMode); virtual; abstract;
    function GetBlendMode: TOpenGLBlendMode; virtual; abstract;
    function GetFaceCulling: TFaceCulling; virtual; abstract;
    procedure SetFaceCulling(AValue: TFaceCulling); virtual; abstract;
    procedure SetActiveFrameBuffer(AValue: TBGLCustomFrameBuffer); virtual;

    function GetLighting: TBGLCustomLighting; virtual;

    procedure InternalStartPutPixel(const pt: TPointF); virtual; abstract;
    procedure InternalStartPolyline(const pt: TPointF); virtual; abstract;
    procedure InternalStartPolygon(const pt: TPointF); virtual; abstract;
    procedure InternalStartTriangleFan(const pt: TPointF); virtual; abstract;
    procedure InternalContinueShape(const pt: TPointF); overload; virtual; abstract;

    procedure InternalContinueShape(const {%H-}pt: TPoint3D); overload; virtual;
    procedure InternalContinueShape(const {%H-}pt: TPoint3D_128); overload; virtual;
    procedure InternalContinueShape(const {%H-}pt, {%H-}normal: TPoint3D_128); overload; virtual;

    procedure InternalEndShape; virtual; abstract;
    procedure InternalSetColor(const AColor: TBGRAPixel); virtual; abstract;
    procedure InternalSetColorF(const AColor: TColorF); virtual; abstract;

    procedure InternalStartBlend; virtual; abstract;
    procedure InternalEndBlend; virtual; abstract;

    procedure InternalStartBlendTriangles; virtual; abstract;
    procedure InternalStartBlendQuads; virtual; abstract;
    procedure InternalEndBlendTriangles; virtual; abstract;
    procedure InternalEndBlendQuads; virtual; abstract;
  public
    constructor Create;
    procedure Fill(AColor: TBGRAPixel); virtual; abstract;

    procedure PutPixels(const APoints: array of TPointF; AColor: TBGRAPixel); overload; virtual;
    procedure PutPixels(const APoints: array of TPointF; const AColors: array of TBGRAPixel); overload; virtual;

    procedure Line(x1,y1,x2,y2: single; AColor: TBGRAPixel; ADrawLastPoint: boolean = true); overload;
    procedure Line(p1,p2: TPointF; AColor: TBGRAPixel; ADrawLastPoint: boolean = true); overload;
    procedure Polylines(const APoints: array of TPointF; AColor: TBGRAPixel; ADrawLastPoints: boolean = true); virtual;

    procedure Polygons(const APoints: array of TPointF; AColor: TBGRAPixel); virtual;
    procedure FillPolyConvex(const APoints: array of TPointF; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true);

    procedure FillTriangleLinearColor(pt1,pt2,pt3: TPointF; c1,c2,c3: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload;
    procedure FillTriangles(const APoints: array of TPointF; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillTrianglesLinearColor(const APoints: array of TPointF; const AColors: array of TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillTrianglesLinearColor(const APoints: array of TPoint3D; const AColors: array of TBGRAPixel); overload; virtual;
    procedure FillTrianglesLinearColor(const APoints: array of TPoint3D_128; const AColors: array of TBGRAPixel); overload; virtual;
    procedure FillTrianglesLinearColor(const APoints, ANormals: array of TPoint3D_128; const AColors: array of TBGRAPixel); overload; virtual;
    procedure FillTrianglesFan(const APoints: array of TPointF; ACenterColor, ABorderColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload; virtual;

    procedure FillTriangleLinearColor(pt1,pt2,pt3: TPointF; c1,c2,c3: TColorF; APixelCenteredCoordinates: boolean = true); overload;
    procedure FillTriangles(const APoints: array of TPointF; AColor: TColorF; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillTrianglesLinearColor(const APoints: array of TPointF; const AColors: array of TColorF; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillTrianglesLinearColor(const APoints: array of TPoint3D; const AColors: array of TColorF); overload; virtual;
    procedure FillTrianglesLinearColor(const APoints: array of TPoint3D_128; const AColors: array of TColorF); overload; virtual;
    procedure FillTrianglesLinearColor(const APoints, ANormals: array of TPoint3D_128; const AColors: array of TColorF); overload; virtual;
    procedure FillTrianglesFan(const APoints: array of TPointF; ACenterColor, ABorderColor: TColorF; APixelCenteredCoordinates: boolean = true); overload; virtual;

    procedure FillQuadLinearColor(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload;
    procedure FillQuads(const APoints: array of TPointF; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillQuadsLinearColor(const APoints: array of TPointF; const AColors: array of TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillQuadsLinearColor(const APoints: array of TPoint3D; const AColors: array of TBGRAPixel); overload; virtual;
    procedure FillQuadsLinearColor(const APoints: array of TPoint3D_128; const AColors: array of TBGRAPixel); overload; virtual;
    procedure FillQuadsLinearColor(const APoints, ANormals: array of TPoint3D_128; const AColors: array of TBGRAPixel); overload; virtual;

    procedure FillQuadLinearColor(pt1,pt2,pt3,pt4: TPointF; c1,c2,c3,c4: TColorF; APixelCenteredCoordinates: boolean = true); overload;
    procedure FillQuads(const APoints: array of TPointF; AColor: TColorF; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillQuadsLinearColor(const APoints: array of TPointF; const AColors: array of TColorF; APixelCenteredCoordinates: boolean = true); overload; virtual;
    procedure FillQuadsLinearColor(const APoints: array of TPoint3D; const AColors: array of TColorF); overload; virtual;
    procedure FillQuadsLinearColor(const APoints: array of TPoint3D_128; const AColors: array of TColorF); overload; virtual;
    procedure FillQuadsLinearColor(const APoints, ANormals: array of TPoint3D_128; const AColors: array of TColorF); overload; virtual;

    procedure DrawPath(APath: TBGLPath; c: TBGRAPixel);
    procedure FillPathConvex(APath: TBGLPath; c: TBGRAPixel; APixelCenteredCoordinates: boolean = true);

    procedure FillRectLinearColor(r: TRect; ATopLeftColor, ATopRightColor, ABottomRightColor, ABottomLeftColor: TBGRAPixel); overload; virtual;
    procedure FillRectLinearColor(x1,y1,x2,y2: single;
         ATopLeftColor, ATopRightColor, ABottomRightColor, ABottomLeftColor: TBGRAPixel;
         APixelCenteredCoordinates: boolean = true); overload; virtual;

    procedure Ellipse(cx,cy,rx,ry: single; AColor: TBGRAPixel); overload;
    procedure EllipseInRect(r: TRect; AColor: TBGRAPixel); overload;
    procedure Ellipse(cx,cy,rx,ry: single; AColor: TBGRAPixel; AFillColor: TBGRAPixel); overload;
    procedure EllipseInRect(r: TRect; AColor: TBGRAPixel; AFillColor: TBGRAPixel); overload;
    procedure EllipseLinearColor(cx,cy,rx,ry: single; AColor: TBGRAPixel; AOuterFillColor, AInnerFillColor: TBGRAPixel); overload;
    procedure EllipseLinearColorInRect(r: TRect; AColor: TBGRAPixel; AOuterFillColor, AInnerFillColor: TBGRAPixel); overload;
    procedure FillEllipse(cx,cy,rx,ry: single; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true);
    procedure FillEllipseInRect(r: TRect; AColor: TBGRAPixel);
    procedure FillEllipseLinearColor(cx, cy, rx, ry: single; AOuterColor, AInnerColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true);
    procedure FillEllipseLinearColorInRect(r: TRect; AOuterColor, AInnerColor: TBGRAPixel);

    procedure Arc(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AColor: TBGRAPixel; ADrawChord: boolean; AFillColor: TBGRAPixel); overload;
    procedure Arc(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; ADrawChord: boolean; AFillColor: TBGRAPixel); overload;
    procedure ArcInRect(r: TRect; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; ADrawChord: boolean; AFillColor: TBGRAPixel);
    procedure ArcLinearColor(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AColor: TBGRAPixel; ADrawChord: boolean; AOuterFillColor, AInnerFillColor: TBGRAPixel); overload;
    procedure ArcLinearColor(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; ADrawChord: boolean; AOuterFillColor, AInnerFillColor: TBGRAPixel); overload;
    procedure ArcLinearColorInRect(r: TRect; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; ADrawChord: boolean; AOuterFillColor, AInnerFillColor: TBGRAPixel);

    procedure Pie(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AColor: TBGRAPixel; AFillColor: TBGRAPixel); overload;
    procedure Pie(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; AFillColor: TBGRAPixel); overload;
    procedure PieInRect(r: TRect; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; AFillColor: TBGRAPixel);
    procedure PieLinearColor(cx,cy,rx,ry: single; const StartPoint,EndPoint: TPointF; AColor: TBGRAPixel; AOuterFillColor, AInnerFillColor: TBGRAPixel); overload;
    procedure PieLinearColor(cx,cy,rx,ry: single; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; AOuterFillColor, AInnerFillColor: TBGRAPixel); overload;
    procedure PieLinearColorInRect(r: TRect; StartAngleRad,EndAngleRad: Single; AColor: TBGRAPixel; AOuterFillColor, AInnerFillColor: TBGRAPixel);

    procedure Rectangle(r: TRect; AColor: TBGRAPixel); overload;
    procedure Rectangle(r: TRect; AColor: TBGRAPixel; AFillColor: TBGRAPixel); overload;
    procedure Rectangle(x1,y1,x2,y2: single; AColor: TBGRAPixel); overload;
    procedure Rectangle(x1,y1,x2,y2: single; AColor: TBGRAPixel; AFillColor: TBGRAPixel); overload;
    procedure Rectangle(x1,y1,x2,y2: single; AColor: TBGRAPixel; w: single; APixelCenteredCoordinates: boolean = true); overload;
    procedure Rectangle(x1,y1,x2,y2: single; AColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload;
    procedure RectangleWithin(x1,y1,x2,y2: single; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload;
    procedure RectangleWithin(r: TRect; ABorderColor: TBGRAPixel; w: single; AFillColor: TBGRAPixel); overload;
    procedure FillRect(x1,y1,x2,y2: single; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean = true); overload;
    procedure FillRect(r: TRect; AColor: TBGRAPixel); overload;
    procedure FillRect(r: TRectF; AColor: TBGRAPixel; APixelCenteredCoordinates: boolean = false); overload;
    procedure FillRect(r: TRect; AScanner: IBGRAScanner); overload; virtual;
    procedure RoundRect(x1,y1,x2,y2,rx,ry: single; ABorderColor: TBGRAPixel; options: TRoundRectangleOptions = []); overload;
    procedure RoundRect(x1,y1,x2,y2,rx,ry: single; ABorderColor,AFillColor: TBGRAPixel; options: TRoundRectangleOptions = []); overload;
    procedure FillRoundRect(x,y,x2,y2,rx,ry: single; AFillColor: TBGRAPixel; options: TRoundRectangleOptions = []; APixelCenteredCoordinates: boolean = true);

    procedure Frame3D(var bounds: TRect; width: integer; Style: TGraphicsBevelCut); overload;
    procedure Frame3D(var bounds: TRect; width: integer;
      Style: TGraphicsBevelCut; LightColor: TBGRAPixel; ShadowColor: TBGRAPixel); overload;

    procedure PutImage(x,y: single; ATexture: IBGLTexture; AAlpha: byte = 255); overload;
    procedure PutImage(x,y: single; ATexture: IBGLTexture; AColor: TBGRAPixel); overload;
    procedure StretchPutImage(x,y,w,h: single; ATexture: IBGLTexture; AAlpha: byte = 255); overload;
    procedure StretchPutImage(x,y,w,h: single; ATexture: IBGLTexture; AColor: TBGRAPixel); overload;
    procedure StretchPutImage(r: TRect; ATexture: IBGLTexture; AAlpha: byte = 255); overload;
    procedure StretchPutImage(r: TRect; ATexture: IBGLTexture; AColor: TBGRAPixel); overload;
    procedure PutImageAngle(x,y: single; ATexture: IBGLTexture; angleDeg: single; AAlpha: byte = 255); overload;
    procedure PutImageAngle(x,y: single; ATexture: IBGLTexture; angleDeg: single; AColor: TBGRAPixel); overload;
    procedure PutImageAffine(const Origin, HAxis, VAxis: TPointF; ATexture: IBGLTexture; AAlpha: byte = 255); overload;
    procedure PutImageAffine(const Origin, HAxis, VAxis: TPointF; ATexture: IBGLTexture; AColor: TBGRAPixel); overload;
    procedure PutImageAffine(x,y: single; ATexture: IBGLTexture; const AMatrix: TAffineMatrix; AAlpha: byte = 255); overload;
    procedure PutImageAffine(x,y: single; ATexture: IBGLTexture; const AMatrix: TAffineMatrix; AColor: TBGRAPixel); overload;

    procedure Translate(x,y: single); virtual;
    procedure Scale(sx,sy: single); virtual;
    procedure RotateDeg(angleCW: single); virtual;
    procedure RotateRad(angleCCW: single); virtual;
    procedure ResetTransform; virtual;

    procedure UseOrthoProjection; overload; virtual;
    procedure UseOrthoProjection(AMinX,AMinY,AMaxX,AMaxY: single); overload; virtual;
    procedure StartZBuffer; virtual;
    procedure EndZBuffer; virtual;
    procedure WaitForGPU({%H-}AOption: TWaitForGPUOption); virtual;

    function GetImage({%H-}x,{%H-}y,{%H-}w,{%H-}h: integer): TBGRACustomBitmap; virtual;
    function CreateFrameBuffer({%H-}AWidth,{%H-}AHeight: integer): TBGLCustomFrameBuffer; virtual;

    procedure NoClip;
    property ActiveFrameBuffer: TBGLCustomFrameBuffer read FActiveFrameBuffer write SetActiveFrameBuffer;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property ClipRect: TRect read GetClipRect write SetClipRect;
    property Matrix: TAffineMatrix read GetMatrix write SetMatrix;
    property ProjectionMatrix: TMatrix4D read GetProjectionMatrix write SetProjectionMatrix;
    property BlendMode: TOpenGLBlendMode read GetBlendMode write SetBlendMode;
    property FaceCulling: TFaceCulling read GetFaceCulling write SetFaceCulling;
    property Lighting: TBGLCustomLighting read GetLighting;
  end;

  { TBGLPath }

  TBGLPath = class(TBGRAPath)
  private
    procedure GLDrawProc(const APoints: array of TPointF; AClosed: boolean; AData: pointer);
    procedure GLFillProc(const APoints: array of TPointF; AData: pointer);
  public
    procedure stroke(ACanvas: TBGLCustomCanvas; AColor: TBGRAPixel; AAcceptedDeviation: single = 0.1); overload;
    procedure fillConvex(ACanvas: TBGLCustomCanvas; AColor: TBGRAPixel; AAcceptedDeviation: single = 0.1; APixelCenteredCoordinates: boolean = true);
  end;

Implementation
End.
