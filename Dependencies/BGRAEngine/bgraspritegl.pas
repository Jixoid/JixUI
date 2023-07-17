// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRASpriteGL;

{$mode objfpc}{$H+}

interface

uses
  BGRAClasses, SysUtils, BGRAOpenGLType,
  BGRABitmapTypes;

type
  { TBGLCustomSprite }

  TBGLCustomSprite = class
  protected
    FHandle: Pointer;
    FTexture: IBGLTexture;
    FFrameLoopStart: integer;
    FFrameLoopEnd : integer;
    procedure SetFrameLoopEnd(AValue: integer);
    procedure SetFrameLoopStart(AValue: integer);
    function GetHorizontalAlign: TAlignment; virtual; abstract;
    function GetVerticalAlign: TTextLayout; virtual; abstract;
    procedure SetHorizontalAlign(AValue: TAlignment); virtual; abstract;
    procedure SetVerticalAlign(AValue: TTextLayout); virtual; abstract;
    function GetAlpha: Integer; virtual; abstract;
    function GetAngle: Single; virtual; abstract;
    function GetColor: TBGRAPixel; virtual; abstract;
    function GetActualFrame: Single; virtual; abstract;
    function GetFrame: Single;
    function GetH: Single; virtual; abstract;
    function GetLayer: Integer; virtual; abstract;
    function GetLocation: TPointF; virtual;
    function GetVisible: Boolean; virtual;
    function GetW: Single; virtual; abstract;
    function GetX: Single; virtual; abstract;
    function GetY: Single; virtual; abstract;
    function GetTexture: IBGLTexture; virtual;
    function GetHandle: Pointer; virtual;
    procedure SetAlpha(AValue: Integer); virtual; abstract;
    procedure SetAngle(AValue: Single); virtual; abstract;
    procedure SetColor(AValue: TBGRAPixel); virtual; abstract;
    procedure SetFrame(AValue: Single);
    procedure SetActualFrame(AValue: Single); virtual; abstract;
    procedure SetH(AValue: Single); virtual; abstract;
    procedure SetLayer(AValue: Integer); virtual; abstract;
    procedure SetLocation(AValue: TPointF); virtual;
    procedure SetW(AValue: Single); virtual; abstract;
    procedure SetVisible({%H-}AValue: boolean); virtual;
    procedure SetX(AValue: Single); virtual; abstract;
    procedure SetY(AValue: Single); virtual; abstract;
    procedure CreateHandle({%H-}ATexture: IBGLTexture; {%H-}ALayer: Integer); virtual;
    procedure OnInit; virtual;
  public
    constructor Create(ATexture: IBGLTexture; ALayer: integer);
    destructor Destroy; override;
    procedure OnDraw; virtual;
    procedure OnElapse({%H-}AElapsedMs: integer); virtual;
    procedure OnTimer; virtual;
    procedure QueryDestroy; virtual; abstract;
    property Layer   : Integer read GetLayer write SetLayer;
    property Location: TPointF read GetLocation write SetLocation;
    property X       : Single read GetX write SetX;
    property Y       : Single read GetY write SetY;
    property W       : Single read GetW write SetW;
    property H       : Single read GetH write SetH;
    property Angle   : Single read GetAngle write SetAngle;
    property Frame   : Single read GetFrame write SetFrame;
    property FrameLoopStart : integer read FFrameLoopStart write SetFrameLoopStart;
    property FrameLoopEnd : integer read FFrameLoopEnd write SetFrameLoopEnd;
    property Alpha   : Integer read GetAlpha write SetAlpha;
    property Color   : TBGRAPixel read GetColor write SetColor;
    property HorizontalAlign: TAlignment read GetHorizontalAlign write SetHorizontalAlign;
    property VerticalAlign: TTextLayout read GetVerticalAlign write SetVerticalAlign;
    property Visible : Boolean read GetVisible write SetVisible;
    property Texture : IBGLTexture read GetTexture;
    property Handle  : Pointer read GetHandle;
  end;

  { TBGLDefaultSprite }

  TBGLDefaultSprite = class(TBGLCustomSprite)
  protected
    FColor  : TBGRAPixel;
    FLocation,FSize: TPointF;
    FAngle,FFrame  : single;
    FHorizontalAlign: TAlignment;
    FVerticalAlign: TTextLayout;
    FQueryDestroy: boolean;
    FLayer: integer;
    FHidden: boolean;
    function GetHorizontalAlign: TAlignment; override;
    function GetVerticalAlign: TTextLayout; override;
    procedure SetHorizontalAlign(AValue: TAlignment); override;
    procedure SetVerticalAlign(AValue: TTextLayout); override;
    function GetAlpha: Integer; override;
    function GetAngle: Single; override;
    function GetColor: TBGRAPixel; override;
    function GetDestroy: Boolean;
    function GetActualFrame: Single; override;
    function GetH: Single; override;
    function GetLayer: Integer; override;
    function GetVisible: Boolean; override;
    function GetW: Single; override;
    function GetX: Single; override;
    function GetY: Single; override;
    procedure SetAlpha(AValue: Integer); override;
    procedure SetAngle(AValue: Single); override;
    procedure SetColor(AValue: TBGRAPixel); override;
    procedure SetDestroy(AValue: Boolean);
    procedure SetActualFrame(AValue: Single); override;
    procedure SetH(AValue: Single); override;
    procedure SetLayer(AValue: Integer); override;
    procedure SetVisible(AValue: boolean); override;
    procedure SetW(AValue: Single); override;
    procedure SetX(AValue: Single); override;
    procedure SetY(AValue: Single); override;
    procedure CreateHandle({%H-}ATexture: IBGLTexture; {%H-}ALayer: Integer); override;
  public
    procedure QueryDestroy; override;
  end;

  { TBGLCustomSpriteEngine }

  TBGLCustomSpriteEngine = class
  protected
    function GetSprite(AIndex: integer): TBGLCustomSprite; virtual; abstract;
    function GetCount: integer; virtual; abstract;
  public
    procedure Add(ASprite: TBGLCustomSprite); virtual; abstract;
    procedure Remove(ASprite: TBGLCustomSprite); virtual; abstract;
    procedure OnDraw; virtual; abstract;
    procedure OnTimer; virtual; abstract;
    procedure OnElapse(AElapsedMs: integer); virtual; abstract;
    procedure Clear; virtual; abstract;
    procedure Delete(AIndex: integer); virtual; abstract;
    property Count: Integer read GetCount;
    property Sprite[AIndex: integer]: TBGLCustomSprite read GetSprite;
  end;

  { TBGLDefaultSpriteEngine }

  TBGLDefaultSpriteEngine = class(TBGLCustomSpriteEngine)
  protected
    FSpriteRemoved: TBGLCustomSprite;
    FSprites: array of TBGLDefaultSprite;
    FSpritesCount: integer;
    function GetSprite(AIndex: integer): TBGLCustomSprite; override;
    function GetCount: integer; override;
  public
    constructor Create;
    procedure Add(ASprite: TBGLCustomSprite); override;
    procedure Remove(ASprite: TBGLCustomSprite); override;
    procedure OnDraw; override;
    procedure OnTimer; override;
    procedure OnElapse(AElapsedMs: integer); override;
    procedure Clear; override;
    procedure Delete(AIndex: integer); override;
  end;

var
  BGLSpriteEngine  : TBGLCustomSpriteEngine;

Implementation
End.
