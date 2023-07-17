// SPDX-License-Identifier: LGPL-3.0-linking-exception
{
  Created by BGRA Controls Team
  Dibo, Circular, lainz (007) and contributors.
  For detailed information see readme.txt

  Site: https://sourceforge.net/p/bgra-controls/
  Wiki: http://wiki.lazarus.freepascal.org/BGRAControls
  Forum: http://forum.lazarus.freepascal.org/index.php/board,46.0.html
}
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)
- FreeMan35

***************************** END CONTRIBUTOR(S) *****************************}
unit BGRASpriteAnimation;

{$I bgracontrols.map.inc}

interface

uses
  Classes, Controls, Dialogs, ExtCtrls, Forms, {$IFDEF FPC}LCLIntF, LResources,{$ENDIF} Graphics,
  {$IFNDEF FPC}Types, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  BCBaseCtrls, BGRABitmap, BGRABitmapTypes, BCTypes, BGRAAnimatedGif;

type
  TBGRASpriteAnimation = class;

  { TSpriteBitmap }

  TSpriteBitmap = class(TBitmap)
  private
    FOwner: TBGRASpriteAnimation;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(AOwner: TBGRASpriteAnimation); overload;
    procedure Assign(Source: TPersistent); override;
  end;

  TFlipMode = (flNone, flHorizontal, flVertical, flBoth);
  TRotationMode = (rtNone, rtClockWise, rtCounterClockWise, rt180);

  { TBGRASpriteAnimation }

  TBGRASpriteAnimation = class(TBGRAGraphicCtrl)
  private
    { Private declarations }
    FAnimInvert: boolean;
    FAnimPosition: cardinal;
    FAnimRepeat: cardinal;
    FAnimRepeatLap: cardinal;
    FAnimSpeed: cardinal;
    FAnimStatic: boolean;
    FAnimTimer: TTimer;
    FCenter: boolean;
    FOnLapChanged: TNotifyEvent;
    FOnLapChanging: TNotifyEvent;
    FOnPositionChanged: TNotifyEvent;
    FOnPositionChanging: TNotifyEvent;
    FOnRedrawAfter: TBGRARedrawEvent;
    FOnRedrawBefore: TBGRARedrawEvent;
    FProportional: boolean;
    FSprite: TBitmap;
    FSpriteCount: cardinal;
    FSpriteFillOpacity: byte;
    FSpriteFlipMode: TFlipMode;
    FSpriteKeyColor: TColor;
    FSpriteResampleFilter: TResampleFilter;
    FSpriteResampleMode: TResampleMode;
    FSpriteRotation: TRotationMode;
    FStretch: boolean;
    FTile: boolean;
    function DoCalculateDestRect(AWidth, AHeight: integer): TRect;
    function DoCalculatePosition(AValue: integer): integer;
    function DoCalculateSize(AValue: cardinal): cardinal;
    procedure DoAnimTimerOnTimer({%H-}Sender: TObject);
    procedure DoSpriteDraw(ABitmap: TBGRABitmap);
    procedure DoSpriteFillOpacity(ABitmap: TBGRABitmap);
    procedure DoSpriteFlip(ABitmap: TBGRABitmap);
    procedure DoSpriteKeyColor(ABitmap: TBGRABitmap);
    procedure DoSpriteResampleFilter(ABitmap: TBGRABitmap);
    procedure SetFAnimInvert(const AValue: boolean);
    procedure SetFAnimPosition(const AValue: cardinal);
    procedure SetFAnimRepeat(const AValue: cardinal);
    procedure SetFAnimRepeatLap(const AValue: cardinal);
    procedure SetFAnimSpeed(const AValue: cardinal);
    procedure SetFAnimStatic(const AValue: boolean);
    procedure SetFCenter(const AValue: boolean);
    procedure SetFProportional(const AValue: boolean);
    procedure SetFSprite(const AValue: TBitmap);
    procedure SetFSpriteCount(const AValue: cardinal);
    procedure SetFSpriteFillOpacity(const AValue: byte);
    procedure SetFSpriteFlipMode(const AValue: TFlipMode);
    procedure SetFSpriteKeyColor(const AValue: TColor);
    procedure SetFSpriteResampleFilter(const AValue: TResampleFilter);
    procedure SetFSpriteResampleMode(const AValue: TResampleMode);
    procedure SetFSpriteRotation(const AValue: TRotationMode);
    procedure SetFStretch(const AValue: boolean);
    procedure SetFTile(const AValue: boolean);
    procedure SpriteChange(Sender: TObject);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      {%H-}WithThemeSpace: Boolean); override;
  public
    { Public declarations }
    procedure GifImageToSprite(Gif: TBGRAAnimatedGif);
    procedure SpriteToGifImage(Gif: TBGRAAnimatedGif);
    procedure LoadFromResourceName(Instance: THandle; const ResName: string); overload;
    procedure LoadFromBitmapResource(const Resource: string); overload;
    {$IF BGRABitmapVersion > 11030100}
    procedure LoadFromBitmapStream(AStream: TStream);
    {$ENDIF}
    procedure LoadFromBGRABitmap(const BGRA: TBGRABitmap);
    procedure SpriteToAnimatedGif(Filename: string);
    procedure AnimatedGifToSprite(Filename: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property AnimInvert: boolean read FAnimInvert write SetFAnimInvert;
    property AnimPosition: cardinal read FAnimPosition write SetFAnimPosition;
    property AnimRepeat: cardinal read FAnimRepeat write SetFAnimRepeat;
    property AnimRepeatLap: cardinal read FAnimRepeatLap write SetFAnimRepeatLap;
    property AnimSpeed: cardinal read FAnimSpeed write SetFAnimSpeed;
    property AnimStatic: boolean read FAnimStatic write SetFAnimStatic;
    property Center: boolean read FCenter write SetFCenter;
    property Proportional: boolean read FProportional write SetFProportional;
    property Sprite: TBitmap read FSprite write SetFSprite;
    property SpriteCount: cardinal read FSpriteCount write SetFSpriteCount;
    property SpriteFillOpacity: byte read FSpriteFillOpacity write SetFSpriteFillOpacity;
    property SpriteFlipMode: TFlipMode read FSpriteFlipMode write SetFSpriteFlipMode;
    property SpriteKeyColor: TColor read FSpriteKeyColor write SetFSpriteKeyColor;
    property SpriteResampleFilter: TResampleFilter
      read FSpriteResampleFilter write SetFSpriteResampleFilter;
    property SpriteResampleMode: TResampleMode
      read FSpriteResampleMode write SetFSpriteResampleMode;
    property SpriteRotation: TRotationMode read FSpriteRotation write SetFSpriteRotation;
    property Stretch: boolean read FStretch write SetFStretch;
    property Tile: boolean read FTile write SetFTile;
  published
    property Align;
    property Anchors;
    property AutoSize;
    property Caption;
    property Color;
    property Enabled;
    property OnClick;
    property OnDblClick;
    property OnLapChanged: TNotifyEvent read FOnLapChanged write FOnLapChanged;
    property OnLapChanging: TNotifyEvent read FOnLapChanging write FOnLapChanging;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnPositionChanged: TNotifyEvent
      read FOnPositionChanged write FOnPositionChanged;
    property OnPositionChanging: TNotifyEvent
      read FOnPositionChanging write FOnPositionChanging;
    property OnRedrawAfter: TBGRARedrawEvent read FOnRedrawAfter write FOnRedrawAfter;
    property OnRedrawBefore: TBGRARedrawEvent read FOnRedrawBefore write FOnRedrawBefore;
    property PopupMenu;
    property Visible;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.
