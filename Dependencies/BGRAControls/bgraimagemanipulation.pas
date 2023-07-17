// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit BGRAImageManipulation;

{ ============================================================================
  BGRAImageManipulation Unit

  originally written in 2011 by - Emerson Cavalcanti <emersoncavalcanti at googlesites>

  ============================================================================
  Description:

  TBGRAImageManipulation is a component designed to make simple changes in an
  image while maintaining the aspect ratio of the final image and allow it to
  cut to reduce the unnecessary edges. The selected area is painted with a
  different transparency level for easy viewing of what will be cut.

  ============================================================================
  History:

  2011-05-03 - Emerson Cavalcanti
             - Initial version

  2011-06-01 - Emerson Cavalcanti
             - Fixed aspect ratio when the image has a dimension smaller than
               the size of the component.
             - Fixed memory leak on temporary bitmaps.
             - Fixed unecessary release of bitmap.
             - Inserted Anchor and Align property on component.
             - Implemented 'Keep aspect Ratio' property. Now you can select an
               area without maintaining the aspect ratio.

  2011-06-03 - Emerson Cavalcanti
             - Improved selection when don't use aspect ratio.
             - Improved response when resize component.
             - Fixed memory leak on resample bitmap.

  2011-06-04 - Circular
             - Fixed divide by zero when calculate aspect ratio on
               getImageRect.

  2011-06-07 - Emerson Cavalcanti
             - Improved function of aspect ratio including a variable to
               provide the value directly in the component, instead of using
               the dimensions of the component as the source of this value.
             - Improved exhibition of anchors on selection.
             - Improved mouse cursor.
             - Included function to get the aspect ratio from image size.
             - Included rotate Left and Right functions.

  2013-10-13 - Massimo Magnano
             - Add multi crop areas
             - Add get Bitmap not resampled (original scale)

  2014-08-04 - lainz-007-
             - Included DataType.inc inside the unit

  2021-03-30 - Massimo Magnano
             - Each CropArea has its own AspectRatio, Add Events, Border Color
  2021-04-30 - Massimo Magnano
             - CropArea list Load/Save, bug fixes
  ============================================================================
}

{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
{$I bgracontrols.map.inc}

interface

uses
  Classes, Contnrs, SysUtils, {$IFDEF FPC}LCLIntf, LResources,{$ENDIF}
  Forms, Controls, Graphics, Dialogs,
  {$IFNDEF FPC}Windows, Messages, BGRAGraphics, GraphType, FPImage, {$ENDIF}
  XMLConf, BCBaseCtrls, BGRABitmap, BGRABitmapTypes, BGRAGradientScanner;

  {$IFNDEF FPC}
const
  crSizeNW      = TCursor(-23);
  crSizeN       = TCursor(-24);
  crSizeNE      = TCursor(-25);
  crSizeW       = TCursor(-26);
  crSizeE       = TCursor(-27);
  crSizeSW      = TCursor(-28);
  crSizeS       = TCursor(-29);
  crSizeSE      = TCursor(-30);
  crUpArrow     = TCursor(-10);
  crHourGlass   = TCursor(-11);
  crDrag        = TCursor(-12);
  crNoDrop      = TCursor(-13);
  crHSplit      = TCursor(-14);
  crVSplit      = TCursor(-15);
  crMultiDrag   = TCursor(-16);
  {$ENDIF}

type
  TCoord = packed record
    x1 : LongInt;
    y1 : LongInt;
    x2 : LongInt;
    y2 : LongInt;
  end;

  TRatio = packed record
    Horizontal : LongInt;
    Vertical : LongInt;
  end;

  TCardinalDirection = (NORTH, SOUTH, WEST, EAST);
  TDirection = set of TCardinalDirection;

  TSizeLimits = packed record
    minWidth  : LongInt;
    maxWidth  : LongInt;
    minHeight : LongInt;
    maxHeight : LongInt;
  end;

  TBGRAImageManipulation = class;

  { TCropArea }
  BoolParent = (bFalse=0, bTrue=1, bParent=2);

  TCropArea = class(TObject)
  private
    rName: String;
    procedure Render_Refresh;
    procedure setName(AValue: String);

  protected
    fOwner   :TBGRAImageManipulation;
    rArea    :TRect;     //Actually we would need two rectangles,
                         //one for the full size image and another for the scaled one.
                         //For now I do a workaround in the TBGRAImageManipulation.Resize method.
    rRatio   :TRatio;
    rAspectX,
    rAspectY,
    rMinHeight,
    rMinWidth : Integer;
    rAspectRatio: string;
    rKeepAspectRatio: BoolParent;

    procedure CopyAspectFromParent;
    procedure setAspectRatio(AValue: string);
    procedure setKeepAspectRatio(AValue: BoolParent);
    procedure setArea(AValue: TRect);
    function getLeft: Longint;
    procedure setLeft(AValue: Longint);
    function getTop: Longint;
    procedure setTop(AValue: Longint);
    function getWidth: Longint;
    procedure setWidth(AValue: Longint);
    function getHeight: Longint;
    procedure setHeight(AValue: Longint);
    function getRealAspectRatio(var ARatio: TRatio):Boolean; //return Real KeepAspect
    function getRealKeepAspectRatio:Boolean;
    function getIndex: Longint;
  public
    Rotate   :double;
    UserData :Integer;
    BorderColor :TBGRAPixel;

    function getBitmap(OriginalRect: TRect): TBGRABitmap;
    function getBitmapFullSize: TBGRABitmap;

    constructor Create(AOwner: TBGRAImageManipulation; AArea: TRect;
                       ARotate: double = 0;
                       AUserData: Integer = 0);
    destructor Destroy; override;

    property Area :TRect read rArea write setArea;
    property Top:Longint read getTop write setTop;
    property Left:Longint read getLeft write setLeft;
    property Width:Longint read getWidth write setWidth;
    property Height:Longint read getHeight write setHeight;
    property AspectRatio: string read rAspectRatio write setAspectRatio;
    property KeepAspectRatio: BoolParent read rKeepAspectRatio write setKeepAspectRatio default bParent;
    property Index:Longint read getIndex;
    property Name:String read rName write setName;
  end;

  { TCropAreaList }

  TCropAreaList = class(TObjectList)
  private
    function getCropArea(aIndex: Integer): TCropArea;
    procedure setCropArea(aIndex: Integer; const Value: TCropArea);

  protected
    fOwner   :TBGRAImageManipulation;
    rName    :String;
    loading  :Boolean;

    procedure Notify(Ptr: Pointer; Action: TListNotification); override;

  public
    constructor Create(AOwner: TBGRAImageManipulation);
    property items[aIndex: integer] : TCropArea read getCropArea write setCropArea; default;
    property Name:String read rName write rName;
    function add(aCropArea: TCropArea): integer;

    procedure Load(const XMLConf: TXMLConfig);
    procedure Save(const XMLConf: TXMLConfig);
    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
  end;

  TgetAllBitmapsCallback = procedure (Bitmap :TBGRABitmap; CropArea: TCropArea) of object;

  { TBGRAImageManipulation }

  TCropAreaEvent = procedure (AOwner: TBGRAImageManipulation; CropArea: TCropArea) of object;

  TBGRAImageManipulation = class(TBGRAGraphicCtrl)
  private
    { Private declarations }

    fAnchorSize:      byte;
    fAnchorSelected:  TDirection;
    fBorderSize:      byte;
    fAspectRatio:     string;
    fAspectX:         integer;
    fAspectY:         integer;
    fKeepAspectRatio: boolean;
    fMinHeight:       integer;
    fMinWidth:        integer;
    fMouseCaught:     boolean;
    fStartPoint:      TPoint;
    fEndPoint:        TPoint;

    fRatio:      TRatio;
    fSizeLimits: TSizeLimits;

    fImageBitmap, fResampledBitmap, fBackground, fVirtualScreen: TBGRABitmap;

    fDeltaX, fDeltaY: integer;

    function getAnchorSize: byte;
    procedure setAnchorSize(const Value: byte);
    function getEmpty: boolean;
    procedure setBitmap(const Value: TBGRABitmap);
    procedure setBorderSize(const Value: byte);
    procedure setAspectRatio(const Value: string);
    procedure setKeepAspectRatio(const Value: boolean);
    procedure setMinHeight(const Value: integer);
    procedure setMinWidth(const Value: integer);
    procedure setSelectedCropArea(AValue: TCropArea);
  protected
    { Protected declarations }
    rCropAreas :TCropAreaList;
    rNewCropArea,
    rSelectedCropArea :TCropArea;
    rOnCropAreaAdded: TCropAreaEvent;
    rOnCropAreaDeleted: TCropAreaEvent;
    rOnCropAreaChanged: TCropAreaEvent;
    rOnSelectedCropAreaChanged: TCropAreaEvent;
    oldWidth, oldHeight :Longint;              //Workaround for Crop Area Resize

    function ApplyDimRestriction(Coords: TCoord; Direction: TDirection;
      Bounds: TRect; AKeepAspectRatio:Boolean): TCoord;
    function ApplyRatioToAxes(Coords: TCoord; Direction: TDirection;
      Bounds: TRect;  ACropArea :TCropArea = Nil): TCoord;
    procedure ApplyRatioToArea(ACropArea :TCropArea);
    procedure CalcMaxSelection(ACropArea :TCropArea);
    procedure findSizeLimits;
    function getDirection(const Point1, Point2: TPoint): TDirection;
    function getImageRect(Picture: TBGRABitmap): TRect;
    function getWorkRect: TRect;
    function isOverAnchor(APoint :TPoint; var AnchorSelected :TDirection; var ACursor :TCursor) :TCropArea;

    procedure Paint; override;
    procedure RepaintBackground;
    procedure Resize; override;
    procedure Render;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
  public
    { Public declarations }

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    function getAspectRatioFromImage(const Value: TBGRABitmap): string;
    function getBitmap(ACropArea :TCropArea = Nil) : TBGRABitmap;
    function getBitmapFullSize(ACropArea :TCropArea = Nil) : TBGRABitmap;

    procedure rotateLeft;
    procedure rotateRight;

    procedure tests;

    //Crop Areas Manipulation functions
    function addCropArea(AArea : TRect; ARotate :double = 0; AUserData: Integer = 0) :TCropArea;
    procedure delCropArea(ACropArea :TCropArea);
    procedure clearCropAreas;
    procedure getAllBitmaps(ACallBack :TgetAllBitmapsCallback);
    procedure getAllBitmapsFullSize(ACallBack :TgetAllBitmapsCallback);

    property SelectedCropArea :TCropArea read rSelectedCropArea write setSelectedCropArea;
    property CropAreas :TCropAreaList read rCropAreas;
  published
    { Published declarations }

    property Align;
    property Anchors;

    property AnchorSize: byte Read getAnchorSize Write setAnchorSize default 5;
    property Bitmap: TBGRABitmap Read fImageBitmap Write setBitmap;
    property BorderSize: byte Read fBorderSize Write setBorderSize default 2;
    property AspectRatio: string Read fAspectRatio Write setAspectRatio;
    property KeepAspectRatio: boolean Read fKeepAspectRatio
      Write setKeepAspectRatio default True;
    property MinHeight: integer Read fMinHeight Write setMinHeight;
    property MinWidth: integer Read fMinWidth Write setMinWidth;
    property Empty: boolean Read getEmpty;

    //Events
    property OnCropAreaAdded:TCropAreaEvent read rOnCropAreaAdded write rOnCropAreaAdded;
    property OnCropAreaDeleted:TCropAreaEvent read rOnCropAreaDeleted write rOnCropAreaDeleted;
    property OnCropAreaChanged:TCropAreaEvent read rOnCropAreaChanged write rOnCropAreaChanged;

             //CropArea Parameter is the Old Selected Area, use SelectedCropArea property for current
    property OnSelectedCropAreaChanged:TCropAreaEvent read rOnSelectedCropAreaChanged write rOnSelectedCropAreaChanged;
  end;

{$IFDEF FPC}procedure Register;{$ENDIF}

Implementation
End.
