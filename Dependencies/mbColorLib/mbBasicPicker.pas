unit mbBasicPicker;

{$mode objfpc}{$H+}

interface

uses
  LMessages, Classes, SysUtils, Graphics, Controls, ExtCtrls, Forms;

type
  THintState = (hsOff, hsWaitingToShow, hsWaitingToHide);
  TGetHintStrEvent = procedure (Sender: TObject; X, Y: Integer; var AText: String) of object;

  { TmbBasicPicker }

  TmbBasicPicker = class(TCustomControl)
  private
    FOnChange: TNotifyEvent;
    FOnGetHintStr: TGetHintStrEvent;
    FLockChange: Integer;
  protected
    FBufferBmp: TBitmap;
    FGradientWidth: Integer;
    FGradientHeight: Integer;
    FHintShown: Boolean;
    procedure CreateGradient; virtual;
    procedure DoChange; virtual;
    function GetColorUnderCursor: TColor; virtual;
    function GetGradientColor({%H-}AValue: Integer): TColor; virtual;
    function GetGradientColor2D({%H-}X, {%H-}Y: Integer): TColor; virtual;
    function GetHintPos(X, Y: Integer): TPoint; virtual;
    function GetHintStr(X, Y: Integer): String; virtual;
    function GetSelectedColor: TColor; virtual; abstract;
    procedure PaintParentBack; virtual; overload;
    procedure PaintParentBack(ACanvas: TCanvas); overload;
    procedure PaintParentBack(ACanvas: TCanvas; ARect: TRect); overload;
    procedure PaintParentBack(ABitmap: TBitmap); overload;
    procedure SetSelectedColor(c: TColor); virtual; abstract;
    procedure CMHintShow(var Message: TCMHintShow); message CM_HINTSHOW;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    property ColorUnderCursor: TColor read GetColorUnderCursor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetHintStr: TGetHintStrEvent read FOnGetHintStr write FOnGetHintStr;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetColorAtPoint(X, Y: Integer): TColor; virtual;
    function GetHexColorAtPoint(X, Y: integer): string;
    function GetHexColorUnderCursor: string; virtual;
    procedure Lock;
    function IsLocked: Boolean;
    procedure Unlock;
  published
    property ParentColor default true;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
  end;

Implementation
End.
