unit mbColorPreview;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType, SysUtils, Classes, Controls, Graphics;

type
  TmbColorPreview = class(TCustomControl)
  private
    FSelColor: TColor;
    FOpacity: integer;
    FOnColorChange: TNotifyEvent;
    FOnOpacityChange: TNotifyEvent;
    FBlockSize: integer;
    FSwatchStyle: boolean;
    function MakeBmp: TBitmap;
    procedure SetSwatchStyle(Value: boolean);
    procedure SetSelColor(c: TColor);
    procedure SetOpacity(o: integer);
    procedure SetBlockSize(s: integer);
  protected
    procedure DoChange;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BlockSize: integer read FBlockSize write SetBlockSize default 6;
    property Color: TColor read FSelColor write SetSelColor default clWhite;
    property Opacity: integer read FOpacity write SetOpacity default 100;
    property SwatchStyle: boolean read FSwatchStyle write SetSwatchStyle default false;
    property Anchors;
    property Align;
    property BorderSpacing;
    property ShowHint;
    property ParentShowHint;
    property Visible;
    property Enabled;
    property PopupMenu;
    property DragCursor;
    property DragMode;
    property DragKind;
    property Constraints;
    property OnColorChange: TNotifyEvent read FOnColorChange write FOnColorChange;
    property OnOpacityChange: TNotifyEvent read FOnOpacityChange write FOnOpacityChange;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
    property OnStartDrag;
    property OnDblClick;
  end;


Implementation
End.
