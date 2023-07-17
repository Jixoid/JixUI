unit JCL.Comps.PropertyEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, TypInfo, StrUtils, Math,
  // LCL
  LCLPlatformDef, InterfaceBase, LCLType, LCLIntf, Forms, Buttons, Graphics,
  Controls, ComCtrls, ExtCtrls, Menus, Dialogs, Themes, LMessages,
  ImgList, ActnList,
  // JCL
  JCL.Controls,
  JCL.Comps.Template,
  JCL.Utils,
  JCL.Comps.ScrollBar,
  JCL.Comps.ScrollBarWin,
  JCL.Comps.CheckBox,
  JCL.Comps.Button,
  JCL.Comps.ComboBox,
  JCL.Comps.Edit,
  // LazUtils
  GraphType, LazLoggerBase, LazStringUtils, LazUTF8,
  // IdeIntf
  IDEImagesIntf, IDEHelpIntf, ObjInspStrConsts, ObjectInspector,
  PropEdits, PropEditUtils, OIFavoriteProperties, ComponentEditors,
  ChangeParentDlg, LResources,
  // BGRA
  BGRABitmapTypes;

type
  TJixPropertyEditor = Class;

  TOIPropertyGridRow = Class
  private
    FTop: integer;
    FHeight: integer;
    FLvl: integer;
    FName: string;
    FExpanded: boolean;
    FTree: TJixPropertyEditor;
    FChildCount:integer;
    FPriorBrother,
    FFirstChild,
    FLastChild,
    FNextBrother,
    FParent: TOIPropertyGridRow;
    FEditor: TPropertyEditor;
    FWidgetSets: TLCLPlatforms;

    FIndex:integer;
    LastPaintedValue: string;

    procedure GetLvl;
  public
    constructor Create(PropertyTree: TJixPropertyEditor;
       PropEditor:TPropertyEditor; ParentNode:TOIPropertyGridRow; WidgetSets: TLCLPlatforms);
    destructor Destroy; override;
    function ConsistencyCheck: integer;
    function HasChild(Row: TOIPropertyGridRow): boolean;
    procedure WriteDebugReport(const Prefix: string);

    function GetBottom: integer;
    function IsReadOnly: boolean;
    function IsDisabled: boolean;
    procedure MeasureHeight(ACanvas: TCanvas);
    function Sort(const Compare: TListSortCompare): boolean; // true if changed
    function IsSorted(const Compare: TListSortCompare): boolean;
    function Next: TOIPropertyGridRow;
    function NextSkipChilds: TOIPropertyGridRow;

    property Editor: TPropertyEditor read FEditor;
    property Top: integer read FTop write FTop;
    property Height: integer read FHeight write FHeight;
    property Bottom: integer read GetBottom;
    property Lvl: integer read FLvl;
    property Name: string read FName;
    property Expanded: boolean read FExpanded;
    property Tree: TJixPropertyEditor read FTree;
    property Parent: TOIPropertyGridRow read FParent;
    property ChildCount: integer read FChildCount;
    property FirstChild: TOIPropertyGridRow read FFirstChild;
    property LastChild: TOIPropertyGridRow read FLastChild;
    property NextBrother: TOIPropertyGridRow read FNextBrother;
    property PriorBrother: TOIPropertyGridRow read FPriorBrother;
    property Index: integer read FIndex;
  end;

  TOIPropertyHintEvent = function(Sender: TObject; PointedRow: TOIPropertyGridRow;
    out AHint: string): boolean of object;

  TJixPropertyEditor = class(TCustomControl, iJixThemable)
  Private
    Var FThemeName: String;
    Var FScroll: TJixScrollBarWin;

    FAutoFreeHook: boolean;
    FSaveOnChangeTIObject: boolean;
    function GetTIObject: TPersistent;
    procedure SetAutoFreeHook(const AValue: boolean);
    procedure SetTIObject(const AValue: TPersistent);

  Public
    property AutoFreeHook: boolean read FAutoFreeHook write SetAutoFreeHook;
    property SaveOnChangeTIObject: boolean read FSaveOnChangeTIObject
                                           write FSaveOnChangeTIObject
                                           default true;
  private
    FBackgroundColor: TColor;
    FColumn: TOICustomPropertyGridColumn;
    FGutterColor: TColor;
    FGutterEdgeColor: TColor;
    FHighlightColor: TColor;
    FLayout: TOILayout;
    FOnEditorFilter: TOIEditorFilterEvent;
    FOnOIKeyDown: TKeyEvent;
    FOnPropertyHint: TOIPropertyHintEvent;
    FOnSelectionChange: TNotifyEvent;
    FReferencesColor: TColor;
    FReadOnlyColor: TColor;
    FRowSpacing: integer;
    FShowGutter: Boolean;
    FSubPropertiesColor: TColor;
    FChangeStep: integer;
    FCurrentButton: TControl; // nil or ValueButton
    FCurrentEdit: TControl;  // nil or ValueEdit or ValueComboBox or ValueCheckBox
    FCurrentEditorLookupRoot: TPersistent;
    FDefaultItemHeight:integer;
    FDragging: boolean;
    FExpandedProperties: TStringList;// used to restore expanded state when switching selected component(s)
    FExpandingRow: TOIPropertyGridRow;
    FFavorites: TOIFavoriteProperties;
    FFilter: TTypeKinds;
    FIndent: integer;
    FItemIndex: integer;
    FNameFont, FDefaultValueFont, FValueFont, FHighlightFont: TFont;
    FValueDifferBackgrndColor: TColor;
    FNewComboBoxItems: TStringListUTF8Fast;
    FOnModified: TNotifyEvent;
    FRows: TFPList;// list of TOIPropertyGridRow
    FSelection: TPersistentSelectionList;
    FNotificationComponents: TFPList;
    FPropertyEditorHook: TPropertyEditorHook;
    FPreferredSplitterX: integer; // best splitter position
    FSplitterX: integer; // current splitter position
    FStates: TOIPropertyGridStates;
    FTopY: integer;
    FDrawHorzGridLines: Boolean;
    FActiveRowImages: TLCLGlyphs;
    FFirstClickTime: DWORD;
    FKeySearchText: string;
    FHideClassNames: Boolean;
    FPropNameFilter : String;
    FPaintRc: TRect;

    // hint stuff
    FLongHintTimer: TTimer;
    FHintManager: THintWindowManager;
    FHintIndex: integer;
    FHintType: TPropEditHint;
    FShowingLongHint: boolean; // last hint was activated by the hinttimer

    ValueEdit: TJixEdit;
    ValueComboBox: TJixComboBox;
    ValueCheckBox: TJixCheckBox;
    ValueButton: TJixButton;

    procedure ActiveRowImagesGetWidthForPPI(Sender: TCustomImageList;
      {%H-}AImageWidth, {%H-}APPI: Integer; var AResultWidth: Integer);
    procedure HintMouseLeave(Sender: TObject);
    procedure HintTimer(Sender: TObject);
    procedure HideHint;
    procedure HintMouseDown(Sender: TObject; Button: TMouseButton;
                            Shift: TShiftState; X, Y: Integer);
    procedure IncreaseChangeStep;
    function GridIsUpdating: boolean;

    function GetRow(Index:integer):TOIPropertyGridRow;
    function GetRowCount:integer;
    procedure ClearRows;
    function GetCurrentEditValue: string;
    procedure SetActiveControl(const AControl: TControl);
    procedure SetCheckboxState(NewValue: string);
    procedure SetColumn(const AValue: TOICustomPropertyGridColumn);
    procedure SetCurrentEditValue(const NewValue: string);
    procedure SetDrawHorzGridLines(const AValue: Boolean);
    procedure SetFavorites(const AValue: TOIFavoriteProperties);
    procedure SetFilter(const AValue: TTypeKinds);
    procedure SetGutterColor(const AValue: TColor);
    procedure SetGutterEdgeColor(const AValue: TColor);
    procedure SetHighlightColor(const AValue: TColor);
    procedure SetItemIndex(NewIndex:integer);
    function IsCurrentEditorAvailable: Boolean;

    function GetNameRowHeight: Integer; // temp solution untill TFont.height returns its actual value

    procedure SetItemsTops;
    procedure AlignEditComponents;
    procedure EndDragSplitter;
    procedure SetRowSpacing(const AValue: integer);
    procedure SetShowGutter(const AValue: Boolean);
    procedure SetSplitterX(const NewValue:integer);
    procedure SetTopY(const NewValue:integer);

    function GetPropNameColor(ARow: TOIPropertyGridRow):TColor;
    function GetTreeIconX(Index: integer):integer;
    function RowRect(ARow: integer):TRect;
    procedure PaintRow(ARow: integer);
    procedure DoPaint(PaintOnlyChangedValues: boolean);

    procedure SetSelection(const ASelection:TPersistentSelectionList);
    procedure SetPropertyEditorHook(NewPropertyEditorHook:TPropertyEditorHook);
    procedure UpdateSelectionNotifications;
    procedure HookGetCheckboxForBoolean(var Value: Boolean);

    procedure AddPropertyEditor(PropEditor: TPropertyEditor);
    procedure AddStringToComboBox(const s: string);
    procedure ExpandRow(Index: integer);
    procedure ShrinkRow(Index: integer);
    procedure AddSubEditor(PropEditor: TPropertyEditor);
    procedure SortSubEditors(ParentRow: TOIPropertyGridRow);
    function CanExpandRow(Row: TOIPropertyGridRow): boolean;

    procedure SetRowValue(CheckFocus, ForceValue: boolean);
    procedure DoCallEdit(Edit: TOIQuickEdit = oiqeEdit);
    procedure RefreshValueEdit;
    procedure ToggleRow;
    procedure ValueEditDblClick(Sender : TObject);
    procedure ValueControlMouseDown(Sender: TObject; {%H-}Button:TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X,{%H-}Y:integer);
    procedure ValueControlMouseMove(Sender: TObject; {%H-}Shift: TShiftState;
      {%H-}X,{%H-}Y:integer);
    procedure ValueEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueEditExit(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure ValueEditMouseUp(Sender: TObject; Button: TMouseButton;
                               Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure ValueCheckBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueCheckBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueCheckBoxExit(Sender: TObject);
    procedure ValueCheckBoxClick(Sender: TObject);
    procedure ValueComboBoxExit(Sender: TObject);
    procedure ValueComboBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueComboBoxKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ValueComboBoxMouseUp(Sender: TObject; Button: TMouseButton;
                                   Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure ValueComboBoxCloseUp(Sender: TObject);
    procedure ValueComboBoxGetItems(Sender: TObject);
    procedure ValueButtonClick(Sender: TObject);
    procedure ValueComboBoxMeasureItem({%H-}Control: TWinControl; Index: Integer;
          var AHeight: Integer);
    procedure ValueComboBoxDrawItem({%H-}Control: TWinControl; Index: Integer;
          ARect: TRect; State: TOwnerDrawState);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure SetIdleEvent(Enable: boolean);
    procedure OnGridMouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
      WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);

    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    procedure SetBackgroundColor(const AValue: TColor);
    procedure SetReferences(const AValue: TColor);
    procedure SetSubPropertiesColor(const AValue: TColor);
    procedure SetReadOnlyColor(const AValue: TColor);
    procedure SetValueDifferBackgrndColor(AValue: TColor);
    procedure UpdateScrollBar;
    function FillComboboxItems: boolean; // true if something changed
    function EditorFilter(const AEditor: TPropertyEditor): Boolean;

  protected
    Procedure SetParent(NewParent: Twincontrol); Override;
    Procedure FDoChange(Sender: TObject); Virtual;

    Procedure SetThemeName(Value: ThemeStr); Virtual;
    Function GetThemeName: ThemeStr; Virtual;

    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
    procedure MouseMove(Shift:TShiftState; X,Y:integer);  override;
    procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
    procedure MouseLeave; override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure HandleStandardKeys(var Key: Word; Shift: TShiftState); virtual;
    procedure HandleKeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure DoTabKey; virtual;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure DoSelectionChange;

  public
    Procedure SetTemplate(Temp: jTemplateCollectionItem); Virtual;

    constructor Create(TheOwner: TComponent); override;
    procedure CreateWithParams({%H-}AnOwner: TComponent;
                                 APropertyEditorHook: TPropertyEditorHook;
                                 TypeFilter: TTypeKinds;
                                 DefItemHeight: integer);
    destructor Destroy;  override;
    function CanEditRowValue(CheckFocus: boolean): boolean;
    procedure SaveChanges;
    function ConsistencyCheck: integer;
    procedure EraseBackground({%H-}DC: HDC); override;
    function GetActiveRow: TOIPropertyGridRow;
    function GetHintTypeAt(RowIndex: integer; X: integer): TPropEditHint;

    function GetRowByPath(const PropPath: string): TOIPropertyGridRow;
    function GridHeight: integer;
    function RealDefaultItemHeight: integer;
    function MouseToIndex(y: integer; MustExist: boolean): integer;
    function PropertyPath(Index: integer):string;
    function PropertyPath(Row: TOIPropertyGridRow):string;
    function PropertyEditorByName(const PropName: string): TPropertyEditor;
    function TopMax: integer;
    procedure BuildPropertyList(OnlyIfNeeded: Boolean = False; FocusEditor: Boolean = True);
    procedure Clear;
    procedure Paint; override;
    procedure PropEditLookupRootChange;
    procedure RefreshPropertyValues;
    procedure ScrollToActiveItem;
    procedure ScrollToItem(NewIndex: Integer);
    procedure SetBounds(aLeft, aTop, aWidth, aHeight: integer); override;
    procedure SetCurrentRowValue(const NewValue: string);
    procedure SetItemIndexAndFocus(NewItemIndex: integer; WasValueClick: Boolean = False);

  Public
    property HighlightColor: TColor read FHighlightColor write SetHighlightColor default DefHighlightColor;
    property ReferencesColor: TColor read FReferencesColor
                                     write SetReferences default DefReferencesColor;
    property SubPropertiesColor: TColor read FSubPropertiesColor
                                     write SetSubPropertiesColor default DefSubPropertiesColor;
    property ReadOnlyColor: TColor read FReadOnlyColor
                                     write SetReadOnlyColor default DefReadOnlyColor;
    property ValueDifferBackgrndColor: TColor read FValueDifferBackgrndColor
           write SetValueDifferBackgrndColor default DefValueDifferBackgrndColor;

    property HighlightFont: TFont read FHighlightFont write FHighlightFont;

    property Column: TOICustomPropertyGridColumn read FColumn write SetColumn;
    property CurrentEditValue: string read GetCurrentEditValue
                                      write SetCurrentEditValue;
    property DrawHorzGridLines: Boolean read FDrawHorzGridLines write
      SetDrawHorzGridLines default True;
    property ExpandedProperties: TStringList read FExpandedProperties;
    property ItemIndex: integer read FItemIndex write SetItemIndex;
    property Layout: TOILayout read FLayout write FLayout default oilHorizontal;
    property OnOIKeyDown: TKeyEvent read FOnOIKeyDown write FOnOIKeyDown;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnPropertyHint: TOIPropertyHintEvent read FOnPropertyHint write FOnPropertyHint;
    property PropertyEditorHook: TPropertyEditorHook read FPropertyEditorHook
                                                    write SetPropertyEditorHook;
    property RowCount: integer read GetRowCount;
    property Rows[Index: integer]: TOIPropertyGridRow read GetRow;
    property RowSpacing: integer read FRowSpacing write SetRowSpacing;
    property Selection: TPersistentSelectionList read FSelection write SetSelection;
    property ShowGutter: Boolean read FShowGutter write SetShowGutter default True;
    property TopY: integer read FTopY write SetTopY default 0;
    property Favorites: TOIFavoriteProperties read FFavorites write SetFavorites;
    property HideClassNames: Boolean read FHideClassNames write FHideClassNames;
    property PropNameFilter : String read FPropNameFilter write FPropNameFilter;

  Published
    Property ThemeName: ThemeStr  Read GetThemeName  Write SetThemeName;

    property GutterColor: TColor read FGutterColor write SetGutterColor default DefGutterColor;
    property GutterEdgeColor: TColor read FGutterEdgeColor write SetGutterEdgeColor default DefGutterEdgeColor;

    property Align;
    property Anchors;
    property BackgroundColor: TColor read FBackgroundColor
                                     write SetBackgroundColor default DefBackgroundColor;
    property BorderSpacing;
    property Constraints;
    property DefaultItemHeight:integer read FDefaultItemHeight
                                       write FDefaultItemHeight default 0;
    property DefaultValueFont: TFont read FDefaultValueFont write FDefaultValueFont;
    property Filter : TTypeKinds read FFilter write SetFilter;
    property Indent: integer read FIndent write FIndent;
    property NameFont: TFont read FNameFont write FNameFont;
    property OnChangeBounds;
    property OnClick;
    property OnEditorFilter: TOIEditorFilterEvent read FOnEditorFilter write FOnEditorFilter;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property PopupMenu;
    property PreferredSplitterX: integer read FPreferredSplitterX
                                         write FPreferredSplitterX default 100;
    property SplitterX: integer read FSplitterX write SetSplitterX default 100;
    //property TabStop;
    property SelectObject: TPersistent read GetTIObject write SetTIObject;
    property ValueFont: TFont read FValueFont write FValueFont;
    property Visible;
  end;

procedure Register;

Implementation
End.
