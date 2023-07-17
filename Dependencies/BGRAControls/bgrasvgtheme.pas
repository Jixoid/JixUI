unit BGRASVGTheme;

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  BGRATheme, BGRABitmap, BGRABitmapTypes, BGRASVG, BGRASVGType, XMLConf,
  ComponentEditors, PropEdits, Menus, BGRASVGImageList, Math;

const
  DEFAULT_CHECKBOX_TEXT_SPACING = 2;
  DEFAULT_GLYPH_TEXT_SPACING = 6;
  DEFAULT_BUTTON_TEXT_SPACING = 6;

type

  { TBGRASVGTheme }

  TBGRASVGTheme = class(TBGRATheme)
  private
    FButtonTextSpacing: integer;
    FCheckboxTextSpacing: integer;
    FColorizeActiveOp: TBlendOperation;
    FColorizeDisabledOp: TBlendOperation;
    FColorizeHoverOp: TBlendOperation;
    FColorizeNormalOp: TBlendOperation;
    FGlyphTextSpacing: integer;
    FOwner: TComponent;
    FButtonActive: TStringList;
    FButtonHover: TStringList;
    FButtonNormal: TStringList;
    FButtonSliceScalingBottom: integer;
    FButtonSliceScalingLeft: integer;
    FButtonSliceScalingRight: integer;
    FButtonSliceScalingTop: integer;
    FCheckBoxChecked: TStringList;
    FCheckBoxUnchecked: TStringList;
    FColorizeActive: string;
    FColorizeDisabled: string;
    FColorizeHover: string;
    FColorizeNormal: string;
    FRadioButtonChecked: TStringList;
    FRadioButtonUnchecked: TStringList;
    procedure SetButtonActive(AValue: TStringList);
    procedure SetButtonHover(AValue: TStringList);
    procedure SetButtonNormal(AValue: TStringList);
    procedure SetButtonSliceScalingBottom(AValue: integer);
    procedure SetButtonSliceScalingLeft(AValue: integer);
    procedure SetButtonSliceScalingRight(AValue: integer);
    procedure SetButtonSliceScalingTop(AValue: integer);
    procedure SetButtonTextSpacing(AValue: integer);
    procedure SetCheckBoxChecked(AValue: TStringList);
    procedure SetCheckboxTextSpacing(AValue: integer);
    procedure SetCheckBoxUnchecked(AValue: TStringList);
    procedure SetColorizeActive(AValue: string);
    procedure SetColorizeActiveOp(AValue: TBlendOperation);
    procedure SetColorizeDisabled(AValue: string);
    procedure SetColorizeDisabledOp(AValue: TBlendOperation);
    procedure SetColorizeHover(AValue: string);
    procedure SetColorizeHoverOp(AValue: TBlendOperation);
    procedure SetColorizeNormal(AValue: string);
    procedure SetColorizeNormalOp(AValue: TBlendOperation);
    procedure SetGlyphTextSpacing(AValue: integer);
    procedure SetRadioButtonChecked(AValue: TStringList);
    procedure SetRadioButtonUnchecked(AValue: TStringList);
  protected
    procedure LoadTheme(const XMLConf: TXMLConfig);
    procedure SaveTheme(const XMLConf: TXMLConfig);
    procedure CheckEmptyResourceException(const aResource: string);
    procedure SliceScalingDraw(const Source: TBGRASVG;
      const marginLeft, marginTop, marginRight, marginBottom: integer;
      const Dest: TBGRABitmap; DestDPI: integer);
    procedure ColorizeSurface(ASurface: TBGRAThemeSurface; AState: TBGRAThemeButtonState);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  public
    function PreferredButtonHeight(const hasGlyph: boolean): Integer; override;
    function PreferredButtonWidth(const hasGlyph: boolean): Integer; override;
    procedure DrawButton(Caption: string; State: TBGRAThemeButtonState;
      Focused: boolean; ARect: TRect; ASurface: TBGRAThemeSurface; AImageIndex: Integer = -1; AImageList: TBGRASVGImageList = nil); override;
    procedure DrawRadioButton(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect;
      ASurface: TBGRAThemeSurface); override;
    procedure DrawCheckBox(Caption: string; State: TBGRAThemeButtonState;
    {%H-}Focused: boolean; Checked: boolean; ARect: TRect;
      ASurface: TBGRAThemeSurface); override;
  public
    // XML File
    procedure SaveToFile(AFileName: string);
    // XML File
    procedure LoadFromFile(AFileName: string);
    // String Stream
    procedure SaveToStream(AStream: TStream);
    // String Stream
    procedure LoadFromStream(AStream: TStream);
    // Resource
    procedure LoadFromResource(AResource: string);
    // Default Theme
    procedure LoadDefaultTheme;
  published
    // Check box unchecked state
    property CheckBoxUnchecked: TStringList read FCheckBoxUnchecked
      write SetCheckBoxUnchecked;
    // Check box checked state
    property CheckBoxChecked: TStringList read FCheckBoxChecked write SetCheckBoxChecked;
    // Radio button unchecked state
    property RadioButtonUnchecked: TStringList
      read FRadioButtonUnchecked write SetRadioButtonUnchecked;
    // Radio button checked state
    property RadioButtonChecked: TStringList
      read FRadioButtonChecked write SetRadioButtonChecked;
    // Spacing between checkbox/radiobutton and its text (in 96 DPI)
    property CheckBoxTextSpacing: integer read FCheckboxTextSpacing write SetCheckboxTextSpacing default DEFAULT_CHECKBOX_TEXT_SPACING;
    // Button normal state
    property ButtonNormal: TStringList read FButtonNormal write SetButtonNormal;
    // Button mouse over state
    property ButtonHover: TStringList read FButtonHover write SetButtonHover;
    // Button pressed state
    property ButtonActive: TStringList read FButtonActive write SetButtonActive;
    // 9-Slice-Scaling margin left
    property ButtonSliceScalingLeft: integer
      read FButtonSliceScalingLeft write SetButtonSliceScalingLeft;
    // 9-Slice-Scaling margin top
    property ButtonSliceScalingTop: integer
      read FButtonSliceScalingTop write SetButtonSliceScalingTop;
    // 9-Slice-Scaling margin right
    property ButtonSliceScalingRight: integer
      read FButtonSliceScalingRight write SetButtonSliceScalingRight;
    // 9-Slice-Scaling margin bottom
    property ButtonSliceScalingBottom: integer
      read FButtonSliceScalingBottom write SetButtonSliceScalingBottom;
    // Spacing between glyph and its text (in 96 DPI)
    property GlyphTextSpacing: integer read FGlyphTextSpacing write SetGlyphTextSpacing default DEFAULT_GLYPH_TEXT_SPACING;
    // Spacing between text and button border (in 96 DPI)
    property ButtonTextSpacing: integer read FButtonTextSpacing write SetButtonTextSpacing default DEFAULT_BUTTON_TEXT_SPACING;
    // CSS Color to tint the normal states, use rgba(0,0,0,0) to disable
    property ColorizeNormal: string read FColorizeNormal write SetColorizeNormal;
    property ColorizeNormalOp: TBlendOperation read FColorizeNormalOp write SetColorizeNormalOp default boTransparent;
    // CSS Color to tint the hover states, use rgba(0,0,0,0) to disable
    property ColorizeHover: string read FColorizeHover write SetColorizeHover;
    property ColorizeHoverOp: TBlendOperation read FColorizeHoverOp write SetColorizeHoverOp default boTransparent;
    // CSS Color to tint the active states, use rgba(0,0,0,0) to disable
    property ColorizeActive: string read FColorizeActive write SetColorizeActive;
    property ColorizeActiveOp: TBlendOperation read FColorizeActiveOp write SetColorizeActiveOp default boTransparent;
    // CSS Color to tint the disabled states, use rgba(0,0,0,0) to disable
    property ColorizeDisabled: string read FColorizeDisabled write SetColorizeDisabled;
    property ColorizeDisabledOp: TBlendOperation read FColorizeDisabledOp write SetColorizeDisabledOp default boTransparent;
  end;

  { TBGRASVGThemeComponentEditor }

  TBGRASVGThemeComponentEditor = class(TBaseComponentEditor)
  private
    FComponent: TBGRASVGTheme;
  public
    constructor Create({%H-}AComponent: TComponent;
      {%H-}ADesigner: TComponentEditorDesigner); override;
    procedure Copy; override;
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetComponent: TComponent; override;
    function GetCustomHint: String; override;
    function GetDesigner: TComponentEditorDesigner; override;
    function GetHook(out Hook: TPropertyEditorHook): boolean; override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    function IsInInlined: Boolean; override;
    procedure PrepareItem({%H-}Index: Integer; const {%H-}AnItem: TMenuItem); override;
    procedure Modified; override;
  end;

procedure Register;

Implementation
End.
