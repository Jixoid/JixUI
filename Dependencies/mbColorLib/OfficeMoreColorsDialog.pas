unit OfficeMoreColorsDialog;

interface

uses
  LCLIntf, LCLType, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, ComCtrls, Windows,
  HexaColorPicker, HSLColorPicker, mbColorConv, mbColorPreview,
  {$IFDEF mbXP_Lib}mbXPSpinEdit, mbXPSizeGrip,{$ELSE} Spin,{$ENDIF}
  HTMLColors, SLHColorPicker, HSLRingPicker, RColorPicker, GColorPicker,
  BColorPicker, JCL.Forms, JCL.Comps.Button, JCL.Comps.Panel,
  JCL.Comps.StaticLabel, JCL.CompTypes, JCL.Comps.ComboBox;

type

  { TOfficeMoreColorsWin }

  TOfficeMoreColorsWin = class(jJixForm)
    Bevel2: Tbevel;
    Btnok: Tjixbutton;
    BTrackbar: TBColorPicker;
    Bevel1: TBevel;
    Cbcolordisplay: Tjixcombobox;
    Edblue: Tspinedit;
    Edgreen: Tspinedit;
    Edhue: Tspinedit;
    Edlumval: Tspinedit;
    Edred: Tspinedit;
    Edsat: Tspinedit;
    GTrackbar: TGColorPicker;
    HSLRing: THSLRingPicker;
    Jixlabel1: Tjixlabel;
    Jixlabel2: Tjixlabel;
    Jixlabel3: Tjixlabel;
    Jixlabel4: Tjixlabel;
    JixPanelStandard: Tjixpanel;
    JixPanelCustom: Tjixpanel;
    Lblblue: Tjixlabel;
    Lblgreen: Tjixlabel;
    Lblhue: Tjixlabel;
    Lbllumval: Tjixlabel;
    Lblpicker: Tjixlabel;
    LblR: TLabel;
    LblG: TLabel;
    LblB: TLabel;
    Lblred: Tjixlabel;
    Lblsat: Tjixlabel;
    nbRGB: TPage;
    Newswatch: Tmbcolorpreview;
    Oldswatch: Tmbcolorpreview;
    Panel1: Tjixpanel;
    PickerNotebook: TNotebook;
    nbHSL: TPage;
    nbHSLRing: TPage;
    nbSLH: TPage;
    RTrackbar: TRColorPicker;
    Sidepanel: Tjixpanel;
    SLH: TSLHColorPicker;
    Hexa: THexaColorPicker;
    HSL: THSLColorPicker;
    Label5: TLabel;
    Procedure Btnokclick(Sender: Tobject);
    procedure cbColorDisplayChange(Sender: TObject);
    procedure ColorPickerChange(Sender: TObject);
    procedure EdBlueChange(Sender: TObject);
    procedure EdGreenChange(Sender: TObject);
    procedure EdHueChange(Sender: TObject);
    procedure EdLumValChange(Sender: TObject);
    procedure EdRedChange(Sender: TObject);
    procedure EdSatChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    Procedure Formshow(Sender: Tobject);
    function GetHint(c: TColor): string;
    procedure HexaChange(Sender: TObject);
    procedure HSLChange(Sender: TObject);
    procedure HSLRingChange(Sender: TObject);
    Procedure Jixlabel3click(Sender: Tobject);
    Procedure Jixlabel4click(Sender: Tobject);
    procedure nbHSLRingResize(Sender: TObject);
    procedure NewSwatchColorChange(Sender: TObject);
    procedure OldSwatchColorChange(Sender: TObject);
    procedure SLHChange(Sender: TObject);
  private
    FMaxHue: Integer;
    FMaxSat: Integer;
    FMaxLum: Integer;
    FMaxVal: Integer;
    FSelectedColor: TColor;
    FBrightnessMode: TBrightnessMode;
    FLockChange: Integer;
    function GetPickerIndex: Integer;
    function GetSelectedColor: TColor;
    function GetShowHint: Boolean;
    procedure SetAllCustom(c: TColor);
    procedure SetAllToSel(c: TColor);
    procedure SetBrightnessMode(AMode: TBrightnessMode);
    procedure SetMaxHue(H: Integer);
    procedure SetMaxLum(L: Integer);
    procedure SetMaxSat(S: Integer);
    procedure SetMaxVal(V: Integer);
    procedure SetPickerIndex(AValue: Integer);
    procedure SetSelectedColor(c: TColor);
    procedure SetShowHint(AValue: boolean);
  protected
    procedure BeginUpdate;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure EndUpdate;
  public
    property PickerIndex: Integer read GetPickerIndex write SetPickerIndex;
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;
    property MaxHue: Integer read FMaxHue write SetMaxHue;
    property MaxSaturation: Integer read FMaxSat write SetMaxSat;
    property MaxLuminance: Integer read FMaxLum write SetMaxLum;
    property MaxValue: Integer read FMaxVal write SetMaxVal;
  published
    property ShowHint: Boolean read GetShowHint write SetShowHint;
  end;

var
  OfficeMoreColorsWin: TOfficeMoreColorsWin;
  Used: Boolean;

Implementation
End.
