Unit JCL.Comps.Syn;

{$Mode ObjFPC}{$H+} //{$Inline On}

//{$Define FpsTest} // Show fps counter
//{$Define DebugHelper}

Interface

Uses
  Classes, SysUtils,
  JCL.Utils,
  JCL.Controls,
  JCL.CompTypes,
  JCL.Comps.ScrollBar,
  JCL.Comps.ScrollBarWin,
  JCL.Generics,
  JCL.Comps.Template,
  LResources, Forms, Controls, Graphics, Dialogs, Windows, Clipbrd, DateUtils,
  LCLType, FPCanvas, JCL.Auxiliary, LazUTF8, BGRABitmap, BGRABitmapTypes;

Type
  TJixSyn = Class; // Fudge JixSyn

  {%Region Highliter}
  rCharStyle = Packed Record
    CStyle: TFontStyles;
    CFore: TColor;
    Cell: String;
  End;

  arCells = Array Of rCharStyle;
  aarCells = Array Of arCells;

  jSynTextStyle = Class
    Private
      FForeColor: TColor;
      FTextStyle: TFontStyles;

    Published
      Property ForeColor: TColor      Read FForeColor Write FForeColor;
      Property TextStyle: TFontStyles Read FTextStyle Write FTextStyle;
  End;

  TJixSynHighliter = Class(TComponent)
    Public
      Procedure Start; Virtual; Abstract;
      Procedure StartLine(Line: StrA); Virtual; Abstract;
      Function Next(Word: StrA; X,Y: Int32U; IsFolded: Boolean; TheSyn: TJixSyn): rCharStyle; Virtual; Abstract;

  End;
  {%EndRegion}

  {%Region JixSyn}
  eMultiSelect = (msNone, msNormal);
  eChangeState = (csNone, csSaved, csModified, csCopyed);
  eFoldStatus = (fsNone, fsActive, fsDeActive, fsBegin, fsEnd);

  lChangeState = Specialize jList<eChangeState>;
  lFoldStatus = Specialize jList<eFoldStatus>;


  TJixSyn = class(TJixWinControl)
    private
      Var FHighliter: TJixSynHighliter;

      Var FStateTip: jJixState;

      Var FJixHorzScrollBar: TJixScrollBarWin;
      Var FJixVertScrollBar: TJixScrollBarWin;

      Var FLeftGutterSize: Int16U;

      Var FActualNumbers: lInt32U;
      Var FActualChanges: lChangeState;

      Var FActualFold: lFoldStatus;
      Var FActualCells: aarCells;

      Var FLines: TStrings;

      Procedure SetLines(Value: TStrings); Inline;
      Function GetLines: TStrings; Inline;

      Var FViewLeft: Int32U;
      Var FViewTop: Int32U;

      Var FActualWidth: Int32U;
      Var FActualHeight: Int32U;

      Var FCellWidth: Int8U;
      Var FCellHeight: Int8U;

      Var FRowInScreen: Int32U;
      Var FColInScreen: Int32U;

      Var FMultiSelect: eMultiSelect;
      Var FSelX1: Int32U;
      Var FSelY1: Int32U;
      Var FSelX2: Int32U;
      Var FSelY2: Int32U;

    private
      Var FColorNone: TColor;
      Var FColorSaved: TColor;
      Var FColorModified: TColor;
      Var FColorCopyed: TColor;

      Procedure SetColorNone(Value: TColor);
      Procedure SetColorSaved(Value: TColor);
      Procedure SetColorModified(Value: TColor);
      Procedure SetColorCopyed(Value: TColor);

    published
      Property ColorNone: TColor      Read FColorNone     Write SetColorNone     Default clWhite;
      Property ColorSaved: TColor     Read FColorSaved    Write SetColorSaved    Default clGreen;
      Property ColorModified: TColor  Read FColorModified Write SetColorModified Default clYellow;
      Property ColorCopyed: TColor    Read FColorCopyed   Write SetColorCopyed   Default clBlue;

    protected
      Var FTipDuraction: Int16U;
      Var FTipVisible: Boolean;
      Var FTips: String;
      Var FTipTime: TTime;

      Procedure Click; Override;
      Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); Override;
      Procedure MouseMove(Shift: TShiftState; X, Y: Integer); Override;
      Function  DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; Override;

      Function  DoUtf8KeyPress(Var Utf8Key: TUtf8Char): Boolean; Override;
      Procedure KeyDown(Var Key: Word; Shift: Tshiftstate); Override;

      Procedure HandleDlgCode(var Msg:TMessage); Message WM_GETDLGCODE;

      Procedure SetParent(Newparent: Twincontrol); Override;
      Procedure FDoChange(Sender: TObject); Virtual;


      Procedure SetVisible(Value: Boolean); Override;

    public
      Procedure StateChanged(ASender: TObject); Override;

      // Must be used correctly
      Property ActualCells: aarCells Read FActualCells Write FActualCells;

      Constructor Create(AOwner: TComponent); Override;
      Destructor Destroy; Override;

      Procedure CodeFoldStart(Y: Int32U; Force: Boolean = False); Inline;
      Procedure CodeFoldFinish(Y: Int32U; Force: Boolean = False); Inline;
      Procedure CodeFoldNone(Y: Int32U); Inline;

      Procedure PaintOver(ACanvas: TCanvas; X, Y: Integer); Override;

      Procedure RecalcuteCells; Inline;

      Procedure SetTemplate(Temp: jTemplateCollectionItem); Override;

      Property ActualWidth: Int32U  Read FActualWidth;
      Property ActualHeight: Int32U Read FActualHeight;

      Function SelStartPoint: TPoint; Inline;
      Function SelFinishPoint: TPoint; Inline;

      Procedure LoadFromFile(AFile: String); Virtual;
      Procedure SaveToFile(AFile: String); Virtual;

      Function VirToAct(Value: Int32U): Int32U; Inline;
      Function ActToVir(Value: Int32U): Int32U; Inline;

    published
      Property Highliter: TJixSynHighliter Read FHighliter Write FHighliter;

      Property Lines: TStrings  Read GetLines Write SetLines;

      Property Cursor Default crIBeam;

      Property Tooltip;
      Property ShowTooltip;

      Property StateNormal;
      Property StateBack;
      Property StateTip: jJixState  Read FStateTip Write FStateTip;

      Property IgnoreTempStyle Default True;
      Property IgnorePicture;

      Property Align;
      Property BorderSpacing;
      Property Anchors;
      //Property AutoSize;
      Property Caption;
      Property Enabled;
      Property Visible;

      Property OnChangeBounds;
      Property OnPaintBefore;
      Property OnPaintAfter;
      Property OnClick;
      Property OnDblClick;
      Property OnMouseDown;
      Property OnMouseEnter;
      Property OnMouseLeave;
      Property OnMouseMove;
      Property OnMouseUp;
      Property OnMouseWheel;
      Property OnMouseWheelDown;
      Property OnMouseWheelUp;
      Property OnResize;
      Property PopupMenu;
  end;
  {%EndRegion}

  Procedure Register;

Implementation
End.
