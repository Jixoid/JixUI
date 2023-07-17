Unit JCL.Controls;

{$mode objfpc}{$H+}{$Interfaces Corba}{$Inline On}

Interface

Uses
  Windows, Classes, SysUtils,Controls, Forms, Graphics, BGRABitmap,
  BGRABitmapTypes, BGRAGradients, BGRAGradientScanner, JCL.Auxiliary, Dialogs,
  JCL.CompTypes,
  JCL.Comps.Template,
  JCL.Utils,
  JCL.Generics,
  JCL.Dialogs;

Type
  {%Region JixComponent Interfaces}
  ThemeStr = Type StrA;

  iJixThemable = Interface
    Procedure SetTemplate(Temp: jTemplateCollectionItem);

    Procedure SetThemeName(Value: ThemeStr); Inline;
    Function GetThemeName: ThemeStr; Inline;

    Property ThemeName: ThemeStr   Read GetThemeName Write SetThemeName;
  End;

  iJixRTPM = Interface // Not complated
    Procedure PropertySaveFile(Path: StrA);
    Procedure PropertyOpenFile(Path: StrA);
  End;
  {%EndRegion}

  {%Region JixDialog}
    TJixDialog = Class(TComponent)
    Private
      Var FCaption: TCaption;

    Public
      Function Execute: Boolean; Virtual; Abstract;

    Published
      Property Caption: TCaption  Read FCaption  Write FCaption;

    End;
  {%EndRegion}

  {%Region JixControl}
  eAnim_Hover = (jahNil, jahEnter, jahLeave, jahClick);

  TJixControl = Class(TGraphicControl, iJixThemable)
    Private
      Var FOnPaintAfter: mNotify;
      Var FOnPaintBefore: mNotify;

      Var FStateNormal: jJixState;
      Var FStateHover: jJixState;
      Var FStateClick: jJixState;
      Var FStateBack: jJixState;

      Var FThemeName: ThemeStr;
      Var FStatic: Boolean;
      Var FIgnoreTStyle: Boolean;
      Var FIgnorePicture: Boolean;
      Var FAnimate: Boolean;

      Var FShowTooltip: Boolean;
      Var FTooltip: TCaption;

      Var Anim_Hover: eAnim_Hover;
      Procedure SetStateAnim(State: jJixState; Flag: eAnim_Hover);

      Function GetThemeName: ThemeStr; Inline;

      Function GetRight: Integer;
      Function GetBottom: Integer;

    Protected
      Procedure SetThemeName(Value: ThemeStr); Virtual;

      Var Bmp: TBGRABitmap;
      Var ToolTipWindow: THintWindow;

      Var UseCustomRect: Boolean;
      Var ScreenRect: TRect;

      Var PaintBackground: Boolean;
      Var PaintBorder: Boolean;
      Var PaintFont: Boolean;
      Var PaintPicture: Boolean;

      Procedure RealSetText(const Value: TCaption); Override;

      Var AutoSizePadding: TSize;
      Var AutoSizeMin: TSize;
      Procedure SetAutoSize(Value: Boolean); Override;

      Function AddAlpha(AColor: TColor; Alpha: Int8U): TBGRAPixel;

      // For state change catch
      Procedure Mouseenter; Override;
      Procedure Mouseleave; Override;
      Procedure Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;
      Procedure Mouseup(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;
      Procedure Mousemove(Shift: Tshiftstate; X, Y: Integer); Override;

    Public
      Var NowState: jJixState;

      Procedure StateChanged({%H-}ASender: TObject); Virtual;

      Constructor Create(Aowner: Tcomponent); Override;
      Destructor Destroy; Override;

      Procedure Paint; Override;
      Procedure RecalculateSize; Virtual;
      Function  AutoSizedWidth: Int32U; Virtual;
      Function  AutoSizedHeight: Int32U; Virtual;
      Procedure PaintOver(ACanvas: TCanvas; X,Y: Integer); Virtual;

      Procedure SetTemplate(Temp: jTemplateCollectionItem); Virtual;

      Property Right: Integer  Read GetRight;
      Property Bottom: Integer Read GetBottom;

    Public
      Property OnPaintAfter: mNotify      Read FOnPaintAfter  Write FOnPaintAfter;
      Property OnPaintBefore: mNotify     Read FOnPaintBefore Write FOnPaintBefore;

      Property StateNormal: jJixState     Read FStateNormal   Write FStateNormal;
      Property StateHover: jJixState      Read FStateHover    Write FStateHover;
      Property StateClick: jJixState      Read FStateClick    Write FStateClick;
      Property StateBack: jJixState       Read FStateBack     Write FStateBack;

      Property Static: Boolean            Read FStatic        Write FStatic        Default False;
      Property IgnoreTempStyle: Boolean   Read FIgnoreTStyle  Write FIgnoreTStyle  Default False;
      Property IgnorePicture: Boolean     Read FIgnorePicture Write FIgnorePicture Default True;
      Property Animate: Boolean           Read FAnimate       Write FAnimate       Default True;

      Property ShowTooltip: Boolean       Read FShowTooltip   Write FShowTooltip   Default False;
      Property Tooltip: TCaption          Read FTooltip       Write FTooltip;

    Published
      Property ThemeName: ThemeStr        Read GetThemeName   Write SetThemeName;
  End;
  {%EndRegion}

  {%Region JixWinControl}
  TJixWinControl = Class(TScrollingWinControl, iJixThemable)
    Private
      Var FOnPaintAfter: mNotify;
      Var FOnPaintBefore: mNotify;

      Var FStateNormal: jJixState;
      Var FStateHover: jJixState;
      Var FStateClick: jJixState;
      Var FStateBack: jJixState;

      Var FThemeName: ThemeStr;
      Var FStatic: Boolean;
      Var FIgnoreTStyle: Boolean;
      Var FIgnorePicture: Boolean;
      Var FAnimate: Boolean;

      Var FShowTooltip: Boolean;
      Var FTooltip: TCaption;

      Var Anim_Hover: eAnim_Hover;
      Procedure SetStateAnim(State: jJixState; Flag: eAnim_Hover);

      Function GetThemeName: ThemeStr; Inline;

      Function GetRight: Integer;
      Function GetBottom: Integer;

    Protected
      Procedure SetThemeName(Value: ThemeStr); Virtual;

      Var Bmp: TBGRABitmap;
      Var ToolTipWindow: THintWindow;

      Var UseCustomRect: Boolean;
      Var ScreenRect: TRect;

      Var PaintBackground: Boolean;
      Var PaintBorder: Boolean;
      Var PaintFont: Boolean;
      Var PaintPicture: Boolean;

      Procedure RealSetText(const Value: TCaption); Override;

      Var AutoSizePadding: TSize;
      Var AutoSizeMin: TSize;
      Procedure SetAutoSize(Value: Boolean); Override;

      Function AddAlpha(AColor: TColor; Alpha: Int8U): TBGRAPixel;

      // For state change catch
      Procedure Mouseenter; Override;
      Procedure Mouseleave; Override;
      Procedure Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;
      Procedure Mouseup(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;
      Procedure Mousemove(Shift: Tshiftstate; X, Y: Integer); Override;

    Public
      Var NowState: jJixState;

      Procedure StateChanged({%H-}ASender: TObject); Virtual;

      Constructor Create(Aowner: Tcomponent); Override;
      Destructor Destroy; Override;

      Procedure Paint; Override;
      Procedure RecalculateSize; Virtual;
      Function  AutoSizedWidth: Int32U; Virtual;
      Function  AutoSizedHeight: Int32U; Virtual;
      Procedure PaintOver(ACanvas: TCanvas; X,Y: Integer); Virtual;

      // for add TGraphicControl's to Canvas
      procedure PaintTo(ACanvas: TCanvas; X, Y: Integer); Overload;

      Procedure SetTemplate(Temp: jTemplateCollectionItem); Virtual;

      Property Right: Integer  Read GetRight;
      Property Bottom: Integer Read GetBottom;

    Public
      Property OnPaintAfter: mNotify      Read FOnPaintAfter  Write FOnPaintAfter;
      Property OnPaintBefore: mNotify     Read FOnPaintBefore Write FOnPaintBefore;

      Property StateNormal: jJixState     Read FStateNormal   Write FStateNormal;
      Property StateHover: jJixState      Read FStateHover    Write FStateHover;
      Property StateClick: jJixState      Read FStateClick    Write FStateClick;
      Property StateBack: jJixState       Read FStateBack     Write FStateBack;

      Property Static: Boolean            Read FStatic        Write FStatic        Default False;
      Property IgnoreTempStyle: Boolean   Read FIgnoreTStyle  Write FIgnoreTStyle  Default False;
      Property IgnorePicture: Boolean     Read FIgnorePicture Write FIgnorePicture Default True;
      Property Animate: Boolean           Read FAnimate       Write FAnimate       Default True;

      Property ShowTooltip: Boolean       Read FShowTooltip   Write FShowTooltip   Default False;
      Property Tooltip: TCaption          Read FTooltip       Write FTooltip;

    Published
      Property ThemeName: ThemeStr        Read GetThemeName   Write SetThemeName;
  End;
  {%EndRegion}

Procedure Register;
Procedure ComponentRegistry(Comp: TComponentClass; Palette: String = 'JixUI');
Procedure ComponentRegistry(Comps: Array Of TComponentClass; Palette: String = 'JixUI');

Type
  jComponentList = Specialize jList<TComponentClass>;
  jComponentPalet = Specialize jList<StrS>;

Var
  Components: jComponentList;
  ComponentsName: jComponentPalet;
  AnimSpeed: Int16U = 10;
  CompilingForLazarus: Boolean = True;

Implementation
End.
