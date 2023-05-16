Unit JixComponent;

{$mode objfpc}{$H+}{$Interfaces Corba}{$Inline On}

Interface

Uses
  Classes, Sysutils, Controls, Graphics, JixPas, PropEdits, Forms,
  Dialogs, BGRABitmap, BGRABitmapTypes, BGRAGradients, BGRAGradientScanner,
  MiniJixoid_JCL, LResources, windows, DefaultTranslator;

Type
  jTemplateCollectionItem = Class;
  TJixControl = Class;

  {%Region JixComponent Interface}
  iJixComponent = Interface
      Procedure SetTemplate(Temp: jTemplateCollectionItem);

      Procedure SetThemeName(Value: String); Inline;
      Function GetThemeName: String; Inline;

      Property ThemeName: String   Read GetThemeName Write SetThemeName;
  End;
  {%EndRegion}

  {%Region JixProperty}
  jJixProperty = Class(TPersistent)
    Private
      Var FOnChanged: mNotify;

      Var MyParent: jJixProperty;

    Protected
      Procedure DoCreate; Virtual;
      Procedure DoChanged; Inline;

    Public
      Procedure {%H-}Assign(Source: jJixProperty); Virtual;
      Constructor Create;
      Constructor Create(Parent: jJixProperty); Overload;

      Property OnChanged: mNotify   Read FOnChanged Write FOnChanged;

  End;
  {%EndRegion}


  {%Region Border}
  eJixBorderStyle = (jbsClear, jbsSolid);

  jJixBorder = Class(jJixProperty)
    Private
      Var FExColor: TColor;
      Var FExOpacity: Int8U;
      Var FExWidth: Int8U;
      Var FInColor: TColor;
      Var FInOpacity: Int8U;
      Var FInWidth: Int8U;
      Var FStyle: eJixBorderStyle;

    Private
      Procedure SetExColor(Value: TColor); Inline;
      Procedure SetExOpacity(Value: Int8U); Inline;
      Procedure SetExWidth(Value: Int8U); Inline;
      Procedure SetInColor(Value: TColor); Inline;
      Procedure SetInOpacity(Value: Int8U); Inline;
      Procedure SetInWidth(Value: Int8U); Inline;
      Procedure SetStyle(Value: eJixBorderStyle); Inline;

    Public
      Procedure Assign(Source: jJixProperty); Override;
      Procedure DoCreate; Override;
      Function GetExColor: jARGB; Inline;
      Function GetInColor: jARGB; Inline;

    Published
      Property ExColor: TColor        Read FExColor   Write SetExColor;
      Property ExOpacity: Int8U       Read FExOpacity Write SetExOpacity;
      Property ExWidth: Int8U         Read FExWidth   Write SetExWidth   Default 1;
      Property InColor: TColor        Read FInColor   Write SetInColor;
      Property InOpacity: Int8U       Read FInOpacity Write SetInOpacity;
      Property InWidth: Int8U         Read FInWidth   Write SetInWidth   Default 0;
      Property Style: eJixBorderStyle Read FStyle     Write SetStyle     Default jbsSolid;
  End;
  {%EndRegion}

  {%Region Background}
  eJixBackgroundStyle = (jbgsClear, jbgsColor, jbgsGradient);
  jJixBackground = Class;

  jGradientPoint = Class(jJixProperty)
    Private
      Var FColor: TColor;
      Var FOpacity: Int8U;
      Var FXPercent: Single;
      Var FYPercent: Single;

      Procedure SetColor(Value: TColor); Inline;
      Procedure SetOpacity(Value: Int8U); Inline;
      Procedure SetXPercent(Value: Single); Inline;
      Procedure SetYPercent(Value: Single); Inline;

    Public
      Procedure Assign(Source: jJixProperty); Override;
      Procedure DoCreate; Override;

    Published
      Property Color: TColor    Read FColor    Write SetColor;
      Property Opacity: Int8U   Read FOpacity  Write SetOpacity;
      Property XPercent: Single Read FXPercent Write SetXPercent;
      Property YPercent: Single Read FYPercent Write SetYPercent;

  End;

  jJixBackground = Class(jJixProperty)
    Private
      Var FColor: TColor;
      Var FOpacity: Int8U;
      Var FStyle: eJixBackgroundStyle;
      Var FGradientType: TGradientType;
      Var FSinus: Boolean;
      Var FPoint1: jGradientPoint;
      Var FPoint2: jGradientPoint;

    Private
      Procedure SetColor(Value: TColor); Inline;
      Procedure SetOpacity(Value: Int8U); Inline;
      Procedure SetStyle(Value: eJixBackgroundStyle); Inline;
      Procedure SetGradientType(Value: TGradientType); Inline;
      Procedure SetSinus(Value: Boolean); Inline;

    Public
      Procedure Assign(Source: jJixProperty); Override;
      Procedure DoCreate; Override;
      Destructor Destroy; Override;

    Published
      Property Color: TColor                    Read FColor        Write SetColor;
      Property Opacity: Int8U                   Read FOpacity      Write SetOpacity;
      Property Style: eJixBackgroundStyle       Read FStyle        Write SetStyle        Default jbgsColor;
      Property GradientType: TGradientType      Read FGradientType Write SetGradientType Default gtLinear;
      Property Sinus: Boolean                   Read FSinus        Write SetSinus        Default False;

      Property Point1: jGradientPoint           Read FPoint1       Write FPoint1;
      Property Point2: jGradientPoint           Read FPoint2       Write FPoint2;
  End;
  {%EndRegion}

  {%Region Font}
  jJixFontEx = Class(jJixProperty)
    Private
      Var FColor: TColor;
      Var FDisColor: TColor;
      Var FOpacity: Int8U;
      Var FHeight: Int16U;
      Var FName: String;
      Var FPadL: Int16S;
      Var FPadT: Int16S;
      Var FPadR: Int16S;
      Var FPadB: Int16S;
      Var FSingleLine: Boolean;
      Var FStyle: TFontStyles;
      Var FWordBreak: Boolean;

      Var FAlignment: TAlignment;
      Var FLayout: TTextLayout;

      Procedure SetColor(Value: TColor);
      Procedure SetDisColor(Value: TColor);
      Procedure SetOpacity(Value: Int8U);
      Procedure SetHeight(Value: Int16U);
      Procedure SetName(Value: String);
      Procedure SetPadL(Value: Int16S);
      Procedure SetPadT(Value: Int16S);
      Procedure SetPadR(Value: Int16S);
      Procedure SetPadB(Value: Int16S);
      Procedure SetSingleLine(Value: Boolean);
      Procedure SetStyle(Value: TFontStyles);
      Procedure SetWordBreak(Value: Boolean);

      Procedure SetAlignment(Value: TAlignment);
      Procedure SetLayout(Value: TTextLayout);

    Public
      Procedure Assign(Source: Jjixproperty); Override;
      Procedure DoCreate; Override;

    Published
      Property Color: TColor         Read FColor      Write SetColor      Default clWhite;
      Property DisabledColor: TColor Read FDisColor   Write SetDisColor   Default clLtGray;
      Property Opacity: Int8U        Read FOpacity    Write SetOpacity    Default 255;
      Property Height: Int16U        Read FHeight     Write SetHeight     Default 12;
      Property Name: String          Read FName       Write SetName;
      Property PaddingLeft: Int16S   Read FPadL       Write SetPadL       Default 0;
      Property PaddingTop: Int16S    Read FPadT       Write SetPadT       Default 0;
      Property PaddingRight: Int16S  Read FPadR       Write SetPadR       Default 0;
      Property PaddingBottom: Int16S Read FPadB       Write SetPadB       Default 0;
      Property SingleLine: Boolean   Read FSingleLine Write SetSingleLine Default True;
      Property Style: TFontStyles    Read FStyle      Write SetStyle      Default [];
      Property WordBreak: Boolean    Read FWordBreak  Write SetWordBreak  Default False;

      Property Alignment: TAlignment Read FAlignment  Write SetAlignment  Default taCenter;
      Property Layout: TTextLayout   Read FLayout     Write SetLayout     Default tlCenter;

  End;
  {%EndRegion}

  {%Region Round}
  jJixRounding = Class(jJixProperty)
    Private
      Var FRoundX: Int8U;
      Var FRoundY: Int8U;
      Var FOptions: TRoundRectangleOptions;

      Procedure SetRoundX(Value: Int8U);
      Procedure SetRoundY(Value: Int8U);
      Procedure SetOptions(Value: TRoundRectangleOptions);

    Public
      Procedure Assign(Source: jJixProperty); Override;
      Procedure DoCreate; Override;

    Published
      Property RoundX: Int8U                   Read FRoundX  Write FRoundX  Default 8;
      Property RoundY: Int8U                   Read FRoundY  Write FRoundY  Default 8;
      Property Options: TRoundRectangleOptions Read FOptions Write FOptions Default [];

  End;
  {%EndRegion}


  {%Region State}
  jJixState = Class(jJixProperty)
    Private
      Var FBackground: jJixBackground;
      Var FBorder: jJixBorder;
      Var FFontEx: jJixFontEx;
      Var FRounding: jJixRounding;

    Public
      Procedure Assign(Source: jJixProperty); Override;
      Procedure DoCreate; Override;
      Destructor Destroy; Override;

    Published
      Property Border: jJixBorder         Read FBorder     Write FBorder;
      Property Background: jJixBackground Read FBackground Write FBackground;
      Property FontEx: jJixFontEx         Read FFontEx     Write FFontEx;
      Property Rounding: jJixRounding     Read FRounding   Write FRounding;
  End;
  {%EndRegion}


  {%Region JixControl}
  TJixControl = Class(TGraphicControl, iJixComponent)
    Private
      Var FStateNormal: jJixState;
      Var FStateHover: jJixState;
      Var FStateClick: jJixState;

      Var FThemeName: String;
      Var FStatic: Boolean;

      Var FShowTooltip: Boolean;
      Var FTooltip: TCaption;

      Procedure SetThemeName(Value: String); Inline;
      Function GetThemeName: String; Inline;

    Private // For static components
      Procedure SetBorder(Value: jJixBorder); Inline;
      Procedure SetBackground(Value: jJixBackground); Inline;
      Procedure SetFontEx(Value: jJixFontEx); Inline;
      Procedure SetRounding(Value: jJixRounding); Inline;

      Function GetBorder: jJixBorder; Inline;
      Function GetBackground: jJixBackground; Inline;
      Function GetFontEx: jJixFontEx; Inline;
      Function GetRounding: jJixRounding; Inline;

    Protected
      Var NowState: jJixState;
      Var Bmp: TBGRABitmap;
      Var ToolTipWindow: THintWindow;

      Procedure StateChanged({%H-}ASender: TObject); Virtual;

      Procedure Paint; Override;

      // For state change catch
      Procedure Mouseenter; Override;
      Procedure Mouseleave; Override;
      Procedure Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;
      Procedure Mouseup(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;
      Procedure Mousemove(Shift: Tshiftstate; X, Y: Integer); Override;

    Public
      Constructor Create(Aowner: Tcomponent); Override;
      Destructor Destroy; Override;

      Procedure SetTemplate(Temp: jTemplateCollectionItem); Virtual;

    Public
      Property Border: jJixBorder         Read GetBorder     Write SetBorder;
      Property Background: jJixBackground Read GetBackground Write SetBackground;
      Property FontEx: jJixFontEx         Read GetFontEx     Write SetFontEx;
      Property Rounding: jJixRounding     Read GetRounding   Write SetRounding;

      Property StateNormal: jJixState     Read FStateNormal  Write FStateNormal;
      Property StateHover: jJixState      Read FStateHover   Write FStateHover;
      Property StateClick: jJixState      Read FStateClick   Write FStateClick;

      Property Static: Boolean            Read FStatic       Write FStatic       Default False;

      Property ShowTooltip: Boolean       Read FShowTooltip  Write FShowTooltip  Default False;
      Property Tooltip: TCaption          Read FTooltip      Write FTooltip;

    Published
      Property ThemeName: String          Read GetThemeName  Write SetThemeName;
  End;
  {%EndRegion}

  {%Region JixWinControl}
  TJixWinControl = Class(TScrollingWinControl, iJixComponent)
    Private
      Var FStateNormal: jJixState;
      Var FStateHover: jJixState;
      Var FStateClick: jJixState;

      Var FThemeName: String;
      Var FStatic: Boolean;

      Var FShowTooltip: Boolean;
      Var FTooltip: TCaption;

      Procedure SetThemeName(Value: String); Inline;
      Function GetThemeName: String; Inline;

    Private // For static components
      Procedure SetBorder(Value: jJixBorder); Inline;
      Procedure SetBackground(Value: jJixBackground); Inline;
      Procedure SetFontEx(Value: jJixFontEx); Inline;
      Procedure SetRounding(Value: jJixRounding); Inline;

      Function GetBorder: jJixBorder; Inline;
      Function GetBackground: jJixBackground; Inline;
      Function GetFontEx: jJixFontEx; Inline;
      Function GetRounding: jJixRounding; Inline;

    Protected
      Var NowState: jJixState;
      Var Bmp: TBGRABitmap;
      Var ToolTipWindow: THintWindow;

      Procedure StateChanged({%H-}ASender: TObject); Virtual;

      Procedure Paint; Override;

      // For state change catch
      Procedure Mouseenter; Override;
      Procedure Mouseleave; Override;
      Procedure Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;
      Procedure Mouseup(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer); Override;
      Procedure Mousemove(Shift: Tshiftstate; X, Y: Integer); Override;

    Public
      Constructor Create(Aowner: Tcomponent); Override;
      Destructor Destroy; Override;

      Procedure SetTemplate(Temp: jTemplateCollectionItem);

    Public
      Property Border: jJixBorder         Read GetBorder     Write SetBorder;
      Property Background: jJixBackground Read GetBackground Write SetBackground;
      Property FontEx: jJixFontEx         Read GetFontEx     Write SetFontEx;
      Property Rounding: jJixRounding     Read GetRounding   Write SetRounding;

      Property StateNormal: jJixState     Read FStateNormal  Write FStateNormal;
      Property StateHover: jJixState      Read FStateHover   Write FStateHover;
      Property StateClick: jJixState      Read FStateClick   Write FStateClick;

      Property Static: Boolean            Read FStatic       Write FStatic       Default False;

      Property ShowTooltip: Boolean       Read FShowTooltip  Write FShowTooltip  Default False;
      Property Tooltip: TCaption          Read FTooltip      Write FTooltip;

    Published
      Property ThemeName: String          Read GetThemeName  Write SetThemeName;
  End;
  {%EndRegion}

  {%Region JixTemplate}
  {$Include Components\JixTemplate\Collection.map.Inc}
  TJixTemplate = class(TComponent)
  private
    FTemplates: jTemplateCollection;

  protected

  public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

  published
    Property Templates: jTemplateCollection Read FTemplates Write FTemplates;

  end;
  {%EndRegion}

procedure Register;

Implementation
Uses
  JForms;

{$Include JCL\JixProperty.cod.inc}
{$Include JCL\JixBorder.cod.inc}
{$Include JCL\JixBackground.cod.inc}
{$Include JCL\JixFont.cod.inc}
{$Include JCL\JixRound.cod.inc}
{$Include JCL\JixState.cod.inc}

{%Region TJixControl}
Constructor TJixControl.Create(Aowner: Tcomponent);
Begin
  Inherited;

  StateNormal:= jJixState.Create;
  StateHover:= jJixState.Create;
  StateClick:= jJixState.Create;

  StateNormal.OnChanged:= @StateChanged;
  StateHover.OnChanged:= @StateChanged;
  StateClick.OnChanged:= @StateChanged;

  NowState:= StateNormal;

  Static:= False;

  ThemeName:= 'Default';
  SetTemplate(FormManager.Templates.Items[0].Templates.Items[0] as jTemplateCollectionItem);
End;

Destructor TJixControl.Destroy;
Begin
  StateNormal.Free;
  StateHover.Free;
  StateClick.Free;

  Inherited;
End;

Procedure TJixControl.Paint;

  Function AddAlpha(Color: TColor; Alpha: Int8U): TBGRAPixel;
  Var
    C: jARGB;
    luma: Byte;
  Begin
    C:= Color;

    if Enabled then
      Result:= BGRA(C.Red, C.Green, C.Blue, Alpha)

    Else Begin
      luma:= Round((0.299 * C.Red) + (0.587 * C.Green) + (0.114 * C.Blue));

      Result:= BGRA(luma, luma, luma, Alpha);
    End;

  End;

Var
  TextStyle: TTextStyle;

  g: TBGRAMultiGradient;
  gs: TBGRAGradientScanner;
Begin
  if not(Visible or (csDesigning in ComponentState)) then Exit;

  Canvas.Changing;

  Bmp:= TBGRABitmap.Create(ClientWidth, ClientHeight, BGRA(0,0,0,0));
  Bmp.JoinStyle:= pjsRound;

  {%Region Background}
  Case NowState.Background.Style of
    jbgsColor:
      Begin
        Bmp.FillRoundRectAntialias(
          0, 0, Width-1, Height-1,
          NowState.Rounding.RoundX, NowState.Rounding.RoundY,
          AddAlpha(NowState.Background.Color ,NowState.Background.Opacity),
          NowState.Rounding.Options
        );
      End;

    jbgsGradient:
      Begin
        g:= TBGRAMultiGradient.Create(
          [
            AddAlpha(
              NowState.Background.Point1.Color,
              NowState.Background.Point1.Opacity
            ),
            AddAlpha(
              NowState.Background.Point2.Color,
              NowState.Background.Point2.Opacity
            )
          ],

          [0, 1], True
        );

        gs:= TBGRAGradientScanner.Create(
          g,
          NowState.Background.GradientType,
          PointF(
            NowState.Background.Point1.XPercent/100*Width,
            NowState.Background.Point1.YPercent/100*Height
          ),
          PointF(
            NowState.Background.Point2.XPercent/100*Width,
            NowState.Background.Point2.YPercent/100*Height
          )
        );
        gs.Sinus:= NowState.Background.Sinus;

        Bmp.FillRoundRectAntialias(
          0, 0, Width-1, Height-1,
          NowState.Rounding.RoundX, NowState.Rounding.RoundY,
          gs, NowState.Rounding.Options
        );
        g.Free;
        gs.Free;
      End;
  End;
  {%EndRegion}

  {%Region Border}
  if NowState.Border.Style = jbsSolid then
  Begin
    Bmp.RoundRectAntialias(
      0, 0, Width-1, Height-1,
      NowState.Rounding.RoundX, NowState.Rounding.RoundY,
      AddAlpha(
        NowState.Border.ExColor,
        NowState.Border.ExOpacity
      ),
      NowState.Border.ExWidth,
      NowState.Rounding.Options
    );
  End;
  {%EndRegion}

  {%Region Text}
  Bmp.FontQuality:= fqSystemClearType;
  Bmp.FontHeight:= NowState.FontEx.Height;
  Bmp.FontName:= NowState.FontEx.Name;
  Bmp.FontStyle:= NowState.FontEx.Style;

  TextStyle.Alignment:= NowState.FontEx.Alignment;
  TextStyle.Layout:= NowState.FontEx.Layout;
  TextStyle.SingleLine:= NowState.FontEx.SingleLine;
  TextStyle.EndEllipsis:= False;
  TextStyle.Wordbreak:= NowState.FontEx.WordBreak;

  Bmp.TextRect(
    Classes.Rect(
      NowState.FontEx.PaddingLeft,
      NowState.FontEx.PaddingTop,
      Width  -NowState.FontEx.PaddingRight,
      Height -NowState.FontEx.PaddingBottom
    ),
    NowState.FontEx.PaddingLeft,
    NowState.FontEx.PaddingTop,

    Caption,
    TextStyle,
    AddAlpha(
      NowState.FontEx.Color,
      255
    )
  );
  {%EndRegion}

  Bmp.Draw(Canvas, 0,0, False);

  Bmp.Free;

  Canvas.Changed;
End;

Procedure TJixControl.Mouseenter;
Var
  ARect: TRect;
  NParent: TForm;
Begin
  Inherited;

  if Assigned(ToolTipWindow) then
  Begin
    ToolTipWindow.Close;
    ToolTipWindow.Free;
    ToolTipWindow:= Nil;
  End;

  if ShowTooltip then
  Begin
    NParent:= FindComponentForm(Self);

    ToolTipWindow:= THintWindow.Create(Nil);

    if Assigned(NParent) then
    Begin

      ARect:= ToolTipWindow.CalcHintRect(Round(NParent.Monitor.Width/2), Tooltip, Nil);

      ToolTipWindow.Color:= NParent.Color;
      ToolTipWindow.Font.Color:= NParent.Font.Color;
    End Else Begin

      ARect:= ToolTipWindow.CalcHintRect(0, Tooltip, Nil);

      ToolTipWindow.Color:= clBlack;
      ToolTipWindow.Font.Color:= clWhite;
    End;

    //AHint.AlphaBlend:= True;
    //AHint.AlphaBlendValue:= 1;

    ToolTipWindow.ActivateHint(
    Classes.Rect(
      Mouse.CursorPos.X+13,
      Mouse.CursorPos.Y+14,
      Mouse.CursorPos.X+ARect.Width+13,
      Mouse.CursorPos.Y+ARect.Height+14
    ),
    Tooltip
  );
  End;

  if Static then
    if NowState <> StateNormal then
    Begin
      NowState:= StateNormal;
      Invalidate;
    End Else Exit;

  NowState:= StateHover;
  Invalidate;
End;

Procedure TJixControl.Mouseleave;
Begin
  Inherited;

  NowState:= StateNormal;
  Invalidate;

  if Assigned(ToolTipWindow) then
  Begin
    ToolTipWindow.Close;
    ToolTipWindow.Free;
    ToolTipWindow:= Nil;
  End;
End;

Procedure TJixControl.Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer);
Begin
  Inherited;
  if Static then
    if NowState <> StateNormal then
    Begin
      NowState:= StateNormal;
      Invalidate;
    End Else Exit;

  if Button = mbLeft then
  Begin
    NowState:= StateClick;
    Invalidate;
  End;
End;

Procedure TJixControl.Mouseup(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer);
Begin
  Inherited;
  if Static then
    if NowState <> StateNormal then
    Begin
      NowState:= StateNormal;
      Invalidate;
    End Else Exit;

  if Button = mbLeft then
  Begin
    if MouseInClient then
      NowState:= StateHover
    else
      NowState:= StateNormal;

    Invalidate;
  End;
End;

Procedure TJixControl.Mousemove(Shift: Tshiftstate; X, Y: Integer);
Begin
  Inherited;

  if ShowTooltip and Assigned(ToolTipWindow) then
  Begin
    ToolTipWindow.Left:= Mouse.CursorPos.X +13;
    ToolTipWindow.Top:=  Mouse.CursorPos.Y +14;
  End;
End;

Procedure TJixControl.StateChanged(ASender: TObject);
Begin
  Invalidate;
End;

Procedure TJixControl.SetTemplate(Temp: jTemplateCollectionItem);
Begin
  if not Assigned(Temp) then
    {$ifopt D-}
      Exit;
    {$Else}
       if
        MessageDlg(
          'Access Violation',
          'Temp parameter isn''t assigned' +#13+#13+
          'Class: '+Self.ClassName +#13+
          'Method: '+'SetTemplate' +#13+
          'Parameter: '+'Temp' +#13,
          mtError, [mbIgnore, mbAbort], 0
        ) = mrAbort
      then
        Raise EAccessViolation.Create('Temp parameter isn''t assigned');
    {$Endif}

  Self.StateNormal.Assign(Temp.StateNormal);
  Self.StateHover.Assign(Temp.StateHover);
  Self.StateClick.Assign(Temp.StateClick);

  if Temp.StaticBackground then begin

    Self.StateHover.Background.Assign(Temp.StateNormal.Background);
    Self.StateClick.Background.Assign(Temp.StateNormal.Background);
  End;

  if Temp.StaticBorder then begin

    Self.StateHover.Border.Assign(Temp.StateNormal.Border);
    Self.StateClick.Border.Assign(Temp.StateNormal.Border);
  End;

  if Temp.StaticFont then begin

    Self.StateHover.FontEx.Assign(Temp.StateNormal.FontEx);
    Self.StateClick.FontEx.Assign(Temp.StateNormal.FontEx);
  End;

  if Temp.StaticRound then begin

    Self.StateHover.Rounding.Assign(Temp.StateNormal.Rounding);
    Self.StateClick.Rounding.Assign(Temp.StateNormal.Rounding);
  End;


  Self.StateNormal.FontEx.Color:= FindFontColor(Temp.BackgroundColor);
  Self.StateHover.FontEx.Color:= FindFontColor(Temp.BackgroundColor);
  Self.StateClick.FontEx.Color:= FindFontColor(Temp.BackgroundColor);
End;

Procedure TJixControl.SetThemeName(Value: String); Inline;
Begin
  FThemeName:= Value;
End;

Function TJixControl.GetThemeName: String; Inline;
Begin
  Result:= FThemeName;
End;

{%Region TJixControl State Pointers}
Procedure TJixControl.SetBorder(Value: jJixBorder);
Begin
  StateNormal.Border:= Value;
End;

Procedure TJixControl.SetBackground(Value: jJixBackground);
Begin
  StateNormal.Background:= Value;
End;

Procedure TJixControl.SetFontEx(Value: jJixFontEx);
Begin
  StateNormal.FontEx:= Value;
End;

Procedure TJixControl.SetRounding(Value: jJixRounding);
Begin
  StateNormal.Rounding:= Value;
End;

Function TJixControl.GetBorder: jJixBorder;
Begin
  Result:= StateNormal.Border;
End;

Function TJixControl.GetBackground: jJixBackground;
Begin
  Result:= StateNormal.Background;
End;

Function TJixControl.GetFontEx: jJixFontEx;
Begin
  Result:= StateNormal.FontEx;
End;

Function TJixControl.GetRounding: jJixRounding;
Begin
  Result:= StateNormal.Rounding;
End;
{%EndRegion}
{%EndRegion}

{%Region TJixWinControl}
Constructor TJixWinControl.Create(Aowner: Tcomponent);
Begin
  Inherited;

  StateNormal:= jJixState.Create;
  StateHover:= jJixState.Create;
  StateClick:= jJixState.Create;

  StateNormal.OnChanged:= @StateChanged;
  StateHover.OnChanged:= @StateChanged;
  StateClick.OnChanged:= @StateChanged;

  NowState:= StateNormal;

  Static:= False;

  ThemeName:= 'Default';
  SetTemplate(FormManager.Templates.Items[0].Templates.Items[0] as jTemplateCollectionItem);
End;

Destructor TJixWinControl.Destroy;
Begin
  StateNormal.Free;
  StateHover.Free;
  StateClick.Free;

  Inherited;
End;

Procedure TJixWinControl.Paint;

  Function AddAlpha(Color: TColor; Alpha: Int8U): TBGRAPixel;
  Var
    C: jARGB;
    luma: Byte;
  Begin
    C:= Color;

    if Enabled then
      Result:= BGRA(C.Red, C.Green, C.Blue, Alpha)

    Else Begin
      luma:= Round((0.299 * C.Red) + (0.587 * C.Green) + (0.114 * C.Blue));

      Result:= BGRA(luma, luma, luma, Alpha);
    End;

  End;

Var
  TextStyle: TTextStyle;

  g: TBGRAMultiGradient;
  gs: TBGRAGradientScanner;
Begin
  if not(Visible or (csDesigning in ComponentState)) then Exit;

  Canvas.Changing;

  Bmp:= TBGRABitmap.Create(ClientWidth, ClientHeight, BGRA(0,0,0,0));
  Bmp.JoinStyle:= pjsRound;

  {%Region Background}
  Case NowState.Background.Style of
    jbgsColor:
      Begin
        Bmp.FillRoundRectAntialias(
          0, 0, Width-1, Height-1,
          NowState.Rounding.RoundX, NowState.Rounding.RoundY,
          AddAlpha(NowState.Background.Color ,NowState.Background.Opacity),
          NowState.Rounding.Options
        );
      End;

    jbgsGradient:
      Begin
        g:= TBGRAMultiGradient.Create(
          [
            AddAlpha(
              NowState.Background.Point1.Color,
              NowState.Background.Point1.Opacity
            ),
            AddAlpha(
              NowState.Background.Point2.Color,
              NowState.Background.Point2.Opacity
            )
          ],

          [0, 1], True
        );

        gs:= TBGRAGradientScanner.Create(
          g,
          NowState.Background.GradientType,
          PointF(
            NowState.Background.Point1.XPercent/100*Width,
            NowState.Background.Point1.YPercent/100*Height
          ),
          PointF(
            NowState.Background.Point2.XPercent/100*Width,
            NowState.Background.Point2.YPercent/100*Height
          )
        );
        gs.Sinus:= NowState.Background.Sinus;

        Bmp.FillRoundRectAntialias(
          0, 0, Width-1, Height-1,
          NowState.Rounding.RoundX, NowState.Rounding.RoundY,
          gs, NowState.Rounding.Options
        );
        g.Free;
        gs.Free;
      End;
  End;
  {%EndRegion}

  {%Region Border}
  if NowState.Border.Style = jbsSolid then
  Begin
    Bmp.RoundRectAntialias(
      0, 0, Width-1, Height-1,
      NowState.Rounding.RoundX, NowState.Rounding.RoundY,
      AddAlpha(
        NowState.Border.ExColor,
        NowState.Border.ExOpacity
      ),
      NowState.Border.ExWidth,
      NowState.Rounding.Options
    );
  End;
  {%EndRegion}

  {%Region Text}
  Bmp.FontQuality:= fqSystemClearType;
  Bmp.FontHeight:= NowState.FontEx.Height;
  Bmp.FontName:= NowState.FontEx.Name;
  Bmp.FontStyle:= NowState.FontEx.Style;

  TextStyle.Alignment:= NowState.FontEx.Alignment;
  TextStyle.Layout:= NowState.FontEx.Layout;
  TextStyle.SingleLine:= NowState.FontEx.SingleLine;
  TextStyle.EndEllipsis:= False;
  TextStyle.Wordbreak:= NowState.FontEx.WordBreak;

  Bmp.TextRect(
    Classes.Rect(
      NowState.FontEx.PaddingLeft,
      NowState.FontEx.PaddingTop,
      Width  -NowState.FontEx.PaddingRight,
      Height -NowState.FontEx.PaddingBottom
    ),
    NowState.FontEx.PaddingLeft,
    NowState.FontEx.PaddingTop,

    Caption,
    TextStyle,
    AddAlpha(
      NowState.FontEx.Color,
      255
    )
  );
  {%EndRegion}

  Bmp.Draw(Canvas, 0,0, False);

  Bmp.Free;

  Canvas.Changed;
End;

Procedure TJixWinControl.Mouseenter;
Var
  ARect: TRect;
  NParent: TForm;
Begin
  Inherited;

  if Assigned(ToolTipWindow) then
  Begin
    ToolTipWindow.Close;
    ToolTipWindow.Free;
    ToolTipWindow:= Nil;
  End;

  if ShowTooltip then
  Begin
    NParent:= FindComponentForm(Self);

    ToolTipWindow:= THintWindow.Create(Nil);

    if Assigned(NParent) then
    Begin

      ARect:= ToolTipWindow.CalcHintRect(Round(NParent.Monitor.Width/2), Tooltip, Nil);

      ToolTipWindow.Color:= NParent.Color;
      ToolTipWindow.Font.Color:= NParent.Font.Color;
    End Else Begin

      ARect:= ToolTipWindow.CalcHintRect(0, Tooltip, Nil);

      ToolTipWindow.Color:= clBlack;
      ToolTipWindow.Font.Color:= clWhite;
    End;

    //AHint.AlphaBlend:= True;
    //AHint.AlphaBlendValue:= 1;

    ToolTipWindow.ActivateHint(
    Classes.Rect(
      Mouse.CursorPos.X+13,
      Mouse.CursorPos.Y+14,
      Mouse.CursorPos.X+ARect.Width+13,
      Mouse.CursorPos.Y+ARect.Height+14
    ),
    Tooltip
  );
  End;

  if Static then
    if NowState <> StateNormal then
    Begin
      NowState:= StateNormal;
      Invalidate;
    End Else Exit;

  NowState:= StateHover;
  Invalidate;
End;

Procedure TJixWinControl.Mouseleave;
Begin
  Inherited;

  NowState:= StateNormal;
  Invalidate;

  if Assigned(ToolTipWindow) then
  Begin
    ToolTipWindow.Close;
    ToolTipWindow.Free;
    ToolTipWindow:= Nil;
  End;
End;

Procedure TJixWinControl.Mousedown(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer);
Begin
  Inherited;
  if Static then
    if NowState <> StateNormal then
    Begin
      NowState:= StateNormal;
      Invalidate;
    End Else Exit;

  if Button = mbLeft then
  Begin
    NowState:= StateClick;
    Invalidate;
  End;
End;

Procedure TJixWinControl.Mouseup(Button: Tmousebutton; Shift: Tshiftstate; X, Y: Integer);
Begin
  Inherited;
  if Static then
    if NowState <> StateNormal then
    Begin
      NowState:= StateNormal;
      Invalidate;
    End Else Exit;

  if Button = mbLeft then
  Begin
    if MouseInClient then
      NowState:= StateHover
    else
      NowState:= StateNormal;

    Invalidate;
  End;
End;

Procedure TJixWinControl.Mousemove(Shift: Tshiftstate; X, Y: Integer);
Begin
  Inherited;

  if ShowTooltip and Assigned(ToolTipWindow) then
  Begin
    ToolTipWindow.Left:= Mouse.CursorPos.X +13;
    ToolTipWindow.Top:=  Mouse.CursorPos.Y +14;
  End;
End;

Procedure TJixWinControl.StateChanged(ASender: TObject);
Begin
  Invalidate;
End;

Procedure TJixWinControl.SetTemplate(Temp: jTemplateCollectionItem);
Begin
  if not Assigned(Temp) then
    {$ifopt D-}
      Exit;
    {$Else}
       if
        MessageDlg(
          'Access Violation',
          'Temp parameter isn''t assigned' +#13+#13+
          'Class: '+Self.ClassName +#13+
          'Method: '+'SetTemplate' +#13+
          'Parameter: '+'Temp' +#13,
          mtError, [mbIgnore, mbAbort], 0
        ) = mrAbort
      then
        Raise EAccessViolation.Create('Temp parameter isn''t assigned');
    {$Endif}

  Self.StateNormal.Assign(Temp.StateNormal);
  Self.StateHover.Assign(Temp.StateHover);
  Self.StateClick.Assign(Temp.StateClick);

  if Temp.StaticBackground then begin

    Self.StateHover.Background.Assign(Temp.StateNormal.Background);
    Self.StateClick.Background.Assign(Temp.StateNormal.Background);
  End;

  if Temp.StaticBorder then begin

    Self.StateHover.Border.Assign(Temp.StateNormal.Border);
    Self.StateClick.Border.Assign(Temp.StateNormal.Border);
  End;

  if Temp.StaticFont then begin

    Self.StateHover.FontEx.Assign(Temp.StateNormal.FontEx);
    Self.StateClick.FontEx.Assign(Temp.StateNormal.FontEx);
  End;

  if Temp.StaticRound then begin

    Self.StateHover.Rounding.Assign(Temp.StateNormal.Rounding);
    Self.StateClick.Rounding.Assign(Temp.StateNormal.Rounding);
  End;


  Self.StateNormal.FontEx.Color:= FindFontColor(Temp.BackgroundColor);
  Self.StateHover.FontEx.Color:= FindFontColor(Temp.BackgroundColor);
  Self.StateClick.FontEx.Color:= FindFontColor(Temp.BackgroundColor);
End;

Procedure TJixWinControl.SetThemeName(Value: String); Inline;
Begin
  FThemeName:= Value;
End;

Function TJixWinControl.GetThemeName: String; Inline;
Begin
  Result:= FThemeName;
End;

{%Region TJixControl State Pointers}
Procedure TJixWinControl.SetBorder(Value: jJixBorder);
Begin
  StateNormal.Border:= Value;
End;

Procedure TJixWinControl.SetBackground(Value: jJixBackground);
Begin
  StateNormal.Background:= Value;
End;

Procedure TJixWinControl.SetFontEx(Value: jJixFontEx);
Begin
  StateNormal.FontEx:= Value;
End;

Procedure TJixWinControl.SetRounding(Value: jJixRounding);
Begin
  StateNormal.Rounding:= Value;
End;

Function TJixWinControl.GetBorder: jJixBorder;
Begin
  Result:= StateNormal.Border;
End;

Function TJixWinControl.GetBackground: jJixBackground;
Begin
  Result:= StateNormal.Background;
End;

Function TJixWinControl.GetFontEx: jJixFontEx;
Begin
  Result:= StateNormal.FontEx;
End;

Function TJixWinControl.GetRounding: jJixRounding;
Begin
  Result:= StateNormal.Rounding;
End;
{%EndRegion}
{%EndRegion}

{%Region JixTemplate}
Constructor TJixTemplate.Create(AOwner: TComponent);
Begin
  Inherited;

  Templates:= jTemplateCollection.Create(jTemplateCollectionItem);

  FormManager.AddTemplate(Self);
End;

Destructor TJixTemplate.Destroy;
Begin
  Templates.Free;

  Inherited;
End;
{$Include Components\JixTemplate\Collection.cod.Inc}
{%EndRegion}

procedure Register;
begin
  {$I Components\JixTemplate\JixTemplate_icon.lrs}
  RegisterComponents('JixUI',[TJixTemplate]);
end;

End.

