Unit JCL.CompTypes;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, BGRABitmapTypes, Graphics, Dialogs,
  {$IfOpt D+}System.UITypes,{$Endif}
  JCL.Utils, JCL.Auxiliary;

Type

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
eJixBackgroundStyle = (jbgsClear, jbgsColor, jbgsGradient, jbgsNi);
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
    Property Style: eJixBackgroundStyle       Read FStyle        Write SetStyle;
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

    Function GetColor: jARGB;

  Published
    Property Color: TColor         Read FColor      Write SetColor;
    Property DisabledColor: TColor Read FDisColor   Write SetDisColor   Default clLtGray;
    Property Opacity: Int8U        Read FOpacity    Write SetOpacity    Default 255;
    Property Height: Int16U        Read FHeight     Write SetHeight     Default 12;
    Property Name: String          Read FName       Write SetName;
    Property PaddingLeft: Int16S   Read FPadL       Write SetPadL       Default 0;
    Property PaddingTop: Int16S    Read FPadT       Write SetPadT       Default 0;
    Property PaddingRight: Int16S  Read FPadR       Write SetPadR       Default 0;
    Property PaddingBottom: Int16S Read FPadB       Write SetPadB       Default 0;
    Property SingleLine: Boolean   Read FSingleLine Write SetSingleLine Default False;
    Property Style: TFontStyles    Read FStyle      Write SetStyle      Default [];
    Property WordBreak: Boolean    Read FWordBreak  Write SetWordBreak  Default False;

    Property Alignment: TAlignment Read FAlignment  Write SetAlignment  Default taCenter;
    Property Layout: TTextLayout   Read FLayout     Write SetLayout     Default tlCenter;

End;
{%EndRegion}

{%Region Picture}
eJixPictureLocate =(
  jplCenter,
  jplFill,
  jplLeftCenter,
  jplLeftTop,
  jplLeftBottom,
  jplRightCenter,
  jplRightTop,
  jplRightBottom,
  jplTopCenter,
  jplBottomCenter,
  jplTextLeft,
  jplTextRight,
  jplTextTop,
  jplTextBottom
);

jJixPicture = Class(jJixProperty)
  Private
    Var FPicture: TPicture;
    Procedure SetPicture(Value: TPicture);

    Var FScale: Single;
    Procedure SetScale(Value: Single);

    Var FOpacity: Int8U;
    Procedure SetOpacity(Value: Int8U);

    Var FLocate: eJixPictureLocate;
    Procedure SetLocate(Value: eJixPictureLocate);

    Var FDisToText: Int16S;
    Procedure SetDisToText(Value: Int16S);

  Public
    Procedure DoCreate; Override;
    Destructor Destroy; Override;

    Procedure Assign(Source: jJixProperty); Override;

  Published
    Property Picture: TPicture         Read FPicture   Write SetPicture;
    Property Scale: Single             Read FScale     Write SetScale;
    Property Opacity: Int8U            Read FOpacity   Write SetOpacity;
    Property Locate: eJixPictureLocate Read FLocate    Write SetLocate;
    Property DistanceToText: Int16S    Read FDisToText Write SetDisToText;

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
    Property RoundX: Int8U                   Read FRoundX  Write SetRoundX;
    Property RoundY: Int8U                   Read FRoundY  Write SetRoundY;
    Property Options: TRoundRectangleOptions Read FOptions Write SetOptions;

End;
{%EndRegion}


{%Region State}
jJixState = Class(jJixProperty)
  Private
    Var FBackground: jJixBackground;
    Var FBorder: jJixBorder;
    Var FFontEx: jJixFontEx;
    Var FPicture: jJixPicture;
    Var FRounding: jJixRounding;

    Var FOpacity: Int8U;
    Procedure SetOpacity(Value:  Int8U);

  Public
    Procedure Assign(Source: jJixProperty); Override;
    Procedure Assign(Source: jJixProperty; IgnorePicture: Boolean); Overload;
    Procedure DoCreate; Override;
    Destructor Destroy; Override;

  Published
    Property Border: jJixBorder         Read FBorder     Write FBorder;
    Property Background: jJixBackground Read FBackground Write FBackground;
    Property FontEx: jJixFontEx         Read FFontEx     Write FFontEx;
    Property Picture: jJixPicture       Read FPicture    Write FPicture;
    Property Rounding: jJixRounding     Read FRounding   Write FRounding;
    Property Opacity: Int8U             Read FOpacity    Write SetOpacity   Default 255;
End;
{%EndRegion}

Implementation
End.
