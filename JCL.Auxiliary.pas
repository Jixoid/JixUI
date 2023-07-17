Unit JCL.Auxiliary;

{$Mode ObjFPC}{$H+}{$ModeSwitch AdvancedRecords}

interface

uses
  Windows, Classes, SysUtils, StrUtils, Registry, Controls,
  Forms, FPReadPNG, FPImage, FileUtil, Graphics,
  JCL.Utils;

Function FindComponentForm(bComp: TControl): TForm;

function Scaling(bForm: TCustomDesignControl; Value: Double): Double;
function ScalingBorder(bForm: TCustomDesignControl): Integer;

Function FindFontColor(bColor: jARGB): jARGB;
Function FindFontColor(bColor: jARGB): Boolean;

function GetDarkTheme: Boolean;
function GetThemeColor: jARGB;
function GetThemeColorInactive: jARGB;
Function IsWindowFrameEnabled: Boolean;

Function FormatFileSize(Boyut: Int64U): StrA;


Procedure CopyDir(Source, Dest: StrA);
function DeleteDirectory(DirName: StrA): Boolean;

Procedure FormRadial(Form: TWinControl; Radial: Integer);

Procedure SetIcon(ImageList: TImageList; Index: Integer; PIcon: TIcon);
Procedure ButClick(Sender: TControl);
Procedure ProcedureRun(Sender: TControl; Proc: TNotifyEvent);
Procedure AnimateClose(Sender: TForm);
//Procedure ButtonRadialEffect(Button: TJixButton; X, Y: Int32S);
//Procedure ButtonRadialEffectHover(Button: TJixButton; X, Y: Int32S);
//Procedure ButtonRadialEffectNormal(Button: TJixButton; X, Y: Int32S);

Function FindWindowByTitle(WindowTitle: StrA): Hwnd;
Procedure Attach2Control(aWnd: Hwnd; aControl: TWinControl);

Procedure GetImageSize(ImagePath: StrA; var Width: Integer; var Height: Integer);

Function MatchTest(KeyWord: StrA; A: TStrings): Boolean;
Function MatchTest(KeyWord: StrA; A: TStrings): Integer;

Function StringsToAOS(A: TStringList): aStrA;
Function AOSToStrings(A: aStrA): TStringList;

Var
  AnimAnimate: Boolean = True;

Type
  rAnimRoutine = Packed Record
    Public
      FSL,FFL,
      FST,FFT,
      FSW,FFW,
      FSH,FFH: Int32S;
      FElement: TControl;

      FIgnoreLeft: Boolean;
      FIgnoreTop: Boolean;
      FIgnoreWidth: Boolean;
      FIgnoreHeight: Boolean;

      Constructor Create(SL,FL,ST,FT,SW,FW,SH,FH: Int32S; Element: TControl; IgnoreLeft: Boolean = False; IgnoreTop: Boolean = False; IgnoreWidth: Boolean = False; IgnoreHeight: Boolean = False);
  End;

Procedure Anim(AnimRoutines: Array of rAnimRoutine);
Procedure Anim(AnimRoutine: rAnimRoutine);

Implementation
End.
