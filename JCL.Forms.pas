{  $Id$  }
{
JForms
|
|- Ver: 0.5
|- Status: Beta
|- Meaning: JixForm
}
Unit JCL.Forms;

{$Mode Delphi}{$M+}{$ModeSwitch ArrayOperators}

{$Warn 5062 off}
interface

Uses
  Classes, SysUtils,  Controls, Forms, ExtCtrls, Dialogs, Windows, Graphics,
  BCButton, BCPanel, BCTypes, JCL.Generics, JCL.Auxiliary, JCL.Utils;

Type
  mChangeTheme = Procedure(Sender: TObject; ThemeColor: jARGB; ThemeDark: Boolean; FontColor: jARGB) of object;
  mChangePPI = Procedure(Sender: TObject; NewPPI, LegPPI: Integer) of object;

Type
  eHorzAlign = (haLeft, haRight, haCenter);
  eHorzAlignNC = haLeft..haRight;

  eVertAlign = (vaTop, vaBottom, vaCenter);
  eVertAlignNC = vaTop..vaBottom;

  jJixForm_Title = Class
    Protected
      OCustom_Caption: TBCPanel;
      OCustom_Halt: TBCButton;
      OCustom_Maximize: TBCButton;
      OCustom_Minimize: TBCButton;
      OCustom_Help: TBCButton;

    Private
      OUpper: TForm;
      OTextPosition: eHorzAlign;
      OButtonsPosition: eHorzAlignNC;

      Procedure SetTextPosition(Value: eHorzAlign);
      Procedure SetButtonsPosition(Value: eHorzAlignNC);

      Function  GetCaption: TCaption;
      Procedure SetCaption(Value: TCaption);

      Function GetForeColor: jARGB;
      Procedure SetForeColor(Value: jARGB);

    Public
      Constructor Create(UpForm: TForm); virtual;
      Destructor Destroy; Override;

      Property Custom_Caption: TBCPanel      Read OCustom_Caption  Write OCustom_Caption;
      Property Custom_Halt: TBCButton        Read OCustom_Halt     Write OCustom_Halt;
      property Custom_Maximize: TBCButton    Read OCustom_Maximize Write OCustom_Maximize;
      Property Custom_Minimize: TBCButton    Read OCustom_Minimize Write OCustom_Minimize;
      Property Custom_Help: TBCButton        Read OCustom_Help     Write OCustom_Help;
      Property ForeColor: jARGB              Read GetForeColor     Write SetForeColor;

    Published
      Property Caption: TCaption             Read GetCaption       Write SetCaption;

      Property TextPosition: eHorzAlign      Read OTextPosition    Write SetTextPosition    Default haLeft;
      Property ButtonsPosition: eHorzAlignNC Read OButtonsPosition Write SetButtonsPosition Default haRight;
 end;

  jCustomJixForm = class(TForm)
    Private
      Var FThemeName: String;

      //Sub Class
      Var OTitle: jJixForm_Title;

      //Event
      Var OWhenFocus: mNotify;
      Var OWhenDeFocus: mNotify;
      Var OWhenHelp: mNotify;

      Var OWhenChangeTheme: mChangeTheme;
      Var OWhenChangePPI: mChangePPI;

      Var FPPi: Int16U;

    Protected

    Public
      Constructor Create(TheOwner: TComponent); Override;
      Destructor Destroy; Override;

      Procedure OpenAnimate(Apply: Boolean = True);
      Procedure CloseAnimate(Apply: Boolean = True);

      Procedure PaintTo(ACanvas: TCanvas; X, Y: Integer); Overload;

      Procedure ShowForce;

    Published
      Var PopupsWindow: TForm;

      Property ThemeName: String             Read FThemeName       Write FThemeName;

      //Sub Class
      Property Title: jJixForm_Title Read OTitle Write OTitle;

      Property PPi: Int16U           Read FPPi   Write FPPi  Default 96;

      //Event
      Property WhenChangeTheme: mChangeTheme Read OWhenChangeTheme Write OWhenChangeTheme;
      Property WhenChangePPI: mChangePPI     Read OWhenChangePPI   Write OWhenChangePPI;
      Property WhenFocus: mNotify            Read OWhenFocus       Write OWhenFocus;
      Property WhenDeFocus: mNotify          Read OWhenDeFocus     Write OWhenDeFocus;
      Property WhenHelp: mNotify             Read OWhenHelp        Write OWhenHelp;
  end;

  jJixForm = class(jCustomJixForm)
    Published
      //Sub Class
      Property Title;

      //Property
      Property WhenChangeTheme;
      Property WhenChangePPI;
      Property WhenFocus;
      Property WhenDeFocus;
      Property WhenHelp;
  end;

Implementation
End.
