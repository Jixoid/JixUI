Type
  mNotify = TNotifyEvent;
  mChangeTheme = Procedure(Sender: TObject; ThemeColor: jARGB; ThemeDark: Boolean; FontColor: jARGB) of object;
  mChangePPI = Procedure(Sender: TObject; NewPPI, LegPPI: Integer) of object;

Type
  eHorzAlign = (haLeft, haRight, haCenter);
  eHorzAlignNC = haLeft..haRight;

  eVertAlign = (vaTop, vaBottom, vaCenter);
  eVertAlignNC = vaTop..vaBottom;

  jJixForm_Title = Class
    Protected
      OCustom_Caption: TBCLabel;
      OCustom_Halt: TBCButton;
      OCustom_Maximize: TBCButton;
      OCustom_Minimize: TBCButton;
      OCustom_Help: TBCButton;

    Private
      OUpper: TForm;
      OCaption: TCaption;
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

      Property Custom_Caption: TBCLabel   Read OCustom_Caption  Write OCustom_Caption;
      Property Custom_Halt: TBCButton     Read OCustom_Halt     Write OCustom_Halt;
      property Custom_Maximize: TBCButton Read OCustom_Maximize Write OCustom_Maximize;
      Property Custom_Minimize: TBCButton Read OCustom_Minimize Write OCustom_Minimize;
      Property Custom_Help: TBCButton     Read OCustom_Help     Write OCustom_Help;
      Property ForeColor: jARGB           Read GetForeColor     Write SetForeColor;

    Published
      Property Caption: TCaption          Read GetCaption       Write SetCaption;

      Property TextPosition: eHorzAlign      Read OTextPosition    Write SetTextPosition    Default haLeft;
      Property ButtonsPosition: eHorzAlignNC Read OButtonsPosition Write SetButtonsPosition Default haRight;
 end;

  jCustomJixForm = class(TForm)
    Private
      //Sub Class
      OTitle: jJixForm_Title;

      //Event
      OWhenFocus: mNotify;
      OWhenDeFocus: mNotify;
      OWhenHelp: mNotify;

      //Using Self
      OActiveTheme: Boolean;

      OWhenChangeTheme: mChangeTheme;
      OWhenChangePPI: mChangePPI;

      Property Caption;

    Public
      Constructor Create(TheOwner: TComponent); override;

    Published
      Property ActiveTheme: Boolean Read OActiveTheme Write OActiveTheme;

      //Sub Class
      Property Title: jJixForm_Title read OTitle write OTitle;

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
