Type
  mPaintTemplate = Procedure(Sender: TObject; Color: jARGB; Dark: Boolean) of object;
  eDarkTheme = (dtAuto, dtDark, dtLight, dtCustom);

  jTemplateCollection = Class(TCollection)

  End;

  jTemplateCollectionItem = Class(TCollectionItem)
    Private
      Var FIgnoreRound: Boolean;
      Var FDarkTheme: eDarkTheme;
      Var FBackgroundColor: TColor;
      Var FThemeName: String;
      Var FStateNormal: TBCButtonState;
      Var FStateHover: TBCButtonState;
      Var FStateClick: TBCButtonState;
      Var FCornerRound: Int8U;

      Var FOnPaintTemplate: mPaintTemplate;

      Procedure SetDarkTheme(Value: eDarkTheme);

    Public
      Procedure Assign(Source: Tpersistent); Override;
      Procedure Assignto(Dest: Tpersistent); Override;

      Constructor Create(ACollection: TCollection); Override;
      Destructor Destroy; Override;

      Procedure PaintTemplate(Color: jARGB; Dark: Boolean);

    Published
      Property ThemeName: String               Read FThemeName       Write FThemeName;

      Property IgnoreRound: Boolean            Read FIgnoreRound     Write FIgnoreRound     Default False;
      Property DarkTheme: eDarkTheme           Read FDarkTheme       Write FDarkTheme       Default dtAuto;
      Property BackgroundColor: TColor         Read FBackgroundColor Write FBackgroundColor Default clBlack;
      Property StateNormal: TBCButtonState     Read FStateNormal     Write FStateNormal;
      Property StateHover: TBCButtonState      Read FStateHover      Write FStateHover;
      Property StateClick: TBCButtonState      Read FStateClick      Write FStateClick;
      Property CornerRound: Int8U              Read FCornerRound     Write FCornerRound     Default 12;

      Property OnPaintTemplate: mPaintTemplate Read FOnPaintTemplate Write FOnPaintTemplate;
  End;
