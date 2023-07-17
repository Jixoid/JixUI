  mPaintTemplate = Procedure(Sender: TObject; Color: jARGB; Dark: Boolean) of object;
  eDarkTheme = (dtAuto, dtDark, dtLight, dtCustom);

  jTemplateCollection = Class(TCollection)

  End;

  jTemplateCollectionItem = Class(TCollectionItem)
    Private
      Var FAnimate: Boolean;

      Var FIgnoreRound: Boolean;
      Var FDarkTheme: eDarkTheme;
      Var FBackgroundColor: TColor;
      Var FThemeName: String;
      Var FStateNormal: jJixState;
      Var FStateHover: jJixState;
      Var FStateClick: jJixState;
      Var FStateBack: jJixState;

      Var FOnPaintTemplate: mPaintTemplate;

      Var FStaticBackground: Boolean;
      Var FStaticBorder: Boolean;
      Var FStaticFont: Boolean;
      Var FStaticRound: Boolean;

      Procedure SetDarkTheme(Value: eDarkTheme);

    Public
      Procedure Assign(Source: Tpersistent); Override;
      Procedure Assignto(Dest: Tpersistent); Override;

      Constructor Create(ACollection: TCollection); Override;
      Destructor Destroy; Override;

      Procedure PaintTemplate(Color: jARGB; Dark: Boolean);

    Published
      Property Animate: Boolean                Read FAnimate          Write FAnimate         Default True;

      Property ThemeName: String               Read FThemeName        Write FThemeName;

      Property IgnoreRound: Boolean            Read FIgnoreRound      Write FIgnoreRound     Default False;
      Property DarkTheme: eDarkTheme           Read FDarkTheme        Write FDarkTheme       Default dtAuto;
      Property BackgroundColor: TColor         Read FBackgroundColor  Write FBackgroundColor Default clBlack;
      Property StateNormal: jJixState          Read FStateNormal      Write FStateNormal;
      Property StateHover: jJixState           Read FStateHover       Write FStateHover;
      Property StateClick: jJixState           Read FStateClick       Write FStateClick;
      Property StateBack: jJixState            Read FStateBack        Write FStateBack;

      Property StaticBackground: Boolean       Read FStaticBackground Write FStaticBackground;
      Property StaticBorder: Boolean           Read FStaticBorder     Write FStaticBorder;
      Property StaticFont: Boolean             Read FStaticFont       Write FStaticFont;
      Property StaticRound: Boolean            Read FStaticRound      Write FStaticRound;

      Property OnPaintTemplate: mPaintTemplate Read FOnPaintTemplate Write FOnPaintTemplate;
  End;
