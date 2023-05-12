Type
  mPaintTemplate = Procedure(Sender: TObject; Color: jARGB; Dark: Boolean) of object;

  jTemplateCollection = Class(TCollection)

  End;

  jTemplateCollectionItem = Class(TCollectionItem)
    Private
      Var FThemeName: String;
      Var FStateNormal: TBCButtonState;
      Var FStateHover: TBCButtonState;
      Var FStateClick: TBCButtonState;
      Var FCornerRound: Int8U;

      Var FOnPaintTemplate: mPaintTemplate;

    Public
      Constructor Create(ACollection: TCollection); Override;
      Destructor Destroy; Override;

      Procedure PaintTemplate(Color: jARGB; Dark: Boolean);

    Published
      Property ThemeName: String             Read FThemeName     Write FThemeName;
      Property StateNormal: TBCButtonState   Read FStateNormal   Write FStateNormal;
      Property StateHover: TBCButtonState    Read FStateHover    Write FStateHover;
      Property StateClick: TBCButtonState    Read FStateClick    Write FStateClick;
      Property CornerRound: Int8U            Read FCornerRound   Write FCornerRound;

      Property OnPaintTemplate: mPaintTemplate Read FOnPaintTemplate Write FOnPaintTemplate;
  End;
