Type
  jItem = Class
    Public
      Color: jARGB;
      Focus: Boolean;
      BIcons: TBorderIcons;
  End;

  jFormManager = Class
    Protected
      Controller: TTimer;
      FormList: TList;
      FormItem: jList<jItem>;

      CompTemps: jList<TJixTemplate>;

      Procedure TimerEvent(Sender: TObject);

    Private
      OGetterActiveColor: mBasicARGB;
      OGetterInActiveColor: mBasicARGB;
      OGetterDarkTheme: mBasicBoolean;

      Function GetActiveColor: jARGB;
      Function GetInActiveColor: jARGB;
      Function GetDarkTheme: Boolean;

    Public
      Property GetterActiveColor: mBasicARGB   Read OGetterActiveColor   Write OGetterActiveColor;
      Property GetterInActiveColor: mBasicARGB Read OGetterInActiveColor Write OGetterInActiveColor;
      Property GetterDarkTheme: mBasicBoolean  Read OGetterDarkTheme     Write OGetterDarkTheme;

      Property ActiveColor: jARGB Read GetActiveColor;
      Property InActiveColor: jARGB Read GetInActiveColor;
      Property DarkTheme: Boolean Read GetDarkTheme;

    Public
      Function FindFormTitleColor(Form: TForm): jARGB;
      Function FindFormColor(Form: TForm): jARGB;

      Procedure AddTemplate(Templates: TJixTemplate);
      Property Templates: jList<TJixTemplate> Read CompTemps;

    Public
      Constructor Create;
      Destructor Destroy; Override;
      Procedure AddForm(Form: jCustomJixForm);
      Procedure DeleteForm(Form: jCustomJixForm);
  End;

Var
  FormManager: jFormManager;

