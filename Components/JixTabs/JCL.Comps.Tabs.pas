unit JCL.Comps.Tabs;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Controls,
  JCL.ControlsEx,
  JCL.Comps.Button,
  JCL.CompTypes,
  JCL.Comps.PaintBox,
  JCL.Generics,
  JCL.Utils,
  Controls, LResources, BGRABitmapTypes, Math, LazUTF8, Graphics, Dialogs;

type
  jRectList = Specialize jList<TRect>;
  mClose = Procedure(Sender: TObject; Index: Int16S) of object;
  mChangeOrder = Procedure(Sender: TObject; Index, ToIndex: Int16S) of object;

  eJixTabOption = (
    jtoCloseBut,
    jtoOrderChangable
  );
  sJixTabOptions = Set Of eJixTabOption;


  TJixTabs = class(TJixScrollingControl)
  private
    Var FTabs: TStrings;
    Var FTabIndex: Int16S;
    Var FHoverIndex: Int16S;
    Var FDownIndex: Int16S;
    Var FButsRect: jRectList;
    Var FOptions: sJixTabOptions;
    Procedure SetOptions(Value: sJixTabOptions);

    Var FButH: Int16U;

    Var FOnChange: mNotify;
    Var FOnChangeOrder: mChangeOrder;
    Var FOnClose: mClose;

    Var FSubPanel: TJixPaintBox;
    Var FBut: TJixButton;

    Procedure SetTabs(Value: TStrings);
    Function GetTabs: TStrings;

    Procedure SetTabIndex(Value: Int16S);

  protected
    Procedure SetThemeName(Value: ThemeStr); Override;

  public
    Procedure StateChanged({%H-}ASender: TObject); Override;

    Constructor Create(Aowner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure PaintPaintBox(Sender: TObject);
    Procedure MDownPaintBox(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    Procedure MUpPaintBox(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    Procedure MMovePaintBox(Sender: TObject; Shift: TShiftState; X, {%H-}Y: Integer);

  published
    Property Tabs: TStrings    Read GetTabs    Write SetTabs;
    Property TabIndex: Int16S  Read FTabIndex  Write SetTabIndex Default -1;
    Property Options: sJixTabOptions Read FOptions Write SetOptions Default [];

    Property OnChange: mNotify Read FOnChange  Write FOnChange;
    Property OnChangeOrder: mChangeOrder Read FOnChangeOrder Write FOnChangeOrder;
    Property OnClose: mClose   Read FOnClose   Write FOnClose;

    Property Tooltip;
    Property ShowTooltip;

    Property StateNormal;
    Property StateHover;
    Property StateClick;
    Property StateBack;

    Property IgnoreTempStyle;
    Property IgnorePicture;

    Property Align;
    Property BorderSpacing;
    Property Anchors;
    //Property AutoSize;
    Property Caption;
    Property Enabled;
    Property Visible;

    Property OnChangeBounds;
    Property OnPaintBefore;
    Property OnPaintAfter;
    Property OnClick;
    Property OnDblClick;
    Property OnMouseDown;
    Property OnMouseEnter;
    Property OnMouseLeave;
    Property OnMouseMove;
    Property OnMouseUp;
    Property OnMouseWheel;
    Property OnMouseWheelDown;
    Property OnMouseWheelUp;
    Property OnResize;
    Property PopupMenu;
  end;

Procedure Register;

Implementation
End.
