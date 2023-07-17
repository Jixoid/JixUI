unit JCL.Comps.List;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Utils,
  JCL.Controls,
  JCL.ControlsEx,
  JCL.Comps.Button,
  JCL.CompTypes,
  JCL.Comps.PaintBox,
  Controls, LResources, LazUTF8, Graphics, Dialogs, BGRABitmapTypes, Math;

type
  eJixListOption = (
    jloCloseWhenPressed
  );
  sJixListOptions = Set Of eJixListOption;

  TJixList = class(TJixScrollingControl)
  private
    Var FItems: TStrings;
    Var FOptions: sJixListOptions;
    Procedure SetOptions(Value: sJixListOptions);

    Var FItemIndex: Int16S;
    Procedure SetItemIndex(Value: Int16S);

    Var FHoverIndex: Int16S;
    Procedure SetHoverIndex(Value: Int16S);

    Var FButH: Int16U;
    Procedure SetButF(Value: Int16U);

    Var FOnChange: mNotify;

    Var FSubPanel: TJixPaintBox;
    Var FBut: TJixButton;

    Procedure SetItems(Value: TStrings);
    Function GetItems: TStrings;

  protected
    Procedure SetThemeName(Value: ThemeStr); Override;

    Procedure MouseLeave; Override;

  public
    Constructor Create(Aowner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure PaintPaintBoxBefore(Sender: TObject);
    Procedure PaintPaintBox(Sender: TObject);
    Procedure MDownPaintBox(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    Procedure MMovePaintBox(Sender: TObject; Shift: TShiftState; {%H-}X, Y: Integer);
    Procedure MUpPaintBox(Sender: TObject; Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, Y: Integer);
    Procedure ClickPaintBox(Sender: TObject);
    Procedure DblClickPaintBox(Sender: TObject);
    Procedure MEnterPaintBox(Sender: TObject);
    Procedure MLeavePaintBox(Sender: TObject);

  published
    Property Options: sJixListOptions Read FOptions Write SetOptions Default [];

    Property Items: TStrings    Read GetItems    Write SetItems;
    Property ItemIndex: Int16S  Read FItemIndex  Write SetItemIndex  Default -1;
    Property HoverIndex: Int16S Read FHoverIndex Write SetHoverIndex;
    Property ItemHeight: Int16U Read FButH       Write SetButF       Default 20;

    Property OnChange: mNotify  Read FOnChange   Write FOnChange;

    Property Tooltip;
    Property ShowTooltip;

    Property StateNormal;
    Property StateHover;
    Property StateClick;
    Property StateBack;

    Property IgnoreTempStyle;

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
