unit JCL.Comps.ComboBox;

{$mode ObjFPC}{$H+}{$Inline On}

interface

uses
  Classes, SysUtils,
  JCL.Forms,
  JCL.Utils,
  JCL.Controls,
  LResources, Forms, Controls, Graphics, Dialogs, Math, BGRABitmap,
  BGRABitmapTypes, BGRAGradients, BGRAGradientScanner, JCL.Auxiliary,
  LazUTF8, StdCtrls;

type
  TJixComboBox = class(TJixControl)
  Private
    Var FOnChange: mNotify;

    Var FItems: TStrings;
    Var FItemIndex: Int16S;

    Var FEdgeSpace: Int8U;
    Procedure SetEdgeSpace(Value: Int8U);

    Function GetItems: TStrings;
    Procedure SetItems(Value: TStrings);

    Procedure SetItemIndex(Value: Int16S);

    Procedure ListClick(Sender: TObject);

  Protected
    Var SelForm: TForm;

    Procedure Click; Override;

  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure PaintOver(ACanvas: TCanvas; X, Y: Integer); Override;

  Published
    Property OnChange: mNotify            Read FOnChange     Write FOnChange;

    Property Items: TStrings    Read GetItems    Write SetItems;
    Property ItemIndex: Int16S  Read FItemIndex  Write SetItemIndex Default -1;
    Property EdgeSpace: Int8U   Read FEdgeSpace  Write SetEdgeSpace Default 16;

    Property Tooltip;
    Property ShowTooltip;

    Property Static;
    Property IgnoreTempStyle;
    Property IgnorePicture;

    Property Align;
    Property BorderSpacing;
    Property Anchors;
    Property AutoSize;
    Property Caption;
    Property Enabled;
    Property Visible;

    Property StateNormal;
    Property StateHover;
    Property StateClick;

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
  End;

  Procedure Register;

Implementation
End.
