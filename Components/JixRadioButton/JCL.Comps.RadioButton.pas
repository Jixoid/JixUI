unit JCL.Comps.RadioButton;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Utils,
  JCL.Controls,
  LResources, Forms, Controls, Graphics, Dialogs, BGRABitmap, BGRABitmapTypes,
  BGRAGradients, BGRAGradientScanner;

type
  TJixRadioButton = class(TJixControl)
  Private
    Var FWorkSpace: String;

    Var FReadOnly: Boolean;
    Var FChecked: Boolean;
    Var FRCTF: Boolean;
    Var FOnChange: mNotify;

    Var FButtonSize: Int8U;

    Procedure SetChecked(Value: Boolean);
    Procedure SetButtonSize(Value: Int8U);

  Protected
    Procedure Click; Override;

  Public
    Constructor Create(AOwner: TComponent); Override;

    Procedure PaintOver(ACanvas: TCanvas; X, Y: Integer); Override;

  Published
    Property Tooltip;
    Property ShowTooltip;

    Property Static;
    Property IgnoreTempStyle;
    Property IgnorePicture;

    Property Align;
    Property BorderSpacing;
    Property Anchors;
    Property AutoSize Default True;
    Property Caption;
    Property Enabled;
    Property Visible;

    Property ReadOnly: Boolean            Read FReadOnly     Write FReadOnly     Default False;
    Property Checked: Boolean             Read FChecked      Write SetChecked;
    Property ReclickThenFree: Boolean     Read FRCTF         Write FRCTF         Default False;
    Property StateNormal;
    Property StateHover;
    Property StateClick;

    Property ButtonSize: Int8U            Read FButtonSize   Write SetButtonSize Default 16;
    Property WorkSpace: String            Read FWorkSpace    Write FWorkSpace;

    Property OnChange: mNotify            Read FOnChange     Write FOnChange;
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
