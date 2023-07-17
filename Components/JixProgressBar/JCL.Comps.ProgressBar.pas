unit JCL.Comps.ProgressBar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Utils,
  JCL.Controls,
  JCL.CompTypes,
  LResources, Forms, Controls, Graphics, Dialogs, BGRABitmap, BGRABitmapTypes,
  BGRAGradients, BGRAGradientScanner;

type
  eProgressOrientation = (
    jpoLeftToRight,
    jpoRightToLeft,
    jpoTopToBottom,
    jpoBottomToTop
  );

  TJixProgressBar = class(TJixControl)
  Private
    Var FMin: Int16S;
    Var FMax: Int16S;
    Var FProgress: Int16S;
    Var FOrientation: eProgressOrientation;

    Procedure SetMin(Value: Int16S);
    Procedure SetMax(Value: Int16S);
    Procedure SetValue(Value: Int16S);
    Procedure SetOrientation(Value: eProgressOrientation);

  Protected

  Public
    Constructor Create(Aowner: Tcomponent); Override;

    Procedure PaintOver(ACanvas: TCanvas; X, Y: Integer); Override;

  Published
    Property Min: Int16S      Read FMin         Write SetMin         Default 0;
    Property Max: Int16S      Read FMax         Write SetMax         Default 100;
    Property Progress: Int16S Read FProgress    Write SetValue;
    Property Orientation: eProgressOrientation
                              Read FOrientation Write SetOrientation Default jpoLeftToRight;

    Property Tooltip;
    Property ShowTooltip;

    Property StateNormal;
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
  End;

  Procedure Register;

Implementation
End.
