unit JCL.Comps.Memo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Controls,
  JCL.Comps.Template,
  JCL.CompTypes,
  JCL.Utils,
  LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, JCL.Auxiliary;

type
  TJixMemo = class(TJixWinControl)
  private
    Var FMemo: TMemo;

    Procedure SetLines(Value: TStrings);
    Function GetLines: TStrings;
    Procedure SetReadOnly(Value: Boolean);
    Function GetReadOnly: Boolean;
    Procedure SetOnChange(Value: mNotify);
    Function GetOnChange: mNotify;

  protected

  public
    Constructor Create(Aowner: Tcomponent); Override;
    Destructor Destroy; Override;

    Procedure PaintOver(ACanvas: TCanvas; X, Y: Integer); Override;

    Procedure Settemplate(Temp: Jtemplatecollectionitem); Override;

  published
    Property Tooltip;
    Property ShowTooltip;

    Property StateNormal;

    Property IgnoreTempStyle;

    Property Lines: TStrings     Read GetLines      Write SetLines;
    Property ReadOnly: Boolean   Read GetReadOnly   Write SetReadOnly  Default False;
    Property OnChange: mNotify   Read GetOnChange   Write SetOnChange;
    Property Align;
    Property BorderSpacing;
    Property Anchors;
    //Property AutoSize;
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
