unit JCL.Comps.Edit;

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
  TJixEdit = class(TJixWinControl)
  private
    Var FEdit: TEdit;

    Procedure SetText(Value: String);
    Function GetText: String;
    Procedure SetTextHint(Value: String);
    Function GetTextHint: String;
    Procedure SetNumberOnly(Value: Boolean);
    Function GetNumberOnly: Boolean;
    Procedure SetOnChange(Value: mNotify);
    Function GetOnChange: mNotify;
    Procedure SetReadOnly(Value: Boolean);
    Function GetReadOnly: Boolean;
    Procedure SetMaxLength(Value: Integer);
    Function GetMaxLength: Integer;

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

    Property Text: String        Read GetText       Write SetText;
    Property TextHint: String    Read GetTextHint   Write SetTextHint;
    Property NumberOnly: Boolean Read GetNumberOnly Write SetNumberOnly Default False;
    Property OnChange: mNotify   Read GetOnChange   Write SetOnChange;
    Property ReadOnly: Boolean   Read GetReadOnly   Write SetReadOnly   Default False;
    Property MaxLength: Integer  Read GetMaxLength  Write SetMaxLength;
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
