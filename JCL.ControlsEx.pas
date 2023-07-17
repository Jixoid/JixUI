Unit JCL.ControlsEx;

{$mode objfpc}{$H+}{$Inline On}

Interface

Uses
  Classes, SysUtils,
  JCL.Controls,
  JCL.Utils,
  JCL.Comps.ScrollBar,
  JCL.Comps.ScrollBarWin,
  Controls, BGRABitmapTypes;

Type

{%Region JixScrollingControl}
  TJixScrollingControl = class(TJixWinControl)
    private
      Var FJixHorzScrollBar: TJixScrollBarWin;
      Var FJixVertScrollBar: TJixScrollBarWin;

      Var FKatX: Int16U;
      Var FKatY: Int16U;

    protected
      Function DoMouseWheel(Shift: Tshiftstate; Wheeldelta: Integer;
        Mousepos: Tpoint): Boolean; Override;

      Procedure Resize; Override;
      Procedure SetParent(Newparent: Twincontrol); Override;
      Procedure FDoChange(Sender: TObject); Virtual;

    public
      Constructor Create(Aowner: TComponent); Override;
      Destructor Destroy; Override;

      Procedure Paint; Override;

    published
      Property JixHorzScrollBar: TJixScrollBarWin Read FJixHorzScrollBar Write FJixHorzScrollBar;
      Property JixVertScrollBar: TJixScrollBarWin Read FJixVertScrollBar Write FJixVertScrollBar;
  End;
{%EndRegion}

Procedure Register;

Implementation
End.
