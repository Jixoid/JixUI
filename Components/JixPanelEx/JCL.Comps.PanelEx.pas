unit JCL.Comps.PanelEx;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  JCL.Utils,
  JCL.Controls,
  JCL.Comps.Panel,
  LResources, Forms, Controls, Graphics, Dialogs, BGRABitmap, BGRABitmapTypes,
  BGRAGradients, BGRAGradientScanner;

type
  TJixPanelEx = class(TJixPanel)
  private

  protected

  public
    Constructor Create(Aowner: Tcomponent); Override;

  published
    Property StateNormal;
    Property StateHover;
    Property StateClick;
  end;

  Procedure Register;

Implementation
End.
