// SPDX-License-Identifier: LGPL-3.0-linking-exception
{******************************* CONTRIBUTOR(S) ******************************
- Edivando S. Santos Brasil | mailedivando@gmail.com
  (Compatibility with delphi VCL 11/2018)

***************************** END CONTRIBUTOR(S) *****************************}
unit BCThemeManager;

{$I bgracontrols.map.inc}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics,
  {$IFDEF FPC}LResources, {$ELSE}Types, BGRAGraphics, GraphType, FPImage,{$ENDIF} Dialogs;

type
  TBCThemeManager = class(TComponent)
  private

  protected

  public
    procedure Apply(AControl: TWinControl); overload; virtual; abstract;
    procedure Apply(); overload; virtual; abstract;
  published

  end;

Implementation
End.
