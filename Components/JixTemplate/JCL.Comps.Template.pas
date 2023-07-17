unit JCL.Comps.Template;

{$mode ObjFPC}{$H+}

interface

Uses
  JCL.Utils,
  JCL.CompTypes,
  Classes, SysUtils, LResources, Controls, Graphics, Dialogs, JCL.Auxiliary;

type
  {$Include Collection.map.Inc}
  TJixTemplate = class(TComponent)
  private
    FTemplates: jTemplateCollection;

  protected

  public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

  published
    Property Templates: jTemplateCollection Read FTemplates Write FTemplates;

  end;

  Procedure Register;

Implementation
End.
