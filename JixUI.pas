{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

Unit Jixui;

{$warn 5023 off : no warning about unused units}
Interface

uses
  JForms, JixPas, JGL, JixAPI, Jedab, JDialogs, JixButton, PortableForm, 
  PortableJixForm, JixComponent, JixTemplate, LazarusPackageIntf;

Implementation

Procedure Register;
Begin
  Registerunit('JixButton', @Jixbutton.Register);
  Registerunit('PortableForm', @Portableform.Register);
  Registerunit('PortableJixForm', @Portablejixform.Register);
  Registerunit('JixTemplate', @Jixtemplate.Register);
End;

Initialization
  Registerpackage('JixUI', @Register);
End.
