unit darwinlib;

{$mode objfpc}{$H+}

{ This unit allows to find the latest implementation of a library.
  Note that between different versions, there may be incompatibilities
  (in the signature of the functions or the record types). So make sure
  the functions you are calling are stable or check the version of the
  library once its loaded using one of its functions.
}

interface

uses
  Classes, SysUtils;

function FindDarwinLibrary(AName: string; AMinimumVersion: integer = 0): string;

Implementation
End.
