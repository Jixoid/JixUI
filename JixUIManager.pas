Unit JixUIManager;

{$mode objfpc}{$H+}

{$IfOpt D+}
  {$Note Added debugger to JixUI}
{$Endif}

{$if defined(Windows)}

  {$If defined(Cpu64)}
    {$Note JixUI compiling for x64}

  {$ElseIf defined(Cpu32)}
    {$Note JixUI compiling for x86, but this platform isn't recomended}

  {$Else}

    {$Fatal JixUI can only be compiled for x64 or x86}
  {$Endif}


{$else}
  {$Fatal JixUI can only be compiled for Windows}
{$endif}

Interface

Uses
  Classes, SysUtils, JCL.Utils;

Function GetFullVersion: StrA;
Function GetVersion: StrA;

Implementation
End.
