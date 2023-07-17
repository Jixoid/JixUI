// SPDX-License-Identifier: LGPL-3.0-linking-exception
unit linuxlib;

{$mode objfpc}{$H+}

{ This unit allows to find the implementation of a library from its
  "linker name" whatever its version. Note that between different versions,
  there may be incompatibilities (in the signature of the functions or the
  record types). So make sure the functions you are calling are stable or
  check the version of the library once its loaded using one of its functions.


  Linker name
  -----------
  The linker name normally can only be used when compiling a program.
  It ends up with .so and does not have any version number. There isn't
  necessarily a file with this name though it may be provided in the
  development package (ending with -dev).
  - libwebp.so
  - libportaudio.so
  - libtiff.so
  - libpython.so


  Soname (qualified with a version number)
  ----------------------------------------
  The soname can be supplied to the LoadLibray function to load at runtime,
  without specifying any path. It is the same as the linker name, but with
  a version number. The file exists most of the time and it is generally
  a symbolic link to the implementation (or "real name").
  - libwebp.so.6
  - libportaudio.so.2
  - libtiff.so.5
  - libpython2.7.so


  Implementation or real name (with minor number)
  -----------------------------------------------
  The real name contains the implementation. It has a minor number and
  an optional release number. Most of the time, you don't need to know this
  name to use the library.
  - libwebp.so.6.0.2
  - libportaudio.so.2.0.0
  - libtiff.so.5.3.0
  - libpython2.7.so.1.0


  See: http://tldp.org/HOWTO/Program-Library-HOWTO/shared-libraries.html }

interface

uses
  BGRAClasses, SysUtils;

function FindLinuxLibrary(ALinkerName: string; AMinimumVersion: integer = 0): string;

Implementation
End.
