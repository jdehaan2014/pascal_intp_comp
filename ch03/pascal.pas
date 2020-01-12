program pascal;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, ubaseparser, usource, ubasescanner, utoken, uEOFToken, umessages,
  ucode, usymtab, ubackend, uPascalParserTD, uPascalScanner, uFrontendFactory,
  uCodeGen, uExecutor, uBackendFactory, uPascal, uPascalErrorHandler,
  uPascalErrorCode, utokentype, uPascalTokentyp, uPascalToken, AnyObject,
  uObjectUtils;

const
  cFlags = '[-ix]';
  cUsage = 'Usage: Pascal execute|compile ' + cFlags + ' <source file path>';

var
  Operation: string;
  i : Integer;
  Flags : String;
  FilePath : String;
  MyPascal : TPascal;

begin
  WriteLn;
  try
    try
      Operation := ParamStr(1);
      if (UpperCase(Operation) <> 'COMPILE') and (UpperCase(Operation) <> 'EXECUTE') then
        raise Exception.Create('Missing command: Compile or Execute.');

      i := 2;
      Flags := '';

      // flags
      while (i < ParamCount) and (ParamStr(i)[1] = '-') do begin
        Flags += ParamStr(i)[1];
        Inc(i);
      end;

      // source path
      if i <= ParamCount then begin
        FilePath := ParamStr(i);
        MyPascal := TPascal.Create(Operation, FilePath, Flags);
      end
      else
        raise Exception.Create('Missing FilePath.');

    except
      WriteLn(cUsage);
    end;

  finally
    MyPascal.Free;
  end;
end.

