program pascal;

{$mode objfpc}{$H+}


uses
  Classes, SysUtils, uPascal;

const
  cFlags = '[-ix]';
  cUsage = 'Usage: Pascal execute|compile ' + cFlags +
           ' <source file path>' + '[<input file path>]';

var
  Operation: string;
  i : Integer;
  Flags : String;
  SourcePath : String = '';
  InputPath : String = '';
  MyPascal : TPascal;

function PlusPlus(var i: integer): integer;
begin
  Inc(i);
  Result := i;
end;

begin
  try
    try
      Operation := ParamStr(1);
      if (Operation.ToLower <> 'compile') and
         (Operation.ToLower <> 'execute') then
        raise Exception.Create('Missing command: Compile or Execute.');

      i := 1;
      Flags := '';

      // flags
      while (PlusPlus(i) <= ParamCount) and (ParamStr(i)[1] = '-') do
        Flags += ParamStr(i).Substring(1);

      // source path
      if i <= ParamCount then begin
        SourcePath := ParamStr(i);
      end
      else
        raise Exception.Create('Missing Source File Path.');

      //Runtime input data file path
      if PlusPlus(i) <= ParamCount then begin
        InputPath := ParamStr(i);
        if not FileExists(InputPath) then
          raise Exception.Create('Input file "'+InputPath+'" does not exist');
      end;

      MyPascal := TPascal.Create(Operation, SourcePath, InputPath, Flags);

    except
      on E:Exception do begin
        Writeln(E.Message);
        WriteLn(cUsage);
      end;
    end;

  finally
    if Assigned(MyPascal) then MyPascal.Free;
  end;
end.

