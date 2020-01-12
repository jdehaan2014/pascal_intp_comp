program pascal;

{$mode objfpc}{$H+}


uses
  Classes, SysUtils, uPascal;

const
  cFlags = '[-ix]';
  cUsage = 'Usage: Pascal execute|compile ' + cFlags + ' <source file path>';

var
  Operation: string;
  i : Integer;
  Flags : String;
  FilePath : String;
  MyPascal : TPascal;



function PlusPlus(var i: integer): integer;
begin
  Inc(i);
  Result := i;
end;

begin
  Writeln;
  try
    Operation := ParamStr(1);
    if (UpperCase(Operation) <> 'COMPILE') and
       (UpperCase(Operation) <> 'EXECUTE') then
      raise Exception.Create('Missing command: Compile or Execute.');

    i := 1;
    Flags := '';

    // flags
    while (PlusPlus(i) <= ParamCount) and (ParamStr(i)[1] = '-') do
      Flags += ParamStr(i)[2];

    // source path
    if i <= ParamCount then begin
      FilePath := ParamStr(i);
      MyPascal := TPascal.Create(Operation, FilePath, Flags);
    end
    else
      raise Exception.Create('Missing FilePath.');

    //writeln('Success!');
  except
    on E:Exception do begin
      Writeln(E.Message);
      WriteLn(cUsage);
    end;
  end;

end.

