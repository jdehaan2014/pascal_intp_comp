program pascal;

{$mode objfpc}{$H+}

{ This Pascal compiler is based on the compiler writing method as described in
  "Writing Compilers and Interpreters, A software engineering approach"
  by Ronald Mak (Third edition). Though the original is written in Java, I tried
  to stay as close as possible to the structure described.
  Since it heavily uses Interfaces this posed me for some challenges. Especially,
  in Java it's possible to have Enums implement Interfaces. This is not possible
  in Free Pascal. I solved this by defining Enums in classes, like:

    TPascalTokenTyp = class(TAnyObject, ITokenTyp)
    public
      type
        Values = (
          // Reserved words.
          ttAnd, ttArray, ttBegin, ttCase, ttConst, ttDiv, ttDo, ttDownto, ttElse,
          ttEnd, ttFile, ttFor, ttFunction, ttGoto, ttIf, ttIn, ttLabel, ttMod,
          ...

  and defining operators to allow assignments, comparisons, etc., e.g.

    Operator := (Value: TPascalTokenTyp.Values): ITokenTyp;

  I decided to use Corba style interfacing to avoid implementing COM style methods
  in quite a number of places. By letting all classes inherit from

    IAnyInterface = Interface
      ['IAnyInterface']
      function getObject: TObject;
      function toString: string;
    end;

    TAnyObject = class(TObject, IAnyInterface)
      function getObject: TObject;
    end;

  I could circumvent most challenges.

  Note: The PDF of the book can be found on internet. By reading the book
  and using it as a reference the Free Pascal code can be understood easily.
}


uses
  Classes, SysUtils, uPascal;

const
  cFlags = '[-ixlafcr]';
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

