unit uRuntimeLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type

  EPascalRuntimeException = class(Exception);

  { TRangeChecker }
  TRangeChecker = class
    //Perform a runtime range check
    public
      class procedure Check(Value, minValue, maxValue: integer);
  end;

  TPascalTextIn = class
    //Runtime text input for Pascal programs.
  end;

  { TRuntimer }

  TRuntimer = class
    //compute and print the elapsed run time of a compiled Pascal program.
    public
      StartTime: TDateTime;
      constructor Create;
      procedure PrintElapsedTime;
  end;

implementation

{ TRangeChecker }

class procedure TRangeChecker.Check(Value, minValue, maxValue: integer);
begin
  if (Value < minValue) or (Value > maxValue) then
    EPascalRuntimeException.CreateFmt(
      'Range error: %1d not in [%1d, %1d]', [Value, minValue, maxValue]
    );
end;

{ TRuntimer }

constructor TRuntimer.Create;
begin
  StartTime := TimeOf(Now);
end;

procedure TRuntimer.PrintElapsedTime;
var
  ElapsedTime: Double;
begin
  ElapsedTime := SecondSpan(StartTime, TimeOf(Now));
  WriteLn(Format(LineEnding + '%20.4f seconds total execution time.', [ElapsedTime]));
end;


end.

