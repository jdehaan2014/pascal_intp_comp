unit uTextReader;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, {streamio,} streamex, uObjectUtils;

type

  { TTextReader }

  TTextReader = class
    private
      FInput: TStreamReader;
      FStandard: boolean;       // standard input
    public
      constructor Create(Source: TStream);
      constructor Create;
      destructor Destroy; override;
      function ReadLine: TString;
  end;

implementation

{ TTextReader }

constructor TTextReader.Create(Source: TStream);
begin
  FInput := TStreamReader.Create(Source);
  FInput.Reset;
  FStandard := False;
end;

constructor TTextReader.Create;
begin
  FStandard := True;
end;

destructor TTextReader.Destroy;
begin
  if assigned(FInput) then begin
    FInput.Close;
    FInput.Free;
  end;
  inherited;
end;

function TTextReader.ReadLine: TString;
var
  fromInput: string;
begin
  Result := Nil;
  if not FStandard then begin
    if not FInput.Eof then begin
      FInput.ReadLine(fromInput);
      Result := TString.Create(fromInput + LineEnding);
    end;
  end
  else begin
    Readln(fromInput);
    Result := TString.Create(fromInput + LineEnding);
  end;
end;


end.

