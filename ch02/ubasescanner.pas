unit ubasescanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usource, utoken;

type

  { TBaseScanner }

  TBaseScanner = class
    protected
      Source: TSource;
      function ExtractToken: TToken; virtual; abstract;
    private
      FCurrentToken: TToken;
    public
      function CurrentToken: TToken;
      function NextToken: TToken;
      function CurrentChar: char;
      function NextChar: char;
      constructor Create(ASource: TSource);
  end;

implementation

{ TBaseScanner }

function TBaseScanner.CurrentChar : char;
begin
  result := Source.CurrentChar;
end;

function TBaseScanner.CurrentToken : TToken;
begin
  result := FCurrentToken;
end;

function TBaseScanner.NextToken : TToken;
begin
  FCurrentToken := ExtractToken;
  Result := FCurrentToken;
end;

function TBaseScanner.NextChar : char;
begin
  result := Source.NextChar;
end;

constructor TBaseScanner.Create(ASource : TSource);
begin
  Source := ASource;
end;

end.

