unit uFrontendFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ubasescanner, uPascalScanner, ubaseparser, uPascalParserTD,
  usource;

type


  { TFrontEndFactory }

  //A factory class that creates parsers for specific source languages.
  TFrontEndFactory = class
    public
      class function CreateParser(Language, Typ : string; Source : TSource) : TBaseParser;
  end;

implementation

{ TFrontEndFactory }

class function TFrontEndFactory.CreateParser(Language, Typ : string;
  Source : TSource) : TBaseParser;
var
  Scanner: TBaseScanner;
begin
  if (UpperCase(Language) = 'PASCAL') and (UpperCase(Typ) = 'TOP-DOWN') then
  begin
    Scanner := TPascalScanner.Create(Source);
    Result := TPascalParserTD.Create(Scanner);
  end
  else if UpperCase(Language) <> 'PASCAL' then
    raise Exception.Create('Parser factory: Invalid language ' + '"' + Language + '"')
  else
    raise Exception.Create('Parser factory: Invalid type ' + '"' + Typ + '"')
end;

end.

