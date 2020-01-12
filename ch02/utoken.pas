unit utoken;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usource;

type

  ITokenTyp = interface
  end;


  { TToken }

  TToken = class
   protected
     Lexeme: string;
     Value : TObject;
     Source : TSource;
     ITyp : ITokenTyp;
     LineNum : integer;            // line number of the token's source line
     Position : integer;           // position of the first token char
     function CurrentChar: char;
     function NextChar: char;
     function PeekChar: char;
     procedure Extract; virtual;
   public
     constructor Create(ASource: TSource);

     function getTyp: ITokenTyp;
     function getText: string;
     function getValue: TObject;
     function getLineNum: integer;
     function getPosition: integer;
  end;

implementation

{ TToken }

function TToken.CurrentChar : char;
begin
  Result := Source.CurrentChar;
end;

function TToken.NextChar : char;
begin
  Result := Source.NextChar;
end;

function TToken.PeekChar : char;
begin
  Result := Source.PeekChar;
end;

constructor TToken.Create(ASource : TSource);
begin
  Source := ASource;
  LineNum := Source.LineNum;
  Position := Source.CurrentPos;
  Extract;
end;

function TToken.getTyp : ITokenTyp;
begin
  result := ITyp;
end;

function TToken.getText : string;
begin
  result := Lexeme;
end;

function TToken.getValue : TObject;
begin
  result := Value;
end;

function TToken.getLineNum : integer;
begin
  result := LineNum;
end;

function TToken.getPosition : integer;
begin
  result := Position;
end;

procedure TToken.Extract;
begin
  Lexeme := CurrentChar;
  Value := Nil;
  NextChar; // consume current character
end;

end.

