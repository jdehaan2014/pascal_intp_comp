unit utoken;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usource, uTokenType, AnyObject;

type

  { TToken }

  TToken = class
   protected
     Lexeme: string;
     Value : TAnyObject;
     Source : TSource;
     Typ : ITokenTyp;
     LineNum : integer;            // line number of the token's source line
     Position : integer;           // position of the first token char
     function CurrentChar: char;
     function NextChar: char;
     function PeekChar: char;
     procedure Extract;  virtual;
   public
     constructor Create(ASource: TSource);

     property getTyp: ITokenTyp read Typ;
     property getText: string read Lexeme;
     property getValue: TAnyObject read Value;
     property getLineNum: integer read LineNum;
     property getPosition: integer read Position;
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

procedure TToken.Extract;
begin
  Lexeme := CurrentChar;
  Value := Nil;
  NextChar; // consume current character
end;

end.

