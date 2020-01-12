unit uEOFToken;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utoken, usource;

type

  { TEOFToken }

  TEOFToken = class(TToken)
    private
      procedure Extract(ASource: TSource);  virtual;
    public
      constructor Create(ASource: TSource);
  end;


implementation

{ TEOFToken }

procedure TEOFToken.Extract(ASource : TSource);
begin
  // do not consume any source tokens
end;

constructor TEOFToken.Create(ASource : TSource);
begin
  Inherited Create(ASource);
end;

end.

