unit uCodeFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCode;

type

  //A factory for creating objects that implement the intermediate code.

  { TCodeFactory }

  TICodeFactory = class
    class function CreateICode: IIntermediateCode;
    class function CreateICodeNode(Typ: ICodeNodeType): ICodeNode;
  end;

implementation

uses uCodeImpl;

{ TCodeFactory }

//Create and return an intermediate code implementation.
class function TICodeFactory.CreateICode: IIntermediateCode;
begin
  Result := TICodeImpl.Create;
end;

//Create and return a node implementation.
class function TICodeFactory.CreateICodeNode(Typ: ICodeNodeType): ICodeNode;
begin
  Result := TICodeNodeImpl.Create(Typ);
end;

end.

