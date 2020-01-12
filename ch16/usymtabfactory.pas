unit uSymtabFactory;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uGenericInterfaces;

type

  //factory for creating objects that implement the symbol table

  { TSymtabFactory }

  TSymtabFactory = class
    public
      class function CreateSymTabStack: ISymTabStack; static;
      class function CreateSymTab(ANestingLevel: integer): ISymTab; static;
      class function CreateSymTabEntry(
        AName: String; ASymTab: ISymTab): ISymTabEntry; static;
  end;


implementation

uses uSymTabImpl;

{ TSymtabFactory }

//Create and return a symbol table stack implementation.
class function TSymtabFactory.CreateSymTabStack: ISymTabStack;
begin
  Result := TSymTabStackImpl.Create;
end;

//Create and return a symbol table implementation.
class function TSymtabFactory.CreateSymTab(ANestingLevel: integer): ISymTab;
begin
  Result := TSymTabImpl.Create(ANestingLevel);
end;

//Create and return a symbol table entry implementation.
class function TSymtabFactory.CreateSymTabEntry(
  AName: String; ASymTab: ISymTab): ISymTabEntry;
begin
  Result := TSymTabEntryImpl.Create(AName, ASymTab);
end;

end.

