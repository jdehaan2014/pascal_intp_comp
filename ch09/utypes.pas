unit uTypes;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, AnyObject;

type


  //the interface for a type specification form
  ITypeForm = interface(IAnyInterface)
    ['ITypeForm']
  end;

  //the interface for a type specification attribute key
  ITypeKey = interface(IAnyInterface)
    ['ITypeKey']
  end;

  //the interface for a type specification
  ITypeSpec = interface(IAnyInterface)
    ['ITypeSpec']
    Function getForm: ITypeForm;
    procedure setIdentifier(Identifier: ISymTabEntry);
    function getIdentifier: ISymTabEntry;
    procedure setAttribute(Key: ITypeKey; Value: TObject);
    function getAttribute(Key: ITypeKey): TObject;
    function isPascalString: boolean;
    function baseType: ITypeSpec;
  end;


implementation
uses uSymTabEntry;

end.

