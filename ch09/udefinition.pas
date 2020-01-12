unit uDefinition;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, AnyObject;

type
  IDefinition = interface(IAnyInterface)
    ['IDefinition']
    function getText: string;
  end;

implementation

end.

