unit AnyObject;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils;

type
  IAnyInterface = Interface
    ['IBaseInterface']
    function getObject: TObject;
    function toString: string;
  end;

  { TAnyObject }

  TAnyObject = class(TObject, IAnyInterface)
    function getObject: TObject;
    function toString: string; virtual; abstract;
  end;

implementation

{ TAnyObject }

function TAnyObject.getObject: TObject;
begin
  Result := Self;
end;

end.

