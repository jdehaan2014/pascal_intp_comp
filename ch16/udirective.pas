unit uDirective;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AnyObject;

type

  { TDirective }

  TDirective = class(TAnyObject)
    public
      type
        Values = (dirCLASS_PUBLIC,  dirEND_CLASS, dirSUPER, dirFIELD_PRIVATE_STATIC,
                  dirMETHOD_PUBLIC, dirMETHOD_STATIC, dirMETHOD_PUBLIC_STATIC,
                  dirMETHOD_PRIVATE_STATIC, dirEND_METHOD, dirLIMIT_LOCALS,
                  dirLIMIT_STACK, dirVAR, dirLINE);
    private
      FValue: Values;
      function getEnum: Values;
      procedure setEnum(AValue: Values);
    public
      property Value: Values read getEnum write setEnum;
      function toString: string; override;
      function toText: string;
      constructor Create(AValue: Values);
  end;

Operator := (V: TDirective.Values): TDirective;
operator := (V: TDirective): TDirective.Values;
operator = (V1: TDirective; V2: TDirective.Values): boolean;
operator = (V1: TDirective.Values; V2: TDirective): boolean;


implementation

{ TDirective }

function TDirective.getEnum: Values;
begin
  Result := FValue;
end;

procedure TDirective.setEnum(AValue: Values);
begin
  FValue := AValue;
end;

function TDirective.toString: string;
begin
  case FValue of
    dirCLASS_PUBLIC          : Result := '.class public';
    dirEND_CLASS             : Result := '.end class';
    dirSUPER                 : Result := '.super';
    dirFIELD_PRIVATE_STATIC  : Result := '.field private static';
    dirMETHOD_PUBLIC         : Result := '.method public';
    dirMETHOD_STATIC         : Result := '.method static';
    dirMETHOD_PUBLIC_STATIC  : Result := '.method public static';
    dirMETHOD_PRIVATE_STATIC : Result := '.method private static';
    dirEND_METHOD            : Result := '.end method';
    dirLIMIT_LOCALS          : Result := '.limit locals';
    dirLIMIT_STACK           : Result := '.limit stack';
    dirVAR                   : Result := '.var';
    dirLINE                  : Result := '.line';
  end;
end;

function TDirective.toText: string;
begin
  WriteStr(Result, FValue);
end;

constructor TDirective.Create(AValue: Values);
begin
  FValue := AValue;
end;

operator := (V: TDirective.Values): TDirective;
begin
  Result := TDirective.Create(V);
end;

operator := (V: TDirective): TDirective.Values;
begin
  Result := TDirective.Create(V);
end;

operator = (V1: TDirective; V2: TDirective.Values): boolean;
var
  Arg: TDirective.Values;
begin
  ReadStr(V1.toText, Arg);
  Result := Arg = V2;
end;

operator = (V1: TDirective.Values; V2: TDirective): boolean;
var
  Arg: TDirective.Values;
begin
  ReadStr(V2.toText, Arg);
  Result := Arg = V1;
end;



end.

