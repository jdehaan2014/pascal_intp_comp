unit uTypesImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uGenericInterfaces, AnyObject, fgl, uObjectUtils,
  uPredefined;

type

  { TTypeFormImpl }

  TTypeFormImpl = class(TAnyObject, ITypeForm)
    public
      type
        Values = (tfScalar, tfEnumeration, tfSubRange, tfArray, tfRecord);
    private
      FValue: Values;
      function getEnum: Values;
      procedure setEnum(AValue: Values);
    public
      property Value: Values read getEnum write setEnum;
      function toString: string; override;
      class function toTyp(from: ITypeForm): Values; static;
      constructor Create(AValue: Values);
  end;

Operator := (T1: TTypeFormImpl.Values): TTypeFormImpl;
Operator := (T1: TTypeFormImpl.Values): ITypeForm;
operator := (T1: ITypeForm): TTypeFormImpl.Values;
operator = (T1: ITypeForm; T2: TTypeFormImpl.Values): boolean;
operator = (T1: TTypeFormImpl.Values; T2: ITypeForm): boolean;

type

  { TTypeKeyImpl }

  TTypeKeyImpl = class(TAnyObject, ITypeKey)
    public
      type
        Values = (//Enumeration
                  tkEnumerationConstants,
                  //SubRange
                  tkSubRangeBaseType, tkSubRangeMinValue, tkSubRangeMaxValue,
                  //Array
                  tkArrayIndexType, tkArrayElementType, tkArrayElementCount,
                  //Record
                  tkRecordSymTab);
    private
      FValue: Values;
      function getEnum: Values;
      procedure setEnum(AValue: Values);
    public
      property Value: Values read getEnum write setEnum;
      function toString: string; override;
      class function toTyp(from: ITypeKey): Values; static;
      constructor Create(AValue: Values);
  end;

Operator := (T1: TTypeKeyImpl.Values): TTypeKeyImpl;
Operator := (T1: TTypeKeyImpl.Values): ITypeKey;
operator := (T1: ITypeKey): TTypeKeyImpl.Values;
operator = (T1: ITypeKey; T2: TTypeKeyImpl.Values): boolean;
operator = (T1: TTypeKeyImpl.Values; T2: ITypeKey): boolean;


type

  TTypeSpecMap = Specialize TFPGMap<TTypeKeyImpl.Values, TObject>;

  //An implementation of a Pascal type specification

  { TTypeSpecImpl }

  TTypeSpecImpl = class(TTypeSpecMap, ITypeSpec)
    private
      Form: ITypeForm;
      Identifier: ISymTabEntry;
    public
      constructor Create(AForm: ITypeForm);
      constructor Create(AValue: string);
      function getObject: TObject;
      Function getForm: ITypeForm;
      procedure setIdentifier(AIdentifier: ISymTabEntry);
      function getIdentifier: ISymTabEntry;
      procedure setAttribute(Key: ITypeKey; Value: TObject);
      function getAttribute(Key: ITypeKey): TObject;
      function isPascalString: boolean;
      function baseType: ITypeSpec;
      function asITypeSpec: ITypeSpec;
  end;

implementation

{ TTypeFormImpl }

function TTypeFormImpl.getEnum: Values;
begin
  Result := FValue;
end;

procedure TTypeFormImpl.setEnum(AValue: Values);
begin
  FValue := AValue;
end;

function TTypeFormImpl.toString: string;
begin
  WriteStr(Result, FValue);
end;

class function TTypeFormImpl.toTyp(from: ITypeForm): Values;
begin
  ReadStr(from.toString, Result)
end;

constructor TTypeFormImpl.Create(AValue: Values);
begin
  FValue := AValue;
end;

Operator := (T1: TTypeFormImpl.Values): TTypeFormImpl;
begin
  Result := TTypeFormImpl.Create(T1);
end;

Operator := (T1: TTypeFormImpl.Values): ITypeForm;
begin
  Result := TTypeFormImpl.Create(T1);
end;

operator := (T1: ITypeForm): TTypeFormImpl.Values;
var
  Arg: TTypeFormImpl.Values;
begin
  ReadStr(T1.toString, Arg);
  Result := TTypeFormImpl.Create(Arg);
end;

operator = (T1: ITypeForm; T2: TTypeFormImpl.Values): boolean;
var
  Arg: TTypeFormImpl.Values;
begin
  ReadStr(T1.toString, Arg);
  Result := Arg = T2;
end;

operator = (T1: TTypeFormImpl.Values; T2: ITypeForm): boolean;
var
  Arg: TTypeFormImpl.Values;
begin
  ReadStr(T2.toString, Arg);
  Result := Arg = T1;
end;

{ TTypeKeyImpl }

function TTypeKeyImpl.getEnum: Values;
begin
  Result := FValue;
end;

procedure TTypeKeyImpl.setEnum(AValue: Values);
begin
  FValue := AValue;
end;

function TTypeKeyImpl.toString: string;
begin
  WriteStr(Result, FValue);
end;

class function TTypeKeyImpl.toTyp(from: ITypeKey): Values;
begin
  ReadStr(from.toString, Result)
end;

constructor TTypeKeyImpl.Create(AValue: Values);
begin
  FValue := AValue;
end;

Operator := (T1: TTypeKeyImpl.Values): TTypeKeyImpl;
begin
  Result := TTypeKeyImpl.Create(T1);
end;

Operator := (T1: TTypeKeyImpl.Values): ITypeKey;
begin
  Result := TTypeKeyImpl.Create(T1);
end;

operator := (T1: ITypeKey): TTypeKeyImpl.Values;
var
  Arg: TTypeKeyImpl.Values;
begin
  ReadStr(T1.toString, Arg);
  Result := TTypeKeyImpl.Create(Arg);
end;

operator = (T1: ITypeKey; T2: TTypeKeyImpl.Values): boolean;
var
  Arg: TTypeKeyImpl.Values;
begin
  ReadStr(T1.toString, Arg);
  Result := Arg = T2;
end;

operator = (T1: TTypeKeyImpl.Values; T2: ITypeKey): boolean;
var
  Arg: TTypeKeyImpl.Values;
begin
  ReadStr(T2.toString, Arg);
  Result := Arg = T1;
end;

{ TTypeSpecImpl }

constructor TTypeSpecImpl.Create(AForm: ITypeForm);
begin
  inherited Create;
  Form := AForm;
  Identifier := Nil;
end;

constructor TTypeSpecImpl.Create(AValue: string);
var
  indexType: ITypeSpec;
begin
  inherited Create;
  Form := tfArray;
  indexType := TTypeSpecImpl.Create(tfSubRange);
  indexType.setAttribute(tkSubRangeBaseType, TPredefined.integerType.getObject);
  indexType.setAttribute(tkSubRangeMinValue, TInteger.Create(1));
  indexType.setAttribute(tkSubRangeMaxValue, TInteger.Create(Length(AValue)));
  setAttribute(tkArrayIndexType, TTypeSpecImpl(indexType.getObject));
  setAttribute(tkArrayElementType, TPredefined.charType.getObject);
  setAttribute(tkArrayElementCount, TInteger.Create(Length(AValue)));
end;

function TTypeSpecImpl.getObject: TObject;
begin
  Result := Self;
end;

function TTypeSpecImpl.getForm: ITypeForm;
begin
  Result := Form;
end;

procedure TTypeSpecImpl.setIdentifier(AIdentifier: ISymTabEntry);
begin
  Identifier := AIdentifier;
end;

function TTypeSpecImpl.getIdentifier: ISymTabEntry;
begin
  Result := Identifier;
end;

procedure TTypeSpecImpl.setAttribute(Key: ITypeKey; Value: TObject);
begin
  KeyData[TTypeKeyImpl.toTyp(Key)] := Value;
end;

function TTypeSpecImpl.getAttribute(Key: ITypeKey): TObject;
var
  index: longint;
begin
  index := indexOf(TTypeKeyImpl.toTyp(Key));
  if index < 0 then
    Result := Nil
  else
    Result := KeyData[TTypeKeyImpl.toTyp(Key)];
end;

function TTypeSpecImpl.isPascalString: boolean;
var
  elmtType, indexType: ITypeSpec;
begin
  if Form = tfArray then begin
    elmtType := TTypeSpecImpl(getAttribute(tkArrayElementType));
    indexType := TTypeSpecImpl(getAttribute(tkArrayIndexType));
    Result := (elmtType.baseType = TPredefined.charType) and
              (indexType.baseType = TPredefined.integerType);
  end
  else Result := False;
end;

function TTypeSpecImpl.baseType: ITypeSpec;
begin
  if Form = tfSubRange then
    Result := TTypeSpecImpl(getAttribute(tkSubRangeBaseType))
  else
    Result := Self;
end;

function TTypeSpecImpl.asITypeSpec: ITypeSpec;
begin
  Result := Self;
end;



end.

