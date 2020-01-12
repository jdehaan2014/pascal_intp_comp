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
        Values = (tfNone, tfScalar, tfEnumeration, tfSubRange, tfArray, tfRecord);
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
      FForm: ITypeForm;
      Identifier: ISymTabEntry;
    public
      constructor Create(AForm: ITypeForm);
      constructor Create(AValue: TString);
      function getObject: TObject;
      function getForm: ITypeForm;
      procedure setIdentifier(AIdentifier: ISymTabEntry);
      function getIdentifier: ISymTabEntry;
      procedure setAttribute(Key: ITypeKey; Value: TObject);
      function getAttribute(Key: ITypeKey): TObject;
      function isPascalString: boolean;
      function baseType: ITypeSpec;
      function toString: string; override;
  end;

  //Perform type checking

  { TTypeChecker }

  TTypeChecker = class
    public
      class function isInteger(Typ: ITypeSpec): boolean; static;
      class function areBothInteger(Typ1, Typ2: ITypeSpec): boolean; static;
      class function isReal(Typ: ITypeSpec): boolean; static;
      class function isIntegerOrReal(Typ: ITypeSpec): boolean; static;
      class function isAtLeastOneReal(Typ1, Typ2: ITypeSpec): boolean; static;
      class function isBoolean(Typ: ITypeSpec): boolean; static;
      class function areBothBoolean(Typ1, Typ2: ITypeSpec): boolean; static;
      class function isChar(Typ: ITypeSpec): boolean; static;
      class function areAssignmentCompatible(TargetTyp, ValueTyp: ITypeSpec): boolean; static;
      class function areComparisonCompatible(Typ1, Typ2: ITypeSpec): boolean; static;
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
  if from <> Nil then
    ReadStr(from.toString, Result)
  else
    Result := tfNone;
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
  FForm := AForm;
  Identifier := Nil;
end;

constructor TTypeSpecImpl.Create(AValue: TString);
var
  indexType: ITypeSpec;
begin
  inherited Create;
  FForm := tfArray;
  indexType := TTypeSpecImpl.Create(tfSubRange);
  indexType.setAttribute(tkSubRangeBaseType, TPredefined.integerType.getObject);
  indexType.setAttribute(tkSubRangeMinValue, TInteger.Create(1));
  indexType.setAttribute(tkSubRangeMaxValue, TInteger.Create(AValue.Length));
  setAttribute(tkArrayIndexType, TTypeSpecImpl(indexType.getObject));
  setAttribute(tkArrayElementType, TPredefined.charType.getObject);
  setAttribute(tkArrayElementCount, TInteger.Create(AValue.Length));
end;

function TTypeSpecImpl.getObject: TObject;
begin
  Result := Self;
end;

function TTypeSpecImpl.getForm: ITypeForm;
begin
  Result := FForm;
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
  if FForm = tfArray then begin
    elmtType := TTypeSpecImpl(getAttribute(tkArrayElementType));
    indexType := TTypeSpecImpl(getAttribute(tkArrayIndexType));
    Result := (elmtType.baseType = TPredefined.charType) and
              (indexType.baseType = TPredefined.integerType);
  end
  else Result := False;
end;

function TTypeSpecImpl.baseType: ITypeSpec;
begin
  if FForm = tfSubRange then
    Result := TTypeSpecImpl(getAttribute(tkSubRangeBaseType))
  else
    Result := Self;
end;

function TTypeSpecImpl.toString: string;
begin
  Result := Identifier.getName;
end;


{ TTypeChecker }

//Check if a type specification is integer.
class function TTypeChecker.isInteger(Typ: ITypeSpec): boolean;
begin
  Result := (Typ <> Nil) and (Typ.baseType = TPredefined.integerType);
end;

//Check if both type specifications are integer.
class function TTypeChecker.areBothInteger(Typ1, Typ2: ITypeSpec): boolean;
begin
  Result := isInteger(Typ1) and isInteger(Typ2);
end;

//Check if a type specification is real.
class function TTypeChecker.isReal(Typ: ITypeSpec): boolean;
begin
  Result := (Typ <> Nil) and (Typ.baseType = TPredefined.realType);
end;

//Check if a type specification is integer or real.
class function TTypeChecker.isIntegerOrReal(Typ: ITypeSpec): boolean;
begin
  Result := isInteger(Typ) or isReal(Typ);
end;

//Check if at least one of two type specifications is real.
class function TTypeChecker.isAtLeastOneReal(Typ1, Typ2: ITypeSpec): boolean;
begin
  Result := (isReal(Typ1) and isReal(Typ2)) or
            (isReal(Typ1) and isInteger(Typ2)) or
            (isInteger(Typ1) and isReal(Typ2));
end;

//Check if a type specification is boolean.
class function TTypeChecker.isBoolean(Typ: ITypeSpec): boolean;
begin
  Result := (Typ <> Nil) and (Typ.baseType = TPredefined.booleanType);
end;

//Check if both type specifications are boolean.
class function TTypeChecker.areBothBoolean(Typ1, Typ2: ITypeSpec): boolean;
begin
  Result := isBoolean(Typ1) and isBoolean(Typ2);
end;

//Check if a type specification is char.
class function TTypeChecker.isChar(Typ: ITypeSpec): boolean;
begin
  Result := (Typ <> Nil) and (Typ.baseType = TPredefined.charType);
end;

//Check if 2 type specs are assigment compatible.
class function TTypeChecker.areAssignmentCompatible(TargetTyp,
  ValueTyp: ITypeSpec): boolean;
var
  Compatible: boolean = False;
begin
  if (TargetTyp = Nil) and (ValueTyp = Nil) then
    Exit(False);
  //writeln('TTypeChecker.areAssignmentCompatible T1, T2 =',
    //TargetTyp.getIdentifier.getName, ', ', ValueTyp.getIdentifier.getName);
  TargetTyp := TargetTyp.baseType;
  ValueTyp := ValueTyp.baseType;
  if TargetTyp = ValueTyp then    //Identical types
    Compatible := True
  else if isReal(TargetTyp) and isInteger(ValueTyp) then // real := integer
    Compatible := True
  else                                      //string := string
    Compatible := TargetTyp.isPascalString and ValueTyp.isPascalString;
  Result := Compatible;
end;

//Check if 2 type specs are comparison compatible.
class function TTypeChecker.areComparisonCompatible(Typ1, Typ2: ITypeSpec
  ): boolean;
var
  Compatible: boolean = False;
  Form: ITypeForm;
begin
  if (Typ1 = Nil) and (Typ2 = Nil) then
    Exit(False);
  Typ1 := Typ1.baseType;
  Typ2 := Typ2.baseType;
  Form := Typ1.getForm;
  //2 identical scalar or enumeration types
  if (Typ1 = Typ2) and ((Form = tfScalar) or (Form = tfEnumeration)) then
    Compatible := True
  else if isAtLeastOneReal(Typ1, Typ2) then   // one integer and one real
    Compatible := True
  else   // two strings
    Compatible := Typ1.isPascalString and Typ2.isPascalString;
  Result := Compatible;
end;




end.

