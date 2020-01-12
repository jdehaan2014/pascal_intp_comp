unit uPredefined;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uSymTabImpl, uGenericInterfaces, uDefinitionImpl,
  uTypesFactory, fgl, uObjectUtils;

type

  { TPredefined }

  TPredefined = class
    public
      class var
        //predefined types
        integerType  : ITypeSpec;
        realType     : ITypeSpec;
        booleanType  : ITypeSpec;
        charType     : ITypeSpec;
        undefinedType: ITypeSpec;
        //predefined identifiers
        integerID    : ISymTabEntry;
        realID       : ISymTabEntry;
        booleanID    : ISymTabEntry;
        charID       : ISymTabEntry;
        falseID      : ISymTabEntry;
        trueID       : ISymTabEntry;
    private
      class procedure InitializeTypes(SymTabStack: ISymTabStack); static;
      class procedure InitializeConstants(SymTabStack: ISymTabStack); static;
    public
      class procedure Initialize(SymTabStack: ISymTabStack); static;
  end;

implementation
uses uTypesImpl;

{ TPredefined }

class procedure TPredefined.InitializeTypes(SymTabStack: ISymTabStack);
begin
  //Type integer
  integerID  := SymTabStack.enterLocal('integer');
  integerType := TTypeFactory.createType(tfScalar);
  integerType.setIdentifier(integerId);
  integerId.setDefinition(defType);
  integerId.setTypeSpec(integerType);

  // Type real.
  realId := symTabStack.enterLocal('real');
  realType := TTypeFactory.createType(tfScalar);
  realType.setIdentifier(realId);
  realId.setDefinition(defType);
  realId.setTypeSpec(realType);

  // Type boolean.
  booleanId := symTabStack.enterLocal('boolean');
  booleanType := TTypeFactory.createType(tfEnumeration);
  booleanType.setIdentifier(booleanId);
  booleanId.setDefinition(defType);
  booleanId.setTypeSpec(booleanType);

  // Type char.
  charId := symTabStack.enterLocal('char');
  charType := TTypeFactory.createType(tfScalar);
  charType.setIdentifier(charId);
  charId.setDefinition(defType);
  charId.setTypeSpec(charType);

  // Undefined type.
  undefinedType := TTypeFactory.createType(tfScalar);
end;

class procedure TPredefined.InitializeConstants(SymTabStack: ISymTabStack);
type
  TConstants = specialize TFPGList<ISymTabEntry>;
var
  Constants: TConstants;
begin
  // Boolean enumeration constant false.
  falseId := symTabStack.enterLocal('false');
  falseId.setDefinition(defEnumeration_Constant);
  falseId.setTypeSpec(booleanType);
  falseId.setAttribute(skConstantValue, TInteger.Create(0));

  // Boolean enumeration constant true.
  trueId := symTabStack.enterLocal('true');
  trueId.setDefinition(defEnumeration_Constant);
  trueId.setTypeSpec(booleanType);
  trueId.setAttribute(skConstantValue, TInteger.Create(1));

  // Add false and true to the boolean enumeration type.
  Constants := TConstants.Create;
  constants.add(falseId);
  constants.add(trueId);
  booleanType.setAttribute(tkEnumerationConstants, Constants);

end;

class procedure TPredefined.Initialize(SymTabStack: ISymTabStack);
begin
  initializeTypes(symTabStack);
  initializeConstants(symTabStack);
end;

end.

