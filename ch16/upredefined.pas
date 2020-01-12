unit uPredefined;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uSymTabImpl, uGenericInterfaces, uDefinitionImpl,
  uTypesFactory, uObjectUtils;

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
        readId       : ISymTabEntry;
        readlnId     : ISymTabEntry;
        writeId      : ISymTabEntry;
        writelnId    : ISymTabEntry;
        absId        : ISymTabEntry;
        arctanId     : ISymTabEntry;
        chrId        : ISymTabEntry;
        cosId        : ISymTabEntry;
        eofId        : ISymTabEntry;
        eolnId       : ISymTabEntry;
        expId        : ISymTabEntry;
        lnId         : ISymTabEntry;
        oddId        : ISymTabEntry;
        ordId        : ISymTabEntry;
        predId       : ISymTabEntry;
        roundId      : ISymTabEntry;
        sinId        : ISymTabEntry;
        sqrId        : ISymTabEntry;
        sqrtId       : ISymTabEntry;
        succId       : ISymTabEntry;
        truncId      : ISymTabEntry;

    private
      class procedure InitializeTypes(SymTabStack: ISymTabStack); static;
      class procedure InitializeConstants(SymTabStack: ISymTabStack); static;
      class procedure InitializeStandardRoutines(SymTabStack: ISymTabStack); static;
      class function EnterStandard(SymTabStack: ISymTabStack; defn: IDefinition;
        Name: string; RoutineCode: TRoutineCodeImpl.Values): ISymTabEntry; static;
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
var
  Constants: TSymTabEntries;
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
  Constants := TSymTabEntries.Create;
  constants.add(falseId);
  constants.add(trueId);
  booleanType.setAttribute(tkEnumerationConstants, Constants);

end;

class procedure TPredefined.InitializeStandardRoutines(SymTabStack: ISymTabStack
  );
begin
  readId := EnterStandard(symTabStack, defProcedure, 'read', rcREAD);
  readLnId := EnterStandard(symTabStack, defProcedure, 'readln', rcREADLN);
  writeId := EnterStandard(symTabStack, defProcedure, 'write', rcWRITE);
  writelnId := EnterStandard(symTabStack, defProcedure, 'writeln', rcWRITELN);

  absId := EnterStandard(symTabStack, defFunction, 'abs', rcABS);
  arctanId := EnterStandard(symTabStack, defFunction, 'arctan', rcARCTAN);
  chrId := EnterStandard(symTabStack, defFunction, 'chr', rcCHR);
  cosId := EnterStandard(symTabStack, defFunction, 'cos', rcCOS);
  eofId := EnterStandard(symTabStack, defFunction, 'eof', rcEOF);
  eolnId := EnterStandard(symTabStack, defFunction, 'eoln', rcEOLN);
  expId := EnterStandard(symTabStack, defFunction, 'exp', rcEXP);
  lnId := EnterStandard(symTabStack, defFunction, 'ln', rcLN);
  oddId := EnterStandard(symTabStack, defFunction, 'odd', rcODD);
  ordId := EnterStandard(symTabStack, defFunction, 'ord', rcORD);
  predId := EnterStandard(symTabStack, defFunction, 'pred', rcPRED);
  roundId := EnterStandard(symTabStack, defFunction, 'round', rcROUND);
  sinId := EnterStandard(symTabStack, defFunction, 'sin', rcSIN);
  sqrId := EnterStandard(symTabStack, defFunction, 'sqr', rcSQR);
  sqrtId := EnterStandard(symTabStack, defFunction, 'sqrt', rcSQRT);
  succId := EnterStandard(symTabStack, defFunction, 'succ', rcSUCC);
  truncId := EnterStandard(symTabStack, defFunction, 'trunc', rcTRUNC);
end;

//Enter a standard procedure or function into the symboltable stack
class function TPredefined.EnterStandard(SymTabStack: ISymTabStack;
  defn: IDefinition; Name: string; RoutineCode: TRoutineCodeImpl.Values
  ): ISymTabEntry;
var
  ProcID: ISymTabEntry;
begin
  ProcID := SymTabStack.EnterLocal(Name);
  ProcID.setDefinition(defn);
  ProcID.setAttribute(skRoutineCode, TRoutineCodeImpl.Create(RoutineCode));
  Result := ProcID;
end;

class procedure TPredefined.Initialize(SymTabStack: ISymTabStack);
begin
  InitializeTypes(SymTabStack);
  InitializeConstants(SymTabStack);
  InitializeStandardRoutines(SymTabStack);
end;

end.

