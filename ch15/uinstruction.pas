unit uInstruction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AnyObject;

type

  { TInstruction }

  TInstruction = class(TAnyObject)
    public
      type
        Values = (
          // Load constant
          _ICONST_0, _ICONST_1, _ICONST_2, _ICONST_3, _ICONST_4, _ICONST_5,
          _ICONST_M1, _FCONST_0, _FCONST_1, _FCONST_2, _ACONST_NULL,
          _BIPUSH, _SIPUSH, _LDC,

          // Load value or address
          _ILOAD_0, _ILOAD_1, _ILOAD_2, _ILOAD_3,
          _FLOAD_0, _FLOAD_1, _FLOAD_2, _FLOAD_3,
          _ALOAD_0, _ALOAD_1, _ALOAD_2, _ALOAD_3,
          _ILOAD, _FLOAD, _ALOAD,
          _GETSTATIC, _GETFIELD,

          // Store value or address
          _ISTORE_0, _ISTORE_1, _ISTORE_2, _ISTORE_3,
          _FSTORE_0, _FSTORE_1, _FSTORE_2, _FSTORE_3,
          _ASTORE_0, _ASTORE_1, _ASTORE_2, _ASTORE_3,
          _ISTORE, _FSTORE, _ASTORE,
          _PUTSTATIC, _PUTFIELD,

          // Operand stack
          _POP, _SWAP, _DUP,

          // Arithmetic and logical
          _IADD, _FADD, _ISUB, _FSUB, _IMUL, _FMUL,
          _IDIV, _FDIV, _IREM, _FREM, _INEG, _FNEG,
          _IINC, _IAND, _IOR, _IXOR,

          // Type conversion and checking
          _I2F, _I2C, _I2D, _F2I, _F2D, _D2F,
          _CHECKCAST,

          // Objects and arrays
          _NEW, _NEWARRAY, _ANEWARRAY, _MULTIANEWARRAY,
          _IALOAD, _FALOAD, _BALOAD, _CALOAD, _AALOAD,
          _IASTORE, _FASTORE, _BASTORE, _CASTORE, _AASTORE,

          // Compare and branch
          _IFEQ, _IFNE, _IFLT, _IFLE, _IFGT, _IFGE,
          _IF_ICMPEQ, _IF_ICMPNE, _IF_ICMPLT, _IF_ICMPLE, _IF_ICMPGT, _IF_ICMPGE,
          _FCMPG, _GOTO, _LOOKUPSWITCH,

          // Call and return
          _INVOKESTATIC, _INVOKEVIRTUAL, _INVOKENONVIRTUAL,
          _RETURN, _IRETURN, _FRETURN, _ARETURN,

          // No operation
          _NOP
        );
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

Operator := (V: TInstruction.Values): TInstruction;
operator := (V: TInstruction): TInstruction.Values;
operator = (V1: TInstruction; V2: TInstruction.Values): boolean;
operator = (V1: TInstruction.Values; V2: TInstruction): boolean;

implementation

{ TInstruction }

function TInstruction.getEnum: Values;
begin
  Result := FValue;
end;

procedure TInstruction.setEnum(AValue: Values);
begin
  FValue := AValue;
end;

function TInstruction.toString: string;
begin
  WriteStr(Result, FValue);
  Result := LowerCase(Copy(Result, 2, 255));
end;

function TInstruction.toText: string;
begin
  WriteStr(Result, FValue);
  Result := Copy(Result, 2, 255);
end;

constructor TInstruction.Create(AValue: Values);
begin
  FValue := AValue;
end;

operator := (V: TInstruction.Values): TInstruction;
begin
  Result := TInstruction.Create(V);
end;

operator := (V: TInstruction): TInstruction.Values;
begin
  Result := TInstruction.Create(V);
end;

operator = (V1: TInstruction; V2: TInstruction.Values): boolean;
var
  Arg: TInstruction.Values;
begin
  ReadStr(V1.toText, Arg);
  Result := Arg = V2;
end;

operator = (V1: TInstruction.Values; V2: TInstruction): boolean;
var
  Arg: TInstruction.Values;
begin
  ReadStr(V2.toText, Arg);
  Result := Arg = V1;
end;


end.

