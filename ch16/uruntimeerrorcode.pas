unit uRuntimeErrorCode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AnyObject, uObjectUtils;

type

{ TRuntimeErrorCode }

TRuntimeErrorCode = class(TAnyObject)
  private
    type
      TErrMsgArray = specialize TDynArray<string>;
  public
    type
      TRunCode = (
        rcUNITIALIZED_VALUE, rcVALUE_RANGE, rcINVALID_CASE_EXPRESSION_VALUE,
        rcDIVISION_BY_ZERO, rcINVALID_STANDARD_FUNCTION_ARGUMENT,
        rcINVALID_INPUT, rcSTACK_OVERFLOW, rcUNIMPLEMENTED_FEATURE
      );
  private
    FValue: TRunCode;
    FMessage: string;
    class var Messages: TErrMsgArray;
  public
    //class constructor Create;
    property Value: TRunCode read FValue;
    constructor Create(AValue: TRunCode);
    function toString: string; override;
    class procedure InitMessages; static;
end;

Operator := (E: TRuntimeErrorCode.TRunCode): TRuntimeErrorCode;

implementation

Operator := (E: TRuntimeErrorCode.TRunCode): TRuntimeErrorCode;
begin
  Result := TRuntimeErrorCode.Create(E);
end;

//class constructor TRuntimeErrorCode.Create;
class procedure TRuntimeErrorCode.InitMessages;
begin
  Messages := TErrMsgArray.Create([
    'Uninitalized value',
    'Value out of range',
    'Invalid CASE expression value',
    'Division by zero',
    'Invalid standard function argument',
    'Invalid input',
    'Runtime stack overflow',
    'Unimplemented runtime feature'
  ]);
end;

constructor TRuntimeErrorCode.Create(AValue: TRunCode);
begin
  FValue := AValue;
  FMessage := Messages[Ord(FValue)];
end;

function TRuntimeErrorCode.toString: string;
begin
  Result := FMessage;
end;

initialization
  TRuntimeErrorCode.InitMessages;
finalization
  TRuntimeErrorCode.Messages.Free;

end.

