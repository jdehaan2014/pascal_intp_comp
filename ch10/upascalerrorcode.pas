unit uPascalErrorCode;

{$mode objfpc}{$H+}{$M+}

interface

uses
  Classes, SysUtils, uObjectUtils, AnyObject;

type

  { TPascalErrorCode }

  TPascalErrorCode = class(TAnyObject)
    public
      type
       TErrorCode = (
        ecALREADY_FORWARDED, ecCASE_CONSTANT_REUSED, ecIDENTIFIER_REDEFINED,
        ecIDENTIFIER_UNDEFINED, ecINCOMPATIBLE_ASSIGNMENT, ecINCOMPATIBLE_TYPES,
        ecINVALID_ASSIGNMENT, ecINVALID_CHARACTER, ecINVALID_CONSTANT,
        ecINVALID_EXPONENT, ecINVALID_EXPRESSION, ecINVALID_FIELD,
        ecINVALID_FRACTION, ecINVALID_IDENTIFIER_USAGE, ecINVALID_INDEX_TYPE,
        ecINVALID_NUMBER, ecINVALID_STATEMENT, ecINVALID_SUBRANGE_TYPE,
        ecINVALID_TARGET, ecINVALID_TYPE, ecINVALID_VAR_PARM, ecMIN_GT_MAX,
        ecMISSING_BEGIN, ecMISSING_COLON, ecMISSING_COLON_EQUALS,
        ecMISSING_COMMA, ecMISSING_CONSTANT, ecMISSING_DO, ecMISSING_DOT_DOT,
        ecMISSING_END, ecMISSING_EQUALS, ecMISSING_FOR_CONTROL,
        ecMISSING_IDENTIFIER, ecMISSING_LEFT_BRACKET, ecMISSING_OF,
        ecMISSING_PERIOD, ecMISSING_PROGRAM, ecMISSING_RIGHT_BRACKET,
        ecMISSING_RIGHT_PAREN, ecMISSING_SEMICOLON, ecMISSING_THEN,
        ecMISSING_TO_DOWNTO, ecMISSING_UNTIL, ecMISSING_VARIABLE,
        ecNOT_CONSTANT_IDENTIFIER, ecNOT_RECORD_VARIABLE, ecNOT_TYPE_IDENTIFIER,
        ecRANGE_INTEGER, ecRANGE_REAL, ecSTACK_OVERFLOW, ecTOO_MANY_LEVELS,
        ecTOO_MANY_SUBSCRIPTS, ecUNEXPECTED_EOF, ecUNEXPECTED_TOKEN,
        ecUNIMPLEMENTED_FEATURE, ecUNRECOGNIZABLE_INPUT, ecWRONG_NUMBER_OF_PARMS,
        // Fatal errors.
        ecIO_ERROR, ecTOO_MANY_ERRORS
      );
    private
      Type
       TErrMsg = record
         Text: string;
         Status: integer;
       end;
       TErrMsgArray = specialize TDynArray<TErrMsg>;
    public
      const
        ALREADY_FORWARDED : TErrMsg = (Text:'Already specified in FORWARD';Status:0);
        CASE_CONSTANT_REUSED : TErrMsg = (Text:'CASE constant reused';Status:0);
        IDENTIFIER_REDEFINED : TErrMsg = (Text:'Redefined identifier';Status:0);
        IDENTIFIER_UNDEFINED : TErrMsg = (Text:'Undefined identifier';Status:0);
        INCOMPATIBLE_ASSIGNMENT : TErrMsg = (Text:'Incompatible assignment';Status:0);
        INCOMPATIBLE_TYPES : TErrMsg = (Text:'Incompatible types';Status:0);
        INVALID_ASSIGNMENT : TErrMsg = (Text:'Invalid assignment statement';Status:0);
        INVALID_CHARACTER : TErrMsg = (Text:'Invalid character';Status:0);
        INVALID_CONSTANT : TErrMsg = (Text:'Invalid constant';Status:0);
        INVALID_EXPONENT : TErrMsg = (Text:'Invalid exponent';Status:0);
        INVALID_EXPRESSION : TErrMsg = (Text:'Invalid expression';Status:0);
        INVALID_FIELD : TErrMsg = (Text:'Invalid field';Status:0);
        INVALID_FRACTION : TErrMsg = (Text:'Invalid fraction';Status:0);
        INVALID_IDENTIFIER_USAGE : TErrMsg = (Text:'Invalid identifier usage';Status:0);
        INVALID_INDEX_TYPE : TErrMsg = (Text:'Invalid index type';Status:0);
        INVALID_NUMBER : TErrMsg = (Text:'Invalid number';Status:0);
        INVALID_STATEMENT : TErrMsg = (Text:'Invalid statement';Status:0);
        INVALID_SUBRANGE_TYPE : TErrMsg = (Text:'Invalid subrange type';Status:0);
        INVALID_TARGET : TErrMsg = (Text:'Invalid assignment target';Status:0);
        INVALID_TYPE : TErrMsg = (Text:'Invalid type';Status:0);
        INVALID_VAR_PARM : TErrMsg = (Text:'Invalid VAR parameter';Status:0);
        MIN_GT_MAX : TErrMsg = (Text:'Min limit greater than max limit';Status:0);
        MISSING_BEGIN : TErrMsg = (Text:'Missing BEGIN';Status:0);
        MISSING_COLON : TErrMsg = (Text:'Missing :';Status:0);
        MISSING_COLON_EQUALS : TErrMsg = (Text:'Missing :=';Status:0);
        MISSING_COMMA : TErrMsg = (Text:'Missing ,';Status:0);
        MISSING_CONSTANT : TErrMsg = (Text:'Missing constant';Status:0);
        MISSING_DO : TErrMsg = (Text:'Missing DO';Status:0);
        MISSING_DOT_DOT : TErrMsg = (Text:'Missing ..';Status:0);
        MISSING_END : TErrMsg = (Text:'Missing END';Status:0);
        MISSING_EQUALS : TErrMsg = (Text:'Missing =';Status:0);
        MISSING_FOR_CONTROL : TErrMsg = (Text:'Invalid FOR control variable';Status:0);
        MISSING_IDENTIFIER : TErrMsg = (Text:'Missing identifier';Status:0);
        MISSING_LEFT_BRACKET : TErrMsg = (Text:'Missing [';Status:0);
        MISSING_OF : TErrMsg = (Text:'Missing OF';Status:0);
        MISSING_PERIOD : TErrMsg = (Text:'Missing .';Status:0);
        MISSING_PROGRAM : TErrMsg = (Text:'Missing PROGRAM';Status:0);
        MISSING_RIGHT_BRACKET : TErrMsg = (Text:'Missing ]';Status:0);
        MISSING_RIGHT_PAREN : TErrMsg = (Text:'Missing )';Status:0);
        MISSING_SEMICOLON : TErrMsg = (Text:'Missing ;';Status:0);
        MISSING_THEN : TErrMsg = (Text:'Missing THEN';Status:0);
        MISSING_TO_DOWNTO : TErrMsg = (Text:'Missing TO or DOWNTO';Status:0);
        MISSING_UNTIL : TErrMsg = (Text:'Missing UNTIL';Status:0);
        MISSING_VARIABLE : TErrMsg = (Text:'Missing variable';Status:0);
        NOT_CONSTANT_IDENTIFIER : TErrMsg = (Text:'Not a constant identifier';Status:0);
        NOT_RECORD_VARIABLE : TErrMsg = (Text:'Not a record variable';Status:0);
        NOT_TYPE_IDENTIFIER : TErrMsg = (Text:'Not a type identifier';Status:0);
        RANGE_INTEGER : TErrMsg = (Text:'Integer literal out of range';Status:0);
        RANGE_REAL : TErrMsg = (Text:'Real literal out of range';Status:0);
        STACK_OVERFLOW : TErrMsg = (Text:'Stack overflow';Status:0);
        TOO_MANY_LEVELS : TErrMsg = (Text:'Nesting level too deep';Status:0);
        TOO_MANY_SUBSCRIPTS : TErrMsg = (Text:'Too many subscripts';Status:0);
        UNEXPECTED_EOF : TErrMsg = (Text:'Unexpected end of file';Status:0);
        UNEXPECTED_TOKEN : TErrMsg = (Text:'Unexpected token';Status:0);
        UNIMPLEMENTED_FEATURE : TErrMsg = (Text:'Unimplemented feature';Status:0);
        UNRECOGNIZABLE_INPUT : TErrMsg = (Text:'Unrecognizable input';Status:0);
        WRONG_NUMBER_OF_PARMS : TErrMsg = (Text:'Wrong number of actual parameters';Status:0);
        // Fatal errors.
        IO_ERROR : TErrMsg = (Text:'Object I/O error';Status:-101);
        TOO_MANY_ERRORS : TErrMsg = (Text:'Too many syntax errors';Status:-102);

    private
      FValue: TErrorCode;
      FStatus: integer;
      FMessage: string;
      class var Messages: TErrMsgArray;
    public
      class constructor Create;
      property Value: TErrorCode read FValue;
      property getStatus: integer read FStatus;
      constructor Create(AValue: TErrorCode);
      function toString: string; override;
  end;

Operator := (E: TPascalErrorCode.TErrorCode): TPascalErrorCode;

implementation

Operator := (E: TPascalErrorCode.TErrorCode): TPascalErrorCode;
begin
  Result := TPascalErrorCode.Create(E);
end;

class constructor TPascalErrorCode.Create;
begin
  Messages := TErrMsgArray.Create([
    ALREADY_FORWARDED, CASE_CONSTANT_REUSED, IDENTIFIER_REDEFINED,
    IDENTIFIER_UNDEFINED, INCOMPATIBLE_ASSIGNMENT, INCOMPATIBLE_TYPES,
    INVALID_ASSIGNMENT, INVALID_CHARACTER, INVALID_CONSTANT,
    INVALID_EXPONENT, INVALID_EXPRESSION, INVALID_FIELD,
    INVALID_FRACTION, INVALID_IDENTIFIER_USAGE, INVALID_INDEX_TYPE,
    INVALID_NUMBER, INVALID_STATEMENT, INVALID_SUBRANGE_TYPE,
    INVALID_TARGET, INVALID_TYPE, INVALID_VAR_PARM, MIN_GT_MAX,
    MISSING_BEGIN, MISSING_COLON, MISSING_COLON_EQUALS,
    MISSING_COMMA, MISSING_CONSTANT, MISSING_DO, MISSING_DOT_DOT,
    MISSING_END, MISSING_EQUALS, MISSING_FOR_CONTROL,
    MISSING_IDENTIFIER, MISSING_LEFT_BRACKET, MISSING_OF,
    MISSING_PERIOD, MISSING_PROGRAM, MISSING_RIGHT_BRACKET,
    MISSING_RIGHT_PAREN, MISSING_SEMICOLON, MISSING_THEN,
    MISSING_TO_DOWNTO, MISSING_UNTIL, MISSING_VARIABLE,
    NOT_CONSTANT_IDENTIFIER, NOT_RECORD_VARIABLE, NOT_TYPE_IDENTIFIER,
    RANGE_INTEGER, RANGE_REAL, STACK_OVERFLOW, TOO_MANY_LEVELS,
    TOO_MANY_SUBSCRIPTS, UNEXPECTED_EOF, UNEXPECTED_TOKEN,
    UNIMPLEMENTED_FEATURE, UNRECOGNIZABLE_INPUT, WRONG_NUMBER_OF_PARMS,
    // Fatal errors.
    IO_ERROR, TOO_MANY_ERRORS
  ]);
end;

constructor TPascalErrorCode.Create(AValue: TErrorCode);
begin
  FValue := AValue;
  FMessage := Messages[Ord(FValue)].Text;
  FStatus := Messages[Ord(FValue)].Status;
end;

function TPascalErrorCode.toString: string;
begin
  Result := FMessage;
end;



end.

