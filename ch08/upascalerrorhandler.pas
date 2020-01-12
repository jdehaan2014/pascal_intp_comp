unit uPascalErrorHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, utoken, umessages, ubaseparser,
  uPascalErrorCode;


type

  //Error handler Pascal syntax errors.

  { TPascalErrorHandler }

  TPascalErrorHandler = class
    private
      const cMaxErrors = 25;
      class var ErrorCount: integer; //count of syntax errors
    public
      function getErrorCount: integer;      // return syntax error count
      procedure Flag(BadToken: TToken; ErrorCode: TPascalErrorCode; Parser: TBaseParser);
      procedure AbortTranslation(ErrorCode: TPascalErrorCode; Parser: TBaseParser);
  end;

implementation

{ TPascalErrorHandler }

function TPascalErrorHandler.getErrorCount : integer;
begin
  Result := ErrorCount;
end;

//Flag an error in the source line.
procedure TPascalErrorHandler.Flag(BadToken : TToken;
  ErrorCode : TPascalErrorCode; Parser : TBaseParser);
begin
  // Notify the parser's listeners
  Parser.sendMessage(TMessage.Create(mtSyntaxError,
                       TSyntaxErrorMsg.Create(
                         BadToken.getLineNum, BadToken.getPosition, BadToken.getText,
                         ErrorCode.toString)));
  Inc(ErrorCount);
  if ErrorCount > cMaxErrors then
    AbortTranslation(TPascalErrorCode.Create(ecTOO_MANY_ERRORS), Parser);
end;

//Abort the translation.
procedure TPascalErrorHandler.AbortTranslation(ErrorCode : TPascalErrorCode;
  Parser : TBaseParser);
var
  FatalText: string;
begin
  // Notify the parser's listeners and then abort.
  FatalText := 'FATAL ERROR: ' + ErrorCode.toString;
  Parser.sendMessage(TMessage.Create(mtSyntaxError,
                       TSyntaxErrorMsg.Create(0, 0, '', FatalText)));
  Halt(ErrorCode.getStatus);
end;

end.

