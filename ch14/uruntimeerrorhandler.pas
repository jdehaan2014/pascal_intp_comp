unit uRuntimeErrorHandler;
// Runtime error handler for the backend interpreter

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uMessages, uBackend, uCode, uCodeImpl, uRuntimeErrorCode,
  uObjectUtils;

type

  { TRuntimeErrorHandler }

  TRuntimeErrorHandler = class
    private
      const MAX_ERRORS = 5;
      class var errorCount: integer;
    public
      //class constructor Create;
      class function getErrorCount: integer; static;
      procedure Flag(Node: ICodeNode; errorCode: TRuntimeErrorCode; Backend: TBackend);
  end;


implementation

{ TRuntimeErrorHandler }

{class constructor TRuntimeErrorHandler.Create;
begin
  errorCount := 0;
end; }

class function TRuntimeErrorHandler.getErrorCount: integer;
begin
  Result := errorCount;
end;

procedure TRuntimeErrorHandler.Flag(Node: ICodeNode;
  errorCode: TRuntimeErrorCode; Backend: TBackend);
//var
  //LineNumber: string = '';
begin
  //Look for the ancestor statement node with a line number attribute
  while (Node <> Nil) and (Node.getAttribute(ckLine) = Nil) do
    Node := Node.getParent;
  //Notify the interpreters listeners
  Backend.sendMessage(
    TMessage.Create(
      mtRuntimeError,
      TRuntimeErrorMsg.Create(
        ErrorCode.toString,
        TInteger(Node.getAttribute(ckLine)).Value
      )
    )
  );
  Inc(errorCount);
  if errorCount > MAX_ERRORS then begin
    Writeln('*** ABORTED AFTER TOO MANY RUNTIME ERRORS.');
    Halt(-1);
  end;
end;

initialization
  TRuntimeErrorHandler.errorCount := 0;


end.

