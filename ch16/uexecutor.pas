unit uExecutor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uBaseScanner, uPascalScanner, uSource, uTextReader,
  ubackend, dateutils, ucode, uCodeImpl, uGenericInterfaces, umessages,
  uRuntimeErrorHandler, uMemoryFactory, uMemory, uCodeFactory, fgl,
  uDebugger, uDebuggerType, uBackendFactory;

type

  TCellList = specialize TFPGList<ICell>;

  { TExecutor }

  TExecutor = class(TBackEnd)
    protected
      class var
        ExecutionCount: integer;
        RuntimeStack: IRuntimeStack;
        ErrorHandler: TRuntimeErrorHandler;
        StandardIn: TBaseScanner;  //standard input
        //StandardOut: TStream;
      var
        Debugger: TDebugger;       //interactive source level debugger
    public
      constructor Create(InputPath: string);
      constructor Create(Parent: TExecutor);
      destructor Destroy;
      function getErrorHandler: TRuntimeErrorHandler;
      procedure Process(AiCode: IIntermediateCode;
        ASymtabStack: ISymtabStack); override;
  end;

implementation
uses uExecutors;

{ TCodeGenerator }

constructor TExecutor.Create(InputPath: string);
begin
  try
    if InputPath <> '' then
      TExecutor.StandardIn := TPascalScanner.Create(
        TSource.Create(
          TTextReader.Create(
            TFileStream.Create(InputPath, fmOpenRead)
          )
        )
      )
    else
      TExecutor.StandardIn := TPascalScanner.Create(
        TSource.Create(
          TTextReader.Create(
            THandleStream.Create(StdInputHandle)
          )
        )
      );

  except
    on Ignore: Exception do begin
      Writeln('IO Error');
    end;
  end;

  Debugger := TBackendFactory.CreateDebugger(dtCOMMAND_LINE, Self, RuntimeStack);
end;

constructor TExecutor.Create(Parent: TExecutor);
begin
  Inherited Create;
  Self.Debugger := Parent.Debugger;
end;

destructor TExecutor.Destroy;
begin
  if assigned(StandardIn) then StandardIn.Free;
  if assigned(Debugger) then Debugger.Free;
  inherited;
end;

function TExecutor.getErrorHandler: TRuntimeErrorHandler;
begin
  Result := errorHandler;
end;

// Execute the source program by
// processing the intermediate code and the symbol table generated by the
// parser..
procedure TExecutor.Process
  (AiCode: IIntermediateCode; ASymtabStack: ISymtabStack);
var
  StartTime: TDateTime;
  ElapsedTime: Double;
  ProgramID: ISymTabEntry;
  CallNode: ICodeNode;
  RuntimeErrors: integer;
  CallExecutor: TCallDeclaredExecutor;
begin
  SymTabStack := ASymTabStack;
  StartTime := TimeOf(Now);

  ProgramID := SymTabStack.getProgramID;

  //Construct an artificial CALL node to the main program
  CallNode := TICodeFactory.CreateICodeNode(ctCall);
  CallNode.setAttribute(ckID, ProgramID.getObject);

  //Execute the main program
  CallExecutor := TCallDeclaredExecutor.Create(Self);
  CallExecutor.Execute(CallNode);

  ElapsedTime := SecondSpan(StartTime, TimeOf(Now));

  runtimeErrors := errorHandler.getErrorCount;

  SendMessage(TMessage.Create(mtInterpreterSummary,
    TInterpreterSummaryMsg.Create(ExecutionCount, RuntimeErrors, ElapsedTime)));

end;

Initialization
  TExecutor.ExecutionCount := 0;
  TExecutor.RuntimeStack := TMemoryFactory.CreateRuntimeStack;
  TExecutor.ErrorHandler := TRuntimeErrorHandler.Create;

Finalization
  TExecutor.ErrorHandler.Free;
end.

