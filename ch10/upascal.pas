unit uPascal;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, usource, ubaseparser, ubackend, uBackendFactory,
  uGenericInterfaces, ucode, uCodeImpl, uCrossRef, umessages, uFrontendFactory,
  uTokenType, uPascalTokenTyp, uObjectUtils,  AnyObject, uParseTreePrinter,
  uSymTabImpl;

type

  { TPascal }

  TPascal = class
    private
     type
       TSourceMessageListener = class(TAnyObject, IMessageListener)
         private
           const
            SourceLineFormat = '%.3d %s';
         public
           procedure MessageReceived(Message: TMessage);
       end;

       TParserMessageListener = class(TAnyObject, IMessageListener)
         private
           const
            TokenFormat = '>>> %-15s line=%.3d, pos=%2d, text="%s"';
            ValueFormat = '>>>                 value=%s';
            ParserSummaryFormat = LineEnding + '%20d source lines.' +
                                  LineEnding + '%20d syntax errors.' +
                                  LineEnding + '%20.4f seconds total parsing time.' + LineEnding;
            PrefixWidth = 5;
         public
           procedure MessageReceived(Message: TMessage);
       end;

       { TBackendMessageListener }

       TBackendMessageListener = class(TAnyObject, IMessageListener)
         private
           const
             InterpreterSummaryFormat =
               LineEnding + '%20d statements executed.' +
               LineEnding + '%20d runtime errors.' +
               LineEnding + '%20.4f seconds total execution time.' + LineEnding;
             CompilerSummaryFormat    =
               LineEnding + '%20d instructions generated. ' +
               LineEnding + '%20.4f seconds total code generation time.' + LineEnding;
             LineFormat = '>>> AT LINE %03d';
             AssignFormat = '>>> LINE %03d: %s := %s';
           class var
             firstOutputMessage: Boolean;
         public
           class constructor Create;
           procedure MessageReceived(Message: TMessage);
       end;

     var
       Parser: TBaseParser;
       Source: TSource;
       ICode: IIntermediateCode;
       symTabStack: ISymTabStack;
       Backend: TBackend;
    public
      constructor Create(AOperation: string; AFilePath: string; AFlags: string);
  end;

implementation

{ TPascal.TSourceMessageListener }
//Listener for source messages.
procedure TPascal.TSourceMessageListener.MessageReceived(Message : TMessage);
var
  MsgTyp : TMessageType;
  Body : TMessageBody;
  LineNumber : Integer;
  LineText : String;
begin
  Body := Message.getBody;
  MsgTyp := Message.getTyp;
  case MsgTyp of
    mtSourceLine:
      begin
        LineNumber := (Body as TSourceLineMsg).LineNum;
        LineText   := (Body as TSourceLineMsg).Line;
        Write(Format(SourceLineFormat, [LineNumber, LineText]));
      end;
  end;
end;

{ TPascal.TParserMessageListener }
// Listener for parser messages.
procedure TPascal.TParserMessageListener.MessageReceived(Message : TMessage);
var
  MsgTyp : TMessageType;
  Body : TMessageBody;
  StatementCount, LineNumber, Position, SpaceCount : Integer;
  SyntaxErrors : Integer;
  ElapsedTime : Double;
  TokenTyp: ITokenTyp;
  TokenText, ErrorMsg, FlagBuffer, TokenValueStr: string;
  TokenValue: TObject;
begin
  MsgTyp := Message.getTyp;
  Body := Message.getBody;
  case MsgTyp of
    mtToken:
      begin
        LineNumber := (Body as TTokenMsg).SourceLineNumber;
        Position   := (Body as TTokenMsg).StartPosition;
        TokenTyp   := (Body as TTokenMsg).TokenTyp;
        TokenText  := (Body as TTokenMsg).TokenText;
        TokenValue := (Body as TTokenMsg).TokenValue;
        WriteLn(Format(TokenFormat,
          [TokenTyp.toString, LineNumber, Position, TokenText]));
        if TokenValue <> Nil then begin
          case TPascalTokenTyp(TokenTyp.getObject).Value of
            ttInteger: TokenValueStr := TInteger(TokenValue).toString;
            ttReal:    TokenValueStr := TFloat(TokenValue).toString;
            ttString: begin
              TokenValueStr := TString(TokenValue).toString;
              TokenValueStr := '"' + TokenValueStr + '"';
            end;
          end;
          Writeln(Format(ValueFormat, [TokenValueStr]));
        end;
      end;
    mtSyntaxError:
      begin
        LineNumber := (Body as TSyntaxErrorMsg).SourceLineNumber;
        Position   := (Body as TSyntaxErrorMsg).StartPosition;
        TokenText  := (Body as TSyntaxErrorMsg).TokenText;
        ErrorMsg   := (Body as TSyntaxErrorMsg).SyntaxErrorMsg;
        SpaceCount := PrefixWidth + Position;
        // write error message at error position
        FlagBuffer := StringOfChar(' ', SpaceCount-1);
        FlagBuffer += '^' + LineEnding + '*** '+ ErrorMsg;
        if TokenText <> '' then
          FlagBuffer += ' [at "' + TokenText + '"]';
        Writeln(FlagBuffer);
      end;
    mtParserSummary:
      begin
        StatementCount := (Body as TParserSummaryMsg).NumSourceLines;
        SyntaxErrors   := (Body as TParserSummaryMsg).ErrorCount;
        ElapsedTime    := (Body as TParserSummaryMsg).ElapsedTime;
        WriteLn(Format(ParserSummaryFormat,
          [StatementCount, SyntaxErrors, ElapsedTime]));
      end;
  end;
end;

class constructor TPascal.TBackendMessageListener.Create;
begin
  firstOutputMessage := True;
end;

{ TPascal.TBackendMessageListener }
//Called by the back end whenever it produces a message.
procedure TPascal.TBackendMessageListener.MessageReceived(Message : TMessage);
var
  MsgTyp : TMessageType;
  Body : TMessageBody;
  ExecutionCount : Integer;
  RuntimeErrors : Integer;
  ElapsedTime : Double;
  InstructionCount : Integer;
  LineNumber: integer;
  VariableName: string;
  Value: TObject;
  ErrorMsg: string;

begin
  Body := Message.getBody;
  MsgTyp := Message.getTyp;
  case MsgTyp of
    mtAssign:
      begin
        if firstOutputMessage then begin
          Writeln;
          Writeln('===== OUTPUT =====');
          Writeln;
          firstOutputMessage := False;
        end;
        LineNumber   := (Body as TAssignMsg).SourceLineNumber;
        VariableName := (Body as TAssignMsg).TargetVariableName;
        Value        := (Body as TAssignMsg).ExpressionValue;
        Writeln(Format(AssignFormat, [LineNumber, VariableName, Value.toString]))
      end;
    mtRuntimeError:
      begin
        ErrorMsg   := (Body as TRuntimeErrorMsg).RuntimeErrorMsg;
        LineNumber := (Body as TRuntimeErrorMsg).SourceLineNumber;
        Write('*** RUNTIME ERROR');
        if LineNumber > 0 then
          Write(Format(LineFormat, [LineNumber]));
        Writeln(': ', ErrorMsg);
      end;
    mtInterpreterSummary:
      begin
        ExecutionCount := (Body as TInterpreterSummaryMsg).ExecutionCount;
        RuntimeErrors  := (Body as TInterpreterSummaryMsg).RuntimeErrors;
        ElapsedTime    := (Body as TInterpreterSummaryMsg).ElapsedTime;
        WriteLn(Format(InterpreterSummaryFormat, [ExecutionCount, RuntimeErrors, ElapsedTime]));
      end;
    mtCompilerSummary:
      begin
        InstructionCount := (Body as TCompilerSummaryMsg).InstructionCount;
        ElapsedTime    := (Body as TCompilerSummaryMsg).ElapsedTime;
        WriteLn(Format(CompilerSummaryFormat, [InstructionCount, ElapsedTime]));
      end;
  end;
end;


{ TPascal }

constructor TPascal.Create(AOperation : string; AFilePath : string;
  AFlags : string);
var
  Intermediate, xref: boolean;
  crossReferencer: TCrossReferencer;
  treePrinter: TParseTreePrinter;
  OutPutList: TStringList;
  OutputLine: string;
  ProgramID: ISymTabEntry;

begin
  try
    Intermediate := Pos('i', AFlags) > 0;
    xref := Pos('x', AFlags) > 0;

    Source := TSource.Create(TMyReader.Create(AFilePath));
    Source.addMessageListener(TSourceMessageListener.Create);

    Parser := TFrontEndFactory.CreateParser('Pascal', 'top-down', Source);
    Parser.addMessageListener(TParserMessageListener.Create);

    Backend := TBackendFactory.CreateBackend(AOperation);
    Backend.addMessageListener(TBackendMessageListener.Create);

    Parser.Parse;
    Source.Destroy;

    if Parser.getErrorCount = 0 then begin
      symtabStack := Parser.getSymTabStack;

      ProgramID := symtabStack.getProgramID;
      ICode := TICodeImpl(ProgramID.getAttribute(skRoutineICode));

      if xref then begin
        crossReferencer := TCrossReferencer.Create;
        crossReferencer.Print(symtabStack);
      end;

      if Intermediate then begin
        OutPutList := TStringList.Create;
        treePrinter := TParseTreePrinter.Create(OutPutList);
        treePrinter.Print(symTabStack, OutPutList);

        for OutputLine in OutPutList do
          writeln(OutputLine);
        OutPutList.Destroy;
      end;

      Backend.Process(ICode, symtabStack);
    end;

  except
    on E: Exception do begin
      Writeln(E.Message);
      Writeln('***** Internal translator error. *****');
    end;
  end;
end;

end.

