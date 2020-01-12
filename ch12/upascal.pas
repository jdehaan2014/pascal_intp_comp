unit uPascal;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, uObjectUtils, uTextReader, usource, ubaseparser,
  uFrontendFactory, uGenericInterfaces, ucode, ubackend, uBackendFactory,
  uCodeImpl, uCrossRef, umessages,
  uTokenType, uPascalTokenTyp, AnyObject, uParseTreePrinter,
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
             FetchFormat = '>>> AT LINE %03d: %s : %s' + LineEnding;
             CallFormat = '>>> AT LINE %03d: CALL %s' + LineEnding;
             ReturnFormat = '>>> AT LINE %03d: RETURN FROM %s' + LineEnding;
           class var
             firstOutputMessage: Boolean;
         public
           //class constructor Create;
           procedure MessageReceived(Message: TMessage);
       end;

     var
       Parser: TBaseParser;
       Source: TSource;
       ICode: IIntermediateCode;
       symTabStack: ISymTabStack;
       Backend: TBackend;
     class var
       Lines, Assign, Fetch, Call, Return: boolean;

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

{class constructor TPascal.TBackendMessageListener.Create;
begin
  TPascal.TBackendMessageListener.firstOutputMessage := True;
end; }

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
  VariableName, RoutineName: string;
  Value: TObject;
  ErrorMsg: string;

begin
  Body := Message.getBody;
  MsgTyp := Message.getTyp;
  case MsgTyp of
    mtSourceLine:
      begin
        if Lines then begin
          LineNumber := (Body as TSourceLineMsg).LineNum;
          WriteLn(Format(LineFormat, [LineNumber]));
        end;
      end;
    mtAssign:
      begin
        if Assign then begin
          LineNumber   := (Body as TAssignMsg).SourceLineNumber;
          VariableName := (Body as TAssignMsg).TargetVariableName;
          Value        := (Body as TAssignMsg).ExpressionValue;
          Writeln(Format(AssignFormat, [LineNumber, VariableName, Value.toString]))
        end;
      end;
    mtFetch:
      begin
        if Fetch then begin
          LineNumber   := (Body as TFetchMsg).SourceLineNumber;
          VariableName := (Body as TFetchMsg).TargetVariableName;
          Value        := (Body as TFetchMsg).ExpressionValue;
          Writeln(Format(FetchFormat, [LineNumber, VariableName, Value.toString]))
        end;
      end;
    mtCall:
      begin
        if Call then begin
          LineNumber  := (Body as TCallMsg).SourceLineNumber;
          RoutineName := (Body as TCallMsg).CalledRoutineName;
          Writeln(Format(CallFormat, [LineNumber, RoutineName]))
        end;
      end;
    mtReturn:
      begin
        if Return then begin
          LineNumber  := (Body as TReturnMsg).SourceLineNumber;
          RoutineName := (Body as TReturnMsg).ReturnedRoutineName;
          Writeln(Format(ReturnFormat, [LineNumber, RoutineName]))
        end;
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
  CrossReferencer: TCrossReferencer;
  TreePrinter: TParseTreePrinter;
  OutPutList: TStringList;
  OutputLine: string;
  ProgramID: ISymTabEntry;

begin
  try
    try
      Intermediate := Pos('i', AFlags) > 0;  //true to print intermediate code
      xref := Pos('x', AFlags) > 0;          //,, to print cross ref listing
      Lines := Pos('l', AFlags) > 0;         //,, to print source line tracing
      Assign := Pos('a', AFlags) > 0;         //,, to print value assignment tracing
      Fetch := Pos('f', AFlags) > 0;         //,, to print value fetch tracing
      Call := Pos('c', AFlags) > 0;         //,, to print routine call tracing
      Return := Pos('r', AFlags) > 0;         //,, to print routine return tracing

      Source := TSource.Create(
        TTextReader.Create(TFileStream.Create(AFilePath, fmOpenRead)));

      Source.addMessageListener(TSourceMessageListener.Create);

      Parser := TFrontEndFactory.CreateParser('Pascal', 'top-down', Source);
      Parser.addMessageListener(TParserMessageListener.Create);

      Backend := TBackendFactory.CreateBackend(AOperation);
      Backend.addMessageListener(TBackendMessageListener.Create);

      Parser.Parse;
      Source.Free;

      if Parser.getErrorCount = 0 then begin
        SymTabStack := Parser.getSymTabStack;

        ProgramID := SymTabStack.getProgramID;
        ICode := TICodeImpl(ProgramID.getAttribute(skRoutineICode));

        if Intermediate then begin
          OutPutList := TStringList.Create;
          TreePrinter := TParseTreePrinter.Create(OutPutList);
          TreePrinter.Print(SymTabStack, OutPutList);

          for OutputLine in OutPutList do
            writeln(OutputLine);
        end;

        if xref then begin
          CrossReferencer := TCrossReferencer.Create;
          CrossReferencer.Print(SymTabStack);
        end;

        Backend.Process(ICode, SymTabStack);
      end;


    except
      on E: Exception do begin
        Writeln(E.Message);
        Writeln('***** Internal translator error. *****');
      end;
    end;

  finally
    Parser.Free;
    Backend.Free;
  end;
end;

initialization
  TPascal.TBackendMessageListener.firstOutputMessage := True;

end.

