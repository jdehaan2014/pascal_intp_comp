unit uPascal;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, uObjectUtils, uTextReader, usource, ubaseparser,
  uFrontendFactory, uGenericInterfaces, ucode, ubackend, uBackendFactory,
  uCrossRef, umessages,
  uTokenType, uPascalTokenTyp, AnyObject, uParseTreePrinter,
  uSymTabImpl, uIDEControl;

type

  { TPascal }

  TPascal = class
    private
     type
       TSourceMessageListener = class(TAnyObject, IMessageListener)
         private
           const
            SourceLineFormat = LISTING_TAG + '%.3d %s';
         public
           procedure MessageReceived(Message: TMessage);
       end;

       TParserMessageListener = class(TAnyObject, IMessageListener)
         private
           const
            ParserSummaryFormat = PARSER_TAG +
                                  '%d source lines, ' +
                                  '%d syntax errors, ' +
                                  '%.4f seconds total parsing time.' + LineEnding;
            PrefixWidth = 5;
         public
           procedure MessageReceived(Message: TMessage);
       end;

       { TBackendMessageListener }

       TBackendMessageListener = class(TAnyObject, IMessageListener)
         private
           const
             InterpreterSummaryFormat = INTERPRETER_TAG +
               '%d statements executed, ' +
               '%d runtime errors, ' +
               '%.4f seconds total execution time.' + LineEnding;
             CompilerSummaryFormat    =
               LineEnding + '%20d instructions generated. ' +
               LineEnding + '%20.4f seconds total code generation time.' + LineEnding;
         public
           procedure MessageReceived(Message: TMessage);
       end;

     var
       Parser: TBaseParser;
       Source: TSource;
       ICode: IIntermediateCode;
       SymTabStack: ISymTabStack;
       Backend: TBackend;

    public
      constructor Create(Operation: string; SourcePath, InputPath: string;
        Flags: string);
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
        Writeln(Format(SourceLineFormat, [LineNumber, LineText]));
      end;
  end;
end;

{ TPascal.TParserMessageListener }
// Listener for parser messages.
procedure TPascal.TParserMessageListener.MessageReceived(Message : TMessage);
var
  MsgTyp : TMessageType;
  Body : TMessageBody;
  StatementCount, LineNumber, Position : Integer;
  SyntaxErrors : Integer;
  ElapsedTime : Double;
  TokenText, ErrorMsg: string;
  FlagBuffer: TString;
begin
  MsgTyp := Message.getTyp;
  Body := Message.getBody;
  case MsgTyp of
    mtParserSummary:
      begin
        StatementCount := (Body as TParserSummaryMsg).NumSourceLines;
        SyntaxErrors   := (Body as TParserSummaryMsg).ErrorCount;
        ElapsedTime    := (Body as TParserSummaryMsg).ElapsedTime;
        Write(Format(ParserSummaryFormat,
          [StatementCount, SyntaxErrors, ElapsedTime]));
      end;
    mtSyntaxError:
      begin
        LineNumber := (Body as TSyntaxErrorMsg).SourceLineNumber;
        Position   := (Body as TSyntaxErrorMsg).StartPosition;
        TokenText  := (Body as TSyntaxErrorMsg).TokenText;
        ErrorMsg   := (Body as TSyntaxErrorMsg).SyntaxErrorMsg;

        FlagBuffer := TString.Create;
        FlagBuffer.Append(Format(SYNTAX_TAG + '%d: %s', [LineNumber, ErrorMsg]));

        //Text, if any of the bad token
        if TokenText <> '' then
          FlagBuffer.Append(' [at "').Append(TokenText).Append('"]');
        Writeln(FlagBuffer.toString);
      end;
  end;
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
begin
  Body := Message.getBody;
  MsgTyp := Message.getTyp;
  case MsgTyp of
    mtInterpreterSummary:
      begin
        ExecutionCount := (Body as TInterpreterSummaryMsg).ExecutionCount;
        RuntimeErrors  := (Body as TInterpreterSummaryMsg).RuntimeErrors;
        ElapsedTime    := (Body as TInterpreterSummaryMsg).ElapsedTime;
        Write(Format(InterpreterSummaryFormat,
                       [ExecutionCount, RuntimeErrors, ElapsedTime]));
      end;
    mtCompilerSummary:
      begin
        InstructionCount := (Body as TCompilerSummaryMsg).InstructionCount;
        ElapsedTime    := (Body as TCompilerSummaryMsg).ElapsedTime;
        Write(Format(CompilerSummaryFormat, [InstructionCount, ElapsedTime]));
      end;
  end;
end;


{ TPascal }

constructor TPascal.Create(Operation : string; SourcePath, InputPath : string;
  Flags : string);
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
      Intermediate := Pos('i', Flags) > 0;  //true to print intermediate code
      xref := Pos('x', Flags) > 0;          //,, to print cross ref listing

      Source := TSource.Create(
        TTextReader.Create(TFileStream.Create(SourcePath, fmOpenRead)));
      Source.addMessageListener(TSourceMessageListener.Create);

      Parser := TFrontEndFactory.CreateParser('Pascal', 'top-down', Source);
      Parser.addMessageListener(TParserMessageListener.Create);

      Backend := TBackendFactory.CreateBackend(Operation, InputPath);
      Backend.addMessageListener(TBackendMessageListener.Create);

      Parser.Parse;
      Source.Free;

      if Parser.getErrorCount = 0 then begin
        SymTabStack := Parser.getSymTabStack;

        ProgramID := SymTabStack.getProgramID;
        ICode := ProgramID.getAttribute(skRoutineICode) as IIntermediateCode;

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
        raise Exception.Create(E.Message + LineEnding +
          '***** Internal translator error. *****');
        //Writeln(E.Message);
        //Writeln('***** Internal translator error. *****');
      end;
    end;

  finally
    Parser.Free;
    Backend.Free;
  end;
end;


end.

