unit uPascal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umessages, utoken, utokens, usource, ubasescanner, uPascalScanner,
  ubaseparser, uPascalParserTD, ucode, usymtab, uFrontendFactory, ubackend, uBackendFactory;

type

  { TPascal }

  TPascal = class
    private
     type
       TSourceMessageListener = class(TInterfacedObject, IMessageListener)
         private
           const
            SourceLineFormat = '%.3d %s';
         public
           procedure MessageReceived(Message: TMessage);
       end;

       TParserMessageListener = class(TInterfacedObject, IMessageListener)
         private
           const
            ParserSummaryFormat = LineEnding + '%20d source lines.' +
                                  LineEnding + '%20d syntax errors.' +
                                  LineEnding + '%20.4f seconds total parsing time.' + LineEnding;
         public
           procedure MessageReceived(Message: TMessage);
       end;

       TBackendMessageListener = class(TInterfacedObject, IMessageListener)
         private
           const
             InterpreterSummaryFormat =
               LineEnding + '%20d statements executed.' +
               LineEnding + '%20d runtime errors.' +
               LineEnding + '%20.4f seconds total execution time.' + LineEnding;
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
       Symtab: ISymtab;
       Backend: TBackend;
    public
      constructor Create(AOperation: string; AFilePath: string; AFlags: string);
  end;

implementation

{ TPascal.TSourceMessageListener }
//Listener for source messages.
procedure TPascal.TSourceMessageListener.MessageReceived(Message : TMessage);
var
  Typ : TMessageType;
  Body : TMessageBody;
  LineNumber : Integer;
  LineText : String;
begin
  Typ := Message.getTyp;
  Body := Message.getBody;

  case Typ of
    mtSourceLine:
      begin
        LineNumber := (Body as TSourceLineMsg).LineNum;
        LineText   := (Body as TSourceLineMsg).Line;
        WriteLn(Format(SourceLineFormat, [LineNumber, LineText]));
      end;
  end;
end;

{ TPascal.TParserMessageListener }
// Listener for parser messages.
procedure TPascal.TParserMessageListener.MessageReceived(Message : TMessage);
var
  Typ : TMessageType;
  Body : TMessageBody;
  StatementCount : Integer;
  SyntaxErrors : Integer;
  ElapsedTime : Real;
begin
  Typ := Message.getTyp;
  Body := Message.getBody;
  case Typ of
    mtParserSummary:
      begin
        StatementCount := (Body as TParserSummaryMsg).NumSourceLines;
        SyntaxErrors   := (Body as TParserSummaryMsg).ErrorCount;
        ElapsedTime    := (Body as TParserSummaryMsg).ElapsedTime;
        WriteLn(Format(ParserSummaryFormat, [StatementCount, SyntaxErrors, ElapsedTime]));
      end;
  end;
end;

{ TPascal.TBackendMessageListener }
//Called by the back end whenever it produces a message.
procedure TPascal.TBackendMessageListener.MessageReceived(Message : TMessage);
var
  Typ : TMessageType;
  Body : TMessageBody;
  ExecutionCount : Integer;
  RuntimeErrors : Integer;
  ElapsedTime : Real;
  InstructionCount : Integer;
begin
  Typ := Message.getTyp;
  Body := Message.getBody;
  case Typ of
    mtInterpreterSummary:
      begin
        ExecutionCount := (Body as TInterpreterSummaryMsg).ExecutionCount;
        RuntimeErrors   := (Body as TInterpreterSummaryMsg).RuntimeErrors;
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

    ICode := Parser.getICode;
    Symtab := Parser.getSymTab;

    Backend.Process(ICode, Symtab);

  except
    on E: Exception do begin
      Writeln(E.Message);
      Writeln('***** Internal translator error. *****');
    end;
  end;
end;

end.

