unit umessages;

{$mode objfpc}{$H+}


interface

uses
  Classes, SysUtils, utokentype, uObjectUtils, AnyObject;


type
  TMessageType = (mtSourceLine, mtSyntaxError, mtParserSummary, mtInterpreterSummary,
                  mtCompilerSummary, mtMiscellaneous, mtToken, mtAssign, mtFetch,
                  mtBreakPoint, mtRuntimeError, mtCall, mtReturn);

  TMessageBody = class
  end;

  { TMessage }

  TMessage = class
    private
      Typ: TMessageType;
      Body: TMessageBody;
    public
      property getTyp: TMessageType read Typ;
      property getBody: TMessageBody read Body;
      constructor Create(ATyp: TMessageType; ABody: TMessageBody);
      destructor Destroy;
  end;

  IMessageListener = interface
    procedure MessageReceived(Message: TMessage); // called to receive a msg sent by a msgproduced
  end;

  IMessageProducer = interface
    procedure addMessageListener(Listener: IMessageListener);  //Add a listener to the listener list
    procedure removeMessageListener(Listener: IMessageListener);  //Remove a listener from the listener list
    procedure sendMessage(AMessage: TMessage);           //Notify listeners after setting the message
  end;

  TListeners = TInterfaceList;

  { TMessageHandler }

  TMessageHandler = class
    private
      Message: TMessage;
      Listeners: TListeners;
      procedure NotifyListeners;
    public
      constructor Create;
      destructor Destroy;
      procedure AddListener(AListener: IMessageListener);  // add listener to list
      procedure RemoveListener(AListener: IMessageListener); // remove from list
      procedure SendMessage(AMessage: TMessage);  // notify listeners
  end;



  { TSourceLineMsg }

  TSourceLineMsg = class(TMessageBody)
    Line: string;
    LineNum: integer;
    constructor Create(ALine: string; ALineNum: integer);
  end;

  { TParserSummaryMsg }

  TParserSummaryMsg = class(TMessageBody)
    NumSourceLines: integer;
    ErrorCount: integer;
    ElapsedTime: LongWord;
    constructor Create(ALines: integer; ACount: integer; ATime: LongWord);
  end;

  { TCompilerSummaryMsg }

  TCompilerSummaryMsg = class(TMessageBody)
    InstructionCount: integer;
    ElapsedTime: LongWord;
    constructor Create(AInstructionCount: integer; AElapsedTime: LongWord);
  end;

  { TInterpreterSummaryMsg }

  TInterpreterSummaryMsg = class(TMessageBody)
    ExecutionCount: integer;
    RuntimeErrors: integer;
    ElapsedTime: LongWord;
    constructor Create(AExecutionCount: integer; ARuntimeErrors: integer;
      AElapsedTime: LongWord);
  end;

  { TSyntaxErrorMsg }

  TSyntaxErrorMsg = class(TMessageBody)
    SourceLineNumber: integer;
    StartPosition: integer;
    TokenText: string;
    SyntaxErrorMsg: string;
    constructor Create(ASourceLineNumber: integer; AStartPosition: integer;
      ATokenText: string; ASyntaxErrorMsg: string);
  end;

  { TTokenMsg }

  TTokenMsg = class(TMessageBody)
    SourceLineNumber: integer;
    StartPosition: integer;
    TokenTyp: ITokenTyp;
    TokenText: string;
    TokenValue: TAnyObject;
    constructor Create(ASourceLineNumber: integer; AStartPosition: integer; ATokenTyp: ITokenTyp;
      ATokenText: string; ATokenValue: TAnyObject);
  end;


implementation

{ TMessageHandler }

procedure TMessageHandler.NotifyListeners;
var
  i: integer;
begin
  for i:= 0 to Listeners.Count-1 do
    IMessageListener(Listeners[i]).MessageReceived(Message);
end;

constructor TMessageHandler.Create;
begin
  Listeners:= TListeners.Create;
end;

destructor TMessageHandler.Destroy;
begin
  Listeners.Destroy;
  Inherited;
end;

procedure TMessageHandler.AddListener(AListener : IMessageListener);
begin
  Listeners.Add(AListener);
end;

procedure TMessageHandler.RemoveListener(AListener : IMessageListener);
begin
  Listeners.Remove(AListener);
end;

procedure TMessageHandler.SendMessage(AMessage : TMessage);
begin
  Message := AMessage;
  NotifyListeners;
end;

{ TMessage }


constructor TMessage.Create(ATyp : TMessageType; ABody : TMessageBody);
begin
  Typ := ATyp;
  Body := ABody;
end;

destructor TMessage.Destroy;
begin
  Body.Destroy;
  inherited;
end;

{ TSourceLineMsg }

constructor TSourceLineMsg.Create(ALine : string; ALineNum : LongInt);
begin
  Line := ALine;
  LineNum := ALineNum;
end;

{ TParserSummaryMsg }

constructor TParserSummaryMsg.Create(ALines: integer; ACount: integer;
  ATime: LongWord);
begin
  NumSourceLines := ALines;
  ErrorCount := ACount;
  ElapsedTime := ATime;
end;

{ TCompilerSummaryMsg }

constructor TCompilerSummaryMsg.Create(AInstructionCount: integer;
  AElapsedTime: LongWord);
begin
  InstructionCount := AInstructionCount;
  ElapsedTime := AElapsedTime;
end;

{ TInterpreterSummaryMsg }

constructor TInterpreterSummaryMsg.Create(AExecutionCount: integer;
  ARuntimeErrors: integer; AElapsedTime: LongWord);
begin
  ExecutionCount := AExecutionCount;
  RuntimeErrors := ARuntimeErrors;
  ElapsedTime := AElapsedTime;
end;

{ TSyntaxErrorMsg }

constructor TSyntaxErrorMsg.Create(ASourceLineNumber : integer;
  AStartPosition : integer; ATokenText : string; ASyntaxErrorMsg : string);
begin
  SourceLineNumber := ASourceLineNumber;
  StartPosition   := AStartPosition;
  TokenText       := ATokenText;
  SyntaxErrorMsg  := ASyntaxErrorMsg;
end;

{ TTokenMsg }

constructor TTokenMsg.Create(ASourceLineNumber: integer;
  AStartPosition: integer; ATokenTyp: ITokenTyp; ATokenText: string;
  ATokenValue: TAnyObject);
begin
  SourceLineNumber := ASourceLineNumber;
  StartPosition := AStartPosition;
  TokenTyp := ATokenTyp;
  TokenText := ATokenText;
  TokenValue := ATokenValue;
end;



end.

