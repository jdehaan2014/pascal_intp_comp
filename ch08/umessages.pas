unit umessages;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, utokentype, AnyObject, fgl;


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
  end;

  IMessageListener = interface(IAnyInterface)
    ['IMessageListener']
    procedure MessageReceived(Message: TMessage); // called to receive a msg sent by a msgproduced
  end;

  IMessageProducer = interface(IAnyInterface)
    ['IMessageProducer']
    procedure addMessageListener(Listener: IMessageListener);  //Add a listener to the listener list
    procedure removeMessageListener(Listener: IMessageListener);  //Remove a listener from the listener list
    procedure sendMessage(AMessage: TMessage);           //Notify listeners after setting the message
  end;

  TListeners = specialize TFPGList<IMessageListener>;


  { TMessageHandler }

  TMessageHandler = class
    private
      Message: TMessage;
      Listeners: TListeners;
      procedure NotifyListeners;
    public
      constructor Create;
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

  { TAssignMsg }

  TAssignMsg = class(TMessageBody)
    SourceLineNumber: integer;
    TargetVariableName: string;
    ExpressionValue: TObject;
    constructor Create(ASourceLineNumber: integer; ATargetVariableName: string;
      AExpressionValue: TObject);
  end;

  { TParserSummaryMsg }

  TParserSummaryMsg = class(TMessageBody)
    NumSourceLines: integer;
    ErrorCount: integer;
    ElapsedTime: Double;
    constructor Create(ALines: integer; ACount: integer; AElapsedTime: Double);
  end;

  { TCompilerSummaryMsg }

  TCompilerSummaryMsg = class(TMessageBody)
    InstructionCount: integer;
    ElapsedTime: Double;
    constructor Create(AInstructionCount: integer; AElapsedTime: Double);
  end;

  { TInterpreterSummaryMsg }

  TInterpreterSummaryMsg = class(TMessageBody)
    ExecutionCount: integer;
    RuntimeErrors: integer;
    ElapsedTime: Double;
    constructor Create(AExecutionCount: integer; ARuntimeErrors: integer;
      AElapsedTime: Double);
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

  { TRuntimeErrorMsg }

  TRuntimeErrorMsg = class(TMessageBody)
    RuntimeErrorMsg: string;
    SourceLineNumber: integer;
    constructor Create(ARuntimeErrorMsg: string; ASourceLineNumber: integer);
  end;

  { TTokenMsg }

  TTokenMsg = class(TMessageBody)
    SourceLineNumber: integer;
    StartPosition: integer;
    TokenTyp: ITokenTyp;
    TokenText: string;
    TokenValue: TObject;
    constructor Create(ASourceLineNumber: integer; AStartPosition: integer; ATokenTyp: ITokenTyp;
      ATokenText: string; ATokenValue: TAnyObject);
    destructor Destroy; override;
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


{ TSourceLineMsg }

constructor TSourceLineMsg.Create(ALine : string; ALineNum : LongInt);
begin
  Line := ALine;
  LineNum := ALineNum;
end;


{ TAssignMsg }

constructor TAssignMsg.Create(ASourceLineNumber: integer;
  ATargetVariableName: string; AExpressionValue: TObject);
begin
  SourceLineNumber := ASourceLineNumber;
  TargetVariableName := ATargetVariableName;
  ExpressionValue := AExpressionValue;
end;


{ TParserSummaryMsg }

constructor TParserSummaryMsg.Create(ALines: integer; ACount: integer;
  AElapsedTime: Double);
begin
  NumSourceLines := ALines;
  ErrorCount := ACount;
  ElapsedTime := AElapsedTime;
end;

{ TCompilerSummaryMsg }

constructor TCompilerSummaryMsg.Create(AInstructionCount: integer;
  AElapsedTime: Double);
begin
  InstructionCount := AInstructionCount;
  ElapsedTime := AElapsedTime;
end;

{ TInterpreterSummaryMsg }

constructor TInterpreterSummaryMsg.Create(AExecutionCount: integer;
  ARuntimeErrors: integer; AElapsedTime: Double);
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
  StartPosition    := AStartPosition;
  TokenText        := ATokenText;
  SyntaxErrorMsg   := ASyntaxErrorMsg;
end;

{ TRuntimeErrorMsg }

constructor TRuntimeErrorMsg.Create(ARuntimeErrorMsg: string;
  ASourceLineNumber: integer);
begin
  RuntimeErrorMsg   := ARuntimeErrorMsg;
  SourceLineNumber := ASourceLineNumber;
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

destructor TTokenMsg.Destroy;
begin
  if assigned(TokenValue) then TokenValue.Free;
  inherited;
end;



end.

