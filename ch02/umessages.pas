unit umessages;

{$mode objfpc}{$H+}
//{$interfaces corba}

interface

uses
  Classes, SysUtils;

const
  StupidInterface = '{3FB19775-F5FA-464C-B10C-D8137D742088}';


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
      function getTyp: TMessageType;
      function getBody: TMessageBody;
      constructor Create(ATyp: TMessageType; ABody: TMessageBody);
      destructor Destroy;
  end;

  IMessageListener = interface
    [StupidInterface]
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
    ElapsedTime: real;
    constructor Create(ALines: integer; ACount: integer; ATime: real);
  end;

  { TCompilerSummaryMsg }

  TCompilerSummaryMsg = class(TMessageBody)
    InstructionCount: integer;
    ElapsedTime: real;
    constructor Create(AInstructionCount: integer; AElapsedTime: real);
  end;

  { TInterpreterSummaryMsg }

  TInterpreterSummaryMsg = class(TMessageBody)
    ExecutionCount: integer;
    RuntimeErrors: integer;
    ElapsedTime: real;
    constructor Create(AExecutionCount: integer; ARuntimeErrors: integer; AElapsedTime: real);
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

function TMessage.getTyp : TMessageType;
begin
  result := Typ;
end;

function TMessage.getBody : TMessageBody;
begin
  result := Body;
end;

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

constructor TParserSummaryMsg.Create(ALines : integer; ACount : integer; ATime : real);
begin
  NumSourceLines := ALines;
  ErrorCount := ACount;
  ElapsedTime := ATime;
end;

{ TCompilerSummaryMsg }

constructor TCompilerSummaryMsg.Create(AInstructionCount : integer;
  AElapsedTime : real);
begin
  InstructionCount := AInstructionCount;
  ElapsedTime := AElapsedTime;
end;

{ TInterpreterSummaryMsg }

constructor TInterpreterSummaryMsg.Create(AExecutionCount : integer;
  ARuntimeErrors : integer; AElapsedTime : real);
begin
  ExecutionCount := AExecutionCount;
  RuntimeErrors := ARuntimeErrors;
  ElapsedTime := AElapsedTime;
end;



end.

