unit usource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umessages; //, uObjectUtils;

const
  FileEnding = #3; //on Unix '^D';   // on windows ^Z    #26

type

  { TMyReader }

  TMyReader = class
   private
    FFileName: string;
    FContent: TStringList;
    FIndex: integer;
   public
    property FileName: string read FFilename;
    constructor Create(AFileName: string);
    destructor Destroy;
    function ReadLine: string;
  end;

  { TInputReader }

  TInputReader = class(THandleStream)
    public
      constructor Create(AHandle: THandle);
      function ReadLine: String;
  end;

{ TSource }

  TSource = class(TInterfacedObject, IMessageProducer)
   private
    Reader: TMyReader;
    FLine : string;
    FLineNum,
    FCurrentPos: integer;
    MessageHandler: TMessageHandler;  // message handler delegate
    procedure ReadLine;  // read the next source line
   public
    property Line: string read FLine;
    property LineNum: integer read FLineNum;
    property CurrentPos: integer read FCurrentPos;
    Constructor Create(AReader: TMyReader);
    Destructor Destroy; override;
    function CurrentChar: char;
    function NextChar: char;
    function PeekChar : char;
    procedure addMessageListener(Listener : IMessageListener);
    procedure removeMessageListener(Listener : IMessageListener);
    procedure sendMessage(Message : TMessage);
  end;

implementation

{ TInputReader }

constructor TInputReader.Create(AHandle: THandle);
begin
  Inherited Create(AHandle);
end;

function TInputReader.ReadLine: String;
begin

end;

{ TMyReader }

constructor TMyReader.Create(AFileName : string);
begin
   try
    FFileName := AFileName;
    FContent := TStringList.Create;
    FContent.LoadFromFile(FFileName);
    FIndex := 0;
   except
    if assigned(FContent) then FContent.Free;
    raise Exception.Create('Error in loading source file')
   end;
end;

destructor TMyReader.Destroy;
begin
  if assigned(FContent) then FContent.Free;
  inherited;
end;

function TMyReader.ReadLine : string;
begin
  if FIndex < FContent.Count then begin
    Result := FContent[FIndex];
    Inc(FIndex);
  end
  else Result := FileEnding;
end;

{ TSource }

constructor TSource.Create(AReader: TMyReader);
begin
  FLineNum := 0;
  FCurrentPos := -1;
  Reader := AReader;
  MessageHandler := TMessageHandler.Create;
end;

destructor TSource.Destroy;
begin
  if assigned(Reader) then Reader.Destroy;
  if assigned(MessageHandler) then MessageHandler.Destroy;
  inherited Destroy;
end;


function TSource.CurrentChar: char;
begin
  if FCurrentPos = -1 then begin        // First time?
    ReadLine;
    Result := NextChar;
  end
  else if FLine[1] = FileEnding then          // at end of file?
    Result := FileEnding
  else if (FCurrentPos = 0) or (FCurrentPos = Length(FLine)) then
    Result := LineEnding
  else if FCurrentPos > Length(FLine) then begin
    ReadLine;
    Result := NextChar;
  end
  else Result := FLine[FCurrentPos];
end;

function TSource.NextChar : char;  //consume current source char and return next char
begin
  Inc(FCurrentPos);
  Result := CurrentChar;
end;

function TSource.PeekChar: char;  //peek at next character, but don't process it
var
  NextPos : integer;
begin
  CurrentChar;
  if FLine[1] = FileEnding then
    Exit(FileEnding);
  NextPos := FCurrentPos + 1;
  if NextPos < Length(FLine) then
    Result := FLine[NextPos]
  else
    Result := LineEnding;
end;

procedure TSource.ReadLine;
begin
  FLine := Reader.ReadLine;
  FCurrentPos := 0;
  if FLine = '' then
    FLine += #13;
  if FLine[1] <> FileEnding then begin
    Inc(FLineNum);
    SendMessage(TMessage.Create(mtSourceLine, TSourceLineMsg.Create(FLine, FLineNum)));
  end;
end;


procedure TSource.addMessageListener(Listener : IMessageListener);
begin
  MessageHandler.addListener(Listener);
end;

procedure TSource.removeMessageListener(Listener : IMessageListener);
begin
  MessageHandler.removeListener(Listener);
end;

procedure TSource.sendMessage(Message : TMessage);
begin
  MessageHandler.sendMessage(Message);
end;


end.

