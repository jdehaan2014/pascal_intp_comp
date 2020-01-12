unit usource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umessages, AnyObject, uTextReader, uObjectUtils;

const
  FileEnding = #3; //on Unix '^D';   // on windows ^Z    #26

type

{ TSource }

  TSource = class(TAnyObject, IMessageProducer)
   private
    FReader: TTextReader;
    FLine : TString;
    FLineNum,
    FCurrentPos: integer;
    MessageHandler: TMessageHandler;  // message handler delegate
    procedure ReadLine;  // read the next source line
   public
    property Line: TString read FLine;
    property LineNum: integer read FLineNum;
    property CurrentPos: integer read FCurrentPos;
    property Reader: TTextReader read FReader;
    constructor Create(AReader: TTextReader);
    destructor Destroy; override;
    function CurrentChar: char;
    function NextChar: char;
    function PeekChar : char;
    function atEol: boolean;
    function atEof: boolean;
    procedure SkipToNextLine;
    procedure addMessageListener(Listener : IMessageListener);
    procedure removeMessageListener(Listener : IMessageListener);
    procedure sendMessage(Message : TMessage);
  end;

implementation

{ TSource }

constructor TSource.Create(AReader: TTextReader);
begin
  FLineNum := 0;
  FCurrentPos := -1;
  FReader := AReader;
  MessageHandler := TMessageHandler.Create;
end;

destructor TSource.Destroy;
begin
  FReader.Free;
  MessageHandler.Free;
  inherited;
end;

//Return the source character at the current position
function TSource.CurrentChar: char;
begin
  if FCurrentPos = -1 then begin        // First time?
    ReadLine;
    Exit(NextChar);
  end
  else if FLine = Nil then          // at end of file?
    Exit(FileEnding)
  else if (FCurrentPos = 0) or (FCurrentPos = FLine.Length) then
    Exit(LineEnding)
  else if FCurrentPos > FLine.Length then begin
    ReadLine;
    Exit(NextChar);
  end
  else Exit(FLine.CharAt(FCurrentPos));
end;

//consume the current source character and return the next
function TSource.NextChar : char;
begin
  Inc(FCurrentPos);
  Result := CurrentChar;
end;

//return the source char following the current char without consuming it
function TSource.PeekChar: char;
var
  NextPos : integer;
begin
  CurrentChar;
  if FLine = Nil then
    Exit(FileEnding);
  NextPos := FCurrentPos + 1;
  if NextPos < FLine.Length then
    Result := FLine.CharAt(NextPos)
  else
    Result := LineEnding;
end;

//return true at the end of the line else return false
function TSource.atEol: boolean;
begin
  Result := (FLine <> Nil) and (FCurrentPos = FLine.Length);
end;

//return true at the end of the file else return false
function TSource.atEof: boolean;
begin
  //first time?
  if FCurrentPos = -1 then
    ReadLine;
  Result := FLine = Nil;
end;

//skip the rest of the current input line
procedure TSource.SkipToNextLine;
begin
  if FLine <> Nil then
    FCurrentPos := FLine.Length + 1;
end;

//Read the next source line
procedure TSource.ReadLine;
begin
  FLine := FReader.ReadLine;  //  At end of file FLine = Nil
  FCurrentPos := 0;
  if FLine <> Nil then begin
    Inc(FLineNum);
    SendMessage(TMessage.Create(
      mtSourceLine, TSourceLineMsg.Create(FLine, FLineNum)));
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

