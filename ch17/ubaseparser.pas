unit ubaseparser;

{$mode objfpc}{$H+}


interface
uses
  Classes, Sysutils, ubasescanner, utoken, umessages, uGenericInterfaces,
  uSymTabFactory, AnyObject;

type

  { TBaseParser }

  TBaseParser = class(TAnyObject, IMessageProducer)
    protected
      class var
        SymTabStack: ISymTabStack;        // symbol table stack
        MessageHandler: TMessageHandler;  // message handler delegate
      var
        Scanner: TBaseScanner;              // scanner used with this parser
    public
      constructor Create(AScanner: TBaseScanner);
      destructor Destroy; override;

      function getScanner: TBaseScanner ;

      function getSymTabStack: ISymTabStack;
      function getMessageHandler: TMessageHandler;

      function currentToken: TToken;
      function nextToken: TToken;

      Function getErrorCount: integer;   virtual; abstract;
      procedure Parse; virtual; abstract;

      procedure addMessageListener(Listener: IMessageListener);   //Add a parser message listener.
      procedure removeMessageListener(Listener: IMessageListener);  //Remove a parser message listener
      procedure sendMessage(Message: TMessage);                    //Notify listeners after setting the message
  end;



implementation

{ TBaseParser }


constructor TBaseParser.Create(AScanner : TBaseScanner);
begin
  inherited Create;
  Scanner := AScanner;
end;

destructor TBaseParser.Destroy;
begin
  Scanner.Free;
  Inherited;
end;

function TBaseParser.getScanner: TBaseScanner;
begin
  Result :=  Scanner;
end;

function TBaseParser.getSymTabStack: ISymTabStack;
begin
  Result := symTabStack;
end;

function TBaseParser.getMessageHandler: TMessageHandler;
begin
  Result := MessageHandler;
end;

function TBaseParser.currentToken: TToken;
begin
  result := Scanner.currentToken;
end;

function TBaseParser.nextToken: TToken;
begin
  result := Scanner.nextToken;
end;

procedure TBaseParser.addMessageListener(Listener : IMessageListener);
begin
  MessageHandler.addListener(Listener);
end;

procedure TBaseParser.removeMessageListener(Listener : IMessageListener);
begin
  MessageHandler.removeListener(Listener);
end;

procedure TBaseParser.sendMessage(Message : TMessage);
begin
  MessageHandler.sendMessage(Message);
end;

initialization
  TBaseParser.SymTabStack := TSymtabFactory.CreateSymTabStack;
  TBaseParser.MessageHandler := TMessageHandler.Create;

finalization
  TBaseParser.MessageHandler.Free;

end.

