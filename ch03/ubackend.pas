unit ubackend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umessages, usymtab, ucode;

type

  { TBackEnd }

  TBackEnd = class(TInterfacedObject, IMessageProducer)
    protected
      class var
        MessageHandler: TMessageHandler;  // message handler delegate
      var
        Symtab: ISymtab;                //  symbol table
        ICode: IIntermediateCode;       // intermediate code
    public
      class constructor Create;                // must be first in definition ??? bug???

      procedure Process(AICode: IIntermediateCode; ASymtab: ISymtab); virtual; abstract;

      function getICode: IIntermediateCode;
      function getSymtab: ISymtab;
      function getMessageHandler: TMessageHandler;

      procedure addMessageListener(Listener: IMessageListener);   //Add a parser message listener.
      procedure removeMessageListener(Listener: IMessageListener);  //Remove a parser message listener
      procedure sendMessage(Message: TMessage);                    //Notify listeners after setting the message
  end;

implementation

{ TBackEnd }

class constructor TBackEnd.Create;
begin
  MessageHandler := TMessageHandler.Create;
end;

function TBackEnd.getICode : IIntermediateCode;
begin
  Result := ICode;
end;

function TBackEnd.getSymtab : ISymtab;
begin
  Result := Symtab;
end;

function TBackEnd.getMessageHandler : TMessageHandler;
begin
  Result := MessageHandler;
end;

procedure TBackEnd.addMessageListener(Listener : IMessageListener);
begin
  MessageHandler.addListener(Listener);
end;

procedure TBackEnd.removeMessageListener(Listener : IMessageListener);
begin
  MessageHandler.removeListener(Listener);
end;

procedure TBackEnd.sendMessage(Message : TMessage);
begin
  MessageHandler.sendMessage(Message);
end;


end.

