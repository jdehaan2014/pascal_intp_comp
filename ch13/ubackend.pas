unit ubackend;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umessages, uGenericInterfaces, ucode, AnyObject;

type

  { TBackEnd }

  TBackEnd = class(TAnyObject, IMessageProducer)
    protected
      class var
        MessageHandler: TMessageHandler;  // message handler delegate
        SymTabStack: ISymtabStack;        //  symbol table stack
      var
        ICode: IIntermediateCode;       // intermediate code
    public
      //class constructor Create;         // must be first in definition ??? bug???

      procedure Process(AICode: IIntermediateCode;
        ASymtabStack: ISymtabStack); virtual; abstract;

      function getICode: IIntermediateCode;
      function getSymTabStack: ISymTabStack;
      function getMessageHandler: TMessageHandler;

      procedure addMessageListener(Listener: IMessageListener);   //Add a parser message listener.
      procedure removeMessageListener(Listener: IMessageListener);  //Remove a parser message listener
      procedure sendMessage(Message: TMessage);                    //Notify listeners after setting the message
  end;

implementation

{ TBackEnd }

{class constructor TBackEnd.Create;
begin
  MessageHandler := TMessageHandler.Create;
end; }

function TBackEnd.getICode : IIntermediateCode;
begin
  Result := ICode;
end;

function TBackEnd.getSymTabStack : ISymTabStack;
begin
  Result := SymTabStack;
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


initialization
  TBackEnd.MessageHandler := TMessageHandler.Create;
finalization
  TBackEnd.MessageHandler.Free;
end.

