unit ucode;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

uses
  Classes, SysUtils, AnyObject, fgl, uGenericInterfaces;

type

  ICodeNode = interface;           //forward decl


  //The framework interface that represents the intermediate code.
  IIntermediateCode = interface(IAnyInterface)
    ['IIntermediateCode']
    Function setRoot(ANode: ICodeNode): ICodeNode;  //Set and return the root node.
    Function getRoot: ICodeNode;        //Get the root node.
  end;

  ICodeNodeType = Interface(IAnyInterface)
    ['ICodeNodeType']
  end;

  ICodeKey = Interface(IAnyInterface)
    ['ICodeKey']
  end;

  TICodeNodeList = specialize TFPGList<ICodeNode>;

  //The interface for a node of the intermediate code.
  ICodeNode = interface (IAnyInterface)
    ['ICodeNode']
    Function getType: ICodeNodeType;         //return the node type.
    Function getParent: ICodeNode;           //Return the parent of this node.
    procedure setTypeSpec(TypeSpec: ITypeSpec); //set the type spec of this node
    function getTypeSpec: ITypeSpec;         //Return the type spec of this node
    Function AddChild(ANode: ICodeNode): ICodeNode;  //Add a child node.
    function getChildren: TICodeNodeList;  //Return an array list of this node's children.
    procedure setAttribute(AKey: ICodeKey; Value: TObject);  //Set a node attribute.
    function getAttribute(AKey: ICodeKey): TObject;  //Get the value of a node attribute.
    function CopyNode: ICodeNode;  //Make a copy of this node.
  end;


implementation

end.

