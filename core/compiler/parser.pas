unit Parser;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Lexer, TypInfo;

type
  TASTNode = class
  end;

  TStringArray = array of string;
  TASTNodeArray = array of TASTNode;


  TVarDeclNode = class(TASTNode)
    Name: string;
    VarType: string;
    IsMutable: boolean;
    constructor Create(AName, AVarType: string; AMutable: boolean);
  end;

  TProgramNode = class(TASTNode)
    Name: string;
    UsedUnits: TStringArray;
    Block: TASTNode;
    constructor Create(AName: string; AUsedUnits: TStringArray; ABlock: TASTNode);
  end;

  TBlockNode = class(TASTNode)
    Statements: TASTNodeArray;
    constructor Create(AStatements: TASTNodeArray);
  end;

  TProcDeclNode = class(TASTNode)
    Name: string;
    Body: TBlockNode;
    constructor Create(AName: string; ABody: TBlockNode);
  end;

  TFuncDeclNode = class(TASTNode)
    Name: string;
    Body: TBlockNode;
    constructor Create(AName: string; ABody: TBlockNode);
  end;

  TExprStmtNode = class(TASTNode)
    Name: string;
    Args: TStringArray;
    constructor Create(AName: string; AArgs: TStringArray);
  end;

  TParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;
    procedure Eat(ATokenType: TTokenType);
    function ParseProgram: TProgramNode;
    function ParseUsesClause: TStringArray;
    function ParseBlock: TBlockNode;
    function ParseStatement: TASTNode;
    function ParseProcDecl: TProcDeclNode;
    function ParseFuncDecl: TFuncDeclNode;
    function ParseExprStmt: TExprStmtNode;
    function ParseVarDecl: TVarDeclNode; // Add this declaration
  public
    constructor Create(ALexer: TLexer);
    function Parse: TProgramNode;
    procedure AddFile(const AFilename: string);
  end;

implementation

constructor TProgramNode.Create(AName: string; AUsedUnits: TStringArray; ABlock: TASTNode);
begin
  Name := AName;
  UsedUnits := AUsedUnits;
  Block := ABlock;
end;

constructor TBlockNode.Create(AStatements: TASTNodeArray);
begin
  Statements := AStatements;
end;

constructor TVarDeclNode.Create(AName, AVarType: string; AMutable: boolean);
begin
  Name := AName;
  VarType := AVarType;
  IsMutable := AMutable;
end;

constructor TProcDeclNode.Create(AName: string; ABody: TBlockNode);
begin
  Name := AName;
  Body := ABody;
end;

constructor TFuncDeclNode.Create(AName: string; ABody: TBlockNode);
begin
  Name := AName;
  Body := ABody;
end;

constructor TExprStmtNode.Create(AName: string; AArgs: TStringArray);
begin
  Name := AName;
  Args := AArgs;
end;

constructor TParser.Create(ALexer: TLexer);
begin
  FLexer := ALexer;
  FCurrentToken := FLexer.GetNextToken;
  WriteLn('Initial token: ', FCurrentToken.Lexeme, ' (', Ord(FCurrentToken.TokenType), ')');
end;

procedure TParser.Eat(ATokenType: TTokenType);
begin
  WriteLn('Eating token: ', FCurrentToken.Lexeme, ' (', Ord(FCurrentToken.TokenType), ')');
  if FCurrentToken.TokenType = ATokenType then
  begin
    FCurrentToken := FLexer.GetNextToken;
    WriteLn('Next token: ', FCurrentToken.Lexeme, ' (', Ord(FCurrentToken.TokenType), ')');
  end
  else
  begin
    raise Exception.CreateFmt('Syntax error: expected %s but found %s at line %d, column %d', 
      [GetEnumName(TypeInfo(TTokenType), Ord(ATokenType)), 
       GetEnumName(TypeInfo(TTokenType), Ord(FCurrentToken.TokenType)), 
       FCurrentToken.Line, FCurrentToken.Column]);
  end;
end;

function TParser.ParseVarDecl: TVarDeclNode;
var
  IsMutable: boolean;
  VarName, VarType: string;
begin
  IsMutable := False;
  if FCurrentToken.TokenType = TOK_MUT then
  begin
    IsMutable := True;
    Eat(TOK_MUT);
  end;
  VarName := FCurrentToken.Lexeme;
  Eat(TOK_IDENTIFIER);
  Eat(TOK_COLON);
  VarType := FCurrentToken.Lexeme;
  Eat(TOK_IDENTIFIER);
  Eat(TOK_SEMICOLON);
  Result := TVarDeclNode.Create(VarName, VarType, IsMutable);
end;

function TParser.ParseProgram: TProgramNode;
var
  Name: string;
  UsedUnits: TStringArray;
  Block: TBlockNode;
begin
  WriteLn('Parsing program...');
  WriteLn('Current token: ', FCurrentToken.Lexeme, ' (', Ord(FCurrentToken.TokenType), ')');
  
  Eat(TOK_PROGRAM);
  WriteLn('After TOK_PROGRAM: ', FCurrentToken.Lexeme, ' (', Ord(FCurrentToken.TokenType), ')');
  
  Name := FCurrentToken.Lexeme;
  Eat(TOK_IDENTIFIER);
  WriteLn('After TOK_IDENTIFIER: ', FCurrentToken.Lexeme, ' (', Ord(FCurrentToken.TokenType), ')');
  
  Eat(TOK_SEMICOLON);
  WriteLn('After TOK_SEMICOLON: ', FCurrentToken.Lexeme, ' (', Ord(FCurrentToken.TokenType), ')');
  
  if FCurrentToken.TokenType = TOK_USES then
  begin
    WriteLn('Parsing uses clause...');
    UsedUnits := ParseUsesClause;
  end
  else
  begin
    WriteLn('No uses clause found.');
    SetLength(UsedUnits, 0); // Initialize UsedUnits if no uses clause
  end;
  
  WriteLn('Parsing block...');
  Block := ParseBlock;
  WriteLn('After parsing block.');
  
  Eat(TOK_DOT);
  WriteLn('After TOK_DOT: ', FCurrentToken.Lexeme, ' (', Ord(FCurrentToken.TokenType), ')');
  
  Result := TProgramNode.Create(Name, UsedUnits, Block);
  WriteLn('Program node created.');
end;

function TParser.ParseUsesClause: TStringArray;
var
  UsesList: TStringArray;
begin
  Eat(TOK_USES);
  SetLength(UsesList, 0);
  repeat
    SetLength(UsesList, Length(UsesList) + 1);
    UsesList[High(UsesList)] := FCurrentToken.Lexeme;
    Eat(TOK_IDENTIFIER);
    if FCurrentToken.TokenType = TOK_COMMA then
      Eat(TOK_COMMA);
  until FCurrentToken.TokenType <> TOK_IDENTIFIER;
  Eat(TOK_SEMICOLON);
  Result := UsesList;
end;

function TParser.ParseBlock: TBlockNode;
var
  Statements: TASTNodeArray;
  Statement: TASTNode;
begin
  Eat(TOK_INIT);
  Eat(TOK_LBRACE);
  SetLength(Statements, 0);
  while FCurrentToken.TokenType <> TOK_RBRACE do
  begin
    if FCurrentToken.TokenType = TOK_VAR then
    begin
      Eat(TOK_VAR);
      Statement := ParseVarDecl;
    end
    else
      Statement := ParseStatement;
    SetLength(Statements, Length(Statements) + 1);
    Statements[High(Statements)] := Statement;
  end;
  Eat(TOK_RBRACE);
  Result := TBlockNode.Create(Statements);
end;

function TParser.ParseStatement: TASTNode;
begin
  WriteLn('Parsing statement: ', FCurrentToken.Lexeme, ' (', Ord(FCurrentToken.TokenType), ')');
  case FCurrentToken.TokenType of
    TOK_PROC: Result := ParseProcDecl;
    TOK_FUNC: Result := ParseFuncDecl;
    TOK_IDENTIFIER: Result := ParseExprStmt;
  else
    raise Exception.Create('Syntax error: unexpected token');
  end;
end;

function TParser.ParseProcDecl: TProcDeclNode;
var
  Name: string;
  Body: TBlockNode;
begin
  Eat(TOK_PROC);
  Name := FCurrentToken.Lexeme;
  Eat(TOK_IDENTIFIER);
  Eat(TOK_LPAREN);
  Eat(TOK_RPAREN);
  Eat(TOK_LBRACE);
  Body := ParseBlock;
  Eat(TOK_RBRACE);
  Result := TProcDeclNode.Create(Name, Body);
end;

function TParser.ParseFuncDecl: TFuncDeclNode;
var
  Name: string;
  Body: TBlockNode;
begin
  Eat(TOK_FUNC);
  Name := FCurrentToken.Lexeme;
  Eat(TOK_IDENTIFIER);
  Eat(TOK_LPAREN);
  Eat(TOK_RPAREN);
  Eat(TOK_LBRACE);
  Body := ParseBlock;
  Eat(TOK_RBRACE);
  Result := TFuncDeclNode.Create(Name, Body);
end;

function TParser.ParseExprStmt: TExprStmtNode;
var
  Name: string;
  Args: TStringArray;
begin
  Name := FCurrentToken.Lexeme;
  Eat(TOK_IDENTIFIER);
  
  if FCurrentToken.TokenType = TOK_LPAREN then
  begin
    Eat(TOK_LPAREN);
    SetLength(Args, 0);
    
    // Handle string literals within the parentheses
    while FCurrentToken.TokenType <> TOK_RPAREN do
    begin
      if FCurrentToken.TokenType = TOK_STRING then
      begin
        SetLength(Args, Length(Args) + 1);
        Args[High(Args)] := FCurrentToken.Lexeme;
        Eat(TOK_STRING);
      end
      else
        raise Exception.Create('Syntax error: expected string literal');
    end;
    
    Eat(TOK_RPAREN);
  end;
  
  Eat(TOK_SEMICOLON);
  Result := TExprStmtNode.Create(Name, Args);
end;

function TParser.Parse: TProgramNode;
begin
  Result := ParseProgram;
  if FCurrentToken.TokenType <> TOK_EOF then
    raise Exception.Create('Syntax error: unexpected token');
end;

procedure TParser.AddFile(const AFilename: string);
begin
  FLexer.AddFile(AFilename);
end;

end.