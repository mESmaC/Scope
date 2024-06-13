program Parser;

uses
  SysUtils, Classes, Lexer;

type
  TASTNode = class
  end;

  TProgramNode = class(TASTNode)
    Name: string;
    Uses: array of string;
    Block: TASTNode;
    constructor Create(AName: string; AUses: array of string; ABlock: TASTNode);
  end;

  TBlockNode = class(TASTNode)
    Statements: array of TASTNode;
    constructor Create(AStatements: array of TASTNode);
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
    constructor Create(AName: string);
  end;

constructor TProgramNode.Create(AName: string; AUses: array of string; ABlock: TASTNode);
begin
  Name := AName;
  Uses := AUses;
  Block := ABlock;
end;

constructor TBlockNode.Create(AStatements: array of TASTNode);
begin
  Statements := AStatements;
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

constructor TExprStmtNode.Create(AName: string);
begin
  Name := AName;
end;

type
  TParser = class
  private
    FLexer: TLexer;
    FCurrentToken: TToken;
    procedure Eat(ATokenType: TTokenType);
    function ParseProgram: TProgramNode;
    function ParseUsesClause: array of string;
    function ParseBlock: TBlockNode;
    function ParseStatement: TASTNode;
    function ParseProcDecl: TProcDeclNode;
    function ParseFuncDecl: TFuncDeclNode;
    function ParseExprStmt: TExprStmtNode;
  public
    constructor Create(ALexer: TLexer);
    function Parse: TProgramNode;
  end;

constructor TParser.Create(ALexer: TLexer);
begin
  FLexer := ALexer;
  FCurrentToken := FLexer.GetNextToken;
end;

procedure TParser.Eat(ATokenType: TTokenType);
begin
  if FCurrentToken.TokenType = ATokenType then
    FCurrentToken := FLexer.GetNextToken
  else
    raise Exception.CreateFmt('Syntax error: expected %s but found %s', [GetEnumName(TypeInfo(TTokenType), Ord(ATokenType)), FCurrentToken.Lexeme]);
end;

function TParser.ParseProgram: TProgramNode;
var
  Name: string;
  Uses: array of string;
  Block: TBlockNode;
begin
  Eat(TOK_PROGRAM);
  Name := FCurrentToken.Lexeme;
  Eat(TOK_IDENTIFIER);
  Eat(TOK_SEMICOLON);
  if FCurrentToken.TokenType = TOK_USES then
    Uses := ParseUsesClause;
  Block := ParseBlock;
  Eat(TOK_DOT);
  Result := TProgramNode.Create(Name, Uses, Block);
end;

function TParser.ParseUsesClause: array of string;
var
  UsesList: array of string;
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
  Statements: array of TASTNode;
  Statement: TASTNode;
begin
  Eat(TOK_INIT);
  Eat(TOK_LBRACE);
  SetLength(Statements, 0);
  while FCurrentToken.TokenType <> TOK_RBRACE do
  begin
    Statement := ParseStatement;
    SetLength(Statements, Length(Statements) + 1);
    Statements[High(Statements)] := Statement;
  end;
  Eat(TOK_RBRACE);
  Result := TBlockNode.Create(Statements);
end;

function TParser.ParseStatement: TASTNode;
begin
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
begin
  Name := FCurrentToken.Lexeme;
  Eat(TOK_IDENTIFIER);
  Eat(TOK_LPAREN);
  Eat(TOK_RPAREN);
  Eat(TOK_SEMICOLON);
  Result := TExprStmtNode.Create(Name);
end;

function TParser.Parse: TProgramNode;
begin
  Result := ParseProgram;
  if FCurrentToken.TokenType <> TOK_EOF then
    raise Exception.Create('Syntax error: unexpected token');
end;

end.
