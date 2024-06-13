program Compiler;

uses
  SysUtils, Classes, Lexer, Parser;

type
  TCodeGenerator = class
  private
    FOutput: TStringList;
    procedure GenerateProgram(Node: TProgramNode);
    procedure GenerateBlock(Node: TBlockNode);
    procedure GenerateProcDecl(Node: TProcDeclNode);
    procedure GenerateFuncDecl(Node: TFuncDeclNode);
    procedure GenerateExprStmt(Node: TExprStmtNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Generate(Node: TASTNode; const AOutputFile: string);
  end;

constructor TCodeGenerator.Create;
begin
  FOutput := TStringList.Create;
end;

destructor TCodeGenerator.Destroy;
begin
  FOutput.Free;
  inherited;
end;

procedure TCodeGenerator.Generate(Node: TASTNode; const AOutputFile: string);
begin
  if Node is TProgramNode then
    GenerateProgram(TProgramNode(Node))
  else
    raise Exception.Create('Unknown AST node type');

  FOutput.SaveToFile(AOutputFile);
end;

procedure TCodeGenerator.GenerateProgram(Node: TProgramNode);
var
  I: Integer;
begin
  FOutput.Add('program ' + Node.Name + ';');
  FOutput.Add('');

  if Length(Node.Uses) > 0 then
  begin
    FOutput.Add('uses');
    for I := 0 to High(Node.Uses) do
    begin
      FOutput.Add('  ' + Node.Uses[I] + IfThen(I < High(Node.Uses), ',', ';'));
    end;
    FOutput.Add('');
  end;

  GenerateBlock(TBlockNode(Node.Block));
  FOutput.Add('.');
end;

procedure TCodeGenerator.GenerateBlock(Node: TBlockNode);
var
  I: Integer;
begin
  FOutput.Add('begin');
  for I := 0 to High(Node.Statements) do
  begin
    if Node.Statements[I] is TProcDeclNode then
      GenerateProcDecl(TProcDeclNode(Node.Statements[I]))
    else if Node.Statements[I] is TFuncDeclNode then
      GenerateFuncDecl(TFuncDeclNode(Node.Statements[I]))
    else if Node.Statements[I] is TExprStmtNode then
      GenerateExprStmt(TExprStmtNode(Node.Statements[I]))
    else
      raise Exception.Create('Unknown statement type');
  end;
  FOutput.Add('end');
end;

procedure TCodeGenerator.GenerateProcDecl(Node: TProcDeclNode);
begin
  FOutput.Add('procedure ' + Node.Name + ';');
  GenerateBlock(Node.Body);
  FOutput.Add(';');
end;

procedure TCodeGenerator.GenerateFuncDecl(Node: TFuncDeclNode);
begin
  FOutput.Add('function ' + Node.Name + ': Integer;'); // Assuming return type is Integer for simplicity
  GenerateBlock(Node.Body);
  FOutput.Add(';');
end;

procedure TCodeGenerator.GenerateExprStmt(Node: TExprStmtNode);
begin
  FOutput.Add(Node.Name + '();');
end;

var
  Lexer: TLexer;
  Parser: TParser;
  CodeGenerator: TCodeGenerator;
  AST: TProgramNode;
begin
  Lexer := TLexer.Create('input.scp');
  try
    Parser := TParser.Create(Lexer);
    try
      AST := Parser.Parse;
      // Process the AST (e.g., interpret or compile it)
      CodeGenerator := TCodeGenerator.Create;
      try
        CodeGenerator.Generate(AST, 'output.pas');
      finally
        CodeGenerator.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    Lexer.Free;
  end;
end.
