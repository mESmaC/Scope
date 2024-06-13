program Compiler;

{$mode objfpc}{$H+}

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
    procedure GenerateVarDecl(Node: TVarDeclNode);
    procedure GenerateClassDecl(Node: TClassDeclNode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Generate(Node: TProgramNode);
    procedure SaveToFile(const AFilename: string);
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

procedure TCodeGenerator.GenerateProgram(Node: TProgramNode);
var
  i: integer;
begin
  FOutput.Add('program ' + Node.Name + ';');
  if Length(Node.UsedUnits) > 0 then
  begin
    FOutput.Add('uses');
    for i := 0 to High(Node.UsedUnits) do
    begin
      if i > 0 then
        FOutput.Add(', ');
      FOutput.Add(Node.UsedUnits[i]);
    end;
    FOutput.Add(';');
  end;
  GenerateBlock(TBlockNode(Node.Block));
  FOutput.Add('.');
end;

procedure TCodeGenerator.GenerateBlock(Node: TBlockNode);
var
  i: integer;
begin
  FOutput.Add('begin');
  for i := 0 to High(Node.Statements) do
  begin
    if Node.Statements[i] is TVarDeclNode then
      GenerateVarDecl(TVarDeclNode(Node.Statements[i]))
    else if Node.Statements[i] is TProcDeclNode then
      GenerateProcDecl(TProcDeclNode(Node.Statements[i]))
    else if Node.Statements[i] is TFuncDeclNode then
      GenerateFuncDecl(TFuncDeclNode(Node.Statements[i]))
    else if Node.Statements[i] is TClassDeclNode then
      GenerateClassDecl(TClassDeclNode(Node.Statements[i])) // Add semicolon here
    else if Node.Statements[i] is TExprStmtNode then
      GenerateExprStmt(TExprStmtNode(Node.Statements[i]));
  end;
  FOutput.Add('end;');
end;

procedure TCodeGenerator.GenerateProcDecl(Node: TProcDeclNode);
begin
  FOutput.Add('procedure ' + Node.Name + '() {');
  GenerateBlock(Node.Body);
  FOutput.Add('};'); // Add semicolon after closing brace
end;

procedure TCodeGenerator.GenerateFuncDecl(Node: TFuncDeclNode);
begin
  FOutput.Add('function ' + Node.Name + '() {');
  GenerateBlock(Node.Body);
  FOutput.Add('};'); // Add semicolon after closing brace
end;

procedure TCodeGenerator.GenerateExprStmt(Node: TExprStmtNode);
var
  i: integer;
begin
  FOutput.Add(Node.Name + '(');
  for i := 0 to High(Node.Args) do
  begin
    if i > 0 then
      FOutput.Add(', ');
    FOutput.Add(Node.Args[i]);
  end;
  FOutput.Add(');');
end;

procedure TCodeGenerator.GenerateVarDecl(Node: TVarDeclNode);
begin
  if Node.IsMutable then
    FOutput.Add('var mut ' + Node.Name + ': ' + Node.VarType + ';')
  else if Node.IsOwner then
    FOutput.Add('var owner ' + Node.Name + ': ' + Node.VarType + ';')
  else if Node.IsBorrow then
    FOutput.Add('var borrow ' + Node.Name + ': ' + Node.VarType + ';')
  else
    FOutput.Add('var ' + Node.Name + ': ' + Node.VarType + ';');
end;

procedure TCodeGenerator.GenerateClassDecl(Node: TClassDeclNode);
var
  i: integer;
begin
  FOutput.Add(Node.Visibility + ' class ' + Node.Name + ' {');
  for i := 0 to High(Node.Members) do
  begin
    if Node.Members[i] is TVarDeclNode then
      GenerateVarDecl(TVarDeclNode(Node.Members[i]))
    else if Node.Members[i] is TProcDeclNode then
      GenerateProcDecl(TProcDeclNode(Node.Members[i]))
    else if Node.Members[i] is TFuncDeclNode then
      GenerateFuncDecl(TFuncDeclNode(Node.Members[i]));
  end;
  FOutput.Add('};'); // Add semicolon after closing brace
end;

procedure TCodeGenerator.Generate(Node: TProgramNode);
begin
  GenerateProgram(Node);
end;

procedure TCodeGenerator.SaveToFile(const AFilename: string);
begin
  FOutput.SaveToFile(AFilename);
end;

var
  LexerInstance: TLexer;
  ParserInstance: TParser;
  CodeGen: TCodeGenerator;
  ProgramNode: TProgramNode;
begin
  if ParamCount < 2 then
  begin
    WriteLn('Usage: compile <inputfile> <outputfile>');
    Exit;
  end;

  LexerInstance := TLexer.Create(ParamStr(1));
  try
    ParserInstance := TParser.Create(LexerInstance);
    try
      ProgramNode := ParserInstance.Parse;
      CodeGen := TCodeGenerator.Create;
      try
        CodeGen.Generate(ProgramNode);
        CodeGen.SaveToFile(ParamStr(2));
      finally
        CodeGen.Free;
      end;
    finally
      ParserInstance.Free;
    end;
  finally
    LexerInstance.Free;
  end;
end.