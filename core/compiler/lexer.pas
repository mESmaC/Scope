unit Lexer;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, TypInfo;

type
TTokenType = (
  TOK_EOF, TOK_PLUS, TOK_MINUS, TOK_MULTIPLY, TOK_DIVIDE,
  TOK_EQUAL, TOK_LESS, TOK_GREATER, TOK_LPAREN, TOK_RPAREN,
  TOK_COMMA, TOK_LBRACE, TOK_RBRACE, TOK_LBRACKET, TOK_RBRACKET,
  TOK_MOD, TOK_AND, TOK_OR, TOK_XOR, TOK_NOT,
  TOK_QUESTION, TOK_DOLLAR, TOK_AT, TOK_HASH,
  TOK_BACKSLASH, TOK_BACKQUOTE, TOK_IDENTIFIER, TOK_NUMBER,
  TOK_SEMICOLON, TOK_COLON, TOK_DOT, TOK_ASSIGN,
  TOK_INIT, TOK_PROC, TOK_FUNC, TOK_USES, TOK_PROGRAM,
  TOK_STRING, TOK_MUT, TOK_VAR, TOK_OWNER, TOK_BORROW,
  TOK_CLASS, TOK_PUBLIC, TOK_PRIVATE, TOK_TRY, TOK_FINALLY,
  TOK_COMMENT 
);

  TKeywordSet = array of string;
  TOperatorSet = array of string;
  TDelimiterSet = array of string;

var
  Keywords: TKeywordSet = (
    'program', 'uses', 'var', 'proc', 'func', 'init', 'if', 'then', 'else', 'while',
    'do', 'for', 'to', 'integer', 'real', 'char', 'string', 'boolean', 'true', 'false', 'mut', 'owner', 'borrow',
    'class', 'public', 'private', 'try', 'finally'
  );

  Operators: TOperatorSet = (
    ':=', '+', '-', '*', '/', '=',
    '<>', '<', '<=', '>', '>=',
    'and', 'not', 'or'
  );

  Delimiters: TDelimiterSet = (
    '{', '}', '(', ')', ';',
    ':', ',', '.', '..'
  );

const
  EOF_MARKER = '}.';

type
  TToken = class
    TokenType: TTokenType;
    Lexeme: string;
    Line: integer;
    Column: integer;

    constructor Create(AType: TTokenType; ALexeme: string; ALine, AColumn: integer);
  end;

  TReader = class
  private
    FStream: TFileStream;
    FLine: integer;
    FColumn: integer;
  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    function Peek: char;
    function PeekNext: char; 
    function Read: char;
    function ReadIdentifier: string;
    function ReadNumber: string;
    function IsEOF: boolean;
    property Line: integer read FLine;
    property Column: integer read FColumn;
  end;

  TLexer = class
  private
    FReaders: TList;
    FCurrentReader: TReader;
    FCurrentToken: TToken;
    procedure SwitchToNextReader;
  public
    constructor Create(AFilename: string);
    destructor Destroy; override;
    function GetNextToken: TToken;
    procedure Expect(ATokenType: TTokenType);
    function IsEOF: boolean;
    procedure AddFile(const AFilename: string);
  end;

function IsAlpha(ch: char): boolean;
function IsAlphaNum(ch: char): boolean;
function IsKeyword(const lexeme: string): boolean;

implementation

constructor TToken.Create(AType: TTokenType; ALexeme: string; ALine, AColumn: integer);
begin
  TokenType := AType;
  Lexeme := ALexeme;
  Line := ALine;
  Column := AColumn;
end;

constructor TReader.Create(const AFilename: string);
begin
  FStream := TFileStream.Create(AFilename, fmOpenRead);
  FLine := 1;
  FColumn := 0;
end;

destructor TReader.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TReader.Peek: char;
var
  ch: char;
begin
  if FStream.Read(ch, 1) = 1 then
  begin
    FStream.Seek(-1, soFromCurrent);
    Result := ch;
  end
  else
    Result := #0;
  WriteLn('Peek: ', Result, ' (', Ord(Result), ') at line ', FLine, ', column ', FColumn);
end;

function TReader.Read: char;
begin
  if FStream.Read(Result, 1) = 1 then
  begin
    Inc(FColumn);
    if Result = #10 then
    begin
      Inc(FLine);
      FColumn := 0;
    end;
  end
  else
    Result := #0;
  WriteLn('Read: ', Result, ' (', Ord(Result), ') at line ', FLine, ', column ', FColumn);
end;

function TReader.ReadIdentifier: string;
begin
  Result := '';
  while IsAlpha(Peek) or IsAlphaNum(Peek) do
    Result := Result + Read;
end;

function TReader.ReadNumber: string;
begin
  Result := '';
  while (Peek >= '0') and (Peek <= '9') do
    Result := Result + Read;
end;

function TReader.IsEOF: boolean;
begin
  Result := FStream.Position >= FStream.Size;
end;

constructor TLexer.Create(AFilename: string);
begin
  FReaders := TList.Create;
  AddFile(AFilename);
end;

destructor TLexer.Destroy;
var
  I: Integer;
begin
  for I := 0 to FReaders.Count - 1 do
    TReader(FReaders[I]).Free;
  FReaders.Free;
  inherited;
end;

procedure TLexer.AddFile(const AFilename: string);
begin
  FReaders.Add(TReader.Create(AFilename));
  if FReaders.Count = 1 then
    FCurrentReader := TReader(FReaders[0]);
end;

procedure TLexer.Expect(ATokenType: TTokenType);
begin
  if FCurrentToken.TokenType <> ATokenType then
    raise Exception.CreateFmt('Expected token %s but found %s', [GetEnumName(TypeInfo(TTokenType), Ord(ATokenType)), GetEnumName(TypeInfo(TTokenType), Ord(FCurrentToken.TokenType))]);
  FCurrentToken := GetNextToken;
end;

procedure TLexer.SwitchToNextReader;
begin
  if FReaders.Count > 0 then
  begin
    TReader(FReaders[0]).Free;
    FReaders.Delete(0);
    if FReaders.Count > 0 then
      FCurrentReader := TReader(FReaders[0])
    else
      FCurrentReader := nil;
  end;
end;

function IsAlpha(ch: char): boolean;
begin
  Result := (ch >= 'a') and (ch <= 'z') or (ch >= 'A') and (ch <= 'Z') or (ch = '_');
end;

function IsAlphaNum(ch: char): boolean;
begin
  Result := IsAlpha(ch) or (ch >= '0') and (ch <= '9');
end;

function IsKeyword(const lexeme: string): boolean;
var
  i: integer;
begin
  Result := False;
  for i := Low(Keywords) to High(Keywords) do
  begin
    if Keywords[i] = lexeme then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TReader.PeekNext: char;
var
  ch: char;
begin
  if FStream.Read(ch, 1) = 1 then
  begin
    if FStream.Read(ch, 1) = 1 then
    begin
      FStream.Seek(-2, soFromCurrent);
      Result := ch;
    end
    else
    begin
      FStream.Seek(-1, soFromCurrent);
      Result := #0;
    end;
  end
  else
    Result := #0;
end;

function TLexer.IsEOF: boolean;
begin
  Result := (FCurrentReader = nil) or (FCurrentToken.TokenType = TOK_EOF);
end;

function TLexer.GetNextToken: TToken;
var
  lexeme: string;
begin
  while (FCurrentReader <> nil) and (FCurrentReader.Peek in [' ', #9, #10, #13]) do
    FCurrentReader.Read;

  if FCurrentReader = nil then
  begin
    Result := TToken.Create(TOK_EOF, '', 0, 0);
    Exit;
  end;

  if FCurrentReader.Peek in ['+', '-', '*', '/', '=', '<', '>', '(', ')', ';', ':', ',', '.', '{', '}', '[', ']', '%', '&', '|', '^', '~', '?', '$', '@', '#', '\', '`'] then
  begin
    case FCurrentReader.Peek of
      '+': Result := TToken.Create(TOK_PLUS, '+', FCurrentReader.Line, FCurrentReader.Column);
      '-': Result := TToken.Create(TOK_MINUS, '-', FCurrentReader.Line, FCurrentReader.Column);
      '*': Result := TToken.Create(TOK_MULTIPLY, '*', FCurrentReader.Line, FCurrentReader.Column);
      '/': 
        begin
          FCurrentReader.Read;
          if FCurrentReader.Peek = '/' then
          begin
            while (FCurrentReader.Peek <> #10) and (not FCurrentReader.IsEOF) do
              FCurrentReader.Read;
            Result := GetNextToken; // Skip single-line comment and get the next token
            Exit;
          end
          else if FCurrentReader.Peek = '*' then
          begin
            FCurrentReader.Read;
            while not ((FCurrentReader.Peek = '*') and (FCurrentReader.PeekNext = '/')) and (not FCurrentReader.IsEOF) do
              FCurrentReader.Read;
            if not FCurrentReader.IsEOF then
            begin
              FCurrentReader.Read; // Read '*'
              FCurrentReader.Read; // Read '/'
            end;
            Result := GetNextToken; // Skip multi-line comment and get the next token
            Exit;
          end
          else
            Result := TToken.Create(TOK_DIVIDE, '/', FCurrentReader.Line, FCurrentReader.Column);
        end;
      '=': Result := TToken.Create(TOK_EQUAL, '=', FCurrentReader.Line, FCurrentReader.Column);
      '<': Result := TToken.Create(TOK_LESS, '<', FCurrentReader.Line, FCurrentReader.Column);
      '>': Result := TToken.Create(TOK_GREATER, '>', FCurrentReader.Line, FCurrentReader.Column);
      '(': Result := TToken.Create(TOK_LPAREN, '(', FCurrentReader.Line, FCurrentReader.Column);
      ')': Result := TToken.Create(TOK_RPAREN, ')', FCurrentReader.Line, FCurrentReader.Column);
      ';': Result := TToken.Create(TOK_SEMICOLON, ';', FCurrentReader.Line, FCurrentReader.Column);
      ':': Result := TToken.Create(TOK_COLON, ':', FCurrentReader.Line, FCurrentReader.Column);
      ',': Result := TToken.Create(TOK_COMMA, ',', FCurrentReader.Line, FCurrentReader.Column);
      '.': Result := TToken.Create(TOK_DOT, '.', FCurrentReader.Line, FCurrentReader.Column);
      '{': Result := TToken.Create(TOK_LBRACE, '{', FCurrentReader.Line, FCurrentReader.Column);
      '}': Result := TToken.Create(TOK_RBRACE, '}', FCurrentReader.Line, FCurrentReader.Column);
      '[': Result := TToken.Create(TOK_LBRACKET, '[', FCurrentReader.Line, FCurrentReader.Column);
      ']': Result := TToken.Create(TOK_RBRACKET, ']', FCurrentReader.Line, FCurrentReader.Column);
      '%': Result := TToken.Create(TOK_MOD, '%', FCurrentReader.Line, FCurrentReader.Column);
      '&': Result := TToken.Create(TOK_AND, '&', FCurrentReader.Line, FCurrentReader.Column);
      '|': Result := TToken.Create(TOK_OR, '|', FCurrentReader.Line, FCurrentReader.Column);
      '^': Result := TToken.Create(TOK_XOR, '^', FCurrentReader.Line, FCurrentReader.Column);
      '~': Result := TToken.Create(TOK_NOT, '~', FCurrentReader.Line, FCurrentReader.Column);
      '?': Result := TToken.Create(TOK_QUESTION, '?', FCurrentReader.Line, FCurrentReader.Column);
      '$': Result := TToken.Create(TOK_DOLLAR, '$', FCurrentReader.Line, FCurrentReader.Column);
      '@': Result := TToken.Create(TOK_AT, '@', FCurrentReader.Line, FCurrentReader.Column);
      '#': Result := TToken.Create(TOK_HASH, '#', FCurrentReader.Line, FCurrentReader.Column);
      '\': Result := TToken.Create(TOK_BACKSLASH, '\', FCurrentReader.Line, FCurrentReader.Column);
      '`': Result := TToken.Create(TOK_BACKQUOTE, '`', FCurrentReader.Line, FCurrentReader.Column);
    end;
    FCurrentReader.Read; 
  end
  else if FCurrentReader.Peek = '''' then
  begin
    lexeme := '';
    FCurrentReader.Read; 
    while (FCurrentReader.Peek <> '''') and (not FCurrentReader.IsEOF) do
    begin
      lexeme := lexeme + FCurrentReader.Read;
    end;
    if FCurrentReader.Peek = '''' then
      FCurrentReader.Read; 
    Result := TToken.Create(TOK_STRING, lexeme, FCurrentReader.Line, FCurrentReader.Column);
  end
  else if IsAlpha(FCurrentReader.Peek) then
  begin
    lexeme := '';
    while IsAlphaNum(FCurrentReader.Peek) do
    begin
      lexeme := lexeme + FCurrentReader.Read;
    end;
    if IsKeyword(lexeme) then
    begin
      if lexeme = 'mut' then
        Result := TToken.Create(TOK_MUT, lexeme, FCurrentReader.Line, FCurrentReader.Column)
      else if lexeme = 'owner' then
        Result := TToken.Create(TOK_OWNER, lexeme, FCurrentReader.Line, FCurrentReader.Column)
      else if lexeme = 'borrow' then
        Result := TToken.Create(TOK_BORROW, lexeme, FCurrentReader.Line, FCurrentReader.Column)
      else if lexeme = 'class' then
        Result := TToken.Create(TOK_CLASS, lexeme, FCurrentReader.Line, FCurrentReader.Column)
      else if lexeme = 'public' then
        Result := TToken.Create(TOK_PUBLIC, lexeme, FCurrentReader.Line, FCurrentReader.Column)
      else if lexeme = 'private' then
        Result := TToken.Create(TOK_PRIVATE, lexeme, FCurrentReader.Line, FCurrentReader.Column)
      else
        Result := TToken.Create(TTokenType(GetEnumValue(TypeInfo(TTokenType), 'TOK_' + UpperCase(lexeme))), lexeme, FCurrentReader.Line, FCurrentReader.Column);
    end
    else
      Result := TToken.Create(TOK_IDENTIFIER, lexeme, FCurrentReader.Line, FCurrentReader.Column);
  end
  else if (FCurrentReader.Peek >= '0') and (FCurrentReader.Peek <= '9') then
  begin
    lexeme := '';
    while (FCurrentReader.Peek >= '0') and (FCurrentReader.Peek <= '9') do
    begin
      lexeme := lexeme + FCurrentReader.Read;
    end;
    Result := TToken.Create(TOK_NUMBER, lexeme, FCurrentReader.Line, FCurrentReader.Column);
  end
  else if FCurrentReader.IsEOF then
  begin
    Result := TToken.Create(TOK_EOF, '', FCurrentReader.Line, FCurrentReader.Column);
  end
  else
    raise Exception.Create('Invalid character: ' + FCurrentReader.Peek);
  
  WriteLn('Generated token: ', Result.Lexeme, ' (', Ord(Result.TokenType), ') at line ', Result.Line, ', column ', Result.Column);
end;

end.