program Lexer;

type
  TTokenType = (
    TOK_EOF, TOK_PLUS, TOK_MINUS, TOK_MULTIPLY, TOK_DIVIDE,
    TOK_EQUAL, TOK_LESS, TOK_GREATER, TOK_LPAREN, TOK_RPAREN,
    TOK_COMMA, TOK_LBRACE, TOK_RBRACE, TOK_LBRACKET, TOK_RBRACKET,
    TOK_MOD, TOK_AND, TOK_OR, TOK_XOR, TOK_NOT,
    TOK_QUESTION, TOK_DOLLAR, TOK_AT, TOK_HASH,
    TOK_BACKSLASH, TOK_BACKQUOTE, TOK_IDENTIFIER, TOK_NUMBER
  );

  TKeywordSet = set of string;
  TOperatorSet = set of string;
  TDelimiterSet = set of string;

const
  Keywords: TKeywordSet = [
    'program', 'uses', 'var', 'proc', 'func',
    'init', 'if', 'then', 'else', 'while',
    'do', 'for', 'to', 'integer', 'real',
    'char', 'string', 'boolean', 'true', 'false',
    'and', 'or', 'not'
  ];
  Operators: TOperatorSet = [
    ':=', '+', '-', '*', '/', '=',
    '<>', '<', '<=', '>', '>=',
    'and', 'or', 'not'
  ];
  Delimiters: TDelimiterSet = [
    '{', '}', '(', ')', ';',
    ':', ',', '.', '..'
  ];

  EOF_MARKER = '}.';

type
  TToken = class
    TokenType: TTokenType;
    Lexeme: string;
    Line: integer;
    Column: integer;

    constructor Create(AType: TTokenType; ALexeme: string; ALine, AColumn: integer);
  end;

type
  TReader = class
  private
    FStream: TFileStream;
    FLine: integer;
    FColumn: integer;
  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    function Peek: char;
    function Read: char;
    function ReadIdentifier: string;
    function ReadNumber: string;
    function IsEOF: boolean;
    property Line: integer read FLine;
    property Column: integer read FColumn;
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

  TLexer = class
  private
    FReader: TReader;
    FCurrentToken: TToken;
  public
    constructor Create(AFilename: string);
    function GetNextToken: TToken;
    procedure Expect(ATokenType: TTokenType);
    function IsEOF: boolean;
  end;

constructor TToken.Create(AType: TTokenType; ALexeme: string; ALine, AColumn: integer);
begin
  TokenType := AType;
  Lexeme := ALexeme;
  Line := ALine;
  Column := AColumn;
end;

constructor TLexer.Create(AFilename: string);
begin
  FReader := TReader.Create(AFilename);
end;

function IsAlpha(ch: char): boolean;
begin
  Result := (ch >= 'a') and (ch <= 'z') or (ch >= 'A') and (ch <= 'Z') or (ch = '_');
end;

function IsAlphaNum(ch: char): boolean;
begin
  Result := IsAlpha(ch) or (ch >= '0') and (ch <= '9');
end;

function TLexer.GetNextToken: TToken;
var
  lexeme: string;
begin
  while FReader.Peek in [' ', #9, #10, #13] do
    FReader.Read;

  if FReader.Peek in ['+', '-', '*', '/', '=', '<', '>', '(', ')', ';', ':', ',', '.'] then
  begin
    case FReader.Peek of
      '+': Result := TToken.Create(TOK_PLUS, '+', FReader.Line, FReader.Column);
      '-': Result := TToken.Create(TOK_MINUS, '-', FReader.Line, FReader.Column);
      '*': Result := TToken.Create(TOK_MULTIPLY, '*', FReader.Line, FReader.Column);
      '/': Result := TToken.Create(TOK_DIVIDE, '/', FReader.Line, FReader.Column);
      '=': Result := TToken.Create(TOK_EQUAL, '=', FReader.Line, FReader.Column);
      '<': Result := TToken.Create(TOK_LESS, '<', FReader.Line, FReader.Column);
      '>': Result := TToken.Create(TOK_GREATER, '>', FReader.Line, FReader.Column);
      '(': Result := TToken.Create(TOK_LPAREN, '(', FReader.Line, FReader.Column);
      ')': Result := TToken.Create(TOK_RPAREN, ')', FReader.Line, FReader.Column);
      ',': Result := TToken.Create(TOK_COMMA, ',', FReader.Line, FReader.Column);
      '{': Result := TToken.Create(TOK_LBRACE, '{', FReader.Line, FReader.Column);
      '}': Result := TToken.Create(TOK_RBRACE, '}', FReader.Line, FReader.Column);
      '[': Result := TToken.Create(TOK_LBRACKET, '[', FReader.Line, FReader.Column);
      ']': Result := TToken.Create(TOK_RBRACKET, ']', FReader.Line, FReader.Column);
      '%': Result := TToken.Create(TOK_MOD, '%', FReader.Line, FReader.Column);
      '&': Result := TToken.Create(TOK_AND, '&', FReader.Line, FReader.Column);
      '|': Result := TToken.Create(TOK_OR, '|', FReader.Line, FReader.Column);
      '^': Result := TToken.Create(TOK_XOR, '^', FReader.Line, FReader.Column);
      '~': Result := TToken.Create(TOK_NOT, '~', FReader.Line, FReader.Column);
      '?': Result := TToken.Create(TOK_QUESTION, '?', FReader.Line, FReader.Column);
      '$': Result := TToken.Create(TOK_DOLLAR, '$', FReader.Line, FReader.Column);
      '@': Result := TToken.Create(TOK_AT, '@', FReader.Line, FReader.Column);
      '#': Result := TToken.Create(TOK_HASH, '#', FReader.Line, FReader.Column);
      '\': Result := TToken.Create(TOK_BACKSLASH, '\', FReader.Line, FReader.Column);
      '`': Result := TToken.Create(TOK_BACKQUOTE, '`', FReader.Line, FReader.Column);
    end;
  end
  else if IsAlpha(FReader.Peek) then
  begin
    lexeme := '';
    while IsAlphaNum(FReader.Peek) do
    begin
      lexeme := lexeme + FReader.Peek;
      FReader.Read;
    end;
    if lexeme in Keywords then
      Result := TToken.Create(KeywordTokens[lexeme], lexeme, FReader.Line, FReader.Column)
    else
      Result := TToken.Create(TOK_IDENTIFIER, lexeme, FReader.Line, FReader.Column);
  end
  else if (FReader.Peek >= '0') and (FReader.Peek <= '9') then
  begin
    lexeme := '';
    while (FReader.Peek >= '0') and (FReader.Peek <= '9') do
    begin
      lexeme := lexeme + FReader.Peek;
      FReader.Read;
    end;
    Result := TToken.Create(TOK_NUMBER, lexeme, FReader.Line, FReader.Column);
  end
  else if FReader.Peek = EOF_MARKER then
  begin
    Result := TToken.Create(TOK_EOF, '', FReader.Line, FReader.Column);
    FReader.Read;
  end
  else
    raise Exception.Create('Invalid character: ' + FReader.Peek);
end;
end;