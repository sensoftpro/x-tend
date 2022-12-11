{---------------------------------------------------------------------------------
  X-Tend runtime

  Contributors:
    Vladimir Kustikov (kustikov@sensoft.pro)
    Sergey Arlamenkov (arlamenkov@sensoft.pro)

  You may retrieve the latest version of this file at the GitHub,
  located at https://github.com/sensoftpro/x-tend.git
 ---------------------------------------------------------------------------------
  MIT License

  Copyright © 2023 Sensoft

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 ---------------------------------------------------------------------------------}

unit uCodeParser;

interface

uses
  Classes, Generics.Collections;

type
  TTokenKind = (tkCommStart, tkCommEnd, tkCommLine, tkLineEnd, tkSpace,
    tkParOpen, tkParClose, tkBracketOpen, tkBracketClose, tkLess, tkGreater,
    tkMinus, tkPlus, tkAsterisk, tkSlash, tkPercent, tkAmpersand, tkVBar, tkCaret, tkExclamation, tkQuestion,
    {tkDot,} tkComma, tkColon, tkSemicolon, tkApostrophe, tkQuote, tkEqual,
    tkLiteral, tkEof);
  TTokenKinds = set of TTokenKind;

type
  TToken = record
    Kind: TTokenKind;
    Value: string;
    LineNo: Integer;
    ColumnNo: Integer;
    Position: Integer;
  end;

type
  TNodeKind = (nkService, nkComment, nkSpace, nkLineEnd, nkParenthesis, nkBracket,
    nkText, nkSymbol, nkLiteral, nkKeyword, nkIdentifier, nkNumber, nkOperator, nkFunction, nkExpression);

type
  TASTNode = class(TEnumerable<TASTNode>)
  private
    FKind: TNodeKind;
    [Weak] FParent: TASTNode;
    FItems: TObjectList<TASTNode>;
    FStartPos: Integer;
    FLineNo: Integer;
    FColumnNo: Integer;
    FText: string;
    function GetEndPos: Integer;
    procedure SetParent(const Value: TASTNode);
    function GetCount: Integer;
    function GetItem(const AIndex: Integer): TASTNode;
  protected
    function DoGetEnumerator: TEnumerator<TASTNode>; override;
  public
    constructor Create(const ANodeKind: TNodeKind; const AParent: TASTNode;
      const AStartPos: Integer);
    destructor Destroy; override;

    property LineNo: Integer read FLineNo write FLineNo;
    property ColumnNo: Integer read FColumnNo write FColumnNo;
    property Text: string read FText write FText;
    property Kind: TNodeKind read FKind write FKind;
    property EndPos: Integer read GetEndPos;
    property Parent: TASTNode read FParent write SetParent;

    function FullText: string;
    function ChildText: string;
    procedure AppendText(const AText: string);
    procedure RemoveSpaces;

    procedure AddChild(const ANode: TASTNode);
    procedure DeleteChild(const AIndex: Integer);
    procedure ExtractChild(const ANode: TASTNode);

    property Count: Integer read GetCount;
    property Items[const AIndex: Integer]: TASTNode read GetItem; default;
  end;

  TCalcNodeKind = (cnkValue, cnkVariable, cnkOperation, cnkFunction);
  TValueKind = (vkString, vkInteger, vkFloat, vkBoolean);
  TOperatorKind = (okNone, okNeg, okAdd, okSub, okMul, okDiv, okMod, okPower, okEq, okNotEq, okGreater, okLess,
    okGreaterOrEg, okLessOrEg, okAnd, okOr, okNot, okBitAnd, okBitOr, okBitShl, okBitShr);

type
  TExpressionAST = class;
  TCalcNode = class;
  TOperationNode = class;

  TCalcEnvironment = class
  protected
    function GetVariableValue(const AVarName: string): Variant; virtual; abstract;
    function DoExecuteFunction(const AFuncName: string; const AParams: array of Variant): Variant; virtual; abstract;
  end;

  TCalcTree = class
  private
    FRootNode: TCalcNode;
    [Weak] FCalcEnv: TCalcEnvironment;
    function CreateOperatorNode(const ALeftNode: TCalcNode; const AOperator: string;
      var AKind: TOperatorKind): TOperationNode;
    function CreateCalcNode(const ANode: TASTNode; const AValuesOnly: Boolean): TCalcNode;

    function VarValueByName(const AVarName: string): Variant;
    function ExecuteFunction(const AFuncName: string; const AParams: array of Variant): Variant;
  public
    constructor Create(const AAST: TExpressionAST; const AValuesOnly: Boolean);
    destructor Destroy; override;

    function Calculate(const ACalcEnv: TCalcEnvironment): Variant;
    function Print: string;
  end;

  TCalcNode = class
  protected
    [Weak] FOwner: TCalcTree;
    function GetValue: Variant; virtual;
  public
    constructor Create(const AOwner: TCalcTree);
    destructor Destroy; override;

    function Print: string; virtual;

    property Value: Variant read GetValue;
  end;

  TValueNode = class(TCalcNode)
  private
    FValue: Variant;
  protected
    function GetValue: Variant; override;
  public
    constructor Create(const AOwner: TCalcTree; const AValue: Variant);

    function Print: string; override;
  end;

  TVariableNode = class(TCalcNode)
  private
    FVarName: string;
  protected
    function GetValue: Variant; override;
  public
    constructor Create(const AOwner: TCalcTree; const AVarName: string);

    function Print: string; override;
  end;

  TFunctionNode = class(TCalcNode)
  private
    FName: string;
    FParameters: TObjectList<TCalcNode>;
    FParamValues: array of Variant;
  protected
    function GetValue: Variant; override;
  public
    constructor Create(const AOwner: TCalcTree; const ANode: TASTNode);
    destructor Destroy; override;

    function Print: string; override;
  end;

  TOperationNode = class(TCalcNode)
  protected
    FOperand: TCalcNode;
    FKind: TOperatorKind;
  public
    constructor Create(const AOwner: TCalcTree; const AKind: TOperatorKind);
    destructor Destroy; override;

    procedure SetOperand(const AOperand: TCalcNode);
  end;

  TUnaryOperationNode = class(TOperationNode)
  protected
    function GetValue: Variant; override;
  public
    function Print: string; override;
  end;

  TBinaryOperationNode = class(TOperationNode)
  private
    FLeftOperand: TCalcNode;
  protected
    function GetValue: Variant; override;
  public
    constructor Create(const AOwner: TCalcTree; const AKind: TOperatorKind);
    destructor Destroy; override;

    procedure SetLeftOperand(const AOperand: TCalcNode);
    function Print: string; override;
  end;

  TExpressionAST = class
  private
    FRootNode: TASTNode;
    [Weak] FCurrentNode: TASTNode;
    [Weak] FLastNode: TASTNode;
    FText: string;
    procedure PickSkippedNode(const AToken: TToken);
    procedure AddChildNode(const ANodeKind: TNodeKind; const AToken: TToken);
    function AddNode(const ANodeKind: TNodeKind; const AToken: TToken): TASTNode;
    procedure AddSpace(const AToken: TToken);
    procedure CloseNode;

    function PrintAST(const ARootNode: TASTNode;
      const AIndent: Integer = 0): string;
    function HandleToken(const AToken: TToken): TTokenKinds;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Tokenize(const AText: string);
    function Print: string;
    procedure RemoveSpaces;

    property RootNode: TASTNode read FRootNode;
  end;

function CreateCalcTree(const AText: string; const AValuesOnly: Boolean): TCalcTree;

implementation

uses
  Types, SysUtils, StrUtils, Math, Variants, Character, RegularExpressions;

const
  tkFirst = tkCommStart;
  tkLast = tkEqual;
  cTokenNames: array[TTokenKind] of string =
   ('COMM_START', 'COMM_END', 'COMM_LINE', 'LINE_END', 'SPACE',
    'PAR_OPEN', 'PAR_CLOSE', 'BRACKET_OPEN', 'BRACKET_CLOSE', 'LESS', 'GREATER',
    'MINUS', 'PLUS', 'ASTERISK', 'SLASH', 'PERCENT', 'AMPERSAND', 'VBAR', 'CARET', 'EXCLAMATION', 'QUESTION',
    {'DOT',} 'COMMA', 'COLON', 'SEMICOLON', 'APOSTROPHE', 'QUOTE', 'EQUAL', 'LITERAL', 'EOF');
  cTokenRE: array[TTokenKind] of string = ('\{', '\}', '//', #13'?'#10, '[ '#9']+',
    '\(', '\)', '\[', '\]', '\<', '\>', '\-', '\+', '\*', '/', '%', '&', '\|', '\^', '\!', '\?',
    {'\.',} ',', ':', ';', '''', '"', '=', '', '');
  cDefaultTokens = [tkFirst..tkLast];
  cNodeKindNames: array[TNodeKind] of string = ('END', 'COMMENT', 'SPACE', 'LINE_END', 'PAR', 'BRACKET',
    'TEXT', 'SYMBOL', 'LITERAL', 'KEYWORD', 'IDENT', 'NUMBER', 'OPERATOR', 'FUNCTION', 'EXPRESSION');
  cOperatorTexts: array[TOperatorKind] of string = ('', '-', '+', '-', '*', '/', '%', '^', '=', '<>', '>', '<',
    '>=', '<=', '&&', '||', '!', '&', '|', '<<', '>>');
  cOperatorPriorities: array[TOperatorKind] of Integer = (-1, 4, 3, 3, 5, 5, 5, 6, 1, 1, 1, 1,
    1, 1, 0, 0, 2, 7, 7, 7, 7);

function CreateCalcTree(const AText: string; const AValuesOnly: Boolean): TCalcTree;
var
  vAST: TExpressionAST;
begin
  vAST := TExpressionAST.Create;
  try
    vAST.Tokenize(AText);
    Result := TCalcTree.Create(vAST, AValuesOnly);
  finally
    FreeAndNil(vAST);
  end;
end;

{ TASTNode }

procedure TASTNode.AddChild(const ANode: TASTNode);
begin
  FItems.Add(ANode);
end;

procedure TASTNode.AppendText(const AText: string);
begin
  FText := FText + AText;
end;

function TASTNode.ChildText: string;
var
  vChildNode: TASTNode;
begin
  Result := '';
  for vChildNode in FItems do
    Result := Result + vChildNode.FullText;
end;

constructor TASTNode.Create(const ANodeKind: TNodeKind; const AParent: TASTNode;
  const AStartPos: Integer);
begin
  inherited Create;

  FKind := ANodeKind;
  FStartPos := AStartPos;
  FItems := TObjectList<TASTNode>.Create;
  FParent := AParent;
  if Assigned(AParent) then
    AParent.AddChild(Self);
end;

procedure TASTNode.DeleteChild(const AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

destructor TASTNode.Destroy;
begin
  FParent := nil;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TASTNode.DoGetEnumerator: TEnumerator<TASTNode>;
begin
  Result := FItems.GetEnumerator;
end;

procedure TASTNode.ExtractChild(const ANode: TASTNode);
begin
  FItems.Extract(ANode);
end;

function TASTNode.FullText: string;
begin
  Result := FText + ChildText;
end;

function TASTNode.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TASTNode.GetEndPos: Integer;
begin
  Result := FStartPos + Length(FText);
end;

function TASTNode.GetItem(const AIndex: Integer): TASTNode;
begin
  Result := FItems[AIndex];
end;

procedure TASTNode.RemoveSpaces;
var
  i: Integer;
  vNode: TASTNode;
  vFuncNode: TASTNode;
  vExpressionNode: TASTNode;
  vExpressions: TList<TASTNode>;
  vChildNode: TASTNode;
begin
  for i := FItems.Count - 1 downto 0 do
  begin
    vNode := FItems[i];
    if vNode.Kind in [nkService, nkComment, nkSpace, nkLineEnd] then
      DeleteChild(i)
    else if vNode.Kind = nkLiteral then begin
      if (i + 1 < FItems.Count) and (FItems[i+1].Kind in [nkParenthesis, nkBracket]) then
      begin
        vFuncNode := FItems[i+1];
        vFuncNode.Kind := nkFunction;
        vFuncNode.Text := vNode.Text;
        vFuncNode.LineNo := vNode.LineNo;
        vFuncNode.ColumnNo := vNode.ColumnNo;
        DeleteChild(i);
      end;
    end
    else if vNode.Kind in [nkParenthesis, nkBracket] then
    begin
      vNode.RemoveSpaces;

      vExpressions := TList<TASTNode>.Create;
      vExpressionNode := TASTNode.Create(nkExpression, nil, vNode.EndPos);

      while vNode.Count > 0 do
      begin
        vChildNode := vNode[0];
        if (vChildNode.Kind = nkSymbol) and (vChildNode.Text = ',') then
        begin
          vExpressions.Add(vExpressionNode);
          vExpressionNode := TASTNode.Create(nkExpression, nil, vChildNode.EndPos);
          vNode.DeleteChild(0);
        end
        else
          vChildNode.Parent := vExpressionNode;
      end;
      vExpressions.Add(vExpressionNode);

      for vExpressionNode in vExpressions do
        vExpressionNode.Parent := vNode;

      FreeAndNil(vExpressions);
    end
    else if vNode.Kind = nkText then
      vNode.RemoveSpaces;
  end;
end;

procedure TASTNode.SetParent(const Value: TASTNode);
begin
  if Assigned(FParent) then
    FParent.ExtractChild(Self);
  FParent := Value;
  if Assigned(FParent) then
    FParent.AddChild(Self);
end;

{ TExpressionAST }

procedure TExpressionAST.AddChildNode(const ANodeKind: TNodeKind;
  const AToken: TToken);
begin
  FCurrentNode := AddNode(ANodeKind, AToken);
end;

function TExpressionAST.AddNode(const ANodeKind: TNodeKind;
  const AToken: TToken): TASTNode;
begin
  // Обработка текста, не захваченного регулярным выражением
  PickSkippedNode(AToken);

  Result := TASTNode.Create(ANodeKind, FCurrentNode, AToken.Position);
  Result.LineNo := AToken.LineNo;
  Result.ColumnNo := AToken.ColumnNo;
  Result.Text := AToken.Value;
  FLastNode := Result;
end;

procedure TExpressionAST.AddSpace(const AToken: TToken);
begin
  // Обработка текста, не захваченного регулярным выражением
  // Нужно, чтобы не потерять нераспознаваемый литерал между пробелами
  PickSkippedNode(AToken);

  if not Assigned(FLastNode) or (FLastNode.FKind <> nkSpace) then
    AddNode(nkSpace, AToken)
  else
    FLastNode.Text := FLastNode.Text + AToken.Value;
end;

procedure TExpressionAST.CloseNode;
begin
  if Assigned(FCurrentNode.Parent) then
    FCurrentNode := FCurrentNode.Parent;
end;

constructor TExpressionAST.Create;
begin
  inherited Create;
  FRootNode := TASTNode.Create(nkService, nil, 0);
  FCurrentNode := FRootNode;
  FLastNode := nil;
end;

destructor TExpressionAST.Destroy;
begin
  FLastNode := nil;
  FCurrentNode := nil;
  FreeAndNil(FRootNode);
  inherited Destroy;
end;

function TExpressionAST.HandleToken(const AToken: TToken): TTokenKinds;
begin
  Result := cDefaultTokens;

  case AToken.Kind of
    tkCommStart: begin
        AddChildNode(nkComment, AToken);
        Result := [tkCommEnd];
      end;
    tkCommEnd: begin
        AddNode(nkService, AToken);
        CloseNode;
      end;
    tkCommLine: begin
        AddChildNode(nkComment, AToken);
        Result := [tkLineEnd];
      end;
    tkLineEnd: begin
        if FCurrentNode.Kind = nkComment then
        begin
          PickSkippedNode(AToken);
          CloseNode;
        end;
        AddNode(nkLineEnd, AToken);
      end;
    tkSpace: AddSpace(AToken);
    tkParOpen: AddChildNode(nkParenthesis, AToken);
    tkParClose: begin
        // Проверка на правильность закрытия
        AddNode(nkService, AToken);
        CloseNode;
      end;
    tkBracketOpen: AddChildNode(nkBracket, AToken);
    tkBracketClose: begin
        // Проверка на правильность закрытия
        AddNode(nkService, AToken);
        CloseNode;
      end;
    tkComma: AddNode(nkSymbol, AToken);
    tkColon: AddNode(nkSymbol, AToken);
    tkSemicolon: AddNode(nkSymbol, AToken);
    tkApostrophe:
      if (FCurrentNode.Kind = nkText) and (FCurrentNode.Text = AToken.Value) then
      begin
        AddNode(nkService, AToken);
        CloseNode;
      end
      else begin
        AddChildNode(nkText, AToken);
        Result := [tkApostrophe];
      end;
    tkQuote:
      if (FCurrentNode.Kind = nkText) and (FCurrentNode.Text = AToken.Value) then
      begin
        AddNode(nkService, AToken);
        CloseNode;
      end
      else begin
        AddChildNode(nkText, AToken);
        Result := [tkQuote];
      end;
    tkEqual:
      if Assigned(FLastNode) and (FLastNode.Kind = nkOperator) and ((FLastNode.Text = '<') or (FLastNode.Text = '>') or (FLastNode.Text = '!')) then
        FLastNode.AppendText(AToken.Value)
      else
        AddNode(nkOperator, AToken);
    tkGreater:
      if Assigned(FLastNode) and (FLastNode.Kind = nkOperator) and ((FLastNode.Text = '<') or (FLastNode.Text = '>')) then
        FLastNode.AppendText(AToken.Value)
      else
        AddNode(nkOperator, AToken);
    tkLess:
      if Assigned(FLastNode) and (FLastNode.Kind = nkOperator) and (FLastNode.Text = '<') then
        FLastNode.AppendText(AToken.Value)
      else
        AddNode(nkOperator, AToken);
    tkMinus, tkPlus, tkAsterisk, tkSlash, tkPercent, tkCaret, tkExclamation, tkQuestion:
      AddNode(nkOperator, AToken);
    tkAmpersand:
      if Assigned(FLastNode) and (FLastNode.Kind = nkOperator) and (FLastNode.Text = '&') then
        FLastNode.AppendText(AToken.Value)
      else
        AddNode(nkOperator, AToken);
    tkVBar:
      if Assigned(FLastNode) and (FLastNode.Kind = nkOperator) and (FLastNode.Text = '|') then
        FLastNode.AppendText(AToken.Value)
      else
        AddNode(nkOperator, AToken);
    tkLiteral:
      AddNode(nkLiteral, AToken);
    tkEof: begin
        if FCurrentNode.Kind = nkComment then
        begin
          PickSkippedNode(AToken);
          CloseNode;
        end;
        AddNode(nkService, AToken);
      end;
  end;
end;

procedure TExpressionAST.PickSkippedNode(const AToken: TToken);
var
  vLastNodeEnd: Integer;
  vLastNodeLine: Integer;
  vLastNodeColumn: Integer;
  vLastNodeText: string;
  vTextNode: TASTNode;
begin
  if not Assigned(FLastNode) then
  begin
    vLastNodeEnd := 1;
    vLastNodeLine := 1;
    vLastNodeColumn := 1;
    vLastNodeText := '';
  end
  else begin
    vLastNodeEnd := FLastNode.EndPos;
    vLastNodeLine := FLastNode.LineNo;
    vLastNodeColumn := FLastNode.ColumnNo;
    vLastNodeText := FLastNode.Text;
  end;

  if AToken.Position <= vLastNodeEnd then
    Exit;

  vTextNode := TASTNode.Create(nkLiteral, FCurrentNode, vLastNodeEnd);
  vTextNode.LineNo := AToken.LineNo;
  if vLastNodeLine = AToken.LineNo then
    vTextNode.ColumnNo := vLastNodeColumn + Length(vLastNodeText)
  else
    vTextNode.ColumnNo := 1;
  vTextNode.Text := Copy(FText, vLastNodeEnd, AToken.Position - vLastNodeEnd);

  FLastNode := vTextNode;
end;

function TExpressionAST.Print: string;
begin
  Result := '>>> Начало обработки текста:' + PrintAST(FRootNode);
end;

function TExpressionAST.PrintAST(const ARootNode: TASTNode;
  const AIndent: Integer = 0): string;
var
  vChildNode: TASTNode;
  s: string;
begin
  Result := '';
  for vChildNode in ARootNode.FItems do
  begin
    s := vChildNode.Text;
    s := StringReplace(s, #13, '←', [rfReplaceAll]);
    s := StringReplace(s, #10, '¶', [rfReplaceAll]);
    s := StringReplace(s, #9, '¬', [rfReplaceAll]);
    if Length(s) > 80 then
      s := Copy(s, 1, 80) + Format('...(%d)', [Length(s)]);

    s := #13#10 + Format('%s (%d:%d pos: %d-%d) - "%s"',
      [StringOfChar(' ', AIndent * 2) + cNodeKindNames[vChildNode.Kind],
       vChildNode.LineNo, vChildNode.ColumnNo, vChildNode.FStartPos, vChildNode.EndPos, s]);

    Result := Result + s + PrintAST(vChildNode, AIndent + 1);
  end;
end;

procedure TExpressionAST.RemoveSpaces;
begin
  FRootNode.RemoveSpaces;
end;

procedure TExpressionAST.Tokenize(const AText: string);
var
  vExpectedTokens: TTokenKinds;
  vPattern: string;
  vKind: TTokenKind;
  vMatches: TMatchCollection;
  vMatch: TMatch;
  vValue: string;
  vLineNo: Integer;
  vStartLinePos: Integer;
  vToken: TToken;
begin
  vPattern := '';
  for vKind := tkFirst to tkLast do
  begin
    if vKind <> tkFirst then
      vPattern := vPattern + '|';
    vPattern := vPattern + Format('(?P<%s>%s)', [cTokenNames[vKind], cTokenRE[vKind]]);
  end;

  FText := AText;
  FLastNode := nil;

  vLineNo := 1;
  vStartLinePos := 0;
  vExpectedTokens := cDefaultTokens;

  vMatches := TRegEx.Matches(FText, vPattern, [roMultiLine]);
  for vMatch in vMatches do
  begin
    // При совпадении с текстовой группой добавление групп в Match заканчивается
    vKind := TTokenKind(vMatch.Groups.Count - 2 - Integer(tkFirst));
    vValue := vMatch.Value;
    vToken.LineNo := vLineNo;
    vToken.ColumnNo := vMatch.Index - vStartLinePos;
    if vKind = tkLineEnd then
    begin
      vStartLinePos := vMatch.Index + Length(vValue) - 1;
      vLineNo := vLineNo + 1;
    end;

    if vKind in vExpectedTokens then
    begin
      vToken.Kind := vKind;
      vToken.Value := vValue;
      vToken.Position := vMatch.Index;

      vExpectedTokens := HandleToken(vToken);
    end;
  end;

  // Обработка конца файла
  vToken.Kind := tkEof;
  vToken.Position := Length(FText) + 1;
  vToken.LineNo := vLineNo;
  vToken.ColumnNo := vToken.Position - vStartLinePos;
  vToken.Value := '';
  HandleToken(vToken);

  Assert(FCurrentNode = FRootNode, 'Ошибка при парсинге');
end;

{ TCalcNode }

constructor TCalcNode.Create(const AOwner: TCalcTree);
begin
  inherited Create;
  FOwner := AOwner;
end;

destructor TCalcNode.Destroy;
begin
  FOwner := nil;
  inherited Destroy;
end;

function TCalcNode.GetValue: Variant;
begin
  Result := Null;
end;

function TCalcNode.Print: string;
begin
  Result := '';
end;

{ TVariableNode }

constructor TVariableNode.Create(const AOwner: TCalcTree; const AVarName: string);
begin
  inherited Create(AOwner);
  FVarName := AVarName;
end;

function TVariableNode.GetValue: Variant;
begin
  Result := FOwner.VarValueByName(FVarName);
end;

function TVariableNode.Print: string;
begin
  Result := ':' + FVarName + ':';
end;

{ TCalcTree }

function TCalcTree.Calculate(const ACalcEnv: TCalcEnvironment): Variant;
begin
  FCalcEnv := ACalcEnv;
  try
    Result := FRootNode.Value;
  finally
    FCalcEnv := nil;
  end;
end;

constructor TCalcTree.Create(const AAST: TExpressionAST; const AValuesOnly: Boolean);
begin
  inherited Create;
  AAST.RemoveSpaces;
  //ShowMessage(AAST.Print);
  FRootNode := CreateCalcNode(AAST.RootNode, AValuesOnly);
  //ShowMessage(Print);
end;

function TCalcTree.CreateCalcNode(const ANode: TASTNode; const AValuesOnly: Boolean): TCalcNode;
var
  vChildNode: TASTNode;
  vCalcNode: TOperationNode;
  vOperatorKind: TOperatorKind;
  vLeftNode: TCalcNode;
  vIntValue: Integer;
  vFloatValue: Extended;
  vNodeText: string;
  vWaitedOperators: TList<TCalcNode>;
  vLastOperator: TOperationNode;
begin
  if ANode.Count = 0 then
  begin
    Result := TValueNode.Create(Self, '');
    Exit;
  end;

  vLeftNode := nil;
  vWaitedOperators := TList<TCalcNode>.Create;
  for vChildNode in ANode do
  begin
    vNodeText := vChildNode.Text;
    if vChildNode.Kind = nkParenthesis then
      vLeftNode := CreateCalcNode(vChildNode, False)
    else if vChildNode.Kind = nkBracket then
      Continue
    else if vChildNode.Kind = nkExpression then
      vLeftNode := CreateCalcNode(vChildNode, False)
    else if vChildNode.Kind = nkText then
      vLeftNode := TValueNode.Create(Self, vChildNode.ChildText)
    else if vChildNode.Kind = nkOperator then
    begin
      vCalcNode := CreateOperatorNode(vLeftNode, vNodeText, vOperatorKind);

      while vWaitedOperators.Count > 0 do
      begin
        vLastOperator := TOperationNode(vWaitedOperators.Last);
        if cOperatorPriorities[vOperatorKind] > cOperatorPriorities[vLastOperator.FKind] then
          Break;

        vLastOperator.SetOperand(vLeftNode);
        vLeftNode := vLastOperator;
        vWaitedOperators.Delete(vWaitedOperators.Count - 1);
      end;

      if (vCalcNode is TBinaryOperationNode) then
        TBinaryOperationNode(vCalcNode).SetLeftOperand(vLeftNode);
      vWaitedOperators.Add(vCalcNode);
      vLeftNode := nil;
    end
    else if vChildNode.Kind = nkFunction then
      vLeftNode := TFunctionNode.Create(Self, vChildNode)
    else if vChildNode.Kind = nkLiteral then
    begin
      if vNodeText = '' then
        vLeftNode := TValueNode.Create(Self, '')
      else if SameText(vNodeText, 'True') then
        vLeftNode := TValueNode.Create(Self, True)
      else if SameText(vNodeText, 'False') then
        vLeftNode := TValueNode.Create(Self, False)
      else if TryStrToInt(vNodeText, vIntValue) then
        vLeftNode := TValueNode.Create(Self, vIntValue)
      else if TryStrToFloat(ReplaceStr(vNodeText, '.', FormatSettings.DecimalSeparator), vFloatValue) then
        vLeftNode := TValueNode.Create(Self, vFloatValue)
      else if AValuesOnly then
        vLeftNode := TValueNode.Create(Self, vNodeText)
      else if vNodeText.Chars[0].IsLetter then
        vLeftNode := TVariableNode.Create(Self, vNodeText)
      else
        Assert(False);
    end
    else
      Assert(False);
  end;

  while vWaitedOperators.Count > 0 do
  begin
    vLastOperator := TOperationNode(vWaitedOperators.Last);
    vLastOperator.SetOperand(vLeftNode);
    vLeftNode := vLastOperator;
    vWaitedOperators.Delete(vWaitedOperators.Count - 1);
  end;

  FreeAndNil(vWaitedOperators);

  Result := vLeftNode;
end;

function TCalcTree.CreateOperatorNode(const ALeftNode: TCalcNode; const AOperator: string; var AKind: TOperatorKind): TOperationNode;

  function OperatorKindByText(const AText: string): TOperatorKind;
  begin
    for Result := Low(TOperatorKind) to High(TOperatorKind) do
      if SameText(cOperatorTexts[Result], AText) then
        Exit;
    Result := okNone;
  end;

begin
  AKind := OperatorKindByText(AOperator);
  if (AKind = okNeg) and Assigned(ALeftNode) and not (ALeftNode is TOperationNode) then
    AKind := okSub;

  if AKind in [okNeg, okNot] then
    Result := TUnaryOperationNode.Create(Self, AKind)
  else
    Result := TBinaryOperationNode.Create(Self, AKind);
end;

destructor TCalcTree.Destroy;
begin
  FCalcEnv := nil;
  FreeAndNil(FRootNode);
  inherited Destroy;
end;

function TCalcTree.ExecuteFunction(const AFuncName: string; const AParams: array of Variant): Variant;
begin
  if Assigned(FCalcEnv) then
    Result := FCalcEnv.DoExecuteFunction(AFuncName, AParams)
  else
    Result := Null;
end;

function TCalcTree.Print: string;
begin
  Result := FRootNode.Print;
end;

function TCalcTree.VarValueByName(const AVarName: string): Variant;
begin
  if Assigned(FCalcEnv) then
    Result := FCalcEnv.GetVariableValue(AVarName)
  else
    Result := Null;
end;

{ TFunctionNode }

constructor TFunctionNode.Create(const AOwner: TCalcTree; const ANode: TASTNode);
var
  vChildNode: TASTNode;
begin
  inherited Create(AOwner);

  FName := ANode.Text;
  FParameters := TObjectList<TCalcNode>.Create;
  for vChildNode in ANode do
    FParameters.Add(FOwner.CreateCalcNode(vChildNode, False));

  SetLength(FParamValues, ANode.Count);
end;

destructor TFunctionNode.Destroy;
begin
  SetLength(FParamValues, 0);
  FreeAndNil(FParameters);

  inherited Destroy;
end;

function TFunctionNode.GetValue: Variant;
var
  i: Integer;
begin
  for i := 0 to FParameters.Count - 1 do
    FParamValues[i] := FParameters[i].Value;

  Result := FOwner.ExecuteFunction(FName, FParamValues);
end;

function TFunctionNode.Print: string;
var
  vCalcNode: TCalcNode;
begin
  Result := '';
  for vCalcNode in FParameters do
  begin
    if Result <> '' then
      Result := Result + ', ' + vCalcNode.Print
    else
      Result := Result + vCalcNode.Print;
  end;
  Result := FName + '(' + Result + ')';
end;

{ TValueNode }

constructor TValueNode.Create(const AOwner: TCalcTree; const AValue: Variant);
begin
  inherited Create(AOwner);
  FValue := AValue;
end;

function TValueNode.GetValue: Variant;
begin
  Result := FValue;
end;

function TValueNode.Print: string;
begin
  Result := VarToStr(FValue);
end;

{ TOperationNode }

constructor TOperationNode.Create(const AOwner: TCalcTree; const AKind: TOperatorKind);
begin
  inherited Create(AOwner);
  FOperand := nil;
  FKind := AKind;
end;

destructor TOperationNode.Destroy;
begin
  FreeAndNil(FOperand);
  inherited Destroy;
end;

procedure TOperationNode.SetOperand(const AOperand: TCalcNode);
begin
  FOperand := AOperand;
end;

{ TUnaryOperationNode }

function TUnaryOperationNode.GetValue: Variant;
begin
  case FKind of
    okNeg: Result := -FOperand.Value;
    okNot: Result := not FOperand.Value;
  else
    Result := Null;
  end;
end;

function TUnaryOperationNode.Print: string;
begin
  Result := cOperatorTexts[FKind] + '(' + FOperand.Print + ')';
end;

{ TBinaryOperationNode }

constructor TBinaryOperationNode.Create(const AOwner: TCalcTree; const AKind: TOperatorKind);
begin
  inherited Create(AOwner, AKind);
  FLeftOperand := nil;
end;

destructor TBinaryOperationNode.Destroy;
begin
  FreeAndNil(FLeftOperand);
  inherited Destroy;
end;

function TBinaryOperationNode.GetValue: Variant;
begin
  case FKind of
    okAdd: Result := FLeftOperand.Value + FOperand.Value;
    okSub: Result := FLeftOperand.Value - FOperand.Value;
    okMul: Result := FLeftOperand.Value * FOperand.Value;
    okDiv:
      if VarIsOrdinal(FLeftOperand.Value) and VarIsOrdinal(FOperand.Value) then
        Result := FLeftOperand.Value div FOperand.Value
      else
        Result := FLeftOperand.Value / FOperand.Value;
    okMod: Result := FLeftOperand.Value mod FOperand.Value;
    okPower: Result := IntPower(FLeftOperand.Value, FOperand.Value);
    okEq: Result := FLeftOperand.Value = FOperand.Value;
    okNotEq: Result := FLeftOperand.Value <> FOperand.Value;
    okGreater: Result := FLeftOperand.Value > FOperand.Value;
    okLess: Result := FLeftOperand.Value < FOperand.Value;
    okGreaterOrEg: Result := FLeftOperand.Value >= FOperand.Value;
    okLessOrEg: Result := FLeftOperand.Value <= FOperand.Value;
    okAnd, okBitAnd: Result := FLeftOperand.Value and FOperand.Value;
    okOr, okBitOr: Result := FLeftOperand.Value or FOperand.Value;
    okBitShl: Result := FLeftOperand.Value shl FOperand.Value;
    okBitShr: Result := FLeftOperand.Value shr FOperand.Value;
  else
    Result := Null;
  end;
end;

function TBinaryOperationNode.Print: string;
begin
  Result := '(' + FLeftOperand.Print + ' ' + cOperatorTexts[FKind] + ' ' + FOperand.Print + ')';
end;

procedure TBinaryOperationNode.SetLeftOperand(const AOperand: TCalcNode);
begin
  FLeftOperand := AOperand;
end;

end.
