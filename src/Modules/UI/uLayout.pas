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

unit uLayout;

interface

uses
  Classes, Generics.Collections, UITypes, uJSON, uConsts;

type
  TLayoutKind = (lkNone, lkPanel, lkPage, lkPages, lkFrame, lkMemo, lkLabel, lkImage, lkBevel, lkShape,
    lkSplitter, lkScrollBox, lkGroup, lkAction);
  TLayoutAlign = (lalNone, lalTop, lalBottom, lalLeft, lalRight, lalClient, lalCustom);
  TPageStyle = (psTabs, psButtons, psFlatButtons);
  TPagePosition = (ppTop, ppBottom, ppLeft, ppRight);
  TLayoutShapeType = (lstRectangle, lstSquare, lstRoundRect, lstRoundSquare, lstEllipse, lstCircle);
  TLayoutBevelStyle = (lbsLowered, lbsRaised);
  TLayoutBevelShape = (lbsBox, lbsFrame, lbsTopLine, lbsBottomLine, lbsLeftLine, lbsRightLine, lbsSpacer);
  TLayoutBevelKind = (bkNone, bkLowered, bkRaised, bkSpace);
  TLayoutViewType = (lvtPanel, lvtPages, lvtAction);
  TLayoutBorderStyle = (lbsNone, lbsSingle, lbsSizeable, lbsDialog, lbsToolWindow, lbsSizeToolWin);

  TLayoutEdges = class
  private
    FLeft, FTop, FRight, FBottom: Integer;
    procedure InternalLoad(const AJSON: TJSONObject);
    function InternalSave: TJSONObject;
  public
    constructor Create; overload;
    constructor Create(const AJSON: TJSONObject); overload;
    constructor Create(const ALeft, ATop, ARight, ABottom: Integer); overload;

    procedure Save(const AParent: TJSONObject; const AName: string);
    function IsEmpty: Boolean;

    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Right: Integer read FRight write FRight;
    property Bottom: Integer read FBottom write FBottom;
  end;

  TLayoutMargins = TLayoutEdges;
  TLayoutPadding = TLayoutEdges;

  TLayoutFont = class
  private
    FColor: TAlphaColor;
    FFamily: string;
    FSize: Integer;
    FStyle: TFontStyles;
    procedure InternalLoad(const AJSON: TJSONObject);
    function InternalSave: TJSONObject;
  public
    constructor Create; overload;
    constructor Create(const AJSON: TJSONObject); overload;
    constructor Create(const AColor: TAlphaColor; const AFamily: string; const ASize: Integer;
      const AStyle: TFontStyles); overload;

    procedure Save(const AParent: TJSONObject; const AName: string);

    property Color: TAlphaColor read FColor write FColor;
    property Family: string read FFamily write FFamily;
    property Size: Integer read FSize write FSize;
    property Style: TFontStyles read FStyle write FStyle;
  end;

  TLayoutPen = class
  private
    FColor: TAlphaColor;
    FWidth: Integer;
    procedure InternalLoad(const AJSON: TJSONObject);
    function InternalSave: TJSONObject;
  public
    constructor Create; overload;
    constructor Create(const AJSON: TJSONObject); overload;
    constructor Create(const AColor: TAlphaColor; const AWidth: Integer); overload;

    procedure Save(const AParent: TJSONObject; const AName: string);

    property Color: TAlphaColor read FColor write FColor;
    property Width: Integer read FWidth write FWidth;
  end;

  TLayoutBrush = class
  private
    FColor: TAlphaColor;
    procedure InternalLoad(const AJSON: TJSONObject);
    function InternalSave: TJSONObject;
  public
    constructor Create; overload;
    constructor Create(const AJSON: TJSONObject); overload;
    constructor Create(const AColor: TAlphaColor); overload;

    procedure Save(const AParent: TJSONObject; const AName: string);

    property Color: TAlphaColor read FColor write FColor;
  end;

  TLayoutConstraints = class
  private
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FMinHeight: Integer;
    FMaxHeight: Integer;
    procedure InternalLoad(const AJSON: TJSONObject);
    function InternalSave: TJSONObject;
  public
    constructor Create; overload;
    constructor Create(const AJSON: TJSONObject); overload;
    constructor Create(const AMinWidth, AMaxWidth, AMinHeight, AMaxHeight: Integer); overload;

    procedure Save(const AParent: TJSONObject; const AName: string);

    property MinWidth: Integer read FMinWidth write FMinWidth;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth;
    property MinHeight: Integer read FMinHeight write FMinHeight;
    property MaxHeight: Integer read FMaxHeight write FMaxHeight;
  end;

const
  cLayoutKindNames: array[TLayoutKind] of string = ('', 'panel', 'page', 'pages', 'frame', 'memo', 'label',
    'image', 'bevel', 'shape', 'splitter', 'scrollbox', 'group', 'action');
  cAnchorKindNames: array[TAnchorKind] of string = ('left', 'top', 'right', 'bottom');
  cFontStyleNames: array[TFontStyle] of string = ('bold', 'italic', 'underline', 'strikeout');
  cAlignNames: array[TLayoutAlign] of string = ('', 'top', 'bottom', 'left', 'right', 'client', 'custom');
  cAlignmentNames: array[TAlignment] of string = ('left-justify', 'right-justify', 'center');
  cPageStyleNames: array[TPageStyle] of string = ('tabs', 'buttons', 'flat-buttons');
  cPagePositionNames: array[TPagePosition] of string = ('top', 'bottom', 'left', 'right');
  cShapeTypeNames: array[TLayoutShapeType] of string = ('rectangle', 'square', 'round-rect', 'round-square', 'ellipse', 'circle');
  cBevelKindNames: array[TLayoutBevelKind] of string = ('', 'lowered', 'raised', 'space');
  cBevelStyleNames: array[TLayoutBevelStyle] of string = ('lowered', 'raised');
  cBevelShapeNames: array[TLayoutBevelShape] of string = ('box', 'frame', 'top-line', 'bottom-line', 'left-line',
    'right-line', 'spacer');
  cViewTypeNames: array[TLayoutViewType] of string = ('panel', 'pages', 'action');
  cBorderStyleNames: array[TLayoutBorderStyle] of string = ('', 'single', 'sizeable', 'dialog', 'tool-window', 'size-tool-window');

function StrToLayoutKind(const s: string): TLayoutKind;
function StrToViewState(const s: string): TViewState;
function StrToAlign(const s: string): TLayoutAlign;
function StrToAlignment(const s: string): TAlignment;
function StrToPagePosition(const s: string): TPagePosition;
function StrToPageStyle(const s: string): TPageStyle;
function StrToShapeType(const s: string): TLayoutShapeType;
function StrToBevelKind(const s: string): TLayoutBevelKind;
function StrToBevelStyle(const s: string): TLayoutBevelStyle;
function StrToBevelShape(const s: string): TLayoutBevelShape;
function StrToViewType(const s: string): TLayoutViewType;
function StrToBorderStyle(const s: string): TLayoutBorderStyle;

type
  TUrlParser = class
  private
    FPath: string;
    FParams: TStrings;
  public
    constructor Create(const AUrl: string);
    destructor Destroy; override;

    function ExtractInteger(const AParamName: string; const ADefault: Integer = -1): Integer;
    function ExtractString(const AParamName: string; const ADefault: string = ''): string;

    property Path: string read FPath;
  end;

  TNavigationItem = class;
  TNavigationItems = TObjectList<TNavigationItem>;

  TLayout = class
  private
    FContentLayout: TLayout;
    FMenu: TNavigationItem;
    FControl: TObject;
    FUrlParser: TUrlParser;
    FKind: TLayoutKind;
    FIsOwner: Boolean;
    FIndex: Integer;
    FPresenter: TObject;

    // Размеры и расположение
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FAnchors: TAnchors;
    FAlignWithMargins: Boolean;
    FAlign: TLayoutAlign;
    FMargins: TLayoutMargins;
    FPadding: TLayoutPadding;
    FConstraints: TLayoutConstraints;

    // Надписи и внутреннее состояние
    FName: string;
    FHint: string;
    FShowCaption: Boolean;
    FCaption: string;
    FUIParams: string;
    FTag: NativeInt;
    FImageIndex: Integer;
    FState: TViewState;
    FColor: TAlphaColor;
    FTransparent: Boolean;
    FAlignment: TAlignment;
    FAutoSize: Boolean;
    FWordWrap: Boolean;

    // Графические объекты
    FPen: TLayoutPen;
    FBrush: TLayoutBrush;
    FFont: TLayoutFont;

    // Свойства конкретных типов
    FShape_Type: TLayoutShapeType;
    FImage_Picture: TStream;
    FImage_Stretch: Boolean;
    FImage_Proportional: Boolean;
    FImage_Center: Boolean;
    FPage_Style: TPageStyle;
    FPage_Position: TPagePosition;
    FPage_Height: Integer;
    FPage_Width: Integer;

    procedure SetContentLayout(const Value: TLayout);
    function GetItems: TList<TLayout>;
    procedure SetMenu(const Value: TNavigationItem);
    procedure SetPictureStream(const Value: TStream);
  protected
    [Weak] FParent: TLayout;
    FItems: TList<TLayout>;

    procedure InternalLoad(const AJSON: TJSONObject); virtual;
    function InternalSave: TJSONObject; virtual;
  public
    class var _Count: Integer;

    constructor Create(const AKind: TLayoutKind; const AControl: TObject; const AIsOwner: Boolean = False);
    destructor Destroy; override;

    procedure Add(const AChild: TLayout);
    procedure Save(const AParent: TJSONObject; const AName: string);
    procedure ArrangeChildAreas;
    procedure SaveToDFM(const AFileName: string);

    procedure SetUrl(const AUrl: string); virtual;
    function ExtractInteger(const AParamName: string; const ADefault: Integer = -1): Integer;
    function ExtractString(const AParamName: string; const ADefault: string = ''): string;

    property Kind: TLayoutKind read FKind;
    property Control: TObject read FControl;
    property Parent: TLayout read FParent;
    property Items: TList<TLayout> read GetItems;

    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Anchors: TAnchors read FAnchors write FAnchors;
    property AlignWithMargins: Boolean read FAlignWithMargins write FAlignWithMargins;
    property Align: TLayoutAlign read FAlign write FAlign;
    property Margins: TLayoutMargins read FMargins;
    property Padding: TLayoutPadding read FPadding;
    property Constraints: TLayoutConstraints read FConstraints;
    property Alignment: TAlignment read FAlignment write FAlignment;
    property Color: TAlphaColor read FColor write FColor;
    property Transparent: Boolean read FTransparent write FTransparent;

    property Name: string read FName write FName;
    property Tag: NativeInt read FTag write FTag;
    property Caption: string read FCaption write FCaption;
    property ShowCaption: Boolean read FShowCaption write FShowCaption;
    property Hint: string read FHint write FHint;
    property UIParams: string read FUIParams write FUIParams;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property State: TViewState read FState write FState;

    property Pen: TLayoutPen read FPen;
    property Brush: TLayoutBrush read FBrush;
    property Font: TLayoutFont read FFont;

    property Shape_Type: TLayoutShapeType read FShape_Type write FShape_Type;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property Image_Picture: TStream read FImage_Picture write SetPictureStream;
    property Image_Stretch: Boolean read FImage_Stretch write FImage_Stretch;
    property Image_Proportional: Boolean read FImage_Proportional write FImage_Proportional;
    property Image_Center: Boolean read FImage_Center write FImage_Center;
    property Page_Style: TPageStyle read FPage_Style write FPage_Style;
    property Page_Position: TPagePosition read FPage_Position write FPage_Position;
    property Page_Height: Integer read FPage_Height write FPage_Height;
    property Page_Width: Integer read FPage_Width write FPage_Width;

    property ContentLayout: TLayout read FContentLayout write SetContentLayout;
    property Menu: TNavigationItem read FMenu write SetMenu;
    property IsOwner: Boolean read FIsOwner;
    property _Index: Integer read FIndex;
    property Presenter: TObject read FPresenter write FPresenter;
  end;

  TNavigationItem = class(TLayout)
  private
    FOwner: TLayout;
    FId: string;
    FCaption: string;
    FHint: string;
    FImageID: Integer;
    FViewName: string;
    FContentLayout: string;
    FContentWorkArea: string;
    FContentCaption: string;
    FGroupIndex: Byte;
    FRadioItem: Boolean;
    function GetOwner: TLayout;
  protected
    procedure InternalLoad(const AJSON: TJSONObject); override;
    function InternalSave: TJSONObject; override;
  public
    constructor Create(const AParent: TNavigationItem; const AJSON: TJSONObject); overload;
    constructor Create(const AParent: TNavigationItem; const AUrl: string); overload;
    destructor Destroy; override;

    function Add(const ACaption: string): TNavigationItem;
    function Insert(const AIndex: Integer; const ACaption: string): TNavigationItem;
    function IsLine: Boolean;
    procedure SetUrl(const AUrl: string); override;

    property Id: string read FId;
    property Caption: string read FCaption;
    property Hint: string read FHint;
    property ImageID: Integer read FImageID;
    property ViewName: string read FViewName;
    property ContentLayout: string read FContentLayout;
    property ContentWorkArea: string read FContentWorkArea;
    property ContentCaption: string read FContentCaption;
    property GroupIndex: Byte read FGroupIndex write FGroupIndex;
    property RadioItem: Boolean read FRadioItem write FRadioItem;
    property Owner: TLayout read GetOwner;
  end;

  TLayoutItem = class(TLayout)
  end;

  TLayoutX = class
  private
    [Weak] FParent: TLayoutX;
    FItems: TObjectList<TLayoutX>;
    FMenu: TNavigationItem;

    FLayoutKind: TLayoutKind;
    FViewType: TLayoutViewType;
    FName: string;
    FCaption: string;
    FUIParams: string;
    FHint: string;
    FShowCaption: Boolean;
    FShowHint: Boolean;
    FImageIndex: Integer;
    FTag: NativeInt;

    // Размеры и расположение
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FAlign: TLayoutAlign;
    FAnchors: TAnchors;
    FAlignWithMargins: Boolean;
    FMargins: TLayoutMargins;

    FPadding: TLayoutPadding;
    FConstraints: TLayoutConstraints;
    FState: TViewState;
    FDoubleBuffered: Boolean;
    FTabStop: Boolean;
    FTabOrder: Integer;

    FColor: TAlphaColor;
    FFont: TLayoutFont;
    FAlignment: TAlignment;

    FPageStyle: TPageStyle;
    FPagePosition: TPagePosition;
    FPageHeight: Integer;
    FPageWidth: Integer;
    FHidePages: Boolean;

    FBevelInner: TLayoutBevelKind;
    FBevelOuter: TLayoutBevelKind;
    FBorderStyle: TLayoutBorderStyle;
    FParentBackground: Boolean;

    FAutoSize: Boolean;
    FTransparent: Boolean;
    FWordWrap: Boolean;

    FPicture: TStream;
    FStretch: Boolean;
    FProportional: Boolean;

    FBevelStyle: TLayoutBevelStyle;
    FBevelShape: TLayoutBevelShape;
    FCursor: Integer;
    FParams: string;

    FStyleName: string;
    FChildLayoutName: string;
    FTargetWorkAreaName: string;
    FTargetLayoutName: string;
    FTargetViewName: string;

    procedure InternalLoad(const AJSON: TJSONObject);
    function InternalSave: TJSONObject;
    procedure SetPictureStream(const Value: TStream);
  public
    constructor Create(const AParent: TLayoutX; const ALayoutKind: TLayoutKind = lkNone;
      const AParams: string = '');
    destructor Destroy; override;

    procedure Load(const AFileName: string);
    procedure Save(const AFileName: string);
    function AddChild: TLayoutX;
    procedure Add(const AChild: TLayoutX);


    property UIParams: string read FUIParams write FUIParams;


    property Padding: TLayoutPadding read FPadding;
    property Constraints: TLayoutConstraints read FConstraints;
    property Color: TAlphaColor read FColor write FColor;
    property Font: TLayoutFont read FFont;
    property State: TViewState read FState write FState;
    property TabStop: Boolean read FTabStop write FTabStop;
    property TabOrder: Integer read FTabOrder write FTabOrder;

    property Alignment: TAlignment read FAlignment write FAlignment;
    property PageStyle: TPageStyle read FPageStyle write FPageStyle;
    property PagePosition: TPagePosition read FPagePosition write FPagePosition;
    property PageHeight: Integer read FPageHeight write FPageHeight;
    property PageWidth: Integer read FPageWidth write FPageWidth;
    property HidePages: Boolean read FHidePages write FHidePages;
    property BevelInner: TLayoutBevelKind read FBevelInner write FBevelInner;
    property BevelOuter: TLayoutBevelKind read FBevelOuter write FBevelOuter;
    property BorderStyle: TLayoutBorderStyle read FBorderStyle write FBorderStyle;
    property ParentBackground: Boolean read FParentBackground write FParentBackground;
    property AutoSize: Boolean read FAutoSize write FAutoSize;
    property Transparent: Boolean read FTransparent write FTransparent;
    property WordWrap: Boolean read FWordWrap write FWordWrap;
    property Picture: TStream read FPicture write SetPictureStream;
    property Stretch: Boolean read FStretch write FStretch;
    property Proportional: Boolean read FProportional write FProportional;
    property BevelStyle: TLayoutBevelStyle read FBevelStyle write FBevelStyle;
    property BevelShape: TLayoutBevelShape read FBevelShape write FBevelShape;
    property Cursor: Integer read FCursor write FCursor;

    property Kind: TLayoutKind read FLayoutKind;
    property ViewType: TLayoutViewType read FViewType write FViewType;
    property StyleName: string read FStyleName write FStyleName;
    property Params: string read FParams write FParams;

    property ChildLayoutName: string read FChildLayoutName write FChildLayoutName;
    property TargetWorkAreaName: string read FTargetWorkAreaName write FTargetWorkAreaName;
    property TargetLayoutName: string read FTargetLayoutName write FTargetLayoutName;
    property TargetViewName: string read FTargetViewName write FTargetViewName;

    property Items: TObjectList<TLayoutX> read FItems;
    property Menu: TNavigationItem read FMenu write FMenu;
  end;

const
  cDefaultColumnWidth = 250;
  cBetweenRows = 27;

implementation

uses
  IOUtils, TypInfo, SysUtils, Math, uDefinition, uUtils, uPresenter, StrUtils, uDomainUtils, Types;

type
  TLayoutParam = class
    Name: string;
    Value: Variant;
    constructor Create(const AName: string);
  end;

  TUILayout = class
  private
    FParams: TList;
    FParent: TUILayout;
    FChildLayouts: TList;
    FName: string;
    FType: string;
    FLevel: Integer;
    procedure Clear;
    procedure FillLayoutParams(const AComponent: TComponent; const ALayout: TLayout);
    procedure GenerateDFMText(const AText: TStrings; const ALevel: Integer);
    procedure GeneratePASText(const AText: TStrings);
    function FindChild(const AName: string): TUILayout;
    function ParamByName(const AName: string): TLayoutParam;
    function CreateChild: TUILayout;
    function GenerateUniqueName(const AComponent: TComponent): string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Build(const ALayout: TLayout);
    procedure SaveToDFM(const AFileName: string);
  end;


function StrToLayoutKind(const s: string): TLayoutKind;
var
  vKind: TLayoutKind;
begin
  for vKind := Low(TLayoutKind) to High(TLayoutKind) do
    if SameText(cLayoutKindNames[vKind], s) then
      Exit(vKind);
  Result := lkNone;
end;

function StrToViewState(const s: string): TViewState;
begin
  if SameText('hidden', s) then
    Result := vsHidden
  else if SameText('disabled', s) then
    Result := vsDisabled
  else if SameText('read-only', s) then
    Result := vsReadOnly
  else if SameText('select-only', s) then
    Result := vsSelectOnly
  else if SameText('full-access', s) then
    Result := vsFullAccess
  else
    Result := vsUndefined;
end;

function ViewStateToStr(const AViewState: TViewState): string;
begin
  case AViewState of
    vsHidden: Result := 'hidden';
    vsDisabled: Result := 'disabled';
    vsReadOnly: Result := 'read-only';
    vsSelectOnly: Result := 'select-only';
    vsFullAccess: Result := 'full-access';
  else
    Result := '';
  end;
end;

function StrToAlign(const s: string): TLayoutAlign;
var
  vAlign: TLayoutAlign;
begin
  for vAlign := Low(TLayoutAlign) to High(TLayoutAlign) do
    if SameText(cAlignNames[vAlign], s) then
      Exit(vAlign);
  Result := lalNone;
end;

function StrToAlignment(const s: string): TAlignment;
var
  vAlignment: TAlignment;
begin
  for vAlignment := Low(TAlignment) to High(TAlignment) do
    if SameText(cAlignmentNames[vAlignment], s) then
      Exit(vAlignment);
  Result := taLeftJustify;
end;

function StrToPagePosition(const s: string): TPagePosition;
var
  vPagePosition: TPagePosition;
begin
  for vPagePosition := Low(TPagePosition) to High(TPagePosition) do
    if SameText(cPagePositionNames[vPagePosition], s) then
      Exit(vPagePosition);
  Result := ppTop;
end;

function StrToPageStyle(const s: string): TPageStyle;
var
  vPageStyle: TPageStyle;
begin
  for vPageStyle := Low(TPageStyle) to High(TPageStyle) do
    if SameText(cPageStyleNames[vPageStyle], s) then
      Exit(vPageStyle);
  Result := psTabs;
end;

function StrToShapeType(const s: string): TLayoutShapeType;
var
  vShapeType: TLayoutShapeType;
begin
  for vShapeType := Low(TLayoutShapeType) to High(TLayoutShapeType) do
    if SameText(cShapeTypeNames[vShapeType], s) then
      Exit(vShapeType);
  Result := lstRectangle;
end;

function StrToBevelKind(const s: string): TLayoutBevelKind;
var
  vBevelKind: TLayoutBevelKind;
begin
  for vBevelKind := Low(TLayoutBevelKind) to High(TLayoutBevelKind) do
    if SameText(cBevelKindNames[vBevelKind], s) then
      Exit(vBevelKind);
  Result := bkNone;
end;

function StrToBevelStyle(const s: string): TLayoutBevelStyle;
var
  vBevelStyle: TLayoutBevelStyle;
begin
  for vBevelStyle := Low(TLayoutBevelStyle) to High(TLayoutBevelStyle) do
    if SameText(cBevelStyleNames[vBevelStyle], s) then
      Exit(vBevelStyle);
  Result := lbsLowered;
end;

function StrToBevelShape(const s: string): TLayoutBevelShape;
var
  vBevelShape: TLayoutBevelShape;
begin
  for vBevelShape := Low(TLayoutBevelShape) to High(TLayoutBevelShape) do
    if SameText(cBevelShapeNames[vBevelShape], s) then
      Exit(vBevelShape);
  Result := lbsBox;
end;

function StrToViewType(const s: string): TLayoutViewType;
var
  vViewType: TLayoutViewType;
begin
  for vViewType := Low(TLayoutViewType) to High(TLayoutViewType) do
    if SameText(cViewTypeNames[vViewType], s) then
      Exit(vViewType);
  Result := lvtPanel;
end;

function StrToBorderStyle(const s: string): TLayoutBorderStyle;
var
  vBorderStyle: TLayoutBorderStyle;
begin
  for vBorderStyle := Low(TLayoutBorderStyle) to High(TLayoutBorderStyle) do
    if SameText(cBorderStyleNames[vBorderStyle], s) then
      Exit(vBorderStyle);
  Result := lbsSingle;
end;

function StrInList(const s: string; const AList: TStrings): Boolean;
var
  i: Integer;
begin
  for i := 0 to AList.Count - 1 do
    if SameText(AList[i], s) then
      Exit(True);
  Result := False;
end;

function BinaryToText(const AStream: TStream): string;
var
  vStrStream: TStringStream;
begin
  vStrStream := TStringStream.Create;
  try
    vStrStream.CopyFrom(AStream, -1);
    vStrStream.Position := 0;
    Result := EncodeBase64(vStrStream.DataString);
  finally
    vStrStream.Free;
  end;
end;

function TextToBinary(const AText: string): TStream;
var
  vDecodedString: string;
  vStrStream: TStringStream;
begin
  vDecodedString := DecodeBase64(AText);
  Result := TMemoryStream.Create;
  vStrStream := TStringStream.Create;
  try
    vStrStream.WriteString(vDecodedString);
    Result.CopyFrom(vStrStream, -1);
    Result.Position := 0;
  finally
    vStrStream.Free;
  end;
end;

{ TLayoutX }

procedure TLayoutX.Add(const AChild: TLayoutX);
begin
  AChild.FParent := Self;
  FItems.Add(AChild);
end;

function TLayoutX.AddChild: TLayoutX;
begin
  Result := TLayoutX.Create(Self);
  FItems.Add(Result);
end;

constructor TLayoutX.Create(const AParent: TLayoutX; const ALayoutKind: TLayoutKind = lkNone; const AParams: string = '');
var
  vParams: TStrings;
begin
  inherited Create;

  FParent := AParent;
  FLayoutKind := ALayoutKind;
  FItems := TObjectList<TLayoutX>.Create;

  FMargins := TLayoutMargins.Create;
  FPadding := TLayoutPadding.Create;
  FFont := TLayoutFont.Create;
  FConstraints := TLayoutConstraints.Create;

  FViewType := lvtPanel;
  FName := '';
  FCaption := '';
  FUIParams := '';
  FHint := '';
  FShowCaption := False;
  FShowHint := False;
  FImageIndex := -1;
  FTag := 0;

  // Размеры и расположение
  FLeft := 0;
  FTop := 0;
  FWidth := 0;
  FHeight := 0;
  FAlign := lalNone;
  FAnchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
  FAlignWithMargins := False;

  FState := vsFullAccess;
  FDoubleBuffered := False;
  FTabStop := True;
  FTabOrder := 0;

  FColor := 0;
  FAlignment := TAlignment.taLeftJustify;

  FPageStyle := psTabs;
  FPagePosition := ppTop;
  FPageHeight := 0;
  FPageWidth := 0;
  FHidePages := False;

  FBevelInner := bkNone;
  FBevelOuter := bkNone;
  FBorderStyle := lbsNone;
  FParentBackground := False;

  FAutoSize := False;
  FTransparent := False;
  FWordWrap := False;

  FPicture := nil;
  FStretch := False;
  FProportional := False;

  FBevelStyle := lbsLowered;
  FBevelShape := lbsBox;
  FCursor := crDefault;
  FParams := AParams;

  FStyleName := '';
  FChildLayoutName := '';
  FTargetWorkAreaName := '';
  FTargetLayoutName := '';
  FTargetViewName := '';

  {case ALayoutKind of
    lkPage: begin
      FClass := 'TTabSheet';
      FTag := 11;
      FAlign := 5;
      Include(FAnchors, TAnchorKind.akRight);
      Include(FAnchors, TAnchorKind.akBottom);
      FVisible := False;
      FColor := $FF000005;
      StoreBoolean('ShowHint', True);
    end;
    lkPanel: begin
      FClass := 'TPanel';
      FWidth := 185;
      FHeight := 41;
      StoreInteger('BevelOuter', 0);
      StoreInteger('BevelInner', 0);
      StoreInteger('BevelKind', 0);
      StoreBoolean('ParentBackground', True);
      StoreInteger('Alignment', 2);
      StoreBoolean('ShowCaption', True);
      StoreBoolean('ShowHint', False);
    end; }

  vParams := CreateDelimitedList(AParams);
  try
    case ALayoutKind of
      lkPanel: begin
          FBevelInner := bkNone;
          FBevelOuter := bkNone;
        end;
      lkPage: begin
          FCaption := vParams.Values['Caption'];
          FImageIndex := StrToIntDef(vParams.Values['ImageIndex'], -1);
          FName := vParams.Values['Name'];
          FTag := 11;
        end;
    end;
  finally
    FreeAndNil(vParams);
  end;


end;

destructor TLayoutX.Destroy;
begin
  FreeAndNil(FFont);
  FreeAndNil(FMargins);
  FreeAndNil(FPadding);
  FreeAndNil(FConstraints);
  FreeAndNil(FItems);
  FreeAndNil(FMenu);
  FreeAndNil(FPicture);

  inherited Destroy;
end;

procedure TLayoutX.InternalLoad(const AJSON: TJSONObject);
var
  vAnchors: TStrings;
  vAnchorKind: TAnchorKind;
  jChildren: TJSONArray;
  vChild: TLayoutX;
  i: Integer;
begin
  FLayoutKind := StrToLayoutKind(AJSON.ExtractString('kind'));
  FViewType := StrToViewType(AJSON.ExtractString('view_type'));

  FName := AJSON.ExtractString('name');
  FTag := AJSON.ExtractInteger('tag');
  FCaption := AJSON.ExtractString('caption');
  FShowCaption := AJSON.ExtractBoolean('show_caption');
  FHint := AJSON.ExtractString('hint');
  FShowHint := AJSON.ExtractBoolean('show_hint');
  FImageIndex := AJSON.ExtractInteger('image_index', -1);

  FUIParams := AJSON.ExtractString('ui_params');

  FLeft := AJSON.ExtractInteger('left');
  FTop := AJSON.ExtractInteger('top');
  FWidth := AJSON.ExtractInteger('width');
  FHeight := AJSON.ExtractInteger('height');
  FAlign := StrToAlign(AJSON.ExtractString('align'));
  if AJSON.Contains('margins') then
    FMargins := TLayoutMargins.Create(AJSON.ExtractObject('margins'))
  else
    FMargins := TLayoutMargins.Create(0, 0, 0, 0);
  FAlignWithMargins := AJSON.ExtractBoolean('align_with_margins');
  if AJSON.Contains('padding') then
    FPadding := TLayoutPadding.Create(AJSON.ExtractObject('padding'))
  else
    FPadding := TLayoutPadding.Create(0, 0, 0, 0);
  if AJSON.Contains('constraints') then
    FConstraints := TLayoutConstraints.Create(AJSON.ExtractObject('constraints'))
  else
    FConstraints := TLayoutConstraints.Create(-1, -1, -1, -1);
  FState := StrToViewState(AJSON.ExtractString('view_state'));
  FDoubleBuffered := AJSON.ExtractBoolean('double_buffered');
  FTabOrder := AJSON.ExtractInteger('tab_order');
  FTabStop := AJSON.ExtractBoolean('tab_stop');
  FColor := AJSON.ExtractColor('color');
  if AJSON.Contains('font') then
    FFont := TLayoutFont.Create(AJSON.ExtractObject('font'))
  else
    FFont := TLayoutFont.Create(TColorRec.Black, '', 0, []);

  FAnchors := [];
  vAnchors := AJSON.ExtractStrings('anchors');
  if Assigned(vAnchors) then
  try
    for vAnchorKind := Low(TAnchorKind) to High(TAnchorKind) do
      if StrInList(cAnchorKindNames[vAnchorKind], vAnchors) then
        Include(FAnchors, vAnchorKind);
  finally
    FreeAndNil(vAnchors);
  end;

  FCursor := AJSON.ExtractInteger('cursor');
  FParams := AJSON.ExtractString('params');

  FStyleName := AJSON.ExtractString('style_name');
  FChildLayoutName := AJSON.ExtractString('child_layout_name');
  FTargetWorkAreaName := AJSON.ExtractString('target_workarea_name');
  FTargetLayoutName := AJSON.ExtractString('target_layout_name');
  FTargetViewName := AJSON.ExtractString('target_view_name');

  FBorderStyle := StrToBorderStyle(AJSON.ExtractString('border_style'));

  if FLayoutKind = lkPanel then
  begin
    FBevelInner := StrToBevelKind(AJSON.ExtractString('bevel_inner'));
    FBevelOuter := StrToBevelKind(AJSON.ExtractString('bevel_outer'));
    FParentBackground := AJSON.ExtractBoolean('parent_background');
  end
  else if FLayoutKind = lkPages then
  begin
    FPageStyle := StrToPageStyle(AJSON.ExtractString('page_style'));
    FPagePosition := StrToPagePosition(AJSON.ExtractString('page_position'));
    FPageHeight := AJSON.ExtractInteger('page_height');
    FPageWidth := AJSON.ExtractInteger('page_width');
    FHidePages := AJSON.ExtractBoolean('hide_pages');
  end
  else if FLayoutKind = lkImage then
  begin
    FStretch := AJSON.ExtractBoolean('stretch');
    FProportional := AJSON.ExtractBoolean('proportional');
    if AJSON.Contains('picture') then
      FPicture := TextToBinary(AJSON.ExtractString('picture'))
    else
      FPicture := nil;
  end
  else if FLayoutKind = lkBevel then
  begin
    FBevelStyle := StrToBevelStyle(AJSON.ExtractString('bevel_style'));
    FBevelShape := StrToBevelShape(AJSON.ExtractString('bevel_shape'));
  end
  else if FLayoutKind = lkLabel then
  begin
    FAutoSize := AJSON.ExtractBoolean('auto_size');
    FTransparent := AJSON.ExtractBoolean('transparent');
    FWordWrap := AJSON.ExtractBoolean('word_wrap');
  end;

  if AJSON.Contains('menu') then
    FMenu := TNavigationItem.Create(nil, AJSON.ExtractObject('menu'))
  else
    FMenu := nil;

  jChildren := AJSON.ExtractArray('items');
  if not Assigned(jChildren) or (jChildren.Size = 0) then
    Exit;

  for i := 0 to jChildren.Size - 1 do
  begin
    vChild := AddChild;
    vChild.InternalLoad(TJSONObject(jChildren.Get(i)));
  end;
end;

function TLayoutX.InternalSave: TJSONObject;
var
  vAnchors: TStrings;
  vAnchorKind: TAnchorKind;
  jChildren: TJSONArray;
  i: Integer;
begin
  Result := TJSONObject.Create;

  Result.StoreString('kind', cLayoutKindNames[FLayoutKind]);
  Result.StoreString('view_type', cViewTypeNames[FViewType]);

  Result.StoreString('name', FName);
  Result.StoreInteger('tag', FTag);
  Result.StoreString('caption', FCaption);
  Result.StoreBoolean('show_caption', FShowCaption);
  Result.StoreString('hint', FHint);
  Result.StoreBoolean('show_hint', FShowHint);
  Result.StoreInteger('image_index', FImageIndex);

  Result.StoreString('ui_params', FUIParams);

  Result.StoreInteger('left', FLeft);
  Result.StoreInteger('top', FTop);
  Result.StoreInteger('width', FWidth);
  Result.StoreInteger('height', FHeight);
  Result.StoreString('align', cAlignNames[FAlign]);
  Result.AddPair('margins', FMargins.InternalSave);
  Result.StoreBoolean('align_with_margins', FAlignWithMargins);
  Result.AddPair('padding', FPadding.InternalSave);
  Result.AddPair('constraints', FConstraints.InternalSave);
  Result.StoreString('view_state', ViewStateToStr(FState));
  Result.StoreBoolean('double_buffered', FDoubleBuffered);
  Result.StoreInteger('tab_order', FTabOrder);
  Result.StoreBoolean('tab_stop', FTabStop);
  Result.StoreColor('color', FColor);
  Result.AddPair('font', FFont.InternalSave);

  vAnchors := TStringList.Create;
  try
    for vAnchorKind := Low(TAnchorKind) to High(TAnchorKind) do
      if vAnchorKind in FAnchors then
        vAnchors.Add(cAnchorKindNames[vAnchorKind]);
    Result.StoreStrings('anchors', vAnchors);
  finally
    FreeAndNil(vAnchors);
  end;

  Result.StoreInteger('cursor', FCursor);
  Result.StoreString('params', FParams);
  Result.StoreString('border_style', cBorderStyleNames[FBorderStyle]);

  Result.StoreString('style_name', FStyleName);
  Result.StoreString('child_layout_name', FChildLayoutName);
  Result.StoreString('target_workarea_name', FTargetWorkAreaName);
  Result.StoreString('target_layout_name', FTargetLayoutName);
  Result.StoreString('target_view_name', FTargetViewName);

  if FLayoutKind = lkPanel then
  begin
    Result.StoreString('bevel_inner', cBevelKindNames[FBevelInner]);
    Result.StoreString('bevel_outer', cBevelKindNames[FBevelOuter]);
    Result.StoreBoolean('parent_background', FParentBackground);
  end
  else if FLayoutKind = lkPages then
  begin
    Result.StoreString('page_style', cPageStyleNames[FPageStyle]);
    Result.StoreString('page_position', cPagePositionNames[FPagePosition]);
    Result.StoreInteger('page_height', FPageHeight);
    Result.StoreInteger('page_width', FPageWidth);
    Result.StoreBoolean('hide_pages', FHidePages);
  end
  else if FLayoutKind = lkImage then
  begin
    Result.StoreBoolean('stretch', FStretch);
    Result.StoreBoolean('proportional', FProportional);
    if Assigned(FPicture) then
      Result.StoreString('picture', BinaryToText(FPicture));
  end
  else if FLayoutKind = lkBevel then
  begin
    Result.StoreString('bevel_style', cBevelStyleNames[FBevelStyle]);
    Result.StoreString('bevel_shape', cBevelShapeNames[FBevelShape]);
  end
  else if FLayoutKind = lkLabel then
  begin
    Result.StoreBoolean('auto_size', FAutoSize);
    Result.StoreBoolean('transparent', FTransparent);
    Result.StoreBoolean('word_wrap', FWordWrap);
  end;

  if Assigned(FMenu) then
    Result.AddPair('menu', FMenu.InternalSave);

  if FItems.Count = 0 then
    Exit;

  jChildren := TJSONArray.Create;
  for i := 0 to FItems.Count - 1 do
    jChildren.Add(FItems[i].InternalSave);

  Result.AddPair('items', jChildren);
end;

procedure TLayoutX.Load(const AFileName: string);
var
  jLayout: TJSONObject;
begin
  jLayout := TJSONObject.LoadFromFile(AFileName);
  try
    InternalLoad(jLayout);
  finally
    FreeAndNil(jLayout);
  end;
end;

procedure TLayoutX.Save(const AFileName: string);
var
  jLayout: TJSONObject;
begin
  jLayout := InternalSave;
  try
    jLayout.SaveToFile(AFileName);
  finally
    FreeAndNil(jLayout);
  end;
end;

procedure TLayoutX.SetPictureStream(const Value: TStream);
begin
  if Assigned(FPicture) then
    FreeAndNil(FPicture);
  FPicture := Value;
end;

{ TLayoutEdges }

constructor TLayoutEdges.Create;
begin
  Create(0, 0, 0, 0);
end;

constructor TLayoutEdges.Create(const ALeft, ATop, ARight, ABottom: Integer);
begin
  inherited Create;
  FLeft := ALeft;
  FTop := ATop;
  FRight := ARight;
  FBottom := ABottom;
end;

constructor TLayoutEdges.Create(const AJSON: TJSONObject);
begin
  inherited Create;
  InternalLoad(AJSON);
end;

procedure TLayoutEdges.InternalLoad(const AJSON: TJSONObject);
begin
  FLeft := AJSON.ExtractInteger('left');
  FTop := AJSON.ExtractInteger('top');
  FRight := AJSON.ExtractInteger('right');
  FBottom := AJSON.ExtractInteger('bottom');
end;

function TLayoutEdges.InternalSave: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.StoreInteger('left', FLeft);
  Result.StoreInteger('top', FTop);
  Result.StoreInteger('right', FRight);
  Result.StoreInteger('bottom', FBottom);
end;

function TLayoutEdges.IsEmpty: Boolean;
begin
  Result := (FLeft = 0) and (FTop = 0) and (FRight = 0) and (FBottom = 0);
end;

procedure TLayoutEdges.Save(const AParent: TJSONObject; const AName: string);
begin
  AParent.AddPair(AName, InternalSave);
end;

{ TLayoutFont }

constructor TLayoutFont.Create(const AColor: TAlphaColor; const AFamily: string; const ASize: Integer;
  const AStyle: TFontStyles);
begin
  inherited Create;
  FColor := AColor;
  FFamily := AFamily;
  FSize := ASize;
  FStyle := AStyle;
end;

constructor TLayoutFont.Create(const AJSON: TJSONObject);
begin
  inherited Create;
  InternalLoad(AJSON);
end;

constructor TLayoutFont.Create;
begin
  Create(TAlphaColorRec.Maroon, 'Tahoma', 8, []);
end;

procedure TLayoutFont.InternalLoad(const AJSON: TJSONObject);
var
  vStyles: TStrings;
  vFontStyle: TFontStyle;
begin
  FColor := AJSON.ExtractColor('color');
  FFamily := AJSON.ExtractString('family');
  FSize := AJSON.ExtractInteger('size');

  FStyle := [];
  vStyles := AJSON.ExtractStrings('style');
  if Assigned(vStyles) then
  begin
    try
      for vFontStyle := Low(TFontStyle) to High(TFontStyle) do
        if StrInList(cFontStyleNames[vFontStyle], vStyles) then
          Include(FStyle, vFontStyle);
    finally
      FreeAndNil(vStyles);
    end;
  end;
end;

function TLayoutFont.InternalSave: TJSONObject;
var
  vFontStyle: TFontStyle;
  vStyles: TStrings;
begin
  Result := TJSONObject.Create;
  Result.StoreColor('color', FColor);
  Result.StoreString('family', FFamily);
  Result.StoreInteger('size', FSize);

  vStyles := TStringList.Create;
  try
    for vFontStyle := Low(TFontStyle) to High(TFontStyle) do
      if vFontStyle in FStyle then
        vStyles.Add(cFontStyleNames[vFontStyle]);
    Result.StoreStrings('style', vStyles);
  finally
    FreeAndNil(vStyles);
  end;
end;

procedure TLayoutFont.Save(const AParent: TJSONObject; const AName: string);
begin
  AParent.AddPair(AName, InternalSave);
end;

{ TLayoutConstraints }

constructor TLayoutConstraints.Create(const AJSON: TJSONObject);
begin
  inherited Create;
  InternalLoad(AJSON);
end;

constructor TLayoutConstraints.Create(const AMinWidth, AMaxWidth, AMinHeight, AMaxHeight: Integer);
begin
  inherited Create;
  FMinWidth := AMinWidth;
  FMaxWidth := AMaxWidth;
  FMinHeight := AMinHeight;
  FMaxHeight := AMaxHeight;
end;

constructor TLayoutConstraints.Create;
begin
  Create(0, 0, 0, 0);
end;

procedure TLayoutConstraints.InternalLoad(const AJSON: TJSONObject);
begin
  FMinWidth := AJSON.ExtractInteger('min_width');
  FMaxWidth := AJSON.ExtractInteger('max_width');
  FMinHeight := AJSON.ExtractInteger('min_height');
  FMaxHeight := AJSON.ExtractInteger('max_height');
end;

function TLayoutConstraints.InternalSave: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.StoreInteger('min_width', FMinWidth);
  Result.StoreInteger('max_width', FMaxWidth);
  Result.StoreInteger('min_height', FMinHeight);
  Result.StoreInteger('max_height', FMaxHeight);
end;

procedure TLayoutConstraints.Save(const AParent: TJSONObject; const AName: string);
begin
  AParent.AddPair(AName, InternalSave);
end;

{ TNavigationItem }

function TNavigationItem.Add(const ACaption: string): TNavigationItem;
begin
  Result := TNavigationItem.Create(Self, ACaption);
  FItems.Add(Result);
end;

constructor TNavigationItem.Create(const AParent: TNavigationItem; const AUrl: string);
begin
  inherited Create(lkAction, nil, True);
  FParent := AParent;
  FOwner := nil;

  SetUrl(AUrl);
  FRadioItem := False;
  FGroupIndex := 0;
end;

constructor TNavigationItem.Create(const AParent: TNavigationItem; const AJSON: TJSONObject);
begin
  inherited Create(lkAction, nil, True);
  FOwner := nil;
  FParent := AParent;
  InternalLoad(AJSON);
end;

destructor TNavigationItem.Destroy;
var
  i: Integer;
begin
  for i := 0 to FItems.Count - 1 do
    FItems[i].Free;
  FParent := nil;
  inherited Destroy;
end;

function TNavigationItem.GetOwner: TLayout;
begin
  if not Assigned(FOwner) and Assigned(FParent) then
    FOwner := TNavigationItem(FParent).Owner;
  Result := FOwner;
end;

function TNavigationItem.Insert(const AIndex: Integer; const ACaption: string): TNavigationItem;
begin
  Result := TNavigationItem.Create(Self, ACaption);
  FItems.Insert(AIndex, Result);
end;

procedure TNavigationItem.InternalLoad(const AJSON: TJSONObject);
var
  vItems: TJSONArray;
  vItem: TNavigationItem;
  i: Integer;
begin
  FCaption := AJSON.ExtractString('caption');
  if AJSON.Contains('items') then
  begin
    vItems := AJSON.ExtractArray('items');
    for i := 0 to vItems.Size - 1 do
    begin
      vItem := TNavigationItem.Create(Self, TJSONObject(vItems.Get(i)));
      FItems.Add(vItem);
    end;
  end;
end;

function TNavigationItem.InternalSave: TJSONObject;
var
  vItems: TJSONArray;
  vItem: TJSONObject;
  i: Integer;
begin
  Result := TJSONObject.Create;
  Result.StoreString('caption', FCaption);
  if FItems.Count = 0 then
    Exit;

  vItems := TJSONArray.Create;
  for i := 0 to FItems.Count - 1 do
  begin
    vItem := FItems[i].InternalSave;
    vItems.Add(vItem);
  end;

  Result.AddPair('items', vItems);
end;

function TNavigationItem.IsLine: Boolean;
begin
  Result := FViewName = '-';
end;

procedure TNavigationItem.SetUrl(const AUrl: string);
begin
  inherited SetUrl(AUrl);

  FViewName := FUrlParser.Path;
  if FViewName = '-' then
    FCaption := '-'
  else
    FCaption := FUrlParser.ExtractString('caption');
  FId := FUrlParser.ExtractString('id');
  FHint := FUrlParser.ExtractString('hint');
  FImageID := FUrlParser.ExtractInteger('imageindex');
  FContentLayout := FUrlParser.ExtractString('contentlayout');
  FContentWorkArea := FUrlParser.ExtractString('contentworkarea');
  FContentCaption := FUrlParser.ExtractString('contentcaption');
end;

{ TLayout }

procedure TLayout.Add(const AChild: TLayout);
begin
  AChild.FParent := Self;
  FItems.Add(AChild);
end;

procedure TLayout.ArrangeChildAreas;
const
  cBetweenColumns = 25;
  cSideRate = 3/2;
  cBorder = 12;
var
  vTotalHeight: Integer;
  vLayout: TLayout;
  vCurColumnHeight: Integer;
  vRealMaxHeightInColumn: Integer;
  vColumnCount: Integer;
  i: Integer;
  vRealColumnCount: Integer;
  vBestRate: Double;
  vHeightInColumn: Integer;
  vCurRate: Double;
  vBestColumnCount: Integer;
  vItems: TList<TLayout>;
begin
  vItems := GetItems;

  vTotalHeight := 0;
  for i := 0 to vItems.Count - 1 do
  begin
    vLayout := vItems[i];
    vTotalHeight := vTotalHeight + TPresenter(vLayout.Presenter).GetLayoutBounds(vLayout).Height + cBetweenRows;
  end;
  vBestRate := -1000;
  vBestColumnCount := -1;
  // select best columnt count
  for vColumnCount := 1 to 4 do
  begin
    vHeightInColumn := vTotalHeight div vColumnCount;
    vCurColumnHeight := 0;
    vRealMaxHeightInColumn := 0;
    for i := 0 to vItems.Count - 1 do
    begin
      vLayout := vItems[i];
      vCurColumnHeight := vCurColumnHeight + TPresenter(vLayout.Presenter).GetLayoutBounds(vLayout).Height + cBetweenRows;
      if vCurColumnHeight >= vHeightInColumn then
      begin
        if vCurColumnHeight > vRealMaxHeightInColumn then
          vRealMaxHeightInColumn := vCurColumnHeight;
        vCurColumnHeight := 0;
      end;
    end;
    //vCurRate := width/height
    vCurRate := (vColumnCount * (cDefaultColumnWidth + cBetweenColumns) - cBetweenColumns) / vRealMaxHeightInColumn;
    if Min(vCurRate, cSideRate) / Max(vCurRate, cSideRate) > Min(vBestRate, cSideRate) / Max(vBestRate, cSideRate) then
    begin
      vBestColumnCount := vColumnCount;
      vBestRate := vCurRate;
    end
    else
      Break;
  end;
  vHeightInColumn := vTotalHeight div vBestColumnCount;
  vCurColumnHeight := 0;
  vRealMaxHeightInColumn := 0;
  vRealColumnCount := 1;

  for i := 0 to vItems.Count - 1 do
  begin
    vLayout := vItems[i];

    TPresenter(Presenter).SetLayoutXY(vLayout, cBorder + (vRealColumnCount - 1) * (cDefaultColumnWidth + cBetweenColumns), cBorder + cBetweenRows + vCurColumnHeight);

    vCurColumnHeight := vCurColumnHeight + TPresenter(vLayout.Presenter).GetLayoutBounds(vLayout).Height + cBetweenRows;
    if vCurColumnHeight >= vHeightInColumn then
    begin
      if vCurColumnHeight > vRealMaxHeightInColumn then
        vRealMaxHeightInColumn := vCurColumnHeight;
      vCurColumnHeight := 0;
      if i < vItems.Count - 1 then  // not last
        vRealColumnCount := vRealColumnCount + 1;
    end;
  end;
  TPresenter(Presenter).SetLayoutBounds(Self, 0, 0,
    (vRealColumnCount * (cDefaultColumnWidth + cBetweenColumns) - cBetweenColumns) + cBorder*2,
    vRealMaxHeightInColumn + cBorder * 2 + 8);
end;

constructor TLayout.Create(const AKind: TLayoutKind; const AControl: TObject; const AIsOwner: Boolean = False);
begin
  inherited Create;
  FParent := nil;
  FItems := TList<TLayout>.Create;
  FContentLayout := nil;
  FMenu := nil;
  FUrlParser := nil;
  FIsOwner := AIsOwner;
  FControl := AControl;
  FKind := AKind;
  Assert(FKind > lkNone);

  FLeft := 0;
  FTop := 0;
  FWidth := 0;
  FHeight := 0;
  FAnchors := [];
  FAlignWithMargins := False;
  FAlign := lalNone;
  FMargins := TLayoutMargins.Create;
  FPadding := TLayoutPadding.Create;
  FConstraints := TLayoutConstraints.Create;
  FAlignment := taLeftJustify;
  FAutoSize := False;
  FWordWrap := False;
  FColor := TAlphaColorRec.MoneyGreen;
  FTransparent := False;

  FName := '';
  FHint := '';
  FShowCaption := False;
  FCaption := '';
  FUIParams := '';
  FTag := 0;
  FImageIndex := -1;

  FPen := TLayoutPen.Create;
  FBrush := TLayoutBrush.Create;
  FFont := TLayoutFont.Create;

  FShape_Type := lstRectangle;
  FImage_Picture := nil;
  FImage_Stretch := False;
  FImage_Proportional := False;
  FImage_Center := False;
  FPage_Style := psTabs;
  FPage_Position := ppTop;
  FPage_Height := 0;
  FPage_Width := 0;

  Inc(_Count);
  FIndex := _Count;
end;

destructor TLayout.Destroy;
begin
  FParent := nil;

  FreeAndNil(FConstraints);
  FreeAndNil(FPadding);
  FreeAndNil(FMargins);
  FreeAndNil(FFont);
  FreeAndNil(FPen);
  FreeAndNil(FBrush);
  FreeAndNil(FImage_Picture);

  FreeAndNil(FContentLayout);
  FreeAndNil(FItems);
  FreeAndNil(FUrlParser);
  if FIsOwner then
    FreeAndNil(FControl)
  else
    FControl := nil;
  FreeAndNil(FMenu);
  inherited Destroy;
end;

procedure TLayout.SaveToDFM(const AFileName: string);
var
  vLayout: TUILayout;
begin
  vLayout := TUILayout.Create;
  try
    vLayout.Build(Self);
    vLayout.SaveToDFM(AFileName);
  finally
    vLayout.Free;
  end;
end;

function TLayout.ExtractInteger(const AParamName: string; const ADefault: Integer): Integer;
begin
  if Assigned(FUrlParser) then
    Result := FUrlParser.ExtractInteger(AParamName, ADefault)
  else
    Result := ADefault;
end;

function TLayout.ExtractString(const AParamName, ADefault: string): string;
begin
  if Assigned(FUrlParser) then
    Result := FUrlParser.ExtractString(AParamName, ADefault)
  else
    Result := ADefault;
end;

function TLayout.GetItems: TList<TLayout>;
begin
  if Assigned(FContentLayout) then
    Result := FContentLayout.FItems
  else
    Result := FItems;
end;

procedure TLayout.InternalLoad(const AJSON: TJSONObject);
begin
end;

function TLayout.InternalSave: TJSONObject;
begin
  Result := TJSONObject.Create;
end;

procedure TLayout.Save(const AParent: TJSONObject; const AName: string);
begin
  AParent.AddPair(AName, InternalSave);
end;

procedure TLayout.SetContentLayout(const Value: TLayout);
begin
  if FContentLayout = Value then
    Exit;

  if Assigned(FContentLayout) then
    FContentLayout.Free;
  FContentLayout := Value;
end;

procedure TLayout.SetMenu(const Value: TNavigationItem);
begin
  FMenu := Value;
  FMenu.FOwner := Self;
end;

procedure TLayout.SetPictureStream(const Value: TStream);
begin
  if Assigned(FImage_Picture) then
    FreeAndNil(FImage_Picture);
  FImage_Picture := Value;
end;

procedure TLayout.SetUrl(const AUrl: string);
begin
  if Assigned(FUrlParser) then
    FreeAndNil(FUrlParser);
  FUrlParser := TUrlParser.Create(AUrl);
end;

{ TUrlParser }

constructor TUrlParser.Create(const AUrl: string);
var
  vUrl: string;
  vPos: Integer;
  vItems: TStrings;
  i: Integer;
begin
  inherited Create;

  FParams := TStringList.Create;
  vUrl := AUrl;
  vPos := Pos('?', vUrl);
  if vPos = 0 then
    FPath := AUrl
  else begin
    FPath := Copy(vUrl, 1, vPos - 1);
    Delete(vUrl, 1, vPos);
    vItems := CreateDelimitedList(vUrl, '&');
    try
      for i := 0 to vItems.Count - 1 do
        FParams.Add(LowerCase(vItems.KeyNames[i]) + FParams.NameValueSeparator + vItems.ValueFromIndex[i]);
    finally
      FreeAndNil(vItems);
    end;
  end;
end;

destructor TUrlParser.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TUrlParser.ExtractInteger(const AParamName: string; const ADefault: Integer): Integer;
var
  vValue: string;
begin
  vValue := FParams.Values[AParamName];
  Result := StrToIntDef(vValue, ADefault);
end;

function TUrlParser.ExtractString(const AParamName, ADefault: string): string;
begin
  Result := FParams.Values[AParamName];
  if Result = '' then
    Result := ADefault;
end;

{ TLayoutParam }

constructor TLayoutParam.Create(const AName: string);
begin
  Name := AName;
end;

{ TUILayout }

function TUILayout.CreateChild: TUILayout;
begin
  Result := TUILayout.Create;
  Result.FParent := Self;
  Result.FLevel := FLevel + 1;
  FChildLayouts.Add(Result);
end;

procedure TUILayout.Build(const ALayout: TLayout);
  procedure Process(const AUILayout: TUILayout; const ALayout: TLayout);
  var
    i: Integer;
  begin
    if Assigned(ALayout.Control) then
      AUILayout.FillLayoutParams(TComponent(ALayout.Control), ALayout);
    for i := 0 to ALayout.Items.Count - 1 do
      Process(AUILayout.CreateChild, ALayout.Items[i]);
  end;
begin
  Clear;
  Process(Self, ALayout);
end;

procedure TUILayout.Clear;
var
  i: Integer;
begin
  for i := 0 to FChildLayouts.Count - 1 do
    TUILayout(FChildLayouts[i]).Free;
  FChildLayouts.Clear;

  for i := 0 to FParams.Count - 1 do
    TLayoutParam(FParams[i]).Free;
  FParams.Clear;
end;

constructor TUILayout.Create;
begin
  FName := 'emptyname';
  FType := 'emptytype';
  FChildLayouts := TList.Create;
  FParams := TList.Create;
end;

function TUILayout.FindChild(const AName: string): TUILayout;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FChildLayouts.Count - 1 do
    if TUILayout(FChildLayouts[i]).FName = AName then
    begin
      Result := TUILayout(FChildLayouts[i]);
      Break;
    end;
end;

destructor TUILayout.Destroy;
begin
  Clear;
  FreeAndNil(FParams);
  FreeAndNil(FChildLayouts);
  inherited;
end;

procedure TUILayout.GenerateDFMText(const AText: TStrings; const ALevel: Integer);
var
  i: Integer;
  vSelfIndent, vChildIndent: string;
  vParam: TLayoutParam;
begin
  vSelfIndent := DupeString('  ', ALevel);
  vChildIndent := DupeString('  ', ALevel + 1);
  AText.Append(vSelfIndent + 'object ' + FName + ': ' + FType);
  for i := 0 to FParams.Count - 1 do
  begin
    vParam := TLayoutParam(FParams[i]);
    AText.Append(vChildIndent +  vParam.Name + ' = ' + VarToString(vParam.Value, ''));
  end;
  for i := 0 to FChildLayouts.Count - 1 do
    TUILayout(FChildLayouts[i]).GenerateDFMText(AText, ALevel + 1);
  AText.Append(vSelfIndent + 'end');
end;

procedure TUILayout.GeneratePASText(const AText: TStrings);
var
  vStr: string;
begin
  vStr :=
    'unit ' + FName + ';' + #13#10#13#10 +
    'interface' + #13#10#13#10 +
    'uses' + #13#10 +
    '  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms;'#13#10#13#10 +
    'type'#13#10 +
    '  ' + FType + ' = class(TFrame)'#13#10 +
    '  end;'#13#10#13#10 +
    'implementation'#13#10#13#10 +
    '{$R *.dfm}'#13#10#13#10 +
    'end.';
  AText.Append(vStr);
end;

function TUILayout.GenerateUniqueName(const AComponent: TComponent): string;
begin
  Result := Copy(AComponent.ClassName, 2, Length(AComponent.ClassName)) + IntToStr(FLevel);
  if Assigned(FParent) then
    while FParent.FindChild(Result) <> nil do
      Result := Result + 'a';
end;

procedure TUILayout.FillLayoutParams(const AComponent: TComponent; const ALayout: TLayout);
var
  vName, vCaption: string;
  vRect: TRect;
begin
  vName := AComponent.Name;
  if Length(vName) = 0 then
    vName := GenerateUniqueName(AComponent);

  FName := vName;
  FType := 'TPanel';

  vRect := TPresenter(ALayout.Presenter).GetLayoutBounds(ALayout);
  vCaption := TPresenter(ALayout.Presenter).GetLayoutCaption(ALayout);

  ParamByName('Width').Value := vRect.Width;
  ParamByName('Height').Value := vRect.Height;
  ParamByName('Left').Value := vRect.Left;
  ParamByName('Top').Value := vRect.Top;
  if vCaption.Length > 0 then
    ParamByName('Caption').Value := QuotedStr(vCaption);

//  if AComponent is TPageControl then
//    ALayout.ParamByName('PagePosition').Value := TPageControl(AComponent).TabPosition;

//  if AComponent is TImage then
//    ALayout.ParamByName('Stretch').Value := TImage(AComponent).Stretch;

{  if AComponent is TPanel then
  begin
    if (not TPanel(AComponent).ParentColor) then
      ALayout.ParamByName('Color').Value := TPanel(AComponent).Color;
  end;}
end;

function TUILayout.ParamByName(const AName: string): TLayoutParam;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FParams.Count - 1 do
    if TLayoutParam(FParams[i]).Name = AName then
    begin
      Result := TLayoutParam(FParams[i]);
      Break;
    end;
  if Result = nil then
  begin
    Result := TLayoutParam.Create(AName);
    FParams.Add(Result);
  end;
end;

procedure TUILayout.SaveToDFM(const AFileName: string);
var
  vFileName: string;
  vFile: TStringList;
begin
  vFileName := AFileName + '.dfm';
  vFile := TStringList.Create;
  try
    GenerateDFMText(vFile, 0);
    vFile.SaveToFile(vFileName);
  finally
    vFile.Free;
  end;

  vFileName := AFileName + '.pas';
  vFile := TStringList.Create;
  try
    GeneratePASText(vFile);
    vFile.SaveToFile(vFileName);
  finally
    vFile.Free;
  end;
end;

{ TLayoutPen }

constructor TLayoutPen.Create;
begin
  Create(TAlphaColorRec.Blueviolet, 1);
end;

constructor TLayoutPen.Create(const AJSON: TJSONObject);
begin
  inherited Create;
  InternalLoad(AJSON);
end;

constructor TLayoutPen.Create(const AColor: TAlphaColor; const AWidth: Integer);
begin
  inherited Create;
  FColor := AColor;
  FWidth := AWidth;
end;

procedure TLayoutPen.InternalLoad(const AJSON: TJSONObject);
begin
  FColor := AJSON.ExtractColor('color');
  FWidth := AJSON.ExtractInteger('width');
end;

function TLayoutPen.InternalSave: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.StoreColor('color', FColor);
  Result.StoreInteger('width', FWidth);
end;

procedure TLayoutPen.Save(const AParent: TJSONObject; const AName: string);
begin
  AParent.AddPair(AName, InternalSave);
end;

{ TLayoutBrush }

constructor TLayoutBrush.Create;
begin
  Create(TAlphaColorRec.Skyblue);
end;

constructor TLayoutBrush.Create(const AJSON: TJSONObject);
begin
  inherited Create;
  InternalLoad(AJSON);
end;

constructor TLayoutBrush.Create(const AColor: TAlphaColor);
begin
  inherited Create;
  FColor := AColor;
end;

procedure TLayoutBrush.InternalLoad(const AJSON: TJSONObject);
begin
  FColor := AJSON.ExtractColor('color');
end;

function TLayoutBrush.InternalSave: TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.StoreColor('color', FColor);
end;

procedure TLayoutBrush.Save(const AParent: TJSONObject; const AName: string);
begin
  AParent.AddPair(AName, InternalSave);
end;

initialization
  TLayout._Count := 0;

end.
