{---------------------------------------------------------------------------------
  X-Tend runtime

  Contributors:
    Vladimir Kustikov (kustikov@sensoft.pro)
    Sergey Arlamenkov (arlamenkov@sensoft.pro)

  You may retrieve the latest version of this file at the GitHub,
  located at https://github.com/sensoftpro/x-tend.git
 ---------------------------------------------------------------------------------
  MIT License

  Copyright © 2021 Sensoft

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

unit uConsts;

interface

uses
  UITypes, Generics.Collections;

const
  cPlatformTitle = 'Платформа';
  cProductCreator = 'Sensoft';

const
  DM_DOMAIN_CHANGED = $0437;
  DM_VIEW_CHANGED = $0438;

type
  TDomainChangedMessage = packed record
    Msg: Word;
    Kind: Word;
    Sender: TObject;
    Parameter: TObject;
    Reserved: Integer;
  end;

  TViewChangedMessage = packed record
    Msg: Word;
    Kind: Word;
    View: TObject;
    Parameter: TObject;
    Reserved: Integer;
  end;

  TRFIDReadEvent = procedure(const ANewRFID: string) of object;

  TFetchDataFunc = reference to procedure(const AFieldDict: TDictionary<string, Integer>; const AData: Variant);

const
  cNewID = -1;
  cEuroSign = '€';

const
  cNullItemName = '< --- >';
  cUnknownName = '...';

  cNullInteger = 0;
  cNullFloat = 0.0;
  cNullDateTime: TDateTime = 0;
  cNullBoolean = False;
  cNullColor = $1;
  cNullCurrency = 0.0;

  cDateNow = 'date';
  cDateTimeNow = 'datetime';
  cDateTomorrow = 'tomorrow';
  cDatePlus1YearMinus1Day = '+1y-1d';
  cDatePlus1Week = '+1w';
  cDateMinus1Week = '-1w';
  cStartOfYear = 'year_start';
  cContextDelimiter = '&';

const
  // UI
  LAYOUT_DFM_EXT = '.dfm';
  cEditFormResizable = 1;
  cMainFormPositionDesign = 2;

const
  // флаги полей и сущностей
  cRequired    = 1;
//  cHidden      = 2;
//  cReadOnly    = 4;
//  cDisabled    = 8;
  cRecommended = 16;
  cLazy        = 32;
  cAnalytic    = 64;
  cCalculated  = 128;
//  cSelectable  = 256;
  cSystem      = 512; //системные поля создаются самой платформой на основании других признаков
  cChartGroup  = 1024;
  cChartSerie  = 2048;
  cHideInGrid  = 4096;
  cReserved1   = 8192;
  cHideInEdit  = 16384;
  cNotSave     = 32768;
  cLocalizable = 65536;

  cAutoCalculatedField = cNotSave or cCalculated or cHideInGrid or cHideInEdit or cLazy;

const
  // флаги коллекций
  ccSystem               = 1;
  ccNavigable            = 2;    // отображается в меню
  ccHierarchical         = 4;
  ccInstantExecution     = 8;    //
  ccLocalOnly            = 16;   // не выгружать при обмене
  ccHideInMenu           = 32;   // не показывать в системном меню
  ccStopEvents           = 64;   // значения в этой коллекции не передают события полям TEntityField
  ccLazyLoad             = 128;  // не загружать при старте
  ccMultiTarget          = 256;  // применять действие на все выделенные сущности
  ccAlwaysShowParameters = 512;  // При наличии параметров всегда выводить форму подтверждения
  ccNotSave              = 1024; // Коллекция для оперативной памяти
  ccContextAction        = 2048; // Указывает на то, что для исполнения действия нужен контекст

type
  TViewState = type Byte;
const
  vsHidden     = $00;
  vsDisabled   = $01;
  vsReadOnly   = $03;
  vsSelectOnly = $07;
  vsFullAccess = $0F;
  vsUndefined  = $80;

  cViewStateNames: array[0..15] of string = (
    'HID', 'DIS', 'RDO', 'RDO', 'SEL', 'SEL', 'SEL', 'SEL',
    '@@@', '@@@', '@@@', '@@@', '@@@', '@@@', '@@@', '@@@');

const
  dckFieldChanged        = 1;
  dckEntityDeleted       = 2;
  dckEntityChanged       = 4;
  dckSelectionChanged    = 8;
  dckListAdded           = 16;
  dckListRemoved         = 32;
  dckViewStateChanged    = 64;
  dckFilterChanged       = 128;
  dckEntitySaved         = 256;
  dckNameChanged         = 512;

type
  TViewStates = (_vsHidden, _vsDisabled, _vsReadOnly, _vsSelectOnly, _vsFullAccess, _vsUndefined);
  TDefinitionFlags = (dfNone, dfSystem, dfNotSave, dfLazyLoad, dfStopEvents, dfLocalOnly, dfHideInMenu,
    dfInstantExecution, dfContextAction, dfMultiTarget, dfAlwaysShowParameters);
  TFieldFlags = (ffNone, ffRequired, ffLocalizable, ffLazy, ffAnalytic, ffCalculated, ffSystem, ffHideInGrid,
    ffHideInEdit, ffNotSave);
  TChangeKinds = (chkNone, chkFieldChanged, chkEntityDeleted, chkEntityChanged, chkSelectionChanged, chkListAdded,
    chkListRemoved, chkViewStateChanged, chkFilterChanged, chkEntitySaved, chkNameChanged);
  TPrecisionType = (ptDefault, ptIntInteger, ptIntInt64, ptFloatSingle, ptFloatDouble);
const
  cViewStateIDs: array[TViewStates] of Integer = ($00, $01, $03, $07, $0F, $80);
  cDefinitionFlagIDs: array[TDefinitionFlags] of Integer = ($000, $001, $002, $004, $008, $010, $020, $040, $080, $100, $200);
  cFieldFlagIDs: array[TFieldFlags] of Integer = ($000, $001, $002, $004, $008, $010, $020, $040, $080, $100);
  cChangeKindIDs: array[TChangeKinds] of Integer = ($000, $001, $002, $004, $008, $010, $020, $040, $080, $100, $200);
  cViewStateCaptions: array[TViewStates] of string = ('Скрыто', 'Заблокировано', 'Только чтение',
    'Только выбор', 'Полный доступ', '-');
  cDefinitionFlagCaptions: array[TDefinitionFlags] of string = ('None', 'System', 'Not Save', 'Lazy Load',
    'Stop Events', 'Local Only', 'Hide In Menu', 'Instant Execution', 'Context Action',
    'Multi Target', 'Always Show Parameters');
  cFieldFlagCaptions: array[TFieldFlags] of string = ('None', 'Required', 'Localizable', 'Lazy', 'Analytic',
    'Calculated', 'System', 'Hide In Grid', 'Hide In Edit', 'Not Save');
  cChangeKindCaptions: array[TChangeKinds] of string = ('None', 'Field Changed', 'Entity Deleted', 'Entity Changed',
    'Selection Changed', 'List Added', 'List Removed', 'View State Changed',
    'Filter Changed', 'Entity Saved', 'Name Changed');

type
  TBlobFormat = (bffRaw, bffText, bffDocument, bffImage, bffAudio, bffVideo);

  TStorageKind = (skNotSave, skDatabase, skSharedFolder);

  TEntityFillingKind = (efkRaw, efkEmpty, efkDefault, efkRandom);

  TPacketType = (ptUpdate, ptFull);

  TWindowArrangement = (waNone, waCascade, waTileHorz, waTileVert, waMozaic);

  TValidateStatus = (vsValid, vsInvalid, vsRequiredIsNull);
  TEntitySortType = (estUserSort, estSortByID, estSortByName,
    estSortByTypeAndName, estSortByColorFieldID, estSortByOrder);
  TSearchType = (stSearchNone, stSearchFromBegin, stSearchEverywhere,
    stSearchMultiEntrance);

  TSystemFlag = (sfRequired, sfHidden, sfReadOnly, sfDisabled, sfRecommended,
    sfAutoCreate, sfAnalytic, sfCalculated, sfSelectable, sfSystem);

  TEntitySaveAction = (esaNoAction, esaInsert, esaUpdate, esaDelete);

  TCollectionKind = (clkNotDefined, clkLibrary, clkDocument, clkMixin);

  TFieldKind = (fkNotDefined, fkString, fkInteger, fkEnum, fkFlag, fkFloat, fkDateTime, fkBoolean,
    fkColor, fkCurrency, fkObject, fkList, fkBlob, fkComplex);

  TConditionKind = (ckUndefined, ckEqualTo, ckMatchesTo,
    ckGreaterThan, ckLessThan, ckPartOf, ckContains, ckCrosses);

  TUIItemType = (uiTextEdit, uiBoolEdit, uiEntityEdit, uiIntegerEdit, uiEnumEdit, uiFlagEdit,
    uiFloatEdit, uiDateEdit, uiCurrencyEdit, uiListEdit, uiBLOBEdit, uiComplexEdit, uiColorEdit,
    uiAction, uiCollection);

  TRelationPower = (rpWeak, rpStrong);

  TIntegrationKind = (ikTranslator, ikMessenger, ikEmail);

  TCommitKind = (ckOnChange, ckOnExit, ckOnSave);

  TEnumType = (etOrdinal, etBitwise, etCustom);

  THTTPHeaderMethod = (hhmGet, hhmPost, hhmPut, hhmDelete, hhmPatch, hhmHead);
  TAuthorizationType = (atNone, atBasic, atBearer, atDigest, atHawk, atAWS);

  TIndexFieldConvertation = (ifcNone, ifcIgnoreCase, ifcIgnoreSpace, ifcIgnoreLang);

  TConditionModifier = (cmNone, cmIgnoreCase, cmIgnoreSpace, cmIgnoreLanguage);

  TLogAction = (laUndefined = 0, laAddRecord = 1, laEditRecord = 2,
    laDeleteRecord = 3, laOpenForm = 4, laCloseForm = 5, laError = 6);

  TMessageKind = (mkAny, mkBaseActions, mkError, mkWarning, mkInfo);

  TAggregationKind = (akNotDefined, akCount, akSum, akAverage, akMin, akMax);
  TCubeAreaKind = (cakNone, cakRow, cakColumn, cakData, cakFilter);
  TSortOrder = (soNone, soAscending, soDescending);

  TDialogResult = (drNone, drOk, drCancel, drYes, drNo);
  TDialogResultSet = set of TDialogResult;

  TCursorType = (crtDefault, crtNone, crtArrow, crtCross, crtWESize, crtNSSize, crtHourGlass,
    crtDrag, crtNoDrop, crtHSplit, crtVSplit, crtMultiDrag, crtHelp, crtHandPoint);

  TMessageType = (msNone, msInfo, msQuestion, msWarning, msError);

  TColorTarget = (ctNone, ctBackground, ctText);

  TTranslationPart = (tpCaption, tpEmptyValue, tpPrefix, tpHint);

  TPeriodType = (ptAll, ptCurrentYear, ptCurrentMonth, ptCurrentWeek, ptCurrentDay,
    ptPastYear, ptPastMonth, ptPastWeek, ptYesterday);
  TMonth = (mnJanuary, mnFebruary, mnMarch, mnApril, mnMay, mnJune, mnJuly, mnAugust,
    mnSeptember, mnOctober, mnNovember, mnDecember);
  TUserKind = (ukGuest, ukSystem, ukUser, ukAdmin);

  TTextPosition = (tpDefault, tpIn, tpLeft, tpRight, tpTop, tpBottom, tpCenter);
  TBrushStyle = (bsSolid, bsClear, bsHorizontal, bsVertical, bsFDiagonal, bsBDiagonal, bsCross, bsDiagCross);
  TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear, psInsideFrame, psUserStyle, psAlternate);

  TBackground = record
    Color: TColor;
    RelativeRed: SmallInt;
    RelativeGreen: SmallInt;
    RelativeBlue: SmallInt;
  end;

  TReportValue = record
    Value: Variant;
    FieldKind: TFieldKind;
    Extra: string; // Тип флага или перечисления
  end;

const
  cNullReportValue: TReportValue = (FieldKind: fkNotDefined);

function Background(AColor: TColor;
  ARelativeRed: SmallInt; ARelativeGreen: SmallInt; ARelativeBlue: SmallInt): TBackground;

const
  cSaveActionNames: array[TEntitySaveAction] of string =
    ('Undefined', 'Insertion', 'Modification', 'Deletion');
  cCollectionKindNames: array[TCollectionKind] of string =
    ('<undefined>', 'Library', 'Document', 'Mixin');
  cFieldKindNames: array[TFieldKind] of string =
    ('<undefined>', 'String', 'Integer', 'Enumeration', 'Flag', 'Float', 'DateTime', 'Boolean',
     'Color', 'Currency', 'Object', 'List', 'Blob', 'Complex');
  cEnumTypeNames: array[TEnumType] of string =
    ('По порядку', 'Битовая маска', 'Произвольный');
  cStorageKindNames: array[TStorageKind] of string =
    ('<not save>', 'Database', 'Shared Folder');
  cSearchTypeNames: array[TSearchType] of string =
    ('<undefined>', 'from begin', 'everywhere', 'everywhere all');
  cSortTypeNames: array[TEntitySortType] of string =
    ('UI', 'by ID', 'by Name', 'by Type & Name', 'by Color', 'by Order');
  cRelationPowerNames: array[TRelationPower] of string =
    ('Weak', 'Strong');
  cBlobFormatNames: array[TBlobFormat] of string =
    ('Raw', 'Text', 'Document', 'Image', 'Audio', 'Video');
  cIntegrationKindNames: array[TIntegrationKind] of string =
    ('Перевод', 'Мессенджеры', 'Электронная почта');
  cHTTPMethodNames: array[THTTPHeaderMethod] of string =
    ('GET', 'POST', 'PUT', 'DELETE', 'PATCH', 'HEAD');
  cAuthorizationTypeNames: array[TAuthorizationType] of string =
    ('', 'Basic', 'Bearer', 'Digest', 'Hawk', 'AWS4-HMAC-SHA256');
  cModifierNames: array[TConditionModifier] of string =
    ('', '@', '#', '%');
  cConditionNames: array[TConditionKind] of string =
    ('', '=', '^', '>', '<', '}', '{', '+');
  cLogMessageTypes: array[TMessageKind] of string =
    ('[ ]', '[A]', '[E]', '[W]', '[I]');
  cColorTargetNames: array[TColorTarget] of string =
    ('', 'Background', 'Foreground');
  cTranslationPartCaptions: array[TTranslationPart] of string =
    ('Caption', 'EmptyValue', 'Prefix', 'Hint');
  cCommitKindNames: array[TCommitKind] of string =
    ('При изменении', 'При выходе', 'При сохранении');
  cCursors: array[TCursorType] of TCursor = (crDefault, crNone, crArrow, crCross, crSizeWE, crSizeNS,
    crHourGlass, crDrag, crNoDrop, crHSplit, crVSplit, crMultiDrag, crHelp, crHandPoint);

const
  cMSAccessConnectionString: string =
    'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s;Persist Security Info=False';
  cTrustedMSSQLConnectionString: string =
    'Provider=SQLOLEDB.1;Integrated Security=SSPI;' +
    'Persist Security Info=False;Data Source=%s;Initial Catalog=%s';
  cRegularMSSQLConnectionString: string =
    'Provider=SQLOLEDB.1;Persist Security Info=False;' +
    'Data Source=%s;Initial Catalog=%s;User ID=%s;Password=%s';

function AggregationKindFromText(const AText: string): TAggregationKind;
function AggregationKindToText(const AKind: TAggregationKind): string;

function StrToCondition(const ACondition: string): TConditionKind;
function ApplyModifier(const AValue: string; const AModifier: TConditionModifier = cmNone): string;

function GetPlatformDir: string;
function GetCommonDir: string;
function GetDesktopDir: string;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows, ShlObj,
{$ENDIF}
  IOUtils, SysUtils;

////{$IFDEF POSIX}
////  Posix.Stdlib;
////function ExecuteFile(const AHandle: Cardinal; const AFileName, AParams: string; const Await: Boolean): Cardinal;
////begin
////  _system(PAnsiChar('open ' + AnsiString(AFileName)))
////end;

function GetPlatformDir: string;
begin
{$IFDEF MSWINDOWS}
  Result := TPath.GetDirectoryName(ParamStr(0));
{$ELSE}
  Result := TPath.Combine(TPath.GetHomePath, 'Common');
{$ENDIF}
end;

function GetCommonDir: string;
begin
{$IFDEF MSWINDOWS}
  Result := TPath.GetDirectoryName(ParamStr(0));
{$ELSE}
  Result := TPath.Combine(TPath.GetSharedDocumentsPath, cProductCreator);
{$ENDIF}
end;

function GetDesktopDir: string;
{$IFDEF MSWINDOWS}
var
  Buf: array[0..MAX_PATH - 1] of Char;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  SHGetSpecialFolderPath(0, Buf, CSIDL_DESKTOP, False);
  Result := Buf;
{$ELSE}
  Result := TPath.GetPublicPath;
{$ENDIF}
end;

const
  cAggregationKindNames: array[TAggregationKind] of string =
    ('', 'COUNT', 'SUM', 'AVG', 'MIN', 'MAX');

function AggregationKindFromText(const AText: string): TAggregationKind;
var
  vKind: TAggregationKind;
begin
  for vKind := Low(TAggregationKind) to High(TAggregationKind) do
    if SameText(AText, cAggregationKindNames[vKind]) then
    begin
      Result := vKind;
      Exit;
    end;
  Result := akNotDefined;
end;

function AggregationKindToText(const AKind: TAggregationKind): string;
begin
  Result := cAggregationKindNames[AKind];
end;

function StrToCondition(const ACondition: string): TConditionKind;
begin
  if ACondition = '=' then
    Result := ckEqualTo
  else if ACondition = '^' then
    Result := ckMatchesTo
  //else if ACondition = '$' then
  //  Result := ckEndsWith
  else if ACondition = '>' then
    Result := ckGreaterThan
  else if ACondition = '<' then
    Result := ckLessThan
  else if ACondition = '}' then
    Result := ckPartOf
  else if ACondition = '{' then
    Result := ckContains
  else if ACondition = '+' then
    Result := ckCrosses
  else begin
    Result := ckUndefined;
    Assert(False, 'Wrong compare operator "' + ACondition + '"');
  end;
end;

function ApplyModifier(const AValue: string; const AModifier: TConditionModifier = cmNone): string;
begin
  case AModifier of
    cmIgnoreCase: Result := AnsiUpperCase(AValue);
    cmIgnoreSpace: Result := Trim(AValue);
  else
    Result := AValue;
  end;
end;


function Background(AColor: TColor;
  ARelativeRed: SmallInt; ARelativeGreen: SmallInt; ARelativeBlue: SmallInt): TBackground;
begin
  Result.Color := AColor;
  Result.RelativeRed := ARelativeRed;
  Result.RelativeGreen := ARelativeGreen;
  Result.RelativeBlue := ARelativeBlue;
end;

end.
