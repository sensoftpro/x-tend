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

unit uDefinition;

interface

uses
  Classes, Generics.Collections, SysUtils, uFastClasses, uConsts, uReaction, uQueryDef;

type
  TDefinition = class;
  TActionDef = class;
  TFieldDef = class;

  TDefItem = class
  protected
    FName: string;
    [Weak] FOwner: TObject;
    [Weak] FDefinition: TDefinition;
    [Weak] FConfiguration: TObject;
  public
    constructor Create(const AOwner: TObject; const AName: string);

    property Name: string read FName;
    property Configuration: TObject read FConfiguration;
    property OwnerDefinition: TDefinition read FDefinition;
  end;

  TDefList<T: TDefItem> = class(TObjectStringList<T>)
  protected
    [Weak] FOwner: TObject;
    procedure ClearObjects; override;
  public
    constructor Create(const AOwner: TObject);

    procedure InternalAdd(const ADefItem: T);
  end;

  // группирование полей для визуального отображения
  {
    EntityDef
      Compositions
        Grid:Composition
        Form:Composition
        List:Composition
          Fields
          Groups
            Fields
  }

  TFilter = class(TDefItem)
  private
    FFullName: string;
    FCaption: string;
    FHint: string;
    FQuery: string;
    FQueryDef: TQueryDef;
  public
    constructor Create(const ADefinition: TDefinition; const AName, ACaption, AQuery: string);
    destructor Destroy; override;

    property FullName: string read FFullName;
    property Caption: string read FCaption;
    property Hint: string read FHint;
    property Query: string read FQuery;
    property QueryDef: TQueryDef read FQueryDef;
  end;

  TFilters = TObjectDictionary<string, TFilter>;

  TReductions = class(TStringList)
  public
    procedure AddReduction(const AFieldName: string; const AReductionKind: TAggregationKind);
    function ReductionForField(const AFieldName: string): TAggregationKind;
  end;

  TCubeField = class
  private
    FCaption: string;
    FField: TFieldDef;
    FAreaKind: TCubeAreaKind;
    FReductionKind: TAggregationKind;
    FPeriodReduction: string;
    FVisible: Boolean;
  public
    constructor Create(const ACaption: string; const AField: TFieldDef; const AAreaKind: TCubeAreaKind;
      const AReductionKind: TAggregationKind; const APeriodReduction: string = ''; const AVisible: Boolean = True);

    property Caption: string read FCaption;
    property Field: TFieldDef read FField;
    property AreaKind: TCubeAreaKind read FAreaKind;
    property ReductionKind: TAggregationKind read FReductionKind;
    property PeriodReduction: string read FPeriodReduction;
    property Visible: Boolean read FVisible;
  end;

  TDataCube = class
  private
    FDefinition: TDefinition;
    FName: string;
    FFilter: string;
    FFields: TObjectList<TCubeField>;
  public
    constructor Create(const ADefinition: TDefinition; const AName, AFilter: string);
    destructor Destroy; override;

    procedure AddField(const ACaption: string; const AFieldName: string; const AAreaKind: TCubeAreaKind;
      const AReductionKind: TAggregationKind; const APeriodReduction: string = ''; const AVisible: Boolean = True);

    property Name: string read FName;
    property Fields: TObjectList<TCubeField> read FFields;
    property Filter: string read FFilter;
  end;

  TDataCubes = class(TObjectStringDictionary<TDataCube>)
  private
    FDefinition: TDefinition;
    function GetCube(const AName: string): TDataCube;
  public
    constructor Create(const ADefinition: TDefinition);
    destructor Destroy; override;

    function AddCube(const AName, AFilter: string): TDataCube;
    property Cube[const AName: string]: TDataCube read GetCube;
  end;

  TActions = class;

  TReportDef = class;
  TReports = class;

  TRTFReport = class;
  TRTFReports = class;

  TNotificationChain = class
  private
    FTransitFields: TList<TFieldDef>;
    FReactions: TObjectDictionary<string, THandlers>;
  public
    destructor Destroy; override;

    procedure AddReaction(const ADefinitionName: string; const AReactionProc: TReactionHandler);
    procedure AddTransitField(const ATransitFieldDef: TFieldDef);
  end;

  TNotificationChains = class
  private
    FCalculations: THandlers;
    FUICalculations: THandlers;
    FChains: TObjectDictionary<string, TNotificationChain>;
    FHasTransit: Boolean;
    FHasListener: Boolean;
    function GetChain(const AChain: string): TNotificationChain;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddCalculation(const AReactionProc: TReactionHandler);
    procedure AddUICalculation(const AReactionProc: TReactionHandler);
    procedure AddListener(const AChain: string; const ADefinitionName: string; const AReactionProc: TReactionHandler);
    procedure AddTransit(const AChain: string; const ANextFieldDef: TFieldDef);

    function HasNotificationChain(const ACurChain: string): Boolean;
    function TryGetReactions(const ACurChain: string; const ADefinitionName: string; var AReactions: THandlers): Boolean;
    function TryGetTransitFields(const ACurChain: string; var AFields: TList<TFieldDef>): Boolean;
    property Calculations: THandlers read FCalculations;
    property UICalculations: THandlers read FUICalculations;
  end;

  TFieldDef = class
  private
    FName: string;
    FKind: TFieldKind;
    FSize: Integer;
    // Локализуемые значения
    FFullName: string; // ключ
    FCaption: string;
    FHint: string;
    // Отображение
    FStyleName: string;
    // Структурные
    FDependentFields: TStrings; // TODO: мастер-поля должны объявляться раньше зависимых
    FUsedInFilters: Boolean;
    // Связь с хранилищем
    FStorageName: string;
    FStorageKind: TStorageKind;
    // Умолчания для инстанцируемых полей
    FFlags: Integer;
    FUIState: TViewState;
    FFormat: string;
    FCommitKind: TCommitKind;
  public
    FNotificationChains: TNotificationChains;
    procedure CopyNotificationChains(const ATarget: TFieldDef);
  protected
    function GetFieldSize: Integer; virtual;
    function CalcStorageName: string; virtual;
  protected
    FOwner: TDefinition;
    function GetConfiguration: TObject;
  public
    // При поступлении нотификации о смене языка, запрашиваем у домена новые переводы
    // Оповещаем видимые элементы при изменениях

    constructor Create(const AOwner: TDefinition; const AName: string; const AKind: TFieldKind = fkString;
      const AUIState: TViewState = vsFullAccess; const AFlags: Integer = 0; const ASize: Integer = -1); overload;
    constructor Create(const AOwner: TDefinition; const AFieldKind: TFieldKind; const AFieldName, AStorageName,
      ACaption, AHint, AViewName: string; const AUIState: TViewState; const AFlags: Integer); overload;
    destructor Destroy; override;

    function SetCaption(const ACaption: string): TFieldDef;     // localize
    function SetHint(const AHint: string): TFieldDef;           // localize
    function SetStorageName(const AStorageName: string): TFieldDef; // calculated
    function SetStorageKind(const AStorageKind: TStorageKind): TFieldDef;
    function SetStyleName(const AStyleName: string): TFieldDef;     // default
    function SetFlags(const AFlags: Cardinal): TFieldDef;           // default
    function SetUIState(const AUIState: TViewState): TFieldDef;           // default
    function SetFormat(const AFormat: string): TFieldDef;
    function SetCommitKind(const ACommitKind: TCommitKind): TFieldDef;

    function HasFlag(const AFlag: Integer): Boolean;

    property Name: string read FName;
    property FullName: string read FFullName;
    property Kind: TFieldKind read FKind;
    property StorageName: string read FStorageName;
    property StorageKind: TStorageKind read FStorageKind;
    property Size: Integer read FSize;
    property _Caption: string read FCaption;
    property _Hint: string read FHint;
    property StyleName: string read FStyleName;
    property UIState: TViewState read FUIState;
    property Format: string read FFormat;
    property CommitKind: TCommitKind read FCommitKind;
    property Flags: Integer read FFlags;
    property UsedInFilters: Boolean read FUsedInFilters;
    property Definition: TDefinition read FOwner;
    property DependentFields: TStrings read FDependentFields;
  end;

  TServiceFieldDef = class(TFieldDef)
  end;

  TSimpleFieldDef = class(TFieldDef)
  private
    FMinValue: Variant;
    FMaxValue: Variant;
    FDefaultValue: Variant;
    FNullValue: Variant;
    FDictionary: string;
    FDictionaryField: string;
    FRankField: string;
    FSearchType: TSearchType;
    FDictionaryText: string;
    FInitialSearchType: TSearchType;
    FPrecision: TPrecisionType;
  protected
    function SetNullValue(const ANullValue: Variant): TSimpleFieldDef;
  public
    constructor Create(const AOwner: TDefinition; const AFieldKind: TFieldKind;
      const AFieldName, AStorageName, ACaption, AHint, AViewName: string;
      const AMinValue, AMaxValue, ADefaultValue: Variant; const AUIState: TViewState;
      const AFlags: Integer; const ADictionary: string;
      const ASearchType: TSearchType);

    function SetDefaultValue(const AValue: Variant): TSimpleFieldDef;
    function SetValueRange(const AMinValue, AMaxValue: Variant): TSimpleFieldDef;
    function SetPrecision(const AType: TPrecisionType): TSimpleFieldDef;

    property MinValue: Variant read FMinValue;
    property MaxValue: Variant read FMaxValue;
    //property CanBeNull: Boolean read FCanBeNull;
    property NullValue: Variant read FNullValue;
    property DefaultValue: Variant read FDefaultValue;
    property Dictionary: string read FDictionary;
    property DictionaryField: string read FDictionaryField;
    property RankField: string read FRankField;
    property SearchType: TSearchType read FSearchType;
  end;

  TBlobFieldDef = class(TFieldDef)
  protected
    FBlobFormat: TBlobFormat;
  public
    constructor Create(const AOwner: TDefinition; const AFieldName, AStorageName, ACaption: string;
      const AFormat: TBlobFormat; const AHint, AViewName: string; const AUIState: TViewState; const AFlags: Integer);
    property BlobFormat: TBlobFormat read FBlobFormat;
  end;

  TComplexFieldDef = class(TFieldDef)
  protected
    FObjectKindName: string;
  public
    constructor Create(const AOwner: TDefinition; const AFieldName, AStorageName, ACaption,
      AObjectKindName, AHint, AViewName: string; const AUIState: TViewState; const AFlags: Integer);
    property ObjectKindName: string read FObjectKindName;
  end;

  TObjectFieldDef = class(TFieldDef)
  private
    FIsSelector: Boolean;
    FDefaultTypeName: string;
    FDefaultTypeID: Integer;
    FHiddenFields: TStringList;
    FQueryString: string;
    FQueryDef: TQueryDef;
    FExclusiveQueryString: string;
    FExclusiveQueryDef: TQueryDef;
    FContentDefinitionName: string;
    FContentDefinition: TDefinition;
    FContentTypeLocator: string;
    FFilter: string;
    function GetContentDefinitions: TList<TDefinition>;
    function GetDefaultDefinitionName: string;
    function GetContentDefinitionsText: string;
    function GetDefaultTypeID: Integer;
  protected
    FNullString: string;
    FHiddenFieldsText: string;
    FDependenciesText: string;
    FSortType: TEntitySortType;
    FSearchType: TSearchType;
    FSelectiveFields: TStrings;
    function DefinitionsToText(const ADefinitions: TList<TDefinition>): string;
    procedure AddExclusiveCondition(const AQuery: TStrings); virtual;
    procedure RecalculateQueries; virtual;
  public
    constructor Create(const AOwner: TDefinition; const AFieldKind: TFieldKind;
      const AFieldName, AStorageName, ACaption, AHint, ANullString,
      AContentTypeName: string; const AViewName: string = '';
      const AUIState: TViewState = vsFullAccess; const AFlags: Integer = 0;
      const ASortType: TEntitySortType = estUserSort;
      const AHiddenFields: string = ''; const ADependencies: string = '';
      const ASearchType: TSearchType = stSearchEverywhere;
      const AFilter: string = '');
    destructor Destroy; override;

    function IsFieldHidden(const AFieldName: string): Boolean;
    procedure CreateRelations;
    procedure SetContentDefinition(const ADefinition: TDefinition);
    // Процедура для миграции.
    // Когда сущность становится частью иерархии, этот метод позволяет указать, какой тип будет установлен в
    //   typeid поле, конкретизирующее селекторный тип.
    procedure SetDefaultTypeName(const ADefaultTypeName: string);
    function SetDependenciesAndFilter(const ADependencies, AFilter: string): TObjectFieldDef;

    property ContentDefinitionName: string read FContentDefinitionName;
    property _ContentDefinition: TDefinition read FContentDefinition;
    property ContentTypeLocator: string read FContentTypeLocator;
    property Filter: string read FFilter;
    property ContentDefinitions: TList<TDefinition> read GetContentDefinitions;
    property ContentDefinitionsText: string read GetContentDefinitionsText;
    property DefaultDefinitionName: string read GetDefaultDefinitionName;
    property IsSelector: Boolean read FIsSelector;
    property DefaultTypeID: Integer read GetDefaultTypeID;
    property SortType: TEntitySortType read FSortType;
    property SearchType: TSearchType read FSearchType;
    property SelectiveFields: TStrings read FSelectiveFields;
    property QueryDef: TQueryDef read FQueryDef;
    property ExclusiveQueryDef: TQueryDef read FExclusiveQueryDef;
  end;

  TEntityFieldDef = class(TObjectFieldDef)
  private
    FDefaultEntityID: Integer;
    FSelectorStorageName: string;
  public
    constructor Create(const AOwner: TDefinition;
      const AFieldName, AStorageName, ACaption, AHint, ANullString,
      AContentTypeName: string; const AViewName: string = '';
      const ADefaultEntityID: Integer = 0;
      const AUIState: TViewState = vsFullAccess; const AFlags: Integer = 0;
      const ASortType: TEntitySortType = estSortByName;
      const ADependencies: string = ''; const AHiddenFields: string = '';
      const ASearchType: TSearchType = stSearchFromBegin;
      const AFilter: string = '');

    property DefaultEntityID: Integer read FDefaultEntityID;
    property SelectorStorageName: string read FSelectorStorageName;
  end;

  TListFieldDef = class(TObjectFieldDef)
  private
    FMasterFieldName: string;
    FColorFieldName: string;
    FRelationPower: TRelationPower;
    FContentQueryDef: TQueryDef;
  protected
    procedure AddExclusiveCondition(const AQuery: TStrings); override;
    procedure RecalculateQueries; override;
  public
    constructor Create(const AOwner: TDefinition;
      const AFieldName, AMasterFieldName, ACaption, AHint, ANullString, AContentTypeName: string;
      const AViewName: string = '';
      const AUIState: TViewState = vsFullAccess; const AFlags: Integer = 0;
      const ASortType: TEntitySortType = estUserSort;
      const AColorFieldName: string = '';
      const ARelationPower: TRelationPower = rpWeak;
      const AHiddenFields: string = ''; const ADependencies: string = '';
      const ASearchType: TSearchType = stSearchFromBegin;
      const AFilter: string = '');
    destructor Destroy; override;

	  property MasterFieldName: string read FMasterFieldName;
    property ColorFieldName: string read FColorFieldName;
    property RelationPower: TRelationPower read FRelationPower;
    property ContentQueryDef: TQueryDef read FContentQueryDef;
  end;

  TOwnerListFields = TObjectDictionary<TFieldDef, TList<TListFieldDef>>;

  TDefinition = class(TDefItem)
  private
    FID: Integer;
    FIsCollection: Boolean;
    FKind: TCollectionKind;
    FStorageName: string;
    FStorageKind: TStorageKind;
    FEmptyValue: string;
    FPrefix: string;
    // Отображение
    FBackground: TBackground;

    // Общие из TBaseDefinition
    FFullName: string;
    FCaption: string;
    FImageID: string;
    FGroupFieldName: string;
    FColorFieldName: string;
    FColorTarget: TColorTarget;
    FLayoutMask: string;
    FUIState: TViewState;
    FFlags: Integer;
    FOrderNum: Integer;

    // Уникальность записей
    FIndexString: string;
    FCheckUniqueQuery: TQueryDef;
    // Из Entity
    FActions: TActions;
    FReactions: TReactions;
    FFilters: TFilters;
    FReductions: TReductions;
    FReports: TReports;
    FRTFReports: TRTFReports;
    FDataCubes: TDataCubes;
  private // Сервисные поля
    // Коллекции-предки
    FAncestors: TStrings;
    // Коллекции-наследники
    FDescendants: TList<TDefinition>;
    // Реальный контент
    FInnerDefinitions: TList<TDefinition>;
    // Все поля сущностей
    FFields: TStringDictionary<TFieldDef>;
    // Сервисные поля
    FServiceFields: TObjectStringList<TFieldDef>;
    // Поле статуса
    FStateFieldDef: TSimpleFieldDef;

    FOwnerLists: TOwnerListFields;

    procedure InternalTryDeleteField(const AFieldDef: TFieldDef);
    procedure InternalAddField(const AFieldDef: TFieldDef);
  protected
    procedure InternalSetFlags(const AFlags: Integer);
    function IsParam: Boolean; virtual;
  public
    constructor Create(const AOwner: TObject; const AIsCollection: Boolean; const AName: string;
      const AAncestors: string = ''; const AKind: TCollectionKind = clkLibrary);
    destructor Destroy; override;

    function FieldExists(const AFieldName: string): Boolean; inline;
    function FieldByName(const AFieldName: string): TFieldDef;
    function IndexOfField(const AFieldName: string): Integer; inline;
    function FieldNameByStorageName(const AStorageName: string): string;
    function ExtractFieldDef(const AFieldName: string): TFieldDef;
    function ActionByName(const AName: string): TActionDef;
    function ReportByName(const AName: string): TDefinition;
    function HasFlag(const AFlag: Integer): Boolean;

    function GetLinkedOwnerLists(const AFieldDef: TFieldDef): TList<TListFieldDef>;
    procedure AddOwnerList(const AListFieldDef: TListFieldDef);

    function SetStorageName(const AStorageName: string): TDefinition;
    function SetStorageKind(const AStorageKind: TStorageKind): TDefinition;
    function SetCaption(const ACaption: string): TDefinition;
    function SetEmptyValue(const AEmptyValue: string): TDefinition;
    function SetPrefix(const APrefix: string): TDefinition;
    function SetFlags(const AFlags: Integer): TDefinition;
    function SetID(const AID: Integer): TDefinition;

    function SetLayoutMask(const AMask: string): TDefinition;
    function SetColorFieldName(const AColorFieldName: string): TDefinition;
    function SetColorTarget(const AColorTarget: TColorTarget): TDefinition;
    function SetImageID(const AImageID: string): TDefinition;
    function SetOrderNum(const AOrderNum: Integer): TDefinition;
    function SetBackground(const ABackground: TBackground): TDefinition;
    function SetGroupFieldName(const AGroupFieldName: string): TDefinition;

    procedure AddDescendant(const ADefinition: TDefinition);
    function IsDescendantOf(const ADefinitionName: string): Boolean;

    procedure ApplyInheritance;
    procedure UpdateInheritance;
    procedure FillContentDefinitions(const AContentDefinitions: TList<TDefinition> = nil);
    procedure CreateObjectRelations;
    procedure CreateListRelations;

    function AddSimpleFieldDef(const AFieldName, AStorageName, ACaption: string;
       const ADefaultValue, AMinValue, AMaxValue: Variant;
       const AFieldKind: TFieldKind = fkString; const AViewName: string = '';
       const AHint: string = ''; const AUIState: TViewState = vsFullAccess; const AFlags: Integer = 0;
       const ADictionary: string = ''; const ASearchType: TSearchType = stSearchNone): TSimpleFieldDef;
    function AddEntityFieldDef(const AFieldName, AStorageName, ACaption, AHint,
      AContentTypeName: string; const ADefaultEntityID: Integer = 0;
      const AUIState: TViewState = vsFullAccess; const AFlags: Integer = 0; const ASortType:
      TEntitySortType = estSortByName; const AViewName: string = '';
      const ADependencies: string = ''; const AHiddenFields: string = ''; const ASearchType:
       TSearchType = stSearchEverywhere; const AFilter: string = ''): TEntityFieldDef;
    function AddListFieldDef(const AFieldName, AOwnerFieldName, ACaption, AContentTypeName: string;
      const ASlaveFieldName: string = ''; const AViewName: string = '';
      const AUIState: TViewState = vsFullAccess; const AFlags: Integer = 0;
      const ASortType: TEntitySortType = estUserSort;
      const AColorFieldName: string = '';
      const ARelationPower: TRelationPower = rpWeak;
      const AHiddenFields: string = ''; const ADependencies: string = '';
      const AFilter: string = ''): TListFieldDef;
    function AddServiceFieldDef(const AFieldName, AStorageName, ACaption: string;
      const AFieldKind: TFieldKind = fkInteger; const ASize: Integer = -1): TServiceFieldDef;
    function AddBlobFieldDef(const AFieldName, AStorageName, ACaption: string; const AFormat: TBlobFormat = bffRaw;
      const AViewName: string = ''; const AUIState: TViewState = vsFullAccess; const AFlags: Integer = 0): TBlobFieldDef;
    function AddComplexFieldDef(const AFieldName, AStorageName, ACaption, AObjectKindName: string;
      const AViewName: string = ''; const AUIState: TViewState = vsFullAccess; const AFlags: Integer = 0): TComplexFieldDef;
    function AddStateFieldDef(const AFieldName, AStorageName, ACaption: string;
      const ADefaultValue: Variant; const AEnumName: string; const AFlags: Integer = 0): TSimpleFieldDef;

    procedure RemoveField(const AFieldName: string);

    procedure AddFilter(const AName, ACaption, AQuery: string);
    procedure AddReduction(const AFieldName: string; const AAggType: TAggregationKind);
    function AddDataCube(const AName, AFilter: string): TDataCube;
    procedure AddUniqueIndex(const AFullIndexString: string);
    function AddAction(const AName, ACaption, AImageID: string; const AFlags: Integer = 0): TActionDef;
    function AddReport(const AReportName, ACaption, AFileName: string; const AOutputFileMask: string = ''): TReportDef;
    function AddRTFReport(const AName, ACaption, AFileName: string): TRTFReport;
    procedure AppendReaction(const AReactiveFields, AFieldChains: TStrings; const AReactionProc: TProc);
    procedure RegisterReaction(const AReactiveFields, AFieldChain: string; const AReactionProc: TProc);

    property StateFieldDef: TSimpleFieldDef read FStateFieldDef;

    property InnerDefinitions: TList<TDefinition> read FInnerDefinitions;

    property ID: Integer read FID;
    property Name: string read FName;
    property FullName: string read FFullName;
    property _Caption: string read FCaption;
    property _ImageID: string read FImageID;
    property OrderNum: Integer read FOrderNum;
    property LayoutMask: string read FLayoutMask;
    property GroupFieldName: string read FGroupFieldName;
    property ColorFieldName: string read FColorFieldName;
    property ColorTarget: TColorTarget read FColorTarget;
    property Flags: Integer read FFlags;
    property UIState: TViewState read FUIState;
    property IsItParameter: Boolean read IsParam;

    property IsCollection: Boolean read FIsCollection;
    property Kind: TCollectionKind read FKind;
    property StorageName: string read FStorageName;
    property StorageKind: TStorageKind read FStorageKind;
    property _EmptyValue: string read FEmptyValue;
    property Prefix: string read FPrefix;
    property Background: TBackground read FBackground;
    property Ancestors: TStrings read FAncestors;

    property IndexString: string read FIndexString;
    procedure GetAllActions(const AActions: TList<TActionDef>);
    procedure GetAllReports(const AReports: TList<TRTFReport>);
    property CheckUniqueQuery: TQueryDef read FCheckUniqueQuery;

    property Actions: TActions read FActions;
    property Reports: TReports read FReports;
    property RTFReports: TRTFReports read FRTFReports;
    property Reactions: TReactions read FReactions;
    property Filters: TFilters read FFilters;
    property Reductions: TReductions read FReductions;
    property DataCubes: TDataCubes read FDataCubes;
    property Fields: TStringDictionary<TFieldDef> read FFields;
    property ServiceFields: TObjectStringList<TFieldDef> read FServiceFields;
    property Descendants: TList<TDefinition> read FDescendants;
  end;

  TDefinitions = class(TObjectStringDictionary<TDefinition>)
  private
    [Weak] FConfiguration: TObject;
  public
    constructor Create(const AConfiguration: TObject);

    function Add(const AName, AAncestors: string; const AKind: TCollectionKind = clkLibrary): TDefinition;
    procedure DefinitionsByKind(const AList: TList<TDefinition>; const ADefinitionKind: TCollectionKind);
  end;

  // Действия
  TActionDef = class(TDefinition)
  protected
    function IsParam: Boolean; override;
  public
    constructor Create(const AOwner: TObject; const AName, ACaption, AImageID: string);
  end;

  TActions = class(TDefList<TActionDef>)
  public
    function Add(const AName, ACaption, AImageID: string; const AFlags: Integer): TActionDef;
  end;

  TComplexClassDef = class(TDefItem)
  private
    FComplexClass: TObject;
  public
    constructor Create(const AOwner: TObject; const AName: string; const AComplexClass: TObject);
    destructor Destroy; override;
    property ComplexClass: TObject read FComplexClass;
  end;

  TComplexClasses = class(TDefList<TComplexClassDef>)
  public
    function Add(const AName: string; const AComplexClass: TObject): TComplexClassDef;
  end;

  // Отчеты

  // Аггрегации поддерживаются для групп и наборов данных
  TAggregationDef = class
  private
    FName: string;
    FAggKind: TAggregationKind;
    FFieldKind: TFieldKind;
    FObservedFieldName: string;
    FIsNot: Boolean;
    FConditionField1: string;
    FConditionField2: string;
    FConditionKind: TConditionKind;

    FIsValid: Boolean;
  public
    constructor Create(const AAggKind: TAggregationKind; const AName,
      AObservedFieldName: string; const AFieldKind: TFieldKind;
      const ACondition: string = '');

    property Name: string read FName;
    property FieldKind: TFieldKind read FFieldKind;
    property Kind: TAggregationKind read FAggKind;
    property ObservedFieldName: string read FObservedFieldName;
    property IsNot: Boolean read FIsNot;
    property ConditionField1: string read FConditionField1;
    property ConditionField2: string read FConditionField2;
    property ConditionKind: TConditionKind read FConditionKind;
  end;

  TAggregationDefs = class(TObjectStringList<TAggregationDef>)
  end;

  TLayer = class
  private
    FAggregationDefs: TAggregationDefs;
  protected
    FHeaderName: string;
    FFooterName: string;
  public
    constructor Create(const AHeaderName: string;
      const AFooterName: string = '');
    destructor Destroy; override;

    procedure AddAggregation(const AAggKind: TAggregationKind; const AName,
      AObservedFieldName: string; const AFieldKind: TFieldKind; const ACondition: string = '');

    property AggregationDefs: TAggregationDefs read FAggregationDefs;
    property HeaderName: string read FHeaderName;
    property FooterName: string read FFooterName;
  end;

  TGroupLayer = class(TLayer)
  private
    FBreakingField: string;
  public
    constructor Create(const AHeaderName, ABreakingField: string;
      const AFooterName: string = '');
  end;

  TDataLayer = class(TLayer)
  private
    FGroups: TObjectList<TLayer>;
    FData: TObjectList<TLayer>;
    FDataName: string;
    FFromClause: string;
    FDataQuery: TQueryDef;
    FBreakingFields: TStrings;
    FCount: Integer;
  public
    constructor Create(const AHeaderName, ADataName, AFooterName,
      AFromClause, AWhereClause, AOrderClause: string; const ACount: Integer);
    destructor Destroy; override;

    function AddGroup(const AHeaderName, ABreakingField: string;
      const AFooterName: string = ''): TGroupLayer;
    function AddData(const AHeaderName, ADataName, AFooterName,
      AFromClause, AWhereClause: string; const AOrderClause: string = ''): TDataLayer;
    function AddUserData(const AHeaderName, ADataName, AFooterName: string;
      const ACount: Integer): TDataLayer;

    property Groups: TObjectList<TLayer> read FGroups;
    property Data: TObjectList<TLayer> read FData;
    property DataName: string read FDataName;
    property Source: string read FFromClause;
    property DataQuery: TQueryDef read FDataQuery;
    property BreakingFields: TStrings read FBreakingFields;
    property Count: Integer read FCount;
  end;

  TReportDef = class(TDefinition)
  private
    FFileName: string;
    FLayers: TObjectList<TLayer>;
    FContent: TStringStream;
    FOutputFileMask: string;
  protected
    function IsParam: Boolean; override;
  public
    constructor Create(const ADefinition: TDefinition;
      const AReportName, ACaption, AFileName, AOutputFileMask: string);
    destructor Destroy; override;

    function AddData(const AHeaderName, ADataName, AFooterName,
      AFromClause, AWhereClause: string; const AOrderClause: string = ''): TDataLayer;
    function AddUserData(const AHeaderName, ADataName, AFooterName: string;
      const ACount: Integer): TDataLayer;

    property Owner: TDefinition read FDefinition;
    property Layers: TObjectList<TLayer> read FLayers;
    property FileName: string read FFileName;
    property Content: TStringStream read FContent;
    property OutputFileMask: string read FOutputFileMask;
  end;

  TReports = class(TDefList<TReportDef>)
  end;

  TRTFReport = class(TDefinition)
  private
    FFileName: string;
    FContent: TStringStream;
  public
    constructor Create(const ADefinition: TDefinition; const AName, ACaption, AFileName: string);
    destructor Destroy; override;

    procedure Save;

    //function GetCaption(const AInteractor: TInteractor): string;

    property Content: TStringStream read FContent;
  end;

  TRTFReports = class(TDefList<TRTFReport>)
  end;

implementation

uses
  IOUtils, Variants, IniFiles, Character, uConfiguration, uUtils;

type
  TCreateContentTypeReactionProc = procedure(const ADefinition: TDefinition; const ATargetFieldName,
    AContentTypePath: string) of object;

{ TDefinition }

function TDefinition.ActionByName(const AName: string): TActionDef;
var
  vDescDefinition: TDefinition;
begin
  Result := FActions.ObjectByName(AName);
  if not Assigned(Result) and (FDescendants.Count > 0) then
    for vDescDefinition in FDescendants do
    begin
      Result := vDescDefinition.ActionByName(AName);
      if Assigned(Result) then
        Exit;
    end;
end;

function TDefinition.AddAction(const AName, ACaption, AImageID: string; const AFlags: Integer): TActionDef;
begin
  Result := TActionDef.Create(Self, AName, ACaption, AImageID);
  if FIsCollection then
    Result.SetFlags(AFlags or ccContextAction);
  FActions.InternalAdd(Result);
end;

procedure TDefinition.AddReduction(const AFieldName: string; const AAggType: TAggregationKind);
begin
  FReductions.AddReduction(AFieldName, AAggType);
end;

function TDefinition.AddBlobFieldDef(const AFieldName, AStorageName, ACaption: string; const AFormat: TBlobFormat;
  const AViewName: string; const AUIState: TViewState; const AFlags: Integer): TBlobFieldDef;
begin
  Result := TBlobFieldDef.Create(Self, AFieldName, AStorageName, ACaption, AFormat, '', AViewName, AUIState, AFlags);
  InternalAddField(Result);
end;

function TDefinition.AddComplexFieldDef(const AFieldName, AStorageName, ACaption, AObjectKindName, AViewName: string;
  const AUIState: TViewState; const AFlags: Integer): TComplexFieldDef;
begin
  Result := TComplexFieldDef.Create(Self, AFieldName, AStorageName, ACaption,
    AObjectKindName, '', AViewName, AUIState, AFlags);
  InternalAddField(Result);
end;

function TDefinition.AddDataCube(const AName, AFilter: string): TDataCube;
begin
  Result := FDataCubes.AddCube(AName, AFilter);
end;

procedure TDefinition.AddDescendant(const ADefinition: TDefinition);
begin
  FDescendants.Add(ADefinition);
end;

function TDefinition.AddEntityFieldDef(const AFieldName, AStorageName, ACaption, AHint,
  AContentTypeName: string; const ADefaultEntityID: Integer; const AUIState: TViewState;
  const AFlags: Integer; const ASortType: TEntitySortType;
  const AViewName, ADependencies, AHiddenFields: string; const ASearchType: TSearchType;
  const AFilter: string): TEntityFieldDef;
begin
  Result := TEntityFieldDef.Create(Self, AFieldName, AStorageName, ACaption,
    AHint, cNullItemName, AContentTypeName, AViewName, ADefaultEntityID,
    AUIState, AFlags, ASortType, ADependencies, AHiddenFields, ASearchType, AFilter);
  InternalAddField(Result);
end;

procedure TDefinition.AddFilter(const AName, ACaption, AQuery: string);
var
  vFilter: TFilter;
begin
  vFilter := TFilter.Create(Self, AName, ACaption, AQuery);
  FFilters.AddOrSetValue(AName, vFilter);
end;

function TDefinition.AddListFieldDef(const AFieldName, AOwnerFieldName, ACaption, AContentTypeName,
  ASlaveFieldName, AViewName: string; const AUIState: TViewState; const AFlags: Integer;
  const ASortType: TEntitySortType; const AColorFieldName: string; const ARelationPower: TRelationPower;
  const AHiddenFields, ADependencies, AFilter: string): TListFieldDef;
begin
  Result := TListFieldDef.Create(Self, AFieldName,
    AOwnerFieldName, ACaption, '', '', AContentTypeName,
    AViewName, AUIState, AFlags, ASortType, AColorFieldName, ARelationPower,
    AHiddenFields, ADependencies, stSearchFromBegin, AFilter);
  InternalAddField(Result);
end;

procedure TDefinition.AddOwnerList(const AListFieldDef: TListFieldDef);
var
  vFilterFields: TStrings;
  i: Integer;

  procedure InternalAdd(const AFieldDef: TFieldDef; const AIsFilter: Boolean);
  var
    vOwnerLists: TList<TListFieldDef>;
  begin
    if not Assigned(AFieldDef) then
      Exit;

    if AIsFilter then
      AFieldDef.FUsedInFilters := True;

    if not FOwnerLists.TryGetValue(AFieldDef, vOwnerLists) then
    begin
      vOwnerLists := TList<TListFieldDef>.Create;
      FOwnerLists.Add(AFieldDef, vOwnerLists);
      vOwnerLists.Add(AListFieldDef);
    end
    else if vOwnerLists.IndexOf(AListFieldDef) < 0 then
      vOwnerLists.Add(AListFieldDef);
  end;
begin
  InternalAdd(FieldByName(AListFieldDef.MasterFieldName), False);

  // Добавить фильтры
  vFilterFields := AListFieldDef.ContentQueryDef.FilterFields;
  if vFilterFields.Count > 1 then
    for i := 1 to vFilterFields.Count - 1 do
      InternalAdd(FieldByName(vFilterFields[i]), True);
end;

procedure TDefinition.InternalAddField(const AFieldDef: TFieldDef);
var
  vField: TFieldDef;
begin
  vField := FFields.ObjectByName(AFieldDef.Name);
  if not Assigned(vField) then
    FFields.AddObject(AFieldDef.Name, AFieldDef)
  else begin
    if vField.FOwner = Self then
      FreeAndNil(vField)
    else // Скопировать подписки
      vField.CopyNotificationChains(AFieldDef);

    FFields.AddOrSetObject(AFieldDef.Name, AFieldDef);
  end;
end;

procedure TDefinition.InternalSetFlags(const AFlags: Integer);
begin
  FFlags := AFlags;
  FUIState := vsFullAccess;
  if HasFlag(ccSystem) then {or HasFlag(ccHideInMenu)}
    FUIState := FUIState and vsReadOnly;
end;

function TDefinition.AddRTFReport(const AName, ACaption, AFileName: string): TRTFReport;
begin
  Result := TRTFReport.Create(Self, AName, ACaption, AFileName);
  FRTFReports.InternalAdd(Result);
end;

function TDefinition.AddServiceFieldDef(const AFieldName, AStorageName, ACaption: string;
  const AFieldKind: TFieldKind = fkInteger; const ASize: Integer = -1): TServiceFieldDef;
begin
  Result := TServiceFieldDef.Create(Self, AFieldName, AFieldKind, vsHidden, cSystem, ASize);
  Result.SetCaption(ACaption);
  FServiceFields.AddObject(AFieldName, Result);
end;

function TDefinition.AddSimpleFieldDef(const AFieldName, AStorageName, ACaption: string;
  const ADefaultValue, AMinValue, AMaxValue: Variant; const AFieldKind: TFieldKind; const AViewName, AHint: string;
  const AUIState: TViewState; const AFlags: Integer; const ADictionary: string;
  const ASearchType: TSearchType): TSimpleFieldDef;
begin
  if (AFieldKind = fkString) and (VarIsNull(AMaxValue) or (AMaxValue = 0)) then
    Assert(False, 'Undefined length for field: ' + AFieldName);

  if (AFieldKind = fkInteger) and (not VarIsNull(AMinValue)) and (not VarIsNull(AMaxValue)) and (AMinValue >= AMaxValue) then
    Assert(False, 'Min value must be less than Max value for field: ' + AFieldName);

  Result := TSimpleFieldDef.Create(Self, AFieldKind, AFieldName, AStorageName,
    ACaption, AHint, AViewName, AMinValue, AMaxValue, ADefaultValue, AUIState, AFlags, ADictionary, ASearchType);
  InternalAddField(Result);

  case AFieldKind of
    fkString: Result.FNullValue := '';
    fkInteger: Result.FNullValue := cNullInteger;
    fkEnum: Result.FNullValue := cNullInteger;
    fkFlag: Result.FNullValue := cNullInteger;
    fkFloat: Result.FNullValue := cNullFloat;
    fkDateTime: Result.FNullValue := cNullDateTime;
    fkBoolean: Result.FNullValue := False;
    fkColor: Result.FNullValue := cNullColor;
    fkCurrency: Result.FNullValue := cNullCurrency;
    fkObject: Result.FNullValue := 0;
    fkBlob: Result.FNullValue := 0;
    fkComplex: Result.FNullValue := 0;
  else
    Result.FNullValue := Null;
  end;
end;

function TDefinition.AddStateFieldDef(const AFieldName, AStorageName, ACaption: string;
  const ADefaultValue: Variant; const AEnumName: string; const AFlags: Integer = 0): TSimpleFieldDef;
begin
  Result := AddSimpleFieldDef(AFieldName, AStorageName, ACaption,
    ADefaultValue, 0, Null, fkEnum, 'info', '', vsReadOnly, AFlags, AEnumName);
  FStateFieldDef := Result;
end;

function TDefinition.AddReport(const AReportName, ACaption, AFileName, AOutputFileMask: string): TReportDef;
begin
  Result := TReportDef.Create(Self, AReportName, ACaption, AFileName, AOutputFileMask);
  FReports.AddObject(AReportName, Result);
end;

procedure TDefinition.AddUniqueIndex(const AFullIndexString: string);
var
  vTempParser: TStringList;
  i: Integer;
  vQuery: string;

  function ParseIndex(const AIndexStr: string): string;
  var
    j: Integer;
    vParser: TStringList;
  begin
    Result := '';
    vParser := TStringList.Create;
    try
      vParser.Delimiter := ',';
      vParser.DelimitedText := AIndexStr;
      for j := 0 to vParser.Count - 1 do
      begin
        if j > 0 then
          Result := Result + ',';
        Result := Result + vParser[j] + '=' + vParser[j];
      end;
      if vParser.Count > 1 then
        Result := '&(' + Result + ')';
    finally
      FreeAndNil(vParser);
    end;
  end;
begin
  FreeAndNil(FCheckUniqueQuery);

  FIndexString := AFullIndexString;
  vQuery := '';
  vTempParser := TStringList.Create;
  vTempParser.Delimiter := ';';
  try
    vTempParser.DelimitedText := AFullIndexString;
    for i := 0 to vTempParser.Count - 1 do
    begin
      if i > 0 then
        vQuery := vQuery + ',';
      vQuery := vQuery + ParseIndex(vTempParser[i]);
    end;
    if vTempParser.Count > 1 then
      vQuery := '|(' + vQuery + ')';
  finally
    FreeAndNil(vTempParser);
  end;

  vQuery := '&(*!=*,' + vQuery + ')';
  FCheckUniqueQuery := TQueryDef.Create(FName, vQuery);
end;

procedure TDefinition.AppendReaction(const AReactiveFields, AFieldChains: TStrings; const AReactionProc: TProc);
var
  vFieldName: string;
  vFieldDef: TFieldDef;
  vDescDefinition: TDefinition;
begin
  for vFieldName in AReactiveFields do
  begin
    if Pos('@', vFieldName) = 1 then
    begin
      vFieldDef := FieldByName(Copy(vFieldName, 2, Length(vFieldName) - 1));
      if Assigned(vFieldDef) then
        // Можно сразу проверить на NotSave и Calculated
        vFieldDef.FNotificationChains.AddUICalculation(AReactionProc);
    end
    else begin
      vFieldDef := FieldByName(vFieldName);
      if Assigned(vFieldDef) then
        // Можно сразу проверить на NotSave и Calculated
        vFieldDef.FNotificationChains.AddCalculation(AReactionProc);
    end;
  end;

  FReactions.RegisterReaction(AFieldChains, AReactionProc);
  for vDescDefinition in FDescendants do
    vDescDefinition.AppendReaction(AReactiveFields, AFieldChains, AReactionProc);
end;

procedure TDefinition.ApplyInheritance;
var
  vDefinitionFilled: Boolean;
  i: Integer;
  vDefinition: TDefinition;
  vFilter: TFilter;
  vAction: TActionDef;
  vReport: TReportDef;
  vRTFReport: TRTFReport;
  vFieldDef: TFieldDef;
  //vCube: TDataCube;
begin
  vDefinitionFilled := False;
  for i := 0 to FAncestors.Count - 1 do
  begin
    vDefinition := TConfiguration(FConfiguration)[FAncestors[i]];
    if not Assigned(vDefinition) then
      Continue;

    if not vDefinitionFilled then
    begin
      FCaption := vDefinition._Caption;
      FEmptyValue := vDefinition._EmptyValue;
      FPrefix := vDefinition.Prefix;
      FFlags := vDefinition.Flags;
      FUIState := vDefinition.UIState;
      FColorFieldName := vDefinition.ColorFieldName;
      FColorTarget := vDefinition.ColorTarget;
      FImageID := vDefinition.FImageID;
      FOrderNum := vDefinition.OrderNum;
      FBackground := vDefinition.Background;
      FGroupFieldName := vDefinition.GroupFieldName;
      vDefinitionFilled := True;
    end;

    vDefinition.AddDescendant(Self);

    for vFieldDef in vDefinition.Fields do
      InternalAddField(vFieldDef);

    if Assigned(vDefinition.StateFieldDef) then
      FStateFieldDef := vDefinition.StateFieldDef;

    // Заполнить структуры от предков
    if not Assigned(FCheckUniqueQuery) and (vDefinition.IndexString <> '') then
      AddUniqueIndex(vDefinition.IndexString);

    FReductions.AddStrings(vDefinition.FReductions);

    for vFilter in vDefinition.Filters.Values do
      AddFilter(vFilter.Name, vFilter.Caption, vFilter.Query);

    for vAction in vDefinition.Actions.Objects do
      FActions.InternalAdd(vAction);

    for vRTFReport in vDefinition.RTFReports.Objects do
      FRTFReports.InternalAdd(vRTFReport);

    for vReport in vDefinition.Reports.Objects do
      FReports.InternalAdd(vReport);

    //for vCube in vDefinition.DataCubes do
    //  FDataCubes.InternalAdd(vCube);
  end;
end;

constructor TDefinition.Create(const AOwner: TObject; const AIsCollection: Boolean;
  const AName: string; const AAncestors: string = ''; const AKind: TCollectionKind = clkLibrary);
begin
  inherited Create(AOwner, AName);

  FFullName := AName;
  FCaption := '';
  FImageID := '';
  FGroupFieldName := '';
  FFlags := 0;
  FUIState := vsFullAccess;
  FOrderNum := 0;

  FIsCollection := AIsCollection;
  //FID := AID;
  FKind := AKind;
  FStorageName := BuildStorageName(FName);
  FStorageKind := skDatabase;

  FFields := TStringDictionary<TFieldDef>.Create;
  FServiceFields := TObjectStringList<TFieldDef>.Create;
  FStateFieldDef := nil;

  FOwnerLists := TOwnerListFields.Create([doOwnsValues]);

  FCheckUniqueQuery := nil;
  FRTFReports := TRTFReports.Create(Self);
  FReports := TReports.Create(Self);
  FActions := TActions.Create(Self);
  FReactions := TReactions.Create(Self);
  FFilters := TFilters.Create([doOwnsValues]);
  FReductions := TReductions.Create;
  FDataCubes := TDataCubes.Create(Self);

  FEmptyValue := ''; // localization
  FPrefix := '';     // localization
  FBackground.Color := cNullColor;

  AddServiceFieldDef('ID', 'id', 'Идентификатор');
  AddServiceFieldDef('LogID', 'log_id', 'Ссылка на последнее изменение');

  FDescendants := TList<TDefinition>.Create;
  FInnerDefinitions := TList<TDefinition>.Create;
  FAncestors := CreateDelimitedList(AAncestors);
  if FAncestors.Count > 0 then
    ApplyInheritance;

  if FIsCollection and (AKind <> clkMixin) and not FieldExists('Name') then
    AddSimpleFieldDef('Name', 'name', 'Наименование', '', Null, 150, fkString, '', '', vsFullAccess, cAutoCalculatedField);
  // AddSimpleFieldDef('Deleted', 'is_deleted', 'Удалено', False, Null, Null, fkBoolean, '', cHidden);
  // AddSimpleFieldDef('Flags', 'flags', 'Флаги сущности', 0, Null, Null, fkFlag, '', '', cHidden or cSystem);
end;

procedure TDefinition.CreateListRelations;
var
  j: Integer;
  vFieldDef: TFieldDef;
  vListFieldDef: TListFieldDef;
  vDefinition: TDefinition;
  vAction: TActionDef;
  vReport: TReportDef;
begin
  for vFieldDef in FFields do
  begin
    if vFieldDef.Kind <> fkList then
      Continue;

    vListFieldDef := TListFieldDef(vFieldDef);
    vListFieldDef.CreateRelations;
    if not Assigned(vListFieldDef._ContentDefinition) then
      Continue;

    for j := 0 to vListFieldDef.ContentDefinitions.Count - 1 do
    begin
      vDefinition := TDefinition(vListFieldDef.ContentDefinitions[j]);
      vDefinition.AddOwnerList(vListFieldDef);
    end;
  end;

  for vReport in FReports.Objects do
    vReport.CreateListRelations;
  for vAction in FActions.Objects do
    vAction.CreateListRelations;
end;

procedure TDefinition.CreateObjectRelations;
var
  vFieldDef: TFieldDef;
  vAction: TActionDef;
  vReport: TReportDef;
begin
  for vFieldDef in FFields do
    if vFieldDef.Kind = fkObject then
      TEntityFieldDef(vFieldDef).CreateRelations;

  for vReport in FReports.Objects do
    vReport.CreateObjectRelations;
  for vAction in FActions.Objects do
    vAction.CreateObjectRelations;
end;

destructor TDefinition.Destroy;
var
  vFieldDef: TFieldDef;
begin
  FreeAndNil(FCheckUniqueQuery);

  FreeAndNil(FOwnerLists);

  FreeAndNil(FReactions);
  FreeAndNil(FReports);
  FreeAndNil(FRTFReports);
  FreeAndNil(FActions);
  FreeAndNil(FReductions);
  FreeAndNil(FFilters);
  FreeAndNil(FDataCubes);

  for vFieldDef in FFields do
    InternalTryDeleteField(vFieldDef);
  FreeAndNil(FFields);
  FreeAndNil(FServiceFields);

  FreeAndNil(FInnerDefinitions);
  FreeAndNil(FAncestors);
  FreeAndNil(FDescendants);

  FConfiguration := nil;

  inherited Destroy;
end;

function TDefinition.ExtractFieldDef(const AFieldName: string): TFieldDef;
var
  vPos: Integer;
  vPath: string;
  vFieldName: string;
  vFieldDef: TFieldDef;
begin
  vPos := Pos('.', AFieldName);
  if vPos > 0 then
  begin
    vPath := AFieldName;
    vFieldName := Copy(vPath, 1, vPos - 1);
    vFieldDef := FieldByName(vFieldName);
    if Assigned(vFieldDef) and (vFieldDef.Kind in [fkObject, fkList]) then
    begin
      Delete(vPath, 1, vPos);
      Result := TDefinition(TObjectFieldDef(vFieldDef)._ContentDefinition).ExtractFieldDef(vPath);
    end
    else
      Result := nil;
  end
  else
    Result := FieldByName(AFieldName);
end;

function TDefinition.FieldByName(const AFieldName: string): TFieldDef;
begin
  Result := FFields.ObjectByName(AFieldName);
end;

function TDefinition.FieldExists(const AFieldName: string): Boolean;
begin
  Result := FFields.Exists(AFieldName);
end;

function TDefinition.FieldNameByStorageName(const AStorageName: string): string;
var
  vFieldDef: TFieldDef;
begin
  Result := '';
  for vFieldDef in FFields do
  begin
    if SameText(vFieldDef.StorageName, AStorageName) then
    begin
      Result := vFieldDef.Name;
      Exit;
    end;
  end;
end;

procedure TDefinition.FillContentDefinitions(const AContentDefinitions: TList<TDefinition> = nil);
var
  vContentDefinitions: TList<TDefinition>;
  vDescDefinition: TDefinition;
begin
  if Assigned(AContentDefinitions) then
    vContentDefinitions := AContentDefinitions
  else
    vContentDefinitions := FInnerDefinitions;

  if (vContentDefinitions.IndexOf(Self) < 0) and (FKind <> clkMixin) then
    vContentDefinitions.Add(Self);
  for vDescDefinition in FDescendants do
    vDescDefinition.FillContentDefinitions(vContentDefinitions);
end;

procedure TDefinition.GetAllActions(const AActions: TList<TActionDef>);
var
  vAction: TActionDef;
  vDescDefinition: TDefinition;
begin
  if FKind <> clkMixin then
  begin
    for vAction in FActions.Objects do
      if AActions.IndexOf(vAction) < 0 then
        AActions.Add(vAction);
  end;

  for vDescDefinition in FDescendants do
    vDescDefinition.GetAllActions(AActions);
end;

procedure TDefinition.GetAllReports(const AReports: TList<TRTFReport>);
var
  vReport: TRTFReport;
  vDescDefinition: TDefinition;
begin
  if FKind <> clkMixin then
  begin
    for vReport in FRTFReports.Objects do
      if AReports.IndexOf(vReport) < 0 then
        AReports.Add(vReport);
  end;

  for vDescDefinition in FDescendants do
    vDescDefinition.GetAllReports(AReports);
end;

function TDefinition.GetLinkedOwnerLists(const AFieldDef: TFieldDef): TList<TListFieldDef>;
begin
  FOwnerLists.TryGetValue(AFieldDef, Result);
end;

function TDefinition.HasFlag(const AFlag: Integer): Boolean;
begin
  Result := AFlag and FFlags <> 0;
end;

function TDefinition.IndexOfField(const AFieldName: string): Integer;
begin
  Result := FFields.IndexOfName(AFieldName);
end;

procedure TDefinition.InternalTryDeleteField(const AFieldDef: TFieldDef);
begin
  if AFieldDef.Definition = Self then
    AFieldDef.DisposeOf;
end;

function TDefinition.IsDescendantOf(const ADefinitionName: string): Boolean;
var
  vDefinition: TDefinition;
  i: Integer;
begin
  Result := SameText(FName, ADefinitionName);
  if Result then
    Exit;

  for i := 0 to FAncestors.Count - 1 do
  begin
    vDefinition := TConfiguration(FConfiguration)[FAncestors[i]];
    if Assigned(vDefinition) and vDefinition.IsDescendantOf(ADefinitionName) then
      Exit(True);
  end;

  Result := False;
end;

function TDefinition.IsParam: Boolean;
begin
  Result := False;
end;

procedure TDefinition.RegisterReaction(const AReactiveFields, AFieldChain: string; const AReactionProc: TProc);
begin
  TConfiguration(FConfiguration).RegisterReaction(FName, AReactiveFields, AFieldChain, AReactionProc);
end;

procedure TDefinition.RemoveField(const AFieldName: string);
//var
//  vIndex: Integer;
begin
{  vIndex := FFields.IndexOf(AFieldName);
  if vIndex >= 0 then
  begin
    if GetField(vIndex).Definition = Self then
      GetField(vIndex).Free;
    FFields.Delete(vIndex);
  end;}
end;

function TDefinition.ReportByName(const AName: string): TDefinition;
var
  vDescDefinition: TDefinition;
begin
  Result := FRTFReports.ObjectByName(AName);
  if not Assigned(Result) then
    Result := FReports.ObjectByName(AName);

  if not Assigned(Result) and (FDescendants.Count > 0) then
    for vDescDefinition in FDescendants do
    begin
      Result := vDescDefinition.ReportByName(AName);
      if Assigned(Result) then
        Exit;
    end;
end;

function TDefinition.SetBackground(const ABackground: TBackground): TDefinition;
begin
  FBackground := ABackground;
  Result := Self;
end;

function TDefinition.SetCaption(const ACaption: string): TDefinition;
begin
  FCaption := ACaption;
  Result := Self;
end;

function TDefinition.SetColorFieldName(const AColorFieldName: string): TDefinition;
begin
  FColorFieldName := AColorFieldName;
  Result := Self;
end;

function TDefinition.SetColorTarget(const AColorTarget: TColorTarget): TDefinition;
begin
  FColorTarget := AColorTarget;
  Result := Self;
end;

function TDefinition.SetEmptyValue(const AEmptyValue: string): TDefinition;
begin
  FEmptyValue := AEmptyValue;
  Result := Self;
end;

function TDefinition.SetFlags(const AFlags: Integer): TDefinition;
begin
  InternalSetFlags(AFlags);
  if FFlags and ccLocalOnly <> 0 then
    if not FServiceFields.Exists('GUID') then
      AddServiceFieldDef('GUID', 'guid', 'Уникальный идентификатор', fkString, 38);
  Result := Self;
end;

function TDefinition.SetGroupFieldName(const AGroupFieldName: string): TDefinition;
begin
  FGroupFieldName := AGroupFieldName;
  Result := Self;
end;

function TDefinition.SetID(const AID: Integer): TDefinition;
begin
  FID := AID;
  Result := Self;
end;

function TDefinition.SetImageID(const AImageID: string): TDefinition;
begin
  FImageID := AImageID;
  Result := Self;
end;

function TDefinition.SetLayoutMask(const AMask: string): TDefinition;
begin
  FLayoutMask := AMask;
  Result := Self;
end;

function TDefinition.SetOrderNum(const AOrderNum: Integer): TDefinition;
begin
  FOrderNum := AOrderNum;
  Result := Self;
end;

function TDefinition.SetPrefix(const APrefix: string): TDefinition;
begin
  FPrefix := APrefix;
  Result := Self;
end;

function TDefinition.SetStorageKind(const AStorageKind: TStorageKind): TDefinition;
begin
  FStorageKind := AStorageKind;
  Result := Self;
end;

function TDefinition.SetStorageName(const AStorageName: string): TDefinition;
begin
  FStorageName := AStorageName;
  Result := Self;
end;

procedure TDefinition.UpdateInheritance;
var
  vDefinition: TDefinition;
  i: Integer;
  vFieldDef: TFieldDef;
begin
  for i := 0 to FAncestors.Count - 1 do
  begin
    vDefinition := TConfiguration(FConfiguration)[FAncestors[i]];
    if Assigned(vDefinition) then
    begin
      for vFieldDef in vDefinition.Fields do
        if not FieldExists(vFieldDef.Name) then
          InternalAddField(vFieldDef);
    end;
  end;
end;

{ TFilter }

constructor TFilter.Create(const ADefinition: TDefinition; const AName, ACaption, AQuery: string);
begin
  inherited Create(ADefinition, AName);
  FFullName := ADefinition.Name + '.fil' + FName;
  FCaption := ACaption;
  FQuery := AQuery;
  FHint := '';
  FQueryDef := TQueryDef.Create(ADefinition.Name, AQuery, '');
end;

destructor TFilter.Destroy;
begin
  FreeAndNil(FQueryDef);
  inherited Destroy;
end;

{ TDefinitions }

function TDefinitions.Add(const AName, AAncestors: string; const AKind: TCollectionKind): TDefinition;
begin
  Assert(not Exists(AName), 'Collection definition [' + AName + '] already exists!');

  Result := TDefinition.Create(FConfiguration, True, AName, AAncestors, AKind);
  AddObject(AName, Result);
end;

constructor TDefinitions.Create(const AConfiguration: TObject);
begin
  inherited Create;
  FConfiguration := AConfiguration;
end;

procedure TDefinitions.DefinitionsByKind(const AList: TList<TDefinition>; const ADefinitionKind: TCollectionKind);
var
  vDefinition: TDefinition;
begin
  for vDefinition in Self do
    if vDefinition.Kind = ADefinitionKind then
      AList.Add(vDefinition);
end;

{ TActionDef }

constructor TActionDef.Create(const AOwner: TObject; const AName, ACaption, AImageID: string);
begin
  inherited Create(AOwner, False, AName);

  FFullName := AName;
  if Assigned(FDefinition) then
    FFullName := FDefinition.Name + '.' + FFullName;
  SetCaption(ACaption).SetImageID(AImageID);
end;

function TActionDef.IsParam: Boolean;
begin
  Result := True;
end;

{ TDefItem }

constructor TDefItem.Create(const AOwner: TObject; const AName: string);
begin
  inherited Create;

  FOwner := AOwner;
  if AOwner is TConfiguration then
  begin
    FDefinition := nil;
    FConfiguration := TConfiguration(AOwner);
  end
  else if AOwner is TDefinition then
  begin
    FDefinition := TDefinition(AOwner);
    FConfiguration := FDefinition.Configuration;
  end
  else begin
    FDefinition := nil;
    FConfiguration := nil;
  end;

  FName := AName;
end;

{ TDefList<T> }

procedure TDefList<T>.ClearObjects;
var
  vItem: TStringListItem;
begin
  for vItem in Self do
    if vItem.&Object.FOwner = FOwner then
      DisposeObject(vItem.&Object);
end;

constructor TDefList<T>.Create(const AOwner: TObject);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TDefList<T>.InternalAdd(const ADefItem: T);
var
  vName: string;
begin
  vName := ADefItem.Name;
  ExtractByName(vName);
  AddObject(vName, ADefItem);
end;

{ TActions }

function TActions.Add(const AName, ACaption, AImageID: string; const AFlags: Integer): TActionDef;
begin
  Result := TActionDef.Create(FOwner, AName, ACaption, AImageID);
  Result.SetFlags(AFlags or ccSystem);
  InternalAdd(Result);
end;

{ TReductions }

procedure TReductions.AddReduction(const AFieldName: string; const AReductionKind: TAggregationKind);
begin
  Add(AFieldName + '=' + AggregationKindToText(AReductionKind));
end;

function TReductions.ReductionForField(const AFieldName: string): TAggregationKind;
begin
  Result := AggregationKindFromText(Values[AFieldName]);
end;

{ TAggregation }

constructor TAggregationDef.Create(const AAggKind: TAggregationKind; const AName,
  AObservedFieldName: string; const AFieldKind: TFieldKind;
  const ACondition: string);
var
  vPos: Integer;
  vCondition: string;
begin
  inherited Create;

  FAggKind := AAggKind;
  FName := AName;
  FObservedFieldName := AObservedFieldName;

  if ACondition = '' then
    FConditionField1 := ''
  else begin
    FIsNot := ACondition.Chars[0] = '!';
    if FIsNot then
      vCondition := Copy(ACondition, 2, Length(ACondition) - 1)
    else
      vCondition := ACondition;
    vPos := vCondition.IndexOfAny(['=', '^', '$', '>', '<', '}', '{']) + 1;
    if vPos > 0 then
    begin
      FConditionField1 := Copy(vCondition, 1, vPos - 1);
      FConditionKind := StrToCondition(Copy(vCondition, vPos, 1));
      FConditionField2 := Copy(vCondition, vPos + 1, Length(vCondition) - vPos);
    end
    else begin
      FConditionField1 := vCondition;
      FConditionField2 := '';
    end;
  end;

  FIsValid := (AFieldKind in [fkCurrency]) or (AAggKind = akCount);
  Assert(FIsValid, 'Aggregation type does not supported for the field');

  FFieldKind := AFieldKind;
end;

{ TLayer }

procedure TLayer.AddAggregation(const AAggKind: TAggregationKind; const AName, AObservedFieldName: string;
  const AFieldKind: TFieldKind; const ACondition: string);
var
  vAggregation: TAggregationDef;
begin
  vAggregation := TAggregationDef.Create(AAggKind, AName, AObservedFieldName,
    AFieldKind, ACondition);
  FAggregationDefs.AddObject(AName, vAggregation);
end;

constructor TLayer.Create(const AHeaderName, AFooterName: string);
begin
  inherited Create;
  FHeaderName := AHeaderName;
  FFooterName := AFooterName;
  FAggregationDefs := TAggregationDefs.Create;
end;

destructor TLayer.Destroy;
begin
  FreeAndNil(FAggregationDefs);
  inherited Destroy;
end;

{ TGroupLayer }

constructor TGroupLayer.Create(const AHeaderName, ABreakingField: string;
  const AFooterName: string = '');
begin
  inherited Create(AHeaderName, AFooterName);
  FBreakingField := ABreakingField;
end;

{ TDataLayer }

function TDataLayer.AddUserData(const AHeaderName, ADataName,
  AFooterName: string; const ACount: Integer): TDataLayer;
begin
  Result := TDataLayer.Create(AHeaderName, ADataName, AFooterName, '', '', '', ACount);
  FData.Add(Result);
end;

function TDataLayer.AddData(const AHeaderName, ADataName, AFooterName,
  AFromClause, AWhereClause: string; const AOrderClause: string = ''): TDataLayer;
begin
  Result := TDataLayer.Create(AHeaderName, ADataName, AFooterName, AFromClause, AWhereClause, AOrderClause, 0);
  FData.Add(Result);
end;

function TDataLayer.AddGroup(const AHeaderName, ABreakingField,
  AFooterName: string): TGroupLayer;
begin
  Result := TGroupLayer.Create(AHeaderName, ABreakingField, AFooterName);
  FGroups.Add(Result);
  FBreakingFields.Add(ABreakingField);
end;

constructor TDataLayer.Create(const AHeaderName, ADataName, AFooterName,
  AFromClause, AWhereClause, AOrderClause: string; const ACount: Integer);
begin
  inherited Create(AHeaderName, AFooterName);

  FDataName := ADataName;
  FFromClause := AFromClause;
  FDataQuery := TQueryDef.Create(AFromClause, AWhereClause, AOrderClause);
  FGroups := TObjectList<TLayer>.Create;
  FData := TObjectList<TLayer>.Create;
  FBreakingFields := TStringList.Create;
  FBreakingFields.Delimiter := ';';
  FCount := ACount;
end;

destructor TDataLayer.Destroy;
begin
  FreeAndNil(FBreakingFields);
  FreeAndNil(FDataQuery);
  FreeAndNil(FGroups);
  FreeAndNil(FData);

  inherited Destroy;
end;

{ TSysReport }

function TReportDef.AddData(const AHeaderName, ADataName, AFooterName,
  AFromClause, AWhereClause: string; const AOrderClause: string = ''): TDataLayer;
begin
  Result := TDataLayer.Create(AHeaderName, ADataName, AFooterName, AFromClause, AWhereClause, AOrderClause, 0);
  FLayers.Add(Result);
end;

function TReportDef.AddUserData(const AHeaderName, ADataName,
  AFooterName: string; const ACount: Integer): TDataLayer;
begin
  Result := TDataLayer.Create(AHeaderName, ADataName, AFooterName, '', '', '', ACount);
  FLayers.Add(Result);
end;

constructor TReportDef.Create(const ADefinition: TDefinition;
  const AReportName, ACaption, AFileName, AOutputFileMask: string);
var
  vRealFileName: string;
begin
  inherited Create(ADefinition, False, AReportName);
  FFullName := ADefinition.Name + '.' + AReportName;
  FCaption := ACaption;
  FFileName := AFileName;
  FOutputFileMask := AOutputFileMask;
  vRealFileName := TConfiguration(FConfiguration).FindLeafFile('reports' + TPath.DirectorySeparatorChar + AFileName);
  if FileExists(vRealFileName) then
  begin
    FContent := TStringStream.Create('');
    FContent.LoadFromFile(vRealFileName);
  end
  else
    FContent := nil;

  FLayers := TObjectList<TLayer>.Create;
end;

destructor TReportDef.Destroy;
begin
  FreeAndNil(FLayers);
  FreeAndNil(FContent);

  FDefinition := nil;

  inherited Destroy;
end;

function TReportDef.IsParam: Boolean;
begin
  Result := True;
end;

{ TRTFReport }

constructor TRTFReport.Create(const ADefinition: TDefinition; const AName, ACaption, AFileName: string);
begin
  inherited Create(ADefinition, False, AName);
  FFullName := ADefinition.Name + '.' + AName;
  FCaption := ACaption;
  FFileName := TConfiguration(FConfiguration).FindLeafFile('reports' + TPath.DirectorySeparatorChar + AFileName);

  FContent := TStringStream.Create('');
  FContent.LoadFromFile(FFileName);
end;

destructor TRTFReport.Destroy;
begin
  FreeAndNil(FContent);
  FDefinition := nil;

  inherited Destroy;
end;

{function TRTFReport.GetCaption(const AInteractor: TInteractor): string;
begin
  if Assigned(AInteractor) then
    Result := AInteractor.Translate(FFullName + '@Caption')
  else
    Result := FCaption;
end;}

procedure TRTFReport.Save;
begin
  FContent.SaveToFile(FFileName);
end;

{ TFieldDef }

function TFieldDef.CalcStorageName: string;
begin
  Result := BuildStorageName(FName);
end;

constructor TFieldDef.Create(const AOwner: TDefinition; const AName: string; const AKind: TFieldKind = fkString;
  const AUIState: TViewState = vsFullAccess; const AFlags: Integer = 0; const ASize: Integer = -1);
begin
  inherited Create;
  FOwner := AOwner;
  FName := AName;
  FKind := AKind;
  FSize := ASize;
  FUIState := AUIState;
  FFlags := AFlags;
  FFormat := '';
  FUsedInFilters := False;
  FCommitKind := ckOnChange;//ckOnExit;

  if Assigned(FOwner) then
    FFullName := TDefinition(FOwner).FullName
  else
    FFullName := '';

  if FFullName = '' then
    FFullName := FName
  else
    FFullName := FFullName + '.' + FName;

  FStorageName := CalcStorageName;
  FStorageKind := skDatabase;
  FCaption := '';   // LocalizationManager.Captions[FullName]
  FHint := '';      // LocalizationManager.Hints[FullName]

  FStyleName := ''; // default value

  FDependentFields := TStringList.Create;
  FNotificationChains := TNotificationChains.Create;
end;

procedure TFieldDef.CopyNotificationChains(const ATarget: TFieldDef);
begin
  //for I := Low to High do

end;

constructor TFieldDef.Create(const AOwner: TDefinition; const AFieldKind: TFieldKind; const AFieldName,
  AStorageName, ACaption, AHint, AViewName: string; const AUIState: TViewState; const AFlags: Integer);
begin
  Create(AOwner, AFieldName, AFieldKind, AUIState, AFlags);
  Self.SetCaption(ACaption).SetHint(AHint).SetStyleName(AViewName).SetStorageName(AStorageName);
end;

destructor TFieldDef.Destroy;
begin
  FreeAndNil(FNotificationChains);
  FreeAndNil(FDependentFields);
  FOwner := nil;
  inherited Destroy;
end;

function TFieldDef.GetConfiguration: TObject;
begin
  Result := TDefinition(FOwner).Configuration;
end;

function TFieldDef.GetFieldSize: Integer;
begin
  Result := 0;
end;

function TFieldDef.HasFlag(const AFlag: Integer): Boolean;
begin
  Result := (FFlags and AFlag) > 0;
end;

function TFieldDef.SetCaption(const ACaption: string): TFieldDef;
begin
  FCaption := ACaption;
  Result := Self;
end;

function TFieldDef.SetCommitKind(const ACommitKind: TCommitKind): TFieldDef;
begin
  FCommitKind := ACommitKind;
  Result := Self;
end;

function TFieldDef.SetFlags(const AFlags: Cardinal): TFieldDef;
begin
  FFlags := AFlags;
  Result := Self;
end;

function TFieldDef.SetFormat(const AFormat: string): TFieldDef;
begin
  FFormat := AFormat;
  Result := Self;
end;

function TFieldDef.SetHint(const AHint: string): TFieldDef;
begin
  FHint := AHint;
  Result := Self;
end;

function TFieldDef.SetStorageKind(const AStorageKind: TStorageKind): TFieldDef;
begin
  FStorageKind := AStorageKind;
  Result := Self;
end;

function TFieldDef.SetStorageName(const AStorageName: string): TFieldDef;
begin
  FStorageName := AStorageName;
  Result := Self;
end;

function TFieldDef.SetStyleName(const AStyleName: string): TFieldDef;
begin
  FStyleName := AStyleName;
  Result := Self;
end;

function TFieldDef.SetUIState(const AUIState: TViewState): TFieldDef;
begin
  FUIState := AUIState;
  Result := Self;
end;

{ TStringFieldDef

constructor TStringField.Create(const AOwner: TObject; const AName: string);
begin
  inherited Create(AOwner, AName, fkString);
end;

function TStringField.GetFieldSize: Integer;
begin
  Result := SizeOf(Pointer);
end;

function TStringField.SetDefaultValue(const ADefaultValue: string): TStringField;
begin
  FDefaultValue := ADefaultValue;
  Result := Self;
end;

function TStringField.SetMaxSize(const AMaxSize: Integer): TStringField;
begin
  FMaxSize := AMaxSize;
  Result := Self;
end;

function TStringField.SetMinSize(const AMinSize: Integer): TStringField;
begin
  FMinSize := AMinSize;
  Result := Self;
end;  }

{ TSimpleFieldDef }

constructor TSimpleFieldDef.Create(const AOwner: TDefinition;
  const AFieldKind: TFieldKind; const AFieldName, AStorageName,
  ACaption, AHint, AViewName: string;
  const AMinValue, AMaxValue, ADefaultValue: Variant;
  const AUIState: TViewState; const AFlags: Integer; const ADictionary: string;
  const ASearchType: TSearchType);
var
  vPos: Integer;
begin
  inherited Create(AOwner, AFieldKind, AFieldName,
    AStorageName, ACaption, AHint, AViewName, AUIState, AFlags);

  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
  FDefaultValue := ADefaultValue;
  FDictionaryText := FDictionary;
  FInitialSearchType := ASearchType;
  FNullValue := Null;
  FPrecision := ptDefault;

  vPos := Pos('.', ADictionary);
  if vPos = 0 then
  begin
    FDictionary := ADictionary;
    FDictionaryField := '';
    FRankField := '';
    FSearchType := stSearchNone;
  end
  else begin
    FDictionaryField := ADictionary;
    FDictionary := Copy(ADictionary, 1, vPos - 1);
    Delete(FDictionaryField, 1, vPos);
    vPos := Pos('&', FDictionaryField);
    if vPos = 0 then
      FRankField := ''
    else begin
      FRankField := FDictionaryField;
      FDictionaryField := Copy(FRankField, 1, vPos - 1);
      Delete(FRankField, 1, vPos);
    end;

    if FDictionaryField = '' then
      FSearchType := stSearchNone
    else
      FSearchType := ASearchType;
  end;
end;

function TSimpleFieldDef.SetDefaultValue(const AValue: Variant): TSimpleFieldDef;
begin
  FDefaultValue := AValue;
  Result := Self;
end;

function TSimpleFieldDef.SetNullValue(const ANullValue: Variant): TSimpleFieldDef;
begin
  FNullValue := ANullValue;
  Result := Self;
end;

function TSimpleFieldDef.SetPrecision(const AType: TPrecisionType): TSimpleFieldDef;
begin
  Result := Self;
end;

function TSimpleFieldDef.SetValueRange(const AMinValue, AMaxValue: Variant): TSimpleFieldDef;
begin
  FMinValue := AMinValue;
  FMaxValue := AMaxValue;
  Result := Self;
end;

{ TBlobFieldDef }

constructor TBlobFieldDef.Create(const AOwner: TDefinition;
  const AFieldName, AStorageName, ACaption: string; const AFormat: TBlobFormat;
  const AHint, AViewName: string; const AUIState: TViewState; const AFlags: Integer);
begin
  inherited Create(AOwner, fkBlob, AFieldName, AStorageName, ACaption,
    AHint, AViewName, AUIState, AFlags);
  FBlobFormat := AFormat;
end;

{ TComplexFieldDef }

constructor TComplexFieldDef.Create(const AOwner: TDefinition; const AFieldName, AStorageName, ACaption,
  AObjectKindName, AHint, AViewName: string; const AUIState: TViewState; const AFlags: Integer);
begin
  inherited Create(AOwner, fkComplex, AFieldName, AStorageName, ACaption,
    AHint, AViewName, AUIState, AFlags);
  FObjectKindName := AObjectKindName;
end;

{ TObjectFieldDef }

procedure TObjectFieldDef.AddExclusiveCondition(const AQuery: TStrings);
begin
end;

constructor TObjectFieldDef.Create(const AOwner: TDefinition;
  const AFieldKind: TFieldKind; const AFieldName, AStorageName, ACaption, AHint, ANullString, AContentTypeName,
  AViewName: string; const AUIState: TViewState; const AFlags: Integer;
  const ASortType: TEntitySortType; const AHiddenFields, ADependencies: string;
  const ASearchType: TSearchType; const AFilter: string);
begin
  inherited Create(AOwner, AFieldKind, AFieldName,
    AStorageName, ACaption, AHint, AViewName, AUIState, AFlags);

  if Pos('~', AContentTypeName) = 1 then
  begin
    FContentDefinitionName := '~';
    FContentTypeLocator := Trim(Copy(AContentTypeName, 2, Length(AContentTypeName) - 1));
    if FContentTypeLocator <> '' then
      TCreateContentTypeReactionProc(TConfiguration(AOwner.Configuration).CreateContentTypeReactionProc)(AOwner, AFieldName, FContentTypeLocator);
  end
  else begin
    FContentDefinitionName := AContentTypeName;
    FContentTypeLocator := '';
  end;

  FDefaultTypeName := '';
  FDefaultTypeID := 0;
  FHiddenFieldsText := AHiddenFields;
  FDependenciesText := ADependencies;

  FNullString := ANullString;
  FSortType := ASortType;
  FSearchType := ASearchType;
  FFilter := AFilter;

  if AHiddenFields <> '' then
  begin
    FHiddenFields := TStringList.Create;
    FHiddenFields.Delimiter := ';';
    FHiddenFields.DelimitedText := AHiddenFields;
  end
  else
    FHiddenFields := nil;

  FSelectiveFields := TStringList.Create;
  FQueryString := '';
  FExclusiveQueryString := '';
  if (Trim(ADependencies) = '') and (Trim(AFilter) = '') then
    Exit;

  RecalculateQueries;
end;

procedure TObjectFieldDef.CreateRelations;
var
  vDefinitions: TList<TDefinition>;
begin
  if FContentDefinitionName = '' then
    Exit;

  FContentDefinition := TConfiguration(GetConfiguration)[FContentDefinitionName];
  Assert(Assigned(FContentDefinition), 'Wrong content type name [' + FContentDefinitionName + ']');

  vDefinitions := GetContentDefinitions;
  FIsSelector := (vDefinitions.Count > 1) or (FContentDefinition.Name = '~');

  if not Assigned(FQueryDef) then
    FQueryDef := TQueryDef.Create(DefinitionsToText(GetContentDefinitions), FQueryString);
  if not Assigned(FExclusiveQueryDef) then
    FExclusiveQueryDef := TQueryDef.Create(DefinitionsToText(GetContentDefinitions), FExclusiveQueryString);
end;

function TObjectFieldDef.DefinitionsToText(const ADefinitions: TList<TDefinition>): string;
var
  vDefinition: TDefinition;
begin
  Result := '';
  for vDefinition in ADefinitions do
  begin
    if Result = '' then
      Result := vDefinition.Name
    else
      Result := Result + ';' + vDefinition.Name;
  end;
end;

destructor TObjectFieldDef.Destroy;
begin
  FreeAndNil(FQueryDef);
  FreeAndNil(FExclusiveQueryDef);
  FreeAndNil(FHiddenFields);
  FreeAndNil(FSelectiveFields);

  inherited Destroy;
end;

function TObjectFieldDef.GetContentDefinitions: TList<TDefinition>;
begin
  Result := TDefinition(FContentDefinition).InnerDefinitions;
end;

function TObjectFieldDef.GetContentDefinitionsText: string;
begin
  Result := DefinitionsToText(GetContentDefinitions);
end;

function TObjectFieldDef.GetDefaultDefinitionName: string;
begin
  Result := TDefinition(TDefinition(FContentDefinition).InnerDefinitions[0]).Name;
end;

function TObjectFieldDef.GetDefaultTypeID: Integer;
var
  vDefinitions: TList<TDefinition>;
  vDefinition: TDefinition;
begin
  if FDefaultTypeID <= 0 then
  begin
    if FDefaultTypeName <> '' then
    begin
      vDefinition := TConfiguration(FOwner.Configuration)[FDefaultTypeName];
      if Assigned(vDefinition) then
        FDefaultTypeID := vDefinition.ID;
      Exit(FDefaultTypeID);
    end;

    if not Assigned(FContentDefinition) then
      Exit(-1);

    vDefinitions := GetContentDefinitions;
    if Assigned(vDefinitions) and (vDefinitions.Count > 0) then
      FDefaultTypeID := vDefinitions[0].ID
    else
      Exit(-1);
  end;
  Result := FDefaultTypeID;
end;

function TObjectFieldDef.IsFieldHidden(const AFieldName: string): Boolean;
begin
  if Assigned(FHiddenFields) then
    Result := FHiddenFields.IndexOf(AFieldName) >= 0
  else
    Result := False;
end;

procedure TObjectFieldDef.RecalculateQueries;
var
  vFieldName: string;
  vMasterName: string;
  vCutMasterName: string;
  vQuery: TStrings;
  vDependencies: TStrings;
  i: Integer;
  vIndex: Integer;
  vPos: Integer;
begin
  FQueryString := '';
  FExclusiveQueryString := '';

  if FSelectiveFields.Count > 0 then
  begin
    for i := 0 to FSelectiveFields.Count - 1 do
    begin
      vFieldName := FSelectiveFields.Names[i];
      vMasterName := FSelectiveFields.ValueFromIndex[i];
      vPos := Pos('.', vMasterName);
      if vPos > 0 then
        vCutMasterName := Copy(vMasterName, 1, vPos - 1)
      else
        vCutMasterName := vMasterName;

      vIndex := FOwner.FieldByName(vCutMasterName).DependentFields.IndexOf(FSelectiveFields[i]);
      if vIndex >= 0 then
        FOwner.FieldByName(vCutMasterName).DependentFields.Delete(vIndex);
    end;

    FSelectiveFields.Clear;
  end;

  vQuery := CreateDelimitedList('', ',');
  vDependencies := CreateDelimitedList(FDependenciesText);
  try
    for i := 0 to vDependencies.Count - 1 do
    begin
      vQuery.Add(vDependencies[i]);

      // Парсинг первой части зависимости
      vMasterName := Trim(vDependencies[i]);
      if Copy(vMasterName, 1, 1) = '!' then
        Delete(vMasterName, 1, 1);
      vPos := 0;
      while (vPos < Length(vMasterName)) and (vMasterName.Chars[vPos].IsLetterOrDigit or (vMasterName.Chars[vPos] = '.')) do
        vPos := vPos + 1;
      vFieldName := vMasterName.Substring(0, vPos);
      //Assert(Pos('.', vFieldName) = 0, 'Selection fields can''t be multilevel!');

      // Ищем вторую часть зависимости, она начинается с буквы
      vMasterName := vMasterName.Remove(0, vPos + 1);
      if vMasterName <> '' then
      begin
        vPos := 0;
        while (vPos < Length(vMasterName)) and not (vMasterName.Chars[vPos].IsLetterOrDigit or (vMasterName.Chars[vPos] = '"')) do
          vPos := vPos + 1;
        vMasterName := vMasterName.Remove(0, vPos);
        if vMasterName <> '' then
        begin
          if vMasterName.Chars[0] = '"' then
            vMasterName := ''
          else begin
            vPos := 0;
            while (vPos < Length(vMasterName)) and (vMasterName.Chars[vPos].IsLetterOrDigit or (vMasterName.Chars[vPos] = '.')) do
              vPos := vPos + 1;
            vMasterName := Trim(vMasterName.Substring(0, vPos));
            //Assert(Pos('.', vMasterName) = 0, 'Selection fields can''t be multilevel!');
          end;
        end;
      end;

      vPos := Pos('.', vMasterName);
      if vPos > 0 then
        vCutMasterName := Copy(vMasterName, 1, vPos - 1)
      else
        vCutMasterName := vMasterName;
      FOwner.FieldByName(vCutMasterName).DependentFields.AddObject(vFieldName + '=' + vMasterName, Self);

      FSelectiveFields.Add(vFieldName + '=' + vMasterName);
    end;

    if FFilter <> '' then
      vQuery.Add(FFilter);

    if vQuery.Count > 1 then
      FQueryString := '&(' + vQuery.DelimitedText + ')'
    else
      FQueryString := vQuery.DelimitedText;

    AddExclusiveCondition(vQuery);
    if vQuery.Count > 1 then
      FExclusiveQueryString := '&(' + vQuery.DelimitedText + ')'
    else
      FExclusiveQueryString := vQuery.DelimitedText;
  finally
    FreeAndNil(vDependencies);
    FreeAndNil(vQuery);
  end;
end;

procedure TObjectFieldDef.SetContentDefinition(const ADefinition: TDefinition);
var
  vDefinitions: TList<TDefinition>;
begin
  FContentDefinition := ADefinition;

  vDefinitions := GetContentDefinitions;
  FIsSelector := vDefinitions.Count > 1;

  if Assigned(FQueryDef) then
    FreeAndNil(FQueryDef);
  FQueryDef := TQueryDef.Create(DefinitionsToText(GetContentDefinitions), FQueryString);

  if Assigned(FExclusiveQueryDef) then
    FreeAndNil(FExclusiveQueryDef);
  FExclusiveQueryDef := TQueryDef.Create(DefinitionsToText(GetContentDefinitions), FExclusiveQueryString);
end;

procedure TObjectFieldDef.SetDefaultTypeName(const ADefaultTypeName: string);
begin
  FDefaultTypeName := ADefaultTypeName;
end;

function TObjectFieldDef.SetDependenciesAndFilter(const ADependencies, AFilter: string): TObjectFieldDef;
begin
  Result := Self;
  if (AFilter = FFilter) and (ADependencies = FDependenciesText) then
    Exit;

  FFilter := AFilter;
  FDependenciesText := ADependencies;
  RecalculateQueries;
end;

{ TEntityFieldDef }

constructor TEntityFieldDef.Create(const AOwner: TDefinition; const AFieldName,
  AStorageName, ACaption, AHint, ANullString, AContentTypeName: string;
  const AViewName: string = ''; const ADefaultEntityID: Integer = 0; const AUIState: TViewState = vsFullAccess;
  const AFlags: Integer = 0; const ASortType: TEntitySortType = estSortByName;
  const ADependencies: string = ''; const AHiddenFields: string = '';
  const ASearchType: TSearchType = stSearchFromBegin; const AFilter: string = '');
begin
  inherited Create(AOwner, fkObject, AFieldName, AStorageName + '_id', ACaption, AHint,
    ANullString, AContentTypeName, AViewName, AUIState,
    AFlags, ASortType, AHiddenFields, ADependencies, ASearchType, AFilter);

  FSelectorStorageName := AStorageName + '_typeid';
  FDefaultEntityID := ADefaultEntityID;
end;

{ TListFieldDef }

procedure TListFieldDef.AddExclusiveCondition(const AQuery: TStrings);
begin
  AQuery.Add('!' + FMasterFieldName + '=*');
end;

constructor TListFieldDef.Create(const AOwner: TDefinition; const AFieldName,
  AMasterFieldName, ACaption, AHint, ANullString,
  AContentTypeName: string; const AViewName: string = ''; const AUIState: TViewState = vsFullAccess;
  const AFlags: Integer = 0; const ASortType: TEntitySortType = estUserSort;
  const AColorFieldName: string = '';
  const ARelationPower: TRelationPower = rpWeak;
  const AHiddenFields: string = ''; const ADependencies: string = '';
  const ASearchType: TSearchType = stSearchFromBegin;
  const AFilter: string = '');
var
  vQueryString: string;
begin
  inherited Create(AOwner, fkList, AFieldName, '',
    ACaption, AHint, ANullString, AContentTypeName, AViewName,
    AUIState, AFlags, ASortType, AHiddenFields, ADependencies, ASearchType, AFilter);

  FMasterFieldName := AMasterFieldName;
  FColorFieldName := AColorFieldName;
  FRelationPower := ARelationPower;

  vQueryString := FMasterFieldName + '=*';
  if AFilter <> '' then
    vQueryString := '&(' + vQueryString + ',' + AFilter + ')';

  FContentQueryDef := TQueryDef.Create('', vQueryString);
end;

destructor TListFieldDef.Destroy;
begin
  FreeAndNil(FContentQueryDef);
  inherited Destroy;
end;

procedure TListFieldDef.RecalculateQueries;
var
  vQueryString: string;
begin
  inherited RecalculateQueries;

  FreeAndNil(FContentQueryDef);
  vQueryString := FMasterFieldName + '=*';
  if FFilter <> '' then
    vQueryString := '&(' + vQueryString + ',' + FFilter + ')';
  FContentQueryDef := TQueryDef.Create('', vQueryString);
end;

{ TComplexClassDef }

constructor TComplexClassDef.Create(const AOwner: TObject; const AName: string; const AComplexClass: TObject);
begin
  inherited Create(AOwner, AName);
  FComplexClass := AComplexClass;
end;

destructor TComplexClassDef.Destroy;
begin
  FComplexClass := nil;
  inherited Destroy;
end;

{ TComplexClasses }

function TComplexClasses.Add(const AName: string; const AComplexClass: TObject): TComplexClassDef;
begin
  Result := TComplexClassDef.Create(FOwner, AName, AComplexClass);
  InternalAdd(Result);
end;

{ TNotificationChains }

procedure TNotificationChains.AddCalculation(const AReactionProc: TReactionHandler);
begin
  if not Assigned(FCalculations) then
  begin
    FCalculations := THandlers.Create;
    FCalculations.Add(AReactionProc);
  end
  else if not FCalculations.Contains(AReactionProc) then
    FCalculations.Add(AReactionProc);
end;

procedure TNotificationChains.AddListener(const AChain: string; const ADefinitionName: string;
  const AReactionProc: TReactionHandler);
begin
  FHasListener := True;
  GetChain(AChain).AddReaction(ADefinitionName, AReactionProc);
end;

procedure TNotificationChains.AddTransit(const AChain: string; const ANextFieldDef: TFieldDef);
begin
  FHasTransit := True;
  GetChain(AChain).AddTransitField(ANextFieldDef);
end;

procedure TNotificationChains.AddUICalculation(const AReactionProc: TReactionHandler);
begin
  if not Assigned(FUICalculations) then
  begin
    FUICalculations := THandlers.Create;
    FUICalculations.Add(AReactionProc);
  end
  else if not FUICalculations.Contains(AReactionProc) then
    FUICalculations.Add(AReactionProc);
end;

constructor TNotificationChains.Create;
begin
  inherited Create;
  FCalculations := nil;
  FUICalculations := nil;
  FHasTransit := False;
  FHasListener := False;
end;

destructor TNotificationChains.Destroy;
begin
  FreeAndNil(FCalculations);
  FreeAndNil(FUICalculations);
  FreeAndNil(FChains);
  inherited Destroy;
end;

function TNotificationChains.GetChain(const AChain: string): TNotificationChain;
begin
  if not Assigned(FChains) then
    FChains := TObjectDictionary<string, TNotificationChain>.Create([doOwnsValues]);

  if not FChains.TryGetValue(AChain, Result) then
  begin
    Result := TNotificationChain.Create;
    FChains.Add(AChain, Result);
  end;
end;

function TNotificationChains.HasNotificationChain(const ACurChain: string): Boolean;
begin
  if Assigned(FChains) then
    Result := FChains.ContainsKey(ACurChain)
  else
    Result := False;
end;

function TNotificationChains.TryGetReactions(const ACurChain: string; const ADefinitionName: string;
  var AReactions: THandlers): Boolean;
var
  vChain: TNotificationChain;
begin
  if FHasListener and FChains.TryGetValue(ACurChain, vChain) then
  begin
    if Assigned(vChain.FReactions) then
      Result := vChain.FReactions.TryGetValue(ADefinitionName, AReactions)
    else
      Result := False;
  end
  else
    Result := False;
end;

function TNotificationChains.TryGetTransitFields(const ACurChain: string; var AFields: TList<TFieldDef>): Boolean;
var
  vChain: TNotificationChain;
begin
  Result := FHasTransit and FChains.TryGetValue(ACurChain, vChain);
  if Result then
    AFields := vChain.FTransitFields;
end;

{ TNotificationChain }

procedure TNotificationChain.AddReaction(const ADefinitionName: string; const AReactionProc: TReactionHandler);
var
  vHandlers: THandlers;
begin
  if not Assigned(FReactions) then
    FReactions := TObjectDictionary<string, THandlers>.Create([doOwnsValues]);

  if not FReactions.TryGetValue(ADefinitionName, vHandlers) then
  begin
    vHandlers := THandlers.Create;
    FReactions.Add(ADefinitionName, vHandlers);
  end;

  if Assigned(AReactionProc) and not vHandlers.Contains(AReactionProc) then
    vHandlers.Add(AReactionProc);
end;

procedure TNotificationChain.AddTransitField(const ATransitFieldDef: TFieldDef);
begin
  if not Assigned(FTransitFields) then
    FTransitFields := TList<TFieldDef>.Create;
  if not FTransitFields.Contains(ATransitFieldDef) then
    FTransitFields.Add(ATransitFieldDef);
end;

destructor TNotificationChain.Destroy;
begin
  FreeAndNil(FTransitFields);
  FreeAndNil(FReactions);
  inherited Destroy;
end;

{ TDataCubes }

function TDataCubes.AddCube(const AName, AFilter: string): TDataCube;
begin
  Result := TDataCube.Create(FDefinition, AName, AFilter);
  AddOrSetObject(AName, Result);
end;

constructor TDataCubes.Create(const ADefinition: TDefinition);
begin
  inherited Create;
  FDefinition := ADefinition;
end;

destructor TDataCubes.Destroy;
begin
  FDefinition := nil;
  inherited Destroy;
end;

function TDataCubes.GetCube(const AName: string): TDataCube;
begin
  if not TryGetObject(AName, Result) then
    Result := nil;
end;

{ TDataCube }

procedure TDataCube.AddField(const ACaption: string; const AFieldName: string; const AAreaKind: TCubeAreaKind;
  const AReductionKind: TAggregationKind; const APeriodReduction: string; const AVisible: Boolean);
var
  vCubeField: TCubeField;
begin
  if not FDefinition.FieldExists(AFieldName) then
  begin
    Assert(False, 'There is no Cube''s dimension field [' + AFieldName + ']');
    Exit
  end;

  vCubeField := TCubeField.Create(ACaption, FDefinition.FieldByName(AFieldName),
    AAreaKind, AReductionKind, APeriodReduction, AVisible);
  FFields.Add(vCubeField);
end;

constructor TDataCube.Create(const ADefinition: TDefinition; const AName, AFilter: string);
begin
  inherited Create;
  FDefinition := ADefinition;
  FName := AName;
  FFilter := AFilter;
  FFields := TObjectList<TCubeField>.Create;
end;

destructor TDataCube.Destroy;
begin
  FreeAndNil(FFields);
  FDefinition := nil;
  inherited Destroy;
end;

{ TReduction }

constructor TCubeField.Create(const ACaption: string; const AField: TFieldDef; const AAreaKind: TCubeAreaKind;
  const AReductionKind: TAggregationKind; const APeriodReduction: string = ''; const AVisible: Boolean = True);
begin
  inherited Create;
  FCaption := ACaption;
  FField := AField;
  FAreaKind := AAreaKind;
  FReductionKind := AReductionKind;
  FPeriodReduction := APeriodReduction;
  FVisible := AVisible;
end;

end.
