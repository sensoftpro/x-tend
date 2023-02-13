unit uTelegramBot;

interface

uses
  System.Classes, System.SysUtils, IdHTTP, IdSSLOpenSSL, IdComponent,
  uJSON, uTask, uScript, uModule, uEntity, uView, uChangeManager, uConsts;

type
  TOnTelegramMessage = reference to function(const ATask: TTaskHandle;
    const AIsExisting: Boolean; const AChatId, AText: string): string;

  TTelegramBot = class(TDomainModule)
  private
    FIsValid: Boolean;
    FBotToken: string;
    FUpdatesTask: TTaskHandle;
    FUpdateRequest: TIdHTTP;
    FSendRequest: TIdHTTP;
    FLastUpdate: Integer;
    FRegisteredRequests: TStrings;

    procedure InternalSendMessage(const AChatId: string; const AText: string);
    function GetUpdate: TJSONObject;
  protected
    function Post(const AMethodName: string; const AParams: TStrings): string;
  public
    constructor Create(const ADomain: TObject; const AName: string); override;
    destructor Destroy; override;

    procedure SendMessage(const AChatId: string; const AMessage: string);
    procedure SendMessageToRecipients(const AMessage: string; const ATargets: TStrings);
    procedure StartPolling(const AOnMessage: TOnTelegramMessage);

    property IsValid: Boolean read FIsValid;
  end;

  TTelegramInclusion = class(TInclusion)
  protected
    procedure DoInit; override;

    procedure DoCreateDefinitions; override;
    procedure DoCreateDefaultEntities(const ADomain: TObject; const AHolder: TChangeHolder); override;
  end;

implementation

uses
  Variants, uDefinition, uDomain, uCollection, uSession;

const
  cTelegramBotUrl = 'https://api.telegram.org/bot';

{ TTelegramBot }

constructor TTelegramBot.Create(const ADomain: TObject; const AName: string);
var
  vHandler: TIdSSLIOHandlerSocketOpenSSL;
  vEntity: TEntity;
begin
  inherited Create(ADomain, AName);

  FBotToken := Trim(TDomain(ADomain).Constant['TelegramBotToken']);
  FIsValid := FBotToken <> '';

  FRegisteredRequests := TStringList.Create;
  for vEntity in TDomain(FDomain)['TelegramRequests'] do
    FRegisteredRequests.Add(vEntity['UserId']);

  FUpdateRequest := TIdHTTP.Create;
  vHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FUpdateRequest);
  vHandler.SSLOptions.Mode := sslmClient;
  vHandler.SSLOptions.Method := sslvSSLv23;
  FUpdateRequest.IOHandler := vHandler;
  FUpdateRequest.HandleRedirects := True;

  FSendRequest := TIdHTTP.Create;
  vHandler := TIdSSLIOHandlerSocketOpenSSL.Create(FSendRequest);
  vHandler.SSLOptions.Mode := sslmClient;
  vHandler.SSLOptions.Method := sslvSSLv23;
  FSendRequest.IOHandler := vHandler;
  FSendRequest.HandleRedirects := True;
end;

destructor TTelegramBot.Destroy;
begin
  if Assigned(FUpdatesTask) then
    FreeAndNil(FUpdatesTask);

  FreeAndNil(FRegisteredRequests);
  FreeAndNil(FUpdateRequest);
  FreeAndNil(FSendRequest);

  inherited Destroy;
end;

procedure TTelegramBot.InternalSendMessage(const AChatId: String; const AText: String);
var
  vParams: TStringList;
  vTargetUrl: string;
begin
  vParams := TStringList.Create;
  vParams.Add('chat_id='+AChatId);
  vParams.Add('text='+AText);
  try
    vTargetUrl := cTelegramBotUrl + FBotToken + '/' + 'sendMessage';
//    if FSendRequest.Connected then
      FSendRequest.Post(vTargetUrl, vParams);
  finally
    FreeAndNil(vParams);
  end;
end;

function TTelegramBot.Post(const AMethodName: string; const AParams: TStrings): string;
var
  vTargetUrl: string;
begin
  vTargetUrl := cTelegramBotUrl + FBotToken + '/' + AMethodName;
  try
    Result := FUpdateRequest.Post(vTargetUrl, AParams)
  except
    Result := '';
  end
end;

procedure TTelegramBot.SendMessage(const AChatId, AMessage: string);
begin
  if not FIsValid then
    Exit;

  TDomain(FDomain).ExecuteManagedTask('TelegramSendOneMessage', procedure(const ATask: TTaskHandle)
    begin
      InternalSendMessage(AChatId, AMessage);
    end);
end;

procedure TTelegramBot.SendMessageToRecipients(const AMessage: string;
  const ATargets: TStrings);
begin
  if not FIsValid then
    Exit;

  TDomain(FDomain).ExecuteManagedTask('TelegramSendMessage', procedure(const ATask: TTaskHandle)
    var
      vId: string;
    begin
      for vId in ATargets do
        InternalSendMessage(vId, AMessage);
    end);
end;

procedure TTelegramBot.StartPolling(const AOnMessage: TOnTelegramMessage);
begin
  if not FIsValid then
    Exit;

  FUpdatesTask := TDomain(FDomain).ExecuteTask('TelegramGetUpdate', procedure(const ATask: TTaskHandle)
    var
      vUpdate: TJSONObject;
      vMessage: TJSONObject;
      vUser: TJSONObject;
      vId: string;
      vUserName: string;
      vRequests: TCollection;
      vSession: TUserSession;
      vAnswer: string;
      vExisting: Boolean;
    begin
      while (ATask.State = tskRunning) and Assigned(FUpdateRequest) do
      begin
        vUpdate := GetUpdate;
        if not Assigned(vUpdate) then
          Continue;

        try
          vMessage := vUpdate.ExtractObject('message');
          if not Assigned(vMessage) then
            Continue;

          vUser := vMessage.ExtractObject('from');
          vId := IntToStr(vUser.ExtractInteger('id'));
          vUserName := vUser.ExtractString('username');

          vExisting := FRegisteredRequests.IndexOf(vId) >= 0;
          if not vExisting then
          begin
            vRequests := TDomain(FDomain)['TelegramRequests'];
            vSession := TDomain(FDomain).DomainSession;
            vExisting := Assigned(vRequests.FindOne(vSession, 'UserId=:userid', [vId]));
            if not vExisting then
              vSession.AtomicModification(ATask, function(const AHolder:TChangeHolder): Boolean
                begin
                  vRequests._CreateNewEntity(AHolder, -1, 'UserName;UserId', [vUserName, vId]);
                  Result := True;
                end);
            FRegisteredRequests.Add(vId);
          end;

          if Assigned(AOnMessage) then
          begin
            vAnswer := AOnMessage(ATask, vExisting, vId, vMessage.ExtractString('text'));
            if vAnswer <> '' then
              InternalSendMessage(vId, vAnswer);
          end;
        finally
          FreeAndNil(vUpdate);
        end;
      end
    end);
end;

function TTelegramBot.GetUpdate: TJSONObject;
var
  vParams: TStringList;
  vAnswer: string;
  jAnswer: TJSONObject;
  vUpdates: TJSONArray;
  vUpdate: TJSONObject;
begin
  Result := nil;
  vParams := TStringList.Create;
  vParams.Add('offset=' + IntToStr(FLastUpdate));
  vParams.Add('limit=1');
  vParams.Add('timeout=5');
  try
    vAnswer := Trim(Post('getUpdates', vParams));
    if vAnswer = '' then
      Exit;
  finally
    FreeAndNil(vParams);
  end;

  jAnswer := TJSONObject.LoadFromText(vAnswer);
  if not Assigned(jAnswer) then
    Exit;
  try
    vUpdates := jAnswer.ExtractArray('result');
    if not Assigned(vUpdates) or (vUpdates.Size = 0) then
      Exit;
    vUpdate := TJSONObject(vUpdates.Get(0));
    if Assigned(vUpdate) then
    begin
      Result := TJSONObject(vUpdate.Clone);
      FLastUpdate := Result.ExtractInteger('update_id') + 1;
    end;
  finally
    FreeAndNil(jAnswer);
  end;
end;

{ TTelegramInclusion }

procedure TTelegramInclusion.DoCreateDefaultEntities(const ADomain: TObject;
  const AHolder: TChangeHolder);
begin
  inherited DoCreateDefaultEntities(ADomain, AHolder);
  // Заполнить параметры по умолчанию
end;

procedure TTelegramInclusion.DoCreateDefinitions;
var
  vDefinition: TDefinition;
begin
  inherited DoCreateDefinitions;

  vDefinition := DefinitionByName('SysConstants');
  vDefinition.AddSimpleFieldDef('TelegramBotToken', 'telegram_bot_token', 'Токен телеграм-бота', Null, Null, 200);
  vDefinition.AddSimpleFieldDef('TelegramBotName', 'telegram_bot_name', 'Имя телеграм-бота', Null, Null, 200);

  vDefinition := AddDefinition('TelegramRequests', '', 'Запросы на регистрацию', cNullItemName);
  vDefinition.AddSimpleFieldDef('UserName', 'user_name', 'Имя пользователя', Null, Null, 50, fkString, '', '', vsReadOnly);
  vDefinition.AddSimpleFieldDef('UserId', 'user_id', 'ID пользователя', Null, Null, 20, fkString, '', '', vsReadOnly);
  vDefinition.AddUniqueIndex('UserId');
end;

procedure TTelegramInclusion.DoInit;
begin
  FAppTitle := 'Чат бот для Telegram';
  FVersion := '0.1';
end;

initialization

TBaseModule.RegisterModule('TelegramBot', '', 'Base', TTelegramBot);
RegisterInclusion('TelegramBot', TTelegramInclusion);

end.
