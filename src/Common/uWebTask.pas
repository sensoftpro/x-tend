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

unit uWebTask;

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections, uTask, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdCookieManager, IdHTTP, IdSMTP, IdMessageClient, IdMessage, IdAttachmentFile, IdText, IdExplicitTLSClientServerBase,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSLHeaders, IdSSLOpenSSL, uConsts;

type
  THTTPTask = class(TTaskHandle)
  private
    FMethod: THTTPHeaderMethod;
    FRequestUrl: string;
    FPostParams: TStream;
    FResponseText: string;
    FResponseCode: Integer;
    FResponseStream: TStream;
    FHTTPClient: TIdHTTP;
    FIOHandler: TIdSSLIOHandlerSocketOpenSSL;
    FCookieManager: TIdCookieManager;
    FTotalBytes: Int64;
    procedure OnStart(ASender: TObject; AWorkMode: TWorkMode; AWorkCountMax: Int64);
    procedure OnWork(ASender: TObject; AWorkMode: TWorkMode; AWorkCount: Int64);
    function GetRequest: TIdHTTPRequest;
    function GetResponse: TIdHTTPResponse;
    function GetContentType: string;
    procedure SetContentType(const Value: string);
  protected
    procedure DoExecute; override;
  public
    constructor Create(const AName: string; const ARequest: string; const AMethod: THTTPHeaderMethod = hhmGet);
    destructor Destroy; override;

    procedure AddHeader(const AName, AValue: string);
    procedure AddCookie(const AHost, ACookie: string);
    procedure AddClientCookie(const ACookie: string);

    procedure SetPostParams(const AText: string; const AEncoding: TEncoding); overload;
    procedure SetPostParams(const AStrings: TStrings); overload;
    procedure SetPostParams(const AContent: TStream); overload;
    //procedure SetPostParams(const AText: UTF8String); overload;

    procedure SetRequest(const ARequest: string; const AMethod: THTTPHeaderMethod = hhmGet);

    property ResponseText: string read FResponseText;
    property ResponseCode: Integer read FResponseCode;
    property ResponseStream: TStream read FResponseStream write FResponseStream;
    property Request: TIdHTTPRequest read GetRequest;
    property Response: TIdHTTPResponse read GetResponse;
    property ContentType: string read GetContentType write SetContentType;
  end;

  TEMailTask = class(TTaskHandle)
  private
    FSMTPClient: TidSMTP;
    FIOHandler: TIdSSLIOHandlerSocketOpenSSL;
    FMessage: TIdMessage;
    FTextPart: TIdText;
    function GetCharset: string;
    function GetContentType: string;
    procedure SetCharset(const Value: string);
    procedure SetContentType(const Value: string);
  protected
    procedure DoExecute; override;
  public
    constructor Create(const AName: string; const AHost: string; const APort: Integer;
      const ALogin, APassword, AServiceName, AReplyTo: string; const ATimeout: Integer);
    destructor Destroy; override;

    procedure InitTask(const ARecipients, ASubject, AMessage: string;
      const AAttachments: TStrings);

    function CheckConnection: string;

    property ContentType: string read GetContentType write SetContentType;
    property Charset: string read GetCharset write SetCharset;
  end;

function WWWEscape(const AText: string): string;

implementation

uses
  Types, Math, IdGlobal, IdUriUtils, IdURI, IdCookie, IdEMailAddress, Zlib;

function WWWEscape(const AText: string): string;
const
  cSafeChars: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789*-._'; {do not localize}
var
  i: Integer;
  vCharLen: Integer;
  vChar: Char;
begin
  Result := '';
  if AText = '' then
    Exit;

  i := 0;
  while i < Length(AText) do
  begin
    vChar := AText[i+1];

    if Ord(vChar) = 32 then
    begin
      Result := Result + '+';
      Inc(i);
    end
    else if Pos(vChar, cSafeChars) > 0 then
    begin
      Result := Result + vChar;
      Inc(i);
    end else
    begin
      vCharLen := CalcUTF16CharLength(AText, i+1);
      if vCharLen = 1 then
        Result := Result + '%' + IntToHex(Ord(vChar), 2)
      else
        Result := Result + '&#' + IntToStr(GetUTF16Codepoint(AText, i+1)) + ';';

      Inc(i, vCharLen);
    end;
  end;
end;

{ THTTPTask }

const
  cUserAgent = 'Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2224.3 Safari/537.36';

procedure THTTPTask.AddClientCookie(const ACookie: string);
begin
  FCookieManager.CookieCollection.AddClientCookie(ACookie);
end;

procedure THTTPTask.AddCookie(const AHost, ACookie: string);
begin
  FCookieManager.AddServerCookie(ACookie, TIdUri.Create(AHost));
end;

procedure THTTPTask.AddHeader(const AName, AValue: string);
begin
  FHTTPClient.Request.CustomHeaders.AddValue(AName, AValue);
end;

constructor THTTPTask.Create(const AName: string; const ARequest: string; const AMethod: THTTPHeaderMethod);
begin
  inherited Create(AName);

  FHTTPClient := TIdHTTP.Create(nil);
  FHTTPClient.HandleRedirects := True;
  FHTTPClient.Request.UserAgent := cUserAgent;
  FHTTPClient.ProtocolVersion := pv1_1;
  FHTTPClient.HTTPOptions := FHTTPClient.HTTPOptions + [hoKeepOrigProtocol];
  FIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  // Нужно пробовать, какой метод подходит к этой версии Инди
  // Варианты: sslvSSLv2, sslvSSLv23, sslvSSLv3, sslvTLSv1, sslvTLSv1_1,sslvTLSv1_2
  FIOHandler.SSLOptions.Method := sslvSSLv23;
  FCookieManager := TIdCookieManager.Create(nil);
  FHTTPClient.CookieManager := FCookieManager;
  FHTTPClient.AllowCookies := True;
  FHTTPClient.IOHandler := FIOHandler;
  FHTTPClient.OnWorkBegin := OnStart;
  FHTTPClient.OnWork := OnWork;

  FRequestUrl := ARequest;
  FMethod := AMethod;
  FResponseStream := nil;
end;

destructor THTTPTask.Destroy;
begin
  if Assigned(FPostParams) then
    FreeAndNil(FPostParams);

  if FHTTPClient.Socket.Opened then
    FHTTPClient.Socket.Close;

  FHTTPClient.Disconnect;

  FHTTPClient.OnWorkBegin := nil;
  FHTTPClient.OnWork := nil;
  FHTTPClient.IOHandler := nil;
  FHTTPClient.CookieManager := nil;

  FreeAndNil(FCookieManager);
  FreeAndNil(FIOHandler);
  FreeAndNil(FHTTPClient);

  inherited Destroy;
end;

procedure THTTPTask.DoExecute;
var
  vDecompressed: TDecompressionStream;
  vTextStream: TStringStream;
begin
  FResponseText := '';
  try
    case FMethod of
      hhmGet:
        // Работа со сжатым ответом
        if Assigned(FResponseStream) then
        begin
          FHTTPClient.Get(FRequestUrl, FResponseStream);
          FResponseStream.Position := 0;
          if AnsiLowerCase(FHTTPClient.Response.CharSet) = 'utf-8' then
            vTextStream := TStringStream.Create('', TEncoding.UTF8)
          else
            vTextStream := TStringStream.Create;

          try
            if FHTTPClient.Response.ContentEncoding <> '' then
            begin
              if AnsiLowerCase(FHTTPClient.Response.ContentEncoding) = 'gzip' then
                vDecompressed := TDecompressionStream.Create(FResponseStream, 15 + 16)
              else
                vDecompressed := TDecompressionStream.Create(FResponseStream);

              try
                vTextStream.LoadFromStream(vDecompressed);
              finally
                vDecompressed.Free;
              end;
            end
            else
              vTextStream.LoadFromStream(FResponseStream);

            FResponseText := vTextStream.DataString;
          finally
            vTextStream.Free;
          end;
        end
        else
          FResponseText := FHTTPClient.Get(FRequestUrl);
      hhmPost: FResponseText := FHTTPClient.Post(FRequestUrl, FPostParams);
      hhmPut: FResponseText := FHTTPClient.Put(FRequestUrl, FPostParams);
      hhmDelete: FHTTPClient.Delete(FRequestUrl);
      hhmPatch: ;
    else
      begin
        FResponseCode := 0;
        Exit;
      end;
    end;
  except
    on E: EIdHttpProtocolException do
    begin
      FResponseText := E.ErrorMessage; // FHTTPClient.ResponseText;
    end;
  end;

  FResponseCode := FHTTPClient.ResponseCode;
end;

function THTTPTask.GetContentType: string;
begin
  Result := FHTTPClient.Request.ContentType;
end;

function THTTPTask.GetRequest: TIdHTTPRequest;
begin
  Result := FHTTPClient.Request;
end;

function THTTPTask.GetResponse: TIdHTTPResponse;
begin
  Result := FHTTPClient.Response;
end;

procedure THTTPTask.OnStart(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCountMax: Int64);
begin
  FTotalBytes := AWorkCountMax;
  if FTotalBytes <= 0 then
    FTotalBytes := 200000;
end;

procedure THTTPTask.OnWork(ASender: TObject; AWorkMode: TWorkMode;
  AWorkCount: Int64);
begin
  if State = tskCanceled then
  begin
    FHTTPClient.OnWork := nil;
    if FHTTPClient.Socket.Opened then
      FHTTPClient.Socket.Close;
    //raise ETaskException.Create(crCancelled);
  end
  else begin
    ////NotifyProgress(AWorkCount, FTotalBytes);
    //if not Assigned(FOnProgress) then
    //  Exit;
    //Synchronize(procedure
    //  begin
    //    FOnProgress(Self);
    //  end);
  end;
end;

procedure THTTPTask.SetContentType(const Value: string);
begin
  FHTTPClient.Request.ContentType := Value;
end;

procedure THTTPTask.SetPostParams(const AText: string; const AEncoding: TEncoding);
var
  vStream: TStringStream;
begin
  if AText <> '' then
    vStream := TStringStream.Create(AText, AEncoding)
  else
    vStream := nil;
  SetPostParams(vStream);
end;

procedure THTTPTask.SetPostParams(const AStrings: TStrings);
var
  vParams: string;
  vParameter: string;
  i: Integer;
begin
  vParams := '';
  for i := 0 to AStrings.Count - 1 do
  begin
    vParameter := Trim(AStrings[i]);
    if vParameter <> '' then
    begin
      if vParams <> '' then
        vParams := vParams + '&';

      if Pos('=', vParameter) > 0 then
        vParams := vParams + Trim(AStrings.Names[i]) + '=' +
          WWWEscape(Trim(AStrings.ValueFromIndex[i]))
      else
        vParams := vParams + WWWEscape(vParameter);
    end;
  end;

  SetPostParams(vParams, TEncoding.UTF8);
  FHTTPClient.Request.ContentType := 'application/x-www-form-urlencoded';
end;

procedure THTTPTask.SetPostParams(const AContent: TStream);
begin
  FPostParams := AContent;
  TStream(FPostParams).Position := 0;
end;

procedure THTTPTask.SetRequest(const ARequest: string; const AMethod: THTTPHeaderMethod);
begin
  FRequestUrl := ARequest;
  FMethod := AMethod;
end;

{ TEMailTask }

function TEMailTask.CheckConnection: string;
begin
  Result := '';

  try
    FSMTPClient.Connect;
  except
    on E: Exception do
    begin
      Result := 'Can''t connect to SMTP-server. Reason: ' + E.Message;
      Exit;
    end;
  end;

  if FSMTPClient.Connected then
  begin
    try
      if not FSMTPClient.Authenticate then
        Result := 'Can''t login to SMTP-server. Account: [' + FSMTPClient.Username + ']';
    except
      on E: Exception do
        Result := 'Can''t login to SMTP-server. Account: [' + FSMTPClient.Username + ']. Reason: ' + E.Message;
    end;

    FSMTPClient.Disconnect(True);
  end
  else
    Result := 'Can''t connect to SMTP-server';
end;

constructor TEMailTask.Create(const AName: string; const AHost: string; const APort: Integer;
  const ALogin, APassword, AServiceName, AReplyTo: string; const ATimeout: Integer);
var
  vAddressItem: TIDEMailAddressItem;
begin
  inherited Create(AName);

  FSMTPClient := TIdSMTP.Create(nil);
  FSMTPClient.AuthType := satDefault;
  FIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create;
  FIOHandler.SSLOptions.Method := sslvSSLv23;
  FIOHandler.SSLOptions.Mode := sslmUnassigned;
  //FIOHandler.OnStatusInfo := Form7.OnStatusInfo;
  //FIOHandler.OnStatusInfoEx := Form7.OnStatusInfoEx;
  //FIOHandler.OnStatus := Form7.OnStatus;
  FSMTPClient.IOHandler := FIOHandler;
  FSMTPClient.UseTLS := utUseExplicitTLS;
  FSMTPClient.Host := AHost;
  FSMTPClient.Port := APort;
  FSMTPClient.ConnectTimeout := ATimeout;
  FSMTPClient.Username := ALogin;
  FSMTPClient.Password := APassword;

  FMessage := TIdMessage.Create(nil);
  if AReplyTo <> '' then
  begin
    vAddressItem := FMessage.ReplyTo.Add;
    vAddressItem.Address := AReplyTo;
  end;
  FMessage.From.Name := AServiceName;
  FMessage.From.Address := ALogin;

  FTextPart := TIdText.Create(FMessage.MessageParts, nil);
  FTextPart.ContentType := 'text/plain';
  FTextPart.CharSet := 'Windows-1251';
end;

destructor TEMailTask.Destroy;
begin
  if FSMTPClient.Socket.Opened then
    FSMTPClient.Socket.Close;
  FSMTPClient.Disconnect;

  FSMTPClient.IOHandler := nil;

  FreeAndNil(FMessage);
  FreeAndNil(FIOHandler);
  FreeAndNil(FSMTPClient);

  inherited Destroy;
end;

procedure TEMailTask.DoExecute;
begin
  try
    FSMTPClient.Connect;
    if FSMTPClient.Connected then
    begin
      try
        FSMTPClient.Send(FMessage);
      except
      end;

      FSMTPClient.Disconnect(True);
    end;
  finally

  end;
end;

function TEMailTask.GetCharset: string;
begin
  Result := FTextPart.CharSet;
end;

function TEMailTask.GetContentType: string;
begin
  Result := FTextPart.ContentType;
end;

procedure TEMailTask.InitTask(const ARecipients, ASubject, AMessage: string; const AAttachments: TStrings);
var
  i: Integer;
  vFileName: string;
begin
  FMessage.Recipients.EMailAddresses := ARecipients;
  FMessage.Subject := ASubject;
  FTextPart.Body.Text := AMessage;

  if Assigned(AAttachments) then
    for i := 0 to AAttachments.Count - 1 do
    begin
      vFileName := AAttachments[i];
      if (vFileName <> '') and FileExists(vFileName) then
        TIdAttachmentFile.Create(FMessage.MessageParts, vFileName);
    end;
end;

procedure TEMailTask.SetCharset(const Value: string);
begin
  FTextPart.CharSet := Value;
end;

procedure TEMailTask.SetContentType(const Value: string);
begin
  FTextPart.ContentType := Value;
end;

end.
