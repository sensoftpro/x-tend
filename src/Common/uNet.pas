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

unit uNet;

interface

uses
  Classes, SysUtils, Generics.Collections,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient,
  IdCookieManager, IdHTTP, IdSMTP, IdMessageClient, IdMessage, IdAttachmentFile, IdText, IdExplicitTLSClientServerBase,
  IdIOHandler, IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSLHeaders, IdSSLOpenSSL,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  uModule, uConsts;

type
  TNetEngine = class;

  TNetPointType = (nptClient, nptServer, nptQueue);
  TNetPointProtocol = (nppTCPClient, nppTCPServer, nppUDPClient, nppUDPServer, nppHTTPClient, nppHTTPServer, nppSMTPClient, nppMQTT);
  TTransportEventType = (tetUnknown, tetConnected, tetDisconnected, tetSubscribed, tetUnsubscribed, tetPublished);
  TSecurityProtocolVersion = (spvSSLv2, spvSSLv23, spvSSLv3, spvTLSv1, spvTLSv11, spvTLSv12, spvTLSv13);

  TMQTTEvent = procedure(const AEventType: TTransportEventType; const ATopic, AMessage: string) of object;

  TNetPoint = class
  private
    FNetEngine: TNetEngine;
  public
    constructor Create(const AEngine: TNetEngine);
    destructor Destroy; override;
  end;

  TNetEngine = class(TDomainModule)
  private
    FNetPoints: TList<TNetPoint>;
  protected
    function DoCreateNetPoint(const ANetPointProtocol: TNetPointProtocol): TNetPoint; virtual; abstract;
  public
    constructor Create(const ADomain: TObject; const AName: string); override;
    destructor Destroy; override;

    function CreateNetPoint(const ANetPointProtocol: TNetPointProtocol): TNetPoint;
  end;

  THTTPClient = class(TNetPoint)
  private
    function ContentAsString(const AStream: TStream; const AEncoding: TEncoding = nil): string;
  protected
    procedure DoSetSecurityProtocolVersion(const AVersion: TSecurityProtocolVersion); virtual;
    procedure DoSetAccept(const AValue, ACharSet, AEncoding, ALanguage: string); virtual;
    procedure DoSetContentSettings(const AType, AEncoding, ALanguage: string); virtual;
    procedure DoAddHeader(const AName, AValue: string); virtual;
    procedure DoAddCookie(const AHost, ACookie: string); virtual;
    procedure DoAddClientCookie(const ACookie: string); virtual;
    procedure DoRequest(const AMethod: THTTPHeaderMethod; const AUrl: string;
      const AParams, AResponseContent: TStream); virtual;
    function GetResponseCharset: string; virtual;
    function GetResponseEncoding: string; virtual;
    function GetResponseCode: Integer; virtual;
    function GetResponseText: string; virtual;
  public
    function SetSecurityProtocolVersion(const AVersion: TSecurityProtocolVersion): THTTPClient;
    function SetAccept(const AValue, ACharSet, AEncoding, ALanguage: string): THTTPClient;
    function SetContentSettings(const AType, AEncoding, ALanguage: string): THTTPClient;

    function AddHeader(const AName, AValue: string): THTTPClient;
    function AddCookie(const AHost, ACookie: string): THTTPClient;
    function AddClientCookie(const ACookie: string): THTTPClient;

    function Request(const AMethod: THTTPHeaderMethod; const AUrl: string; const AParams: TStream = nil): string;

    property ResponseCode: Integer read GetResponseCode;
    property ResponseText: string read GetResponseText;
  end;

  TSMTPClient = class(TNetPoint)
  protected
    procedure DoSetCharset(const ACharset: string); virtual;
    procedure DoSetContentType(const AContentType: string); virtual;

    procedure DoInit(const AHost: string; const APort: Integer; const ALogin, APassword,
      AServiceName, AReplyTo: string; const ATimeout: Integer); virtual;
    procedure DoSend(const ARecipients, ASubject, AMessage: string; const AAttachments: TStrings); virtual;
    function DoCheckConnection: string; virtual;
  public
    function SetCharset(const ACharset: string): TSMTPClient;
    function SetContentType(const AContentType: string): TSMTPClient;

    procedure Init(const AHost: string; const APort: Integer; const ALogin, APassword,
      AServiceName, AReplyTo: string; const ATimeout: Integer);
    procedure Send(const ARecipients, ASubject, AMessage: string; const AAttachments: TStrings);
    function CheckConnection: string;
  end;

  TIndyHTTPClient = class(THTTPClient)
  private
    FHTTPClient: TIdHTTP;
    FIOHandler: TIdSSLIOHandlerSocketOpenSSL;
    FCookieManager: TIdCookieManager;
  protected
    procedure DoSetSecurityProtocolVersion(const AVersion: TSecurityProtocolVersion); override;
    procedure DoSetAccept(const AValue, ACharSet, AEncoding, ALanguage: string); override;
    procedure DoSetContentSettings(const AType, AEncoding, ALanguage: string); override;
    procedure DoAddHeader(const AName, AValue: string); override;
    procedure DoAddCookie(const AHost, ACookie: string); override;
    procedure DoAddClientCookie(const ACookie: string); override;
    procedure DoRequest(const AMethod: THTTPHeaderMethod; const AUrl: string;
      const AParams, AResponseContent: TStream); override;
    function GetResponseCharset: string; override;
    function GetResponseEncoding: string; override;
    function GetResponseCode: Integer; override;
    function GetResponseText: string; override;
  public
    constructor Create(const AEngine: TNetEngine);
    destructor Destroy; override;
  end;

  TIndySMTPClient = class(TSMTPClient)
  private
    FSMTPClient: TidSMTP;
    FIOHandler: TIdSSLIOHandlerSocketOpenSSL;
    FMessage: TIdMessage;
    FTextPart: TIdText;
  protected
    procedure DoSetCharset(const ACharset: string); override;
    procedure DoSetContentType(const AContentType: string); override;

    procedure DoInit(const AHost: string; const APort: Integer; const ALogin, APassword,
      AServiceName, AReplyTo: string; const ATimeout: Integer); override;
    procedure DoSend(const ARecipients, ASubject, AMessage: string; const AAttachments: TStrings); override;
    function DoCheckConnection: string; override;
  public
    constructor Create(const AEngine: TNetEngine);
    destructor Destroy; override;
  end;

  TSystemHTTPClient = class(THTTPClient)
  private
    FHTTPClient: TNetHTTPClient;
    FResponse: IHTTPResponse;
  protected
    procedure DoSetSecurityProtocolVersion(const AVersion: TSecurityProtocolVersion); override;
    procedure DoSetAccept(const AValue, ACharSet, AEncoding, ALanguage: string); override;
    procedure DoSetContentSettings(const AType, AEncoding, ALanguage: string); override;
    procedure DoAddHeader(const AName, AValue: string); override;
    procedure DoAddCookie(const AHost, ACookie: string); override;
    procedure DoAddClientCookie(const ACookie: string); override;
    procedure DoRequest(const AMethod: THTTPHeaderMethod; const AUrl: string;
      const AParams, AResponseContent: TStream); override;
    function GetResponseCharset: string; override;
    function GetResponseEncoding: string; override;
    function GetResponseCode: Integer; override;
    function GetResponseText: string; override;
  public
    constructor Create(const AEngine: TNetEngine);
    destructor Destroy; override;
  end;

  TIndyNetEngine = class(TNetEngine)
  protected
    function DoCreateNetPoint(const ANetPointProtocol: TNetPointProtocol): TNetPoint; override;
  end;

  TSystemNetEngine = class(TNetEngine)
  protected
    function DoCreateNetPoint(const ANetPointProtocol: TNetPointProtocol): TNetPoint; override;
  end;

function TextToPostParams(const AText: string; const AEncoding: TEncoding): TStream;
function StringsToPostParams(const AStrings: TStrings): TStream;


implementation

uses
  IdGlobal, IdGlobalProtocols, IdUriUtils, IdURI, IdCookie, IdEMailAddress, Zlib;

const
  cUserAgent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/108.0.0.0 YaBrowser/23.1.2.987 Yowser/2.5 Safari/537.36';

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

function TextToPostParams(const AText: string; const AEncoding: TEncoding): TStream;
begin
  if AText <> '' then
    Result := TStringStream.Create(AText, AEncoding)
  else
    Result := nil;
end;

function StringsToPostParams(const AStrings: TStrings): TStream;
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

  Result := TextToPostParams(vParams, TEncoding.UTF8);
  //FHTTPClient.Request.ContentType := 'application/x-www-form-urlencoded';
end;

{ TNetEngine }

constructor TNetEngine.Create(const ADomain: TObject; const AName: string);
begin
  inherited Create(ADomain, AName);
  FNetPoints := TList<TNetPoint>.Create;
end;

function TNetEngine.CreateNetPoint(const ANetPointProtocol: TNetPointProtocol): TNetPoint;
begin
  Result := DoCreateNetPoint(ANetPointProtocol);
  if Assigned(Result) then
    FNetPoints.Add(Result);
end;

destructor TNetEngine.Destroy;
begin
  FreeAndNil(FNetPoints);
  inherited Destroy;
end;

{ TIndyNetEngine }

function TIndyNetEngine.DoCreateNetPoint(const ANetPointProtocol: TNetPointProtocol): TNetPoint;
begin
  case ANetPointProtocol of
    nppHTTPClient: Result := TIndyHTTPClient.Create(Self);
    nppSMTPClient: Result := TIndyHTTPClient.Create(Self);
  else
    Result := nil;
  end;
end;

{ THTTPClient }

function THTTPClient.AddClientCookie(const ACookie: string): THTTPClient;
begin
  DoAddClientCookie(ACookie);
  Result := Self;
end;

function THTTPClient.AddCookie(const AHost, ACookie: string): THTTPClient;
begin
  DoAddCookie(AHost, ACookie);
  Result := Self;
end;

function THTTPClient.AddHeader(const AName, AValue: string): THTTPClient;
begin
  DoAddHeader(AName, AValue);
  Result := Self;
end;

function THTTPClient.ContentAsString(const AStream: TStream; const AEncoding: TEncoding): string;
var
  vReader: TStringStream;
  vInputStream: TStream;
  vResponseCharset: string;
  vResponseEncoding: string;
begin
  Result := '';
  if AEncoding = nil then
  begin
    vResponseCharset := GetResponseCharset;
    if (vResponseCharset <> '') and not SameText(vResponseCharset, 'utf-8') then // do not translate
      vReader := TStringStream.Create('', TEncoding.GetEncoding(vResponseCharset), True)
    else
      vReader := TStringStream.Create('', TEncoding.UTF8, False);
  end
  else
    vReader := TStringStream.Create('', AEncoding, False);

  try
    vResponseEncoding := GetResponseEncoding;
    if SameText(vResponseEncoding, 'deflate') then // do not translate
      // 15 is the default mode.
      vInputStream := TDecompressionStream.Create(AStream, 15)
    else if SameText(vResponseEncoding, 'gzip') then
      // +16 to enable gzip mode.  http://www.zlib.net/manual.html#Advanced
      vInputStream := TDecompressionStream.Create(AStream, 15 + 16)
    else
      vInputStream := AStream;

    try
      vReader.CopyFrom(vInputStream, 0);
      Result := vReader.DataString;
    finally
      if vInputStream <> AStream then
        FreeAndNil(vInputStream);
    end;
  finally
    FreeAndNil(vReader);
  end;
end;

procedure THTTPClient.DoAddClientCookie(const ACookie: string);
begin
end;

procedure THTTPClient.DoAddCookie(const AHost, ACookie: string);
begin
end;

procedure THTTPClient.DoAddHeader(const AName, AValue: string);
begin
end;

procedure THTTPClient.DoRequest(const AMethod: THTTPHeaderMethod; const AUrl: string;
  const AParams, AResponseContent: TStream);
begin
end;

procedure THTTPClient.DoSetAccept(const AValue, ACharSet, AEncoding, ALanguage: string);
begin
end;

procedure THTTPClient.DoSetContentSettings(const AType, AEncoding, ALanguage: string);
begin
end;

procedure THTTPClient.DoSetSecurityProtocolVersion(const AVersion: TSecurityProtocolVersion);
begin
end;

function THTTPClient.GetResponseCharset: string;
begin
  Result := '';
end;

function THTTPClient.GetResponseCode: Integer;
begin
  Result := -1;
end;

function THTTPClient.GetResponseEncoding: string;
begin
  Result := '';
end;

function THTTPClient.GetResponseText: string;
begin
  Result := '';
end;

function THTTPClient.Request(const AMethod: THTTPHeaderMethod; const AUrl: string; const AParams: TStream = nil): string;
var
  vResponseContent: TStream;
begin
  if Assigned(AParams) then
    AParams.Position := 0;
  //application/x-www-form-urlencoded?

  vResponseContent := TMemoryStream.Create;
  try
    try
      DoRequest(AMethod, AUrl, AParams, vResponseContent);
      if vResponseContent.Size > 0 then
        Result := ContentAsString(vResponseContent)
      else
        Result := '';
    except
      vResponseContent.Size := 0;
    end;
  finally
    FreeAndNil(vResponseContent);
  end;
end;

function THTTPClient.SetAccept(const AValue, ACharSet, AEncoding, ALanguage: string): THTTPClient;
begin
  DoSetAccept(AValue, ACharSet, AEncoding, ALanguage);
  Result := Self;
end;

function THTTPClient.SetContentSettings(const AType, AEncoding, ALanguage: string): THTTPClient;
begin
  DoSetContentSettings(AType, AEncoding, ALanguage);
  Result := Self;
end;

function THTTPClient.SetSecurityProtocolVersion(const AVersion: TSecurityProtocolVersion): THTTPClient;
begin
  DoSetSecurityProtocolVersion(AVersion);
  Result := Self;
end;

{ TIndyHTTPClient }

constructor TIndyHTTPClient.Create(const AEngine: TNetEngine);
begin
  inherited Create(AEngine);

  FHTTPClient := TIdHTTP.Create(nil);
  FHTTPClient.HandleRedirects := True;
  FHTTPClient.Request.UserAgent := cUserAgent;
  FHTTPClient.ProtocolVersion := pv1_1;
  FHTTPClient.HTTPOptions := FHTTPClient.HTTPOptions + [hoKeepOrigProtocol];

  FCookieManager := TIdCookieManager.Create(nil);
  FHTTPClient.CookieManager := FCookieManager;
  FHTTPClient.AllowCookies := True;

  FIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
  FIOHandler.SSLOptions.Method := sslvSSLv23;
  FHTTPClient.IOHandler := FIOHandler;
end;

destructor TIndyHTTPClient.Destroy;
begin
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

procedure TIndyHTTPClient.DoAddClientCookie(const ACookie: string);
begin
  FCookieManager.CookieCollection.AddClientCookie(ACookie);
end;

procedure TIndyHTTPClient.DoAddCookie(const AHost, ACookie: string);
begin
  FCookieManager.AddServerCookie(ACookie, TIdUri.Create(AHost));
end;

procedure TIndyHTTPClient.DoAddHeader(const AName, AValue: string);
begin
  FHTTPClient.Request.CustomHeaders.AddValue(AName, AValue);
end;

procedure TIndyHTTPClient.DoRequest(const AMethod: THTTPHeaderMethod; const AUrl: string;
  const AParams, AResponseContent: TStream);
begin
  case AMethod of
    hhmGet: FHTTPClient.Get(AUrl, AResponseContent);
    hhmPost: FHTTPClient.Post(AUrl, AParams, AResponseContent);
    hhmPut: FHTTPClient.Put(AUrl, AParams, AResponseContent);
    hhmDelete: FHTTPClient.Delete(AUrl, AResponseContent);
    hhmPatch: FHTTPClient.Patch(AUrl, AParams, AResponseContent);
    hhmHead: FHTTPClient.Head(AUrl);
  end;
end;

procedure TIndyHTTPClient.DoSetAccept(const AValue, ACharSet, AEncoding, ALanguage: string);
begin
  FHTTPClient.Request.Accept := AValue;
  FHTTPClient.Request.AcceptCharSet := ACharSet;
  FHTTPClient.Request.AcceptEncoding := AEncoding;
  FHTTPClient.Request.AcceptLanguage := ALanguage;
end;

procedure TIndyHTTPClient.DoSetContentSettings(const AType, AEncoding, ALanguage: string);
begin
  FHTTPClient.Request.ContentType := AType;
  FHTTPClient.Request.ContentEncoding := AEncoding;
  FHTTPClient.Request.ContentLanguage := ALanguage;
end;

procedure TIndyHTTPClient.DoSetSecurityProtocolVersion(const AVersion: TSecurityProtocolVersion);
begin
  case AVersion of
    spvSSLv2: FIOHandler.SSLOptions.Method := sslvSSLv2;
    spvSSLv3: FIOHandler.SSLOptions.Method := sslvSSLv3;
    spvTLSv1: FIOHandler.SSLOptions.Method := sslvTLSv1;
    spvTLSv11: FIOHandler.SSLOptions.Method := sslvTLSv1_1;
    spvTLSv12: FIOHandler.SSLOptions.Method := sslvTLSv1_2;
    spvTLSv13: FIOHandler.SSLOptions.Method := sslvTLSv1_2;
  else
    FIOHandler.SSLOptions.Method := sslvSSLv23
  end;
end;

function TIndyHTTPClient.GetResponseCharset: string;
begin
  if Assigned(FHTTPClient.Response) then
    Result := FHTTPClient.Response.CharSet
  else
    Result := '';
end;

function TIndyHTTPClient.GetResponseCode: Integer;
begin
  if Assigned(FHTTPClient.Response) then
    Result := FHTTPClient.ResponseCode
  else
    Result := -1;
end;

function TIndyHTTPClient.GetResponseEncoding: string;
begin
  if Assigned(FHTTPClient.Response) then
    Result := FHTTPClient.Response.ContentEncoding
  else
    Result := '';
end;

function TIndyHTTPClient.GetResponseText: string;
begin
  if Assigned(FHTTPClient.Response) then
    Result := FHTTPClient.ResponseText
  else
    Result := '';
end;

{ TNetPoint }

constructor TNetPoint.Create(const AEngine: TNetEngine);
begin
  inherited Create;
  FNetEngine := AEngine;
end;

destructor TNetPoint.Destroy;
begin
  FNetEngine := nil;
  inherited Destroy;
end;

{ TSystemHTTPClient }

constructor TSystemHTTPClient.Create(const AEngine: TNetEngine);
begin
  inherited Create(AEngine);

  FHTTPClient := TNetHTTPClient.Create(nil);
  FHTTPClient.HandleRedirects := True;
  FHTTPClient.UserAgent := cUserAgent;

  FResponse := nil;
end;

destructor TSystemHTTPClient.Destroy;
begin
  FreeAndNil(FHTTPClient);

  inherited Destroy;
end;

procedure TSystemHTTPClient.DoAddClientCookie(const ACookie: string);
begin
end;

procedure TSystemHTTPClient.DoAddCookie(const AHost, ACookie: string);
begin
  FHTTPClient.CookieManager.AddServerCookie(ACookie, AHost);
end;

procedure TSystemHTTPClient.DoAddHeader(const AName, AValue: string);
begin
  FHTTPClient.CustomHeaders[AName] := AValue;
end;

procedure TSystemHTTPClient.DoRequest(const AMethod: THTTPHeaderMethod; const AUrl: string; const AParams,
  AResponseContent: TStream);
begin
  case AMethod of
    hhmGet: FResponse := FHTTPClient.Get(AUrl, AResponseContent);
    hhmPost: FResponse := FHTTPClient.Post(AUrl, AParams, AResponseContent);
    hhmPut: FResponse := FHTTPClient.Put(AUrl, AParams, AResponseContent);
    hhmDelete: FResponse := FHTTPClient.Delete(AUrl, AResponseContent);
    hhmPatch: FResponse := FHTTPClient.Patch(AUrl, AParams, AResponseContent);
    hhmHead: FResponse := FHTTPClient.Head(AUrl);
  else
    FResponse := nil;
  end;
end;

procedure TSystemHTTPClient.DoSetAccept(const AValue, ACharSet, AEncoding, ALanguage: string);
begin
  FHTTPClient.Accept := AValue;
  FHTTPClient.AcceptCharSet := ACharSet;
  FHTTPClient.AcceptEncoding := AEncoding;
  FHTTPClient.AcceptLanguage := ALanguage;
end;

procedure TSystemHTTPClient.DoSetContentSettings(const AType, AEncoding, ALanguage: string);
begin
  FHTTPClient.ContentType := AType;
  //FHTTPClient.CustomHeaders['content-encoding'] := AEncoding;
  //FHTTPClient.CustomHeaders['content-language'] := ALanguage;
end;

procedure TSystemHTTPClient.DoSetSecurityProtocolVersion(const AVersion: TSecurityProtocolVersion);
begin
  case AVersion of
    spvSSLv2: FHTTPClient.SecureProtocols := [THTTPSecureProtocol.SSL2];
    spvSSLv23: FHTTPClient.SecureProtocols := [THTTPSecureProtocol.SSL2, THTTPSecureProtocol.SSL3];
    spvSSLv3: FHTTPClient.SecureProtocols := [THTTPSecureProtocol.SSL3];
    spvTLSv1: FHTTPClient.SecureProtocols := [THTTPSecureProtocol.TLS1];
    spvTLSv11: FHTTPClient.SecureProtocols := [THTTPSecureProtocol.TLS11];
    spvTLSv12: FHTTPClient.SecureProtocols := [THTTPSecureProtocol.TLS12];
    spvTLSv13: FHTTPClient.SecureProtocols := [THTTPSecureProtocol.TLS13];
  else
    FHTTPClient.SecureProtocols := []
  end;
end;

function TSystemHTTPClient.GetResponseCharset: string;
begin
  if Assigned(FResponse) then
    Result := FResponse.ContentCharSet
  else
    Result := '';
end;

function TSystemHTTPClient.GetResponseCode: Integer;
begin
  if Assigned(FResponse) then
    Result := FResponse.StatusCode
  else
    Result := -1;
end;

function TSystemHTTPClient.GetResponseEncoding: string;
begin
  if Assigned(FResponse) then
    Result := FResponse.ContentEncoding
  else
    Result := '';
end;

function TSystemHTTPClient.GetResponseText: string;
begin
  if Assigned(FResponse) then
    Result := FResponse.StatusText
  else
    Result := '';
end;

{ TSystemNetEngine }

function TSystemNetEngine.DoCreateNetPoint(const ANetPointProtocol: TNetPointProtocol): TNetPoint;
begin
  case ANetPointProtocol of
    nppHTTPClient: Result := TSystemHTTPClient.Create(Self);
  else
    Result := nil;
  end;
end;

{ TIndySMTPClient }

constructor TIndySMTPClient.Create(const AEngine: TNetEngine);
begin
  inherited Create(AEngine);

  FSMTPClient := TIdSMTP.Create(nil);
  FSMTPClient.AuthType := satDefault;
  FIOHandler := TIdSSLIOHandlerSocketOpenSSL.Create;
  FIOHandler.SSLOptions.Method := sslvSSLv23;
  FIOHandler.SSLOptions.Mode := sslmUnassigned;
  FSMTPClient.IOHandler := FIOHandler;
  FSMTPClient.UseTLS := utUseExplicitTLS;

  FMessage := TIdMessage.Create(nil);
  FTextPart := TIdText.Create(FMessage.MessageParts, nil);
  FTextPart.ContentType := 'text/plain';
  FTextPart.CharSet := 'Windows-1251';
end;

destructor TIndySMTPClient.Destroy;
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

function TIndySMTPClient.DoCheckConnection: string;
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

procedure TIndySMTPClient.DoInit(const AHost: string; const APort: Integer; const ALogin, APassword, AServiceName,
  AReplyTo: string; const ATimeout: Integer);
var
  vAddressItem: TIDEMailAddressItem;
begin
  FSMTPClient.Host := AHost;
  FSMTPClient.Port := APort;
  FSMTPClient.ConnectTimeout := ATimeout;
  FSMTPClient.Username := ALogin;
  FSMTPClient.Password := APassword;

  if AReplyTo <> '' then
  begin
    vAddressItem := FMessage.ReplyTo.Add;
    vAddressItem.Address := AReplyTo;
  end;
  FMessage.From.Name := AServiceName;
  FMessage.From.Address := ALogin;
end;

procedure TIndySMTPClient.DoSend(const ARecipients, ASubject, AMessage: string; const AAttachments: TStrings);
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

  FSMTPClient.Connect;
  if FSMTPClient.Connected then
  begin
    try
      FSMTPClient.Send(FMessage);
    except
    end;

    FSMTPClient.Disconnect(True);
  end;
end;

procedure TIndySMTPClient.DoSetCharset(const ACharset: string);
begin
  FTextPart.CharSet := ACharset;
end;

procedure TIndySMTPClient.DoSetContentType(const AContentType: string);
begin
  FTextPart.ContentType := AContentType;
end;

{ TSMTPClient }

function TSMTPClient.CheckConnection: string;
begin
  Result := DoCheckConnection;
end;

function TSMTPClient.DoCheckConnection: string;
begin
  Result := '';
end;

procedure TSMTPClient.DoInit(const AHost: string; const APort: Integer; const ALogin, APassword, AServiceName,
  AReplyTo: string; const ATimeout: Integer);
begin
end;

procedure TSMTPClient.DoSend(const ARecipients, ASubject, AMessage: string; const AAttachments: TStrings);
begin
end;

procedure TSMTPClient.DoSetCharset(const ACharset: string);
begin
end;

procedure TSMTPClient.DoSetContentType(const AContentType: string);
begin
end;

procedure TSMTPClient.Init(const AHost: string; const APort: Integer; const ALogin, APassword, AServiceName,
  AReplyTo: string; const ATimeout: Integer);
begin
  DoInit(AHost, APort, ALogin, APassword, AServiceName, AReplyTo, ATimeout)
end;

procedure TSMTPClient.Send(const ARecipients, ASubject, AMessage: string; const AAttachments: TStrings);
begin
  DoSend(ARecipients, ASubject, AMessage, AAttachments);
end;

function TSMTPClient.SetCharset(const ACharset: string): TSMTPClient;
begin
  DoSetCharset(ACharset);
  Result := Self;
end;

function TSMTPClient.SetContentType(const AContentType: string): TSMTPClient;
begin
  DoSetContentType(AContentType);
  Result := Self;
end;

initialization
  TBaseModule.RegisterModule('NetEngine', '', 'Indy', TIndyNetEngine);
  TBaseModule.RegisterModule('NetEngine', '', 'System', TSystemNetEngine);

end.
