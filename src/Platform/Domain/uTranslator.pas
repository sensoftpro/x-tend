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

unit uTranslator;

interface

uses
  Classes, uLocalizator;

type
  TTranslator = class
  private
    FLanguage: string;
    [Weak] FLocalizator: TLocalizator;
    [Weak] FDictionary: TLanguage;
    procedure SetLanguage(const Value: string);
  public
    constructor Create(const ALocalizator: TLocalizator; const ALanguage: string);
    destructor Destroy; override;

    function Translate(const AKey, ADefault: string): string;

    property Language: string read FLanguage write SetLanguage;
  end;

implementation

{ TTranslator }

constructor TTranslator.Create(const ALocalizator: TLocalizator; const ALanguage: string);
begin
  inherited Create;

  FLocalizator := ALocalizator;
  SetLanguage(ALanguage);
end;

destructor TTranslator.Destroy;
begin
  FDictionary := nil;
  FLocalizator := nil;

  inherited Destroy;
end;

procedure TTranslator.SetLanguage(const Value: string);
begin
  FLanguage := Value;
  FDictionary := FLocalizator.LanguageByName(FLanguage);
end;

function TTranslator.Translate(const AKey, ADefault: string): string;
begin
  if not Assigned(FDictionary) then
    Exit(ADefault);

  if FDictionary.KeyExists(AKey) then
    Result := FDictionary.Values[AKey]
  else
    Result := FLocalizator.Translate(AKey, ADefault);
end;

end.
