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

unit uParameters;

interface

uses
  Classes;

type
  TParameterKind = (pkSimple, pkBlob);

  TBaseParameter = class
  protected
    FParamName: string;
    FParamKind: TParameterKind;
  public
    constructor Create(const AName: string; const AParamKind: TParameterKind);

    property Name: string read FParamName;
    property ParamKind: TParameterKind read FParamKind;
  end;

  TSimpleParameter = class(TBaseParameter)
  private
    FParamValue: Variant;
    FIsKey: Boolean;
  public
    constructor Create(const AName: string; const AValue: Variant;
      const AIsKey: Boolean = False);
    property Value: Variant read FParamValue;
    property IsKey: Boolean read FIsKey;
  end;

  TBlobParameter = class(TBaseParameter)
  private
    FStream: TStream;
  public
    constructor Create(const AName: string; const AValue: TStream);
    property Value: TStream read FStream;
  end;

  TGetDBParameterFunc = function(const AParamName: string): TBaseParameter of object;

implementation

{ TBaseParameter }

constructor TBaseParameter.Create(const AName: string;
  const AParamKind: TParameterKind);
begin
  inherited Create;

  FParamName := AName;
  FParamKind := AParamKind;
end;

{ TSimpleParameter }

constructor TSimpleParameter.Create(const AName: string; const AValue: Variant;
  const AIsKey: Boolean);
begin
  inherited Create(AName, pkSimple);
  FParamValue := AValue;
  FIsKey := AIsKey;
end;

{ TBlobParameter }

constructor TBlobParameter.Create(const AName: string; const AValue: TStream);
begin
  inherited Create(AName, pkBlob);
  FStream := AValue;
end;

end.
