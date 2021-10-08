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

unit uChartData;

interface

uses
  Classes, Generics.Collections, uComplexObject, uTensor, uJSON, uStorage;

type
  TSerieDisplayMode = (sdmPoints, sdmSpots, sdmLines, sdmArea);
  TAxeType = (atLinear, atLogarithmic);
  TAxeOption = (aopNone);
  TAxeOptions = set of TAxeOption;
  TAxeOrientation = (aorHorizontal, aorVertical);

  TChartSerie = class
  private
    FTitle: string;
    FSerieDataIndex: Integer;
    FDisplayMode: TSerieDisplayMode;
  public
    procedure Init(const ASerieDataIndex: Integer; const ATitle: string;
      const ADisplayMode: TSerieDisplayMode);

    property Title: string read FTitle;
    property SerieDataIndex: Integer read FSerieDataIndex;
    property DisplayMode: TSerieDisplayMode read FDisplayMode;
  end;

  TChartAxe = class
  private
    FTitle: string;
    FOrientation: TAxeOrientation;
    FAxeType: TAxeType;
    FOptions: TAxeOptions;
  public
    procedure Init(const ATitle: string; const AOrientation: TAxeOrientation;
      const AAxeType: TAxeType; const AOptions: TAxeOptions);

    function HasOption(const AOption: TAxeOption): Boolean;

    property Title: string read FTitle;
    property Orientation: TAxeOrientation read FOrientation;
    property AxeType: TAxeType read FAxeType;
  end;

  TChartData = class(TComplexObject)
  private
    FTensor: TTensor;
    FXDataIndex: Integer; // Индекс данных, выводимых по оси X (-1 - использовать встроенную индексацию)
    FSeries: TObjectList<TChartSerie>;
    FDefaultSerie: TChartSerie;
    FAxes: TObjectList<TChartAxe>;
    FDefaultAxe: TChartAxe;
    FMinValue: Double;
    FMaxValue: Double;
    function GetSize: Integer;
    function GetValue(const AIndex: Integer): Double;
    function GetMaxDimension: Integer;
    function GetMinDimension: Integer;
  protected
    procedure DoFill(const AObject: TComplexObject); override;
    function GetJSONValue: TJSONValue; override;
    procedure SetJSONValue(const Value: TJSONValue); override;
    procedure DoTransfer(const AStorage: TStorage); override;
    procedure DoLoad(const AStorage: TStorage); override;
    procedure DoUpdateContent; override;
  public
    constructor Create(const AField: TObject; const AOnChanged: TNotifyEvent); override;
    destructor Destroy; override;

    function AddSerie(const ASerieDataIndex: Integer; const ATitle: string;
      const ADisplayMode: TSerieDisplayMode = sdmLines): TChartSerie;
    function AddAxe(const ATitle: string; const AOrientation: TAxeOrientation;
      const AAxeType: TAxeType = atLinear; const AOptions: TAxeOptions = []): TChartAxe;

    property Values[const AIndex: Integer]: Double read GetValue; default;
    property Size: Integer read GetSize;
    property MinValue: Double read FMinValue;
    property MaxValue: Double read FMaxValue;
    property MinDimension: Integer read GetMinDimension;
    property MaxDimension: Integer read GetMaxDimension;

    property Tensor: TTensor read FTensor;
    //property Series: TChartSeries read FSeries;
    property DefaultSerie: TChartSerie read FDefaultSerie;
    //property Axes: TChartAxes read FAxes;
    property DefaultAxe: TChartAxe read FDefaultAxe;
  end;

implementation

uses
  SysUtils;

{ TChartData }

function TChartData.AddAxe(const ATitle: string; const AOrientation: TAxeOrientation; const AAxeType: TAxeType;
  const AOptions: TAxeOptions): TChartAxe;
begin
  Result := TChartAxe.Create;
  FAxes.Add(Result);
  Result.Init(ATitle, AOrientation, AAxeType, AOptions);
end;

function TChartData.AddSerie(const ASerieDataIndex: Integer; const ATitle: string;
  const ADisplayMode: TSerieDisplayMode = sdmLines): TChartSerie;
begin
  Result := TChartSerie.Create;
  FSeries.Add(Result);
  Result.Init(ASerieDataIndex, ATitle, ADisplayMode);
end;

constructor TChartData.Create(const AField: TObject; const AOnChanged: TNotifyEvent);
begin
  inherited Create(AField, AOnChanged);

  FXDataIndex := -1;
  FSeries := TObjectList<TChartSerie>.Create;
  FDefaultSerie := TChartSerie.Create;
  FAxes := TObjectList<TChartAxe>.Create;
  FDefaultAxe := TChartAxe.Create;

  FTensor := TTensor.Create([0, 1]);
  DoUpdateContent;
end;

destructor TChartData.Destroy;
begin
  FreeAndNil(FDefaultSerie);
  FreeAndNil(FSeries);
  FreeAndNil(FDefaultAxe);
  FreeAndNil(FAxes);

  FreeAndNil(FTensor);

  inherited Destroy;
end;

procedure TChartData.DoFill(const AObject: TComplexObject);
begin
end;

procedure TChartData.DoLoad(const AStorage: TStorage);
begin
end;

procedure TChartData.DoTransfer(const AStorage: TStorage);
begin
end;

procedure TChartData.DoUpdateContent;
begin
  FTensor.MinMax(FMinValue, FMaxValue);
end;

function TChartData.GetJSONValue: TJSONValue;
begin
  Result := nil;
end;

function TChartData.GetMaxDimension: Integer;
begin
  Result := FTensor.Size - 1;
end;

function TChartData.GetMinDimension: Integer;
begin
  Result := 0;
end;

function TChartData.GetSize: Integer;
begin
  Result := FTensor.Size;
end;

function TChartData.GetValue(const AIndex: Integer): Double;
begin
  Result := FTensor.FlatValues[AIndex];
end;

procedure TChartData.SetJSONValue(const Value: TJSONValue);
begin
end;

{ TChartSerie }

procedure TChartSerie.Init(const ASerieDataIndex: Integer; const ATitle: string;
  const ADisplayMode: TSerieDisplayMode);
begin
  FSerieDataIndex := ASerieDataIndex;
  FTitle := ATitle;
  FDisplayMode := ADisplayMode;
end;

{ TChartAxe }

function TChartAxe.HasOption(const AOption: TAxeOption): Boolean;
begin
  Result := AOption in FOptions;
end;

procedure TChartAxe.Init(const ATitle: string; const AOrientation: TAxeOrientation;
  const AAxeType: TAxeType; const AOptions: TAxeOptions);
begin
  FTitle := ATitle;
  FOrientation := AOrientation;
  FAxeType := AAxeType;
  FOptions := AOptions;
end;

end.
