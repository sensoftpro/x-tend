﻿{---------------------------------------------------------------------------------
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

unit fmxBlobEditors;

interface

uses
  uUIBuilder, fmxArea, uScene, uSimpleChart, uLayout, uView;

type
  TFMXCanvasArea = class(TFMXControl)
  protected
    FScene: TScene;
    procedure DoActivate(const AAreaState: string = ''); override;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoDisableContent; override;
    procedure DoBeforeFreeControl; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TFMXChartArea = class(TFMXCanvasArea)
  protected
    FChart: TSimpleChart;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
  end;

  TFMXSceneArea = class(TFMXCanvasArea)
  protected
    FSceneObject: TDomainSceneObject;
    procedure RefillArea(const AKind: Word); override;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoExecuteUIAction(const AView: TView); override;
  end;

implementation

uses
  uPlatform, uModule, uDomain, uDrawStyles, uPresenter, uConfiguration, uConsts,
  uFMXPresenter, fmxScene;

{ TFMXCanvasArea }

procedure TFMXCanvasArea.DoActivate(const AAreaState: string);
begin
  inherited;
  FScene.Activate;
end;

procedure TFMXCanvasArea.DoBeforeFreeControl;
begin
  FScene.Free;
end;

function TFMXCanvasArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
var
  vDomain: TDomain;
  vModuleInfo: TModuleInfo;
begin
  vDomain := TDomain(FView.Domain);
  // Domain oriented code
  //vModuleName := _Platform.ResolveModulename(vDomain.Settings, 'ChartPainter');
  //vSceneClass := TSceneClass(TPresenter(FPresenter).GetCanvasClass(vModuleName);
  vModuleInfo := _Platform.ResolveModuleInfo(vDomain.Settings, 'ChartPainter', 'Painting');
  FScene := TSceneClass(vModuleInfo.ModuleClass).Create(GetRealControl(AParent));
  Result := TFMXScene(FScene).Control;
end;

procedure TFMXCanvasArea.DoDisableContent;
begin
  FScene.Enabled := False;
end;

procedure TFMXCanvasArea.RefillArea(const AKind: Word);
begin
  if AKind <> dckNameChanged then
    FScene.Repaint;
end;

{ TFMXChartArea }

function TFMXChartArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := inherited DoCreateControl(AParent, ALayout);
  ALayout.Id := 'Chart';
  FChart := TDataChart.Create(FScene, nil);
end;

{ TFMXSceneArea }

type
  TCrackedDomainSceneObject = class(TDomainSceneObject) end;

function TFMXSceneArea.DoCreateControl(const AParent: TUIArea;
  const ALayout: TLayout): TObject;
var
  vObjectClass: TDomainSceneObjectClass;
begin
  Result := inherited DoCreateControl(AParent, ALayout);
  vObjectClass := TPresenter.GetSceneObjectClass(ALayout.Id);
  if Assigned(vObjectClass) then
    FSceneObject := vObjectClass.Create(FOwner, FScene)
  else
    Assert(False, 'Class of scene object for name "' + ALayout.Id + '" is not supported');
end;

procedure TFMXSceneArea.DoExecuteUIAction(const AView: TView);
begin
  TCrackedDomainSceneObject(FSceneObject).ExecuteUIAction(FOwner, AView);
end;

procedure TFMXSceneArea.RefillArea(const AKind: Word);
begin
  if AKind = dckNameChanged then
    Exit;
  TCrackedDomainSceneObject(FSceneObject).UpdateBinding(Self);
  inherited RefillArea(AKind);
end;

initialization

TPresenter.RegisterControlClass('FMX', uiComplexEdit, 'chart', TFMXChartArea);
TPresenter.RegisterControlClass('FMX', uiEntityEdit, 'scene', TFMXSceneArea);

end.
