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

unit uniBlobEditors;

interface

uses
  uUIBuilder, uniArea, uScene, uSimpleChart, uLayout;

type
  TUniGUISceneArea = class(TUniGUIControl)
  protected
    FScene: TScene;
    procedure DoActivate(const AAreaState: string = ''); override;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoDisableContent; override;
    procedure DoBeforeFreeControl; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TUniGUIChartArea = class(TUniGUISceneArea)
  protected
    FChart: TSimpleChart;
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
  end;

implementation

uses
  uPlatform, uModule, uDomain, uDrawStyles, uPresenter, uConfiguration, uConsts,
  uUniGUIPresenter, uniScene;

{ TUniGUISceneArea }

procedure TUniGUISceneArea.DoActivate(const AAreaState: string);
begin
  inherited;
  FScene.Activate;
end;

procedure TUniGUISceneArea.DoBeforeFreeControl;
begin
  FScene.Free;
end;

function TUniGUISceneArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
//var
  //vDomain: TDomain;
  //vModuleInfo: TModuleInfo;
begin
  //vDomain := TDomain(FView.Domain);
  // Domain oriented code
  //vModuleName := _Platform.ResolveModulename(vDomain.Settings, 'ChartPainter');
  //vSceneClass := TSceneClass(TPresenter(FPresenter).GetCanvasClass(vModuleName);
  //vModuleInfo := _Platform.ResolveModuleInfo(vDomain.Settings, 'ChartPainter', 'Painting');
  FScene := TUniGUIScene.Create(GetRealControl(AParent));
  Result := TUniGUIScene(FScene).Control;
end;

procedure TUniGUISceneArea.DoDisableContent;
begin
  FScene.Enabled := False;
end;

procedure TUniGUISceneArea.RefillArea(const AKind: Word);
begin
  if AKind <> dckNameChanged then
    FScene.Repaint;
end;

{ TUniGUIChartArea }

function TUniGUIChartArea.DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject;
begin
  Result := inherited DoCreateControl(AParent, ALayout);
  ALayout.Id := 'Chart';
  FChart := TDataChart.Create(FScene, nil);
end;

initialization

TPresenter.RegisterControlClass('Web.UniGUI', uiComplexEdit, 'chart', TUniGUIChartArea);

end.
