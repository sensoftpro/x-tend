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

unit fgxArea;

interface

uses
  System.Classes, System.Generics.Collections, System.Types, System.UITypes, System.SysUtils,


  uConsts, uUIBuilder, uDefinition, uEntity, uView, uLayout;

type
  TFGXControl = class(TNativeControlHolder)
  private
    procedure BeforeContextMenuShow(Sender: TObject);
  protected
    function IndexOfSender(const ASender: TObject): Integer; override;

    procedure DoActivate(const AUrlParams: string); override;
    procedure DoClose(const AModalResult: Integer); override;
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;

    procedure AssignFromLayout(const ALayout: TLayout; const AParams: string); override;

    procedure SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl); override;
    procedure SetControl(const AControl: TObject); override;
    procedure SetParent(const AParent: TUIArea); override;
    function GetFocused: Boolean; override;
    procedure SetFocused(const Value: Boolean); override;
    function GetBounds: TRect; override;
    procedure SetBounds(const Value: TRect); override;
    function GetViewState: TViewState; override;
    procedure SetViewState(const AViewState: TViewState); override;
    function GetTabOrder: Integer; override;
    procedure SetTabOrder(const ATabOrder: Integer); override;
    function GetActiveChildArea: TUIArea; override;
    procedure SetActiveChildArea(const AArea: TUIArea); override;
    function GetModalResult: TModalResult; override;
    procedure SetModalResult(const AModalResult: TModalResult); override;
    function GetWindowState: TWindowState; override;
    procedure SetWindowState(const AWindowState: TWindowState); override;
    procedure SetAlignment(const AAlignment: TAlignment); override;

    function DoCreateCaption(const AParent: TUIArea; const ACaption, AHint: string): TObject; override;
    procedure PlaceLabel; override;
    procedure SetCaptionProperty(const ALayout: TLayout); virtual;
    procedure UpdateCaptionVisibility; override;
  public
    constructor Create(const AOwner: TUIArea; const AParams: string = ''); override;
    destructor Destroy; override;
  end;

  {TFMXButton = class(TFGXControl)
  private
    FTypeSelectionMenu: TPopupMenu;
    procedure ShowPopup(Sender: TObject);
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure DoBeforeFreeControl; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TFMXLink = class(TFGXControl)
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    procedure RefillArea(const AKind: Word); override;
  end;

  TFMXMainMenuNavigation = class(TFGXControl)
  private
    FMenu: TMainMenu;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TFMXToolBarNavigation = class(TFGXControl)
  private
    FToolBar: TToolBar;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TFMXTreeViewNavigation = class(TFGXControl)
  private
    FTreeView: TTreeView;
    FDefaultWorkArea: string;
    procedure SelectNode(const ANode: TTreeViewItem);
    procedure OnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Refill;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
    procedure DoExecuteUIAction(const AView: TView); override;
  end;

  TFMXNavBarNavigation = class(TFGXControl)
  private
    FNavBar: TPanel;
    FNavBarGroup: TExpander;
    FNavBarItem: TButton;
    FLastPos: Single;
  protected
    function DoCreateControl(const AParent: TUIArea; const ALayout: TLayout): TObject; override;
    function DoCreateItem(const AParent: TUIArea; const ANavItem: TNavigationItem;
      const ACaption, AHint: string; const AImageIndex: Integer): TObject; override;
  end;

  TFMXForm = class(TForm)
  end;

function AlignToAlignLayout(const AAlign: Byte): TAlignLayout; overload;
function AlignToAlignLayout(const AAlign: TLayoutAlign): TAlignLayout; overload; }

implementation

uses
  Math, StrUtils, Generics.Defaults, Variants,

  //FGX.Application, FGX.Forms,

  uPresenter, uFGXPresenter, uConfiguration, uSession, uInteractor, uUtils, uCollection,
  uEntityList, uDomainUtils, uChangeManager;


{ TFGXControl }

procedure TFGXControl.AssignFromLayout(const ALayout: TLayout; const AParams: string);
var
  //vForm: TForm;
  vWS: Integer;
begin
  {if FIsForm then
  begin
    vForm := TForm(FControl);

    if ALayout.Kind = lkFrame then
    begin
      if (ALayout.Tag and cFormResizable) > 0 then
      begin
        vForm.BorderStyle := TFmxFormBorderStyle.Sizeable;
        vForm.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
      end;

      if (ALayout.Tag and cFormDisableMinimizeButton) > 0 then
        vForm.BorderIcons := vForm.BorderIcons - [TBorderIcon.biMinimize];

      if (ALayout.Tag and cFormDisableMaximizeButton) > 0 then
        vForm.BorderIcons := vForm.BorderIcons - [TBorderIcon.biMaximize];

      if (ALayout.Tag and cFormPositionDesign) > 0 then
        vForm.Position := TFormPosition.Designed;

      if (ALayout.Tag and cFormNotResizable) > 0 then
        vForm.BorderStyle := TFmxFormBorderStyle.Single;

      vWS := StrToIntDef(GetUrlParam(AParams, 'WindowState', ''), -1);
      if vWS > -1 then
        vForm.WindowState := TWindowState(vWS);

      if (vForm.WindowState = TWindowState.wsNormal) then
      begin
        //if (ALayout.Tag and cFormUseDesignSizes) > 0 then
        //begin
          vForm.ClientWidth := ALayout.Width;
          if FOwner.View.DefinitionKind = dkDomain then
            vForm.ClientHeight := ALayout.Height
          else
            vForm.ClientHeight := ALayout.Height + cServiceAreaHeight;
        //end;
      end;

      if ALayout.Caption <> '' then
        vForm.Caption := ALayout.Caption;
      if ALayout.Color <> TAlphaColorRec.Gray then
      begin
        vForm.Fill.Color := ALayout.Color;
        vForm.Fill.Kind := TBrushKind.Solid;
      end;
    end
    else if ALayout.Kind = lkPanel then
    begin
      vForm.BorderStyle := TFmxFormBorderStyle.Sizeable;
      vForm.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
      vForm.Caption := ALayout.Caption;
      vForm.ClientWidth := ALayout.Width;
      if FOwner.View.DefinitionKind = dkDomain then
        vForm.ClientHeight := ALayout.Height
      else
        vForm.ClientHeight := ALayout.Height + cServiceAreaHeight;
      vForm.Position := TFormPosition.ScreenCenter;
    end
    else if ALayout.Kind = lkPage then
    begin
      vForm.BorderStyle := TFmxFormBorderStyle.Sizeable;
      vForm.BorderIcons := [TBorderIcon.biSystemMenu, TBorderIcon.biMinimize, TBorderIcon.biMaximize];
      vForm.Caption := ALayout.Caption;
    end
    else
      Assert(False, 'Непонятно какой контрол в лэйауте');
  end
  else if ALayout.Kind = lkFrame then
  begin
    if (ALayout.Caption <> '') and (FControl is TTabItem) then
    begin
      TTabItem(FControl).Text := ALayout.Caption;
      if Pos('=', ALayout.Caption) > 0 then // Hint содержит url-строку с параметрами
      begin
        TTabItem(FControl).Text := GetUrlParam(ALayout.Caption, 'Caption', '');
        TTabItem(FControl).ImageIndex := FOwner.GetImageIndex(GetUrlParam(ALayout.Caption, 'ImageIndex', ''));
      end;
    end;
  end
  else if (ALayout.Kind in [lkPanel, lkMemo, lkShape]) and (FControl is TControl) then
  begin
    SetBounds(Rect(ALayout.Left, ALayout.Top,
      ALayout.Left + ALayout.Width, ALayout.Top + ALayout.Height));
    SetCaptionProperty(ALayout);

    if FControl is TCustomEdit then
    begin
      CopyTextSettings(TCustomEdit(FControl).TextSettings, ALayout);
      TCustomEdit(FControl).StyledSettings := [];
    end
    else if FControl is TPresentedTextControl then
    begin
      CopyTextSettings(TPresentedTextControl(FControl).TextSettings, ALayout);
      TPresentedTextControl(FControl).StyledSettings := [];
    end
    else if FControl is TTextControl then
    begin
      CopyTextSettings(TTextControl(FControl).TextSettings, ALayout);
      TTextControl(FControl).StyledSettings := [];
    end;

    TControl(FControl).Anchors := ALayout.Anchors;
    TControl(FControl).Align := AlignToAlignLayout(ALayout.Align);
    CopyMargins(TControl(FControl), ALayout);
    CopyPadding(TControl(FControl), ALayout);
//    SetAlignment(ALayout.Alignment);

    // TODO
  end; }
end;

procedure TFGXControl.BeforeContextMenuShow(Sender: TObject);
{var
  vMenu: TPopupMenu;
  vMenuItem: TFmxObject;

  procedure CheckMenuItems(const AMenuItem: TMenuItem);
  var
    vChildItem: TFmxObject;
    vArea: TUIArea;
  begin
    if AMenuItem.ChildrenCount > 0 then
    begin
      for vChildItem in AMenuItem.Children do
        CheckMenuItems(TMenuItem(vChildItem));
      Exit;
    end;

    vArea := AreaFromSender(AMenuItem);
    if not Assigned(vArea) then
      Exit;
    if not Assigned(vArea.View) then
      Exit;
    if vArea.View.DefinitionKind <> dkAction then
      Exit;
    if not Assigned(vArea.View.DomainObject) then
      Exit;

    if TEntity(vArea.View.DomainObject).FieldExists('IsChecked') then
      AMenuItem.IsChecked := TEntity(vArea.View.DomainObject)['IsChecked'];
  end; }

begin
  //vMenu := TPopupMenu(Sender);
  //for vMenuItem in vMenu.Children do
  //  CheckMenuItems(TMenuItem(vMenuItem));
end;

constructor TFGXControl.Create(const AOwner: TUIArea; const AParams: string);
begin
  inherited Create(AOwner, AParams);
end;

destructor TFGXControl.Destroy;
var
  vControl: TObject;
  vIsFloat: Boolean;
begin
  //if Assigned(FCaption) then
  //  TLabel(FCaption).Parent := nil;

  vControl := FControl;
  vIsFloat := (FOwner.Id = 'float') or (FOwner.Id = 'free');

  inherited Destroy;

  if not Assigned(vControl) then
    Exit;
  if not (vControl is TComponent) then
    Exit;

  //if vControl is TForm then
  begin
    if vIsFloat then
      FreeAndNil(vControl);
  end
  //else
  //  FreeAndNil(vControl);
end;

procedure TFGXControl.DoActivate(const AUrlParams: string);
var
  //vForm: TForm;
  vChangeTab: Boolean;
begin
  {if FControl is TForm then
  begin
    vForm := TForm(FControl);
    vForm.Activate;
  end
  else if FControl is TTabItem then
  begin
    vChangeTab := SameText(GetUrlParam(AUrlParams, 'TabActivationOption', ''), 'ChangeTab');
    if Assigned(Parent.CreateParams) and (Parent.CreateParams.IndexOfName('TabActivationOption') >= 0) and
      (not SameText(Parent.CreateParams.Values['TabActivationOption'], 'ChangeTab')) then
      vChangeTab := False;
    if vChangeTab then
      TTabItem(FControl).TabControl.ActiveTab := TTabItem(FControl);
  end; }
end;

procedure TFGXControl.DoBeginUpdate;
begin
end;

procedure TFGXControl.DoClose(const AModalResult: Integer);
//var
//  vForm: TForm;
begin
  if FIsForm then
  begin
    //vForm := TForm(FControl);

    if AModalResult = mrNone then
    begin
      TThread.CreateAnonymousThread(procedure
      begin
        TThread.Queue(nil, procedure
        begin
          //vForm.Close;
        end);
      end).Start;
    end
    else begin
      //vForm.Close;
      //vForm.ModalResult := AModalResult;
    end;
  end;
end;

function TFGXControl.DoCreateCaption(const AParent: TUIArea; const ACaption, AHint: string): TObject;
var
  //vLabel: TLabel;
  vFontSize: Single;
begin
{  vLabel := TLabel.Create(nil);
  Result := vLabel;

  vLabel.Parent := TFmxObject(GetRealControl(FParent));
//  vLabel.Align := TAlignLayout.Client;
  vLabel.Visible := True;
  vLabel.Text := ACaption;
  vLabel.Hint := AHint;

  if Assigned(FOwner.View.Parent) and (FOwner.View.Parent.DefinitionKind = dkAction)
    and TDefinition(FOwner.View.Parent.Definition).HasFlag(ccInstantExecution) then
  begin
    vLabel.StyledSettings := vLabel.StyledSettings - [TStyledSetting.Size];
    vLabel.Font.Size := 12;
  end
  else begin
    vLabel.StyledSettings := vLabel.StyledSettings - [TStyledSetting.Size, TStyledSetting.FontColor];
    vFontSize := vLabel.Font.Size;
    vFontSize := vFontSize - 3;
    if vFontSize < 8 then
      vFontSize := 8;
    vLabel.Font.Size := vFontSize * 96 / 72;
    vLabel.FontColor := TAlphaColorRec.Gray;
  end; }
end;

procedure TFGXControl.DoEndUpdate;
begin
end;

function TFGXControl.GetActiveChildArea: TUIArea;
begin
  //if FControl is TTabControl then
  //  Result := AreaFromSender(TTabControl(FControl).ActiveTab)
  //else
    Result := nil;
end;

function TFGXControl.GetBounds: TRect;
begin
  //if FIsForm then
  //  Result := TRect.Create(TForm(FControl).Left, TForm(FControl).Top,
  //    TForm(FControl).Left + TForm(FControl).Width, TForm(FControl).Top + TForm(FControl).Height)
  //else
  //  Result := TControl(FControl).BoundsRect.Round;
end;

function TFGXControl.GetFocused: Boolean;
begin
  //if FControl is TControl then
  //  Result := TControl(FControl).IsFocused
  //else
  //  Result := False;
end;

function TFGXControl.GetModalResult: TModalResult;
begin
  //if FControl is TForm then
  //  Result := TForm(FControl).ModalResult
  //else
  //  Result := mrNone;
end;

function TFGXControl.GetTabOrder: Integer;
begin
  //if (FControl is TControl) and TControl(FControl).TabStop then
  //  Result := TControl(FControl).TabOrder
  //else
    Result := -1;
end;

function TFGXControl.GetViewState: TViewState;
begin
  //if not (FControl is TControl) or FIsForm then
  //  Result := vsFullAccess
  //else if not TControl(FControl).Visible then
  //  Result := vsHidden
  //else if not TControl(FControl).Enabled then
  //  Result := vsDisabled
  //else
    Result := vsFullAccess;
end;

function TFGXControl.GetWindowState: TWindowState;
begin
  //if FControl is TForm then
  //  Result := TForm(FControl).WindowState
  //else
    Result := inherited GetWindowState;
end;

function TFGXControl.IndexOfSender(const ASender: TObject): Integer;
begin
  //Result := TMenuItem(ASender).Index;
end;

procedure TFGXControl.PlaceLabel;
var
  vSpace: Single;
  //vLabel: TLabel;
begin
  if FCaption = nil then
    Exit;

{  vLabel := TLabel(FCaption);
  vLabel.WordWrap := false;
  if FLabelPosition = lpTop then
  begin
    vLabel.Position.X := TControl(FControl).Position.X;
    vLabel.Position.Y := TControl(FControl).Position.Y - vLabel.Height - 4;
    vLabel.AutoSize := True;
    vLabel.TextSettings.VertAlign := TTextAlign.Leading;
  end
  else if FLabelPosition = lpLeft then
  begin
    vSpace := vLabel.Width + 8;
    vLabel.Position.X := TControl(FControl).AbsoluteToLocal(PointF(0,0)).X - vSpace;
    vLabel.Position.Y := TControl(FControl).AbsoluteToLocal(PointF(0,0)).Y + 3;
    vLabel.AutoSize := False;
  end;
//  vLabel.Align := TAlignLayout.Center;
  vLabel.Parent := TControl(FControl).Parent; }
end;

procedure TFGXControl.SetActiveChildArea(const AArea: TUIArea);
begin
  //
end;

procedure TFGXControl.SetAlignment(const AAlignment: TAlignment);
//const
//  cTextAligns: array[TAlignment] of TTextAlign =
//    (TTextAlign.Leading, TTextAlign.Trailing, TTextAlign.Center);
//  cLayoutAlign: array[TAlignment] of TALignLayout =
//   (TALignLayout.Left, TALignLayout.Right, TALignLayout.Center);
begin
//  if FControl is TCustomEdit then
//    TCustomEdit(FControl).TextSettings.HorzAlign := cTextAligns[AAlignment]
//  else if FControl is TPresentedTextControl then
//    TPresentedTextControl(FControl).TextSettings.HorzAlign := cTextAligns[AAlignment]
//  else if FControl is TTextControl then
//    TTextControl(FControl).TextSettings.HorzAlign := cTextAligns[AAlignment]
//  else
//    TControl(FControl).Align := cLayoutAlign[AAlignment];
end;

procedure TFGXControl.SetBounds(const Value: TRect);
begin
  //if FIsForm then
  //  TForm(FControl).SetBoundsF(Value.Left, Value.Top, Value.Width, Value.Height)
  //else if FControl is TControl then
  //  TControl(FControl).SetBounds(Value.Left, Value.Top, Value.Width, Value.Height);

  PlaceLabel;
end;

procedure TFGXControl.SetCaptionProperty(const ALayout: TLayout);
begin
  if not Assigned(FCaption) then
    Exit;

  if ALayout.Kind in [lkMemo, lkPanel, lkShape] then
  begin
    FShowCaption := ALayout.ShowCaption;
    //TLabel(FCaption).Visible := FShowCaption and (FOwner.View.State > vsHidden);
    //if TAnchorKind.akBottom in ALayout.Anchors then
    //  TLabel(FCaption).Anchors := [TAnchorKind.akLeft, TAnchorKind.akBottom]
    //else
    //  TLabel(FCaption).Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];

    if ALayout.Caption_AtLeft then
    begin
      FLabelPosition := lpLeft;
      PlaceLabel;
    end;
  end;
end;

procedure TFGXControl.SetControl(const AControl: TObject);
begin
  //if FControl is TControl then
  //begin
  //  TControl(FControl).OnEnter := nil;
  //  TControl(FControl).OnExit := nil;
  //end;

  inherited SetControl(AControl);

  //if FControl is TControl then
  //begin
  //  TControl(FControl).OnEnter := FOwner.OnEnter;
  //  TControl(FControl).OnExit := FOwner.OnExit;
  //end;
end;

procedure TFGXControl.SetFocused(const Value: Boolean);
begin
  //if not (FControl is TControl) then
  //  Exit;

  //if Value and TControl(FControl).CanFocus then
  //  TControl(FControl).SetFocus;
end;

procedure TFGXControl.SetLinkedControl(const ATargetName: string; const ALinkedControl: TNativeControl);
//var
//  vPopupMenu: TPopupMenu;
begin
  //if not Assigned(FControl) then
  //  Exit;

  if SameText(ATargetName, 'popup') then
  begin
  //  vPopupMenu := TPopupMenu(TFGXControl(ALinkedControl).Control);
  //  if not Assigned(vPopupMenu.OnPopup) then
  //    vPopupMenu.OnPopup := BeforeContextMenuShow;
  //  TControl(FControl).PopupMenu := vPopupMenu;
  end;
end;

procedure TFGXControl.SetModalResult(const AModalResult: TModalResult);
begin
  //if FControl is TForm then
  //  TForm(FControl).ModalResult := AModalResult;
end;

procedure TFGXControl.SetParent(const AParent: TUIArea);
begin
  inherited SetParent(AParent);

  //if not (FControl is TControl) then
  //  Exit;

  // Установка родителя для контрола
  //if FIsForm or not Assigned(AParent) then
  //  TControl(FControl).Parent := nil
  //else if not Assigned(TControl(FControl).Parent) then
  //begin
  //  try
  //    TControl(FControl).Parent := TFmxObject(GetRealControl(AParent));
  //  except
      // Fails after CloseAllPages
  //  end;
  //end;
end;

procedure TFGXControl.SetTabOrder(const ATabOrder: Integer);
begin
  //if not (FControl is TControl) then
  //  Exit;

  if ATabOrder >= 0 then
  begin
  //  TControl(FControl).TabStop := True;
  //  TControl(FControl).TabOrder := ATabOrder;
  end
  //else
  //  TControl(FControl).TabStop := False;
end;

procedure TFGXControl.SetViewState(const AViewState: TViewState);
begin
  if FIsForm then
    Exit;

  //if (FControl is TControl) and not (FControl is TAniIndicator) then
  //begin
  //  TControl(FControl).Visible := AViewState > vsHidden;
  //  TControl(FControl).Enabled := AViewState > vsDisabled;
  //end;
end;

procedure TFGXControl.SetWindowState(const AWindowState: TWindowState);
begin
  //if FControl is TForm then
  //  TForm(FControl).WindowState := AWindowState;
end;

procedure TFGXControl.UpdateCaptionVisibility;
begin
  //if Assigned(FCaption) then
  //  TLabel(FCaption).Visible := FShowCaption and (FOwner.View.State > vsHidden);
end;

initialization

end.
