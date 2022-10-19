unit uBaseLayout;

interface

uses
  System.UITypes, System.SysUtils, System.Classes, Generics.Collections,
  uJSON, uUtils,
  Vcl.Controls, Vcl.Graphics;

type

  TCrackedWinControl = class(TWinControl)
  public
    property Font;
    property Caption;
    property PopupMenu;
    property Color;
    property ParentColor;
    property ParentBackground;

    property OnEnter;
    Property OnExit;
  end;
  TCrackedControl = class(TControl) end;

  TBaseLayout = class
  private
    [Weak] FParentLayout: TBaseLayout;

    FObject: TJSONObject;

    FName: string;
    FClass: string;
    FTag: Integer;

    FCaption: string;
    FHint: string;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FAlign: TAlign;
    FPadding: TPadding;
    FMargins: TMargins;
    FAlignWithMargins: Boolean;
    FAnchors: TAnchors;
    FEnabled: Boolean;
    FVisible: Boolean;
    FFont: TFont;
    FColor: TColor;

    FDoubleBuffered: Boolean;

    FPopupMenu: String;
    FMenuItems: TObjectList<TBaseLayout>;

    FChildrens: TObjectList<TBaseLayout>;

    function DoGetMenu(const AName: String): TBaseLayout;
    function GetMenu: TBaseLayout;
  public
    constructor Create; overload;
    constructor Create(AParent: TBaseLayout); overload;
    destructor Destroy; override;

    procedure Load(const AJSON: TJSONObject);

    function ExtractString(const AKey: string): string;
    function ExtractInteger(const AKey: string): Integer;
    function ExtractBoolean(const AKey: string): boolean;
    function ExtractColor(const AKey: string): Cardinal;
    function ExtractFloat(const AKey: string): Double;

    procedure StoreString(const AKey: string; AStringValue: string);
    procedure StoreInteger(const AKey: string; AIntegerValue: Integer);
    procedure StoreBoolean(const AKey: string; ABooleanValue: boolean);
    procedure StoreColor(const AKey: string; AColorValue: TAlphaColor);
    procedure StoreFloat(const AKey: string; AFloatValue: Real);

    function AddChild(AChild: TBaseLayout): Integer;

    property Childs: TObjectList<TBaseLayout> read FChildrens;

    // Component's properties
    property Name: string read FName write FName;
    property LayoutClass: string read FClass write FClass;
    property Tag: Integer read FTag write FTag;

    // Control's properties
    property Caption: String read FCaption write FCaption;
    property Hint: string read FHint write FHint;
    property Left: Integer read FLeft write FLeft;
    property Top: Integer read FTop write FTop;
    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property Align: TAlign read FAlign write FAlign;
    property Margins: TMargins read FMargins write FMargins;
    property AlignWithMargins: Boolean read FAlignWithMargins write FAlignWithMargins;
    property Anchors: TAnchors read FAnchors write FAnchors;
    property Font: TFont read FFont write FFont;
    property Color: TColor read FColor write FColor;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Visible: Boolean read FVisible write FVisible;

    // WinControl's properties
    property DoubleBuffered: Boolean read FDoubleBuffered write FDoubleBuffered;
    property Padding: TPadding read FPadding write FPadding;

    property Menu: TBaseLayout read GetMenu;
    property Items: TObjectList<TBaseLayout> read FMenuItems write FMenuItems;
  end;

function SubComponentToJSON(AComponent:TComponent):TJSONObject;
function ComponentToJSON(AComponent:TComponent):TJSONObject;

implementation

uses
  vcl.forms, vcl.ExtCtrls, vcl.comctrls, Vcl.menus, Vcl.Stdctrls, Vcl.Dialogs;

function SubComponentToJSON(AComponent:TComponent):TJSONObject;
var
  i: integer;
  vToolButton: TToolButton absolute AComponent;
  vMenuItem: TMenuItem absolute AComponent;
  vComponents: TJSONArray;
begin
  result := TJSONObject.Create;
  result.StoreString('Class', AComponent.ClassName);
  result.StoreString('Name', AComponent.Name);
  if AComponent is TToolButton then
  begin
    result.StoreInteger('Left', vToolButton.Left);
    result.StoreInteger('Top', vToolButton.Top);
    result.StoreString('Caption', vToolButton.Caption);
    result.StoreInteger('ImageIndex', vToolButton.ImageIndex);
    result.StoreBoolean('Wrap', vToolButton.Wrap);
  end
  else if AComponent is TMenuItem then
  begin
    result.StoreString('Caption', vMenuItem.Caption);
    Result.StoreBoolean('RadioItem', vMenuItem.RadioItem);
    Result.StoreInteger('GroupIndex', vMenuItem.GroupIndex);
    if vMenuItem.Count > 0 then
    begin
      vComponents := TJSONArray.Create;
      for i := 0 to vMenuItem.Count - 1 do
        vComponents.AddElement(SubComponentToJSON(vMenuItem[i]));
      result.AddPair('Childs', vComponents);
    end;
  end;
end;

function ComponentToJSON(AComponent:TComponent):TJsonObject;
var
  i: integer;
  vFrame: TFrame absolute AComponent;
  vPanel: TPanel absolute AComponent;
  vToolBar: TToolBar absolute AComponent;
  vLabel: TLabel absolute AComponent;
  vMainMenu: TMainMenu absolute AComponent;
  vPopupMenu: TPopupMenu absolute AComponent;
  vBevel: TBevel absolute AComponent;
  vPageControl: TPageControl absolute AComponent;
  vTabSheet: TTabSheet Absolute AComponent;
  vSplitter: TSplitter absolute AComponent;
  vComponents: TJSONArray;
begin
  Result := TJSONObject.Create;
  Result.StoreString('Class', AComponent.ClassName);
  Result.StoreString('Name', AComponent.Name);
  Result.StoreBoolean('IsControl', AComponent is TControl);
  Result.StoreBoolean('IsWinControl', AComponent is TWinControl);
  Result.StoreInteger('Tag', AComponent.Tag);

  if (AComponent is TControl)then
  begin
    Result.StoreString('Caption', TCrackedControl(AComponent).Caption);
    Result.StoreString('Hint', TControl(AComponent).Hint);
    Result.StoreInteger('Left', TControl(AComponent).Left);
    Result.StoreInteger('Top', TControl(AComponent).Top);
    Result.StoreInteger('Width', TControl(AComponent).Width);
    Result.StoreInteger('Height', TControl(AComponent).Height);
    Result.StoreInteger('Align', Ord(TControl(AComponent).Align));
    Result.StoreInteger('Margins.Left', TControl(AComponent).Margins.Left);
    Result.StoreInteger('Margins.Right', TControl(AComponent).Margins.Right);
    Result.StoreInteger('Margins.Top', TControl(AComponent).Margins.Top);
    Result.StoreInteger('Margins.Bottom', TControl(AComponent).Margins.Bottom);
    Result.StoreBoolean('AlignWithMargins', TControl(AComponent).AlignWithMargins);
    Result.StoreBoolean('Anchors.akLeft', akLeft in TControl(AComponent).Anchors);
    Result.StoreBoolean('Anchors.akTop', akTop in TControl(AComponent).Anchors);
    Result.StoreBoolean('Anchors.akRight', akRight in TControl(AComponent).Anchors);
    Result.StoreBoolean('Anchors.akBottom', akBottom in TControl(AComponent).Anchors);
    Result.StoreBoolean('Enabled', TControl(AComponent).Enabled);
    Result.StoreBoolean('Visible', TControl(AComponent).Visible);
    Result.StoreColor('Font.Color', TCrackedControl(AComponent).Font.Color);
    Result.StoreInteger('Font.Size', TCrackedControl(AComponent).Font.Size);
    Result.StoreBoolean('Font.Style.fsBold', fsBold in TCrackedControl(AComponent).Font.Style);
    Result.StoreBoolean('Font.Style.fsItalic', fsItalic in TCrackedControl(AComponent).Font.Style);
    Result.StoreBoolean('Font.Style.fsUnderline', fsUnderline in TCrackedControl(AComponent).Font.Style);
    Result.StoreBoolean('Font.Style.fsStrikeOut', fsStrikeOut in TCrackedControl(AComponent).Font.Style);
    Result.StoreColor('Color', TCrackedControl(AComponent).Color);
    if Assigned(TCrackedControl(AComponent).PopupMenu) then
      Result.StoreString('PopupMenu', TCrackedControl(AComponent).PopupMenu.Name);
  end;

  if (AComponent is TWinControl)then
  begin
    Result.StoreBoolean('DoubleBuffered', TWinControl(AComponent).DoubleBuffered);
    Result.StoreInteger('Padding.Left', TWinControl(AComponent).Padding.Left);
    Result.StoreInteger('Padding.Right', TWinControl(AComponent).Padding.Right);
    Result.StoreInteger('Padding.Top', TWinControl(AComponent).Padding.Top);
    Result.StoreInteger('Padding.Bottom', TWinControl(AComponent).Padding.Bottom);
  end;

  if (AComponent is TPageControl) or (AComponent is TTabSheet) then
  begin
    vComponents := TJSONArray.create;
    for i := 0 to TWinControl(AComponent).ControlCount - 1 do
        vComponents.AddElement(ComponentToJSON(TWinControl(AComponent).Controls[i]));
    Result.AddPair('Childs', vComponents);
  end;

  if AComponent is TMemo then
  begin
    Result.StoreString('Lines.Text', TMemo(AComponent).Lines.Text);
  end
  else if AComponent is TFrame then
  begin
    Result.StoreInteger('TabOrder', vFrame.TabOrder);
    Result.StoreInteger('ClientWidth', vFrame.ClientWidth);
    Result.StoreInteger('ClientHeight', vFrame.ClientHeight);
    Result.StoreInteger('MinWidth', vFrame.Constraints.MinWidth);
    Result.StoreInteger('MinHeight', vFrame.Constraints.MinHeight);
    Result.StoreColor('Color', vFrame.Color);
    vComponents := TJSONArray.create;
    for i := 0 to AComponent.ComponentCount - 1 do
      if (AComponent.Components[i] is TMenu) then
        vComponents.AddElement(ComponentToJSON(AComponent.Components[i]));
    for i := 0 to TWinControl(AComponent).ControlCount - 1 do
        vComponents.AddElement(ComponentToJSON(TWinControl(AComponent).Controls[i]));
    Result.AddPair('Childs', vComponents);
  end
  else if AComponent is TPanel then
  begin
    Result.StoreInteger('BevelOuter', Ord(vPanel.BevelOuter));
    Result.StoreInteger('BevelInner', Ord(vPanel.BevelInner));
    Result.StoreInteger('BevelKind', Ord(vPanel.BevelKind));
    Result.StoreBoolean('ParentBackground', vPanel.ParentBackground);
    Result.StoreInteger('Alignment', ord(vPanel.Alignment));
    Result.StoreBoolean('ShowCaption', vPanel.ShowCaption);
    Result.StoreBoolean('ShowHint', vPanel.ShowHint);
    vComponents := TJSONArray.create;
    for i := 0 to TWinControl(AComponent).ControlCount - 1 do
        vComponents.AddElement(ComponentToJSON(TWinControl(AComponent).Controls[i]));
    Result.AddPair('Childs', vComponents);
  end
  else if AComponent is TImage then
  begin
    Result.StoreBoolean('Stretch', TImage(AComponent).Stretch);
    Result.StoreBoolean('Proportional', TImage(AComponent).Proportional);
    Result.StoreString('Picture', PictureGraphicToString(TImage(AComponent).Picture.Graphic));
  end
  else if AComponent is TToolBar then
  begin
    Result.StoreBoolean('AutoSize', vToolBar.AutoSize);
    Result.StoreInteger('ButtonHeight', vToolBar.ButtonHeight);
    Result.StoreInteger('ButtonWidth', vToolBar.ButtonWidth);
    Result.StoreBoolean('ParentFont', vToolBar.ParentFont);
    Result.StoreBoolean('ShowCaption', vToolBar.ShowCaptions);
    Result.StoreInteger('TabOrder', vToolBar.TabOrder);
    vComponents := TJSONArray.create;
    for i := 0 to vToolBar.ButtonCount - 1 do
        vComponents.AddElement(SubComponentToJSON(vToolBar.Buttons[i]));
    Result.AddPair('Childs', vComponents);
  end
  else if AComponent is TPageControl then
  begin
    Result.StoreInteger('TabPosition', ord(vPageControl.TabPosition));
    Result.StoreInteger('TabHeight', vPageControl.TabHeight);
    Result.StoreInteger('TabWidth', vPageControl.TabWidth);
    Result.StoreInteger('Style', Ord(vPageControl.Style));
    Result.StoreBoolean('ShowHint', vPageControl.ShowHint);
  end
  else if AComponent is TTabSheet then
  begin
    Result.StoreInteger('ImageIndex', vTabSheet.ImageIndex);
    Result.StoreBoolean('TabVisible', vTabSheet.TabVisible);
  end
  else if (AComponent is TMenu) then
  begin
      vComponents := TJSONArray.Create;
      for i := 0 to TMenu(AComponent).Items.Count - 1 do
        vComponents.AddElement(SubComponentToJSON(TMenu(AComponent).items[i]));
      result.AddPair('Childs', vComponents);
  end
  else if AComponent is TLabel then
  begin
    Result.StoreBoolean('Transparent', vLabel.Transparent);
    Result.StoreBoolean('AutoSize', vLabel.AutoSize);
    Result.StoreBoolean('WordWrap', vLabel.WordWrap);
  end
  else if AComponent is TBevel then
  begin
    Result.StoreInteger('Shape', Ord(vBevel.Shape));
    Result.StoreInteger('Style', Ord(vBevel.Style));
  end
  else if AComponent is TScrollBox then
  begin
    Result.StoreInteger('BorderStyle', Ord(TScrollBox(AComponent).BorderStyle));
  end
  else if AComponent is TSplitter then
    Result.StoreInteger('Cursor', vSplitter.Cursor)
  else if AComponent is TShape then
  begin
    Result.StoreColor('Pen.Color', TShape(AComponent).Pen.Color);
    Result.StoreColor('Brush.Color', TShape(AComponent).Brush.Color);
  end;


end;

{ TBaseLayout }

constructor TBaseLayout.Create;
begin
  inherited;

  FObject := TJSONObject.Create;

  FName := '';
  FClass := '';
  FTag := 0;

  FCaption := '';
  FHint := '';
  FLeft := 0;
  FTop := 0;
  FWidth := 0;
  FHeight := 0;
  FAlign := alNone;
  FPadding := TPadding.Create(nil);
  FMargins := TMargins.Create(nil);
  FAlignWithMargins := False;
  FAnchors := [];
  FEnabled := True;
  FVisible := True;
  FFont := TFont.Create;
  FFont.Style := [];
  FColor := clBtnFace;

  FDoubleBuffered := False;

  FPopupMenu := '';
  FMenuItems := nil;

  FParentLayout := nil;
  FChildrens := TObjectList<TBaseLayout>.Create;
end;

constructor TBaseLayout.Create(AParent: TBaseLayout);
begin
  Create;
  FParentLayout := AParent;
end;

destructor TBaseLayout.Destroy;
begin
  FreeAndNil(FChildrens);
  FreeAndNil(FObject);
  FreeAndNil(FMenuItems);
  inherited;
end;

procedure TBaseLayout.Load(const AJSON: TJSONObject);
var
  vChilds: TJSONArray;
  vChild: TBaseLayout;
  i: integer;
begin
  FObject := AJSON;

  FClass := FObject.ExtractString('Class');
  FName := FObject.ExtractString('Name');
  FTag := FObject.ExtractInteger('Tag');

  if FObject.ExtractBoolean('IsControl') then
  begin
    FCaption := FObject.ExtractString('Caption');
    FHint := FObject.ExtractString('Hint');
    FLeft := FObject.ExtractInteger('Left');
    FTop := FObject.ExtractInteger('Top');
    FWidth := FObject.ExtractInteger('Width');
    FHeight := FObject.ExtractInteger('Height');
    FAlign := TAlign(FObject.ExtractInteger('Align'));
    FMargins.Left := FObject.ExtractInteger('Margins.Left');
    FMargins.Top := FObject.ExtractInteger('Margins.Top');
    FMargins.Right := FObject.ExtractInteger('Margins.Right');
    FMargins.Bottom := FObject.ExtractInteger('Margins.Bottom');
    FAlignWithMargins := FObject.ExtractBoolean('AlignWithMargins');
    if FObject.ExtractBoolean('Anchors.akLeft') then
      Include(FAnchors, akLeft);
    if FObject.ExtractBoolean('Anchors.akTop') then
      Include(FAnchors, akTop);
    if FObject.ExtractBoolean('Anchors.akRight') then
      Include(FAnchors, akRight);
    if FObject.ExtractBoolean('Anchors.akBottom') then
      Include(FAnchors, akBottom);
    FEnabled := FObject.ExtractBoolean('Enabled');
    FVisible := FObject.ExtractBoolean('Visible');
    FFont.Color := FObject.ExtractColor('Font.Color');
    FFont.Size := FObject.ExtractInteger('Font.Size');
    if FObject.ExtractBoolean('Font.Style.fsBold') then
      FFont.Style := FFont.Style + [fsBold];
    if FObject.ExtractBoolean('Font.Style.fsItalic') then
      FFont.Style := FFont.Style + [fsItalic];
    if FObject.ExtractBoolean('Font.Style.fsUnderline') then
      FFont.Style := FFont.Style + [fsUnderline];
    if FObject.ExtractBoolean('Font.Style.fsStrikeOut') then
      FFont.Style := FFont.Style + [fsStrikeOut];
    FColor := FObject.ExtractColor('Color');
    FPopupMenu := FObject.ExtractString('PopupMenu');
  end;

  if FObject.ExtractBoolean('IsWinControl') then
  begin
    FDoubleBuffered := FObject.ExtractBoolean('DoubleBuffered');
    FPadding.Left := FObject.ExtractInteger('Padding.Left');
    FPadding.Top := FObject.ExtractInteger('Padding.Top');
    FPadding.Right := FObject.ExtractInteger('Padding.Right');
    FPadding.Bottom := FObject.ExtractInteger('Padding.Bottom');
  end;

  if (FClass = 'TPopupMenu') or (FClass = 'TMainMenu') or (FClass = 'TMenuItem') then
    FMenuItems := TObjectList<TBaseLayout>.Create;

  if FClass = 'TMenuItem' then
    FCaption := FObject.ExtractString('Caption');

  try
    vChilds := FObject.ExtractArray('Childs');
    if not Assigned(vChilds) then
      exit;
    FObject.RemovePair('Childs');
    for i := 0 to vChilds.Size - 1 do
    begin
      vChild := TBaseLayout.Create(self);
      vChild.load(TJSONObject(vChilds.Get(i)));
      vChild.FParentLayout := Self;
      if (FClass = 'TPopupMenu') or (FClass = 'TMainMenu') or (FClass = 'TMenuItem') then
        FMenuItems.Add(vChild)
      else
        FChildrens.Add(vChild);
    end;
  finally
    //FreeAndNil(vChilds);
  end;
end;


function TBaseLayout.ExtractString(const AKey: string): string;
begin
  result := FObject.ExtractString(AKey);
end;

function TBaseLayout.ExtractInteger(const AKey: string): integer;
begin
  result := FObject.ExtractInteger(AKey);
end;

function TBaseLayout.ExtractBoolean(const AKey: string): boolean;
begin
  result := FObject.ExtractBoolean(AKey);
end;

function TBaseLayout.ExtractColor(const AKey: string): Cardinal;
begin
  result := FObject.ExtractColor(AKey);
end;

function TBaseLayout.ExtractFloat(const AKey: string): Double;
begin
  result := FObject.ExtractFloat(AKey);
end;

procedure TBaseLayout.StoreString(const Akey: string; AStringValue: string);
begin
  FObject.StoreString(AKey, AStringValue);
end;

procedure TBaseLayout.StoreInteger(const Akey: string; AIntegerValue: Integer);
begin
  FObject.StoreInteger(AKey, AIntegerValue);
end;

procedure TBaseLayout.StoreBoolean(const Akey: string; ABooleanValue: Boolean);
begin
  FObject.StoreBoolean(AKey, ABooleanValue);
end;

procedure TBaseLayout.StoreColor(const Akey: String; AColorValue: TAlphaColor);
begin
  FObject.StoreColor(AKey, AColorValue);
end;

procedure TBaseLayout.StoreFloat(const Akey: String; AFloatValue: Real);
begin
  FObject.StoreFloat(AKey, AFloatValue);
end;

function TBaseLayout.AddChild(AChild: TBaseLayout): integer;
begin
  AChild.FParentLayout := Self;
  Result := FChildrens.Add(AChild);
end;

function TBaseLayout.DoGetMenu(const AName: String): TBaseLayout;
var
  vLayout: TBaseLayout;
begin
  Result := nil;
  if Assigned(FParentLayout) then
    Result := FParentLayout.DoGetMenu(AName)
  else
    for vLayout in FChildrens do
      if (vLayout.LayoutClass = 'TPopupMenu') and (vLayout.Name = AName) then
        Result := vLayout;
end;

function TBaseLayout.GetMenu: TBaseLayout;
begin
  Result := DoGetMenu(FPopupMenu);
end;

end.
