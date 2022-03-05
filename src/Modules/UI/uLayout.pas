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

unit uLayout;

interface

uses
  Classes, Generics.Collections, UITypes;

type
  TLayoutKind = (lkPanel, lkPage, lkPages, lkFrame, lkMenu, lkMemo);
  TAlign = (alNone, alTop, alBottom, alLeft, alRight, alClient, alCustom);
  TPageStyle = (psTabs, psButtons, psFlatButtons);
  TPagePosition = (ppTop, ppBottom, ppLeft, ppRight);
  TBevelKind = (bkNone, bkLowered, bkRaised, bkSpace);

  TLayout = class
  private
    FControl: TObject;
    FItems: TObjectList<TLayout>;
    FLayoutKind: TLayoutKind;
    FName: string;
    FCaption: string;
    FImageIndex: Integer;
    FTag: Integer;

    FAlign: TAlign;
    FAlignment: TAlignment;
    FAnchors: TAnchors;
    FShowCaption: Boolean;

    FFontColor: TColor;
    FFontSize: Integer;

    FWidth: Integer;
    FHeight: Integer;
    FLeft: Integer;
    FTop: Integer;

    FPageStyle: TPageStyle;
    FPagePosition: TPagePosition;
    FPageHeight: Integer;
    FHidePages: Boolean;

    FBevelInner: TBevelKind;
    FBevelOuter: TBevelKind;

    procedure EnumerateControls(const AControl: TObject);
    procedure ExtractSettings(const AControl: TObject);
  public
    constructor Create(const AControl: TObject); overload;
    constructor Create(const ALayoutKind: TLayoutKind; const AParams: string = ''); overload;
    destructor Destroy; override;

    procedure Load(const AFileName: string);
    procedure Save(const AFileName: string);

    function ExtractCaption: string;
    procedure SetCaption(const AValue: string);

    property Caption: string read FCaption write FCaption;
    property ShowCaption: Boolean read FShowCaption;
    property Name: string read FName write FName;
    property ImageIndex: Integer read FImageIndex write FImageIndex;
    property Tag: Integer read FTag;
    property Align: TAlign read FAlign;
    property Alignment: TAlignment read FAlignment;
    property Anchors: TAnchors read FAnchors;
    property FontColor: TColor read FFontColor;
    property FontSize: Integer read FFontSize;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
    property Left: Integer read FLeft;
    property Top: Integer read FTop;
    property PageStyle: TPageStyle read FPageStyle;
    property PagePosition: TPagePosition read FPagePosition;
    property PageHeight: Integer read FPageHeight;
    property HidePages: Boolean read FHidePages;
    property BevelInner: TBevelKind read FBevelInner;
    property BevelOuter: TBevelKind read FBevelOuter;

    property LayoutKind: TLayoutKind read FLayoutKind;

    property Items: TObjectList<TLayout> read FItems;
  end;

implementation

uses
  IOUtils, TypInfo, SysUtils, {>> Windows} Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Menus, {Windows <<} uDefinition, uUtils;

{ TLayout }

constructor TLayout.Create(const AControl: TObject);
begin
  inherited Create;

  FControl := AControl;
  if FControl is TPanel then
    FLayoutKind := lkPanel
  else if FControl is TTabSheet then
    FLayoutKind := lkPage
  else if FControl is TPageControl then
    FLayoutKind := lkPages
  else if FControl is TMemo then
    FLayoutKind := lkMemo
  else
    FLayoutKind := lkFrame;

  FItems := TObjectList<TLayout>.Create;

  ExtractSettings(AControl);
  EnumerateControls(AControl);
end;

constructor TLayout.Create(const ALayoutKind: TLayoutKind; const AParams: string = '');
var
  vParams: TStrings;
begin
  inherited Create;

  FLayoutKind := ALayoutKind;
  FControl := nil;

  vParams := CreateDelimitedList(AParams);
  case ALayoutKind of
    lkPanel: begin
        FBevelInner := bkNone;
        FBevelOuter := bkNone;
      end;
    lkPage: begin
        FCaption := vParams.Values['Caption'];
        FImageIndex := StrToIntDef(vParams.Values['ImageIndex'], -1);
        FName := vParams.Values['Name'];
        FTag := 11;
      end;
  end;
  FreeAndNil(vParams);

  FItems := TObjectList<TLayout>.Create;
end;

destructor TLayout.Destroy;
begin
  FControl := nil;
  FreeAndNil(FItems);

  inherited Destroy;
end;

procedure TLayout.EnumerateControls(const AControl: TObject);
var
  vParentControl: TWinControl;
  i: Integer;
begin
  if not (AControl is TWinControl) then
    Exit;
  if TWinControl(AControl).ControlCount <= 0 then
    Exit;

  vParentControl := TWinControl(AControl);
  for i := 0 to vParentControl.ComponentCount - 1 do
    if vParentControl.Components[i] is TMenu then
      FItems.Add(TLayout.Create(vParentControl.Components[i]));

  for i := 0 to vParentControl.ControlCount - 1 do
    FItems.Add(TLayout.Create(vParentControl.Controls[i]));
end;

function TLayout.ExtractCaption: string;
begin
  case FLayoutKind of
    lkPages: Result := TPageControl(FControl).Hint;
    lkMemo:
      begin
        TMemo(FControl).WordWrap := False;
        TMemo(FControl).WantReturns := False;
        Result := TMemo(FControl).Lines.Text;
      end;
  else
    Result := TPanel(FControl).Caption;
  end;
end;

procedure TLayout.ExtractSettings(const AControl: TObject);
var
  vPanel: TPanel absolute AControl;
  vPC: TPageControl absolute AControl;
  vTab: TTabSheet absolute AControl;
begin
  FName := TControl(AControl).Name;
  FTag := TControl(AControl).Tag;

  if FLayoutKind = lkPanel then
  begin
    FAlign := TAlign(vPanel.Align);
    FAlignment := vPanel.Alignment;
    FAnchors := vPanel.Anchors;
    FFontColor := vPanel.Font.Color;
    FFontSize := vPanel.Font.Size;
    FWidth := vPanel.Width;
    FHeight := vPanel.Height;
    FLeft := vPanel.Left;
    FTop := vPanel.Top;
    FShowCaption := vPanel.ShowCaption;
    FBevelInner := TBevelKind(vPanel.BevelInner);
    FBevelOuter := TBevelKind(vPanel.BevelOuter);
  end
  else if FLayoutKind = lkPage then
  begin
    FCaption := vTab.Caption;
    FImageIndex := vTab.ImageIndex;
  end
  else if FLayoutKind = lkPages then
  begin
    FAlign := TAlign(vPC.Align);
    FAnchors := vPanel.Anchors;
    FFontColor := vPC.Font.Color;
    FFontSize := vPC.Font.Size;
    FWidth := vPC.Width;
    FHeight := vPC.Height;
    FPageStyle := TPageStyle(vPC.Style);
    FPagePosition := TPagePosition(vPC.TabPosition);
    FPageHeight := vPC.TabHeight;
    if (FPageStyle <> psTabs) and (vPC.PageCount > 0) then
    begin
      FLeft := vPC.Left + vPC.Pages[0].Left;
      FTop := vPC.Top + vPC.Pages[0].Top;
    end
    else begin
      FLeft := vPC.Left;
      FTop := vPC.Top;
    end;
    FHidePages := not vPC.ShowHint or ((vPC.PageCount > 0) and not vPC.Pages[0].TabVisible);
  end
  else if FLayoutKind = lkFrame then
  begin

  end
  else if FLayoutKind = lkMenu then
  begin

  end
  else if FLayoutKind = lkMemo then
  begin

  end;
end;

procedure TLayout.Load(const AFileName: string);
var
  vFileStream: TStream;
  vMemStream: TStream;
  vFrame: TComponent;
begin
  vFrame := TFrame.Create(nil);
  try
    vFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    vMemStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(vFileStream, vMemStream);

      vMemStream.Position := 0;
      vMemStream.ReadComponent(vFrame);
    finally
      FreeAndNil(vFileStream);
      FreeAndNil(vMemStream);
    end;

    EnumerateControls(vFrame);
  finally
    FreeAndNil(vFrame);
  end;
end;

procedure TLayout.Save(const AFileName: string);
begin

end;

procedure TLayout.SetCaption(const AValue: string);
begin
  TPanel(FControl).Caption := AValue;
end;

end.
