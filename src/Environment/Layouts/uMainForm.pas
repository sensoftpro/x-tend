unit uMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.WinXCtrls, Vcl.Grids, Vcl.StdCtrls, Generics.Collections,
  Vcl.ComCtrls;

type
  TLayout = class
  private
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
  public
    constructor Create(const ALeft, ATop, AWidth, AHeight: Integer);

    property Left: Integer read FLeft;
    property Top: Integer read FTop;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;
  end;

  TGridSizeKind = (gskAbsolute, gskRelative);

  TGridSize = class
  private
    FKind: TGridSizeKind;
    FValue: Integer;
  public
    constructor Create(const AKind: TGridSizeKind; const AValue: Integer);

    property Kind: TGridSizeKind read FKind write FKind;
    property Value: Integer read FValue write FValue;
  end;

  TGridSizes = class(TObjectList<TGridSize>)
  end;

  TGridLayout = class(TLayout)
  private
    FColumns: TGridSizes;
    FRows: TGridSizes;
  public
    constructor Create(const ALeft, ATop, AWidth, AHeight: Integer);
    destructor Destroy; override;

    procedure AddColumn(const ASizeKind: TGridSizeKind; const AValue: Integer);
    procedure AddRow(const ASizeKind: TGridSizeKind; const AValue: Integer);

    property Columns: TGridSizes read FColumns;
    property Rows: TGridSizes read FRows;
  end;

  TfrMain = class(TForm)
    btnNewLayout: TButton;
    btnAddGrid: TButton;
    btnAddRow: TButton;
    btnAddColumn: TButton;
    ScrollBox1: TScrollBox;
    Label1: TLabel;
    cbxColumn: TComboBox;
    Label3: TLabel;
    edtColumn: TEdit;
    udnColumn: TUpDown;
    Label2: TLabel;
    cbxRow: TComboBox;
    Label4: TLabel;
    edtRow: TEdit;
    udnRow: TUpDown;
    btnApply: TButton;
    procedure btnNewLayoutClick(Sender: TObject);
    procedure btnAddGridClick(Sender: TObject);
    procedure btnAddRowClick(Sender: TObject);
    procedure btnAddColumnClick(Sender: TObject);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnApplyClick(Sender: TObject);
  private
    FPrevSizes: array of Integer;
    FPanel: TPanel;
    FGrid: TDrawGrid;
    FLayout: TGridLayout;
    FSelection: TGridRect;

    procedure ApplyLayout(const AGrid: TDrawGrid; const ALayout: TGridLayout);
    function SameSelections(const ASelection1, ASelection2: TGridRect): Boolean;
    procedure UpdateProperties;
  public
    { Public declarations }
  end;

var
  frMain: TfrMain;

implementation

{$R *.dfm}

procedure TfrMain.ApplyLayout(const AGrid: TDrawGrid; const ALayout: TGridLayout);
var
  vWidth, vHeight: Integer;
  vTotalRelSize: Integer;
  vTotalAbsSize: Integer;
  vSize: Integer;
  i: Integer;
begin
  vWidth := FPanel.ClientWidth - ALayout.Columns.Count - 2;
  vHeight := FPanel.ClientHeight - ALayout.Rows.Count - 2;

  AGrid.ColCount := ALayout.Columns.Count + 1;
  AGrid.RowCount := ALayout.Rows.Count + 1;

  vTotalRelSize := 0;
  vTotalAbsSize := vWidth;
  for i := 0 to ALayout.Columns.Count - 1 do
    if ALayout.Columns[i].Kind = gskRelative then
      vTotalRelSize := vTotalRelSize + ALayout.Columns[i].Value
    else
      vTotalAbsSize := vTotalAbsSize - ALayout.Columns[i].Value;

  for i := 0 to ALayout.Columns.Count - 1 do
    if ALayout.Columns[i].Kind = gskRelative then
    begin
      if (vTotalAbsSize <= 0) or (vTotalRelSize = 0) then
        AGrid.ColWidths[i + 1] := 0
      else begin
        vSize := Round((ALayout.Columns[i].Value / vTotalRelSize) * vTotalAbsSize);
        vTotalRelSize := vTotalRelSize - ALayout.Columns[i].Value;
        vTotalAbsSize := vTotalAbsSize - vSize;
        AGrid.ColWidths[i + 1] := vSize;
      end;
    end
    else
      AGrid.ColWidths[i + 1] := ALayout.Columns[i].Value;

  vTotalRelSize := 0;
  vTotalAbsSize := vHeight;
  for i := 0 to ALayout.Rows.Count - 1 do
    if ALayout.Rows[i].Kind = gskRelative then
      vTotalRelSize := vTotalRelSize + ALayout.Rows[i].Value
    else
      vTotalAbsSize := vTotalAbsSize - ALayout.Rows[i].Value;

  for i := 0 to ALayout.Rows.Count - 1 do
    if ALayout.Rows[i].Kind = gskRelative then
    begin
      if (vTotalAbsSize <= 0) or (vTotalRelSize = 0) then
        AGrid.RowHeights[i + 1] := 0
      else begin
        vSize := Round((ALayout.Rows[i].Value / vTotalRelSize) * vTotalAbsSize);
        vTotalRelSize := vTotalRelSize - ALayout.Rows[i].Value;
        vTotalAbsSize := vTotalAbsSize - vSize;
        AGrid.RowHeights[i + 1] := vSize;
      end;
    end
    else
      AGrid.RowHeights[i + 1] := ALayout.Rows[i].Value;
end;

procedure TfrMain.btnAddColumnClick(Sender: TObject);
begin
  FLayout.AddColumn(gskRelative, 20);
  ApplyLayout(FGrid, FLayout);
end;

procedure TfrMain.btnAddGridClick(Sender: TObject);
begin
  FGrid := TDrawGrid.Create(FPanel);
  FGrid.Parent := FPanel;
  FGrid.Align := alClient;
  FGrid.BorderStyle := bsNone;
  ApplyLayout(FGrid, FLayout);
  FGrid.DefaultColWidth := 0;
  FGrid.DefaultRowHeight := 0;
  FGrid.Options := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSizing, goColSizing, goRangeSelect];
  FGrid.OnMouseDown := GridMouseDown;
  FGrid.OnMouseUp := GridMouseUp;
  FGrid.OnKeyUp := GridKeyUp;
end;

procedure TfrMain.btnAddRowClick(Sender: TObject);
begin
  FLayout.AddRow(gskRelative, 20);
  ApplyLayout(FGrid, FLayout);
end;

procedure TfrMain.btnApplyClick(Sender: TObject);
begin
  if (FSelection.Left = FSelection.Right) and (FSelection.Left > 0) then
  begin
    if cbxColumn.ItemIndex > 0 then
      FLayout.Columns[FSelection.Left - 1].Kind := TGridSizeKind(cbxColumn.ItemIndex - 1);
    FLayout.Columns[FSelection.Left - 1].Value := udnColumn.Position;
  end;

  if (FSelection.Top = FSelection.Bottom) and (FSelection.Top > 0) then
  begin
    if cbxRow.ItemIndex > 0 then
      FLayout.Rows[FSelection.Top - 1].Kind := TGridSizeKind(cbxRow.ItemIndex - 1);
    FLayout.Rows[FSelection.Top - 1].Value := udnRow.Position;
  end;

  ApplyLayout(FGrid, FLayout);
end;

procedure TfrMain.btnNewLayoutClick(Sender: TObject);
begin
  FPanel := TPanel.Create(nil);
  FPanel.Parent := ScrollBox1;
  FPanel.Width := ScrollBox1.ClientWidth;
  FPanel.Height := ScrollBox1.ClientHeight;
  FPanel.ParentColor := False;
  FPanel.ParentBackground := False;
  FPanel.Color := clBtnFace;

  FLayout := TGridLayout.Create(0, 0, FPanel.ClientWidth, FPanel.ClientHeight);
end;

procedure TfrMain.GridKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if not SameSelections(FSelection, FGrid.Selection) then
  begin
    FSelection := FGrid.Selection;
    UpdateProperties;
    //ShowMessage(Format('Selection: %d, %d, %d, %d', [FSelection.Left, FSelection.Top, FSelection.Right, FSelection.Bottom]));
  end;
end;

function TfrMain.SameSelections(const ASelection1, ASelection2: TGridRect): Boolean;
begin
  Result := (ASelection1.Left = ASelection2.Left) and (ASelection1.Top = ASelection2.Top) and
    (ASelection1.Right = ASelection2.Right) and (ASelection1.Bottom = ASelection2.Bottom);
end;

procedure TfrMain.UpdateProperties;
begin
  udnColumn.Enabled := (FSelection.Left = FSelection.Right) and (FSelection.Left > 0);
  edtColumn.Enabled := udnColumn.Enabled;
  cbxColumn.Enabled := udnColumn.Enabled;

  udnRow.Enabled := (FSelection.Top = FSelection.Bottom) and (FSelection.Top > 0);
  edtRow.Enabled := udnRow.Enabled;
  cbxRow.Enabled := udnRow.Enabled;

  if udnColumn.Enabled then
  begin
    udnColumn.Position := FLayout.Columns[FSelection.Left - 1].Value;
    cbxColumn.ItemIndex := Integer(FLayout.Columns[FSelection.Left - 1].Kind) + 1;
  end
  else
    cbxColumn.ItemIndex := 0;

  if udnRow.Enabled then
  begin
    udnRow.Position := FLayout.Rows[FSelection.Top - 1].Value;
    cbxRow.ItemIndex := Integer(FLayout.Rows[FSelection.Top - 1].Kind) + 1;
  end
  else
    cbxRow.ItemIndex := 0;
end;

procedure TfrMain.GridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  if Length(FPrevSizes) <> FGrid.ColCount + FGrid.RowCount then
    SetLength(FPrevSizes, FGrid.ColCount + FGrid.RowCount);
  for i := 0 to FGrid.ColCount - 1 do
    FPrevSizes[i] := FGrid.ColWidths[i];
  for i := 0 to FGrid.RowCount - 1 do
    FPrevSizes[i + FGrid.ColCount] := FGrid.RowHeights[i];
end;

procedure TfrMain.GridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  vChanged: Boolean;
  i: Integer;
begin
  vChanged := Length(FPrevSizes) <> FGrid.ColCount + FGrid.RowCount;
  if not vChanged then
  begin
    for i := 0 to FGrid.ColCount - 1 do
      if FPrevSizes[i] <> FGrid.ColWidths[i] then
      begin
        vChanged := True;
        Break;
      end;
    if not vChanged then
      for i := 0 to FGrid.RowCount - 1 do
        if FPrevSizes[i + FGrid.ColCount] <> FGrid.RowHeights[i] then
        begin
          vChanged := True;
          Break;
        end;
  end;

  if vChanged then
  begin
    for i := 1 to FGrid.ColCount - 1 do
      FLayout.Columns[i-1].Value := FGrid.ColWidths[i];
    for i := 1 to FGrid.RowCount - 1 do
      FLayout.Rows[i-1].Value := FGrid.RowHeights[i];

    ApplyLayout(FGrid, FLayout);
  end;

  if not SameSelections(FSelection, FGrid.Selection) then
  begin
    FSelection := FGrid.Selection;
    UpdateProperties;
    //ShowMessage(Format('Selection: %d, %d, %d, %d', [FSelection.Left, FSelection.Top, FSelection.Right, FSelection.Bottom]));
  end;
end;

{ TGridLayout }

procedure TGridLayout.AddColumn(const ASizeKind: TGridSizeKind; const AValue: Integer);
var
  vColumn: TGridSize;
begin
  vColumn := TGridSize.Create(ASizeKind, AValue);
  FColumns.Add(vColumn);
end;

procedure TGridLayout.AddRow(const ASizeKind: TGridSizeKind; const AValue: Integer);
var
  vRow: TGridSize;
begin
  vRow := TGridSize.Create(ASizeKind, AValue);
  FRows.Add(vRow);
end;

constructor TGridLayout.Create(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited Create(ALeft, ATop, AWidth, AHeight);

  FColumns := TGridSizes.Create;
  FRows := TGridSizes.Create;

  AddColumn(gskRelative, 20);
  AddRow(gskRelative, 20);
end;

destructor TGridLayout.Destroy;
begin
  FreeAndNil(FColumns);
  FreeAndNil(FRows);
  inherited Destroy;
end;

{ TLayout }

constructor TLayout.Create(const ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited Create;
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
  FHeight := AHeight;
end;

{ TGridSize }

constructor TGridSize.Create(const AKind: TGridSizeKind; const AValue: Integer);
begin
  inherited Create;
  FKind := AKind;
  FValue := AValue;
end;

end.

