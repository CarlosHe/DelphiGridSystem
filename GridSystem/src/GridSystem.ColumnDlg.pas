unit GridSystem.ColumnDlg;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.TypInfo,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti, FMX.Grid.Style, FMX.StdCtrls, FMX.Edit, FMX.EditBox, FMX.SpinBox,
  FMX.ListBox, FMX.Grid, FMX.Controls.Presentation, FMX.ScrollBox, GridSystem.Types;

type

  TGridSystemColumnDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cboxScreenType: TComboBox;
    sboxColumnCount: TSpinBox;
    Button3: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Button4: TButton;
    strgridColumns: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FColumns: TGSColumnsDictionary;
    { Private declarations }
    procedure AddScreenTypeToComboBox;
    procedure SetColumnWidthToSpinBox;
    procedure BuildStrGrid;
    procedure SetColumns(const Value: TGSColumnsDictionary);
  public
    { Public declarations }
    property Columns: TGSColumnsDictionary read FColumns write SetColumns;
  end;

var
  GridSystemColumnDlg: TGridSystemColumnDlg;

implementation

{$R *.fmx}

procedure TGridSystemColumnDlg.AddScreenTypeToComboBox;
var
  I: Integer;
begin
  cboxScreenType.Clear;
  for I := Integer(Low(TScreenType)) to Integer(High(TScreenType)) do
    cboxScreenType.Items.Add(GetEnumName(TypeInfo(TScreenType), I));
  cboxScreenType.ItemIndex := 0;
end;

procedure TGridSystemColumnDlg.BuildStrGrid;
var
  Key: TScreenType;
  FRow: Integer;
  I: Integer;
begin
  FRow := 0;
  strgridColumns.RowCount := FColumns.Count;

  for Key in FColumns.Keys do
  begin
    strgridColumns.Cells[0, FRow] := GetEnumName(TypeInfo(TScreenType), Integer(Key));
    strgridColumns.Cells[1, FRow] := IntToStr(FColumns.Items[Key]);
    Inc(FRow);
  end;
end;

procedure TGridSystemColumnDlg.Button3Click(Sender: TObject);
begin
  if not FColumns.ContainsKey(TScreenType(GetEnumValue(TypeInfo(TScreenType), cboxScreenType.Items[cboxScreenType.ItemIndex]))) then
  begin
    FColumns.Add(TScreenType(GetEnumValue(TypeInfo(TScreenType), cboxScreenType.Items[cboxScreenType.ItemIndex])), Round(sboxColumnCount.Value));
    strgridColumns.RowCount := strgridColumns.RowCount + 1;
    strgridColumns.Cells[0, strgridColumns.RowCount - 1] := cboxScreenType.Items[cboxScreenType.ItemIndex];
    strgridColumns.Cells[1, strgridColumns.RowCount - 1] := Round(sboxColumnCount.Value).ToString;
  end;
end;

procedure TGridSystemColumnDlg.Button4Click(Sender: TObject);
begin
  if (strgridColumns.Selected >= 0) and (strgridColumns.Selected <= FColumns.Count) then
  begin
    FColumns.Remove(TScreenType(GetEnumValue(TypeInfo(TScreenType), strgridColumns.Cells[0, strgridColumns.Selected])));
    BuildStrGrid;
  end;
end;

procedure TGridSystemColumnDlg.FormCreate(Sender: TObject);
begin
  FColumns := TGSColumnsDictionary.Create;
  AddScreenTypeToComboBox;
  SetColumnWidthToSpinBox;
end;

procedure TGridSystemColumnDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FColumns);
end;

procedure TGridSystemColumnDlg.FormShow(Sender: TObject);
begin
  BuildStrGrid;
end;

procedure TGridSystemColumnDlg.SetColumns(const Value: TGSColumnsDictionary);
begin
  FColumns := Value;
end;

procedure TGridSystemColumnDlg.SetColumnWidthToSpinBox;
begin
  sboxColumnCount.Min := Low(GSColumnWidth);
  sboxColumnCount.Max := High(GSColumnWidth);
end;

end.
