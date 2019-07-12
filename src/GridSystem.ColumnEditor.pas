unit GridSystem.ColumnEditor;

interface

uses
  FMX.Dialogs, System.SysUtils, System.Classes, System.UITypes, DesignEditors, DesignIntf, GridSystem.ColumnDlg, GridSystem.Types;

type
  TGridSystemColumnComponentEditor = class(TComponentEditor)
  private
    procedure ShowDesigner;
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  TGridSystemColumnPropertyEditor = class(TClassProperty)
  private
    procedure ShowDesigner;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

implementation

{ TGridSystemColumnComponentEditor }

procedure TGridSystemColumnComponentEditor.ExecuteVerb(Index: Integer);
begin
  inherited;
  case Index of
    0:
      ShowDesigner;
  else
    raise ENotImplemented.Create('TClockLabelEditor has only one verb (index = 0) supported.');
  end;
end;

function TGridSystemColumnComponentEditor.GetVerb(Index: Integer): string;
begin
  inherited;
  case Index of
    0:
      Result := '&Edit columns';
  else
    raise ENotImplemented.Create('TGridSystemColumnComponentEditor has only one verb (index = 0) supported.');
  end;
end;

function TGridSystemColumnComponentEditor.GetVerbCount: Integer;
begin
  inherited;
  Result := 1;
end;

procedure TGridSystemColumnComponentEditor.ShowDesigner;
var
  DesignerForm: TGridSystemColumnDlg;
  Key: TScreenType;
begin
  DesignerForm := TGridSystemColumnDlg.Create(nil);
  try
    if Supports(Component, IGSCol) then
    begin
      for Key in (Component as IGSCol).Columns.Keys do
      begin
        DesignerForm.Columns.Add(Key, (Component as IGSCol).Columns.Items[Key]);
      end;
    end;

    if DesignerForm.ShowModal = mrOk then
    begin
      (Component as IGSCol).Columns.Clear;
      for Key in DesignerForm.Columns.Keys do
      begin
        (Component as IGSCol).Columns.Add(Key, DesignerForm.Columns.Items[Key]);
      end;
    end;

    Designer.Modified;
  finally
    DesignerForm.Free;
  end;
end;

{ TGridSystemColumnPropertyEditor }

procedure TGridSystemColumnPropertyEditor.Edit;
begin
  inherited;
  ShowDesigner;
end;

function TGridSystemColumnPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paReadOnly];
end;

procedure TGridSystemColumnPropertyEditor.ShowDesigner;
var
  DesignerForm: TGridSystemColumnDlg;
  ColumnsDictionary: TGSColumnsDictionary;
  Key: TScreenType;
begin
  DesignerForm := TGridSystemColumnDlg.Create(nil);
  try
    ColumnsDictionary:=TGSColumnsDictionary(Self.GetOrdValue);

    for Key in TGSColumnsDictionary(Self.GetOrdValue).Keys do
    begin
      DesignerForm.Columns.Add(Key, ColumnsDictionary.Items[Key]);
    end;

    if DesignerForm.ShowModal = mrOk then
    begin
      ColumnsDictionary.Clear;
      for Key in DesignerForm.Columns.Keys do
      begin
        ColumnsDictionary.Add(Key, DesignerForm.Columns.Items[Key]);
      end;
    end;
    Designer.Modified;
  finally
    DesignerForm.Free;
  end;
end;

end.
