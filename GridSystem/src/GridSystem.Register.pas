unit GridSystem.Register;

interface

uses System.Classes, DesignIntf, GridSystem, GridSystem.Types, GridSystem.ColumnEditor;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Grid System', [TGSContainer]);
  RegisterComponents('Grid System', [TGSRow]);
  RegisterComponents('Grid System', [TGSCol]);
  RegisterComponentEditor(TGSCol, TGridSystemColumnComponentEditor);
  RegisterPropertyEditor(TypeInfo(TGSColumnsDictionary), TGSCol, 'Columns', TGridSystemColumnPropertyEditor);
end;

end.
