unit GridSystem;

interface

uses
  FMX.Dialogs, System.SysUtils, System.Types, FMX.Graphics, System.Classes, System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Layouts,
  GridSystem.Types,
  System.TypInfo;

type

  TGSContainer = class(TLayout, IGSContainer)
  private
    FMinHeight: Single;
    FAutoHeight: Boolean;
    FFill: TBrush;
    FSides: TSides;
    FXRadius: Single;
    FYRadius: Single;
    FCorners: TCorners;
    FCornerType: TCornerType;
    function IsSidesStored: Boolean;
    procedure SetMinHeight(const Value: Single);
    function GetMinHeight: Single;
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetFill(const Value: TBrush);
    procedure SetSides(const Value: TSides);
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
    procedure SetCorners(const Value: TCorners);
    procedure SetCornerType(const Value: TCornerType);
    { private declarations }
  protected
    { protected declarations }
    procedure Paint; override;
    function CalcNewHeight: Single;
    function GetScreenType: TScreenType;
    procedure DoRealign; override;
    procedure Resize; override;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { published declarations }
    property ScreenType: TScreenType read GetScreenType;
    property MinHeight: Single read GetMinHeight write FMinHeight;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight;
    property Fill: TBrush read FFill write SetFill;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property Corners: TCorners read FCorners write SetCorners;
    property CornerType: TCornerType read FCornerType write SetCornerType default TCornerType.Round;
  end;

  TGSRow = class(TFlowLayout, IGSRow)
  private
    { private declarations }
    FMinHeight: Single;
    FAutoHeight: Boolean;
    FFill: TBrush;
    FCornerType: TCornerType;
    FCorners: TCorners;
    FSides: TSides;
    FXRadius: Single;
    FYRadius: Single;
    function IsSidesStored: Boolean;
    procedure SetMinHeight(const Value: Single);
    function GetMinHeight: Single;
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetFill(const Value: TBrush);
    procedure SetCorners(const Value: TCorners);
    procedure SetCornerType(const Value: TCornerType);
    procedure SetSides(const Value: TSides);
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
  protected
    { protected declarations }
    procedure Paint; override;
    function CalcNewHeight: Single;
    procedure DoRealign; override;
    procedure Resize; override;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { published declarations }
    property MinHeight: Single read GetMinHeight write FMinHeight;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight;
    property Fill: TBrush read FFill write SetFill;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property Corners: TCorners read FCorners write SetCorners;
    property CornerType: TCornerType read FCornerType write SetCornerType default TCornerType.Round;
  end;

  TGSCol = class(TLayout, IGSCol)
  private
    { private declarations }
    FColumns: TGSColumnsDictionary;
    FOnColumnsChanged: TNotifyEvent;
    FMinHeight: Single;
    FAutoHeight: Boolean;
    FFill: TBrush;
    FCornerType: TCornerType;
    FCorners: TCorners;
    FSides: TSides;
    FXRadius: Single;
    FYRadius: Single;
    function IsSidesStored: Boolean;
    procedure SetColumns(const Value: TGSColumnsDictionary);
    function GetColumns: TGSColumnsDictionary;
    procedure DoValueNotify(Sender: TObject; const Item: GSColumnWidth; Action: TCollectionNotification);
    function GetMinHeight: Single;
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetFill(const Value: TBrush);
    procedure SetCorners(const Value: TCorners);
    procedure SetCornerType(const Value: TCornerType);
    procedure SetSides(const Value: TSides);
    procedure SetXRadius(const Value: Single);
    procedure SetYRadius(const Value: Single);
  protected
    { protected declarations }
    procedure Paint; override;
    procedure SetWidthByColumn;
    function CalcNewHeight: Single;
    procedure DoRealign; override;
    procedure Resize; override;
    procedure LoadCompProperty(Stream: TStream);
    procedure StoreCompProperty(Stream: TStream);
    procedure DefineProperties(Filer: TFiler); override;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    { published declarations }
    property Columns: TGSColumnsDictionary read GetColumns write SetColumns;
    property MinHeight: Single read GetMinHeight write FMinHeight;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight;
    property Fill: TBrush read FFill write SetFill;
    property Sides: TSides read FSides write SetSides stored IsSidesStored;
    property XRadius: Single read FXRadius write SetXRadius;
    property YRadius: Single read FYRadius write SetYRadius;
    property Corners: TCorners read FCorners write SetCorners;
    property CornerType: TCornerType read FCornerType write SetCornerType default TCornerType.Round;
  end;

implementation

{ TGSGrid }

function TGSContainer.CalcNewHeight: Single;
  function MaxBottomControl(Control: TControl): Single;
  begin
    if Control.Visible then
      Result := Control.Position.Y + Control.Height + Control.Margins.Bottom + TControl(Control.Parent).Padding.Bottom
    else
      Result := 0;
  end;

var
  I: Integer;
  NewHeight: Single;
begin
  NewHeight := FMinHeight;
  for I := 0 to ControlsCount - 1 do
  begin
    if MaxBottomControl(Controls[I]) > NewHeight then
      NewHeight := MaxBottomControl(Controls[I]);
  end;
  Result := NewHeight;
end;

constructor TGSContainer.Create(AOwner: TComponent);
begin
  inherited;
  CanParentFocus := True;
  HitTest := False;
  FMinHeight := 50;
  FFill := TBrush.Create(TBrushKind.None, $FFFFFFFF);
end;

destructor TGSContainer.Destroy;
begin
  FreeAndNil(FFill);
  inherited;
end;

procedure TGSContainer.DoRealign;
begin
  inherited;
  if FAutoHeight then
    Self.Height := CalcNewHeight;
end;

function TGSContainer.GetMinHeight: Single;
begin
  Result := FMinHeight;
end;

function TGSContainer.GetScreenType: TScreenType;
begin
  Result := TScreenType.ScreenExtraSmall;

  if Width >= 576 then
    Result := TScreenType.ScreenSmall;

  if Width >= 768 then
    Result := TScreenType.ScreenMedium;

  if Width >= 992 then
    Result := TScreenType.ScreenLarge;

  if Width >= 1200 then
    Result := TScreenType.ScreenExtraLarge;
end;

function TGSContainer.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

procedure TGSContainer.Paint;
var
  LShapeRect: TRectF;
  Off: Single;

begin
  inherited;
  try
    LShapeRect := TRectF.Create(0, 0, Width, Height);
    if Sides <> AllSides then
    begin
      Off := LShapeRect.Left;
      if not(TSide.Top in FSides) then
        LShapeRect.Top := LShapeRect.Top - Off;
      if not(TSide.Left in FSides) then
        LShapeRect.Left := LShapeRect.Left - Off;
      if not(TSide.Bottom in FSides) then
        LShapeRect.Bottom := LShapeRect.Bottom + Off;
      if not(TSide.Right in FSides) then
        LShapeRect.Right := LShapeRect.Right + Off;
      Canvas.FillRect(LShapeRect, XRadius, YRadius, FCorners, AbsoluteOpacity, FFill, CornerType);
    end
    else
    begin
      Canvas.FillRect(LShapeRect, XRadius, YRadius, FCorners, AbsoluteOpacity, FFill, CornerType);
    end;
  finally

  end;
end;

procedure TGSContainer.Resize;
begin
  inherited;
  Realign;
end;

procedure TGSContainer.SetAutoHeight(const Value: Boolean);
begin
  FAutoHeight := Value;
end;

procedure TGSContainer.SetCorners(const Value: TCorners);
begin
  FCorners := Value;
end;

procedure TGSContainer.SetCornerType(const Value: TCornerType);
begin
  FCornerType := Value;
end;

procedure TGSContainer.SetFill(const Value: TBrush);
begin
  FFill := Value;
end;

procedure TGSContainer.SetMinHeight(const Value: Single);
begin
  FMinHeight := Value;
end;

procedure TGSContainer.SetSides(const Value: TSides);
begin
  FSides := Value;
end;

procedure TGSContainer.SetXRadius(const Value: Single);
begin
  FXRadius := Value;
end;

procedure TGSContainer.SetYRadius(const Value: Single);
begin
  FYRadius := Value;
end;

{ TGSRow }

function TGSRow.CalcNewHeight: Single;
  function MaxBottomControl(Control: TControl): Single;
  begin
    if Control.Visible then
      Result := Control.Position.Y + Control.Height + Control.Margins.Bottom + TControl(Control.Parent).Padding.Bottom
    else
      Result := 0;
  end;

var
  I: Integer;
  NewHeight: Single;
begin
  NewHeight := FMinHeight;

  for I := 0 to ControlsCount - 1 do
  begin
    if MaxBottomControl(Controls[I]) > NewHeight then
      NewHeight := MaxBottomControl(Controls[I]);
  end;
  Result := NewHeight;
end;

constructor TGSRow.Create(AOwner: TComponent);
begin
  inherited;
  CanParentFocus := True;
  HitTest := False;
  FMinHeight := 50;
  FFill := TBrush.Create(TBrushKind.None, $FFFFFFFF);
end;

destructor TGSRow.Destroy;
begin
  FreeAndNil(FFill);
  inherited;
end;

procedure TGSRow.DoRealign;
begin
  inherited;
  if FAutoHeight then
    Height := CalcNewHeight;
end;

function TGSRow.GetMinHeight: Single;
begin
  Result := FMinHeight
end;

function TGSRow.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

procedure TGSRow.Paint;
var
  LShapeRect: TRectF;
  Off: Single;
begin
  inherited;
  try
    LShapeRect := TRectF.Create(0, 0, Width, Height);
    if Sides <> AllSides then
    begin
      Off := LShapeRect.Left;
      if not(TSide.Top in FSides) then
        LShapeRect.Top := LShapeRect.Top - Off;
      if not(TSide.Left in FSides) then
        LShapeRect.Left := LShapeRect.Left - Off;
      if not(TSide.Bottom in FSides) then
        LShapeRect.Bottom := LShapeRect.Bottom + Off;
      if not(TSide.Right in FSides) then
        LShapeRect.Right := LShapeRect.Right + Off;
      Canvas.FillRect(LShapeRect, XRadius, YRadius, FCorners, AbsoluteOpacity, FFill, CornerType);
    end
    else
    begin
      Canvas.FillRect(LShapeRect, XRadius, YRadius, FCorners, AbsoluteOpacity, FFill, CornerType);
    end;
  finally

  end;
end;

procedure TGSRow.Resize;
var
  I: Integer;
begin
  inherited;
  for I := 0 to ControlsCount - 1 do
  begin
    if Supports(Controls[I], IGSCol) then
    begin
      TGSCol(Controls[I]).Realign;
    end;
  end;
  Realign;
end;

procedure TGSRow.SetAutoHeight(const Value: Boolean);
begin
  FAutoHeight := Value;
end;

procedure TGSRow.SetCorners(const Value: TCorners);
begin
  FCorners := Value;
end;

procedure TGSRow.SetCornerType(const Value: TCornerType);
begin
  FCornerType := Value;
end;

procedure TGSRow.SetFill(const Value: TBrush);
begin
  FFill := Value;
end;

procedure TGSRow.SetMinHeight(const Value: Single);
begin
  FMinHeight := Value;
end;

procedure TGSRow.SetSides(const Value: TSides);
begin
  FSides := Value;
end;

procedure TGSRow.SetXRadius(const Value: Single);
begin
  FXRadius := Value;
end;

procedure TGSRow.SetYRadius(const Value: Single);
begin
  FYRadius := Value;
end;

{ TGSCol }

function TGSCol.CalcNewHeight: Single;
  function MaxBottomControl(Control: TControl): Single;
  begin
    if Control.Visible then
      Result := Control.Position.Y + Control.Height + Control.Margins.Bottom + TControl(Control.Parent).Padding.Bottom
    else
      Result := 0;
  end;

var
  I: Integer;
  NewHeight: Single;
begin
  NewHeight := FMinHeight;

  for I := 0 to ControlsCount - 1 do
  begin
    if MaxBottomControl(Controls[I]) > NewHeight then
      NewHeight := MaxBottomControl(Controls[I]);
  end;
  Result := NewHeight;
end;

constructor TGSCol.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := TGSColumnsDictionary.Create;
  FColumns.OnValueNotify := DoValueNotify;
  FMinHeight := 50;
  FFill := TBrush.Create(TBrushKind.None, $FFFFFFFF);
end;

procedure TGSCol.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  begin
    if Filer.Ancestor <> nil then
    begin
      Result := True;
      if Filer.Ancestor is TGSCol then
      begin
        Result := True; // not Equals(TGSCol(Filer.Ancestor));
      end
    end
    else
      Result := Columns.Count > 0;
  end;

begin
  inherited;
  Filer.DefineBinaryProperty('DataColumns', LoadCompProperty, StoreCompProperty, DoWrite);
end;

destructor TGSCol.Destroy;
begin
  FColumns.Clear;
  FColumns.Free;
  FreeAndNil(FFill);
  inherited;
end;

procedure TGSCol.DoRealign;
begin
  inherited;
  SetWidthByColumn;
  if FAutoHeight then
    Height := CalcNewHeight;
end;

procedure TGSCol.DoValueNotify(Sender: TObject; const Item: GSColumnWidth; Action: TCollectionNotification);
begin

  Realign;
end;

function TGSCol.GetColumns: TGSColumnsDictionary;
begin
  Result := FColumns;
end;

function TGSCol.GetMinHeight: Single;
begin
  Result := FMinHeight;
end;

function TGSCol.IsSidesStored: Boolean;
begin

end;

procedure TGSCol.LoadCompProperty(Stream: TStream);
var
  StringList: TStringList;
  I: Integer;
begin

  StringList := TStringList.Create;
  try
    StringList.LoadFromStream(Stream);

    Columns.Clear;
    for I := 0 to StringList.Count - 1 do
    begin
      Columns.Add(TScreenType(GetEnumValue(TypeInfo(TScreenType), StringList.Names[I])), StringList.Values[StringList.Names[I]].ToInteger);
    end;
    SetWidthByColumn;
    Realign;
  finally
    FreeAndNil(StringList);
  end;
end;

procedure TGSCol.Paint;
var
  LShapeRect: TRectF;
  Off: Single;
begin
  inherited;
  try
    LShapeRect := TRectF.Create(0, 0, Width, Height);
    if Sides <> AllSides then
    begin
      Off := LShapeRect.Left;
      if not(TSide.Top in FSides) then
        LShapeRect.Top := LShapeRect.Top - Off;
      if not(TSide.Left in FSides) then
        LShapeRect.Left := LShapeRect.Left - Off;
      if not(TSide.Bottom in FSides) then
        LShapeRect.Bottom := LShapeRect.Bottom + Off;
      if not(TSide.Right in FSides) then
        LShapeRect.Right := LShapeRect.Right + Off;
      Canvas.FillRect(LShapeRect, XRadius, YRadius, FCorners, AbsoluteOpacity, FFill, CornerType);
    end
    else
    begin
      Canvas.FillRect(LShapeRect, XRadius, YRadius, FCorners, AbsoluteOpacity, FFill, CornerType);
    end;
  finally

  end;
end;

procedure TGSCol.Resize;
begin
  inherited;
  Realign;
end;

procedure TGSCol.SetAutoHeight(const Value: Boolean);
begin
  FAutoHeight := Value;
end;

procedure TGSCol.SetColumns(const Value: TGSColumnsDictionary);
begin
  FColumns := Value;
end;

procedure TGSCol.SetCorners(const Value: TCorners);
begin
  FCorners := Value;
end;

procedure TGSCol.SetCornerType(const Value: TCornerType);
begin
  FCornerType := Value;
end;

procedure TGSCol.SetFill(const Value: TBrush);
begin
  FFill := Value;
end;

procedure TGSCol.SetSides(const Value: TSides);
begin
  FSides := Value;
end;

procedure TGSCol.SetWidthByColumn;
var
  GSRow: TGSRow;
  GSContainer: TGSContainer;
begin
  if Supports(Parent, IGSRow) then
  begin
    GSRow := Parent as TGSRow;
    if Supports(GSRow.Parent, IGSContainer) then
    begin
      GSContainer := GSRow.Parent as TGSContainer;
      if Columns.ContainsKey(GSContainer.ScreenType) then
      begin
        Width := TControl(GSContainer).Width * Columns.Items[GSContainer.ScreenType] / High(GSColumnWidth) - Margins.Left - Margins.Right - 0.001
      end
      else
        Width := TControl(GSContainer).Width - Margins.Left - Margins.Right;
    end;
  end;
end;

procedure TGSCol.SetXRadius(const Value: Single);
begin
  FXRadius := Value;
end;

procedure TGSCol.SetYRadius(const Value: Single);
begin
  FYRadius := Value;
end;

procedure TGSCol.StoreCompProperty(Stream: TStream);
var
  Key: TScreenType;
  StrList: TStringList;
begin
  if Columns <> nil then
  begin
    StrList := TStringList.Create;
    try
      for Key in FColumns.Keys do
      begin
        StrList.AddPair(GetEnumName(TypeInfo(TScreenType), Integer(Key)), IntToStr(Columns.Items[Key]));
      end;
    finally
      StrList.SaveToStream(Stream);
      FreeAndNil(StrList);
      SetWidthByColumn;
      Realign;
    end;

  end;
end;

end.
