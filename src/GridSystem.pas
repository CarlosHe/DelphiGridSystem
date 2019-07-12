unit GridSystem;

interface

uses
  FMX.Dialogs, System.SysUtils, System.Types, FMX.Graphics, System.Classes, System.Generics.Collections, FMX.Types, FMX.Controls, FMX.Layouts,
  GridSystem.Types,
  System.TypInfo;

type

  TGSLayout = class(TLayout)
  private
    { private declarations }
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
  protected
    { protected declarations }
    procedure DoFillChange(Sender: TObject);
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

  TGSFlowLayout = class(TFlowLayout)
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

  TGSContainer = class(TGSLayout, IGSContainer)
  private
    { private declarations }
  protected
    { protected declarations }
    function GetScreenType: TScreenType;
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { published declarations }
    property ScreenType: TScreenType read GetScreenType;
  end;

  TGSRow = class(TGSFlowLayout, IGSRow)
  private
    { private declarations }
  protected
    { protected declarations }
  public
    { public declarations }
  published
    { published declarations }
  end;

  TGSCol = class(TGSLayout, IGSCol)
  private
    { private declarations }
    FColumns: TGSColumnsDictionary;
    FOnColumnsChanged: TNotifyEvent;
    procedure SetColumns(const Value: TGSColumnsDictionary);
    function GetColumns: TGSColumnsDictionary;
    procedure DoValueNotify(Sender: TObject; const Item: GSColumnWidth; Action: TCollectionNotification);
  protected
    { protected declarations }
    procedure DoRealign; override;
    procedure SetWidthByColumn;
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
  end;

implementation

{ TGSContainer }

constructor TGSContainer.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TGSContainer.Destroy;
begin

  inherited;
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

{ TGSCol }

constructor TGSCol.Create(AOwner: TComponent);
begin
  inherited;
  FColumns := TGSColumnsDictionary.Create;
  FColumns.OnValueNotify := DoValueNotify;
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
  inherited;
end;

procedure TGSCol.DoRealign;
begin
  inherited;
  SetWidthByColumn;
end;

procedure TGSCol.DoValueNotify(Sender: TObject; const Item: GSColumnWidth; Action: TCollectionNotification);
begin
  SetWidthByColumn;
  Realign;
end;

function TGSCol.GetColumns: TGSColumnsDictionary;
begin
  Result := FColumns;
end;

procedure TGSCol.LoadCompProperty(Stream: TStream);
var
  LStringList: TStringList;
  LStrCount: Integer;
begin
  LStringList := TStringList.Create;
  try
    LStringList.LoadFromStream(Stream);
    Columns.Clear;
    for LStrCount := 0 to LStringList.Count - 1 do
    begin
      Columns.Add(TScreenType(GetEnumValue(TypeInfo(TScreenType), LStringList.Names[LStrCount])),
        LStringList.Values[LStringList.Names[LStrCount]].ToInteger);
    end;
    SetWidthByColumn;
    Realign;
  finally
    FreeAndNil(LStringList);
  end;
end;

procedure TGSCol.SetColumns(const Value: TGSColumnsDictionary);
begin
  FColumns := Value;
end;

procedure TGSCol.SetWidthByColumn;
var
  LGSRow: TGSRow;
  LGSContainer: TGSContainer;
  LColumnsCount: Integer;
  LNewWidth: Single;
  LColumnWidth: GSColumnWidth;
begin
  if Supports(Parent, IGSRow) then
  begin
    LGSRow := Parent as TGSRow;
    if Supports(LGSRow.Parent, IGSContainer) then
    begin
      LGSContainer := LGSRow.Parent as TGSContainer;
      LNewWidth := TControl(LGSContainer).Width - Margins.Left - Margins.Right - 0.001;
      if Columns.ContainsKey(LGSContainer.ScreenType) then
      begin
        LNewWidth := TControl(LGSContainer).Width * Columns.Items[LGSContainer.ScreenType] / High(GSColumnWidth) - Margins.Left -
          Margins.Right - 0.001;
      end
      else
      begin
        for LColumnsCount := 0 to Integer(High(TScreenType)) do
        begin
          if Columns.TryGetValue(TScreenType(LColumnsCount), LColumnWidth) then
          begin
            LNewWidth := TControl(LGSContainer).Width * LColumnWidth / High(GSColumnWidth) - Margins.Left - Margins.Right - 0.001;
          end;
          if LGSContainer.ScreenType < TScreenType(LColumnsCount) then
            Break;
        end;
      end;
      Width := LNewWidth;
    end;
  end;
end;

procedure TGSCol.StoreCompProperty(Stream: TStream);
var
  LKey: TScreenType;
  LStrList: TStringList;
begin
  if Columns <> nil then
  begin
    LStrList := TStringList.Create;
    try
      for LKey in FColumns.Keys do
      begin
        LStrList.AddPair(GetEnumName(TypeInfo(TScreenType), Integer(LKey)), IntToStr(Columns.Items[LKey]));
      end;
    finally
      LStrList.SaveToStream(Stream);
      FreeAndNil(LStrList);
      SetWidthByColumn;
      Realign;
    end;
  end;
end;

{ TGSLayout }

function TGSLayout.CalcNewHeight: Single;
  function MaxBottomControl(Control: TControl): Single;
  begin
    if Control.Visible then
      Result := Control.Position.Y + Control.Height + Control.Margins.Bottom + TControl(Control.Parent).Padding.Bottom
    else
      Result := 0;
  end;

var
  LControlsCount: Integer;
  LNewHeight: Single;
begin
  LNewHeight := FMinHeight;
  for LControlsCount := 0 to ControlsCount - 1 do
  begin
    if MaxBottomControl(Controls[LControlsCount]) > LNewHeight then
      LNewHeight := MaxBottomControl(Controls[LControlsCount]);
  end;
  Result := LNewHeight;
end;

constructor TGSLayout.Create(AOwner: TComponent);
begin
  inherited;
  CanParentFocus := True;
  HitTest := False;
  FMinHeight := 50;
  FFill := TBrush.Create(TBrushKind.None, $FFFFFFFF);
  FFill.OnChanged:= DoFillChange;
end;

destructor TGSLayout.Destroy;
begin
  FreeAndNil(FFill);
  inherited;
end;

procedure TGSLayout.DoFillChange(Sender: TObject);
begin
  Repaint;
end;

procedure TGSLayout.DoRealign;
begin
  inherited;
  if FAutoHeight then
    Self.Height := CalcNewHeight;
end;

function TGSLayout.GetMinHeight: Single;
begin
  Result := FMinHeight;
end;

function TGSLayout.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

procedure TGSLayout.Paint;
var
  LShapeRect: TRectF;
  LOff: Single;
begin
  inherited;
  try
    LShapeRect := TRectF.Create(0, 0, Width, Height);
    if Sides <> AllSides then
    begin
      LOff := LShapeRect.Left;
      if not(TSide.Top in FSides) then
        LShapeRect.Top := LShapeRect.Top - LOff;
      if not(TSide.Left in FSides) then
        LShapeRect.Left := LShapeRect.Left - LOff;
      if not(TSide.Bottom in FSides) then
        LShapeRect.Bottom := LShapeRect.Bottom + LOff;
      if not(TSide.Right in FSides) then
        LShapeRect.Right := LShapeRect.Right + LOff;
      Canvas.FillRect(LShapeRect, XRadius, YRadius, FCorners, AbsoluteOpacity, FFill, CornerType);
    end
    else
    begin
      Canvas.FillRect(LShapeRect, XRadius, YRadius, FCorners, AbsoluteOpacity, FFill, CornerType);
    end;
  finally

  end;
end;

procedure TGSLayout.Resize;
begin
  inherited;
  Realign;
end;

procedure TGSLayout.SetAutoHeight(const Value: Boolean);
begin
  FAutoHeight := Value;
end;

procedure TGSLayout.SetCorners(const Value: TCorners);
begin
  FCorners := Value;
end;

procedure TGSLayout.SetCornerType(const Value: TCornerType);
begin
  FCornerType := Value;
end;

procedure TGSLayout.SetFill(const Value: TBrush);
begin
  FFill := Value;
end;

procedure TGSLayout.SetMinHeight(const Value: Single);
begin
  FMinHeight := Value;
end;

procedure TGSLayout.SetSides(const Value: TSides);
begin
  FSides := Value;
end;

procedure TGSLayout.SetXRadius(const Value: Single);
begin
  FXRadius := Value;
end;

procedure TGSLayout.SetYRadius(const Value: Single);
begin
  FYRadius := Value;
end;

{ TGSFlowLayout }

function TGSFlowLayout.CalcNewHeight: Single;
  function MaxBottomControl(Control: TControl): Single;
  begin
    if Control.Visible then
      Result := Control.Position.Y + Control.Height + Control.Margins.Bottom + TControl(Control.Parent).Padding.Bottom
    else
      Result := 0;
  end;

var
  LControlsCount: Integer;
  LNewHeight: Single;
begin
  LNewHeight := FMinHeight;

  for LControlsCount := 0 to ControlsCount - 1 do
  begin
    if MaxBottomControl(Controls[LControlsCount]) > LNewHeight then
      LNewHeight := MaxBottomControl(Controls[LControlsCount]);
  end;
  Result := LNewHeight;
end;

constructor TGSFlowLayout.Create(AOwner: TComponent);
begin
  inherited;
  CanParentFocus := True;
  HitTest := False;
  FMinHeight := 50;
  FFill := TBrush.Create(TBrushKind.None, $FFFFFFFF);
end;

destructor TGSFlowLayout.Destroy;
begin
  FreeAndNil(FFill);
  inherited;
end;

procedure TGSFlowLayout.DoRealign;
begin
  inherited;
  if FAutoHeight then
    Height := CalcNewHeight;
end;

function TGSFlowLayout.GetMinHeight: Single;
begin
  Result := FMinHeight
end;

function TGSFlowLayout.IsSidesStored: Boolean;
begin
  Result := FSides * AllSides <> AllSides
end;

procedure TGSFlowLayout.Paint;
var
  LShapeRect: TRectF;
  LOff: Single;
begin
  inherited;
  try
    LShapeRect := TRectF.Create(0, 0, Width, Height);
    if Sides <> AllSides then
    begin
      LOff := LShapeRect.Left;
      if not(TSide.Top in FSides) then
        LShapeRect.Top := LShapeRect.Top - LOff;
      if not(TSide.Left in FSides) then
        LShapeRect.Left := LShapeRect.Left - LOff;
      if not(TSide.Bottom in FSides) then
        LShapeRect.Bottom := LShapeRect.Bottom + LOff;
      if not(TSide.Right in FSides) then
        LShapeRect.Right := LShapeRect.Right + LOff;
      Canvas.FillRect(LShapeRect, XRadius, YRadius, FCorners, AbsoluteOpacity, FFill, CornerType);
    end
    else
    begin
      Canvas.FillRect(LShapeRect, XRadius, YRadius, FCorners, AbsoluteOpacity, FFill, CornerType);
    end;
  finally

  end;
end;

procedure TGSFlowLayout.Resize;
var
  LControlsCount: Integer;
begin
  inherited;
  for LControlsCount := 0 to ControlsCount - 1 do
  begin
    if Supports(Controls[LControlsCount], IGSCol) then
    begin
      TGSCol(Controls[LControlsCount]).Realign;
    end;
  end;
  Realign;
end;

procedure TGSFlowLayout.SetAutoHeight(const Value: Boolean);
begin
  FAutoHeight := Value;
end;

procedure TGSFlowLayout.SetCorners(const Value: TCorners);
begin
  FCorners := Value;
end;

procedure TGSFlowLayout.SetCornerType(const Value: TCornerType);
begin
  FCornerType := Value;
end;

procedure TGSFlowLayout.SetFill(const Value: TBrush);
begin
  FFill := Value;
end;

procedure TGSFlowLayout.SetMinHeight(const Value: Single);
begin
  FMinHeight := Value;
end;

procedure TGSFlowLayout.SetSides(const Value: TSides);
begin
  FSides := Value;
end;

procedure TGSFlowLayout.SetXRadius(const Value: Single);
begin
  FXRadius := Value;
end;

procedure TGSFlowLayout.SetYRadius(const Value: Single);
begin
  FYRadius := Value;
end;

end.
