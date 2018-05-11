unit GridSystem.Types;

interface

uses
  System.Classes, FMX.Types, System.Generics.Collections;

type

  TScreenType = (ScreenExtraSmall, ScreenSmall, ScreenMedium, ScreenLarge, ScreenExtraLarge);

  GSColumnWidth = 1 .. 12;

  TGSColumnsDictionary = TDictionary<TScreenType, GSColumnWidth>;

  IGSContainer = interface(IControl)
    ['{6BB2E0E3-9146-40BB-8C6B-E0788D586E20}']
    function GetScreenType: TScreenType;
    procedure SetMinHeight(const Value: Single);
    function GetMinHeight: Single;

    property ScreenType: TScreenType read GetScreenType;
    property MinHeight: Single read GetMinHeight write SetMinHeight;
  end;

  IGSRow = interface(IControl)
    ['{6CF1989D-75C3-4744-8B9B-5DC9CF8D1B9B}']
    procedure SetMinHeight(const Value: Single);
    function GetMinHeight: Single;
    property MinHeight: Single read GetMinHeight write SetMinHeight;
  end;

  IGSCol = interface
    ['{31E963D8-9305-452A-AED3-9C0408AEEFCF}']
    procedure SetColumns(const Value: TGSColumnsDictionary);
    function GetColumns: TGSColumnsDictionary;
    property Columns: TGSColumnsDictionary read GetColumns write SetColumns;
  end;

implementation

end.
