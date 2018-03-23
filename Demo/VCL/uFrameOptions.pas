unit uFrameOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, zint, Grids, ValEdit, uRTTIHelper, TypInfo;

type
  TFrameOptions = class(TFrame)
    ValueListEditor1: TValueListEditor;
    procedure ValueListEditor1GetPickList(Sender: TObject;
      const KeyName: string; Values: TStrings);
    procedure ValueListEditor1Validate(Sender: TObject; ACol, ARow: Integer;
      const KeyName, KeyValue: string);
  private
    FSymbol : TZintSymbol;
    FOptionPropertyObj : TCustomZintSymbolOptions;
    FOnChange: TNotifyEvent;
  public
    procedure Init(ASymbol: TZintSymbol; AOptionProperty : String);
    procedure RefreshFrame(Sender: TObject);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{$R *.dfm}

{ TFrameOptions }

procedure TFrameOptions.Init(ASymbol: TZintSymbol; AOptionProperty: String);
var
  Properties : TStringList;
  i: Integer;
begin
  FSymbol:=ASymbol;
  FOptionPropertyObj:=TCustomZintSymbolOptions({$IF declared(NativeInt)}NativeInt{$ELSE}Integer{$IFEND}(rttihGetPropertyValue(FSymbol, AOptionProperty)));

  Properties:=TStringList.Create;
  try
    rttihGetPropertiesList(FOptionPropertyObj, Properties);

    for i := 0 to Properties.Count - 1 do
      ValueListEditor1.InsertRow(Properties[i], '', true);

  finally
    Properties.Free;
  end;
end;

procedure TFrameOptions.RefreshFrame(Sender: TObject);
var
  Properties, EnumVals : TStringList;
  PropfInfo : PTypeInfo;
  value : Variant;
  i, idx: Integer;
begin
  Properties:=TStringList.Create;
  EnumVals:=TStringList.Create;
  try
    rttihGetPropertiesList(FOptionPropertyObj, Properties);

    for i := 0 to Properties.Count - 1 do
    begin
      value:=rttihGetPropertyValue(FOptionPropertyObj, Properties[i]);

      PropfInfo:=GetPropInfo(FOptionPropertyObj.ClassType, Properties[i]).PropType^;

      if PropfInfo.Kind = tkEnumeration then
      begin
        EnumVals.Clear;
        rttihEnumToList(PropfInfo, EnumVals);
        idx:=EnumVals.IndexOfObject(TObject(Integer(value)));
        if idx>=0 then
          Value:=EnumVals.Names[idx];
      end;

      ValueListEditor1.Values[Properties[i]]:=value;
    end;
  finally
    EnumVals.Free;
    Properties.Free;
  end;
end;

procedure TFrameOptions.ValueListEditor1GetPickList(Sender: TObject;
  const KeyName: string; Values: TStrings);
var
  PropfInfo : PTypeInfo;
  EnumVals : TStringList;
  I: Integer;
begin
  PropfInfo:=GetPropInfo(FOptionPropertyObj.ClassType, KeyName).PropType^;
  EnumVals:=TStringList.Create;

  try

    if PropfInfo.Kind = tkEnumeration then
    begin
      rttihEnumToList(PropfInfo, EnumVals);

      for I := 0 to EnumVals.Count - 1 do
        Values.Add(EnumVals.Names[i]);
    end;

  finally
    EnumVals.Free;
  end;
end;

procedure TFrameOptions.ValueListEditor1Validate(Sender: TObject; ACol,
  ARow: Integer; const KeyName, KeyValue: string);
var
  PropfInfo : PTypeInfo;
  EnumVals : TStringList;
  I: Integer;
  Value : Variant;
begin
  PropfInfo:=GetPropInfo(FOptionPropertyObj.ClassType, KeyName).PropType^;
  EnumVals:=TStringList.Create;
  try
    if PropfInfo.Kind = tkEnumeration then
    begin
      rttihEnumToList(PropfInfo, EnumVals);
      i:=EnumVals.IndexOfName(KeyValue);
      if i>=0 then
        Value:=StrToInt(EnumVals.ValueFromIndex[i]);
    end
    else
      Value:=KeyValue;

    try
     rttihSetPropertyValue(FOptionPropertyObj, KeyName, Value);
    except
    end;

    RefreshFrame(nil);
  finally
    EnumVals.Free;
  end;

  if Assigned(FOnChange) then
    FOnChange(self);
end;

end.
