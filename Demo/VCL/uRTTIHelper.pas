{-----------------------------------------------------------------------------
 Project: RTTIHelper_D100R
 Purpose: Helper functions for working with RTTI 
 Created: 06.11.2008 09:02:38
 
 (c) by TheUnknownOnes under Apache License 2.0
 see http://www.TheUnknownOnes.net
-----------------------------------------------------------------------------}

unit uRTTIHelper;

interface

uses
  System.TypInfo,
  System.Classes,
  System.SysUtils,
  System.Variants,
  System.StrUtils,
  System.Math;


function rttihGetPropertiesList(AInstance : TObject;
                                const AList : TStrings;
                                ARecursive : Boolean = false;
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : String = '';
                                ADelimiter : Char = '.';
                                AIgnoreClasses : TList = nil) : Integer; overload;
function rttihGetPropertiesList(AClassInfo : PTypeInfo;
                                const AList : TStrings;
                                ARecursive : Boolean = false;
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : String = '';
                                ADelimiter : Char = '.';
                                AIgnoreClasses : TList = nil) : Integer; overload;
function rttihGetPropertiesList(AClass : TClass;
                                const AList : TStrings;
                                ARecursive : Boolean = false;
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : String = '';
                                ADelimiter : Char = '.';
                                AIgnoreClasses : TList = nil) : Integer; overload;
//Lists all properties in the format "PropertyName[.SubPropertyName]"
// if ATypeKinds = [] then all properties a returned
// AIncludeTypeKinds has the priority before AExcludeTypeKinds


//==============================================================================


function rttihGetPropertyByName(AInstance : TObject;
                                APropertyName : String;
                                ADelimiter : Char = '.') : PPropInfo; overload;
function rttihGetPropertyByName(AClass : TClass;
                                APropertyName : String;
                                ADelimiter : Char = '.') : PPropInfo; overload;
function rttihGetPropertyByName(AClassInfo : PTypeInfo;
                                APropertyName : String;
                                ADelimiter : Char = '.') : PPropInfo; overload;
//Returns the property if existing, otherwise nil
//Example: rttihGetPropertyByName(Memo1, 'Font.Name');


//==============================================================================


function rttihGetPropertyValue(AInstance : TObject;
                               APropertyName : String;
                               ADelimiter : Char = '.') : Variant;
//returns the current value of the property or null if something went wrong
//Example: v := rttihGetPropertyValue(Button1, 'Font.Name');


//==============================================================================


procedure rttihSetPropertyValue(AInstance : TObject;
                                APropertyName : String;
                                AValue : Variant;
                                ADelimiter : Char = '.');
//sets the value to the specified property
//Example: rttihSetPropertyValue(Button1, 'Font.Name', 'Webdings');


//==============================================================================


function rttihGetInheritancePath(AClassInfo : PTypeInfo;
                                 ADelimiter : Char = '.') : String; overload;
function rttihGetInheritancePath(AClass : TClass;
                                 ADelimiter : Char = '.') : String; overload;
function rttihGetInheritancePath(AInstance : TObject;
                                 ADelimiter : Char = '.') : String; overload;
//returns the inheritance path of a class
//Example: rttihGetInheritancePath(Button1) returns 'TObject.TPersistent.TComponent.TControl.TWinControl.TButtonControl.TButton'


//==============================================================================


function rttihGetUnit(AClassInfo : PTypeInfo) : String; overload;
function rttihGetUnit(AClass : TClass) : String; overload;
function rttihGetUnit(AInstance : TObject) : String; overload;
//returns the name of the unit where the class is defined


//==============================================================================


function rttihOrdinalToString(ATypeInfo : PTypeInfo; Value : Integer) : String;
function rttihSetToList(ATypeInfo : PTypeInfo; const AList : TStrings) : Integer;
function rttihEnumToList(ATypeInfo : PTypeInfo; const AList : TStrings) : Integer;


//==============================================================================

type
  TrttihMethodParam = packed record
    Flags : TParamFlags;
    Name : String;
    TypeName : String;
  end;

  TrttihMethodParamList = array of TrttihMethodParam;

  TrttihMethodInfo = packed record
    Kind : TMethodKind;
    Params : TrttihMethodParamList;
    ReturnType : String;
  end;

function rttihMethodKindToString(AMethodKind : TMethodKind) : String;

function rttihGetMethodInfo(AMethodProperty : PTypeInfo) : TrttihMethodInfo; overload;
function rttihGetMethodInfo(AMethodProperty : PPropInfo) : TrttihMethodInfo; overload;


//==============================================================================


procedure rttihCloneObject(ASourceObj, ATargetObj : TObject; ASkipExceptions : Boolean = true);


implementation

function rttihGetPropertiesList(AInstance : TObject;
                                const AList : TStrings;
                                ARecursive : Boolean = false;
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : String = '';
                                ADelimiter : Char = '.';
                                AIgnoreClasses : TList = nil) : Integer;
begin
  Assert(Assigned(AInstance), 'Invalid object');
  Result := rttihGetPropertiesList(AInstance.ClassInfo,
                                   AList,
                                   ARecursive,
                                   AIncludeTypeKinds,
                                   AExcludeTypeKinds,
                                   ASkipExceptions,
                                   APrefix,
                                   ADelimiter,
                                   AIgnoreClasses);
end;

function rttihGetPropertiesList(AClass : TClass;
                                const AList : TStrings;
                                ARecursive : Boolean = false;
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : String = '';
                                ADelimiter : Char = '.';
                                AIgnoreClasses : TList = nil) : Integer; overload;
begin
  Assert(Assigned(AClass), 'Invalid class');
  Result := rttihGetPropertiesList(AClass.ClassInfo,
                                   AList,
                                   ARecursive,
                                   AIncludeTypeKinds,
                                   AExcludeTypeKinds,
                                   ASkipExceptions,
                                   APrefix,
                                   ADelimiter,
                                   AIgnoreClasses);
end;

function rttihGetPropertiesList(AClassInfo : PTypeInfo;
                                const AList : TStrings;
                                ARecursive : Boolean = false;  
                                AIncludeTypeKinds : TTypeKinds = [];
                                AExcludeTypeKinds : TTypeKinds = [];
                                ASkipExceptions : Boolean = true;
                                APrefix : String = '';
                                ADelimiter : Char = '.';
                                AIgnoreClasses : TList = nil) : Integer;
var
  TypeData : PTypeData;
  PropInfoList : PPropList;
  idx : Integer;
  FreeIgnoreClasses : Boolean;
begin
  Assert(Assigned(AClassInfo) and (AClassinfo.Kind = tkClass), 'Invalid classinfo');

  Result := 0;

  TypeData := GetTypeData(AClassInfo);

  GetMem(PropInfoList, TypeData.PropCount * SizeOf(PPropInfo));
  try
    GetPropInfos(AClassInfo, PropInfoList);

    for idx := 0 to TypeData.PropCount - 1 do
    begin
      try
        if (AIncludeTypeKinds = []) and (AExcludeTypeKinds = []) or
           ((PropInfoList[idx].PropType^.Kind in AIncludeTypeKinds) and (AIncludeTypeKinds <> [])) or
           ((not (PropInfoList[idx].PropType^.Kind in AExcludeTypeKinds)) and (AExcludeTypeKinds <> [])) then
        begin
          AList.AddObject(APrefix + String(PropInfoList[idx].Name), TObject(PropInfoList[idx]));

          Inc(Result);
        end;

        if ARecursive and (PropInfoList[idx].PropType^.Kind = tkClass) then
        begin
          if not Assigned(AIgnoreClasses) then
          begin
            AIgnoreClasses := TList.Create;
            FreeIgnoreClasses := true;
          end
          else
            FreeIgnoreClasses := false;

          try

            if AIgnoreClasses.IndexOf(AIgnoreClasses) = -1 then
            begin
              AIgnoreClasses.Add(AIgnoreClasses);

              rttihGetPropertiesList(PropInfoList[idx].PropType^,
                                     AList,
                                     ARecursive,
                                     AIncludeTypeKinds,
                                     AExcludeTypeKinds,
                                     ASkipExceptions,
                                     APrefix + String(PropInfoList[idx].Name) + ADelimiter,
                                     ADelimiter,
                                     AIgnoreClasses);
            end;

          finally
            if FreeIgnoreClasses then
            begin
              AIgnoreClasses.Free;
              AIgnoreClasses := nil;
            end;
          end;
        end;
        
      except
        on E : Exception do
          if not ASkipExceptions then
            raise;
      end;    
    end;

  finally
    FreeMem(PropInfoList, TypeData.PropCount * SizeOf(PPropInfo));
  end;
end;


//==============================================================================


function rttihGetPropertyByName(AInstance : TObject;
                                APropertyName : String;
                                ADelimiter : Char = '.') : PPropInfo;
begin
  Assert(Assigned(AInstance), 'Invalid object');
  Result := rttihGetPropertyByName(AInstance.ClassInfo, APropertyName, ADelimiter);
end;

function rttihGetPropertyByName(AClass : TClass;
                                APropertyName : String;
                                ADelimiter : Char = '.') : PPropInfo; overload;
begin
  Assert(Assigned(AClass), 'Invalid class');
  Result := rttihGetPropertyByName(AClass.ClassInfo, APropertyName, ADelimiter);
end;

function rttihGetPropertyByName(AClassInfo : PTypeInfo;
                                APropertyName : String;
                                ADelimiter : Char = '.') : PPropInfo;
var
  PosOfDelim : Integer;
  PropName : String;
begin
  Assert(Assigned(AClassInfo) and (AClassinfo.Kind = tkClass), 'Invalid classinfo');

  Result := nil;

  PosOfDelim := Pos(ADelimiter, APropertyName);
  if PosOfDelim = 0 then
    PosOfDelim := Length(APropertyName) + 1;


  if PosOfDelim > 0 then
  begin
    PropName := Copy(APropertyName, 1, PosOfDelim - 1);
    Result := GetPropInfo(AClassInfo, PropName);

    if Assigned(Result) and
       (Result.PropType^.Kind = tkClass) and
       (PosOfDelim < Length(APropertyName)) then
    begin
      Result := rttihGetPropertyByName(Result.PropType^, Copy(APropertyName, PosOfDelim + 1, Length(APropertyName)));
    end;

  end;
end;


//==============================================================================


function rttihGetPropertyValue(AInstance : TObject;
                               APropertyName : String;
                               ADelimiter : Char = '.') : Variant;
var
  PosOfDelim : Integer;
  PropName : String;
  PropInfo : PPropInfo;
  Method : TMethod;
begin
  Result := null;
  PropInfo := nil;

  PosOfDelim := Pos(ADelimiter, APropertyName);
  if PosOfDelim = 0 then
    PosOfDelim := Length(APropertyName) + 1;


  if PosOfDelim > 0 then
  begin
    PropName := Copy(APropertyName, 1, PosOfDelim - 1);
    if Assigned(AInstance) then
    begin
      PropInfo := GetPropInfo(AInstance, PropName);
      if PropInfo^.PropType^.Kind = tkInterface then
        Result:=GetInterfaceProp(AInstance, APropertyName)
      else
      begin
        Result := GetPropValue(AInstance, PropName, false);

        if PropInfo^.PropType^.Kind = tkMethod then
        begin
          Method := GetMethodProp(AInstance, PropInfo);
          Result := VarArrayOf([Integer(Method.Code), Integer(Method.Data)]);
        end;
      end;
    end
    else
      Result := null;

    if (not VarIsNull(Result)) and (PropInfo <> nil) then
    if (PropInfo.PropType^.Kind = tkClass) and
       (PosOfDelim < Length(APropertyName)) then
    begin
      Result := rttihGetPropertyValue(TObject(Integer(Result)),
                                      Copy(APropertyName, PosOfDelim + 1, Length(APropertyName)));
    end
  end;
end;


//==============================================================================


procedure rttihSetPropertyValue(AInstance : TObject;
                                APropertyName : String;
                                AValue : Variant;
                                ADelimiter : Char = '.');
var
  PosOfDelim : Integer;
  PropName : String;
  PropInfo : PPropInfo;
  PropValue : Variant;
  Method : TMethod;
begin
  PosOfDelim := Pos(ADelimiter, APropertyName);
  if PosOfDelim = 0 then
    PosOfDelim := Length(APropertyName) + 1;


  if PosOfDelim > 0 then
  begin
    PropName := Copy(APropertyName, 1, PosOfDelim - 1);
    PropValue := GetPropValue(AInstance, PropName, false);
    PropInfo := GetPropInfo(AInstance, PropName);

    if (PropInfo.PropType^.Kind = tkClass) and
       (Integer(PropValue) <> 0) and
       (PosOfDelim < Length(APropertyName)) then
    begin
      rttihSetPropertyValue(TObject(Integer(PropValue)),
                            Copy(APropertyName,PosOfDelim + 1, Length(APropertyName)),
                            AValue,
                            ADelimiter);
    end
    else
    if PosOfDelim > Length(APropertyName) then
    begin
      case PropInfo.PropType^.Kind of
        tkClass: SetOrdProp(AInstance, PropInfo, AValue);
        tkMethod:
        begin
          Method.Code := Pointer(Integer(AValue[0]));
          Method.Data := Pointer(Integer(AValue[1]));
          SetMethodProp(AInstance, PropInfo, Method);
        end
        else
          SetPropValue(AInstance, PropInfo, AValue);
      end;
    end;

  end;
end;


//==============================================================================


function rttihGetInheritancePath(AClass : TClass;
                                 ADelimiter : Char = '.') : String;
begin
  Result := rttihGetInheritancePath(AClass.ClassInfo,
                                    ADelimiter);
end;

function rttihGetInheritancePath(AInstance : TObject;
                                 ADelimiter : Char = '.') : String;
begin
  Result := rttihGetInheritancePath(AInstance.ClassInfo,
                                    ADelimiter);
end;

function rttihGetInheritancePath(AClassInfo : PTypeInfo;
                                 ADelimiter : Char = '.') : String;
var
  TypeData : PTypeData;
begin
  Assert(Assigned(AClassInfo) and (AClassinfo.Kind = tkClass), 'Invalid classinfo');

  repeat
    Result := String(PTypeInfo(AClassInfo).Name) + ADelimiter + Result;

    TypeData := GetTypeData(AClassInfo);
    if Assigned(TypeData.ParentInfo) then
      AClassInfo := TypeData.ParentInfo^
    else
      AClassInfo := nil;
  until not Assigned(AClassInfo);

  Result := LeftStr(Result, Length(Result) - 1); //cut the last delimiter

end;


//==============================================================================


function rttihGetUnit(AClassInfo : PTypeInfo) : String;
var
  TypeData : PTypeData;
begin
  Assert(Assigned(AClassInfo) and (AClassinfo.Kind = tkClass), 'Invalid classinfo');

  TypeData := GetTypeData(AClassInfo);

  if Assigned(TypeData) then
    Result := String(TypeData.UnitName)
  else
    Result := EmptyStr;
end;

function rttihGetUnit(AClass : TClass) : String;
begin
  Assert(Assigned(AClass), 'Invalid class');
  Result := rttihGetUnit(AClass.ClassInfo);
end;

function rttihGetUnit(AInstance : TObject) : String;
begin
  Assert(Assigned(AInstance), 'Invalid object');
  Result := rttihGetUnit(AInstance.ClassInfo);
end;


//==============================================================================


function rttihOrdinalToString(ATypeInfo : PTypeInfo; Value : Integer) : String;
const
  AsciiChars = [32..127];
begin
  Assert(Assigned(ATypeInfo) and
        (ATypeInfo.Kind in [tkInteger, tkInt64, tkChar, tkWChar, tkEnumeration]), 'Invalid TypeInfo');

  case ATypeInfo.Kind of
    tkInteger, tkInt64: Result := IntToStr(Value);
    tkChar, tkWChar:
    begin
      if Value in AsciiChars then
        Result := '''' + Chr(Value) + ''''
      else
        Result := Format('#%d', [Value]);
    end;
    tkEnumeration:
    begin
      Result := GetEnumName(ATypeInfo, Value);
    end;

  end;
end;

function rttihSetToList(ATypeInfo : PTypeInfo; const AList : TStrings) : Integer;
var
  TypeInfoComp : PTypeInfo;
  TypeData,
  TypeDataComp : PTypeData;
  Element : 0..255; //May be adapted to future changes
begin
  Assert(Assigned(ATypeInfo) and (ATypeInfo.Kind = tkSet), 'Invalid TypeInfo');

  Result := 0;

  TypeData := GetTypeData(ATypeInfo);
  TypeInfoComp := TypeData.CompType^;
  TypeDataComp := GetTypeData(TypeInfoComp);

  for Element := TypeDataComp.MinValue to TypeDataComp.MaxValue do
  begin
    AList.AddObject(Format('%s%s%d', [rttihOrdinalToString(TypeInfoComp, Element),
                                          AList.NameValueSeparator,
                                          Trunc(Power(2, Element))]),
                    TObject(Trunc(Power(2, Element))));
    Inc(Result);
  end;
end;

function rttihEnumToList(ATypeInfo : PTypeInfo; const AList : TStrings) : Integer;
var
  TypeData : PTypeData;
  Element : 0..255; //May be adapted to future changes
begin
  Assert(Assigned(ATypeInfo) and (ATypeInfo.Kind = tkEnumeration), 'Invalid TypeInfo');

  TypeData := GetTypeData(ATypeInfo);

  Result := 0;

  for Element := TypeData.MinValue to TypeData.MaxValue do
  begin
    AList.AddObject(Format('%s%s%d', [rttihOrdinalToString(ATypeInfo, Element),
                                          AList.NameValueSeparator,
                                          Element]),
                   TObject(Element));
    Inc(Result);
  end;
    
end;


//==============================================================================


function rttihMethodKindToString(AMethodKind : TMethodKind) : String;
begin
  case AMethodKind of
    mkProcedure: Result := 'procedure';
    mkFunction: Result := 'function';
    mkConstructor: Result := 'constructor';
    mkDestructor: Result := 'destructor';
    mkClassProcedure: Result := 'class procedure';
    mkClassFunction: Result := 'class function';
    mkClassConstructor: Result := '';
    mkOperatorOverload: Result := 'class operator';
    mkSafeProcedure: Result := 'procedure';
    mkSafeFunction: Result := 'function';
  end;
end;

function rttihGetMethodInfo(AMethodProperty : PPropInfo) : TrttihMethodInfo;
begin
  Assert(Assigned(AMethodProperty), 'Invalid property');

  Result := rttihGetMethodInfo(AMethodProperty.PropType^);
end;

function rttihGetMethodInfo(AMethodProperty : PTypeInfo) : TrttihMethodInfo;
var
  td : PTypeData;
  idx : Integer;
  CurParam : Pointer;
  Len : Byte;
type
  PParamFlags = ^TParamFlags;
begin
  Assert(Assigned(AMethodProperty) and (AMethodProperty.Kind = tkMethod), 'Invalid property');

  td := GetTypeData(AMethodProperty);

  Assert(Assigned(td), 'Can not determine type data');

  Result.Kind := td.MethodKind;

  SetLength(Result.Params, td.ParamCount);

  CurParam := @td.ParamList;
  for idx := 0 to td.ParamCount - 1 do
  begin
    Result.Params[idx].Flags := PParamFlags(CurParam)^;
    Inc(PParamFlags(CurParam), 1);

    Len := PByte(CurParam)^;
    Result.Params[idx].Name := String(PShortString(CurParam)^);
    Inc(PChar(CurParam), Len + 1);

    Len := PByte(CurParam)^;
    Result.Params[idx].TypeName := String(PShortString(CurParam)^);
    Inc(PChar(CurParam), Len + 1);
  end;

  if td.MethodKind in [mkFunction, mkSafeFunction] then
    Result.ReturnType := String(PShortString(CurParam)^)
  else
    Result.ReturnType := '';
end;


//==============================================================================


procedure rttihCloneObject(ASourceObj, ATargetObj : TObject; ASkipExceptions : Boolean = true);
var
  Props : TStringList;
  idx : Integer;
  PropSource,
  PropTarget : PPropInfo;
begin
  Assert(Assigned(ASourceObj) and Assigned(ATargetObj), 'Source and target has to be valid objects!');

  Props := TStringList.Create;
  try
    rttihGetPropertiesList(ASourceObj,
                           Props,
                           true,
                           [],
                           [tkUnknown, tkInterface]);
    for idx := 0 to Props.Count - 1 do
    begin
      PropSource := rttihGetPropertyByName(ASourceObj, Props[idx]);
      PropTarget := rttihGetPropertyByName(ATargetObj, Props[idx]);

      if Assigned(PropSource) and
         Assigned(PropTarget) and
         Assigned(PropSource^.GetProc) and
         Assigned(PropTarget^.SetProc) then
      begin
        try
          rttihSetPropertyValue(ATargetObj, Props[idx], rttihGetPropertyValue(ASourceObj, Props[idx]));
        except
          if not ASkipExceptions then
            raise;
        end;
      end;
    end;

  finally
    Props.Free;
  end;

end;

end.
