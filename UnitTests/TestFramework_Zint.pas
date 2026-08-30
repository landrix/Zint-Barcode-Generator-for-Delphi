unit TestFramework_Zint;

{$I zint_test.inc}

{
  Brueckenschicht, damit dieselben Testunits unter Delphi (DUnitX) und
  Free Pascal (fpcunit) laufen.

  Hintergrund: DUnitX hat keinerlei FPC-Unterstuetzung (weder die hier
  verwendete Kopie noch der Upstream). Statt zwei Testsuiten zu pflegen,
  laufen die Testunits gegen diese Schicht:

    - ZAssert          Assertions, delegiert an DUnitX.Assert bzw. TAssert
    - TZintFixture     Basisklasse; unter FPC TTestCase, unter Delphi TObject
    - ZRegisterFixture Registrierung, delegiert an TDUnitX bzw. testregistry
    - Attribut-Dummies Unter FPC bleiben [Test]/[TestFixture] stehen und
                       werden ignoriert; FPC 3.3.1 parst Custom Attributes.

  Wichtig fuer Testautoren:
    - Testmethoden gehoeren in einen published-Abschnitt (fpcunit findet sie
      per RTTI ueber GetMethodList).
    - Setup/TearDown gehoeren NICHT nach published. GetMethodList sammelt jede
      published Methode als Testfall, auch geerbte.

  Siehe docs/PORTING_WORKFLOW.md Abschnitt 4.4.
}

interface

uses
  {$IFDEF FPC}
  fpcunit, testregistry
  {$ELSE}
  DUnitX.TestFramework
  {$ENDIF}
  ;

{$IFDEF FPC}
type
  { Attribut-Dummies. Sie tragen keine Semantik, sie erlauben nur, dass die
    Delphi-Annotationen in den Testunits unveraendert stehen bleiben. }
  TestFixtureAttribute = class(TCustomAttribute)
  public
    constructor Create; overload;
    constructor Create(const AName: string); overload;
    constructor Create(const AName, ADescription: string); overload;
  end;

  TestAttribute = class(TCustomAttribute)
  public
    constructor Create; overload;
    constructor Create(const AEnabled: Boolean); overload;
  end;

  { Auch parameterlose Attribute brauchen einen eigenen Konstruktor: FPC findet
    sonst nur TObject.Create und meldet "Wrong number of parameters". }
  SetupAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;

  TearDownAttribute = class(TCustomAttribute)
  public
    constructor Create;
  end;
{$ENDIF}

type
  { Basisklasse aller Testfixtures. }
  TZintFixture = class({$IFDEF FPC}TTestCase{$ELSE}TObject{$ENDIF});

  TZintFixtureClass = class of TZintFixture;

  { Assertions. Bewusst auf die drei Formen begrenzt, die im Bestand
    tatsaechlich vorkommen (AreEqual, IsTrue, IsFalse). }
  ZAssert = class
  public
    class procedure AreEqual(const AExpected, AActual: Integer; const AMessage: string = ''); overload;
    class procedure AreEqual(const AExpected, AActual: Int64; const AMessage: string = ''); overload;
    class procedure AreEqual(const AExpected, AActual: string; const AMessage: string = ''); overload;
    class procedure AreEqual(const AExpected, AActual: Boolean; const AMessage: string = ''); overload;
    class procedure AreEqual(const AExpected, AActual: Char; const AMessage: string = ''); overload;
    class procedure AreEqual(const AExpected, AActual: Single; const AMessage: string = ''); overload;

    class procedure IsTrue(const ACondition: Boolean; const AMessage: string = ''); overload;
    class procedure IsFalse(const ACondition: Boolean; const AMessage: string = ''); overload;

    class procedure Fail(const AMessage: string = '');
  end;

procedure ZRegisterFixture(AClass: TClass);

implementation

uses
  SysUtils;

{$IFDEF FPC}
constructor TestFixtureAttribute.Create;
begin
  inherited Create;
end;

constructor TestFixtureAttribute.Create(const AName: string);
begin
  inherited Create;
end;

constructor TestFixtureAttribute.Create(const AName, ADescription: string);
begin
  inherited Create;
end;

constructor TestAttribute.Create;
begin
  inherited Create;
end;

constructor TestAttribute.Create(const AEnabled: Boolean);
begin
  inherited Create;
end;

constructor SetupAttribute.Create;
begin
  inherited Create;
end;

constructor TearDownAttribute.Create;
begin
  inherited Create;
end;
{$ENDIF}

{ ZAssert }

class procedure ZAssert.AreEqual(const AExpected, AActual: Integer; const AMessage: string);
begin
  {$IFDEF FPC}
  TAssert.AssertEquals(AMessage, AExpected, AActual);
  {$ELSE}
  Assert.AreEqual(AExpected, AActual, AMessage);
  {$ENDIF}
end;

class procedure ZAssert.AreEqual(const AExpected, AActual: Int64; const AMessage: string);
begin
  {$IFDEF FPC}
  TAssert.AssertEquals(AMessage, AExpected, AActual);
  {$ELSE}
  Assert.AreEqual(AExpected, AActual, AMessage);
  {$ENDIF}
end;

class procedure ZAssert.AreEqual(const AExpected, AActual: string; const AMessage: string);
begin
  {$IFDEF FPC}
  TAssert.AssertEquals(AMessage, AExpected, AActual);
  {$ELSE}
  Assert.AreEqual(AExpected, AActual, AMessage);
  {$ENDIF}
end;

class procedure ZAssert.AreEqual(const AExpected, AActual: Boolean; const AMessage: string);
begin
  {$IFDEF FPC}
  TAssert.AssertEquals(AMessage, AExpected, AActual);
  {$ELSE}
  Assert.AreEqual<Boolean>(AExpected, AActual, AMessage);
  {$ENDIF}
end;

class procedure ZAssert.AreEqual(const AExpected, AActual: Char; const AMessage: string);
begin
  {$IFDEF FPC}
  TAssert.AssertEquals(AMessage, AExpected, AActual);
  {$ELSE}
  { DUnitX hat keine nicht-generische Char-Ueberladung. }
  Assert.AreEqual<Char>(AExpected, AActual, AMessage);
  {$ENDIF}
end;

class procedure ZAssert.AreEqual(const AExpected, AActual: Single; const AMessage: string);
begin
  {$IFDEF FPC}
  { fpcunit prueft ueber Abs(Expected - Actual) <= Delta. Bei zwei gleichen
    unendlichen Werten ergibt die Differenz NaN, und NaN <= 0 ist False - der
    Vergleich schluege fehl, waehrend der generische Comparer von DUnitX ihn
    bestehen laesst. Deshalb vorab auf Bitgleichheit pruefen; erst danach der
    numerische Vergleich mit Delta 0 (exakt, wie unter Delphi). }
  if AExpected = AActual then
    TAssert.AssertTrue(AMessage, True)
  else
    TAssert.AssertEquals(AMessage, AExpected, AActual, 0);
  {$ELSE}
  Assert.AreEqual<Single>(AExpected, AActual, AMessage);
  {$ENDIF}
end;

class procedure ZAssert.IsTrue(const ACondition: Boolean; const AMessage: string);
begin
  {$IFDEF FPC}
  TAssert.AssertTrue(AMessage, ACondition);
  {$ELSE}
  Assert.IsTrue(ACondition, AMessage);
  {$ENDIF}
end;

class procedure ZAssert.IsFalse(const ACondition: Boolean; const AMessage: string);
begin
  {$IFDEF FPC}
  TAssert.AssertFalse(AMessage, ACondition);
  {$ELSE}
  Assert.IsFalse(ACondition, AMessage);
  {$ENDIF}
end;

class procedure ZAssert.Fail(const AMessage: string);
begin
  {$IFDEF FPC}
  TAssert.Fail(AMessage);
  {$ELSE}
  Assert.Fail(AMessage);
  {$ENDIF}
end;

procedure ZRegisterFixture(AClass: TClass);
begin
  {$IFDEF FPC}
  RegisterTest(TTestCaseClass(AClass));
  {$ELSE}
  TDUnitX.RegisterTestFixture(AClass);
  {$ENDIF}
end;

end.
