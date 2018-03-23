unit zint_reedsol;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b complete
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  zint, zint_common;

type
  TRSGlobals = record
    gfpoly : Integer;
    symsize : Integer;    // in bits
    logmod : Integer;    // 2**Globals.symsize - 1
    rlen : Integer;

    logt, alog, rspoly : TArrayOfInteger;
  end;

procedure rs_init_gf(poly : Integer; var Globals : TRSGlobals);
procedure rs_init_code(nsym : Integer; index : Integer; var Globals : TRSGlobals);
procedure rs_encode(len : Integer; const data : TArrayOfByte; var res : TArrayOfByte; var Globals : TRSGlobals);
procedure rs_encode_long(len : Integer; data : TArrayOfCardinal; var res : TArrayOfCardinal; var Globals : TRSGlobals);
procedure rs_free(var Globals : TRSGlobals);

implementation

// It is not written with high efficiency in mind, so is probably
// not suitable for real-time encoding.  The aim was to keep it
// simple, general and clear.
//
// <Some notes on the theory and implementation need to be added here>

// Usage:
// First call rs_init_gf(poly) to set up the Galois Field parameters.
// Then  call rs_init_code(size, index) to set the encoding size
// Then  call rs_encode(datasize, data, out) to encode the data.
//
// These can be called repeatedly as required - but note that
// rs_init_code must be called following any rs_init_gf call.
//
// If the parameters are fixed, some of the statics below can be
// replaced with constants in the obvious way, and additionally
// malloc/free can be avoided by using static arrays of a suitable
// size.


// rs_init_gf(poly) initialises the parameters for the Galois Field.
// The symbol size is determined from the highest bit set in poly
// This implementation will support sizes up to 30 bits (though that
// will result in very large log/antilog tables) - bit sizes of
// 8 or 4 are typical
//
// The poly is the bit pattern representing the GF characteristic
// polynomial.  e.g. for ECC200 (8-bit symbols) the polynomial is
// a**8 + a**5 + a**3 + a**2 + 1, which translates to 0x12d.

procedure rs_init_gf(poly : Integer; var Globals : TRSGlobals);
var
  m, b, p, v : Integer;
begin
  // Find the top bit, and hence the symbol size
  b := 1;
  m := 0;
  while (b <= poly) do
  begin
    Inc(m);
    b := b shl 1;
  end;

  b := b shr 1;
  Dec(m);
  Globals.gfpoly := poly;
  Globals.symsize := m;

  // Calculate the log/Globals.alog tables
  Globals.logmod := (1 shl m) - 1;
  SetLength(Globals.logt, Globals.logmod + 1);
  SetLength(Globals.alog, Globals.logmod);

  p := 1;
  v := 0;
  while (v < Globals.logmod) do
  begin
    Globals.alog[v] := p;
    Globals.logt[p] := v;
    p := p shl 1;
    if (p and b) <> 0 then
      p := p xor poly;
    Inc(v);
  end;
end;

// rs_init_code(nsym, index) initialises the Reed-Solomon encoder
// nsym is the number of symbols to be generated (to be appended
// to the input data).  index is usually 1 - it is the index of
// the constant in the first term (i) of the RS generator polynomial:
// (x + 2**i)*(x + 2**(i+1))*...   [nsym terms]
// For ECC200, index is 1.

procedure rs_init_code(nsym : Integer; index : Integer; var Globals : TRSGlobals);
var
  i, k : Integer;
begin
  SetLength(Globals.rspoly, nsym + 1);

  Globals.rlen := nsym;

  Globals.rspoly[0] := 1;
  for i := 1 to nsym do
  begin
    Globals.rspoly[i] := 1;
    for k := i - 1 downto 1 do
    begin
      if (Globals.rspoly[k] <> 0) then
        Globals.rspoly[k] := Globals.alog[(Globals.logt[Globals.rspoly[k]] + index) mod Globals.logmod];
      Globals.rspoly[k] := Globals.rspoly[k] xor Globals.rspoly[k - 1];
    end;
    Globals.rspoly[0] := Globals.alog[(Globals.logt[Globals.rspoly[0]] + index) mod Globals.logmod];
    Inc(index);
  end;
end;

procedure rs_encode(len : Integer; const data : TArrayOfByte; var res : TArrayOfByte; var Globals : TRSGlobals);
var
  i, k, m : Integer;
begin
  for i := 0 to Globals.rlen - 1 do
    res[i] := 0;
  for i := 0 to len - 1 do
  begin
    m := res[Globals.rlen - 1] xor data[i];
    for k := Globals.rlen - 1 downto 1 do
    begin
      if (m <> 0) and (Globals.rspoly[k] <> 0) then
        res[k] := res[k - 1] xor Globals.alog[(Globals.logt[m] + Globals.logt[Globals.rspoly[k]]) mod Globals.logmod]
      else
        res[k] := res[k - 1];
    end;
    if (m <> 0) and (Globals.rspoly[0] <> 0) then
      res[0] := Globals.alog[(Globals.logt[m] + Globals.logt[Globals.rspoly[0]]) mod Globals.logmod]
    else
      res[0] := 0;
  end;
end;

procedure rs_encode_long(len : Integer; data : TArrayOfCardinal; var res : TArrayOfCardinal; var Globals : TRSGlobals);
{ The same as above but for larger bitlengths - Aztec code compatible }
var
  i, k, m : Integer;
begin
  for i := 0 to Globals.rlen - 1 do
    res[i] := 0;
  for i := 0 to len - 1 do
  begin
    m := res[Globals.rlen - 1] xor data[i];
    for k := Globals.rlen - 1 downto 1 do
    begin
      if (m <> 0) and (Globals.rspoly[k] <> 0) then
        res[k] := res[k - 1] xor Globals.alog[(Globals.logt[m] + Globals.logt[Globals.rspoly[k]]) mod Globals.logmod]
      else
        res[k] := res[k - 1];
    end;
    if (m <> 0) and (Globals.rspoly[0] <> 0) then
      res[0] := Globals.alog[(Globals.logt[m] + Globals.logt[Globals.rspoly[0]]) mod Globals.logmod]
    else
      res[0] := 0;
  end;
end;

procedure rs_free(var Globals : TRSGlobals);
begin { Free memory }
  SetLength(Globals.logt, 0);
  SetLength(Globals.alog, 0);
  SetLength(Globals.rspoly, 0);
end;

end.

