# PDF417 C Test File Analysis  
## Zint Library - backend/tests/test_pdf417.c (v. b3a3c0d)

### Overview
Complete test coverage inventory for PDF417/MicroPDF417 barcode encoding. Total of **10 test functions** with **725 unique test items** across all tests (excluding duplicated test data in some functions tested with both FAST_MODE and standard modes).

---

## Test Function Breakdown

### 1. `test_large` — 12 items (indices 0–11)
**Purpose:** Test maximum input length boundaries and constraints.

**Key Test Case Variations:**
- PDF417 vs MicroPDF417 symbology
- Different input patterns: ASCII letters ('A'), binary bytes ('\200'), numeric ('1')
- Option testing: ECC levels (option_1), columns (option_2), rows (option_3)
- Option constraints and interactions (cols too small, rows too small/large)

**Data Structure:**
```
symbology | option_1 | option_2 | option_3 | pattern | length | ret | expected_rows | expected_width | expected_errtxt
```

**Critical Assertions:**
- Return code: Success (0) vs Error conditions (ZINT_ERROR_TOO_LONG, ZINT_WARN_INVALID_OPTION)
- Symbol dimensions: rows, width
- Error message exact match

**Special Conditions:**
- **PDF417:** Max 1850 chars (ASCII), 1108 bytes (binary), 2710 numeric
- **MicroPDF417:** Max 250 chars
- Both modes tested with and without encoding mode FAST_MODE
- Row/column boundary violations with warning escalation

**Example Items:**
| Index | Content | Max Length | Expected Result |
|-------|---------|-----------|-----------------|
| 0 | "A" × 1850 | 1850 | 0 (success, 32 rows, 562 width) |
| 1 | "A" × 1851 | 1851 | ZINT_ERROR_TOO_LONG (928 codeword max) |
| 4 | "1" × 2710 | 2710 | 0 (numeric mode, success) |
| 10 | "A" × 250 (MicroPDF417) | 250 | 0 (44 rows, 99 width) |
| 11 | "A" × 251 (MicroPDF417) | 251 | ZINT_ERROR_TOO_LONG (127 codeword max) |

---

### 2. `test_options` — 51 items (indices 0–50)
**Purpose:** Comprehensive option validation (ECC levels, columns, rows, Structured Append).

**Key Test Case Variations:**
- ECC levels (option_1): 0–8, -1 (auto), invalid (9+)
- Columns (option_2): 1–30, -1 (auto), invalid (31+)
- Rows (option_3): 3–90, -1 (auto), invalid (1–2, 91+)
- Interaction constraints: cols × rows ≤ 928 codewords
- Structured Append: index/count validation, ID field (up to 30 digits, triplet format 000–899)
- Warning level escalation (WARN_FAIL_ALL)

**Data Structure:**
```
symbology | option_1 | option_2 | option_3 | warn_level | structapp | data | ret_encode | ret_vector | expected_rows | expected_width | expected_errtxt | expected_option_1 | expected_option_2 | expected_option_3 | compare_previous
```

**Critical Assertions:**
- Return code (success with/without warning, error)
- Adjusted dimensions (rows/width after auto-correction)
- Final option values (option_1/2/3 after auto-setting)
- Option range validation against spec limits
- Structured Append specific: index out of range, count limits (2–99999), ID format

**Special Conditions – PDF417:**
- Both PDF417 and MicroPDF417 options tested
- MicroPDF417 cannot specify rows (option_3 → Error 476)
- Column auto-upping with warning (746, 748)
- ECC auto-setting from -1
- Structured Append adds terminator codeword overhead (mode 2 = last segment)

**Example Items:**
| Index | ECC | Cols | Rows | Data | Result |
|-------|-----|------|------|------|--------|
| 0 | -1 | -1 | -1 | "12345" | 0 (ECC→2, cols→2, rows→6) |
| 5 | 8 | 2 | -1 | "12345" | ZINT_WARN_INVALID_OPTION (cols upped 2→6) |
| 10 | 9 | -1 | -1 | "12345" | ZINT_WARN_INVALID_OPTION (ECC 9 out of range) |
| 34–37 | -1 | 1 | -1 | Long text | ZINT_WARN_INVALID_OPTION (cols auto-upped 1→2/3 with Struct Append) |
| 45–49 | -1 | 1 | -1 | — | ZINT_ERROR_INVALID_OPTION (Struct Append ID validation) |
| 50 | — | — | — | — | MicroPDF417 Struct Append ID length error |

---

### 3. `test_reader_init` — 2 items (indices 0–1)
**Purpose:** Test READER_INIT output mode and ZXing-C++ decoder validation.

**Key Test Case Variations:**
- READER_INIT option set
- Both PDF417 and MicroPDF417

**Data Structure:**
```
symbology | input_mode | output_options | data | ret | expected_rows | expected_width | expected (codeword array) | comment
```

**Critical Assertions:**
- Return code (0 for success)
- Dimensions (rows, width)
- Codeword dump in errtxt: `(count) cw1 cw2 ... cwN`
- ZXing-C++ cross-validation if enabled (debug flag)

**Special Conditions:**
- Tests "Test Alpha flag 900" codeword (specific to reader initialization)
- Full codeword sequence output requires `symbol->debug = ZINT_DEBUG_TEST`

**Example Items:**
| Index | Symbology | Data | Rows | Width | Codewords |
|-------|-----------|------|------|-------|-----------|
| 0 | PDF417 | "A" | 6 | 103 | (12) 4 921 29 900 209 917 … |
| 1 | MicroPDF417 | "A" | 11 | 38 | (11) 921 900 29 … |

---

### 4. `test_input` — 84 items (indices 0–83)
**Purpose:** Test ECI (Extended Channel Interpretation), multiple encodation modes, and GS1/HIBC modes.

**Key Test Case Variations:**
- **ECI Handling:**
  - Explicit ECI codes (3, 26, 9, 899, 900, 810899, 810900, 811799, 811800 out of range)
  - Auto-detect with ZINT_WARN_USES_ECI (e.g., Unicode chars needing specific ECIs)
  - ECI vs explicit spec conflicts
- **Input Modes:** UNICODE_MODE, UNICODE_MODE | FAST_MODE, DATA_MODE
- **Encoding Variations:** 
  - Latin-1 (é U+00E9, byte 233)
  - ISO 8859-7 Greek (β U+03B2, ECI 9)
  - Numeric, byte, and mixed content
  - Braced text (e.g., "{}", "{}  C#+")
- **Symbology:** PDF417, MicroPDF417, HIBC_PDF, HIBC_MICPDF
- **GS1 & Structured Append:** Multi-segment encoding with ECI per segment

**Data Structure:**
```
symbology | input_mode | eci | option_1 | option_2 | structapp | data | ret | expected_eci | expected_rows | expected_width | expected (codeword array) | bwipp_cmp | comment
```

**Critical Assertions:**
- Return code (0 success, ZINT_WARN_USES_ECI, ZINT_ERROR_INVALID_DATA, etc.)
- ECI flag and value in symbol
- Dimensions (rows, width)
- Codeword output (codeword dump for detection verification)
- BWIPP/ZXing-C++ compatibility flag (some entries differ from BWIPP)

**Special Conditions:**
- **ECI bounds:** Valid 0–811799; 811800+ = Error 472
- **ECI range codes:**
  - 0–127: Direct
  - 128–16383: Extended (encodes as 926 + 2 values)
  - 16384+: Extended (encodes as 926 + 3 values)
- **Invalid ECI + Data:** Error 244 (e.g., ECI 3 Latin + Beta char)
- **HIBC Mode:** Restricted character set (alphanumerics, space, "-.$/+%")
- **Fast Mode Optimization:** Numeric encoding comparison (FAST_MODE often produces smaller codeword count)

**Example Items:**
| Index | Input | ECI | Mode | Result | Rows/Width |
|-------|-------|-----|------|--------|-----------|
| 0 | "é" | -1 | UNICODE\|FAST | 0 (no ECI needed) | 6/103 |
| 2 | "é" | 3 | UNICODE\|FAST | 0 (explicit Latin-1) | 7/103 |
| 8–9 | "β" | -1 | UNICODE(\|FAST) | ZINT_WARN_USES_ECI (Greek auto-detect) | 7/103 |
| 11–20 | "A" | 899–810900 | UNICODE(\|FAST) | 0 (ECI-specific tests) | varies |
| 46–49 | Mixed "{}" | -1 | UNICODE(\|FAST) | 0 (different encodation paths) | varies |
| 63–66 | "A" + Struct Append | 8 | UNICODE | 0 (multi-segment ECI) | 41/290 |
| 71–77 | — | — | UNICODE | 0 (MicroPDF417 multi-seg) | varies |
| 78–83 | "123456…" | -1 | DATA(\|FAST) | 0 (numeric byte encoding) | varies |

---

### 5. `test_encode` — 211 items (indices 0–210)
**Purpose:** Comprehensive encoding tests with binary module output validation.

**Key Test Case Variations:**
- **Symbology:** PDF417, MicroPDF417, PDF417COMP (compact), HIBC_PDF (all main variants)
- **ECI:** Default (-1), specific (e.g., 29, 899)
- **Input Modes:** UNICODE_MODE, UNICODE_MODE | FAST_MODE, DATA_MODE
- **Encodation Strategies:** Different codeword count paths
- **Options:** ECC levels (option_1: 0–8), columns (varies by mode)
- **Data Patterns:** 
  - Text (ISO examples from spec)
  - Currency symbols (￥, €, $)
  - Mixed encodation (text + brackets + numeric)
  - Scale/CJK characters

**Data Structure:**
```
symbology | eci | input_mode | option_1 | option_2 | option_3 | data | ret | expected_rows | expected_width | bwipp_cmp | zxingcpp_cmp | comment | expected (binary module string)
```

**Critical Assertions:**
- Return code (0 for success)
- Dimensions (rows, width)
- **Exact binary module data:** Each row as bit string; used for cross-validation
- BWIPP compatibility (flag for known differences)
- ZXing-C++ decoder compatibility

**Special Conditions:**
- **ISO 15438:2015 Reference Examples:** Items include official spec examples
- **Encodation Differences:** FAST_MODE may produce different (optimized) codeword sequences but same output size
- **Compact Mode (PDF417COMP):** Reduced column width vs standard PDF417
- **HIBC Variants:** Character set restrictions affect encodation
- **Module Dump Format:** Full binary representation for bit-level verification
- **Cross-Tool Validation:** BWIPP (mark 0) when differences intentional; ZXing-C++ decoder confirmation

**Example Items (First Few):**
| Index | Symbology | Data | Rows | Width | Mode | Comment |
|-------|-----------|------|------|-------|------|---------|
| 0–1 | PDF417 | "PDF417 Symbology Standard" | 10 | 103 | UNICODE(\|FAST) | ISO 15438:2015 Fig 1 |
| 2–3 | PDF417 | "PDF417" | 5 | 103 | UNICODE(\|FAST) | ISO 15438:2015 Annex Q ECC example |
| 210 | PDF417 | "￥3149.79" | 10 | 103 | UNICODE | BWIPP BYTE1 (non-standard encodation) |

---

### 6. `test_encode_segs` — 44 items (indices 0–43)
**Purpose:** Multi-segment encoding with per-segment ECI and BARCODE_CONTENT_SEGS flag.

**Key Test Case Variations:**
- **Segment ECI:** Each segment can specify its own ECI (0–3 supported)
- **Input Modes:** UNICODE_MODE ± FAST_MODE, DATA_MODE
- **Symbology:** PDF417, MicroPDF417, PDF417COMP, HIBC variants
- **Structured Append:** Multi-segment with ID field
- **Mixed Character Sets:** Greek (Cyrillic Ж U+0416), Pilcrow (¶ U+00B6), CJK (Japanese/Chinese), etc.

**Data Structure:**
```
symbology | input_mode | option_1 | option_2 | option_3 | structapp | segs[3] (TU(text), length, eci) | ret | expected_rows | expected_width | bwipp_cmp | comment | expected (binary)
```
where `segs` are:
```
{ text, length (-1 for strlen), eci }
```

**Critical Assertions:**
- Return code
- Dimensions
- Binary module data
- BARCODE_CONTENT_SEGS flag behavior: triggers multi-segment output

**Special Conditions:**
- **Segment Structure:** Array of up to 3 segments; empty segment terminates list (text=NULL or length=0)
- **ECI per Segment:** Allows ISO 8859-7 Greek (9) + UTF-8 (0) mixing
- **Compact Mode (PDF417COMP):** Narrower width (~69 vs 103)
- **MicroPDF417:** Limited capacity; same segments produce different row/column layout
- **Auto-ECI:** ZINT_WARN_USES_ECI when implicit ECI detection needed
- **Structured Append:** Index/count/ID passed via structapp struct

**Example Items:**
| Index | Symbolgy | Seg1 / ECI | Seg2 / ECI | Seg3 / ECI | Rows | Width | Note |
|-------|----------|-----------|-----------|-----------|------|-------|------|
| 0–1 | PDF417 | "¶" / 0 | "Ж" / 7 | — / 0 | 8 | 103 | Standard Cyrillic mix |
| 8–9 | PDF417 | "$439.97" / 3 | "￥3149.79" / 29 | "Produkt:…€" / 17 | 10 | 137 | AIM ITS/04-023:2022 Annex A (shortened) |
| 12–13 | PDF417 | Long text / 3, 29, 17 | — | — | 23 | 188 | Full AIM example (3 ECIs) |
| 30–31 | MicroPDF417 | Long multi-seg | — | — | 44 | 99 | MicroPDF417 variant of AIM example |
| 42–43 | HIBC_* | Segments with HIBC prefix | — | — | varies | — | HIBC restriction testing |

---

### 7. `test_rt` — 26 items (indices 0–25)
**Purpose:** Round-trip encoding/decoding validation with BARCODE_CONTENT_SEGS.

**Key Test Case Variations:**
- **ECI Specifications:** Explicit ECI vs auto-detect
- **Input Modes:** UNICODE_MODE ± FAST_MODE, DATA_MODE
- **Symbology:** All PDF417 variants (PDF417, MicroPDF417, PDF417COMP, HIBC_PDF, HIBC_MICPDF)
- **Round-Trip Content:** Text stored and retrieved via BARCODE_CONTENT_SEGS

**Data Structure:**
```
symbology | input_mode | eci | data | ret | expected_eci | expected_rows | expected_width | comment
```

**Critical Assertions:**
- Return code (0 for success, ZINT_WARN_USES_ECI for auto-detect)
- Symbol ECI value
- Dimensions
- Content retrieval capability (set BARCODE_CONTENT_SEGS flag to extract segments back)

**Special Conditions:**
- **Round-Trip:** Text encoded, then symbol's `content_segs` re-extracted to verify integrity
- **HIBC Modes:** Data prepended with '+' and appended with 'D' check digit
- **ECI Persistence:** ECI encoded in symbol, available for decode verification

**Example Items:**
| Index | Symbology | Data | ECI | Result | Rows/Width |
|-------|-----------|------|-----|--------|-----------|
| 0 | PDF417 | "é" | -1 | 0 (no ECI) | 6/103 |
| 1 | PDF417 | "é" | -1 (CONTENT_SEGS) | 0 (retrieves "é") | 6/103 |
| 10–11 | MicroPDF417 | "é" | 26 (explicit) | 0 / 0 (CONTENT_SEGS) | 11/38 |
| 24–25 | HIBC_MICPDF | "H123ABC01234567890" | -1 | 0 / 0 (CONTENT_SEGS) | — |

---

### 8. `test_rt_segs` — 14 items (indices 0–13)
**Purpose:** Round-trip multi-segment encoding/decoding with BARCODE_CONTENT_SEGS.

**Key Test Case Variations:**
- **Multi-Segment:** Up to 3 segments total
- **ECI per Segment:** Segments may have different ECIs
- **Input Modes:** UNICODE_MODE, DATA_MODE
- **Symbology:** PDF417, PDF417COMP, MicroPDF417

**Data Structure:**
```
symbology | input_mode | output_opts (flags) | segs[3] (TU(text), length, eci) | ret | expected_rows | expected_width | expected_segs (populated when CONTENT_SEGS set) | num_segs
```

**Critical Assertions:**
- Return code
- Dimensions
- Output segment array: Text restored, byte count, ECI per segment (when CONTENT_SEGS flag set)
- Segment count match

**Special Conditions:**
- **UTF-8 Handling:** Both UNICODE_MODE (UTF-8 text) and DATA_MODE (byte sequences)
- **ECI Encoding:** Each segment's ECI encoded; roundtrip retrieves it
- **Output Segs:** When BARCODE_CONTENT_SEGS set on encode, output reflects input structure

**Example Items:**
| Index | Symbolgy | Seg Details | Ret | Rows/Width | Note |
|-------|----------|-------------|-----|-----------|------|
| 0–1 | PDF417 | Seg1: "¶" ECI0, Seg2: "Ж" ECI7 | 0 / 0 | 8/103 | Cyrillic + Pilcrow |
| 2–3 | PDF417 | Seg1-3: Multi-ECI (3,9,20) | WARN / WARN | 8/120 | Multiple ECIs auto-detect |
| 4–5 | PDF417 | DATA_MODE segs | 0 / 0 | 8/120 | Raw byte segments |
| 10–11 | MicroPDF417 | Seg1-3: Multi-ECI | WARN / WARN | 24/38 | MicroPDF417 multi-ECI |

---

### 9. `test_fuzz` — 301 items (indices 0–300)
**Purpose:** Fuzzing/stress testing with malformed/random input data.

**Key Test Case Variations:**
- **Symbology:** PDF417, MicroPDF417
- **Input Modes:** DATA_MODE ± FAST_MODE, often with binary/escaped content
- **Data Patterns:** 
  - Very long binary sequences (up to 2690+ bytes)
  - Invalid/malformed UTF-8
  - Repeated escape sequences, null bytes, control characters
  - OSS-Fuzz-discovered crash triggers
- **Negative Testing:** Expect ZINT_ERROR_TOO_LONG, crashes prevented

**Data Structure:**
```
symbology | input_mode | option_1 | option_2 | data | length | ret | bwipp_cmp | zxingcpp_cmp | comment
```

**Critical Assertions:**
- Return code (error or success as expected)
- No crash/segfault
- BWIPP/ZXing-C++ compatibility check (if applicable)

**Special Conditions:**
- **OSS-Fuzz Triggers:** Indices 0–1 are original fuzzer-discovered crash cases
- **Coverage:** Tests for buffer overflows, stack issues, invalid state transitions
- **Mostly Error Cases:** Many expect ZINT_ERROR_TOO_LONG
- **No Dimension Checks:** Focus is on non-crash behavior

**Example Items:**
| Index | Symbolgy | Data Type | Length | Expected Result | Note |
|-------|----------|-----------|--------|-----------------|------|
| 0–1 | PDF417 | Binary fuzz (octal escapes) | 1001 | ZINT_ERROR_TOO_LONG | Original OSS-Fuzz #10 case |
| 2–99 | Mixed | Varied binary patterns | — | Various errors | Fuzz iteration set |
| 300 | PDF417 | Binary escape sequence | 2690 | ZINT_ERROR_TOO_LONG | André Maute contribution |

---

### 10. `test_numbprocess` — 52 items (indices 0–51)
**Purpose:** Numeric mode encoding/codeword generation internals.

**Key Test Case Variations:**
- **Input:** Numeric strings only (0–9)
- **Length Variants:** 1–44 digits, covering single to 16-codeword encoding

**Data Structure:**
```
data (numeric string) | expected_len (number of codewords) | expected[] (codeword array)
```

**Critical Assertions:**
- Expected codeword count
- Individual codeword values match (array comparison)
- No dimension/structure output; internal encoder testing

**Special Conditions:**
- **Codeword Format:** First (cluster) codeword is always 902 (numeric mode indicator)
- **Digit Grouping:** Numeric mode encodes digits in 3-digit/2-digit/1-digit clusters
  - Odd-length strings pad group structure accordingly
  - E.g., "000" → single codeword 100; "123" → single codeword 223
- **Codeword Math:** Value = (d1 × 10 + d2) × 10 + d3 for 3-digit groups; simpler for 2/1

**Example Items:**
| Index | Input | Expected Codewords | Note |
|-------|-------|-------------------|------|
| 0 | "1" | [902, 11] | Single digit: 902 (mode) + 11 (1) |
| 4 | "000" | [902, 1, 100] | 3 digits: 902 + codeword(000)=1 + codeword(00)=100 |
| 12 | "123456" | [902, 1, 348, 256] | 6 digits: 902 + cw(123) + cw(456) |
| 50 | "12345678901234567890123456789012345678901234" | [902, 491, 81, 137, …] | Long string: multiple 3-digit clusters |

---

## Summary Statistics

| Metric | Value |
|--------|-------|
| **Total Test Functions** | 10 |
| **Total Test Items** | 725 |
| **Largest Test** | test_fuzz (301 items) |
| **Medium Tests** | test_encode (211), test_input (84), test_options (51) |
| **Encoding Tests** | test_encode_segs (44), test_options (51) |
| **Round-Trip Tests** | test_rt (26), test_rt_segs (14) |
| **Feature Tests** | test_large (12), test_reader_init (2) |
| **Direct Encoder Tests** | test_numbprocess (52) |

---

## Critical Assertions Across All Tests

### Universal Assertions
1. **Return Code** – Match expected ret value; escalate from 0 (success) → ZINT_WARN_* → ZINT_ERROR_*
2. **Symbol Dimensions** – Verify rows and width against expected values
3. **Error Text** – Exact string match for `errtxt` field
4. **No Crashes** – All tests run to completion without segfault/heap corruption

### Encoding-Specific Assertions
5. **ECI Value** – Match expected ECI; 0 if none, specific if auto-detected or explicit
6. **Codeword Output** – Exact codeword array or binary module string
7. **Option Persistence** – Final option_1/2/3 values after auto-correction
8. **Structured Append** – Index, count, ID validation

### Cross-Tool Assertions
9. **BWIPP Compatibility** – Module dump comparison (when bwipp_cmp == 1)
10. **ZXing-C++ Decodability** – Decoder output match (when zxingcpp_cmp == 1)

---

## Deltas & Known Divergences from C Behavior

### Common BWIPP Differences (Intentional)
- **Encodation Strategy:** FAST_MODE vs standard may split numeric/alpha differently
  - e.g., Item test_input#46: FAST_MODE produces 3-codeword savings; BWIPP different path
- **Module String:** Binary representation matches visually but codeword sequence differs
  - Comment: "BWIPP uses different encodation, same codeword count"

### Known Limitations (Test-Level Deltas)
- Some test items marked `bwipp_cmp = 0`: Known BWIPP incompatibility, intentional (documented in `comment`)
- test_encode_segs items 8–9, 12–13, 30–31 note BWIPP encodation divergence

### Zint-Specific Extensions
- ECI handling (911800+ range) vs standard PDF417 spec
- Structured Append triplet validation (000–899 per triplet)
- HIBC mode variants (HIBC_PDF, HIBC_MICPDF) specific to Zint

---

## Recommendations for Delphi Port

### Immediate Port Priority
1. **test_large** → Boundary validation (core constraint logic)
2. **test_options** → Option validation & auto-correction
3. **test_input** → ECI handling (complex, critical)
4. **test_encode** → Full encoding pipeline with 211 reference cases
5. **test_numbprocess** → Internal numeric encoding (isolated logic, high confidence)

### Sequential Port Strategy
- Port test_large & test_options → Validate option validation layer
- Port test_numbprocess → Validate internal codeword generation
- Port test_input → Validate ECI & mode handling
- Port test_encode → Full integration test with binary reference
- Port test_rt / test_rt_segs → Content round-trip verification
- Port test_encode_segs → Multi-segment encoding (depends on test_input)
- Port test_reader_init → Advanced feature (low priority initially)
- Port test_fuzz → Regression/stress (final validation phase)

### Delphi-Specific Gaps to Address
- **ECI Encoding Range:** 0–811799 (extend standard spec)
- **Codeword Math:** Verify 3-digit numeric cluster encoding exactly matches C reference
- **Segment Structure:** TZintSegment equivalent for multi-segment tests
- **Binary Module Format:** Ensure bit-string representation matches C dump format
- **Structured Append ID:** Triplet validation logic (3-digit groups, 000–899 range)

---

## Cross-Reference Index

### By Feature
- **ECI Tests:** test_input (items 2–21), test_encode_segs (items 2–3, 6–13), test_rt (items 8–11), test_rt_segs (items 2–3, 10–11)
- **Structured Append:** test_options (items 34–50), test_input (items 63–77), test_encode_segs (items 20–21, 30–31)
- **MicroPDF417:** test_large (items 10–11), test_options (items 24–32), throughout most tests paired with PDF417
- **Binary Encoding:** test_encode (211 items), test_encode_segs (44 items)
- **Round-Trip:** test_rt (26 items), test_rt_segs (14 items)

### By Symbology
- **PDF417:** Primary focus; all tests
- **MicroPDF417:** test_large (2), test_options (9), test_reader_init (1), test_input (43 split + 6 HIBC), test_encode (~60), test_encode_segs (11), test_rt (6), test_rt_segs (3), test_fuzz (~50)
- **HIBC_PDF / HIBC_MICPDF:** Subset of test_input, test_rt, test_encode_segs

---

## Execution Notes

### Test Harness Behavior
- **testContinue()** hook allows selective test skip by index
- **testUtilCanBwipp() / testUtilCanZXingCPP()** conditional cross-validation (debug flags)
- **FAST_MODE** paired tests: Most encoding tests run twice (with & without FAST_MODE) to verify optimization consistency
- **symbol->debug = ZINT_DEBUG_TEST** required for codeword dump in test_reader_init and test_input

### Build & Run Commands (C Reference)
```bash
# Compile test
gcc -o test_pdf417 test_pdf417.c [zint lib] [zint tests common]

# Run all tests
./test_pdf417

# Run selective test (e.g., test_large)
./test_pdf417 test_large

# Run with debug output
./test_pdf417 test_input --debug-print
```

---

**Analysis Complete**  
**Last Updated:** March 23, 2026  
**Zint Version:** b3a3c0d (master-2026-03-13)
