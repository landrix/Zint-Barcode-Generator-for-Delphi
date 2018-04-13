unit zint_imail;

{
  Based on Zint (done by Robin Stuart and the Zint team)
  http://github.com/zint/zint

  Translation by TheUnknownOnes
  http://theunknownones.net

  License: Apache License 2.0

  Status:
    3432bc9aff311f2aea40f0e9883abfe6564c080b work in progress
}

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, zint;

function imail(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;

implementation

uses zint_common, zint_large, zint_helper;

const SODIUM = '0123456789-';

{ The following lookup tables were generated using the code in Appendix C }

const AppxD_I : array[0..1286] of Word = ( { Appendix D Table 1 - 5 of 13 characters }
	$001F, $1F00, $002F, $1E80, $0037, $1D80, $003B, $1B80, $003D, $1780,
	$003E, $0F80, $004F, $1E40, $0057, $1D40, $005B, $1B40, $005D, $1740,
	$005E, $0F40, $0067, $1CC0, $006B, $1AC0, $006D, $16C0, $006E, $0EC0,
	$0073, $19C0, $0075, $15C0, $0076, $0DC0, $0079, $13C0, $007A, $0BC0,
	$007C, $07C0, $008F, $1E20, $0097, $1D20, $009B, $1B20, $009D, $1720,
	$009E, $0F20, $00A7, $1CA0, $00AB, $1AA0, $00AD, $16A0, $00AE, $0EA0,
	$00B3, $19A0, $00B5, $15A0, $00B6, $0DA0, $00B9, $13A0, $00BA, $0BA0,
	$00BC, $07A0, $00C7, $1C60, $00CB, $1A60, $00CD, $1660, $00CE, $0E60,
	$00D3, $1960, $00D5, $1560, $00D6, $0D60, $00D9, $1360, $00DA, $0B60,
	$00DC, $0760, $00E3, $18E0, $00E5, $14E0, $00E6, $0CE0, $00E9, $12E0,
	$00EA, $0AE0, $00EC, $06E0, $00F1, $11E0, $00F2, $09E0, $00F4, $05E0,
	$00F8, $03E0, $010F, $1E10, $0117, $1D10, $011B, $1B10, $011D, $1710,
	$011E, $0F10, $0127, $1C90, $012B, $1A90, $012D, $1690, $012E, $0E90,
	$0133, $1990, $0135, $1590, $0136, $0D90, $0139, $1390, $013A, $0B90,
	$013C, $0790, $0147, $1C50, $014B, $1A50, $014D, $1650, $014E, $0E50,
	$0153, $1950, $0155, $1550, $0156, $0D50, $0159, $1350, $015A, $0B50,
	$015C, $0750, $0163, $18D0, $0165, $14D0, $0166, $0CD0, $0169, $12D0,
	$016A, $0AD0, $016C, $06D0, $0171, $11D0, $0172, $09D0, $0174, $05D0,
	$0178, $03D0, $0187, $1C30, $018B, $1A30, $018D, $1630, $018E, $0E30,
	$0193, $1930, $0195, $1530, $0196, $0D30, $0199, $1330, $019A, $0B30,
	$019C, $0730, $01A3, $18B0, $01A5, $14B0, $01A6, $0CB0, $01A9, $12B0,
	$01AA, $0AB0, $01AC, $06B0, $01B1, $11B0, $01B2, $09B0, $01B4, $05B0,
	$01B8, $03B0, $01C3, $1870, $01C5, $1470, $01C6, $0C70, $01C9, $1270,
	$01CA, $0A70, $01CC, $0670, $01D1, $1170, $01D2, $0970, $01D4, $0570,
	$01D8, $0370, $01E1, $10F0, $01E2, $08F0, $01E4, $04F0, $01E8, $02F0,
	$020F, $1E08, $0217, $1D08, $021B, $1B08, $021D, $1708, $021E, $0F08,
	$0227, $1C88, $022B, $1A88, $022D, $1688, $022E, $0E88, $0233, $1988,
	$0235, $1588, $0236, $0D88, $0239, $1388, $023A, $0B88, $023C, $0788,
	$0247, $1C48, $024B, $1A48, $024D, $1648, $024E, $0E48, $0253, $1948,
	$0255, $1548, $0256, $0D48, $0259, $1348, $025A, $0B48, $025C, $0748,
	$0263, $18C8, $0265, $14C8, $0266, $0CC8, $0269, $12C8, $026A, $0AC8,
	$026C, $06C8, $0271, $11C8, $0272, $09C8, $0274, $05C8, $0278, $03C8,
	$0287, $1C28, $028B, $1A28, $028D, $1628, $028E, $0E28, $0293, $1928,
	$0295, $1528, $0296, $0D28, $0299, $1328, $029A, $0B28, $029C, $0728,
	$02A3, $18A8, $02A5, $14A8, $02A6, $0CA8, $02A9, $12A8, $02AA, $0AA8,
	$02AC, $06A8, $02B1, $11A8, $02B2, $09A8, $02B4, $05A8, $02B8, $03A8,
	$02C3, $1868, $02C5, $1468, $02C6, $0C68, $02C9, $1268, $02CA, $0A68,
	$02CC, $0668, $02D1, $1168, $02D2, $0968, $02D4, $0568, $02D8, $0368,
	$02E1, $10E8, $02E2, $08E8, $02E4, $04E8, $0307, $1C18, $030B, $1A18,
	$030D, $1618, $030E, $0E18, $0313, $1918, $0315, $1518, $0316, $0D18,
	$0319, $1318, $031A, $0B18, $031C, $0718, $0323, $1898, $0325, $1498,
	$0326, $0C98, $0329, $1298, $032A, $0A98, $032C, $0698, $0331, $1198,
	$0332, $0998, $0334, $0598, $0338, $0398, $0343, $1858, $0345, $1458,
	$0346, $0C58, $0349, $1258, $034A, $0A58, $034C, $0658, $0351, $1158,
	$0352, $0958, $0354, $0558, $0361, $10D8, $0362, $08D8, $0364, $04D8,
	$0383, $1838, $0385, $1438, $0386, $0C38, $0389, $1238, $038A, $0A38,
	$038C, $0638, $0391, $1138, $0392, $0938, $0394, $0538, $03A1, $10B8,
	$03A2, $08B8, $03A4, $04B8, $03C1, $1078, $03C2, $0878, $03C4, $0478,
	$040F, $1E04, $0417, $1D04, $041B, $1B04, $041D, $1704, $041E, $0F04,
	$0427, $1C84, $042B, $1A84, $042D, $1684, $042E, $0E84, $0433, $1984,
	$0435, $1584, $0436, $0D84, $0439, $1384, $043A, $0B84, $043C, $0784,
	$0447, $1C44, $044B, $1A44, $044D, $1644, $044E, $0E44, $0453, $1944,
	$0455, $1544, $0456, $0D44, $0459, $1344, $045A, $0B44, $045C, $0744,
	$0463, $18C4, $0465, $14C4, $0466, $0CC4, $0469, $12C4, $046A, $0AC4,
	$046C, $06C4, $0471, $11C4, $0472, $09C4, $0474, $05C4, $0487, $1C24,
	$048B, $1A24, $048D, $1624, $048E, $0E24, $0493, $1924, $0495, $1524,
	$0496, $0D24, $0499, $1324, $049A, $0B24, $049C, $0724, $04A3, $18A4,
	$04A5, $14A4, $04A6, $0CA4, $04A9, $12A4, $04AA, $0AA4, $04AC, $06A4,
	$04B1, $11A4, $04B2, $09A4, $04B4, $05A4, $04C3, $1864, $04C5, $1464,
	$04C6, $0C64, $04C9, $1264, $04CA, $0A64, $04CC, $0664, $04D1, $1164,
	$04D2, $0964, $04D4, $0564, $04E1, $10E4, $04E2, $08E4, $0507, $1C14,
	$050B, $1A14, $050D, $1614, $050E, $0E14, $0513, $1914, $0515, $1514,
	$0516, $0D14, $0519, $1314, $051A, $0B14, $051C, $0714, $0523, $1894,
	$0525, $1494, $0526, $0C94, $0529, $1294, $052A, $0A94, $052C, $0694,
	$0531, $1194, $0532, $0994, $0534, $0594, $0543, $1854, $0545, $1454,
	$0546, $0C54, $0549, $1254, $054A, $0A54, $054C, $0654, $0551, $1154,
	$0552, $0954, $0561, $10D4, $0562, $08D4, $0583, $1834, $0585, $1434,
	$0586, $0C34, $0589, $1234, $058A, $0A34, $058C, $0634, $0591, $1134,
	$0592, $0934, $05A1, $10B4, $05A2, $08B4, $05C1, $1074, $05C2, $0874,
	$0607, $1C0C, $060B, $1A0C, $060D, $160C, $060E, $0E0C, $0613, $190C,
	$0615, $150C, $0616, $0D0C, $0619, $130C, $061A, $0B0C, $061C, $070C,
	$0623, $188C, $0625, $148C, $0626, $0C8C, $0629, $128C, $062A, $0A8C,
	$062C, $068C, $0631, $118C, $0632, $098C, $0643, $184C, $0645, $144C,
	$0646, $0C4C, $0649, $124C, $064A, $0A4C, $0651, $114C, $0652, $094C,
	$0661, $10CC, $0662, $08CC, $0683, $182C, $0685, $142C, $0686, $0C2C,
	$0689, $122C, $068A, $0A2C, $0691, $112C, $0692, $092C, $06A1, $10AC,
	$06A2, $08AC, $06C1, $106C, $06C2, $086C, $0703, $181C, $0705, $141C,
	$0706, $0C1C, $0709, $121C, $070A, $0A1C, $0711, $111C, $0712, $091C,
	$0721, $109C, $0722, $089C, $0741, $105C, $0742, $085C, $0781, $103C,
	$0782, $083C, $080F, $1E02, $0817, $1D02, $081B, $1B02, $081D, $1702,
	$081E, $0F02, $0827, $1C82, $082B, $1A82, $082D, $1682, $082E, $0E82,
	$0833, $1982, $0835, $1582, $0836, $0D82, $0839, $1382, $083A, $0B82,
	$0847, $1C42, $084B, $1A42, $084D, $1642, $084E, $0E42, $0853, $1942,
	$0855, $1542, $0856, $0D42, $0859, $1342, $085A, $0B42, $0863, $18C2,
	$0865, $14C2, $0866, $0CC2, $0869, $12C2, $086A, $0AC2, $0871, $11C2,
	$0872, $09C2, $0887, $1C22, $088B, $1A22, $088D, $1622, $088E, $0E22,
	$0893, $1922, $0895, $1522, $0896, $0D22, $0899, $1322, $089A, $0B22,
	$08A3, $18A2, $08A5, $14A2, $08A6, $0CA2, $08A9, $12A2, $08AA, $0AA2,
	$08B1, $11A2, $08B2, $09A2, $08C3, $1862, $08C5, $1462, $08C6, $0C62,
	$08C9, $1262, $08CA, $0A62, $08D1, $1162, $08D2, $0962, $08E1, $10E2,
	$0907, $1C12, $090B, $1A12, $090D, $1612, $090E, $0E12, $0913, $1912,
	$0915, $1512, $0916, $0D12, $0919, $1312, $091A, $0B12, $0923, $1892,
	$0925, $1492, $0926, $0C92, $0929, $1292, $092A, $0A92, $0931, $1192,
	$0932, $0992, $0943, $1852, $0945, $1452, $0946, $0C52, $0949, $1252,
	$094A, $0A52, $0951, $1152, $0961, $10D2, $0983, $1832, $0985, $1432,
	$0986, $0C32, $0989, $1232, $098A, $0A32, $0991, $1132, $09A1, $10B2,
	$09C1, $1072, $0A07, $1C0A, $0A0B, $1A0A, $0A0D, $160A, $0A0E, $0E0A,
	$0A13, $190A, $0A15, $150A, $0A16, $0D0A, $0A19, $130A, $0A1A, $0B0A,
	$0A23, $188A, $0A25, $148A, $0A26, $0C8A, $0A29, $128A, $0A2A, $0A8A,
	$0A31, $118A, $0A43, $184A, $0A45, $144A, $0A46, $0C4A, $0A49, $124A,
	$0A51, $114A, $0A61, $10CA, $0A83, $182A, $0A85, $142A, $0A86, $0C2A,
	$0A89, $122A, $0A91, $112A, $0AA1, $10AA, $0AC1, $106A, $0B03, $181A,
	$0B05, $141A, $0B06, $0C1A, $0B09, $121A, $0B11, $111A, $0B21, $109A,
	$0B41, $105A, $0B81, $103A, $0C07, $1C06, $0C0B, $1A06, $0C0D, $1606,
	$0C0E, $0E06, $0C13, $1906, $0C15, $1506, $0C16, $0D06, $0C19, $1306,
	$0C23, $1886, $0C25, $1486, $0C26, $0C86, $0C29, $1286, $0C31, $1186,
	$0C43, $1846, $0C45, $1446, $0C49, $1246, $0C51, $1146, $0C61, $10C6,
	$0C83, $1826, $0C85, $1426, $0C89, $1226, $0C91, $1126, $0CA1, $10A6,
	$0CC1, $1066, $0D03, $1816, $0D05, $1416, $0D09, $1216, $0D11, $1116,
	$0D21, $1096, $0D41, $1056, $0D81, $1036, $0E03, $180E, $0E05, $140E,
	$0E09, $120E, $0E11, $110E, $0E21, $108E, $0E41, $104E, $0E81, $102E,
	$0F01, $101E, $100F, $1E01, $1017, $1D01, $101B, $1B01, $101D, $1701,
	$1027, $1C81, $102B, $1A81, $102D, $1681, $1033, $1981, $1035, $1581,
	$1039, $1381, $1047, $1C41, $104B, $1A41, $104D, $1641, $1053, $1941,
	$1055, $1541, $1059, $1341, $1063, $18C1, $1065, $14C1, $1069, $12C1,
	$1071, $11C1, $1087, $1C21, $108B, $1A21, $108D, $1621, $1093, $1921,
	$1095, $1521, $1099, $1321, $10A3, $18A1, $10A5, $14A1, $10A9, $12A1,
	$10B1, $11A1, $10C3, $1861, $10C5, $1461, $10C9, $1261, $10D1, $1161,
	$1107, $1C11, $110B, $1A11, $110D, $1611, $1113, $1911, $1115, $1511,
	$1119, $1311, $1123, $1891, $1125, $1491, $1129, $1291, $1131, $1191,
	$1143, $1851, $1145, $1451, $1149, $1251, $1183, $1831, $1185, $1431,
	$1189, $1231, $1207, $1C09, $120B, $1A09, $120D, $1609, $1213, $1909,
	$1215, $1509, $1219, $1309, $1223, $1889, $1225, $1489, $1229, $1289,
	$1243, $1849, $1245, $1449, $1283, $1829, $1285, $1429, $1303, $1819,
	$1305, $1419, $1407, $1C05, $140B, $1A05, $140D, $1605, $1413, $1905,
	$1415, $1505, $1423, $1885, $1425, $1485, $1443, $1845, $1483, $1825,
	$1503, $1815, $1603, $180D, $1807, $1C03, $180B, $1A03, $1813, $1903,
	$1823, $1883, $1843, $1445, $1249, $1151, $10E1, $0C46, $0A4A, $0952,
	$08E2, $064C, $0554, $04E4, $0358, $02E8, $01F0);

const AppxD_II : array[0..77] of Word = ( { Appendix D Table II - 2 of 13 characters }
	$0003, $1800, $0005, $1400, $0006, $0C00, $0009, $1200, $000A, $0A00,
	$000C, $0600, $0011, $1100, $0012, $0900, $0014, $0500, $0018, $0300,
	$0021, $1080, $0022, $0880, $0024, $0480, $0028, $0280, $0030, $0180,
	$0041, $1040, $0042, $0840, $0044, $0440, $0048, $0240, $0050, $0140,
	$0060, $00C0, $0081, $1020, $0082, $0820, $0084, $0420, $0088, $0220,
	$0090, $0120, $0101, $1010, $0102, $0810, $0104, $0410, $0108, $0210,
	$0201, $1008, $0202, $0808, $0204, $0408, $0401, $1004, $0402, $0804,
	$0801, $1002, $1001, $0802, $0404, $0208, $0110, $00A0 );

const AppxD_IV : array[0..129] of Integer = ( { Appendix D Table IV - Bar-to-Character Mapping (reverse lookup) }
	67, 6, 78, 16, 86, 95, 34, 40, 45, 113, 117, 121, 62, 87, 18, 104, 41, 76, 57, 119, 115, 72, 97,
	2, 127, 26, 105, 35, 122, 52, 114, 7, 24, 82, 68, 63, 94, 44, 77, 112, 70, 100, 39, 30, 107,
	15, 125, 85, 10, 65, 54, 88, 20, 106, 46, 66, 8, 116, 29, 61, 99, 80, 90, 37, 123, 51, 25, 84,
	129, 56, 4, 109, 96, 28, 36, 47, 11, 71, 33, 102, 21, 9, 17, 49, 124, 79, 64, 91, 42, 69, 53,
	60, 14, 1, 27, 103, 126, 75, 89, 50, 120, 19, 32, 110, 92, 111, 130, 59, 31, 12, 81, 43, 55,
	5, 74, 22, 101, 128, 58, 118, 48, 108, 38, 98, 93, 23, 83, 13, 73, 3);

{**************************************************************************
  ** USPS_MSB_Math_CRC11GenerateFrameCheckSequence
  **
  ** Inputs:
  **   ByteAttayPtr is the address of a 13 byte array holding 102 bytes which
  **   are right justified - ie: the leftmost 2 bits of the first byte do not
  **   hold data and must be set to zero.
  **
  ** Outputs:
  **   return uint16_t - 11 bit Frame Check Sequence (right justified)
**************************************************************************}

function USPS_MSB_Math_CRC11GenerateFrameCheckSequence(ByteArrayPtr : PByte) : Word;
var
  GeneratorPolynomial : Word;
  FrameCheckSequence : Word;
  Data : Word;
  ByteIndex, Bit : Integer;
begin
  GeneratorPolynomial := $0F35;
  FrameCheckSequence := $07FF;

  { Do most significant byte skipping the 2 most significant bits }
  Data := ByteArrayPtr^ shl 5;
  Inc(ByteArrayPtr);
  for Bit := 2 to 7  do
  begin
    if ((FrameCheckSequence xor Data) and $400) <> 0 then
      FrameCheckSequence := (FrameCheckSequence shl 1) xor GeneratorPolynomial
    else
      FrameCheckSequence := (FrameCheckSequence shl 1);
    FrameCheckSequence := FrameCheckSequence and $7FF;
    Data := Data shl 1;
  end;

  { Do rest of the bytes }
  for ByteIndex := 1 to 12 do
  begin
    Data := ByteArrayPtr^ shl 3;
    Inc(ByteArrayPtr);
    for Bit := 0 to 7 do
    begin
      if ((FrameCheckSequence xor Data) and $0400) <> 0 then
        FrameCheckSequence := (FrameCheckSequence shl 1) xor GeneratorPolynomial
      else
        FrameCheckSequence := (FrameCheckSequence shl 1);
      FrameCheckSequence := FrameCheckSequence and $7FF;
      Data := Data shl 1;
    end;
  end;
  result := FrameCheckSequence; exit;
end;

procedure breakup(var fcs_bit : TArrayOfSmallInt; usps_crc : Word);
var
  i : Integer;
begin
  for i := 0 to 12 do
    fcs_bit[i] := 0;

  if (usps_crc >= 4096) then
  begin
    fcs_bit[12] := 1;
    Dec(usps_crc, 4096);
  end;
  if (usps_crc >= 2048) then
  begin
    fcs_bit[11] := 1;
    Dec(usps_crc, 2048);
  end;
  if (usps_crc >= 1024) then
  begin
    fcs_bit[10] := 1;
    Dec(usps_crc, 1024);
  end;
  if (usps_crc >= 512) then
  begin
    fcs_bit[9] := 1;
    Dec(usps_crc, 512);
  end;
  if (usps_crc >= 256) then
  begin
    fcs_bit[8] := 1;
    Dec(usps_crc, 256);
  end;
  if (usps_crc >= 128) then
  begin
    fcs_bit[7] := 1;
    Dec(usps_crc, 128);
  end;
  if (usps_crc >= 64) then
  begin
    fcs_bit[6] := 1;
    Dec(usps_crc, 64);
  end;
  if (usps_crc >= 32) then
  begin
    fcs_bit[5] := 1;
    Dec(usps_crc, 32);
  end;
  if (usps_crc >= 16) then
  begin
    fcs_bit[4] := 1;
    Dec(usps_crc, 16);
  end;
  if (usps_crc >= 8) then
  begin
    fcs_bit[3] := 1;
    Dec(usps_crc, 8);
  end;
  if (usps_crc >= 4) then
  begin
    fcs_bit[2] := 1;
    Dec(usps_crc, 4);
  end;
  if (usps_crc >= 2) then
  begin
    fcs_bit[1] := 1;
    Dec(usps_crc, 2);
  end;
  if (usps_crc = 1) then
  begin
    fcs_bit[0] := 1;
  end;
end;

function imail(symbol : zint_symbol; const source : TArrayOfByte; _length : Integer) : Integer;
var
  data_pattern : TArrayOfChar;
  error_number : Integer;
  i, j, read : Integer;
  zip, tracker, zip_adder, temp : TArrayOfChar;
  accum, x_reg, y_reg : TArrayOfSmallInt;
  byte_array : TArrayOfByte;
  usps_crc : Word;
  codeword : TArrayOfInteger;
  characters : TArrayOfWord;
  bit_pattern, bar_map : TArrayOfSmallInt;
begin
  SetLength(data_pattern, 200);
  SetLength(zip, 35);
  SetLength(tracker, 35);
  SetLength(zip_adder, 11);
  SetLength(temp, 2);
  SetLength(accum, 112);
  SetLength(x_reg, 112);
  SetLength(y_reg, 112);
  SetLength(byte_array, 13);
  SetLength(codeword, 10);
  SetLength(characters, 10);
  SetLength(bit_pattern, 13);
  SetLength(bar_map, 130);

  //error_number := 0;

  if (_length > 32) then
  begin
    strcpy(symbol.errtxt, 'Input too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  error_number := is_sane(SODIUM, source, _length);
  if (error_number = ZERROR_INVALID_DATA) then
  begin
    strcpy(symbol.errtxt, 'Invalid characters in data');
    result := error_number; exit;
  end;

  strcpy(zip, '');
  strcpy(tracker, '');

  { separate the tracking code from the routing code }

  read := 0;
  j := 0;
  for i := 0 to _length - 1 do
  begin
    if (source[i] = Ord('-')) then
    begin
      tracker[read] := #0;
      j := 1;
      read := 0;
    end
    else
    begin
      if (j = 0) then
      begin
        { reading tracker }
        tracker[read] := Chr(source[i]);
        Inc(read);
      end
      else
      begin
        { reading zip code }
        zip[read] := Chr(source[i]);
        Inc(read);
      end;
    end;
  end;
  if (j = 0) then
    tracker[read] := #0
  else
    zip[read] := #0;

  if (strlen(tracker) <> 20) then
  begin
    strcpy(symbol.errtxt, 'Invalid length tracking code');
    result := ZERROR_INVALID_DATA; exit;
  end;
  if (strlen(zip) > 11) then
  begin
    strcpy(symbol.errtxt, 'Invalid ZIP code');
    result := ZERROR_INVALID_DATA; exit;
  end;

  { *** Step 1 - Conversion of Data Fields into Binary Data *** }

  { Routing code first }

  for i := 0 to 111 do
    accum[i] := 0;

  for read := 0 to strlen(zip) - 1 do
  begin
    for i := 0 to 111 do
      x_reg[i] := accum[i];

    for i := 0 to 8 do
      binary_add(accum, x_reg);

    x_reg[0] := BCD[ctoi(zip[read]) * 4];
    x_reg[1] := BCD[(ctoi(zip[read]) * 4) + 1];
    x_reg[2] := BCD[(ctoi(zip[read]) * 4) + 2];
    x_reg[3] := BCD[(ctoi(zip[read]) * 4) + 3];
    for i := 4 to 111 do
      x_reg[i] := 0;

    binary_add(accum, x_reg);
  end;

  { add weight to routing code }

  for i := 0 to 111 do
    x_reg[i] := accum[i];

  if (strlen(zip) > 9) then
  begin
    strcpy(zip_adder, '1000100001')
  end
  else
  begin
    if (strlen(zip) > 5) then
    begin
      strcpy(zip_adder, '100001');
    end
    else
    begin
      if (strlen(zip) > 0) then
        strcpy(zip_adder, '1')
      else
        strcpy(zip_adder, '0');
    end;
  end;

  for i := 0 to 111 do
    accum[i] := 0;

  for read := 0 to strlen(zip_adder) - 1 do
  begin
    for i := 0 to 111 do
      y_reg[i] := accum[i];

    for i := 0 to 8 do
      binary_add(accum, y_reg);

    y_reg[0] := BCD[ctoi(zip_adder[read]) * 4];
    y_reg[1] := BCD[(ctoi(zip_adder[read]) * 4) + 1];
    y_reg[2] := BCD[(ctoi(zip_adder[read]) * 4) + 2];
    y_reg[3] := BCD[(ctoi(zip_adder[read]) * 4) + 3];
    for i := 4 to 111 do
      y_reg[i] := 0;

    binary_add(accum, y_reg);
  end;

  binary_add(accum, x_reg);

  { tracking code }

  { multiply by 10 }
  for i := 0 to 111 do
    y_reg[i] := accum[i];

  for i := 0 to 8 do
    binary_add(accum, y_reg);

  { add first digit of tracker }
  y_reg[0] := BCD[ctoi(tracker[0]) * 4];
  y_reg[1] := BCD[(ctoi(tracker[0]) * 4) + 1];
  y_reg[2] := BCD[(ctoi(tracker[0]) * 4) + 2];
  y_reg[3] := BCD[(ctoi(tracker[0]) * 4) + 3];
  for i := 4 to 111 do
    y_reg[i] := 0;

  binary_add(accum, y_reg);

  { multiply by 5 }
  for i := 0 to 111 do
    y_reg[i] := accum[i];

  for i := 0 to 3 do
    binary_add(accum, y_reg);

  { add second digit }
  y_reg[0] := BCD[ctoi(tracker[1]) * 4];
  y_reg[1] := BCD[(ctoi(tracker[1]) * 4) + 1];
  y_reg[2] := BCD[(ctoi(tracker[1]) * 4) + 2];
  y_reg[3] := BCD[(ctoi(tracker[1]) * 4) + 3];
  for i := 4 to 111 do
    y_reg[i] := 0;

  binary_add(accum, y_reg);

  { and then the rest }

  for read := 2 to strlen(tracker) - 1 do
  begin
    for i := 0 to 111 do
      y_reg[i] := accum[i];

    for i := 0 to 8 do
      binary_add(accum, y_reg);

    y_reg[0] := BCD[ctoi(tracker[read]) * 4];
    y_reg[1] := BCD[(ctoi(tracker[read]) * 4) + 1];
    y_reg[2] := BCD[(ctoi(tracker[read]) * 4) + 2];
    y_reg[3] := BCD[(ctoi(tracker[read]) * 4) + 3];
    for i := 4 to 111 do
      y_reg[i] := 0;

    binary_add(accum, y_reg);
  end;

  { *** Step 2 - Generation of 11-bit CRC on Binary Data *** }

  accum[103] := 0;
  accum[102] := 0;

  Fill(byte_array, 13, 0);
  for j := 0 to 12 do
  begin
    i := 96 - (8 * j);
    byte_array[j] := 0;
    Inc(byte_array[j], accum[i]);
    Inc(byte_array[j], 2 * accum[i + 1]);
    Inc(byte_array[j], 4 * accum[i + 2]);
    Inc(byte_array[j], 8 * accum[i + 3]);
    Inc(byte_array[j], 16 * accum[i + 4]);
    Inc(byte_array[j], 32 * accum[i + 5]);
    Inc(byte_array[j], 64 * accum[i + 6]);
    Inc(byte_array[j], 128 * accum[i + 7]);
  end;

  usps_crc := USPS_MSB_Math_CRC11GenerateFrameCheckSequence(@byte_array[0]);

  { *** Step 3 - Conversion from Binary Data to Codewords *** }

  { start with codeword J which is base 636 }
  for i := 0 to 111 do
  begin
    x_reg[i] := 0;
    y_reg[i] := 0;
  end;

  x_reg[101] := 1;
  x_reg[98] := 1;
  x_reg[97] := 1;
  x_reg[96] := 1;
  x_reg[95] := 1;
  x_reg[94] := 1;

  for i := 92 downto 0 do
  begin
    y_reg[i] := islarger(accum, x_reg);
    if (y_reg[i] = 1) then
      binary_subtract(accum, x_reg);
    shiftdown(x_reg);
  end;

  codeword[9] := (accum[9] * 512) + (accum[8] * 256) + (accum[7] * 128) + (accum[6] * 64) +
                 (accum[5] * 32) + (accum[4] * 16) + (accum[3] * 8) + (accum[2] * 4) +
                 (accum[1] * 2) + accum[0];

  { then codewords I to B with base 1365 }

  for j := 8 downto 1 do
  begin
    for i := 0 to 111 do
    begin
      accum[i] := y_reg[i];
      y_reg[i] := 0;
      x_reg[i] := 0;
    end;
    x_reg[101] := 1;
    x_reg[99] := 1;
    x_reg[97] := 1;
    x_reg[95] := 1;
    x_reg[93] := 1;
    x_reg[91] := 1;
    for i := 91 downto 0 do
    begin
      y_reg[i] := islarger(accum, x_reg);
      if (y_reg[i] = 1) then
        binary_subtract(accum, x_reg);
      shiftdown(x_reg);
    end;

    codeword[j] := (accum[10] * 1024) + (accum[9] * 512) + (accum[8] * 256) +
                   (accum[7] * 128) + (accum[6] * 64) + (accum[5] * 32) +
                   (accum[4] * 16) + (accum[3] * 8) + (accum[2] * 4) +
                   (accum[1] * 2) + accum[0];
  end;

  codeword[0] := (y_reg[10] * 1024) + (y_reg[9] * 512) + (y_reg[8] * 256) +
                 (y_reg[7] * 128) + (y_reg[6] * 64) + (y_reg[5] * 32) +
                 (y_reg[4] * 16) + (y_reg[3] * 8) + (y_reg[2] * 4) +
                 (y_reg[1] * 2) + y_reg[0];

  for i := 0 to 7 do
  begin
    if (codeword[i] = 1365) then
    begin
      codeword[i] := 0;
      Inc(codeword[i + 1]);
    end;
  end;

  { *** Step 4 - Inserting Additional Information into Codewords *** }

  codeword[9] := codeword[9] * 2;

  if (usps_crc >= 1024) then
    Inc(codeword[0], 659);

  { *** Step 5 - Conversion from Codewords to Characters *** }

  for i := 0 to 9 do
  begin
    if (codeword[i] < 1287) then
      characters[i] := AppxD_I[codeword[i]]
    else
      characters[i] := AppxD_II[codeword[i] - 1287];
  end;

  breakup(bit_pattern, usps_crc);

  for i := 0 to 9 do
  begin
    if (bit_pattern[i] = 1) then
      characters[i] := $1FFF - characters[i];
  end;


  { *** Step 6 - Conversion from Characters to the Intelligent Mail Barcode *** }

  for i := 0 to 9 do
  begin
    breakup(bit_pattern, characters[i]);
    for j := 0 to 12 do
      bar_map[AppxD_IV[(13 * i) + j] - 1] := bit_pattern[j];
  end;

  strcpy(data_pattern, '');
  temp[1] := #0;
  for i := 0 to 64 do
  begin
    j := 0;
    if (bar_map[i] = 0) then
      Inc(j, 1);
    if (bar_map[i + 65] = 0) then
      Inc(j, 2);
    temp[0] := itoc(j);
    concat(data_pattern, temp);
  end;

  { Translate 4-state data pattern to symbol }
  read := 0;
  for i := 0 to strlen(data_pattern) - 1 do
  begin
    if ((data_pattern[i] = '1') or (data_pattern[i] = '0')) then
      set_module(symbol, 0, read);
    set_module(symbol, 1, read);
    if ((data_pattern[i] = '2') or (data_pattern[i] = '0')) then
      set_module(symbol, 2, read);
    Inc(read, 2);
  end;

  symbol.row_height[0] := 3;
  symbol.row_height[1] := 2;
  symbol.row_height[2] := 3;

  symbol.rows := 3;
  symbol.width := read - 1;
  result := error_number; exit;
end;


end.

