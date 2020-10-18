unit zint_pdf417;

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
  SysUtils, zint, zint_common;

function pdf417(symbol : zint_symbol; chaine : TArrayOfByte; _length : Integer) : Integer;
function pdf417enc(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
function micro_pdf417(symbol : zint_symbol; chaine : TArrayOfByte; _length : Integer) : Integer;

procedure byteprocess(var chainemc : TArrayOfInteger; var mc_length : Integer; chaine : TArrayOfByte; start : Integer; _length : Integer; block : Integer);

const TEX = 900;
const BYT = 901;
const NUM = 902;

const BRSET : String = 'ABCDEFabcdefghijklmnopqrstuvwxyz*+-';

{ Left and Right Row Address Pattern from Table 2 }
const RAPLR : array[0..52] of String = ('', '221311', '311311', '312211', '222211', '213211', '214111', '223111',
	'313111', '322111', '412111', '421111', '331111', '241111', '232111', '231211', '321211',
	'411211', '411121', '411112', '321112', '312112', '311212', '311221', '311131', '311122',
	'311113', '221113', '221122', '221131', '221221', '222121', '312121', '321121', '231121',
	'231112', '222112', '213112', '212212', '212221', '212131', '212122', '212113', '211213',
	'211123', '211132', '211141', '211231', '211222', '211312', '211321', '211411', '212311' );

{ Centre Row Address Pattern from Table 2 }
const RAPC : array[0..52] of String = ('', '112231', '121231', '122131', '131131', '131221', '132121', '141121',
	'141211', '142111', '133111', '132211', '131311', '122311', '123211', '124111', '115111',
	'114211', '114121', '123121', '123112', '122212', '122221', '121321', '121411', '112411',
	'113311', '113221', '113212', '113122', '122122', '131122', '131113', '122113', '113113',
	'112213', '112222', '112312', '112321', '111421', '111331', '111322', '111232', '111223',
	'111133', '111124', '111214', '112114', '121114', '121123', '121132', '112132', '112141' );

{ PDF417 error correction coefficients from Grand Zebu }
const coefrs : array[0..1021] of Integer = (
  { k = 2 }
  27, 917,

  { k = 4 }
  522, 568, 723, 809,

  { k = 8 }
  237, 308, 436, 284, 646, 653, 428, 379,

  { k = 16 }
  274, 562, 232, 755, 599, 524, 801, 132, 295, 116, 442, 428, 295, 42, 176, 65,

  { k = 32 }
  361, 575, 922, 525, 176, 586, 640, 321, 536, 742, 677, 742, 687, 284, 193, 517,
  273, 494, 263, 147, 593, 800, 571, 320, 803, 133, 231, 390, 685, 330, 63, 410,

  { k = 64 }
  539, 422, 6, 93, 862, 771, 453, 106, 610, 287, 107, 505, 733, 877, 381, 612,
  723, 476, 462, 172, 430, 609, 858, 822, 543, 376, 511, 400, 672, 762, 283, 184,
  440, 35, 519, 31, 460, 594, 225, 535, 517, 352, 605, 158, 651, 201, 488, 502,
  648, 733, 717, 83, 404, 97, 280, 771, 840, 629, 4, 381, 843, 623, 264, 543,

  { k = 128 }
  521, 310, 864, 547, 858, 580, 296, 379, 53, 779, 897, 444, 400, 925, 749, 415,
  822, 93, 217, 208, 928, 244, 583, 620, 246, 148, 447, 631, 292, 908, 490, 704,
  516, 258, 457, 907, 594, 723, 674, 292, 272, 96, 684, 432, 686, 606, 860, 569,
  193, 219, 129, 186, 236, 287, 192, 775, 278, 173, 40, 379, 712, 463, 646, 776,
  171, 491, 297, 763, 156, 732, 95, 270, 447, 90, 507, 48, 228, 821, 808, 898,
  784, 663, 627, 378, 382, 262, 380, 602, 754, 336, 89, 614, 87, 432, 670, 616,
  157, 374, 242, 726, 600, 269, 375, 898, 845, 454, 354, 130, 814, 587, 804, 34,
  211, 330, 539, 297, 827, 865, 37, 517, 834, 315, 550, 86, 801, 4, 108, 539,

  { k = 256 }
  524, 894, 75, 766, 882, 857, 74, 204, 82, 586, 708, 250, 905, 786, 138, 720,
  858, 194, 311, 913, 275, 190, 375, 850, 438, 733, 194, 280, 201, 280, 828, 757,
  710, 814, 919, 89, 68, 569, 11, 204, 796, 605, 540, 913, 801, 700, 799, 137,
  439, 418, 592, 668, 353, 859, 370, 694, 325, 240, 216, 257, 284, 549, 209, 884,
  315, 70, 329, 793, 490, 274, 877, 162, 749, 812, 684, 461, 334, 376, 849, 521,
  307, 291, 803, 712, 19, 358, 399, 908, 103, 511, 51, 8, 517, 225, 289, 470,
  637, 731, 66, 255, 917, 269, 463, 830, 730, 433, 848, 585, 136, 538, 906, 90,
  2, 290, 743, 199, 655, 903, 329, 49, 802, 580, 355, 588, 188, 462, 10, 134,
  628, 320, 479, 130, 739, 71, 263, 318, 374, 601, 192, 605, 142, 673, 687, 234,
  722, 384, 177, 752, 607, 640, 455, 193, 689, 707, 805, 641, 48, 60, 732, 621,
  895, 544, 261, 852, 655, 309, 697, 755, 756, 60, 231, 773, 434, 421, 726, 528,
  503, 118, 49, 795, 32, 144, 500, 238, 836, 394, 280, 566, 319, 9, 647, 550,
  73, 914, 342, 126, 32, 681, 331, 792, 620, 60, 609, 441, 180, 791, 893, 754,
  605, 383, 228, 749, 760, 213, 54, 297, 134, 54, 834, 299, 922, 191, 910, 532,
  609, 829, 189, 20, 167, 29, 872, 449, 83, 402, 41, 656, 505, 579, 481, 173,
  404, 251, 688, 95, 497, 555, 642, 543, 307, 159, 924, 558, 648, 55, 497, 10,

  { k = 512 }
  352, 77, 373, 504, 35, 599, 428, 207, 409, 574, 118, 498, 285, 380, 350, 492,
  197, 265, 920, 155, 914, 299, 229, 643, 294, 871, 306, 88, 87, 193, 352, 781,
  846, 75, 327, 520, 435, 543, 203, 666, 249, 346, 781, 621, 640, 268, 794, 534,
  539, 781, 408, 390, 644, 102, 476, 499, 290, 632, 545, 37, 858, 916, 552, 41,
  542, 289, 122, 272, 383, 800, 485, 98, 752, 472, 761, 107, 784, 860, 658, 741,
  290, 204, 681, 407, 855, 85, 99, 62, 482, 180, 20, 297, 451, 593, 913, 142,
  808, 684, 287, 536, 561, 76, 653, 899, 729, 567, 744, 390, 513, 192, 516, 258,
  240, 518, 794, 395, 768, 848, 51, 610, 384, 168, 190, 826, 328, 596, 786, 303,
  570, 381, 415, 641, 156, 237, 151, 429, 531, 207, 676, 710, 89, 168, 304, 402,
  40, 708, 575, 162, 864, 229, 65, 861, 841, 512, 164, 477, 221, 92, 358, 785,
  288, 357, 850, 836, 827, 736, 707, 94, 8, 494, 114, 521, 2, 499, 851, 543,
  152, 729, 771, 95, 248, 361, 578, 323, 856, 797, 289, 51, 684, 466, 533, 820,
  669, 45, 902, 452, 167, 342, 244, 173, 35, 463, 651, 51, 699, 591, 452, 578,
  37, 124, 298, 332, 552, 43, 427, 119, 662, 777, 475, 850, 764, 364, 578, 911,
  283, 711, 472, 420, 245, 288, 594, 394, 511, 327, 589, 777, 699, 688, 43, 408,
  842, 383, 721, 521, 560, 644, 714, 559, 62, 145, 873, 663, 713, 159, 672, 729,
  624, 59, 193, 417, 158, 209, 563, 564, 343, 693, 109, 608, 563, 365, 181, 772,
  677, 310, 248, 353, 708, 410, 579, 870, 617, 841, 632, 860, 289, 536, 35, 777,
  618, 586, 424, 833, 77, 597, 346, 269, 757, 632, 695, 751, 331, 247, 184, 45,
  787, 680, 18, 66, 407, 369, 54, 492, 228, 613, 830, 922, 437, 519, 644, 905,
  789, 420, 305, 441, 207, 300, 892, 827, 141, 537, 381, 662, 513, 56, 252, 341,
  242, 797, 838, 837, 720, 224, 307, 631, 61, 87, 560, 310, 756, 665, 397, 808,
  851, 309, 473, 795, 378, 31, 647, 915, 459, 806, 590, 731, 425, 216, 548, 249,
  321, 881, 699, 535, 673, 782, 210, 815, 905, 303, 843, 922, 281, 73, 469, 791,
  660, 162, 498, 308, 155, 422, 907, 817, 187, 62, 16, 425, 535, 336, 286, 437,
  375, 273, 610, 296, 183, 923, 116, 667, 751, 353, 62, 366, 691, 379, 687, 842,
  37, 357, 720, 742, 330, 5, 39, 923, 311, 424, 242, 749, 321, 54, 669, 316,
  342, 299, 534, 105, 667, 488, 640, 672, 576, 540, 316, 486, 721, 610, 46, 656,
  447, 171, 616, 464, 190, 531, 297, 321, 762, 752, 533, 175, 134, 14, 381, 433,
  717, 45, 111, 20, 596, 284, 736, 138, 646, 411, 877, 669, 141, 919, 45, 780,
  407, 164, 332, 899, 165, 726, 600, 325, 498, 655, 357, 752, 768, 223, 849, 647,
  63, 310, 863, 251, 366, 304, 282, 738, 675, 410, 389, 244, 31, 121, 303, 263 );

{ converts values into bar patterns - replacing Grand Zebu's true type font }
const PDFttf : array[0..34] of String = ( '00000', '00001', '00010', '00011', '00100', '00101', '00110', '00111',
  '01000', '01001', '01010', '01011', '01100', '01101', '01110', '01111', '10000', '10001',
  '10010', '10011', '10100', '10101', '10110', '10111', '11000', '11001', '11010',
  '11011', '11100', '11101', '11110', '11111', '01', '1111111101010100', '11111101000101001');

{ MicroPDF417 coefficients from ISO/IEC 24728:2006 Annex F }
const Microcoeffs : array[0..343] of Integer = (
  { k = 7 }
  76, 925, 537, 597, 784, 691, 437,

  { k = 8 }
  237, 308, 436, 284, 646, 653, 428, 379,

  { k = 9 }
  567, 527, 622, 257, 289, 362, 501, 441, 205,

  { k = 10 }
  377, 457, 64, 244, 826, 841, 818, 691, 266, 612,

  { k = 11 }
  462, 45, 565, 708, 825, 213, 15, 68, 327, 602, 904,

  { k = 12 }
  597, 864, 757, 201, 646, 684, 347, 127, 388, 7, 69, 851,

  { k = 13 }
  764, 713, 342, 384, 606, 583, 322, 592, 678, 204, 184, 394, 692,

  { k = 14 }
  669, 677, 154, 187, 241, 286, 274, 354, 478, 915, 691, 833, 105, 215,

  { k = 15 }
  460, 829, 476, 109, 904, 664, 230, 5, 80, 74, 550, 575, 147, 868, 642,

  { k = 16 }
  274, 562, 232, 755, 599, 524, 801, 132, 295, 116, 442, 428, 295, 42, 176, 65,

  { k = 18 }
  279, 577, 315, 624, 37, 855, 275, 739, 120, 297, 312, 202, 560, 321, 233, 756,
  760, 573,

  { k = 21 }
  108, 519, 781, 534, 129, 425, 681, 553, 422, 716, 763, 693, 624, 610, 310, 691,
  347, 165, 193, 259, 568,

  { k = 26 }
  443, 284, 887, 544, 788, 93, 477, 760, 331, 608, 269, 121, 159, 830, 446, 893,
  699, 245, 441, 454, 325, 858, 131, 847, 764, 169,

  { k = 32 }
  361, 575, 922, 525, 176, 586, 640, 321, 536, 742, 677, 742, 687, 284, 193, 517,
  273, 494, 263, 147, 593, 800, 571, 320, 803, 133, 231, 390, 685, 330, 63, 410,

  { k = 38 }
  234, 228, 438, 848, 133, 703, 529, 721, 788, 322, 280, 159, 738, 586, 388, 684,
  445, 680, 245, 595, 614, 233, 812, 32, 284, 658, 745, 229, 95, 689, 920, 771,
  554, 289, 231, 125, 117, 518,

  { k = 44 }
  476, 36, 659, 848, 678, 64, 764, 840, 157, 915, 470, 876, 109, 25, 632, 405,
  417, 436, 714, 60, 376, 97, 413, 706, 446, 21, 3, 773, 569, 267, 272, 213,
  31, 560, 231, 758, 103, 271, 572, 436, 339, 730, 82, 285,

  { k = 50 }
  923, 797, 576, 875, 156, 706, 63, 81, 257, 874, 411, 416, 778, 50, 205, 303,
  188, 535, 909, 155, 637, 230, 534, 96, 575, 102, 264, 233, 919, 593, 865, 26,
  579, 623, 766, 146, 10, 739, 246, 127, 71, 244, 211, 477, 920, 876, 427, 820,
  718, 435 );
{ rows, columns, error codewords, k-offset of valid MicroPDF417 sizes from ISO/IEC 24728:2006 }

const MicroVariants : array[0..135] of Integer =
(	1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
  11, 14, 17, 20, 24, 28, 8, 11, 14, 17, 20, 23, 26, 6, 8, 10, 12, 15, 20, 26, 32, 38, 44, 4, 6, 8, 10, 12, 15, 20, 26, 32, 38, 44,
  7, 7, 7, 8, 8, 8, 8, 9, 9, 10, 11, 13, 15, 12, 14, 16, 18, 21, 26, 32, 38, 44, 50, 8, 12, 14, 16, 18, 21, 26, 32, 38, 44, 50,
  0, 0, 0, 7, 7, 7, 7, 15, 15, 24, 34, 57, 84, 45, 70, 99, 115, 133, 154, 180, 212, 250, 294, 7, 45, 70, 99, 115, 133, 154, 180, 212, 250, 294 );
{ rows, columns, error codewords, k-offset }

{ following is Left RAP, Centre RAP, Right RAP and Start Cluster from ISO/IEC 24728:2006 tables 10, 11 and 12 }
const RAPTable : array[0..135] of Integer =
(	1, 8, 36, 19, 9, 25, 1, 1, 8, 36, 19, 9, 27, 1, 7, 15, 25, 37, 1, 1, 21, 15, 1, 47, 1, 7, 15, 25, 37, 1, 1, 21, 15, 1,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 7, 15, 25, 37, 17, 9, 29, 31, 25, 19, 1, 7, 15, 25, 37, 17, 9, 29, 31, 25,
  9, 8, 36, 19, 17, 33, 1, 9, 8, 36, 19, 17, 35, 1, 7, 15, 25, 37, 33, 17, 37, 47, 49, 43, 1, 7, 15, 25, 37, 33, 17, 37, 47, 49,
  0, 3, 6, 0, 6, 0, 0, 0, 3, 6, 0, 6, 6, 0, 0, 6, 0, 0, 0, 0, 6, 6, 0, 3, 0, 0, 6, 0, 0, 0, 0, 6, 6, 0 );

const codagemc : array[0..2786] of String = ( 'urA', 'xfs', 'ypy', 'unk', 'xdw', 'yoz', 'pDA', 'uls', 'pBk', 'eBA',
  'pAs', 'eAk', 'prA', 'uvs', 'xhy', 'pnk', 'utw', 'xgz', 'fDA', 'pls', 'fBk', 'frA', 'pvs',
  'uxy', 'fnk', 'ptw', 'uwz', 'fls', 'psy', 'fvs', 'pxy', 'ftw', 'pwz', 'fxy', 'yrx', 'ufk',
  'xFw', 'ymz', 'onA', 'uds', 'xEy', 'olk', 'ucw', 'dBA', 'oks', 'uci', 'dAk', 'okg', 'dAc',
  'ovk', 'uhw', 'xaz', 'dnA', 'ots', 'ugy', 'dlk', 'osw', 'ugj', 'dks', 'osi', 'dvk', 'oxw',
  'uiz', 'dts', 'owy', 'dsw', 'owj', 'dxw', 'oyz', 'dwy', 'dwj', 'ofA', 'uFs', 'xCy', 'odk',
  'uEw', 'xCj', 'clA', 'ocs', 'uEi', 'ckk', 'ocg', 'ckc', 'ckE', 'cvA', 'ohs', 'uay', 'ctk',
  'ogw', 'uaj', 'css', 'ogi', 'csg', 'csa', 'cxs', 'oiy', 'cww', 'oij', 'cwi', 'cyy', 'oFk',
  'uCw', 'xBj', 'cdA', 'oEs', 'uCi', 'cck', 'oEg', 'uCb', 'ccc', 'oEa', 'ccE', 'oED', 'chk',
  'oaw', 'uDj', 'cgs', 'oai', 'cgg', 'oab', 'cga', 'cgD', 'obj', 'cib', 'cFA', 'oCs', 'uBi',
  'cEk', 'oCg', 'uBb', 'cEc', 'oCa', 'cEE', 'oCD', 'cEC', 'cas', 'cag', 'caa', 'cCk', 'uAr',
  'oBa', 'oBD', 'cCB', 'tfk', 'wpw', 'yez', 'mnA', 'tds', 'woy', 'mlk', 'tcw', 'woj', 'FBA',
  'mks', 'FAk', 'mvk', 'thw', 'wqz', 'FnA', 'mts', 'tgy', 'Flk', 'msw', 'Fks', 'Fkg', 'Fvk',
  'mxw', 'tiz', 'Fts', 'mwy', 'Fsw', 'Fsi', 'Fxw', 'myz', 'Fwy', 'Fyz', 'vfA', 'xps', 'yuy',
  'vdk', 'xow', 'yuj', 'qlA', 'vcs', 'xoi', 'qkk', 'vcg', 'xob', 'qkc', 'vca', 'mfA', 'tFs',
  'wmy', 'qvA', 'mdk', 'tEw', 'wmj', 'qtk', 'vgw', 'xqj', 'hlA', 'Ekk', 'mcg', 'tEb', 'hkk',
  'qsg', 'hkc', 'EvA', 'mhs', 'tay', 'hvA', 'Etk', 'mgw', 'taj', 'htk', 'qww', 'vij', 'hss',
  'Esg', 'hsg', 'Exs', 'miy', 'hxs', 'Eww', 'mij', 'hww', 'qyj', 'hwi', 'Eyy', 'hyy', 'Eyj',
  'hyj', 'vFk', 'xmw', 'ytj', 'qdA', 'vEs', 'xmi', 'qck', 'vEg', 'xmb', 'qcc', 'vEa', 'qcE',
  'qcC', 'mFk', 'tCw', 'wlj', 'qhk', 'mEs', 'tCi', 'gtA', 'Eck', 'vai', 'tCb', 'gsk', 'Ecc',
  'mEa', 'gsc', 'qga', 'mED', 'EcC', 'Ehk', 'maw', 'tDj', 'gxk', 'Egs', 'mai', 'gws', 'qii',
  'mab', 'gwg', 'Ega', 'EgD', 'Eiw', 'mbj', 'gyw', 'Eii', 'gyi', 'Eib', 'gyb', 'gzj', 'qFA',
  'vCs', 'xli', 'qEk', 'vCg', 'xlb', 'qEc', 'vCa', 'qEE', 'vCD', 'qEC', 'qEB', 'EFA', 'mCs',
  'tBi', 'ghA', 'EEk', 'mCg', 'tBb', 'ggk', 'qag', 'vDb', 'ggc', 'EEE', 'mCD', 'ggE', 'qaD',
  'ggC', 'Eas', 'mDi', 'gis', 'Eag', 'mDb', 'gig', 'qbb', 'gia', 'EaD', 'giD', 'gji', 'gjb',
  'qCk', 'vBg', 'xkr', 'qCc', 'vBa', 'qCE', 'vBD', 'qCC', 'qCB', 'ECk', 'mBg', 'tAr', 'gak',
  'ECc', 'mBa', 'gac', 'qDa', 'mBD', 'gaE', 'ECC', 'gaC', 'ECB', 'EDg', 'gbg', 'gba', 'gbD',
  'vAq', 'vAn', 'qBB', 'mAq', 'EBE', 'gDE', 'gDC', 'gDB', 'lfA', 'sps', 'wey', 'ldk', 'sow',
  'ClA', 'lcs', 'soi', 'Ckk', 'lcg', 'Ckc', 'CkE', 'CvA', 'lhs', 'sqy', 'Ctk', 'lgw', 'sqj',
  'Css', 'lgi', 'Csg', 'Csa', 'Cxs', 'liy', 'Cww', 'lij', 'Cwi', 'Cyy', 'Cyj', 'tpk', 'wuw',
  'yhj', 'ndA', 'tos', 'wui', 'nck', 'tog', 'wub', 'ncc', 'toa', 'ncE', 'toD', 'lFk', 'smw',
  'wdj', 'nhk', 'lEs', 'smi', 'atA', 'Cck', 'tqi', 'smb', 'ask', 'ngg', 'lEa', 'asc', 'CcE',
  'asE', 'Chk', 'law', 'snj', 'axk', 'Cgs', 'trj', 'aws', 'nii', 'lab', 'awg', 'Cga', 'awa',
  'Ciw', 'lbj', 'ayw', 'Cii', 'ayi', 'Cib', 'Cjj', 'azj', 'vpA', 'xus', 'yxi', 'vok', 'xug',
  'yxb', 'voc', 'xua', 'voE', 'xuD', 'voC', 'nFA', 'tms', 'wti', 'rhA', 'nEk', 'xvi', 'wtb',
  'rgk', 'vqg', 'xvb', 'rgc', 'nEE', 'tmD', 'rgE', 'vqD', 'nEB', 'CFA', 'lCs', 'sli', 'ahA',
  'CEk', 'lCg', 'slb', 'ixA', 'agk', 'nag', 'tnb', 'iwk', 'rig', 'vrb', 'lCD', 'iwc', 'agE',
  'naD', 'iwE', 'CEB', 'Cas', 'lDi', 'ais', 'Cag', 'lDb', 'iys', 'aig', 'nbb', 'iyg', 'rjb',
  'CaD', 'aiD', 'Cbi', 'aji', 'Cbb', 'izi', 'ajb', 'vmk', 'xtg', 'ywr', 'vmc', 'xta', 'vmE',
  'xtD', 'vmC', 'vmB', 'nCk', 'tlg', 'wsr', 'rak', 'nCc', 'xtr', 'rac', 'vna', 'tlD', 'raE',
  'nCC', 'raC', 'nCB', 'raB', 'CCk', 'lBg', 'skr', 'aak', 'CCc', 'lBa', 'iik', 'aac', 'nDa',
  'lBD', 'iic', 'rba', 'CCC', 'iiE', 'aaC', 'CCB', 'aaB', 'CDg', 'lBr', 'abg', 'CDa', 'ijg',
  'aba', 'CDD', 'ija', 'abD', 'CDr', 'ijr', 'vlc', 'xsq', 'vlE', 'xsn', 'vlC', 'vlB', 'nBc',
  'tkq', 'rDc', 'nBE', 'tkn', 'rDE', 'vln', 'rDC', 'nBB', 'rDB', 'CBc', 'lAq', 'aDc', 'CBE',
  'lAn', 'ibc', 'aDE', 'nBn', 'ibE', 'rDn', 'CBB', 'ibC', 'aDB', 'ibB', 'aDq', 'ibq', 'ibn',
  'xsf', 'vkl', 'tkf', 'nAm', 'nAl', 'CAo', 'aBo', 'iDo', 'CAl', 'aBl', 'kpk', 'BdA', 'kos',
  'Bck', 'kog', 'seb', 'Bcc', 'koa', 'BcE', 'koD', 'Bhk', 'kqw', 'sfj', 'Bgs', 'kqi', 'Bgg',
  'kqb', 'Bga', 'BgD', 'Biw', 'krj', 'Bii', 'Bib', 'Bjj', 'lpA', 'sus', 'whi', 'lok', 'sug',
  'loc', 'sua', 'loE', 'suD', 'loC', 'BFA', 'kms', 'sdi', 'DhA', 'BEk', 'svi', 'sdb', 'Dgk',
  'lqg', 'svb', 'Dgc', 'BEE', 'kmD', 'DgE', 'lqD', 'BEB', 'Bas', 'kni', 'Dis', 'Bag', 'knb',
  'Dig', 'lrb', 'Dia', 'BaD', 'Bbi', 'Dji', 'Bbb', 'Djb', 'tuk', 'wxg', 'yir', 'tuc', 'wxa',
  'tuE', 'wxD', 'tuC', 'tuB', 'lmk', 'stg', 'nqk', 'lmc', 'sta', 'nqc', 'tva', 'stD', 'nqE',
  'lmC', 'nqC', 'lmB', 'nqB', 'BCk', 'klg', 'Dak', 'BCc', 'str', 'bik', 'Dac', 'lna', 'klD',
  'bic', 'nra', 'BCC', 'biE', 'DaC', 'BCB', 'DaB', 'BDg', 'klr', 'Dbg', 'BDa', 'bjg', 'Dba',
  'BDD', 'bja', 'DbD', 'BDr', 'Dbr', 'bjr', 'xxc', 'yyq', 'xxE', 'yyn', 'xxC', 'xxB', 'ttc',
  'wwq', 'vvc', 'xxq', 'wwn', 'vvE', 'xxn', 'vvC', 'ttB', 'vvB', 'llc', 'ssq', 'nnc', 'llE',
  'ssn', 'rrc', 'nnE', 'ttn', 'rrE', 'vvn', 'llB', 'rrC', 'nnB', 'rrB', 'BBc', 'kkq', 'DDc',
  'BBE', 'kkn', 'bbc', 'DDE', 'lln', 'jjc', 'bbE', 'nnn', 'BBB', 'jjE', 'rrn', 'DDB', 'jjC',
  'BBq', 'DDq', 'BBn', 'bbq', 'DDn', 'jjq', 'bbn', 'jjn', 'xwo', 'yyf', 'xwm', 'xwl', 'tso',
  'wwf', 'vto', 'xwv', 'vtm', 'tsl', 'vtl', 'lko', 'ssf', 'nlo', 'lkm', 'rno', 'nlm', 'lkl',
  'rnm', 'nll', 'rnl', 'BAo', 'kkf', 'DBo', 'lkv', 'bDo', 'DBm', 'BAl', 'jbo', 'bDm', 'DBl',
  'jbm', 'bDl', 'jbl', 'DBv', 'jbv', 'xwd', 'vsu', 'vst', 'nku', 'rlu', 'rlt', 'DAu', 'bBu',
  'jDu', 'jDt', 'ApA', 'Aok', 'keg', 'Aoc', 'AoE', 'AoC', 'Aqs', 'Aqg', 'Aqa', 'AqD', 'Ari',
  'Arb', 'kuk', 'kuc', 'sha', 'kuE', 'shD', 'kuC', 'kuB', 'Amk', 'kdg', 'Bqk', 'kvg', 'kda',
  'Bqc', 'kva', 'BqE', 'kvD', 'BqC', 'AmB', 'BqB', 'Ang', 'kdr', 'Brg', 'kvr', 'Bra', 'AnD',
  'BrD', 'Anr', 'Brr', 'sxc', 'sxE', 'sxC', 'sxB', 'ktc', 'lvc', 'sxq', 'sgn', 'lvE', 'sxn',
  'lvC', 'ktB', 'lvB', 'Alc', 'Bnc', 'AlE', 'kcn', 'Drc', 'BnE', 'AlC', 'DrE', 'BnC', 'AlB',
  'DrC', 'BnB', 'Alq', 'Bnq', 'Aln', 'Drq', 'Bnn', 'Drn', 'wyo', 'wym', 'wyl', 'swo', 'txo',
  'wyv', 'txm', 'swl', 'txl', 'kso', 'sgf', 'lto', 'swv', 'nvo', 'ltm', 'ksl', 'nvm', 'ltl',
  'nvl', 'Ako', 'kcf', 'Blo', 'ksv', 'Dno', 'Blm', 'Akl', 'bro', 'Dnm', 'Bll', 'brm', 'Dnl',
  'Akv', 'Blv', 'Dnv', 'brv', 'yze', 'yzd', 'wye', 'xyu', 'wyd', 'xyt', 'swe', 'twu', 'swd',
  'vxu', 'twt', 'vxt', 'kse', 'lsu', 'ksd', 'ntu', 'lst', 'rvu', 'ypk', 'zew', 'xdA', 'yos',
  'zei', 'xck', 'yog', 'zeb', 'xcc', 'yoa', 'xcE', 'yoD', 'xcC', 'xhk', 'yqw', 'zfj', 'utA',
  'xgs', 'yqi', 'usk', 'xgg', 'yqb', 'usc', 'xga', 'usE', 'xgD', 'usC', 'uxk', 'xiw', 'yrj',
  'ptA', 'uws', 'xii', 'psk', 'uwg', 'xib', 'psc', 'uwa', 'psE', 'uwD', 'psC', 'pxk', 'uyw',
  'xjj', 'ftA', 'pws', 'uyi', 'fsk', 'pwg', 'uyb', 'fsc', 'pwa', 'fsE', 'pwD', 'fxk', 'pyw',
  'uzj', 'fws', 'pyi', 'fwg', 'pyb', 'fwa', 'fyw', 'pzj', 'fyi', 'fyb', 'xFA', 'yms', 'zdi',
  'xEk', 'ymg', 'zdb', 'xEc', 'yma', 'xEE', 'ymD', 'xEC', 'xEB', 'uhA', 'xas', 'yni', 'ugk',
  'xag', 'ynb', 'ugc', 'xaa', 'ugE', 'xaD', 'ugC', 'ugB', 'oxA', 'uis', 'xbi', 'owk', 'uig',
  'xbb', 'owc', 'uia', 'owE', 'uiD', 'owC', 'owB', 'dxA', 'oys', 'uji', 'dwk', 'oyg', 'ujb',
  'dwc', 'oya', 'dwE', 'oyD', 'dwC', 'dys', 'ozi', 'dyg', 'ozb', 'dya', 'dyD', 'dzi', 'dzb',
  'xCk', 'ylg', 'zcr', 'xCc', 'yla', 'xCE', 'ylD', 'xCC', 'xCB', 'uak', 'xDg', 'ylr', 'uac',
  'xDa', 'uaE', 'xDD', 'uaC', 'uaB', 'oik', 'ubg', 'xDr', 'oic', 'uba', 'oiE', 'ubD', 'oiC',
  'oiB', 'cyk', 'ojg', 'ubr', 'cyc', 'oja', 'cyE', 'ojD', 'cyC', 'cyB', 'czg', 'ojr', 'cza',
  'czD', 'czr', 'xBc', 'ykq', 'xBE', 'ykn', 'xBC', 'xBB', 'uDc', 'xBq', 'uDE', 'xBn', 'uDC',
  'uDB', 'obc', 'uDq', 'obE', 'uDn', 'obC', 'obB', 'cjc', 'obq', 'cjE', 'obn', 'cjC', 'cjB',
  'cjq', 'cjn', 'xAo', 'ykf', 'xAm', 'xAl', 'uBo', 'xAv', 'uBm', 'uBl', 'oDo', 'uBv', 'oDm',
  'oDl', 'cbo', 'oDv', 'cbm', 'cbl', 'xAe', 'xAd', 'uAu', 'uAt', 'oBu', 'oBt', 'wpA', 'yes',
  'zFi', 'wok', 'yeg', 'zFb', 'woc', 'yea', 'woE', 'yeD', 'woC', 'woB', 'thA', 'wqs', 'yfi',
  'tgk', 'wqg', 'yfb', 'tgc', 'wqa', 'tgE', 'wqD', 'tgC', 'tgB', 'mxA', 'tis', 'wri', 'mwk',
  'tig', 'wrb', 'mwc', 'tia', 'mwE', 'tiD', 'mwC', 'mwB', 'FxA', 'mys', 'tji', 'Fwk', 'myg',
  'tjb', 'Fwc', 'mya', 'FwE', 'myD', 'FwC', 'Fys', 'mzi', 'Fyg', 'mzb', 'Fya', 'FyD', 'Fzi',
  'Fzb', 'yuk', 'zhg', 'hjs', 'yuc', 'zha', 'hbw', 'yuE', 'zhD', 'hDy', 'yuC', 'yuB', 'wmk',
  'ydg', 'zEr', 'xqk', 'wmc', 'zhr', 'xqc', 'yva', 'ydD', 'xqE', 'wmC', 'xqC', 'wmB', 'xqB',
  'tak', 'wng', 'ydr', 'vik', 'tac', 'wna', 'vic', 'xra', 'wnD', 'viE', 'taC', 'viC', 'taB',
  'viB', 'mik', 'tbg', 'wnr', 'qyk', 'mic', 'tba', 'qyc', 'vja', 'tbD', 'qyE', 'miC', 'qyC',
  'miB', 'qyB', 'Eyk', 'mjg', 'tbr', 'hyk', 'Eyc', 'mja', 'hyc', 'qza', 'mjD', 'hyE', 'EyC',
  'hyC', 'EyB', 'Ezg', 'mjr', 'hzg', 'Eza', 'hza', 'EzD', 'hzD', 'Ezr', 'ytc', 'zgq', 'grw',
  'ytE', 'zgn', 'gny', 'ytC', 'glz', 'ytB', 'wlc', 'ycq', 'xnc', 'wlE', 'ycn', 'xnE', 'ytn',
  'xnC', 'wlB', 'xnB', 'tDc', 'wlq', 'vbc', 'tDE', 'wln', 'vbE', 'xnn', 'vbC', 'tDB', 'vbB',
  'mbc', 'tDq', 'qjc', 'mbE', 'tDn', 'qjE', 'vbn', 'qjC', 'mbB', 'qjB', 'Ejc', 'mbq', 'gzc',
  'EjE', 'mbn', 'gzE', 'qjn', 'gzC', 'EjB', 'gzB', 'Ejq', 'gzq', 'Ejn', 'gzn', 'yso', 'zgf',
  'gfy', 'ysm', 'gdz', 'ysl', 'wko', 'ycf', 'xlo', 'ysv', 'xlm', 'wkl', 'xll', 'tBo', 'wkv',
  'vDo', 'tBm', 'vDm', 'tBl', 'vDl', 'mDo', 'tBv', 'qbo', 'vDv', 'qbm', 'mDl', 'qbl', 'Ebo',
  'mDv', 'gjo', 'Ebm', 'gjm', 'Ebl', 'gjl', 'Ebv', 'gjv', 'yse', 'gFz', 'ysd', 'wke', 'xku',
  'wkd', 'xkt', 'tAu', 'vBu', 'tAt', 'vBt', 'mBu', 'qDu', 'mBt', 'qDt', 'EDu', 'gbu', 'EDt',
  'gbt', 'ysF', 'wkF', 'xkh', 'tAh', 'vAx', 'mAx', 'qBx', 'wek', 'yFg', 'zCr', 'wec', 'yFa',
  'weE', 'yFD', 'weC', 'weB', 'sqk', 'wfg', 'yFr', 'sqc', 'wfa', 'sqE', 'wfD', 'sqC', 'sqB',
  'lik', 'srg', 'wfr', 'lic', 'sra', 'liE', 'srD', 'liC', 'liB', 'Cyk', 'ljg', 'srr', 'Cyc',
  'lja', 'CyE', 'ljD', 'CyC', 'CyB', 'Czg', 'ljr', 'Cza', 'CzD', 'Czr', 'yhc', 'zaq', 'arw',
  'yhE', 'zan', 'any', 'yhC', 'alz', 'yhB', 'wdc', 'yEq', 'wvc', 'wdE', 'yEn', 'wvE', 'yhn',
  'wvC', 'wdB', 'wvB', 'snc', 'wdq', 'trc', 'snE', 'wdn', 'trE', 'wvn', 'trC', 'snB', 'trB',
  'lbc', 'snq', 'njc', 'lbE', 'snn', 'njE', 'trn', 'njC', 'lbB', 'njB', 'Cjc', 'lbq', 'azc',
  'CjE', 'lbn', 'azE', 'njn', 'azC', 'CjB', 'azB', 'Cjq', 'azq', 'Cjn', 'azn', 'zio', 'irs',
  'rfy', 'zim', 'inw', 'rdz', 'zil', 'ily', 'ikz', 'ygo', 'zaf', 'afy', 'yxo', 'ziv', 'ivy',
  'adz', 'yxm', 'ygl', 'itz', 'yxl', 'wco', 'yEf', 'wto', 'wcm', 'xvo', 'yxv', 'wcl', 'xvm',
  'wtl', 'xvl', 'slo', 'wcv', 'tno', 'slm', 'vro', 'tnm', 'sll', 'vrm', 'tnl', 'vrl', 'lDo',
  'slv', 'nbo', 'lDm', 'rjo', 'nbm', 'lDl', 'rjm', 'nbl', 'rjl', 'Cbo', 'lDv', 'ajo', 'Cbm',
  'izo', 'ajm', 'Cbl', 'izm', 'ajl', 'izl', 'Cbv', 'ajv', 'zie', 'ifw', 'rFz', 'zid', 'idy',
  'icz', 'yge', 'aFz', 'ywu', 'ygd', 'ihz', 'ywt', 'wce', 'wsu', 'wcd', 'xtu', 'wst', 'xtt',
  'sku', 'tlu', 'skt', 'vnu', 'tlt', 'vnt', 'lBu', 'nDu', 'lBt', 'rbu', 'nDt', 'rbt', 'CDu',
  'abu', 'CDt', 'iju', 'abt', 'ijt', 'ziF', 'iFy', 'iEz', 'ygF', 'ywh', 'wcF', 'wsh', 'xsx',
  'skh', 'tkx', 'vlx', 'lAx', 'nBx', 'rDx', 'CBx', 'aDx', 'ibx', 'iCz', 'wFc', 'yCq', 'wFE',
  'yCn', 'wFC', 'wFB', 'sfc', 'wFq', 'sfE', 'wFn', 'sfC', 'sfB', 'krc', 'sfq', 'krE', 'sfn',
  'krC', 'krB', 'Bjc', 'krq', 'BjE', 'krn', 'BjC', 'BjB', 'Bjq', 'Bjn', 'yao', 'zDf', 'Dfy',
  'yam', 'Ddz', 'yal', 'wEo', 'yCf', 'who', 'wEm', 'whm', 'wEl', 'whl', 'sdo', 'wEv', 'svo',
  'sdm', 'svm', 'sdl', 'svl', 'kno', 'sdv', 'lro', 'knm', 'lrm', 'knl', 'lrl', 'Bbo', 'knv',
  'Djo', 'Bbm', 'Djm', 'Bbl', 'Djl', 'Bbv', 'Djv', 'zbe', 'bfw', 'npz', 'zbd', 'bdy', 'bcz',
  'yae', 'DFz', 'yiu', 'yad', 'bhz', 'yit', 'wEe', 'wgu', 'wEd', 'wxu', 'wgt', 'wxt', 'scu',
  'stu', 'sct', 'tvu', 'stt', 'tvt', 'klu', 'lnu', 'klt', 'nru', 'lnt', 'nrt', 'BDu', 'Dbu',
  'BDt', 'bju', 'Dbt', 'bjt', 'jfs', 'rpy', 'jdw', 'roz', 'jcy', 'jcj', 'zbF', 'bFy', 'zjh',
  'jhy', 'bEz', 'jgz', 'yaF', 'yih', 'yyx', 'wEF', 'wgh', 'wwx', 'xxx', 'sch', 'ssx', 'ttx',
  'vvx', 'kkx', 'llx', 'nnx', 'rrx', 'BBx', 'DDx', 'bbx', 'jFw', 'rmz', 'jEy', 'jEj', 'bCz',
  'jaz', 'jCy', 'jCj', 'jBj', 'wCo', 'wCm', 'wCl', 'sFo', 'wCv', 'sFm', 'sFl', 'kfo', 'sFv',
  'kfm', 'kfl', 'Aro', 'kfv', 'Arm', 'Arl', 'Arv', 'yDe', 'Bpz', 'yDd', 'wCe', 'wau', 'wCd',
  'wat', 'sEu', 'shu', 'sEt', 'sht', 'kdu', 'kvu', 'kdt', 'kvt', 'Anu', 'Bru', 'Ant', 'Brt',
  'zDp', 'Dpy', 'Doz', 'yDF', 'ybh', 'wCF', 'wah', 'wix', 'sEh', 'sgx', 'sxx', 'kcx', 'ktx',
  'lvx', 'Alx', 'Bnx', 'Drx', 'bpw', 'nuz', 'boy', 'boj', 'Dmz', 'bqz', 'jps', 'ruy', 'jow',
  'ruj', 'joi', 'job', 'bmy', 'jqy', 'bmj', 'jqj', 'jmw', 'rtj', 'jmi', 'jmb', 'blj', 'jnj',
  'jli', 'jlb', 'jkr', 'sCu', 'sCt', 'kFu', 'kFt', 'Afu', 'Aft', 'wDh', 'sCh', 'sax', 'kEx',
  'khx', 'Adx', 'Avx', 'Buz', 'Duy', 'Duj', 'buw', 'nxj', 'bui', 'bub', 'Dtj', 'bvj', 'jus',
  'rxi', 'jug', 'rxb', 'jua', 'juD', 'bti', 'jvi', 'btb', 'jvb', 'jtg', 'rwr', 'jta', 'jtD',
  'bsr', 'jtr', 'jsq', 'jsn', 'Bxj', 'Dxi', 'Dxb', 'bxg', 'nyr', 'bxa', 'bxD', 'Dwr', 'bxr',
  'bwq', 'bwn', 'pjk', 'urw', 'ejA', 'pbs', 'uny', 'ebk', 'pDw', 'ulz', 'eDs', 'pBy', 'eBw',
  'zfc', 'fjk', 'prw', 'zfE', 'fbs', 'pny', 'zfC', 'fDw', 'plz', 'zfB', 'fBy', 'yrc', 'zfq',
  'frw', 'yrE', 'zfn', 'fny', 'yrC', 'flz', 'yrB', 'xjc', 'yrq', 'xjE', 'yrn', 'xjC', 'xjB',
  'uzc', 'xjq', 'uzE', 'xjn', 'uzC', 'uzB', 'pzc', 'uzq', 'pzE', 'uzn', 'pzC', 'djA', 'ors',
  'ufy', 'dbk', 'onw', 'udz', 'dDs', 'oly', 'dBw', 'okz', 'dAy', 'zdo', 'drs', 'ovy', 'zdm',
  'dnw', 'otz', 'zdl', 'dly', 'dkz', 'yno', 'zdv', 'dvy', 'ynm', 'dtz', 'ynl', 'xbo', 'ynv',
  'xbm', 'xbl', 'ujo', 'xbv', 'ujm', 'ujl', 'ozo', 'ujv', 'ozm', 'ozl', 'crk', 'ofw', 'uFz',
  'cns', 'ody', 'clw', 'ocz', 'cky', 'ckj', 'zcu', 'cvw', 'ohz', 'zct', 'cty', 'csz', 'ylu',
  'cxz', 'ylt', 'xDu', 'xDt', 'ubu', 'ubt', 'oju', 'ojt', 'cfs', 'oFy', 'cdw', 'oEz', 'ccy',
  'ccj', 'zch', 'chy', 'cgz', 'ykx', 'xBx', 'uDx', 'cFw', 'oCz', 'cEy', 'cEj', 'caz', 'cCy',
  'cCj', 'FjA', 'mrs', 'tfy', 'Fbk', 'mnw', 'tdz', 'FDs', 'mly', 'FBw', 'mkz', 'FAy', 'zFo',
  'Frs', 'mvy', 'zFm', 'Fnw', 'mtz', 'zFl', 'Fly', 'Fkz', 'yfo', 'zFv', 'Fvy', 'yfm', 'Ftz',
  'yfl', 'wro', 'yfv', 'wrm', 'wrl', 'tjo', 'wrv', 'tjm', 'tjl', 'mzo', 'tjv', 'mzm', 'mzl',
  'qrk', 'vfw', 'xpz', 'hbA', 'qns', 'vdy', 'hDk', 'qlw', 'vcz', 'hBs', 'qky', 'hAw', 'qkj',
  'hAi', 'Erk', 'mfw', 'tFz', 'hrk', 'Ens', 'mdy', 'hns', 'qty', 'mcz', 'hlw', 'Eky', 'hky',
  'Ekj', 'hkj', 'zEu', 'Evw', 'mhz', 'zhu', 'zEt', 'hvw', 'Ety', 'zht', 'hty', 'Esz', 'hsz',
  'ydu', 'Exz', 'yvu', 'ydt', 'hxz', 'yvt', 'wnu', 'xru', 'wnt', 'xrt', 'tbu', 'vju', 'tbt',
  'vjt', 'mju', 'mjt', 'grA', 'qfs', 'vFy', 'gnk', 'qdw', 'vEz', 'gls', 'qcy', 'gkw', 'qcj',
  'gki', 'gkb', 'Efs', 'mFy', 'gvs', 'Edw', 'mEz', 'gtw', 'qgz', 'gsy', 'Ecj', 'gsj', 'zEh',
  'Ehy', 'zgx', 'gxy', 'Egz', 'gwz', 'ycx', 'ytx', 'wlx', 'xnx', 'tDx', 'vbx', 'mbx', 'gfk',
  'qFw', 'vCz', 'gds', 'qEy', 'gcw', 'qEj', 'gci', 'gcb', 'EFw', 'mCz', 'ghw', 'EEy', 'ggy',
  'EEj', 'ggj', 'Eaz', 'giz', 'gFs', 'qCy', 'gEw', 'qCj', 'gEi', 'gEb', 'ECy', 'gay', 'ECj',
  'gaj', 'gCw', 'qBj', 'gCi', 'gCb', 'EBj', 'gDj', 'gBi', 'gBb', 'Crk', 'lfw', 'spz', 'Cns',
  'ldy', 'Clw', 'lcz', 'Cky', 'Ckj', 'zCu', 'Cvw', 'lhz', 'zCt', 'Cty', 'Csz', 'yFu', 'Cxz',
  'yFt', 'wfu', 'wft', 'sru', 'srt', 'lju', 'ljt', 'arA', 'nfs', 'tpy', 'ank', 'ndw', 'toz',
  'als', 'ncy', 'akw', 'ncj', 'aki', 'akb', 'Cfs', 'lFy', 'avs', 'Cdw', 'lEz', 'atw', 'ngz',
  'asy', 'Ccj', 'asj', 'zCh', 'Chy', 'zax', 'axy', 'Cgz', 'awz', 'yEx', 'yhx', 'wdx', 'wvx',
  'snx', 'trx', 'lbx', 'rfk', 'vpw', 'xuz', 'inA', 'rds', 'voy', 'ilk', 'rcw', 'voj', 'iks',
  'rci', 'ikg', 'rcb', 'ika', 'afk', 'nFw', 'tmz', 'ivk', 'ads', 'nEy', 'its', 'rgy', 'nEj',
  'isw', 'aci', 'isi', 'acb', 'isb', 'CFw', 'lCz', 'ahw', 'CEy', 'ixw', 'agy', 'CEj', 'iwy',
  'agj', 'iwj', 'Caz', 'aiz', 'iyz', 'ifA', 'rFs', 'vmy', 'idk', 'rEw', 'vmj', 'ics', 'rEi',
  'icg', 'rEb', 'ica', 'icD', 'aFs', 'nCy', 'ihs', 'aEw', 'nCj', 'igw', 'raj', 'igi', 'aEb',
  'igb', 'CCy', 'aay', 'CCj', 'iiy', 'aaj', 'iij', 'iFk', 'rCw', 'vlj', 'iEs', 'rCi', 'iEg',
  'rCb', 'iEa', 'iED', 'aCw', 'nBj', 'iaw', 'aCi', 'iai', 'aCb', 'iab', 'CBj', 'aDj', 'ibj',
  'iCs', 'rBi', 'iCg', 'rBb', 'iCa', 'iCD', 'aBi', 'iDi', 'aBb', 'iDb', 'iBg', 'rAr', 'iBa',
  'iBD', 'aAr', 'iBr', 'iAq', 'iAn', 'Bfs', 'kpy', 'Bdw', 'koz', 'Bcy', 'Bcj', 'Bhy', 'Bgz',
  'yCx', 'wFx', 'sfx', 'krx', 'Dfk', 'lpw', 'suz', 'Dds', 'loy', 'Dcw', 'loj', 'Dci', 'Dcb',
  'BFw', 'kmz', 'Dhw', 'BEy', 'Dgy', 'BEj', 'Dgj', 'Baz', 'Diz', 'bfA', 'nps', 'tuy', 'bdk',
  'now', 'tuj', 'bcs', 'noi', 'bcg', 'nob', 'bca', 'bcD', 'DFs', 'lmy', 'bhs', 'DEw', 'lmj',
  'bgw', 'DEi', 'bgi', 'DEb', 'bgb', 'BCy', 'Day', 'BCj', 'biy', 'Daj', 'bij', 'rpk', 'vuw',
  'xxj', 'jdA', 'ros', 'vui', 'jck', 'rog', 'vub', 'jcc', 'roa', 'jcE', 'roD', 'jcC', 'bFk',
  'nmw', 'ttj', 'jhk', 'bEs', 'nmi', 'jgs', 'rqi', 'nmb', 'jgg', 'bEa', 'jga', 'bED', 'jgD',
  'DCw', 'llj', 'baw', 'DCi', 'jiw', 'bai', 'DCb', 'jii', 'bab', 'jib', 'BBj', 'DDj', 'bbj',
  'jjj', 'jFA', 'rms', 'vti', 'jEk', 'rmg', 'vtb', 'jEc', 'rma', 'jEE', 'rmD', 'jEC', 'jEB',
  'bCs', 'nli', 'jas', 'bCg', 'nlb', 'jag', 'rnb', 'jaa', 'bCD', 'jaD', 'DBi', 'bDi', 'DBb',
  'jbi', 'bDb', 'jbb', 'jCk', 'rlg', 'vsr', 'jCc', 'rla', 'jCE', 'rlD', 'jCC', 'jCB', 'bBg',
  'nkr', 'jDg', 'bBa', 'jDa', 'bBD', 'jDD', 'DAr', 'bBr', 'jDr', 'jBc', 'rkq', 'jBE', 'rkn',
  'jBC', 'jBB', 'bAq', 'jBq', 'bAn', 'jBn', 'jAo', 'rkf', 'jAm', 'jAl', 'bAf', 'jAv', 'Apw',
  'kez', 'Aoy', 'Aoj', 'Aqz', 'Bps', 'kuy', 'Bow', 'kuj', 'Boi', 'Bob', 'Amy', 'Bqy', 'Amj',
  'Bqj', 'Dpk', 'luw', 'sxj', 'Dos', 'lui', 'Dog', 'lub', 'Doa', 'DoD', 'Bmw', 'ktj', 'Dqw',
  'Bmi', 'Dqi', 'Bmb', 'Dqb', 'Alj', 'Bnj', 'Drj', 'bpA', 'nus', 'txi', 'bok', 'nug', 'txb',
  'boc', 'nua', 'boE', 'nuD', 'boC', 'boB', 'Dms', 'lti', 'bqs', 'Dmg', 'ltb', 'bqg', 'nvb',
  'bqa', 'DmD', 'bqD', 'Bli', 'Dni', 'Blb', 'bri', 'Dnb', 'brb', 'ruk', 'vxg', 'xyr', 'ruc',
  'vxa', 'ruE', 'vxD', 'ruC', 'ruB', 'bmk', 'ntg', 'twr', 'jqk', 'bmc', 'nta', 'jqc', 'rva',
  'ntD', 'jqE', 'bmC', 'jqC', 'bmB', 'jqB', 'Dlg', 'lsr', 'bng', 'Dla', 'jrg', 'bna', 'DlD',
  'jra', 'bnD', 'jrD', 'Bkr', 'Dlr', 'bnr', 'jrr', 'rtc', 'vwq', 'rtE', 'vwn', 'rtC', 'rtB',
  'blc', 'nsq', 'jnc', 'blE', 'nsn', 'jnE', 'rtn', 'jnC', 'blB', 'jnB', 'Dkq', 'blq', 'Dkn',
  'jnq', 'bln', 'jnn', 'rso', 'vwf', 'rsm', 'rsl', 'bko', 'nsf', 'jlo', 'bkm', 'jlm', 'bkl',
  'jll', 'Dkf', 'bkv', 'jlv', 'rse', 'rsd', 'bke', 'jku', 'bkd', 'jkt', 'Aey', 'Aej', 'Auw',
  'khj', 'Aui', 'Aub', 'Adj', 'Avj', 'Bus', 'kxi', 'Bug', 'kxb', 'Bua', 'BuD', 'Ati', 'Bvi',
  'Atb', 'Bvb', 'Duk', 'lxg', 'syr', 'Duc', 'lxa', 'DuE', 'lxD', 'DuC', 'DuB', 'Btg', 'kwr',
  'Dvg', 'lxr', 'Dva', 'BtD', 'DvD', 'Asr', 'Btr', 'Dvr', 'nxc', 'tyq', 'nxE', 'tyn', 'nxC',
  'nxB', 'Dtc', 'lwq', 'bvc', 'nxq', 'lwn', 'bvE', 'DtC', 'bvC', 'DtB', 'bvB', 'Bsq', 'Dtq',
  'Bsn', 'bvq', 'Dtn', 'bvn', 'vyo', 'xzf', 'vym', 'vyl', 'nwo', 'tyf', 'rxo', 'nwm', 'rxm',
  'nwl', 'rxl', 'Dso', 'lwf', 'bto', 'Dsm', 'jvo', 'btm', 'Dsl', 'jvm', 'btl', 'jvl', 'Bsf',
  'Dsv', 'btv', 'jvv', 'vye', 'vyd', 'nwe', 'rwu', 'nwd', 'rwt', 'Dse', 'bsu', 'Dsd', 'jtu',
  'bst', 'jtt', 'vyF', 'nwF', 'rwh', 'DsF', 'bsh', 'jsx', 'Ahi', 'Ahb', 'Axg', 'kir', 'Axa',
  'AxD', 'Agr', 'Axr', 'Bxc', 'kyq', 'BxE', 'kyn', 'BxC', 'BxB', 'Awq', 'Bxq', 'Awn', 'Bxn',
  'lyo', 'szf', 'lym', 'lyl', 'Bwo', 'kyf', 'Dxo', 'lyv', 'Dxm', 'Bwl', 'Dxl', 'Awf', 'Bwv',
  'Dxv', 'tze', 'tzd', 'lye', 'nyu', 'lyd', 'nyt', 'Bwe', 'Dwu', 'Bwd', 'bxu', 'Dwt', 'bxt',
  'tzF', 'lyF', 'nyh', 'BwF', 'Dwh', 'bwx', 'Aiq', 'Ain', 'Ayo', 'kjf', 'Aym', 'Ayl', 'Aif',
  'Ayv', 'kze', 'kzd', 'Aye', 'Byu', 'Ayd', 'Byt', 'szp' );

implementation

uses zint_helper;

{
   Three figure numbers in comments give the location of command equivalents in the
   original Visual Basic source code file pdf417.frm
   this code retains some original (French) procedure and variable names to ease conversion }

{ text mode processing tables }

const asciix : array[0..94] of Integer = ( 7, 8, 8, 4, 12, 4, 4, 8, 8, 8, 12, 4, 12, 12, 12, 12, 4, 4, 4, 4, 4, 4, 4, 4,
  4, 4, 12, 8, 8, 4, 8, 8, 8, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
  1, 1, 1, 1, 8, 8, 8, 4, 8, 8, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
  2, 2, 2, 2, 8, 8, 8, 8 );

const asciiy : array[0..94] of Integer = ( 26, 10, 20, 15, 18, 21, 10, 28, 23, 24, 22, 20, 13, 16, 17, 19, 0, 1, 2, 3,
  4, 5, 6, 7, 8, 9, 14, 0, 1, 23, 2, 25, 3, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
  16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 4, 5, 6, 24, 7, 8, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
  11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 21, 27, 9 );

{ Automatic sizing table }

const MicroAutosize : array[0..55] of Integer =
(	4, 6, 7, 8, 10, 12, 13, 14, 16, 18, 19, 20, 24, 29, 30, 33, 34, 37, 39, 46, 54, 58, 70, 72, 82, 90, 108, 126,
  1, 14, 2, 7, 3, 25, 8, 16, 5, 17, 9, 6, 10, 11, 28, 12, 19, 13, 29, 20, 30, 21, 22, 31, 23, 32, 33, 34
);

type
  TGLoballiste = array[0..1] of array[0..999] of Integer;

{ 866 }

function quelmode(codeascii : Char) : Integer;
var
  mode : Integer;
begin
  mode := BYT;
  if ((codeascii = #9) or (codeascii = #10) or (codeascii = #13) or ((codeascii >= ' ') and (codeascii <= '~'))) then mode := TEX
  else if ((codeascii >= '0') and (codeascii <= '9')) then mode := NUM;
  { 876 }
  result := mode; exit;
end;

{ 844 }
procedure regroupe(var indexliste : Integer; var liste : TGLoballiste);
var
  i, j : Integer;
begin
  { bring together same type blocks }
  if (indexliste > 1) then
  begin
    i := 1;
    while(i < indexliste) do
    begin
      if (liste[1][i - 1] = liste[1][i]) then
      begin
        { bring together }
        liste[0][i - 1] := liste[0][i - 1] + liste[0][i];
        j := i + 1;

        { decreace the list }
        while(j < indexliste) do
        begin
          liste[0][j - 1] := liste[0][j];
          liste[1][j - 1] := liste[1][j];
          Inc(j);
        end;
        indexliste := indexliste - 1;
        Dec(i);
      end;
      Inc(i);
    end;
  end;
  { 865 }
end;


{ 478 }
procedure pdfsmooth(var indexliste : Integer; var liste : TGLoballiste);
var
  i, crnt, last, next, _length : Integer;
begin
  for i := 0 to indexliste - 1 do
  begin
    crnt := liste[1][i];
    _length := liste[0][i];
    if (i <> 0) then last := liste[1][i - 1] else last := _FALSE;
    if (i <> indexliste - 1) then next := liste[1][i + 1] else next := _FALSE;

    if (crnt = NUM) then
    begin
      if (i = 0) then
      begin { first block }
        if (indexliste > 1) then
        begin { and there are others }
          if ((next = TEX) and (_length < 8)) then liste[1][i] := TEX;
          if ((next = BYT) and (_length = 1)) then liste[1][i] := BYT;
        end;
      end
      else
      begin
        if (i = indexliste - 1) then
        begin { last block }
          if ((last = TEX) and (_length < 7)) then liste[1][i] := TEX;
          if ((last = BYT) and (_length = 1)) then liste[1][i] := BYT;
        end
        else
        begin { not first or last block }
          if (((last = BYT) and (next = BYT)) and (_length < 4)) then liste[1][i] := BYT;
          if (((last = BYT) and (next = TEX)) and (_length < 4)) then liste[1][i] := TEX;
          if (((last = TEX) and (next = BYT)) and (_length < 5)) then liste[1][i] := TEX;
          if (((last = TEX) and (next = TEX)) and (_length < 8)) then liste[1][i] := TEX;
        end;
      end;
    end;
  end;
  regroupe(indexliste, liste);
  { 520 }
  for i := 0 to indexliste - 1 do
  begin
    crnt := liste[1][i];
    _length := liste[0][i];
    if (i <> 0) then last := liste[1][i - 1] else last := _FALSE;
    if (i <> indexliste - 1) then next := liste[1][i + 1] else next := _FALSE;

    if ((crnt = TEX) and (i > 0)) then
    begin { not the first }
      if (i = indexliste - 1) then
      begin { the last one }
        if ((last = BYT) and (_length = 1)) then liste[1][i] := BYT;
      end
      else
      begin { not the last one }
        if (((last = BYT) and (next = BYT)) and (_length < 5)) then liste[1][i] := BYT;
        if ((((last = BYT) and (next <> BYT)) or ((last <> BYT) and (next = BYT))) and (_length < 3)) then
          liste[1][i] := BYT;
      end;
    end;
  end;
  { 540 }
  regroupe(indexliste, liste);
end;

{ 547 }
procedure textprocess(var chainemc : TArrayOfInteger; var mc_length : Integer; chaine : TArrayOfChar; start : Integer; _length : Integer; block : Integer);
var
  j, indexlistet, curtable : Integer;
  listet : array[0..1] of array[0..4999] of Integer;
  chainet: array[0..4999] of Integer;
  wnet : Integer;
  codeascii : Char;
  flag : Integer;
  newtable : Integer;
  cw_number : Integer;
begin
  //codeascii := #0;
  wnet := 0;

  for j := 0 to 999 do
    listet[0][j] := 0;

  { listet will contain the table numbers and the value of each characters }
  for indexlistet := 0 to _length - 1 do
  begin
    codeascii := chaine[start + indexlistet];
    case codeascii of
      #9:
      begin
        listet[0][indexlistet] := 12; listet[1][indexlistet] := 12;
      end;
      #10:
      begin
        listet[0][indexlistet] := 8; listet[1][indexlistet] := 15;
      end;
      #13:
      begin
        listet[0][indexlistet] := 12; listet[1][indexlistet] := 11;
      end;
      else
      begin
        listet[0][indexlistet] := asciix[Ord(codeascii) - 32];
        listet[1][indexlistet] := asciiy[Ord(codeascii) - 32];
      end;
    end;
  end;

  { 570 }
  curtable := 1; { default table }
  for j := 0 to _length - 1 do
  begin
    if (listet[0][j] and curtable) <> 0 then
    begin { The character is in the current table }
      chainet[wnet] := listet[1][j];
      Inc(wnet);
    end
    else
    begin { Obliged to change table }
      flag := _FALSE; { True if we change table for only one character }
      if (j = (_length - 1)) then
        flag := _TRUE
      else
        if not ((listet[0][j] and listet[0][j + 1]) <> 0) then flag := _TRUE;

      if (flag <> 0) then
      begin { we change only one character - look for temporary switch }
        if (((listet[0][j] and 1) <> 0) and (curtable = 2)) then
        begin { T_UPP }
          chainet[wnet] := 27;
          chainet[wnet + 1] := listet[1][j];
          Inc(wnet, 2);
        end;
        if (listet[0][j] and 8) <> 0 then
        begin { T_PUN }
          chainet[wnet] := 29;
          chainet[wnet + 1] := listet[1][j];
          Inc(wnet, 2);
        end;
        if (not ((((listet[0][j] and 1) <> 0) and (curtable = 2)) or ((listet[0][j] and 8) <> 0))) then
        begin
          { No temporary switch available }
          flag := _FALSE;
        end;
      end;

      { 599 }
      if (not (flag <> 0)) then
      begin
        if (j = (_length - 1)) then
          newtable := listet[0][j]
        else
        begin
          if (not ((listet[0][j] and listet[0][j + 1]) <> 0)) then
            newtable := listet[0][j]
          else
            newtable := listet[0][j] and listet[0][j + 1];
        end;

        { Maintain the first if several tables are possible }
        case newtable of
          3,
          5,
          7,
          9,
          11,
          13,
          15:
            newtable := 1;
          6,
          10,
          14:
            newtable := 2;
          12:
            newtable := 4;
        end;

        { 619 - select the switch }
        case curtable of
          1:
            case newtable of
              2: begin chainet[wnet] := 27; Inc(wnet); end;
              4: begin chainet[wnet] := 28; Inc(wnet); end;
              8: begin chainet[wnet] := 28; Inc(wnet); chainet[wnet] := 25; Inc(wnet); end;
            end;
          2:
            case newtable of
              1: begin chainet[wnet] := 28; Inc(wnet); chainet[wnet] := 28; Inc(wnet);  end;
              4: begin chainet[wnet] := 28; Inc(wnet);  end;
              8: begin chainet[wnet] := 28; Inc(wnet); chainet[wnet] := 25; Inc(wnet);  end;
            end;
          4:
            case newtable of
              1: begin chainet[wnet] := 28; Inc(wnet);  end;
              2: begin chainet[wnet] := 27; Inc(wnet);  end;
              8: begin chainet[wnet] := 25; Inc(wnet);  end;
            end;
          8:
            case newtable of
              1: begin chainet[wnet] := 29; Inc(wnet);  end;
              2: begin chainet[wnet] := 29; Inc(wnet); chainet[wnet] := 27; Inc(wnet);  end;
              4: begin chainet[wnet] := 29; Inc(wnet); chainet[wnet] := 28; Inc(wnet);  end;
            end;
        end;
        curtable := newtable;
        { 659 - at last we add the character }
        chainet[wnet] := listet[1][j];
        Inc(wnet);
      end;
    end;
  end;

  { 663 }
  if (wnet and 1) <> 0 then
  begin
    chainet[wnet] := 29;
    Inc(wnet);
  end;
  { Now translate the string chainet into codewords }
  chainemc[mc_length] := 900;
  mc_length := mc_length + 1;

  j := 0;
  while j < wnet do
  begin
    cw_number := (30 * chainet[j]) + chainet[j + 1];
    chainemc[mc_length] := cw_number;
    mc_length := mc_length + 1;
    Inc(j, 2);
  end;
end;

{ 671 }
procedure byteprocess(var chainemc : TArrayOfInteger; var mc_length : Integer; chaine : TArrayOfByte; start : Integer; _length : Integer; block : Integer);
var
  len : Integer;
  chunkLen : UInt64;
  mantisa : UInt64;
  total : UInt64;
begin
  len := 0;
  //chunkLen := 0;
  //mantisa := 0;
  //total := 0;

  if (_length = 1) then
  begin
    chainemc[mc_length] := 913;
    Inc(mc_length);
    chainemc[mc_length] := chaine[start];
    Inc(mc_length);
  end
  else
  begin
    { select the switch for multiple of 6 bytes }
    if (_length mod 6 = 0) then
    begin
      chainemc[mc_length] := 924;
      Inc(mc_length);
    end
    else
    begin
      chainemc[mc_length] := 901;
      Inc(mc_length);
    end;

    while (len < _length) do
    begin
      chunkLen := _length - len;
      if (6 <= chunkLen) then { Take groups of 6 }
      begin
        chunkLen  := 6;
        Inc(len, chunkLen);
        total := 0;

        while (chunkLen > 0) do
        begin
          Dec(chunkLen);
          mantisa := chaine[start];
          Inc(start);
          total := total or (mantisa shl UInt64(chunkLen * 8));
        end;

        chunkLen := 5;

        while (chunkLen > 0) do
        begin
          Dec(chunkLen);
          chainemc[mc_length + Int64(chunkLen)] := Integer(total mod 900);
          total := total div 900;
        end;
        Inc(mc_length, 5);
      end
      else  {  If it remain a group of less than 6 bytes   }
      begin
        Inc(len, chunkLen);
        while (chunkLen > 0) do
        begin
          Dec(chunkLen);
          chainemc[mc_length] := chaine[start];
          Inc(mc_length);
          Inc(start);
        end;
      end;
    end;
  end;
end;

{ 712 }
procedure numbprocess(var chainemc : TArrayOfInteger; var mc_length : Integer; chaine : TArrayOfChar; start : Integer; _length : Integer; block : Integer);
var
  j, loop, longueur, dum_length, diviseur, nombre : Integer;
  dummy : array[0..99] of Integer;
  chainemod, chainemult : TArrayOfChar;
  temp : Char;
begin
  SetLength(chainemod, 50);
  SetLength(chainemult, 100);
  strcpy(chainemod, '');
  for loop := 0 to 50 do
    dummy[loop] := 0;

  chainemc[mc_length] := 902;
  mc_length := mc_length + 1;

  j := 0;
  while(j < _length) do
  begin
    dum_length := 0;
    strcpy(chainemod, '');
    longueur := _length - j;
    if (longueur > 44) then longueur := 44;
    concat(chainemod, '1');
    for loop := 1 to longueur do
      chainemod[loop] := chaine[start + loop + j - 1];
    chainemod[longueur + 1] := #0;

    repeat
      diviseur := 900;

      { 877 - gosub Modulo }
      strcpy(chainemult, '');
      nombre := 0;
      while (strlen(chainemod) <> 0) do
      begin
        nombre := nombre * 10;
        Inc(nombre, ctoi(chainemod[0]));
        for loop := 0 to strlen(chainemod) - 1 do
          chainemod[loop] := chainemod[loop + 1];

        if (nombre < diviseur) then
        begin
          if (strlen(chainemult) <> 0) then concat(chainemult, '0');
        end
        else
        begin
          temp := Chr((nombre div diviseur) + Ord('0'));
          chainemult[strlen(chainemult) + 1] := #0;
          chainemult[strlen(chainemult)] := temp;
        end;
        nombre := nombre mod diviseur;
      end;
      diviseur := nombre;
      { return to 723 }

      for loop := dum_length downto 1 do
        dummy[loop] := dummy[loop - 1];

      dummy[0] := diviseur;
      Inc(dum_length);
      strcpy(chainemod, chainemult);
    until not (strlen(chainemult) <> 0);

    for loop := 0 to dum_length - 1 do
    begin
      chainemc[mc_length] := dummy[loop];
      mc_length := mc_length + 1;
    end;
    Inc(j, longueur);
  end;
end;

{ 366 }
function pdf417(symbol : zint_symbol; chaine : TArrayOfByte; _length : Integer) : Integer;
var
  i, k, j, indexchaine, indexliste, mode, longueur, loop, offset : Integer;
  mccorrection : array[0..519] of Integer;
  total, mc_length, c1, c2, c3, codeerr : Integer;
  chainemc : TArrayOfInteger;
  dummy : array[0..34] of Integer;
  codebarre, pattern : TArrayOfChar;
  liste : TGLoballiste;
begin
  SetLength(chainemc, 2700);
  SetLength(codebarre, 140);
  SetLength(pattern, 580);
  codeerr := 0;

  { 456 }
  indexliste := 0;
  indexchaine := 0;

  mode := quelmode(Chr(chaine[indexchaine]));

  for i := 0 to 999 do
    liste[0][i] := 0;

  { 463 }
  repeat
    liste[1][indexliste] := mode;
    while ((liste[1][indexliste] = mode) and (indexchaine < _length)) do
    begin
      Inc(liste[0][indexliste]);
      Inc(indexchaine);
      mode := quelmode(Chr(chaine[indexchaine]));
    end;
    Inc(indexliste);
  until not (indexchaine < _length);

  { 474 }
  pdfsmooth(indexliste, liste);

  { 541 - now compress the data }
  indexchaine := 0;
  mc_length := 0;
  if (symbol.output_options and READER_INIT) <> 0 then
  begin
    chainemc[mc_length] := 921; { Reader Initialisation }
    Inc(mc_length);
  end;
  for i := 0 to indexliste - 1 do
  begin
    case liste[1][i] of
      TEX: { 547 - text mode }
        textprocess(chainemc, mc_length, ArrayOfByteToArrayOfChar(chaine), indexchaine, liste[0][i], i);
      BYT: { 670 - octet stream mode }
        byteprocess(chainemc, mc_length, chaine, indexchaine, liste[0][i], i);
      NUM: { 712 - numeric mode }
        numbprocess(chainemc, mc_length, ArrayOfByteToArrayOfChar(chaine), indexchaine, liste[0][i], i);
    end;
    indexchaine := indexchaine + liste[0][i];
  end;

  { 752 - Now take care of the number of CWs per row }
  if (symbol.option_1 < 0) then
  begin
    symbol.option_1 := 6;
    if (mc_length <= 863) then symbol.option_1 := 5;
    if (mc_length <= 320) then symbol.option_1 := 4;
    if (mc_length <= 160) then symbol.option_1 := 3;
    if (mc_length <= 40) then symbol.option_1 := 2;
  end;
  k := 1;
  for loop := 1 to symbol.option_1 + 1 do
    k := k * 2;

  longueur := mc_length;
  if (symbol.option_2 > 30) then symbol.option_2 := 30;
  if (symbol.option_2 < 1) then
    symbol.option_2 := Trunc(0.5 + sqrt((longueur + k) / 3.0));

  if (((longueur + k) / symbol.option_2) > 90) then
    { stop the symbol from becoming too high }
    symbol.option_2 := symbol.option_2 + 1;


  if (longueur + k > 928) then
  begin
    { Enforce maximum codeword limit }
    result := 2; exit;
  end;

  if (((longueur + k) / symbol.option_2) > 90) then
  begin
    result := 4; exit;
  end;

  { 781 - Padding calculation }
  longueur := mc_length + 1 + k;
  i := 0;
  if ((longueur / symbol.option_2) < 3) then
    i := (symbol.option_2 * 3) - longueur { A bar code must have at least three rows }
  else
    if ((longueur mod symbol.option_2) > 0) then i := symbol.option_2 - (longueur mod symbol.option_2);

  { We add the padding }
  while (i > 0) do
  begin
    chainemc[mc_length] := 900;
    Inc(mc_length);
    Dec(i);
  end;
  { we add the _length descriptor }
  for i := mc_length downto 1 do
    chainemc[i] := chainemc[i - 1];
  chainemc[0] := mc_length + 1;
  Inc(mc_length);

  { 796 - we now take care of the Reed Solomon codes }
  case symbol.option_1 of
    1: offset := 2;
    2: offset := 6;
    3: offset := 14;
    4: offset := 30;
    5: offset := 62;
    6: offset := 126;
    7: offset := 254;
    8: offset := 510;
    else
      offset := 0;
  end;

  longueur := mc_length;
  for loop := 0 to 519 do
    mccorrection[loop] := 0;

  //total := 0;
  for i := 0 to longueur - 1 do
  begin
    total := (chainemc[i] + mccorrection[k - 1]) mod 929;
    for j := k - 1 downto 1 do
      mccorrection[j] := (mccorrection[j - 1] + 929 - (total * coefrs[offset + j]) mod 929) mod 929;
    j := 0;
    mccorrection[0] := (929 - (total * coefrs[offset + j]) mod 929) mod 929;
  end;

  { we add these codes to the string }
  for i := k - 1 downto 0 do
  begin
    if mccorrection[i] <> 0 then
      chainemc[mc_length] := 929 - mccorrection[i]
    else
      chainemc[mc_length] := 0;
    Inc(mc_length);
  end;

  { 818 - The CW string is finished }
  c1 := (mc_length div symbol.option_2 - 1) div 3;
  c2 := symbol.option_1 * 3 + (mc_length div symbol.option_2 - 1) mod 3;
  c3 := symbol.option_2 - 1;

  { we now encode each row }
  for i := 0 to (mc_length div symbol.option_2) - 1 do
  begin
    for j := 0 to symbol.option_2 - 1 do
      dummy[j + 1] := chainemc[i * symbol.option_2 + j];

    k := (i div 3) * 30;
    case i mod 3 of
        { follows this pattern from US Patent 5,243,655:
      Row 0: L0 (row #, # of rows)         R0 (row #, # of columns)
      Row 1: L1 (row #, security level)    R1 (row #, # of rows)
      Row 2: L2 (row #, # of columns)      R2 (row #, security level)
      Row 3: L3 (row #, # of rows)         R3 (row #, # of columns)
        etc. }
      0:
      begin
        dummy[0] := k + c1;
        dummy[symbol.option_2 + 1] := k + c3;
      end;
      1:
      begin
        dummy[0] := k + c2;
        dummy[symbol.option_2 + 1] := k + c1;
      end;
      2:
      begin
        dummy[0] := k + c3;
        dummy[symbol.option_2 + 1] := k + c2;
      end;
    end;
    strcpy(codebarre, '+*'); { Start with a start char and a separator }
    if (symbol.symbology = BARCODE_PDF417TRUNC) then
    begin
      { truncated - so same as before except knock off the last 5 chars }
      for j := 0 to symbol.option_2 do
      begin
        case i mod 3 of
          1: offset := 929;
          2: offset := 1858;
          else
            offset := 0;
        end;
        concat(codebarre, codagemc[offset + dummy[j]]);
        concat(codebarre, '*');
      end;
    end
    else
    begin
      { normal PDF417 symbol }
      for j := 0 to symbol.option_2 + 1 do
      begin
        case i mod 3 of
          1: offset := 929; { cluster(3) }
          2: offset := 1858; { cluster(6) }
          else
            offset := 0; { cluster(0) }
        end;
        concat(codebarre, codagemc[offset + dummy[j]]);
        concat(codebarre, '*');
      end;
      concat(codebarre, '-');
    end;

    strcpy(pattern, '');
    for loop := 0 to strlen(codebarre) - 1 do
      lookup(BRSET, PDFttf, codebarre[loop], pattern);

    for loop := 0 to strlen(pattern) - 1 do
      if (pattern[loop] = '1') then set_module(symbol, i, loop);

    //if (symbol.height = 0) then
      symbol.row_height[i] := 3;
  end;
  symbol.rows := (mc_length div symbol.option_2);
  symbol.width := strlen(pattern);

  { 843 }
  result := codeerr; exit;
end;

{ 345 }
function pdf417enc(symbol : zint_symbol; source : TArrayOfByte; _length : Integer) : Integer;
var
  codeerr, error_number : Integer;
begin
  error_number := 0;

  if ((symbol.option_1 < -1) or (symbol.option_1 > 8)) then
  begin
    strcpy(symbol.errtxt, 'Security value out of range');
    symbol.option_1 := -1;
    error_number := ZWARN_INVALID_OPTION;
  end;
  if ((symbol.option_2 < 0) or (symbol.option_2 > 30)) then
  begin
    strcpy(symbol.errtxt, 'Number of columns out of range');
    symbol.option_2 := 0;
    error_number := ZWARN_INVALID_OPTION;
  end;

  { 349 }
  codeerr := pdf417(symbol, source, _length);

  { 352 }
  if (codeerr <> 0) then
  begin
    case codeerr of
      1:
      begin
        strcpy(symbol.errtxt, 'No such file or file unreadable');
        error_number := ZERROR_INVALID_OPTION;
      end;
      2:
      begin
        strcpy(symbol.errtxt, 'Input string too long');
        error_number := ZERROR_TOO_LONG;
      end;
      3:
      begin
        strcpy(symbol.errtxt, 'Number of codewords per row too small');
        error_number := ZWARN_INVALID_OPTION;
      end;
      4:
      begin
        strcpy(symbol.errtxt, 'Data too long for specified number of columns');
        error_number := ZERROR_TOO_LONG;
      end;
      else
        strcpy(symbol.errtxt, 'Something strange happened');
        error_number := ZERROR_ENCODING_PROBLEM;
    end;
  end;

  { 364 }
  result := error_number; exit;
end;

{ like PDF417 only much smaller! }
function micro_pdf417(symbol : zint_symbol; chaine : TArrayOfByte; _length : Integer) : Integer;
var
  i, k, j, indexchaine, indexliste, mode, longueur, offset : Integer;
  mccorrection : array[0..49] of Integer;
  total, mc_length, codeerr : Integer;
  chainemc : TArrayOfInteger;
  dummy : array[0..5] of Integer;
  codebarre, pattern : TArrayOfChar;
  variant, LeftRAPStart, CentreRAPStart, RightRAPStart, StartCluster : Integer;
  LeftRAP, CentreRAP, RightRAP, Cluster, writer, flip, loop : Integer;
  liste : TGLoballiste;
begin
  SetLength(chainemc, 2700);
  SetLength(codebarre, 100);
  SetLength(pattern, 580);
  { Encoding starts out the same as PDF417, so use the same code }
  codeerr := 0;

  { 456 }
  indexliste := 0;
  indexchaine := 0;

  mode := quelmode(Chr(chaine[indexchaine]));

  for i := 0 to 999 do
    liste[0][i] := 0;

  { 463 }
  repeat
    liste[1][indexliste] := mode;
    while ((liste[1][indexliste] = mode) and (indexchaine < _length)) do
    begin
      Inc(liste[0][indexliste]);
      Inc(indexchaine);
      mode := quelmode(Chr(chaine[indexchaine]));
    end;
    Inc(indexliste);
  until not (indexchaine < _length);

  { 474 }
  pdfsmooth(indexliste, liste);

  { 541 - now compress the data }
  indexchaine := 0;
  mc_length := 0;
  if (symbol.output_options and READER_INIT) <> 0 then
  begin
    chainemc[mc_length] := 921; { Reader Initialisation }
    Inc(mc_length);
  end;
  for i := 0 to indexliste - 1 do
  begin
    case liste[1][i] of
      TEX: { 547 - text mode }
        textprocess(chainemc, mc_length, ArrayOfByteToArrayOfChar(chaine), indexchaine, liste[0][i], i);
      BYT: { 670 - octet stream mode }
        byteprocess(chainemc, mc_length, chaine, indexchaine, liste[0][i], i);
      NUM: { 712 - numeric mode }
        numbprocess(chainemc, mc_length, ArrayOfByteToArrayOfChar(chaine), indexchaine, liste[0][i], i);
    end;
    indexchaine := indexchaine + liste[0][i];
  end;

  { This is where it all changes! }

  if (mc_length > 126) then
  begin
    strcpy(symbol.errtxt, 'Input data too long');
    result := ZERROR_TOO_LONG; exit;
  end;
  if (symbol.option_2 > 4) then
  begin
    strcpy(symbol.errtxt, 'Specified width out of range');
    symbol.option_2 := 0;
    codeerr := ZWARN_INVALID_OPTION;
  end;

  { Now figure out which variant of the symbol to use and load values accordingly }

  variant := 0;

  if ((symbol.option_2 = 1) and (mc_length > 20)) then
  begin
    { the user specified 1 column but the data doesn't fit - go to automatic }
    symbol.option_2 := 0;
    strcpy(symbol.errtxt, 'Specified symbol size too small for data');
    codeerr := ZWARN_INVALID_OPTION;
  end;

  if ((symbol.option_2 = 2) and (mc_length > 37)) then
  begin
    { the user specified 2 columns but the data doesn't fit - go to automatic }
    symbol.option_2 := 0;
    strcpy(symbol.errtxt, 'Specified symbol size too small for data');
    codeerr := ZWARN_INVALID_OPTION;
  end;

  if ((symbol.option_2 = 3) and (mc_length > 82)) then
  begin
    { the user specified 3 columns but the data doesn't fit - go to automatic }
    symbol.option_2 := 0;
    strcpy(symbol.errtxt, 'Specified symbol size too small for data');
    codeerr := ZWARN_INVALID_OPTION;
  end;

  if (symbol.option_2 = 1) then
  begin
    { the user specified 1 column and the data does fit }
    variant := 6;
    if (mc_length <= 16) then variant := 5;
    if (mc_length <= 12) then variant := 4;
    if (mc_length <= 10) then variant := 3;
    if (mc_length <= 7) then variant := 2;
    if (mc_length <= 4) then variant := 1;
  end;

  if (symbol.option_2 = 2) then
  begin
    { the user specified 2 columns and the data does fit }
    variant := 13;
    if (mc_length <= 33) then variant := 12;
    if (mc_length <= 29) then variant := 11;
    if (mc_length <= 24) then variant := 10;
    if (mc_length <= 19) then variant := 9;
    if (mc_length <= 13) then variant := 8;
    if (mc_length <= 8) then variant := 7;
  end;

  if (symbol.option_2 = 3) then
  begin
    { the user specified 3 columns and the data does fit }
    variant := 23;
    if (mc_length <= 70) then variant := 22;
    if (mc_length <= 58) then variant := 21;
    if (mc_length <= 46) then variant := 20;
    if (mc_length <= 34) then variant := 19;
    if (mc_length <= 24) then variant := 18;
    if (mc_length <= 18) then variant := 17;
    if (mc_length <= 14) then variant := 16;
    if (mc_length <= 10) then variant := 15;
    if (mc_length <= 6) then variant := 14;
  end;

  if (symbol.option_2 = 4) then
  begin
    { the user specified 4 columns and the data does fit }
    variant := 34;
    if (mc_length <= 108) then variant := 33;
    if (mc_length <= 90) then variant := 32;
    if (mc_length <= 72) then variant := 31;
    if (mc_length <= 54) then variant := 30;
    if (mc_length <= 39) then variant := 29;
    if (mc_length <= 30) then variant := 28;
    if (mc_length <= 24) then variant := 27;
    if (mc_length <= 18) then variant := 26;
    if (mc_length <= 12) then variant := 25;
    if (mc_length <= 8) then variant := 24;
  end;

  if (variant = 0) then
  begin
    { Zint can choose automatically from all available variations }
    for i := 27 downto 0 do
    begin
      if (MicroAutosize[i] >= mc_length) then
        variant := MicroAutosize[i + 28];
    end;
  end;

  { Now we have the variant we can load the data }
  Dec(variant);
  symbol.option_2 := MicroVariants[variant]; { columns }
  symbol.rows := MicroVariants[variant + 34]; { rows }
  k := MicroVariants[variant + 68]; { number of EC CWs }
  longueur := (symbol.option_2 * symbol.rows) - k; { number of non-EC CWs }
  i := longueur - mc_length; { amount of padding required }
  offset := MicroVariants[variant + 102]; { coefficient offset }

  { We add the padding }
  while (i > 0) do
  begin
    chainemc[mc_length] := 900;
    Inc(mc_length);
    Dec(i);
  end;

  { Reed-Solomon error correction }
  longueur := mc_length;
  for loop := 0 to 49 do
    mccorrection[loop] := 0;

  //total := 0;
  for i := 0 to longueur - 1 do
  begin
    total := (chainemc[i] + mccorrection[k - 1]) mod 929;
    for j := k - 1 downto 0 do
    begin
      if (j = 0) then
        mccorrection[j] := (929 - (total * Microcoeffs[offset + j]) mod 929) mod 929
      else
        mccorrection[j] := (mccorrection[j - 1] + 929 - (total * Microcoeffs[offset + j]) mod 929) mod 929;
    end;
  end;

  for j := 0 to k - 1 do
    if (mccorrection[j] <> 0) then mccorrection[j] := 929 - mccorrection[j];

  { we add these codes to the string }
  for i := k - 1 downto 0 do
  begin
    chainemc[mc_length] := mccorrection[i];
    Inc(mc_length);
  end;

  { Now get the RAP (Row Address Pattern) start values }
  LeftRAPStart := RAPTable[variant];
  CentreRAPStart := RAPTable[variant + 34];
  RightRAPStart := RAPTable[variant + 68];
  StartCluster := RAPTable[variant + 102] div 3;

  { That's all values loaded, get on with the encoding }

  LeftRAP := LeftRAPStart;
  CentreRAP := CentreRAPStart;
  RightRAP := RightRAPStart;
  Cluster := StartCluster; { Cluster can be 0, 1 or 2 for Cluster(0), Cluster(3) and Cluster(6) }

  for i := 0 to symbol.rows - 1 do
  begin
    strcpy(codebarre, '');
    offset := 929 * Cluster;
    for j := 0 to 4 do
      dummy[j] := 0;

    for j := 0 to symbol.option_2 - 1 do
      dummy[j + 1] := chainemc[i * symbol.option_2 + j];

    { Copy the data into codebarre }
    concat(codebarre, RAPLR[LeftRAP]);
    concat(codebarre, '1');
    concat(codebarre, codagemc[offset + dummy[1]]);
    concat(codebarre, '1');
    if (symbol.option_2 = 3) then
      concat(codebarre, RAPC[CentreRAP]);

    if (symbol.option_2 >= 2) then
    begin
      concat(codebarre, '1');
      concat(codebarre, codagemc[offset + dummy[2]]);
      concat(codebarre, '1');
    end;
    if (symbol.option_2 = 4) then
      concat(codebarre, RAPC[CentreRAP]);

    if (symbol.option_2 >= 3) then
    begin
      concat(codebarre, '1');
      concat(codebarre, codagemc[offset + dummy[3]]);
      concat(codebarre, '1');
    end;
    if (symbol.option_2 = 4) then
    begin
      concat(codebarre, '1');
      concat(codebarre, codagemc[offset + dummy[4]]);
      concat(codebarre, '1');
    end;
    concat(codebarre, RAPLR[RightRAP]);
    concat(codebarre, '1'); { stop }

    { Now codebarre is a mixture of letters and numbers }

    writer := 0;
    flip := 1;
    strcpy(pattern, '');
    for loop := 0 to strlen(codebarre) - 1 do
    begin
      if ((codebarre[loop] >= '0') and (codebarre[loop] <= '9')) then
      begin
        for k := 0 to ctoi(codebarre[loop]) - 1 do
        begin
          if (flip = 0) then
            pattern[writer] := '0'
          else
            pattern[writer] :=  '1';
          Inc(writer);
        end;
        pattern[writer] := #0;
        if (flip = 0) then
          flip := 1
        else
          flip := 0;
      end
      else
      begin
        lookup(BRSET, PDFttf, codebarre[loop], pattern);
        Inc(writer, 5);
      end;
    end;
    symbol.width := writer;

    { so now pattern[] holds the string of '1's and '0's. - copy this to the symbol }
    for loop := 0 to strlen(pattern) - 1 do
      if (pattern[loop] = '1') then set_module(symbol, i, loop);

    symbol.row_height[i] := 2;

    { Set up RAPs and Cluster for next row }
    Inc(LeftRAP);
    Inc(CentreRAP);
    Inc(RightRAP);
    Inc(Cluster);

    if (LeftRAP = 53) then
      LeftRAP := 1;

    if (CentreRAP = 53) then
      CentreRAP := 1;

    if (RightRAP = 53) then
      RightRAP := 1;

    if (Cluster = 3) then
      Cluster := 0;
  end;

  result := codeerr; exit;
end;

end.

