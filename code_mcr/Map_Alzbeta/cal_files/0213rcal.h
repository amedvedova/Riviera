int an_serial_number = 213;
long magnitude_calibration_table[361] = {
73224,73295,73367,73438,73510,73582,73164,72747,72329,71912,
71495,71046,70597,70148,69699,69251,69246,69242,69237,69233,
69229,69151,69074,68997,68920,68843,68798,68754,68710,68665,
68621,68577,68613,68650,68686,68723,68745,68768,68790,68813,
68836,68845,68854,68864,68873,68883,68808,68734,68659,68585,
68504,68423,68342,68261,68181,68153,68125,68098,68070,68043,
68139,68235,68331,68428,68523,68619,68715,68811,68907,68972,
69037,69102,69167,69232,69288,69344,69401,69457,69514,69556,
69598,69641,69683,69726,69706,69687,69668,69649,69630,69667,
69704,69741,69778,69815,69875,69936,69996,70057,70117,70178,
70211,70245,70279,70313,70347,70801,71256,71711,72166,72621,
73044,73467,73890,74313,74736,74672,74608,74544,74481,74461,
74442,74422,74403,74384,74091,73798,73505,73212,72919,72597,
72275,71953,71631,71309,70987,70992,70998,71003,71009,71015,
70932,70850,70768,70686,70604,70474,70344,70215,70085,69956,
70021,70086,70151,70216,70282,70298,70314,70330,70346,70363,
70362,70362,70361,70361,70361,70265,70169,70073,69977,69882,
69745,69608,69471,69334,69198,69143,69088,69033,68979,69030,
69081,69132,69183,69235,69336,69438,69540,69642,69744,69750,
69757,69764,69771,69778,69801,69824,69847,69870,69893,69909,
69925,69941,69957,69973,69873,69773,69674,69574,69475,69551,
69628,69704,69781,69858,69863,69868,69873,69878,69883,69888,
69860,69832,69805,69777,69750,70105,70460,70815,71170,71525,
71867,72210,72553,72896,73239,73148,73057,72966,72875,72785,
72860,72935,73010,73085,72633,72182,71731,71280,70829,70428,
70027,69627,69226,68825,68425,68433,68441,68450,68458,68467,
68422,68378,68333,68289,68245,68239,68233,68227,68221,68215,
68246,68278,68310,68342,68374,68461,68549,68636,68724,68812,
68902,68992,69082,69172,69263,69266,69269,69273,69276,69280,
69239,69198,69157,69116,69075,69069,69064,69058,69053,69048,
69124,69200,69277,69353,69430,69529,69629,69729,69829,69929,
69968,70007,70047,70086,70126,70132,70138,70144,70150,70157,
70124,70091,70058,70025,69993,69946,69899,69853,69806,69759,
69713,69731,69749,69767,69785,69804,69817,69830,69843,69856,
69870,69837,69805,69772,69740,69708,69963,70219,70475,70730,
70986,71242,71712,72183,72653,73124,73595,73502,73409,73316,
73224};

long direction_calibration_table[361] = {
 -457, -251,  -45,  159,  365,  571,  525,  479,  434,  388,
  343,  251,  160,   68,  -22, -114,  -45,   22,   91,  159,
  228,  136,   45,  -45, -136, -228, -304, -380, -457, -533,
 -609, -686, -457, -229,    0,  228,  228,  228,  228,  228,
  228,  251,  274,  297,  320,  343,  428,  514,  600,  686,
  777,  868,  960, 1051, 1143, 1257, 1371, 1486, 1600, 1715,
 1858, 2001, 2144, 2287, 2332, 2378, 2424, 2470, 2516, 2516,
 2516, 2516, 2516, 2516, 2493, 2470, 2447, 2424, 2402, 2356,
 2310, 2264, 2218, 2173, 2241, 2310, 2378, 2447, 2516, 2447,
 2378, 2310, 2241, 2173, 2096, 2020, 1944, 1867, 1791, 1715,
 1692, 1669, 1646, 1623, 1601, 1601, 1601, 1601, 1601, 1601,
 1463, 1326, 1189, 1052,  915, 1143, 1372, 1601, 1830, 1990,
 2150, 2310, 2470, 2630, 2515, 2401, 2286, 2172, 2058, 1962,
 1867, 1772, 1676, 1581, 1486, 1509, 1532, 1555, 1578, 1601,
 1463, 1326, 1189, 1052,  915,  892,  869,  846,  823,  800,
  868,  937, 1005, 1074, 1143, 1074, 1005,  937,  868,  800,
  800,  800,  800,  800,  800,  868,  937, 1005, 1074, 1143,
 1166, 1189, 1212, 1235, 1258, 1429, 1601, 1772, 1944, 2058,
 2172, 2287, 2401, 2516, 2516, 2516, 2516, 2516, 2516, 2470,
 2424, 2378, 2332, 2287, 2218, 2149, 2081, 2012, 1944, 1944,
 1944, 1944, 1944, 1944, 1989, 2035, 2081, 2127, 2173, 2150,
 2127, 2104, 2081, 2058, 1962, 1867, 1772, 1676, 1581, 1486,
 1486, 1486, 1486, 1486, 1486, 1486, 1486, 1486, 1486, 1486,
 1394, 1303, 1211, 1120, 1029, 1097, 1166, 1234, 1303, 1372,
 1658, 1944, 2230, 2516, 2378, 2241, 2104, 1967, 1830, 1810,
 1791, 1772, 1753, 1734, 1715, 1715, 1715, 1715, 1715, 1715,
 1577, 1440, 1303, 1166, 1029, 1029, 1029, 1029, 1029, 1029,
 1120, 1211, 1303, 1394, 1486, 1486, 1486, 1486, 1486, 1486,
 1463, 1440, 1417, 1394, 1372, 1303, 1234, 1166, 1097, 1029,
 1097, 1166, 1234, 1303, 1372, 1417, 1463, 1509, 1555, 1601,
 1601, 1601, 1601, 1601, 1601, 1623, 1646, 1669, 1692, 1715,
 1669, 1623, 1577, 1531, 1486, 1371, 1257, 1143, 1029,  915,
  892,  869,  846,  823,  800,  761,  723,  685,  647,  609,
  571,  548,  525,  502,  479,  457,  342,  228,  114,    0,
 -114,  -91,  -68,  -45,  -22,    0, -114, -228, -343, -457,
 -571, -686, -754, -823, -891, -960,-1029, -886, -743, -600,
 -457};