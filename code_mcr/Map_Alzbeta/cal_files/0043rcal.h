int an_serial_number = 43;
long magnitude_calibration_table[361] = {
70445 70451 70456 70462 70468 70487 70507 70527 70547 70567
70626 70685 70744 70803 70829 70855 70881 70907 70933 70960
70966 70972 70978 70984 70990 70937 70884 70832 70779 70727
70696 70666 70636 70606 70576 70494 70412 70331 70249 70167
70086 69946 69806 69666 69526 69387 69275 69163 69051 68939
68857 68776 68694 68613 68532 68544 68556 68569 68581 68594
68707 68820 68933 69046 69159 69285 69411 69537 69664 69751
69838 69926 70013 70101 70180 70259 70338 70417 70497 70591
70686 70780 70875 70970 71022 71075 71127 71180 71233 71346
71459 71573 71686 71800 71802 71804 71806 71808 71810 71812
71799 71787 71775 71763 72072 72382 72692 73002 73312 73729
74146 74563 74980 75397 75815 75802 75789 75776 75763 75976
76189 76402 76615 76202 75789 75376 74963 74550 74137 73884
73631 73378 73125 72872 72619 72645 72672 72699 72726 72753
72734 72716 72697 72679 72661 72536 72412 72287 72163 72039
72097 72155 72213 72271 72330 72405 72480 72556 72631 72706
72782 72701 72620 72539 72458 72378 72523 72668 72813 72958
73525 74092 74659 75226 75793 76360 76095 75831 75566 75302
75164 75026 74888 74750 74612 74165 73719 73272 72826 72404
71982 71561 71139 70718 70637 70557 70477 70397 70317 70323
70329 70336 70342 70349 70286 70223 70160 70097 70034 70026
70019 70011 70004 69997 70036 70075 70114 70153 70192 70231
70300 70370 70440 70510 70624 70739 70854 70969 71084 71612
72140 72668 73196 73724 73804 73884 73964 74045 74237 74429
74621 74813 74503 74194 73885 73575 73266 72957 72559 72161
71763 71365 70968 71002 71036 71070 71104 71139 71112 71085
71058 71031 71005 70994 70984 70974 70964 70954 70965 70977
70988 71000 71012 71011 71010 71009 71008 71008 71036 71065
71093 71122 71151 71127 71104 71080 71057 71034 70997 70960
70923 70886 70849 70802 70756 70710 70664 70618 70719 70820
70922 71023 71125 71247 71369 71491 71613 71736 71853 71970
72087 72204 72322 72392 72462 72532 72602 72672 72703 72735
72767 72799 72831 72833 72836 72838 72841 72844 72850 72856
72863 72869 72875 72882 72882 72882 72882 72882 72882 72852
72823 72793 72764 72735 72631 72528 72425 72322 72219 72039
71859 71679 71499 71320 71173 71026 70880 70733 70586 70440
70445 ;

long  irect on_ca ibrat on_ta le[36 ] = {
 1715  1715  1715  1715  1715  1715  1715  1715  1715  1715
 1743  1772  1801  1830  1772  1715  1658  1600  1543  1486
 1371  1257  1143  1029   915   892   869   846   823   800
  777   754   731   708   686   666   647   628   609   590
  571   548   525   502   479   457   628   800   971  1143
 1188  1234  1280  1326  1372  1372  1372  1372  1372  1372
 1440  1509  1577  1646  1715  1943  2172  2401  2630  2653
 2676  2699  2722  2745  2767  2790  2813  2836  2859  2836
 2813  2790  2767  2745  2767  2790  2813  2836  2859  2721
 2584  2447  2310  2173  2058  1944  1829  1715  1600  1486
 1572  1658  1744  1830  1852  1875  1898  1921  1944  1905
 1867  1829  1791  1753  1715  1886  2058  2230  2402  2602
 2802  3002  3202  2973  2744  2516  2287  2058  1830  1753
 1677  1601  1524  1448  1372  1394  1417  1440  1463  1486
 1463  1440  1417  1394  1372  1303  1234  1166  1097  1029
 1006   983   960   937   915   781   648   514   381   247
  114   136   159   182   205   228   399   571   743   915
  819   724   629   533   438   343   543   743   943  1143
 1234  1326  1417  1509  1601  1686  1772  1858  1944  2012
 2081  2149  2218  2287  2355  2424  2492  2561  2630  2561
 2492  2424  2355  2287  2218  2149  2081  2012  1944  1966
 1989  2012  2035  2058  1981  1905  1829  1753  1677  1601
 1715  1829  1943  2058  2172  2286  2401  2515  2630  2584
 2538  2493  2447  2402  2602  2802  3002  3202  3430  3659
 3888  4117  4021  3926  3831  3735  3640  3545  3476  3407
 3339  3270  3202  3339  3476  3613  3750  3888  3796  3705
 3613  3522  3431  3339  3247  3156  3064  2973  3064  3156
 3247  3339  3431  3499  3568  3636  3705  3774  3751  3728
 3705  3682  3660  3682  3705  3728  3751  3774  3751  3728
 3705  3682  3660  3637  3614  3591  3568  3545  3590  3636
 3682  3728  3774  3819  3865  3911  3957  4003  3980  3957
 3934  3911  3888  3888  3888  3888  3888  3888  3819  3750
 3682  3613  3545  3476  3407  3339  3270  3202  3144  3087
 3030  2973  2916  2859  2813  2767  2721  2675  2630  2538
 2447  2355  2264  2173  2104  2035  1967  1898  1830  1852
 1875  1898  1921  1944  1905  1867  1829  1791  1753  1715
 1715 ;
