V33 :0x4 turbulence_tran
64 /users/geirund/code/cosmo_nwp_progCCNINP/src/turbulence_tran.f90 S624 0
07/17/2019  16:51:16
use turbulence_utilities private
use meteo_utilities private
use data_turbulence private
use data_runcontrol private
use data_parameters private
use mo_kind private
use data_flake private
use data_constants private
enduse
D 56 24 840 168 839 7
D 71 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 74 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 77 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 80 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 83 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 86 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 89 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 92 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 95 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 98 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 101 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 104 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 107 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 110 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 113 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 116 21 9 1 3 48 0 0 0 0 0
 0 48 3 3 48 48
D 486 21 9 3 884 892 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
 0 889 890 3 891 891
D 489 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 492 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 495 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 498 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 501 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 504 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 507 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 510 21 9 3 884 896 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
 0 894 890 3 895 895
D 513 21 9 3 884 896 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
 0 894 890 3 895 895
D 516 21 9 3 884 896 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
 0 894 890 3 895 895
D 519 21 9 3 884 896 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
 0 894 890 3 895 895
D 522 21 9 3 884 896 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
 0 894 890 3 895 895
D 525 21 9 3 884 896 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
 0 894 890 3 895 895
D 528 21 9 4 897 900 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
 0 889 890 3 891 891
 0 898 892 3 899 899
D 531 21 9 3 884 892 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
 0 889 890 3 891 891
D 534 21 9 3 884 892 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
 0 889 890 3 891 891
D 537 21 9 3 884 892 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
 0 889 890 3 891 891
D 540 21 9 3 884 892 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
 0 889 890 3 891 891
D 543 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 546 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 549 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 552 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 555 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 558 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 561 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 564 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 567 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 570 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 573 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 576 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 579 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
D 582 21 9 2 893 890 0 0 1 0 0
 0 885 3 3 886 886
 0 887 886 3 888 888
S 624 24 0 0 0 8 1 0 5015 10005 0 A 0 0 0 0 B 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 turbulence_tran
S 626 23 0 0 0 8 730 624 5046 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t0_melt
S 627 23 0 0 0 8 748 624 5054 0 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 grav
S 629 23 0 0 0 8 746 624 5061 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 con_m
S 630 23 0 0 0 8 747 624 5067 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 con_h
S 631 23 0 0 0 8 761 624 5073 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b1
S 632 23 0 0 0 8 762 624 5076 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b2w
S 633 23 0 0 0 8 764 624 5080 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b3
S 634 23 0 0 0 8 765 624 5083 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b4w
S 635 23 0 0 0 8 736 624 5087 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 cp_d
S 636 23 0 0 0 8 735 624 5092 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rvd_m_o
S 637 23 0 0 0 8 731 624 5100 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 r_d
S 638 23 0 0 0 6 743 624 5104 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lhocp
S 639 23 0 0 0 8 733 624 5110 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rdv
S 640 23 0 0 0 6 740 624 5114 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lh_v
S 641 23 0 0 0 8 767 624 5119 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b234w
S 642 23 0 0 0 8 734 624 5125 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 o_m_rdv
S 643 23 0 0 0 8 738 624 5133 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rdocp
S 644 23 0 0 0 8 768 624 5139 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 uc1
S 645 23 0 0 0 8 769 624 5143 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 uc2
S 646 23 0 0 0 8 770 624 5147 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ucl
S 647 23 0 0 0 8 763 624 5151 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b2i
S 648 23 0 0 0 8 766 624 5155 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b4i
S 650 23 0 0 0 8 869 624 5170 4 0 A 0 0 0 0 B 0 149 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 h_ice_min_flk
S 652 23 0 0 0 8 706 624 5200 4 0 A 0 0 0 0 B 0 152 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 wp
S 653 23 0 0 0 6 717 624 5203 4 0 A 0 0 0 0 B 0 152 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iintegers
S 655 23 0 0 0 6 1009 624 5229 4 0 A 0 0 0 0 B 0 156 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lseaice
S 656 23 0 0 0 6 1010 624 5237 4 0 A 0 0 0 0 B 0 156 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 llake
S 657 23 0 0 0 6 968 624 5243 4 0 A 0 0 0 0 B 0 156 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 icldm_tran
S 658 23 0 0 0 6 1019 624 5254 4 0 A 0 0 0 0 B 0 156 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lprfcor
S 659 23 0 0 0 6 961 624 5262 4 0 A 0 0 0 0 B 0 156 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 itype_synd
S 660 23 0 0 0 6 962 624 5273 4 0 A 0 0 0 0 B 0 156 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 imode_tran
S 661 23 0 0 0 6 958 624 5284 4 0 A 0 0 0 0 B 0 156 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 itype_wcld
S 662 23 0 0 0 6 1138 624 5295 4 0 A 0 0 0 0 B 0 156 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 itype_diag_t2m
S 664 23 0 0 0 8 1270 624 5326 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 zt_ice
S 665 23 0 0 0 8 1271 624 5333 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z0_ice
S 666 23 0 0 0 6 1276 624 5340 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 len_min
S 667 23 0 0 0 8 1297 624 5348 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tet_g
S 668 23 0 0 0 8 1299 624 5354 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rim
S 669 23 0 0 0 8 1278 624 5358 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 akt
S 670 23 0 0 0 6 1312 624 5362 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 l_scal
S 671 23 0 0 0 8 1264 624 5369 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 alpha0
S 672 23 0 0 0 8 1262 624 5376 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rat_sea
S 673 23 0 0 0 8 1258 624 5384 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rlam_mom
S 674 23 0 0 0 8 1259 624 5393 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rlam_heat
S 675 23 0 0 0 8 1261 624 5403 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rat_can
S 676 23 0 0 0 8 1260 624 5411 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rat_lam
S 677 23 0 0 0 8 1263 624 5419 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z0m_dia
S 678 23 0 0 0 8 1294 624 5427 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 epsi
S 679 23 0 0 0 6 1314 624 5432 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 it_end
S 680 23 0 0 0 8 1277 624 5439 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 vel_min
S 681 23 0 0 0 8 1291 624 5447 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tkesmot
S 682 23 0 0 0 8 1307 624 5455 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 a_3
S 683 23 0 0 0 8 1308 624 5459 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 a_5
S 684 23 0 0 0 8 1309 624 5463 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 a_6
S 685 23 0 0 0 8 1310 624 5467 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b_1
S 686 23 0 0 0 8 1311 624 5471 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b_2
S 687 23 0 0 0 8 1304 624 5475 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 d_4
S 688 23 0 0 0 8 1298 624 5479 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 c_g
S 689 23 0 0 0 8 1282 624 5483 0 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 d_m
S 691 23 0 0 0 8 1286 624 5493 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 clc_diag
S 692 23 0 0 0 8 1287 624 5502 4 0 A 0 0 0 0 B 0 160 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 q_crit
S 694 23 0 0 0 8 1447 624 5525 4 0 A 0 0 0 0 B 0 168 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 cloud_diag
S 696 23 0 0 0 8 1706 624 5557 4 0 A 0 0 0 0 B 0 169 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 stab_funct
S 697 23 0 0 0 8 1824 624 5568 4 0 A 0 0 0 0 B 0 169 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 turb_param
S 698 23 0 0 0 8 1801 624 5579 4 0 A 0 0 0 0 B 0 169 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 turb_cloud
S 699 23 0 0 0 8 1610 624 5590 4 0 A 0 0 0 0 B 0 169 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 diag_level
S 703 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 706 16 3 mo_kind wp
R 717 16 4 data_parameters iintegers
R 730 6 4 data_constants t0_melt
R 731 6 5 data_constants r_d
R 733 6 7 data_constants rdv
R 734 6 8 data_constants o_m_rdv
R 735 6 9 data_constants rvd_m_o
R 736 6 10 data_constants cp_d
R 738 6 12 data_constants rdocp
R 740 6 14 data_constants lh_v
R 743 6 17 data_constants lhocp
R 746 6 20 data_constants con_m
R 747 6 21 data_constants con_h
R 748 6 22 data_constants g
R 761 6 35 data_constants b1
R 762 6 36 data_constants b2w
R 763 6 37 data_constants b2i
R 764 6 38 data_constants b3
R 765 6 39 data_constants b4w
R 766 6 40 data_constants b4i
R 767 6 41 data_constants b234w
R 768 6 42 data_constants uc1
R 769 6 43 data_constants uc2
R 770 6 44 data_constants ucl
S 780 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1069128089 -1717986918 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 783 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1076101120 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 784 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 10 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 790 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1074266112 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 816 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1070176665 -1717986918 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 820 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1107468383 536870912 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 821 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1072483532 -858993459 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 822 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 823 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1076959641 -1717986918 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 824 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1075891404 -858993459 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 825 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1077477376 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 826 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1076756480 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 827 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1097011920 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
R 839 25 12 data_flake opticpar_medium
R 840 5 13 data_flake nband_optic opticpar_medium
R 841 5 14 data_flake frac_optic opticpar_medium
R 842 5 15 data_flake extincoef_optic opticpar_medium
R 869 16 42 data_flake h_ice_min_flk
R 894 6 67 data_flake i
R 896 6 69 data_flake opticpar_water_ref$ac
R 898 6 71 data_flake opticpar_water_trans$ac
R 900 6 73 data_flake opticpar_whiteice_ref$ac
R 902 6 75 data_flake opticpar_blueice_ref$ac
R 904 6 77 data_flake opticpar_drysnow_ref$ac
R 906 6 79 data_flake opticpar_meltingsnow_ref$ac
R 908 6 81 data_flake opticpar_ice_opaque$ac
R 910 6 83 data_flake opticpar_snow_opaque$ac
R 958 6 34 data_runcontrol itype_wcld
R 961 6 37 data_runcontrol itype_synd
R 962 6 38 data_runcontrol imode_tran
R 968 6 44 data_runcontrol icldm_tran
R 1009 6 85 data_runcontrol lseaice
R 1010 6 86 data_runcontrol llake
R 1019 6 95 data_runcontrol lprfcor
R 1138 6 214 data_runcontrol itype_diag_t2m
S 1253 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1074790400 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
R 1258 6 4 data_turbulence rlam_mom
R 1259 6 5 data_turbulence rlam_heat
R 1260 6 6 data_turbulence rat_lam
R 1261 6 7 data_turbulence rat_can
R 1262 6 8 data_turbulence rat_sea
R 1263 6 9 data_turbulence z0m_dia
R 1264 6 10 data_turbulence alpha0
R 1270 6 16 data_turbulence zt_ice
R 1271 6 17 data_turbulence z0_ice
R 1276 6 22 data_turbulence len_min
R 1277 6 23 data_turbulence vel_min
R 1278 6 24 data_turbulence akt
R 1282 6 28 data_turbulence d_mom
R 1286 6 32 data_turbulence clc_diag
R 1287 6 33 data_turbulence q_crit
R 1291 6 37 data_turbulence tkesmot
R 1294 6 40 data_turbulence epsi
R 1297 6 43 data_turbulence tet_g
R 1298 6 44 data_turbulence c_g
R 1299 6 45 data_turbulence rim
R 1304 6 50 data_turbulence d_4
R 1307 6 53 data_turbulence a_3
R 1308 6 54 data_turbulence a_5
R 1309 6 55 data_turbulence a_6
R 1310 6 56 data_turbulence b_1
R 1311 6 57 data_turbulence b_2
R 1312 6 58 data_turbulence l_scal
R 1314 6 60 data_turbulence it_end
R 1447 14 130 meteo_utilities cloud_diag
R 1610 14 46 turbulence_utilities diag_level
R 1706 14 142 turbulence_utilities stab_funct
R 1801 14 237 turbulence_utilities turb_cloud
R 1824 14 260 turbulence_utilities turb_param
S 1836 16 0 0 0 9 1 624 12108 4 400000 A 0 0 0 0 B 0 182 0 0 0 0 0 0 617 137 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z0
S 1837 16 1 0 0 9 1 624 12118 4 400000 A 0 0 0 0 B 0 182 0 0 0 0 0 0 618 52 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z1
S 1838 16 1 0 0 9 1 624 12128 4 400000 A 0 0 0 0 B 0 182 0 0 0 0 0 0 619 77 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z2
S 1839 16 1 0 0 9 1 624 12138 4 400000 A 0 0 0 0 B 0 182 0 0 0 0 0 0 790 87 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z3
S 1840 16 0 0 0 9 1 624 12148 4 400000 A 0 0 0 0 B 0 182 0 0 0 0 0 0 1253 619 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z4
S 1841 16 0 0 0 9 1 624 12158 4 400000 A 0 0 0 0 B 0 182 0 0 0 0 0 0 1842 866 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z5
S 1842 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1075052544 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 1843 16 0 0 0 9 1 624 12168 4 400000 A 0 0 0 0 B 0 182 0 0 0 0 0 0 1844 868 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z6
S 1844 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1075314688 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 1845 16 0 0 0 9 1 624 12178 4 400000 A 0 0 0 0 B 0 182 0 0 0 0 0 0 1846 870 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z7
S 1846 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1075576832 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 1847 16 0 0 0 9 1 624 12188 4 400000 A 0 0 0 0 B 0 182 0 0 0 0 0 0 1848 872 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z8
S 1848 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1075838976 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 1849 16 0 0 0 9 1 624 12198 4 400000 A 0 0 0 0 B 0 182 0 0 0 0 0 0 1850 874 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z9
S 1850 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1075970048 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 1851 16 0 0 0 9 1 624 12208 4 400000 A 0 0 0 0 B 0 182 0 0 0 0 0 0 783 46 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z10
S 1852 6 4 0 0 9 1853 624 12220 80000c 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 1859 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z1d2
S 1853 6 4 0 0 9 1855 624 12225 80000c 0 A 0 0 0 0 B 0 195 0 0 0 8 0 0 0 0 0 0 1859 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z1d3
S 1855 6 4 0 0 9 1856 624 12230 80000c 0 A 0 0 0 0 B 0 195 0 0 0 16 0 0 0 0 0 0 1859 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z2d3
S 1856 6 4 0 0 9 1 624 12235 80000c 0 A 0 0 0 0 B 0 195 0 0 0 24 0 0 0 0 0 0 1859 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z3d2
S 1857 6 4 0 0 6 1858 624 12240 80000c 0 A 0 0 0 0 B 0 201 0 0 0 0 0 0 0 0 0 0 1860 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 istat
S 1858 6 4 0 0 16 1 624 12246 80000c 0 A 0 0 0 0 B 0 204 0 0 0 4 0 0 0 0 0 0 1860 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lerror
S 1859 11 0 0 0 8 1317 624 12253 40800000 805000 A 0 0 0 0 B 0 208 0 0 0 32 0 0 1852 1856 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 _turbulence_tran$10
S 1860 11 0 0 0 8 1859 624 12273 40800000 805000 A 0 0 0 0 B 0 208 0 0 0 8 0 0 1857 1858 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 _turbulence_tran$8
S 1861 23 5 0 0 0 1914 624 12292 0 0 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 turbtran
S 1862 1 3 1 0 9 1 1861 12301 4 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 dt_tke
S 1863 1 3 1 0 16 1 1861 12308 4 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lstfnct
S 1864 1 3 1 0 6 1 1861 12316 4 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iini
S 1865 1 3 1 0 6 1 1861 12321 4 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nprv
S 1866 1 3 1 0 6 1 1861 12326 4 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ntur
S 1867 6 3 1 0 6 1 1861 12331 800004 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ntim
S 1868 1 3 3 0 6 1 1861 12336 4 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 nvor
S 1869 6 3 1 0 6 1 1861 10364 800004 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ie
S 1870 6 3 1 0 6 1 1861 10367 800004 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 je
S 1871 6 3 1 0 6 1 1861 10370 800004 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ke
S 1872 6 3 1 0 6 1 1861 10667 800004 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ke1
S 1873 1 3 1 0 6 1 1861 11451 4 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 kcm
S 1874 1 3 1 0 6 1 1861 12341 4 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 vst
S 1875 1 3 1 0 6 1 1861 11455 4 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 istartpar
S 1876 6 3 1 0 6 1 1861 11465 800004 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 iendpar
S 1877 1 3 1 0 6 1 1861 11473 4 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 jstartpar
S 1878 6 3 1 0 6 1 1861 11483 800004 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 jendpar
S 1879 7 3 1 0 486 1 1861 11180 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 hhl
S 1880 7 3 1 0 489 1 1861 11491 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 fr_land
S 1881 7 3 1 0 492 1 1861 12345 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 depth_lk
S 1882 7 3 1 0 495 1 1861 11630 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 sai
S 1883 7 3 3 0 510 1 1861 12354 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 u
S 1884 7 3 3 0 513 1 1861 12356 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 v
S 1885 7 3 3 0 516 1 1861 10342 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 t
S 1886 7 3 3 0 519 1 1861 10347 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 qv
S 1887 7 3 3 0 522 1 1861 10350 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 qc
S 1888 7 3 3 0 525 1 1861 11970 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 prs
S 1889 7 3 1 0 501 1 1861 10546 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ps
S 1890 7 3 1 0 504 1 1861 12358 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 qv_s
S 1891 7 3 1 0 507 1 1861 12363 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 t_g
S 1892 7 3 1 0 498 1 1861 12367 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 h_ice
S 1893 7 3 3 0 543 1 1861 12373 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 gz0
S 1894 7 3 3 0 546 1 1861 12377 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tcm
S 1895 7 3 3 0 549 1 1861 12381 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tch
S 1896 7 3 3 0 552 1 1861 12385 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tfm
S 1897 7 3 3 0 555 1 1861 12389 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tfh
S 1898 7 3 3 0 558 1 1861 12393 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tfv
S 1899 7 3 3 0 528 1 1861 12397 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tke
S 1900 7 3 3 0 531 1 1861 12401 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tkvm
S 1901 7 3 3 0 534 1 1861 12406 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tkvh
S 1902 7 3 3 0 537 1 1861 10677 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rcld
S 1903 7 3 2 0 561 1 1861 12411 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 t_2m
S 1904 7 3 2 0 564 1 1861 12416 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 qv_2m
S 1905 7 3 2 0 567 1 1861 12422 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 td_2m
S 1906 7 3 2 0 570 1 1861 12428 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rh_2m
S 1907 7 3 2 0 573 1 1861 12434 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 u_10m
S 1908 7 3 2 0 576 1 1861 12440 800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 v_10m
S 1909 7 3 3 0 540 1 1861 12446 80800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 edr
S 1910 7 3 2 0 579 1 1861 12450 80800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 shfl_s
S 1911 7 3 2 0 582 1 1861 12457 80800204 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 lhfl_s
S 1912 1 3 2 0 28 1 1861 12464 4 43000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 yerrormsg
S 1913 1 3 2 0 6 1 1861 12474 4 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ierrstat
S 1914 14 5 0 0 0 1 1861 12292 200 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 277 52 0 0 0 0 0 0 0 0 0 0 0 0 215 0 624 0 0 0 0 turbtran
F 1914 52 1862 1863 1864 1865 1866 1867 1868 1869 1870 1871 1872 1873 1874 1875 1876 1877 1878 1879 1880 1881 1882 1883 1884 1885 1886 1887 1888 1889 1890 1891 1892 1893 1894 1895 1896 1897 1898 1899 1900 1901 1902 1903 1904 1905 1906 1907 1908 1909 1910 1911 1912 1913
S 1915 6 1 0 0 6 1 1861 12483 40800006 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_884
S 1916 6 1 0 0 6 1 1861 12491 40800006 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_885
S 1917 6 1 0 0 6 1 1861 12499 40800006 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_889
S 1918 6 1 0 0 6 1 1861 12507 40800006 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_886
S 1919 6 1 0 0 6 1 1861 12515 40800006 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_892
S 1920 6 1 0 0 6 1 1861 12523 40800006 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_895
S 1921 6 1 0 0 6 1 1861 12531 40800006 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_894
S 1922 6 1 0 0 6 1 1861 12539 40800006 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_898
S 1923 6 1 0 0 6 1 1861 12547 40800006 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_900
S 1924 6 1 0 0 6 1 1861 12555 40800006 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_902
S 1925 6 1 0 0 6 1 1861 12563 40800006 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_904
S 1926 6 1 0 0 6 1 1861 12571 40800006 3000 A 0 0 0 0 B 0 1448 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_906
A 20 2 0 0 0 6 703 0 0 0 20 0 0 0 0 0 0 0 0 0 0 0
A 38 2 0 0 0 9 780 0 0 0 38 0 0 0 0 0 0 0 0 0 0 0
A 46 2 0 0 0 9 783 0 0 0 46 0 0 0 0 0 0 0 0 0 0 0
A 48 2 0 0 0 6 784 0 0 0 48 0 0 0 0 0 0 0 0 0 0 0
A 52 2 0 0 0 9 618 0 0 0 52 0 0 0 0 0 0 0 0 0 0 0
A 77 2 0 0 0 9 619 0 0 0 77 0 0 0 0 0 0 0 0 0 0 0
A 87 2 0 0 0 9 790 0 0 0 87 0 0 0 0 0 0 0 0 0 0 0
A 131 2 0 0 0 9 816 0 0 0 131 0 0 0 0 0 0 0 0 0 0 0
A 137 2 0 0 0 9 617 0 0 0 137 0 0 0 0 0 0 0 0 0 0 0
A 148 2 0 0 0 9 820 0 0 0 148 0 0 0 0 0 0 0 0 0 0 0
A 164 2 0 0 0 9 821 0 0 0 164 0 0 0 0 0 0 0 0 0 0 0
A 200 2 0 0 0 9 823 0 0 0 200 0 0 0 0 0 0 0 0 0 0 0
A 226 2 0 0 0 9 824 0 0 0 226 0 0 0 0 0 0 0 0 0 0 0
A 252 2 0 0 0 9 825 0 0 0 252 0 0 0 0 0 0 0 0 0 0 0
A 278 2 0 0 0 9 826 0 0 0 278 0 0 0 0 0 0 0 0 0 0 0
A 304 2 0 0 0 9 827 0 0 0 304 0 0 0 0 0 0 0 0 0 0 0
A 369 1 0 0 0 56 896 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 370 2 0 0 0 6 822 0 0 0 370 0 0 0 0 0 0 0 0 0 0 0
A 395 1 0 0 0 56 898 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 420 1 0 0 0 56 900 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 445 1 0 0 0 56 902 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 470 1 0 0 0 56 904 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 495 1 0 0 0 56 906 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 520 1 0 0 0 56 908 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 545 1 0 0 0 56 910 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 619 2 0 0 80 9 1253 0 0 0 619 0 0 0 0 0 0 0 0 0 0 0
A 866 2 0 0 671 9 1842 0 0 0 866 0 0 0 0 0 0 0 0 0 0 0
A 868 2 0 0 664 9 1844 0 0 0 868 0 0 0 0 0 0 0 0 0 0 0
A 870 2 0 0 675 9 1846 0 0 0 870 0 0 0 0 0 0 0 0 0 0 0
A 872 2 0 0 680 9 1848 0 0 0 872 0 0 0 0 0 0 0 0 0 0 0
A 874 2 0 0 682 9 1850 0 0 0 874 0 0 0 0 0 0 0 0 0 0 0
A 884 1 0 0 688 6 1920 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 885 1 0 0 624 6 1869 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 886 1 0 0 89 6 1915 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 887 1 0 0 578 6 1870 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 888 1 0 0 678 6 1916 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 889 1 0 0 650 6 1872 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 890 1 0 0 679 6 1917 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 891 1 0 0 683 6 1918 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 892 1 0 0 684 6 1919 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 893 1 0 0 689 6 1921 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 894 1 0 0 83 6 1871 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 895 1 0 0 665 6 1922 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 896 1 0 0 667 6 1923 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 897 1 0 0 576 6 1926 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 898 1 0 0 613 6 1867 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 899 1 0 0 669 6 1924 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 900 1 0 0 673 6 1925 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
Z
J 282 1 1
V 369 56 7 0
S 0 56 0 0 0
A 0 6 0 0 1 3 1
R 0 71 0 1
A 0 9 0 0 1 52 1
O 894 20 48 3 0
A 0 9 0 0 1 137 0
R 0 74 0 0
A 0 9 0 0 1 87 1
O 894 20 48 3 0
A 0 9 0 0 1 148 0
J 282 1 1
V 395 56 7 0
S 0 56 0 0 0
A 0 6 0 0 1 20 1
R 0 77 0 1
A 0 9 0 0 1 38 1
A 0 9 0 0 1 164 1
O 894 370 48 3 0
A 0 9 0 0 1 137 0
R 0 80 0 0
A 0 9 0 0 1 77 1
A 0 9 0 0 1 131 1
O 894 370 48 3 0
A 0 9 0 0 1 148 0
J 282 1 1
V 420 56 7 0
S 0 56 0 0 0
A 0 6 0 0 1 3 1
R 0 83 0 1
A 0 9 0 0 1 52 1
O 894 20 48 3 0
A 0 9 0 0 1 137 0
R 0 86 0 0
A 0 9 0 0 1 200 1
O 894 20 48 3 0
A 0 9 0 0 1 148 0
J 282 1 1
V 445 56 7 0
S 0 56 0 0 0
A 0 6 0 0 1 3 1
R 0 89 0 1
A 0 9 0 0 1 52 1
O 894 20 48 3 0
A 0 9 0 0 1 137 0
R 0 92 0 0
A 0 9 0 0 1 226 1
O 894 20 48 3 0
A 0 9 0 0 1 148 0
J 282 1 1
V 470 56 7 0
S 0 56 0 0 0
A 0 6 0 0 1 3 1
R 0 95 0 1
A 0 9 0 0 1 52 1
O 894 20 48 3 0
A 0 9 0 0 1 137 0
R 0 98 0 0
A 0 9 0 0 1 252 1
O 894 20 48 3 0
A 0 9 0 0 1 148 0
J 282 1 1
V 495 56 7 0
S 0 56 0 0 0
A 0 6 0 0 1 3 1
R 0 101 0 1
A 0 9 0 0 1 52 1
O 894 20 48 3 0
A 0 9 0 0 1 137 0
R 0 104 0 0
A 0 9 0 0 1 278 1
O 894 20 48 3 0
A 0 9 0 0 1 148 0
J 282 1 1
V 520 56 7 0
S 0 56 0 0 0
A 0 6 0 0 1 3 1
R 0 107 0 1
A 0 9 0 0 1 52 1
O 894 20 48 3 0
A 0 9 0 0 1 137 0
R 0 110 0 0
A 0 9 0 0 1 304 1
O 894 20 48 3 0
A 0 9 0 0 1 148 0
J 282 1 1
V 545 56 7 0
S 0 56 0 0 0
A 0 6 0 0 1 3 1
R 0 113 0 1
A 0 9 0 0 1 52 1
O 894 20 48 3 0
A 0 9 0 0 1 137 0
R 0 116 0 0
A 0 9 0 0 1 304 1
O 894 20 48 3 0
A 0 9 0 0 1 148 0
Z
