V33 :0x4 data_turbulence
64 /users/geirund/code/cosmo_nwp_progCCNINP/src/data_turbulence.f90 S624 0
07/17/2019  16:51:08
use data_parameters private
use mo_kind private
enduse
S 624 24 0 0 0 8 1 0 5015 10005 0 A 0 0 0 0 B 0 4 0 0 0 0 0 0 0 0 0 0 183 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 data_turbulence
S 626 23 0 0 0 8 635 624 5047 4 0 A 0 0 0 0 B 0 52 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 wp
S 627 23 0 0 0 6 646 624 5050 4 0 A 0 0 0 0 B 0 52 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iintegers
S 628 23 0 0 0 8 653 624 5060 4 0 A 0 0 0 0 B 0 52 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 repsilon
R 635 16 3 mo_kind wp
S 641 3 0 0 0 9 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 28825476 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
R 646 16 4 data_parameters iintegers
R 653 16 11 data_parameters repsilon
S 656 6 4 0 0 9 657 624 5179 80000c 0 A 0 0 0 0 B 0 78 0 0 0 0 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rlam_mom
S 657 6 4 0 0 9 658 624 5188 80000c 0 A 0 0 0 0 B 0 78 0 0 0 8 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rlam_heat
S 658 6 4 0 0 9 659 624 5198 80000c 0 A 0 0 0 0 B 0 78 0 0 0 16 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rat_lam
S 659 6 4 0 0 9 660 624 5206 80000c 0 A 0 0 0 0 B 0 78 0 0 0 24 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rat_can
S 660 6 4 0 0 9 662 624 5214 80000c 0 A 0 0 0 0 B 0 78 0 0 0 32 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rat_sea
S 661 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1077149696 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 662 6 4 0 0 9 664 624 5222 80000c 0 A 0 0 0 0 B 0 78 0 0 0 40 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z0m_dia
S 663 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1070176665 -1717986918 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 664 6 4 0 0 9 666 624 5230 80000c 0 A 0 0 0 0 B 0 78 0 0 0 48 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 alpha0
S 665 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1065955518 233646221 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 666 6 4 0 0 9 667 624 5237 80000c 0 A 0 0 0 0 B 0 78 0 0 0 56 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 alpha1
S 667 6 4 0 0 9 668 624 5244 80000c 0 A 0 0 0 0 B 0 96 0 0 0 64 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 c_lnd
S 668 6 4 0 0 9 670 624 5250 80000c 0 A 0 0 0 0 B 0 96 0 0 0 72 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 c_sea
S 669 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1073217536 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 670 6 4 0 0 9 671 624 5256 80000c 0 A 0 0 0 0 B 0 96 0 0 0 80 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 c_soil
S 671 6 4 0 0 9 672 624 5263 80000c 0 A 0 0 0 0 B 0 96 0 0 0 88 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 e_surf
S 672 6 4 0 0 9 675 624 5270 80000c 0 A 0 0 0 0 B 0 106 0 0 0 96 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 zt_ice
S 674 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 -1074056397 858993459 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 675 6 4 0 0 9 677 624 5277 80000c 0 A 0 0 0 0 B 0 106 0 0 0 104 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 z0_ice
S 676 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1062232653 -755914244 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 677 6 4 0 0 9 679 624 5284 80000c 0 A 0 0 0 0 B 0 114 0 0 0 112 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tur_len
S 678 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1082081280 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 679 6 4 0 0 9 680 624 5292 80000c 0 A 0 0 0 0 B 0 114 0 0 0 120 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 pat_len
S 680 6 4 0 0 9 682 624 5300 80000c 0 A 0 0 0 0 B 0 114 0 0 0 128 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 c_smag
S 681 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1070596096 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 682 6 4 0 0 9 684 624 5307 80000c 0 A 0 0 0 0 B 0 114 0 0 0 136 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 c_pr
S 683 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1070931640 1374389535 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 684 6 4 0 0 9 686 624 5312 80000c 0 A 0 0 0 0 B 0 114 0 0 0 144 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 len_min
S 685 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1051772663 -1598689907 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 686 6 4 0 0 9 688 624 5320 80000c 0 A 0 0 0 0 B 0 114 0 0 0 152 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 vel_min
S 687 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1065646817 1202590843 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 688 6 4 0 0 9 690 624 5328 80000c 0 A 0 0 0 0 B 0 114 0 0 0 160 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 akt
S 689 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1071225241 -1717986918 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 690 6 4 0 0 9 692 624 5332 80000c 0 A 0 0 0 0 B 0 114 0 0 0 168 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 a_heat
S 691 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1072147988 2061584302 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 692 6 4 0 0 9 694 624 5339 80000c 0 A 0 0 0 0 B 0 114 0 0 0 176 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 a_mom
S 693 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1072525475 -687194767 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 694 6 4 0 0 9 696 624 5345 80000c 0 A 0 0 0 0 B 0 114 0 0 0 184 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 d_heat
S 695 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1076114227 858993459 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 696 6 4 0 0 9 698 624 5352 80000c 0 A 0 0 0 0 B 0 114 0 0 0 192 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 d_mom
S 697 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1076926873 -1717986918 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 698 6 4 0 0 9 699 624 5358 80000c 0 A 0 0 0 0 B 0 114 0 0 0 200 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 c_diff
S 699 6 4 0 0 9 700 624 5365 80000c 0 A 0 0 0 0 B 0 114 0 0 0 208 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 a_hshr
S 700 6 4 0 0 9 701 624 5372 80000c 0 A 0 0 0 0 B 0 114 0 0 0 216 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 a_stab
S 701 6 4 0 0 9 702 624 5379 80000c 0 A 0 0 0 0 B 0 114 0 0 0 224 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 clc_diag
S 702 6 4 0 0 9 704 624 5388 80000c 0 A 0 0 0 0 B 0 114 0 0 0 232 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 q_crit
S 703 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1074790400 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 704 6 4 0 0 9 705 624 5395 80000c 0 A 0 0 0 0 B 0 114 0 0 0 240 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 c_scld
S 705 6 4 0 0 9 706 624 5402 80000c 0 A 0 0 0 0 B 0 114 0 0 0 248 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tkhmin
S 706 6 4 0 0 9 707 624 5409 80000c 0 A 0 0 0 0 B 0 114 0 0 0 256 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tkmmin
S 707 6 4 0 0 9 709 624 5416 80000c 0 A 0 0 0 0 B 0 159 0 0 0 264 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tkesmot
S 708 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1069757235 858993459 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 709 6 4 0 0 9 710 624 5424 80000c 0 A 0 0 0 0 B 0 159 0 0 0 272 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 wichfakt
S 710 6 4 0 0 9 711 624 5433 80000c 0 A 0 0 0 0 B 0 159 0 0 0 280 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 securi
S 711 6 4 0 0 9 712 624 5440 80000c 0 A 0 0 0 0 B 0 159 0 0 0 288 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 epsi
S 712 6 4 0 0 9 1 624 5445 80000c 0 A 0 0 0 0 B 0 159 0 0 0 296 0 0 0 0 0 0 732 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 eps_div
S 713 6 4 0 0 9 714 624 5453 4 8 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 c_tke
S 714 6 4 0 0 9 715 624 5459 4 8 A 0 0 0 0 B 0 171 0 0 0 8 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tet_g
S 715 6 4 0 0 9 716 624 5465 4 8 A 0 0 0 0 B 0 171 0 0 0 16 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 c_g
S 716 6 4 0 0 9 717 624 5469 4 8 A 0 0 0 0 B 0 171 0 0 0 24 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rim
S 717 6 4 0 0 9 718 624 5473 4 8 A 0 0 0 0 B 0 171 0 0 0 32 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 d_0
S 718 6 4 0 0 9 719 624 5477 4 8 A 0 0 0 0 B 0 171 0 0 0 40 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 d_1
S 719 6 4 0 0 9 720 624 5481 4 8 A 0 0 0 0 B 0 171 0 0 0 48 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 d_2
S 720 6 4 0 0 9 721 624 5485 4 8 A 0 0 0 0 B 0 171 0 0 0 56 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 d_3
S 721 6 4 0 0 9 722 624 5489 4 8 A 0 0 0 0 B 0 171 0 0 0 64 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 d_4
S 722 6 4 0 0 9 723 624 5493 4 8 A 0 0 0 0 B 0 171 0 0 0 72 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 d_5
S 723 6 4 0 0 9 724 624 5497 4 8 A 0 0 0 0 B 0 171 0 0 0 80 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 d_6
S 724 6 4 0 0 9 725 624 5501 4 8 A 0 0 0 0 B 0 171 0 0 0 88 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 a_3
S 725 6 4 0 0 9 726 624 5505 4 8 A 0 0 0 0 B 0 171 0 0 0 96 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 a_5
S 726 6 4 0 0 9 727 624 5509 4 8 A 0 0 0 0 B 0 171 0 0 0 104 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 a_6
S 727 6 4 0 0 9 728 624 5513 4 8 A 0 0 0 0 B 0 171 0 0 0 112 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b_1
S 728 6 4 0 0 9 729 624 5517 4 8 A 0 0 0 0 B 0 171 0 0 0 120 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b_2
S 729 6 4 0 0 9 730 624 5521 4 8 A 0 0 0 0 B 0 171 0 0 0 128 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 l_scal
S 730 6 4 0 0 9 1 624 5528 4 8 A 0 0 0 0 B 0 171 0 0 0 136 0 0 0 0 0 0 733 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 l_hori
S 731 6 4 0 0 6 1 624 5535 80000c 0 A 0 0 0 0 B 0 178 0 0 0 0 0 0 0 0 0 0 734 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 it_end
S 732 11 0 0 0 8 655 624 5542 40800008 805000 A 0 0 0 0 B 0 183 0 0 0 304 0 0 656 712 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 _data_turbulence$10
S 733 11 0 0 0 8 732 624 5562 40800000 805000 A 0 0 0 0 B 0 183 0 0 0 144 0 0 713 730 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 _data_turbulence$2
S 734 11 0 0 0 8 733 624 5581 40800008 805000 A 0 0 0 0 B 0 183 0 0 0 4 0 0 731 731 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 _data_turbulence$8
A 30 2 0 0 0 9 641 0 0 0 30 0 0 0 0 0 0 0 0 0 0 0
A 33 2 0 0 0 9 617 0 0 0 33 0 0 0 0 0 0 0 0 0 0 0
A 34 1 0 0 0 9 656 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 35 2 0 0 0 9 618 0 0 0 35 0 0 0 0 0 0 0 0 0 0 0
A 36 1 0 0 0 9 657 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 37 1 0 0 0 9 658 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 38 1 0 0 0 9 659 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 39 2 0 0 0 9 661 0 0 0 39 0 0 0 0 0 0 0 0 0 0 0
A 40 1 0 0 0 9 660 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 41 2 0 0 0 9 663 0 0 0 41 0 0 0 0 0 0 0 0 0 0 0
A 42 1 0 0 0 9 662 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 43 2 0 0 0 9 665 0 0 0 43 0 0 0 0 0 0 0 0 0 0 0
A 44 1 0 0 0 9 664 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 45 1 0 0 0 9 666 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 46 2 0 0 0 9 619 0 0 0 46 0 0 0 0 0 0 0 0 0 0 0
A 47 1 0 0 0 9 667 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 48 2 0 0 0 9 669 0 0 0 48 0 0 0 0 0 0 0 0 0 0 0
A 49 1 0 0 0 9 668 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 50 1 0 0 0 9 670 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 51 1 0 0 0 9 671 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 53 2 0 0 0 9 674 0 0 0 53 0 0 0 0 0 0 0 0 0 0 0
A 54 1 0 0 0 9 672 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 55 2 0 0 0 9 676 0 0 0 55 0 0 0 0 0 0 0 0 0 0 0
A 56 1 0 0 0 9 675 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 57 2 0 0 0 9 678 0 0 0 57 0 0 0 0 0 0 0 0 0 0 0
A 58 1 0 0 0 9 677 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 59 1 0 0 0 9 679 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 60 2 0 0 0 9 681 0 0 0 60 0 0 0 0 0 0 0 0 0 0 0
A 61 1 0 0 0 9 680 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 62 2 0 0 0 9 683 0 0 0 62 0 0 0 0 0 0 0 0 0 0 0
A 63 1 0 0 0 9 682 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 64 2 0 0 0 9 685 0 0 0 64 0 0 0 0 0 0 0 0 0 0 0
A 65 1 0 0 0 9 684 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 66 2 0 0 0 9 687 0 0 0 66 0 0 0 0 0 0 0 0 0 0 0
A 67 1 0 0 0 9 686 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 68 2 0 0 0 9 689 0 0 0 68 0 0 0 0 0 0 0 0 0 0 0
A 69 1 0 0 0 9 688 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 70 2 0 0 0 9 691 0 0 0 70 0 0 0 0 0 0 0 0 0 0 0
A 71 1 0 0 0 9 690 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 72 2 0 0 0 9 693 0 0 0 72 0 0 0 0 0 0 0 0 0 0 0
A 73 1 0 0 0 9 692 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 74 2 0 0 0 9 695 0 0 0 74 0 0 0 0 0 0 0 0 0 0 0
A 75 1 0 0 0 9 694 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 76 2 0 0 0 9 697 0 0 0 76 0 0 0 0 0 0 0 0 0 0 0
A 77 1 0 0 0 9 696 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 78 1 0 0 0 9 698 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 79 1 0 0 0 9 699 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 80 1 0 0 0 9 700 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 81 2 0 0 0 9 620 0 0 0 81 0 0 0 0 0 0 0 0 0 0 0
A 82 1 0 0 0 9 701 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 83 2 0 0 0 9 703 0 0 0 83 0 0 0 0 0 0 0 0 0 0 0
A 84 1 0 0 0 9 702 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 85 1 0 0 0 9 704 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 86 1 0 0 0 9 705 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 87 1 0 0 0 9 706 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 88 2 0 0 0 9 708 0 0 0 88 0 0 0 0 0 0 0 0 0 0 0
A 89 1 0 0 0 9 707 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 90 1 0 0 0 9 709 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 91 1 0 0 0 9 710 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 92 1 0 0 0 9 711 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 93 1 0 0 0 9 712 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 94 1 0 0 46 6 731 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
Z
Z
