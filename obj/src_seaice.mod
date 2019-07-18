V33 :0x4 src_seaice
59 /users/geirund/code/cosmo_nwp_progCCNINP/src/src_seaice.f90 S624 0
07/17/2019  16:51:35
use data_tracer_metadata private
use iso_c_binding private
use src_tracer private
use environment private
use meteo_utilities private
use data_parallel private
use data_runcontrol private
use data_soil private
use data_turbulence private
use data_fields private
use data_constants private
use data_modelconfig private
use data_parameters private
use mo_kind private
enduse
D 3512 24 5765 8 5764 7
D 3521 24 5768 8 5767 7
D 3921 18 8806
D 3923 21 3921 1 2 44 0 0 0 0 0
 2 8832 3 2 8832 44
D 3926 21 3921 1 3 44 0 0 0 0 0
 0 44 3 3 44 44
D 3941 24 6838 760 6837 7
D 4513 18 8806
S 624 24 0 0 0 8 1 0 5015 10005 0 A 0 0 0 0 B 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 src_seaice
S 626 23 0 0 0 8 701 624 5042 4 0 A 0 0 0 0 B 0 54 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 wp
S 627 23 0 0 0 6 712 624 5045 4 0 A 0 0 0 0 B 0 54 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iintegers
S 629 23 0 0 0 6 735 624 5072 4 0 A 0 0 0 0 B 0 60 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ie
S 630 23 0 0 0 6 736 624 5075 4 0 A 0 0 0 0 B 0 60 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 je
S 631 23 0 0 0 6 737 624 5078 4 0 A 0 0 0 0 B 0 60 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ke
S 632 23 0 0 0 6 773 624 5081 4 0 A 0 0 0 0 B 0 60 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 istartpar
S 633 23 0 0 0 6 774 624 5091 4 0 A 0 0 0 0 B 0 60 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iendpar
S 634 23 0 0 0 6 781 624 5099 4 0 A 0 0 0 0 B 0 60 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jstartpar
S 635 23 0 0 0 6 782 624 5109 4 0 A 0 0 0 0 B 0 60 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jendpar
S 636 23 0 0 0 8 804 624 5117 4 0 A 0 0 0 0 B 0 60 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 dt
S 637 23 0 0 0 6 824 624 5120 4 0 A 0 0 0 0 B 0 60 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 idt_qv
S 639 23 0 0 0 8 872 624 5142 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t0_melt
S 640 23 0 0 0 8 873 624 5150 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 r_d
S 641 23 0 0 0 8 875 624 5154 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rdv
S 642 23 0 0 0 8 876 624 5158 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 o_m_rdv
S 643 23 0 0 0 8 877 624 5166 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rvd_m_o
S 644 23 0 0 0 8 878 624 5174 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 cp_d
S 645 23 0 0 0 8 880 624 5179 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rdocp
S 646 23 0 0 0 6 883 624 5185 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lh_f
S 647 23 0 0 0 6 884 624 5190 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lh_s
S 648 23 0 0 0 8 897 624 5195 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rho_ice
S 649 23 0 0 0 8 903 624 5203 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b1
S 650 23 0 0 0 8 905 624 5206 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b2i
S 651 23 0 0 0 8 906 624 5210 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b3
S 652 23 0 0 0 8 908 624 5213 4 0 A 0 0 0 0 B 0 93 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b4i
S 654 23 0 0 0 8 950 624 5229 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 p0
S 655 23 0 0 0 8 1485 624 5232 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 depth_lk
S 656 23 0 0 0 6 1520 624 5241 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 llandmask
S 657 23 0 0 0 6 1527 624 5251 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lseamask
S 658 23 0 0 0 8 1582 624 5260 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 u
S 659 23 0 0 0 8 1591 624 5262 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 v
S 660 23 0 0 0 8 1609 624 5264 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t
S 661 23 0 0 0 8 1618 624 5266 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 pp
S 662 23 0 0 0 8 2882 624 5269 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ps
S 663 23 0 0 0 8 2890 624 5272 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_snow
S 664 23 0 0 0 8 3102 624 5279 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_snow_mult
S 665 23 0 0 0 8 2898 624 5291 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_s
S 666 23 0 0 0 8 2906 624 5295 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_g
S 667 23 0 0 0 8 2953 624 5299 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 w_snow
S 668 23 0 0 0 8 3160 624 5306 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_ice
S 669 23 0 0 0 8 3168 624 5312 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 h_ice
S 670 23 0 0 0 8 3393 624 5318 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tch
S 671 23 0 0 0 8 3414 624 5322 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tfv
S 672 23 0 0 0 8 3505 624 5326 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 sobs
S 673 23 0 0 0 8 3512 624 5331 4 0 A 0 0 0 0 B 0 119 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 thbs
S 675 23 0 0 0 8 4828 624 5352 4 0 A 0 0 0 0 B 0 166 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 zt_ice
S 677 23 0 0 0 8 5032 624 5369 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 cf_snow
S 679 23 0 0 0 6 5092 624 5393 4 0 A 0 0 0 0 B 0 177 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nstart
S 680 23 0 0 0 6 5095 624 5400 4 0 A 0 0 0 0 B 0 177 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ntstep
S 681 23 0 0 0 6 5097 624 5407 4 0 A 0 0 0 0 B 0 177 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnow
S 682 23 0 0 0 6 5098 624 5412 4 0 A 0 0 0 0 B 0 177 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnew
S 683 23 0 0 0 6 5173 624 5417 4 0 A 0 0 0 0 B 0 177 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lmulti_snow
S 684 23 0 0 0 6 5175 624 5429 4 0 A 0 0 0 0 B 0 177 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 llake
S 685 23 0 0 0 6 5210 624 5435 4 0 A 0 0 0 0 B 0 177 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 l2tls
S 687 23 0 0 0 6 5427 624 5455 4 0 A 0 0 0 0 B 0 201 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 my_cart_id
S 689 23 0 0 0 8 5745 624 5482 4 0 A 0 0 0 0 B 0 207 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tgcom
S 691 23 0 0 0 6 6388 624 5500 4 0 A 0 0 0 0 B 0 211 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 model_abort
S 693 23 0 0 0 8 7965 624 5523 4 0 A 0 0 0 0 B 0 215 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 trcr_get
S 694 23 0 0 0 8 8811 624 5532 4 0 A 0 0 0 0 B 0 215 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 trcr_errorstr
R 701 16 3 mo_kind wp
R 712 16 4 data_parameters iintegers
S 726 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 728 3 0 0 0 16 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16
R 735 6 7 data_modelconfig ie
R 736 6 8 data_modelconfig je
R 737 6 9 data_modelconfig ke
R 773 6 45 data_modelconfig istartpar
R 774 6 46 data_modelconfig iendpar
R 781 6 53 data_modelconfig jstartpar
R 782 6 54 data_modelconfig jendpar
R 804 6 76 data_modelconfig dt
R 824 6 96 data_modelconfig idt_qv
R 872 6 4 data_constants t0_melt
R 873 6 5 data_constants r_d
R 875 6 7 data_constants rdv
R 876 6 8 data_constants o_m_rdv
R 877 6 9 data_constants rvd_m_o
R 878 6 10 data_constants cp_d
R 880 6 12 data_constants rdocp
R 883 6 15 data_constants lh_f
R 884 6 16 data_constants lh_s
R 897 6 29 data_constants rho_ice
R 903 6 35 data_constants b1
R 905 6 37 data_constants b2i
R 906 6 38 data_constants b3
R 908 6 40 data_constants b4i
R 950 7 19 data_fields p0
R 1485 7 554 data_fields depth_lk
R 1520 7 589 data_fields llandmask
R 1527 7 596 data_fields lseamask
R 1582 7 651 data_fields u
R 1591 7 660 data_fields v
R 1609 7 678 data_fields t
R 1618 7 687 data_fields pp
R 2882 7 1951 data_fields ps
R 2890 7 1959 data_fields t_snow
R 2898 7 1967 data_fields t_s
R 2906 7 1975 data_fields t_g
R 2953 7 2022 data_fields w_snow
R 3102 7 2171 data_fields t_snow_mult
R 3160 7 2229 data_fields t_ice
R 3168 7 2237 data_fields h_ice
R 3393 7 2462 data_fields tch
R 3414 7 2483 data_fields tfv
R 3505 7 2574 data_fields sobs
R 3512 7 2581 data_fields thbs
R 4828 6 16 data_turbulence zt_ice
R 5032 6 32 data_soil cf_snow
R 5092 6 3 data_runcontrol nstart
R 5095 6 6 data_runcontrol ntstep
R 5097 6 8 data_runcontrol nnow
R 5098 6 9 data_runcontrol nnew
R 5173 6 84 data_runcontrol lmulti_snow
R 5175 6 86 data_runcontrol llake
R 5210 6 121 data_runcontrol l2tls
R 5427 6 19 data_parallel my_cart_id
R 5745 14 243 meteo_utilities tgcom
R 5764 25 6 iso_c_binding c_ptr
R 5765 5 7 iso_c_binding val c_ptr
R 5767 25 9 iso_c_binding c_funptr
R 5768 5 10 iso_c_binding val c_funptr
R 5802 6 44 iso_c_binding c_null_ptr$ac
R 5804 6 46 iso_c_binding c_null_funptr$ac
R 5805 26 47 iso_c_binding ==
R 5807 26 49 iso_c_binding !=
S 5834 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 5855 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 6388 14 389 environment model_abort
S 6798 3 0 0 0 4513 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 59491 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 55 4e 44 45 46 20 20 20 20 20 20 20
S 6799 3 0 0 0 4513 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 59504 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 49 4e 54 45 47 45 52 20 20 20 20 20
S 6800 3 0 0 0 4513 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 59517 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 52 45 41 4c 20 20 20 20 20 20 20 20
S 6801 3 0 0 0 4513 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 59530 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 44 4f 55 42 4c 45 20 20 20 20 20 20
S 6802 3 0 0 0 4513 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 59543 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 53 54 52 49 4e 47 20 20 20 20 20 20
S 6803 3 0 0 0 4513 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 59556 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 4c 4f 47 49 43 41 4c 20 20 20 20 20
S 6804 3 0 0 0 4513 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 59569 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 50 4f 49 4e 54 45 52 20 20 20 20 20
R 6831 7 27 data_tracer_metadata y_data_type$ac
R 6837 25 33 data_tracer_metadata t_metadata
R 6838 5 34 data_tracer_metadata lisready t_metadata
R 6839 5 35 data_tracer_metadata imaxkeys t_metadata
R 6840 5 36 data_tracer_metadata inumkey t_metadata
R 6841 5 37 data_tracer_metadata iunique t_metadata
R 6842 5 38 data_tracer_metadata iidx t_metadata
R 6844 5 40 data_tracer_metadata iidx$sd t_metadata
R 6845 5 41 data_tracer_metadata iidx$p t_metadata
R 6846 5 42 data_tracer_metadata iidx$o t_metadata
R 6848 5 44 data_tracer_metadata ykey t_metadata
R 6850 5 46 data_tracer_metadata ykey$sd t_metadata
R 6851 5 47 data_tracer_metadata ykey$p t_metadata
R 6852 5 48 data_tracer_metadata ykey$o t_metadata
R 6854 5 50 data_tracer_metadata iattr t_metadata
R 6856 5 52 data_tracer_metadata iattr$sd t_metadata
R 6857 5 53 data_tracer_metadata iattr$p t_metadata
R 6858 5 54 data_tracer_metadata iattr$o t_metadata
R 6860 5 56 data_tracer_metadata itype t_metadata
R 6862 5 58 data_tracer_metadata itype$sd t_metadata
R 6863 5 59 data_tracer_metadata itype$p t_metadata
R 6864 5 60 data_tracer_metadata itype$o t_metadata
R 6866 5 62 data_tracer_metadata ipos t_metadata
R 6868 5 64 data_tracer_metadata ipos$sd t_metadata
R 6869 5 65 data_tracer_metadata ipos$p t_metadata
R 6870 5 66 data_tracer_metadata ipos$o t_metadata
R 6873 5 69 data_tracer_metadata ilen t_metadata
R 6874 5 70 data_tracer_metadata ilen$sd t_metadata
R 6875 5 71 data_tracer_metadata ilen$p t_metadata
R 6876 5 72 data_tracer_metadata ilen$o t_metadata
R 6878 5 74 data_tracer_metadata imaxbuf t_metadata
R 6879 5 75 data_tracer_metadata inumbuf t_metadata
R 6880 5 76 data_tracer_metadata imaxbuflen t_metadata
R 6881 5 77 data_tracer_metadata ydefault t_metadata
R 6883 5 79 data_tracer_metadata ydefault$sd t_metadata
R 6884 5 80 data_tracer_metadata ydefault$p t_metadata
R 6885 5 81 data_tracer_metadata ydefault$o t_metadata
R 6887 5 83 data_tracer_metadata ybuf t_metadata
R 6890 5 86 data_tracer_metadata ybuf$sd t_metadata
R 6891 5 87 data_tracer_metadata ybuf$p t_metadata
R 6892 5 88 data_tracer_metadata ybuf$o t_metadata
R 7965 19 52 src_tracer trcr_get
R 8811 14 898 src_tracer trcr_errorstr
S 8813 23 5 0 0 0 8814 624 69975 0 0 A 0 0 0 0 B 0 524 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 seaice
S 8814 14 5 0 0 0 1 8813 69975 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1303 0 0 0 0 0 0 0 0 0 0 0 0 0 235 0 624 0 0 0 0 seaice
F 8814 0
A 44 2 0 0 0 6 726 0 0 0 44 0 0 0 0 0 0 0 0 0 0 0
A 94 2 0 0 0 16 728 0 0 0 94 0 0 0 0 0 0 0 0 0 0 0
A 8740 1 0 0 7989 3512 5802 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 8743 1 0 0 8215 3521 5804 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 8806 2 0 0 7106 6 5855 0 0 0 8806 0 0 0 0 0 0 0 0 0 0 0
A 8832 2 0 0 3961 6 5834 0 0 0 8832 0 0 0 0 0 0 0 0 0 0 0
A 9543 2 0 0 9418 3921 6798 0 0 0 9543 0 0 0 0 0 0 0 0 0 0 0
A 9544 2 0 0 9419 3921 6799 0 0 0 9544 0 0 0 0 0 0 0 0 0 0 0
A 9545 2 0 0 9421 3921 6800 0 0 0 9545 0 0 0 0 0 0 0 0 0 0 0
A 9546 2 0 0 9422 3921 6801 0 0 0 9546 0 0 0 0 0 0 0 0 0 0 0
A 9547 2 0 0 9424 3921 6802 0 0 0 9547 0 0 0 0 0 0 0 0 0 0 0
A 9548 2 0 0 9425 3921 6803 0 0 0 9548 0 0 0 0 0 0 0 0 0 0 0
A 9549 2 0 0 9427 3921 6804 0 0 0 9549 0 0 0 0 0 0 0 0 0 0 0
A 9635 1 0 11 8188 3923 6831 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
Z
J 149 1 1
V 8740 3512 7 0
S 0 3512 0 0 0
A 0 6 0 0 1 2 0
J 150 1 1
V 8743 3521 7 0
S 0 3521 0 0 0
A 0 6 0 0 1 2 0
J 99 1 1
V 9635 3923 7 0
R 0 3926 0 0
A 0 3921 0 0 1 9543 1
A 0 3921 0 0 1 9544 1
A 0 3921 0 0 1 9545 1
A 0 3921 0 0 1 9546 1
A 0 3921 0 0 1 9547 1
A 0 3921 0 0 1 9548 1
A 0 3921 0 0 1 9549 0
T 6837 3941 0 3 0 0
A 6838 16 0 0 1 94 0
Z
