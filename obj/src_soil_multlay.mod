V33 :0x4 src_soil_multlay
65 /users/geirund/code/cosmo_nwp_progCCNINP/src/src_soil_multlay.f90 S624 0
07/17/2019  16:51:36
use data_soil public 0 direct
use data_tracer_metadata private
use iso_c_binding private
use parallel_utilities private
use meteo_utilities private
use environment private
use src_tracer private
use data_parallel private
use data_io private
use data_runcontrol private
use data_fields private
use data_constants private
use data_modelconfig private
use data_parameters private
use mo_kind private
enduse
D 3619 24 5931 8 5930 7
D 3628 24 5934 8 5933 7
D 3649 18 8538
D 3651 21 3649 1 2 44 0 0 0 0 0
 2 8919 3 2 8919 44
D 3654 21 3649 1 3 44 0 0 0 0 0
 0 44 3 3 44 44
D 3669 24 6045 760 6044 7
D 3945 18 8538
D 5028 18 8477
S 624 24 0 0 0 8 1 0 5015 10005 0 A 0 0 0 0 B 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 src_soil_multlay
S 626 23 0 0 0 8 774 624 5048 4 0 A 0 0 0 0 B 0 151 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 wp
S 627 23 0 0 0 6 785 624 5051 4 0 A 0 0 0 0 B 0 151 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iintegers
S 629 23 0 0 0 6 808 624 5078 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ie
S 630 23 0 0 0 6 809 624 5081 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 je
S 631 23 0 0 0 6 810 624 5084 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ke
S 632 23 0 0 0 6 811 624 5087 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ke_soil
S 633 23 0 0 0 6 812 624 5095 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ke_snow
S 634 23 0 0 0 6 804 624 5103 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ie_tot
S 635 23 0 0 0 6 805 624 5110 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 je_tot
S 636 23 0 0 0 8 822 624 5117 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 czmls
S 637 23 0 0 0 6 846 624 5123 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 istartpar
S 638 23 0 0 0 6 847 624 5133 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iendpar
S 639 23 0 0 0 6 854 624 5141 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jstartpar
S 640 23 0 0 0 6 855 624 5151 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jendpar
S 641 23 0 0 0 8 877 624 5159 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 dt
S 642 23 0 0 0 6 897 624 5162 4 0 A 0 0 0 0 B 0 157 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 idt_qv
S 644 23 0 0 0 8 945 624 5184 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t0_melt
S 645 23 0 0 0 8 946 624 5192 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 r_d
S 646 23 0 0 0 8 948 624 5196 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rdv
S 647 23 0 0 0 8 949 624 5200 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 o_m_rdv
S 648 23 0 0 0 8 950 624 5208 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rvd_m_o
S 649 23 0 0 0 8 951 624 5216 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 cp_d
S 650 23 0 0 0 8 953 624 5221 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rdocp
S 651 23 0 0 0 6 955 624 5227 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lh_v
S 652 23 0 0 0 6 956 624 5232 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lh_f
S 653 23 0 0 0 6 957 624 5237 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lh_s
S 654 23 0 0 0 8 963 624 5242 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 g
S 655 23 0 0 0 8 973 624 5244 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 sigma
S 656 23 0 0 0 8 976 624 5250 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b1
S 657 23 0 0 0 8 977 624 5253 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b2w
S 658 23 0 0 0 8 978 624 5257 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b2i
S 659 23 0 0 0 8 979 624 5261 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b3
S 660 23 0 0 0 8 980 624 5264 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b4w
S 661 23 0 0 0 8 981 624 5268 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 b4i
S 662 23 0 0 0 8 969 624 5272 4 0 A 0 0 0 0 B 0 198 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rho_w
S 664 23 0 0 0 8 1023 624 5290 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 p0
S 665 23 0 0 0 8 1177 624 5293 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 soiltyp
S 666 23 0 0 0 8 1408 624 5301 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 plcov
S 667 23 0 0 0 8 1443 624 5307 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rootdp
S 668 23 0 0 0 8 1429 624 5314 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 sai
S 669 23 0 0 0 8 1422 624 5318 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tai
S 670 23 0 0 0 8 1436 624 5322 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 eai
S 671 23 0 0 0 6 1593 624 5326 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 llandmask
S 672 23 0 0 0 8 1148 624 5336 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rsmin2d
S 673 23 0 0 0 8 1655 624 5344 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 u
S 674 23 0 0 0 8 1664 624 5346 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 v
S 675 23 0 0 0 8 1682 624 5348 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t
S 676 23 0 0 0 8 1691 624 5350 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 pp
S 677 23 0 0 0 8 2955 624 5353 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ps
S 678 23 0 0 0 8 2963 624 5356 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_snow
S 679 23 0 0 0 8 3175 624 5363 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_snow_mult
S 680 23 0 0 0 8 2987 624 5375 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tg_radstep
S 681 23 0 0 0 8 2971 624 5386 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_s
S 682 23 0 0 0 8 2979 624 5390 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_g
S 683 23 0 0 0 8 2994 624 5394 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 qv_s
S 684 23 0 0 0 8 3026 624 5399 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 w_snow
S 685 23 0 0 0 8 3137 624 5406 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rho_snow
S 686 23 0 0 0 8 3211 624 5415 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rho_snow_mult
S 687 23 0 0 0 8 3129 624 5429 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 h_snow
S 688 23 0 0 0 8 3145 624 5436 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 fr_snow
S 689 23 0 0 0 8 3152 624 5444 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 fr_wi
S 690 23 0 0 0 8 3159 624 5450 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ustar_fv
S 691 23 0 0 0 8 3034 624 5459 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 w_i
S 692 23 0 0 0 8 3017 624 5463 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_so
S 693 23 0 0 0 8 3066 624 5468 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 w_so
S 694 23 0 0 0 8 1155 624 5473 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 swi
S 695 23 0 0 0 8 3075 624 5477 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 w_so_ice
S 696 23 0 0 0 8 2435 624 5486 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 wso_relax
S 697 23 0 0 0 8 3107 624 5496 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 s_so
S 698 23 0 0 0 8 4274 624 5501 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_2m
S 699 23 0 0 0 8 4316 624 5506 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 u_10m
S 700 23 0 0 0 8 4330 624 5512 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 v_10m
S 701 23 0 0 0 8 3115 624 5518 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 freshsnow
S 702 23 0 0 0 8 3193 624 5528 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 wliq_snow
S 703 23 0 0 0 8 3202 624 5538 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 w_snow_mult
S 704 23 0 0 0 8 3184 624 5550 4 0 A 0 0 0 0 B 0 230 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 dzh_snow_mult
S 705 23 0 0 0 8 3699 624 5564 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 prr_con
S 706 23 0 0 0 8 3706 624 5572 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 prs_con
S 707 23 0 0 0 8 3842 624 5580 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 prr_gsp
S 708 23 0 0 0 8 3849 624 5588 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 prs_gsp
S 709 23 0 0 0 8 3856 624 5596 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 prg_gsp
S 710 23 0 0 0 8 3863 624 5604 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 prh_gsp
S 711 23 0 0 0 8 3466 624 5612 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tch
S 712 23 0 0 0 8 3459 624 5616 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tcm
S 713 23 0 0 0 8 3487 624 5620 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tfv
S 714 23 0 0 0 8 3578 624 5624 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 sobs
S 715 23 0 0 0 8 3585 624 5629 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 thbs
S 716 23 0 0 0 8 3592 624 5634 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 pabs
S 717 23 0 0 0 8 4554 624 5639 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 runoff_s
S 718 23 0 0 0 8 4561 624 5648 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 runoff_g
S 719 23 0 0 0 8 3122 624 5657 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 snow_melt
S 720 23 0 0 0 8 4189 624 5667 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rstom
S 721 23 0 0 0 6 4196 624 5673 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lhfl_bs
S 722 23 0 0 0 6 4203 624 5681 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lhfl_pl
S 723 23 0 0 0 8 3220 624 5689 4 0 A 0 0 0 0 B 0 289 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 w_so_r
S 725 23 0 0 0 6 4876 624 5712 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nstart
S 726 23 0 0 0 6 4879 624 5719 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ntstep
S 727 23 0 0 0 6 4880 624 5726 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nold
S 728 23 0 0 0 6 4881 624 5731 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnow
S 729 23 0 0 0 6 4882 624 5736 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnew
S 730 23 0 0 0 6 4954 624 5741 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lmelt
S 731 23 0 0 0 6 4955 624 5747 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lmelt_var
S 732 23 0 0 0 6 4929 624 5757 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ibot_w_so
S 733 23 0 0 0 6 4957 624 5767 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lmulti_snow
S 734 23 0 0 0 6 4906 624 5779 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 itype_gscp
S 735 23 0 0 0 6 4904 624 5790 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 itype_trvg
S 736 23 0 0 0 6 4905 624 5801 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 itype_evsl
S 737 23 0 0 0 6 4908 624 5812 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 itype_tran
S 738 23 0 0 0 6 4921 624 5823 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 itype_root
S 739 23 0 0 0 6 4922 624 5834 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 itype_heatcond
S 740 23 0 0 0 6 4923 624 5849 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 itype_hydbound
S 741 23 0 0 0 6 4937 624 5864 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lstomata
S 742 23 0 0 0 6 4994 624 5873 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 l2tls
S 743 23 0 0 0 6 4899 624 5879 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nextrad
S 744 23 0 0 0 8 4981 624 5887 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hnextrad
S 745 23 0 0 0 8 4980 624 5896 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hincrad
S 746 23 0 0 0 6 4898 624 5904 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nincrad
S 747 23 0 0 0 6 5058 624 5912 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lrelax_soil
S 748 23 0 0 0 6 4975 624 5924 4 0 A 0 0 0 0 B 0 326 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lbud
S 750 23 0 0 0 6 5404 624 5937 4 0 A 0 0 0 0 B 0 368 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lana_rho_snow
S 752 23 0 0 0 6 5617 624 5965 4 0 A 0 0 0 0 B 0 374 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 num_compute
S 753 23 0 0 0 6 5638 624 5977 4 0 A 0 0 0 0 B 0 374 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 icomm_cart
S 754 23 0 0 0 6 5641 624 5988 4 0 A 0 0 0 0 B 0 374 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 imp_reals
S 755 23 0 0 0 6 5622 624 5998 4 0 A 0 0 0 0 B 0 374 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 my_cart_id
S 758 23 0 0 0 8 6775 624 6030 4 0 A 0 0 0 0 B 0 390 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 trcr_get
S 759 23 0 0 0 8 7621 624 6039 4 0 A 0 0 0 0 B 0 390 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 trcr_errorstr
S 761 23 0 0 0 8 8087 624 6065 4 0 A 0 0 0 0 B 0 396 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 collapse
S 762 23 0 0 0 6 8159 624 6074 4 0 A 0 0 0 0 B 0 396 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 model_abort
S 764 23 0 0 0 8 8809 624 6102 4 0 A 0 0 0 0 B 0 397 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tgcom
S 766 23 0 0 0 8 9148 624 6127 4 0 A 0 0 0 0 B 0 399 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 distribute_values
S 767 23 0 0 0 8 9639 624 6145 4 0 A 0 0 0 0 B 0 399 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 gather_field
R 774 16 3 mo_kind wp
R 785 16 4 data_parameters iintegers
S 799 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 801 3 0 0 0 16 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16
R 804 6 3 data_modelconfig ie_tot
R 805 6 4 data_modelconfig je_tot
R 808 6 7 data_modelconfig ie
R 809 6 8 data_modelconfig je
R 810 6 9 data_modelconfig ke
R 811 6 10 data_modelconfig ke_soil
R 812 6 11 data_modelconfig ke_snow
R 822 7 21 data_modelconfig czmls
R 846 6 45 data_modelconfig istartpar
R 847 6 46 data_modelconfig iendpar
R 854 6 53 data_modelconfig jstartpar
R 855 6 54 data_modelconfig jendpar
R 877 6 76 data_modelconfig dt
R 897 6 96 data_modelconfig idt_qv
R 945 6 4 data_constants t0_melt
R 946 6 5 data_constants r_d
R 948 6 7 data_constants rdv
R 949 6 8 data_constants o_m_rdv
R 950 6 9 data_constants rvd_m_o
R 951 6 10 data_constants cp_d
R 953 6 12 data_constants rdocp
R 955 6 14 data_constants lh_v
R 956 6 15 data_constants lh_f
R 957 6 16 data_constants lh_s
R 963 6 22 data_constants g
R 969 6 28 data_constants rho_w
R 973 6 32 data_constants sigma
R 976 6 35 data_constants b1
R 977 6 36 data_constants b2w
R 978 6 37 data_constants b2i
R 979 6 38 data_constants b3
R 980 6 39 data_constants b4w
R 981 6 40 data_constants b4i
R 1023 7 19 data_fields p0
R 1148 7 144 data_fields rsmin2d
R 1155 7 151 data_fields swi
R 1177 7 173 data_fields soiltyp
R 1408 7 404 data_fields plcov
R 1422 7 418 data_fields tai
R 1429 7 425 data_fields sai
R 1436 7 432 data_fields eai
R 1443 7 439 data_fields rootdp
R 1593 7 589 data_fields llandmask
R 1655 7 651 data_fields u
R 1664 7 660 data_fields v
R 1682 7 678 data_fields t
R 1691 7 687 data_fields pp
R 2435 7 1431 data_fields wso_relax
R 2955 7 1951 data_fields ps
R 2963 7 1959 data_fields t_snow
R 2971 7 1967 data_fields t_s
R 2979 7 1975 data_fields t_g
R 2987 7 1983 data_fields tg_radstep
R 2994 7 1990 data_fields qv_s
R 3017 7 2013 data_fields t_so
R 3026 7 2022 data_fields w_snow
R 3034 7 2030 data_fields w_i
R 3066 7 2062 data_fields w_so
R 3075 7 2071 data_fields w_so_ice
R 3107 7 2103 data_fields s_so
R 3115 7 2111 data_fields freshsnow
R 3122 7 2118 data_fields snow_melt
R 3129 7 2125 data_fields h_snow
R 3137 7 2133 data_fields rho_snow
R 3145 7 2141 data_fields fr_snow
R 3152 7 2148 data_fields fr_wi
R 3159 7 2155 data_fields ustar_fv
R 3175 7 2171 data_fields t_snow_mult
R 3184 7 2180 data_fields dzh_snow_mult
R 3193 7 2189 data_fields wliq_snow
R 3202 7 2198 data_fields w_snow_mult
R 3211 7 2207 data_fields rho_snow_mult
R 3220 7 2216 data_fields w_so_r
R 3459 7 2455 data_fields tcm
R 3466 7 2462 data_fields tch
R 3487 7 2483 data_fields tfv
R 3578 7 2574 data_fields sobs
R 3585 7 2581 data_fields thbs
R 3592 7 2588 data_fields pabs
R 3699 7 2695 data_fields prr_con
R 3706 7 2702 data_fields prs_con
R 3842 7 2838 data_fields prr_gsp
R 3849 7 2845 data_fields prs_gsp
R 3856 7 2852 data_fields prg_gsp
R 3863 7 2859 data_fields prh_gsp
R 4189 7 3185 data_fields rstom
R 4196 7 3192 data_fields lhfl_bs
R 4203 7 3199 data_fields lhfl_pl
R 4274 7 3270 data_fields t_2m
R 4316 7 3312 data_fields u_10m
R 4330 7 3326 data_fields v_10m
R 4554 7 3550 data_fields runoff_s
R 4561 7 3557 data_fields runoff_g
R 4876 6 3 data_runcontrol nstart
R 4879 6 6 data_runcontrol ntstep
R 4880 6 7 data_runcontrol nold
R 4881 6 8 data_runcontrol nnow
R 4882 6 9 data_runcontrol nnew
R 4898 6 25 data_runcontrol nincrad
R 4899 6 26 data_runcontrol nextrad
R 4904 6 31 data_runcontrol itype_trvg
R 4905 6 32 data_runcontrol itype_evsl
R 4906 6 33 data_runcontrol itype_gscp
R 4908 6 35 data_runcontrol itype_tran
R 4921 6 48 data_runcontrol itype_root
R 4922 6 49 data_runcontrol itype_heatcond
R 4923 6 50 data_runcontrol itype_hydbound
R 4929 6 56 data_runcontrol ibot_w_so
R 4937 6 64 data_runcontrol lstomata
R 4954 6 81 data_runcontrol lmelt
R 4955 6 82 data_runcontrol lmelt_var
R 4957 6 84 data_runcontrol lmulti_snow
R 4975 6 102 data_runcontrol lbud
R 4980 6 107 data_runcontrol hincrad
R 4981 6 108 data_runcontrol hnextrad
R 4994 6 121 data_runcontrol l2tls
R 5058 6 185 data_runcontrol lrelax_soil
S 5226 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 80 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 5229 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 5404 6 175 data_io lana_rho_snow
R 5617 6 14 data_parallel num_compute
R 5622 6 19 data_parallel my_cart_id
R 5638 6 35 data_parallel icomm_cart
R 5641 6 38 data_parallel imp_reals
R 5930 25 6 iso_c_binding c_ptr
R 5931 5 7 iso_c_binding val c_ptr
R 5933 25 9 iso_c_binding c_funptr
R 5934 5 10 iso_c_binding val c_funptr
R 5968 6 44 iso_c_binding c_null_ptr$ac
R 5970 6 46 iso_c_binding c_null_funptr$ac
R 5971 26 47 iso_c_binding ==
R 5973 26 49 iso_c_binding !=
S 6003 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 6005 3 0 0 0 3945 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 54631 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 55 4e 44 45 46 20 20 20 20 20 20 20
S 6006 3 0 0 0 3945 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 54644 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 49 4e 54 45 47 45 52 20 20 20 20 20
S 6007 3 0 0 0 3945 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 54657 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 52 45 41 4c 20 20 20 20 20 20 20 20
S 6008 3 0 0 0 3945 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 54670 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 44 4f 55 42 4c 45 20 20 20 20 20 20
S 6009 3 0 0 0 3945 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 54683 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 53 54 52 49 4e 47 20 20 20 20 20 20
S 6010 3 0 0 0 3945 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 54696 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 4c 4f 47 49 43 41 4c 20 20 20 20 20
S 6011 3 0 0 0 3945 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 54709 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 50 4f 49 4e 54 45 52 20 20 20 20 20
R 6038 7 27 data_tracer_metadata y_data_type$ac
R 6044 25 33 data_tracer_metadata t_metadata
R 6045 5 34 data_tracer_metadata lisready t_metadata
R 6046 5 35 data_tracer_metadata imaxkeys t_metadata
R 6047 5 36 data_tracer_metadata inumkey t_metadata
R 6048 5 37 data_tracer_metadata iunique t_metadata
R 6049 5 38 data_tracer_metadata iidx t_metadata
R 6051 5 40 data_tracer_metadata iidx$sd t_metadata
R 6052 5 41 data_tracer_metadata iidx$p t_metadata
R 6053 5 42 data_tracer_metadata iidx$o t_metadata
R 6055 5 44 data_tracer_metadata ykey t_metadata
R 6057 5 46 data_tracer_metadata ykey$sd t_metadata
R 6058 5 47 data_tracer_metadata ykey$p t_metadata
R 6059 5 48 data_tracer_metadata ykey$o t_metadata
R 6061 5 50 data_tracer_metadata iattr t_metadata
R 6063 5 52 data_tracer_metadata iattr$sd t_metadata
R 6064 5 53 data_tracer_metadata iattr$p t_metadata
R 6065 5 54 data_tracer_metadata iattr$o t_metadata
R 6067 5 56 data_tracer_metadata itype t_metadata
R 6069 5 58 data_tracer_metadata itype$sd t_metadata
R 6070 5 59 data_tracer_metadata itype$p t_metadata
R 6071 5 60 data_tracer_metadata itype$o t_metadata
R 6073 5 62 data_tracer_metadata ipos t_metadata
R 6075 5 64 data_tracer_metadata ipos$sd t_metadata
R 6076 5 65 data_tracer_metadata ipos$p t_metadata
R 6077 5 66 data_tracer_metadata ipos$o t_metadata
R 6080 5 69 data_tracer_metadata ilen t_metadata
R 6081 5 70 data_tracer_metadata ilen$sd t_metadata
R 6082 5 71 data_tracer_metadata ilen$p t_metadata
R 6083 5 72 data_tracer_metadata ilen$o t_metadata
R 6085 5 74 data_tracer_metadata imaxbuf t_metadata
R 6086 5 75 data_tracer_metadata inumbuf t_metadata
R 6087 5 76 data_tracer_metadata imaxbuflen t_metadata
R 6088 5 77 data_tracer_metadata ydefault t_metadata
R 6090 5 79 data_tracer_metadata ydefault$sd t_metadata
R 6091 5 80 data_tracer_metadata ydefault$p t_metadata
R 6092 5 81 data_tracer_metadata ydefault$o t_metadata
R 6094 5 83 data_tracer_metadata ybuf t_metadata
R 6097 5 86 data_tracer_metadata ybuf$sd t_metadata
R 6098 5 87 data_tracer_metadata ybuf$p t_metadata
R 6099 5 88 data_tracer_metadata ybuf$o t_metadata
R 6775 19 52 src_tracer trcr_get
R 7621 14 898 src_tracer trcr_errorstr
R 8087 14 317 environment collapse
R 8159 14 389 environment model_abort
R 8809 14 243 meteo_utilities tgcom
R 9148 19 335 parallel_utilities distribute_values
R 9639 14 826 parallel_utilities gather_field
S 9818 23 5 0 0 0 9821 624 72099 0 0 A 0 0 0 0 B 0 4472 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 terra_multlay
S 9819 1 3 0 0 5028 1 9818 72113 4 3000 A 0 0 0 0 B 0 4472 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 yerror
S 9820 1 3 0 0 6 1 9818 66857 4 3000 A 0 0 0 0 B 0 4472 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 ierror
S 9821 14 5 0 0 0 1 9818 72099 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1731 2 0 0 0 0 0 0 0 0 0 0 0 0 450 0 624 0 0 0 0 terra_multlay
F 9821 2 9819 9820
A 44 2 0 0 0 6 799 0 0 0 44 0 0 0 0 0 0 0 0 0 0 0
A 94 2 0 0 0 16 801 0 0 0 94 0 0 0 0 0 0 0 0 0 0 0
A 8477 2 0 0 8464 6 5226 0 0 0 8477 0 0 0 0 0 0 0 0 0 0 0
A 8538 2 0 0 6771 6 5229 0 0 0 8538 0 0 0 0 0 0 0 0 0 0 0
A 8900 1 0 0 6725 3619 5968 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 8903 1 0 0 5541 3628 5970 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 8919 2 0 0 7467 6 6003 0 0 0 8919 0 0 0 0 0 0 0 0 0 0 0
A 8930 2 0 0 7825 3649 6005 0 0 0 8930 0 0 0 0 0 0 0 0 0 0 0
A 8931 2 0 0 8578 3649 6006 0 0 0 8931 0 0 0 0 0 0 0 0 0 0 0
A 8932 2 0 0 8512 3649 6007 0 0 0 8932 0 0 0 0 0 0 0 0 0 0 0
A 8933 2 0 0 8144 3649 6008 0 0 0 8933 0 0 0 0 0 0 0 0 0 0 0
A 8934 2 0 0 6658 3649 6009 0 0 0 8934 0 0 0 0 0 0 0 0 0 0 0
A 8935 2 0 0 5935 3649 6010 0 0 0 8935 0 0 0 0 0 0 0 0 0 0 0
A 8936 2 0 0 8424 3649 6011 0 0 0 8936 0 0 0 0 0 0 0 0 0 0 0
A 9022 1 0 13 8786 3651 6038 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
Z
J 149 1 1
V 8900 3619 7 0
S 0 3619 0 0 0
A 0 6 0 0 1 2 0
J 150 1 1
V 8903 3628 7 0
S 0 3628 0 0 0
A 0 6 0 0 1 2 0
J 99 1 1
V 9022 3651 7 0
R 0 3654 0 0
A 0 3649 0 0 1 8930 1
A 0 3649 0 0 1 8931 1
A 0 3649 0 0 1 8932 1
A 0 3649 0 0 1 8933 1
A 0 3649 0 0 1 8934 1
A 0 3649 0 0 1 8935 1
A 0 3649 0 0 1 8936 0
T 6044 3669 0 3 0 0
A 6045 16 0 0 1 94 0
Z
