V33 :0x4 src_lheating
61 /users/geirund/code/cosmo_nwp_progCCNINP/src/src_lheating.f90 S624 0
07/17/2019  16:51:12
use iso_c_binding private
use environment private
use data_lheat_nudge private
use data_parallel private
use data_fields private
use data_runcontrol private
use data_parameters private
use mo_kind private
enduse
D 3359 24 5166 8 5165 7
D 3368 24 5169 8 5168 7
D 3756 18 8604
D 3758 21 9 3 9378 9387 1 1 0 0 1
 3 9379 3 3 9379 9380
 3 9381 9382 3 9381 9383
 3 9384 9385 3 9384 9386
S 624 24 0 0 0 8 1 0 5015 10005 0 A 0 0 0 0 B 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 src_lheating
S 626 23 0 0 0 8 646 624 5044 4 0 A 0 0 0 0 B 0 47 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 wp
S 627 23 0 0 0 6 657 624 5047 4 0 A 0 0 0 0 B 0 47 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iintegers
S 629 23 0 0 0 6 689 624 5073 4 0 A 0 0 0 0 B 0 48 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnew
S 631 23 0 0 0 8 1686 624 5090 4 0 A 0 0 0 0 B 0 49 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t
S 633 23 0 0 0 6 4891 624 5106 4 0 A 0 0 0 0 B 0 50 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 my_cart_id
S 635 23 0 0 0 8 5015 624 5134 4 0 A 0 0 0 0 B 0 51 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tt_lheat
S 636 23 0 0 0 6 4995 624 5143 4 0 A 0 0 0 0 B 0 51 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ktop_lhn
S 637 23 0 0 0 6 4996 624 5152 4 0 A 0 0 0 0 B 0 51 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 kbot_lhn
S 639 23 0 0 0 6 5788 624 5173 4 0 A 0 0 0 0 B 0 53 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 model_abort
R 646 16 3 mo_kind wp
R 657 16 4 data_parameters iintegers
R 689 6 9 data_runcontrol nnew
R 1686 7 678 data_fields t
R 4891 6 19 data_parallel my_cart_id
R 4995 6 27 data_lheat_nudge ktop_lhn
R 4996 6 28 data_lheat_nudge kbot_lhn
R 5015 7 47 data_lheat_nudge tt_lheat
R 5165 25 6 iso_c_binding c_ptr
R 5166 5 7 iso_c_binding val c_ptr
R 5168 25 9 iso_c_binding c_funptr
R 5169 5 10 iso_c_binding val c_funptr
R 5203 6 44 iso_c_binding c_null_ptr$ac
R 5205 6 46 iso_c_binding c_null_funptr$ac
R 5206 26 47 iso_c_binding ==
R 5208 26 49 iso_c_binding !=
S 5236 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 5788 14 389 environment model_abort
S 6196 23 5 0 0 0 6201 624 57899 0 0 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 get_gs_lheating
S 6197 1 3 1 0 3756 1 6196 57915 4 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 cmode
S 6198 1 3 1 0 6 1 6196 57921 4 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 kup
S 6199 1 3 1 0 6 1 6196 57925 4 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 klow
S 6200 7 3 1 0 3758 1 6196 57930 a0000004 10003000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tinc
S 6201 14 5 0 0 0 1 6196 57899 20000000 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 260 4 0 0 0 0 0 0 0 0 0 0 0 0 67 0 624 0 0 0 0 get_gs_lheating
F 6201 4 6197 6198 6199 6200
S 6202 6 1 0 0 6 1 6196 57935 40800006 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_1_1
S 6203 6 1 0 0 6 1 6196 57943 40800006 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_2_1
S 6204 6 1 0 0 6 1 6196 57951 40800006 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_4_1
S 6205 6 1 0 0 6 1 6196 57959 40800006 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_5_1
S 6206 6 1 0 0 6 1 6196 57967 40800006 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_7_1
S 6207 6 1 0 0 6 1 6196 57975 40800006 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_8_1
S 6208 6 1 0 0 6 1 6196 57983 40800006 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_9_1
S 6209 6 1 0 0 6 1 6196 57991 40800006 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_9389
S 6210 6 1 0 0 6 1 6196 58000 40800006 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_9392
S 6211 6 1 0 0 6 1 6196 58009 40800006 3000 A 0 0 0 0 B 0 176 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_9395
A 8599 1 0 0 8323 3359 5203 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 8602 1 0 0 8579 3368 5205 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 8604 2 0 0 8002 6 5236 0 0 0 8604 0 0 0 0 0 0 0 0 0 0 0
A 9378 1 0 0 7840 6 6208 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 9379 1 0 0 8159 6 6202 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 9380 1 0 0 9296 6 6209 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 9381 1 0 0 8224 6 6204 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 9382 1 0 0 8145 6 6203 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 9383 1 0 0 7734 6 6210 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 9384 1 0 0 8223 6 6206 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 9385 1 0 0 7626 6 6205 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 9386 1 0 0 8232 6 6211 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 9387 1 0 0 7745 6 6207 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
Z
J 149 1 1
V 8599 3359 7 0
S 0 3359 0 0 0
A 0 6 0 0 1 2 0
J 150 1 1
V 8602 3368 7 0
S 0 3368 0 0 0
A 0 6 0 0 1 2 0
Z
