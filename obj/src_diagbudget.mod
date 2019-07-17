V33 :0x4 src_diagbudget
61 /users/dedekind/code/yves_gesa_inp_ccn/src/src_diagbudget.f90 S624 0
07/07/2019  22:41:51
use data_tracer_metadata private
use iso_c_binding private
use data_tracer private
use src_tracer private
use environment private
use data_parallel private
use data_runcontrol private
use data_fields private
use data_modelconfig private
use data_parameters private
use mo_kind private
enduse
D 3260 24 5153 8 5152 7
D 3269 24 5156 8 5155 7
D 3669 18 8463
D 3671 21 3669 1 2 44 0 0 0 0 0
 2 8489 3 2 8489 44
D 3674 21 3669 1 3 44 0 0 0 0 0
 0 44 3 3 44 44
D 3689 24 6226 760 6225 7
D 4555 18 8463
D 4621 21 9 3 10925 10933 0 0 1 0 0
 0 10926 3 3 10927 10927
 0 10928 10927 3 10929 10929
 0 10930 10931 3 10932 10932
D 4624 21 9 2 10934 10931 0 0 1 0 0
 0 10926 3 3 10927 10927
 0 10928 10927 3 10929 10929
S 624 24 0 0 0 8 1 0 5015 10005 0 A 0 0 0 0 B 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 src_diagbudget
S 626 23 0 0 0 8 676 624 5046 4 0 A 0 0 0 0 B 0 52 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 wp
S 627 23 0 0 0 6 687 624 5049 4 0 A 0 0 0 0 B 0 52 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iintegers
S 629 23 0 0 0 6 710 624 5076 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ie
S 630 23 0 0 0 6 711 624 5079 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 je
S 631 23 0 0 0 6 712 624 5082 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ke
S 632 23 0 0 0 6 742 624 5085 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 istart
S 633 23 0 0 0 6 743 624 5092 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iend
S 634 23 0 0 0 6 750 624 5097 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jstart
S 635 23 0 0 0 6 751 624 5104 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jend
S 636 23 0 0 0 8 769 624 5109 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 eddlon
S 637 23 0 0 0 8 770 624 5116 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 eddlat
S 638 23 0 0 0 8 779 624 5123 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 dt
S 639 23 0 0 0 6 799 624 5126 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 idt_qv
S 640 23 0 0 0 6 800 624 5133 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 idt_qc
S 641 23 0 0 0 6 801 624 5140 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 idt_qi
S 643 23 0 0 0 8 914 624 5159 4 0 A 0 0 0 0 B 0 96 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hhl
S 644 23 0 0 0 8 1210 624 5163 4 0 A 0 0 0 0 B 0 96 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 crlat
S 645 23 0 0 0 8 1217 624 5169 4 0 A 0 0 0 0 B 0 96 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 acrlat
S 646 23 0 0 0 8 1506 624 5176 4 0 A 0 0 0 0 B 0 96 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 u
S 647 23 0 0 0 8 1515 624 5178 4 0 A 0 0 0 0 B 0 96 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 v
S 648 23 0 0 0 8 1524 624 5180 4 0 A 0 0 0 0 B 0 96 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 w
S 649 23 0 0 0 8 1533 624 5182 4 0 A 0 0 0 0 B 0 96 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t
S 650 23 0 0 0 8 1542 624 5184 4 0 A 0 0 0 0 B 0 96 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 pp
S 651 23 0 0 0 8 3164 624 5187 4 0 A 0 0 0 0 B 0 115 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rho
S 652 23 0 0 0 8 3970 624 5191 4 0 A 0 0 0 0 B 0 115 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 qvsflx
S 653 23 0 0 0 8 4419 624 5198 4 0 A 0 0 0 0 B 0 115 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 tdiv_hum
S 654 23 0 0 0 8 4426 624 5207 4 0 A 0 0 0 0 B 0 115 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 aevap_s
S 656 23 0 0 0 6 4731 624 5231 4 0 A 0 0 0 0 B 0 129 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nold
S 657 23 0 0 0 6 4732 624 5236 4 0 A 0 0 0 0 B 0 129 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnow
S 658 23 0 0 0 6 4733 624 5241 4 0 A 0 0 0 0 B 0 129 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnew
S 659 23 0 0 0 6 4782 624 5246 4 0 A 0 0 0 0 B 0 129 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lphys
S 660 23 0 0 0 6 4800 624 5252 4 0 A 0 0 0 0 B 0 129 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lgsp
S 662 23 0 0 0 6 5062 624 5271 4 0 A 0 0 0 0 B 0 144 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 my_cart_id
S 664 23 0 0 0 6 5776 624 5294 4 0 A 0 0 0 0 B 0 148 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 model_abort
S 666 23 0 0 0 8 7353 624 5317 4 0 A 0 0 0 0 B 0 152 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 trcr_get
S 667 23 0 0 0 8 8199 624 5326 4 0 A 0 0 0 0 B 0 152 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 trcr_errorstr
S 669 23 0 0 0 8 8252 624 5352 4 0 A 0 0 0 0 B 0 153 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_err_notfound
R 676 16 3 mo_kind wp
R 687 16 4 data_parameters iintegers
S 701 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 703 3 0 0 0 16 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16
R 710 6 7 data_modelconfig ie
R 711 6 8 data_modelconfig je
R 712 6 9 data_modelconfig ke
R 742 6 39 data_modelconfig istart
R 743 6 40 data_modelconfig iend
R 750 6 47 data_modelconfig jstart
R 751 6 48 data_modelconfig jend
R 769 6 66 data_modelconfig eddlon
R 770 6 67 data_modelconfig eddlat
R 779 6 76 data_modelconfig dt
R 799 6 96 data_modelconfig idt_qv
R 800 6 97 data_modelconfig idt_qc
R 801 6 98 data_modelconfig idt_qi
R 914 7 59 data_fields hhl
R 1210 7 355 data_fields crlat
R 1217 7 362 data_fields acrlat
R 1506 7 651 data_fields u
R 1515 7 660 data_fields v
R 1524 7 669 data_fields w
R 1533 7 678 data_fields t
R 1542 7 687 data_fields pp
R 3164 7 2309 data_fields rho
R 3970 7 3115 data_fields qvsflx
R 4419 7 3564 data_fields tdiv_hum
R 4426 7 3571 data_fields aevap_s
R 4731 6 7 data_runcontrol nold
R 4732 6 8 data_runcontrol nnow
R 4733 6 9 data_runcontrol nnew
R 4782 6 58 data_runcontrol lphys
R 4800 6 76 data_runcontrol lgsp
R 5062 6 19 data_parallel my_cart_id
R 5152 25 6 iso_c_binding c_ptr
R 5153 5 7 iso_c_binding val c_ptr
R 5155 25 9 iso_c_binding c_funptr
R 5156 5 10 iso_c_binding val c_funptr
R 5190 6 44 iso_c_binding c_null_ptr$ac
R 5192 6 46 iso_c_binding c_null_funptr$ac
R 5193 26 47 iso_c_binding ==
R 5195 26 49 iso_c_binding !=
S 5222 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 5243 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 5776 14 389 environment model_abort
S 6186 3 0 0 0 4555 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 57471 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 55 4e 44 45 46 20 20 20 20 20 20 20
S 6187 3 0 0 0 4555 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 57484 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 49 4e 54 45 47 45 52 20 20 20 20 20
S 6188 3 0 0 0 4555 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 57497 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 52 45 41 4c 20 20 20 20 20 20 20 20
S 6189 3 0 0 0 4555 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 57510 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 44 4f 55 42 4c 45 20 20 20 20 20 20
S 6190 3 0 0 0 4555 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 57523 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 53 54 52 49 4e 47 20 20 20 20 20 20
S 6191 3 0 0 0 4555 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 57536 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 4c 4f 47 49 43 41 4c 20 20 20 20 20
S 6192 3 0 0 0 4555 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 57549 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 50 4f 49 4e 54 45 52 20 20 20 20 20
R 6219 7 27 data_tracer_metadata y_data_type$ac
R 6225 25 33 data_tracer_metadata t_metadata
R 6226 5 34 data_tracer_metadata lisready t_metadata
R 6227 5 35 data_tracer_metadata imaxkeys t_metadata
R 6228 5 36 data_tracer_metadata inumkey t_metadata
R 6229 5 37 data_tracer_metadata iunique t_metadata
R 6230 5 38 data_tracer_metadata iidx t_metadata
R 6232 5 40 data_tracer_metadata iidx$sd t_metadata
R 6233 5 41 data_tracer_metadata iidx$p t_metadata
R 6234 5 42 data_tracer_metadata iidx$o t_metadata
R 6236 5 44 data_tracer_metadata ykey t_metadata
R 6238 5 46 data_tracer_metadata ykey$sd t_metadata
R 6239 5 47 data_tracer_metadata ykey$p t_metadata
R 6240 5 48 data_tracer_metadata ykey$o t_metadata
R 6242 5 50 data_tracer_metadata iattr t_metadata
R 6244 5 52 data_tracer_metadata iattr$sd t_metadata
R 6245 5 53 data_tracer_metadata iattr$p t_metadata
R 6246 5 54 data_tracer_metadata iattr$o t_metadata
R 6248 5 56 data_tracer_metadata itype t_metadata
R 6250 5 58 data_tracer_metadata itype$sd t_metadata
R 6251 5 59 data_tracer_metadata itype$p t_metadata
R 6252 5 60 data_tracer_metadata itype$o t_metadata
R 6254 5 62 data_tracer_metadata ipos t_metadata
R 6256 5 64 data_tracer_metadata ipos$sd t_metadata
R 6257 5 65 data_tracer_metadata ipos$p t_metadata
R 6258 5 66 data_tracer_metadata ipos$o t_metadata
R 6261 5 69 data_tracer_metadata ilen t_metadata
R 6262 5 70 data_tracer_metadata ilen$sd t_metadata
R 6263 5 71 data_tracer_metadata ilen$p t_metadata
R 6264 5 72 data_tracer_metadata ilen$o t_metadata
R 6266 5 74 data_tracer_metadata imaxbuf t_metadata
R 6267 5 75 data_tracer_metadata inumbuf t_metadata
R 6268 5 76 data_tracer_metadata imaxbuflen t_metadata
R 6269 5 77 data_tracer_metadata ydefault t_metadata
R 6271 5 79 data_tracer_metadata ydefault$sd t_metadata
R 6272 5 80 data_tracer_metadata ydefault$p t_metadata
R 6273 5 81 data_tracer_metadata ydefault$o t_metadata
R 6275 5 83 data_tracer_metadata ybuf t_metadata
R 6278 5 86 data_tracer_metadata ybuf$sd t_metadata
R 6279 5 87 data_tracer_metadata ybuf$p t_metadata
R 6280 5 88 data_tracer_metadata ybuf$o t_metadata
R 7353 19 52 src_tracer trcr_get
R 8199 14 898 src_tracer trcr_errorstr
R 8252 16 17 data_tracer t_err_notfound
S 8379 23 5 0 0 0 8380 624 69698 0 0 A 0 0 0 0 B 0 259 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 organize_diagbudget
S 8380 14 5 0 0 0 1 8379 69698 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1124 0 0 0 0 0 0 0 0 0 0 0 0 0 170 0 624 0 0 0 0 organize_diagbudget
F 8380 0
S 8381 23 5 0 0 0 8384 624 69718 0 0 A 0 0 0 0 B 0 358 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 horizontal_divergence
S 8382 7 3 1 0 4621 1 8381 69740 800204 3000 A 0 0 0 0 B 0 358 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 psi
S 8383 7 3 2 0 4624 1 8381 69744 800204 3000 A 0 0 0 0 B 0 358 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 div_h
S 8384 14 5 0 0 0 1 8381 69718 200 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1125 2 0 0 0 0 0 0 0 0 0 0 0 0 266 0 624 0 0 0 0 horizontal_divergence
F 8384 2 8382 8383
S 8385 6 1 0 0 6 1 8381 69750 40800006 3000 A 0 0 0 0 B 0 358 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_10925
S 8386 6 1 0 0 6 1 8381 69760 40800006 3000 A 0 0 0 0 B 0 358 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_10926
S 8387 6 1 0 0 6 1 8381 69770 40800006 3000 A 0 0 0 0 B 0 358 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_10930
S 8388 6 1 0 0 6 1 8381 69780 40800006 3000 A 0 0 0 0 B 0 358 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_10927
S 8389 6 1 0 0 6 1 8381 69790 40800006 3000 A 0 0 0 0 B 0 358 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_10933
S 8390 6 1 0 0 6 1 8381 69800 40800006 3000 A 0 0 0 0 B 0 358 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_10936
S 8391 6 1 0 0 6 1 8381 69810 40800006 3000 A 0 0 0 0 B 0 358 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_10935
A 44 2 0 0 0 6 701 0 0 0 44 0 0 0 0 0 0 0 0 0 0 0
A 94 2 0 0 0 16 703 0 0 0 94 0 0 0 0 0 0 0 0 0 0 0
A 8397 1 0 0 8000 3260 5190 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 8400 1 0 0 7935 3269 5192 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 8463 2 0 0 5868 6 5243 0 0 0 8463 0 0 0 0 0 0 0 0 0 0 0
A 8489 2 0 0 8245 6 5222 0 0 0 8489 0 0 0 0 0 0 0 0 0 0 0
A 9200 2 0 0 7488 3669 6186 0 0 0 9200 0 0 0 0 0 0 0 0 0 0 0
A 9201 2 0 0 7724 3669 6187 0 0 0 9201 0 0 0 0 0 0 0 0 0 0 0
A 9202 2 0 0 8210 3669 6188 0 0 0 9202 0 0 0 0 0 0 0 0 0 0 0
A 9203 2 0 0 7132 3669 6189 0 0 0 9203 0 0 0 0 0 0 0 0 0 0 0
A 9204 2 0 0 9197 3669 6190 0 0 0 9204 0 0 0 0 0 0 0 0 0 0 0
A 9205 2 0 0 9024 3669 6191 0 0 0 9205 0 0 0 0 0 0 0 0 0 0 0
A 9206 2 0 0 7729 3669 6192 0 0 0 9206 0 0 0 0 0 0 0 0 0 0 0
A 9292 1 0 9 9005 3671 6219 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 10925 1 0 0 9896 6 8390 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 10926 1 0 0 9784 6 710 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 10927 1 0 0 10431 6 8385 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 10928 1 0 0 9994 6 711 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 10929 1 0 0 10428 6 8386 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 10930 1 0 0 10883 6 712 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 10931 1 0 0 10430 6 8387 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 10932 1 0 0 9646 6 8388 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 10933 1 0 0 9643 6 8389 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 10934 1 0 0 8889 6 8391 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
Z
J 149 1 1
V 8397 3260 7 0
S 0 3260 0 0 0
A 0 6 0 0 1 2 0
J 150 1 1
V 8400 3269 7 0
S 0 3269 0 0 0
A 0 6 0 0 1 2 0
J 99 1 1
V 9292 3671 7 0
R 0 3674 0 0
A 0 3669 0 0 1 9200 1
A 0 3669 0 0 1 9201 1
A 0 3669 0 0 1 9202 1
A 0 3669 0 0 1 9203 1
A 0 3669 0 0 1 9204 1
A 0 3669 0 0 1 9205 1
A 0 3669 0 0 1 9206 0
T 6225 3689 0 3 0 0
A 6226 16 0 0 1 94 0
Z
