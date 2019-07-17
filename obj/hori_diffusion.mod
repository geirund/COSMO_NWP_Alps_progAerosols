V33 :0x4 hori_diffusion
61 /users/dedekind/code/yves_gesa_inp_ccn/src/hori_diffusion.f90 S624 0
07/07/2019  22:42:13
use data_tracer_metadata private
use iso_c_binding private
use parallel_utilities private
use numeric_utilities private
use time_utilities private
use environment private
use src_tracer private
use data_tracer private
use data_constants private
use data_runcontrol private
use data_parallel private
use data_fields private
use data_modelconfig private
use data_parameters private
use mo_kind private
enduse
D 3260 24 5269 8 5268 7
D 3269 24 5272 8 5271 7
D 3290 18 8401
D 3292 21 3290 1 2 44 0 0 0 0 0
 2 8419 3 2 8419 44
D 3295 21 3290 1 3 44 0 0 0 0 0
 0 44 3 3 44 44
D 3310 24 5386 760 5385 7
D 3964 18 8401
D 5289 21 9 3 12159 12165 0 0 1 0 0
 0 11565 3 3 12160 12160
 0 11567 12160 3 12161 12161
 0 12162 12163 3 12164 12164
D 5292 21 9 3 12159 12165 0 0 1 0 0
 0 11565 3 3 12160 12160
 0 11567 12160 3 12161 12161
 0 12162 12163 3 12164 12164
D 5295 21 9 3 12159 12165 0 0 1 0 0
 0 11565 3 3 12160 12160
 0 11567 12160 3 12161 12161
 0 12162 12163 3 12164 12164
D 5298 21 9 3 12159 12165 0 0 1 0 0
 0 11565 3 3 12160 12160
 0 11567 12160 3 12161 12161
 0 12162 12163 3 12164 12164
D 5301 21 9 3 12159 12165 0 0 1 0 0
 0 11565 3 3 12160 12160
 0 11567 12160 3 12161 12161
 0 12162 12163 3 12164 12164
D 5304 21 9 3 12159 12165 0 0 1 0 0
 0 11565 3 3 12160 12160
 0 11567 12160 3 12161 12161
 0 12162 12163 3 12164 12164
D 5307 21 9 3 12166 12171 0 0 1 0 0
 0 11565 3 3 12167 12167
 0 11567 12167 3 12168 12168
 0 12162 12169 3 12170 12170
S 624 24 0 0 0 8 1 0 5015 10005 0 A 0 0 0 0 B 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 hori_diffusion
S 626 23 0 0 0 8 741 624 5046 4 0 A 0 0 0 0 B 0 84 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 wp
S 627 23 0 0 0 6 752 624 5049 4 0 A 0 0 0 0 B 0 84 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iintegers
S 629 23 0 0 0 6 775 624 5076 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ie
S 630 23 0 0 0 6 776 624 5079 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 je
S 631 23 0 0 0 6 777 624 5082 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ke
S 632 23 0 0 0 6 781 624 5085 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ke1
S 633 23 0 0 0 6 807 624 5089 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 istart
S 634 23 0 0 0 6 808 624 5096 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iend
S 635 23 0 0 0 6 809 624 5101 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 istartu
S 636 23 0 0 0 6 810 624 5109 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iendu
S 637 23 0 0 0 6 811 624 5115 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 istartv
S 638 23 0 0 0 6 812 624 5123 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iendv
S 639 23 0 0 0 6 815 624 5129 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jstart
S 640 23 0 0 0 6 816 624 5136 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jend
S 641 23 0 0 0 6 817 624 5141 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jstartu
S 642 23 0 0 0 6 818 624 5149 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jendu
S 643 23 0 0 0 6 819 624 5155 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jstartv
S 644 23 0 0 0 6 820 624 5163 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jendv
S 645 23 0 0 0 6 821 624 5169 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jstartpar
S 646 23 0 0 0 6 822 624 5179 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 jendpar
S 647 23 0 0 0 8 834 624 5187 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 eddlon
S 648 23 0 0 0 8 835 624 5194 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 eddlat
S 649 23 0 0 0 8 844 624 5201 4 0 A 0 0 0 0 B 0 90 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 dt
S 651 23 0 0 0 8 923 624 5216 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rho0
S 652 23 0 0 0 8 971 624 5221 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t0
S 653 23 0 0 0 8 939 624 5224 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 p0
S 654 23 0 0 0 8 979 624 5227 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hhl
S 655 23 0 0 0 8 1171 624 5231 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_mask_dcoeff_p
S 656 23 0 0 0 8 1179 624 5248 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_mask_dcoeff_t
S 657 23 0 0 0 8 1187 624 5265 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_mask_dcoeff_q
S 658 23 0 0 0 8 1195 624 5282 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_mask_dcoeff_u
S 659 23 0 0 0 8 1203 624 5299 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ofa_hdx
S 660 23 0 0 0 8 1211 624 5307 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ofa_hdy
S 661 23 0 0 0 8 1275 624 5315 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 crlat
S 662 23 0 0 0 8 1282 624 5321 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 acrlat
S 663 23 0 0 0 8 1571 624 5328 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 u
S 664 23 0 0 0 8 1580 624 5330 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 v
S 665 23 0 0 0 8 1589 624 5332 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 w
S 666 23 0 0 0 8 1598 624 5334 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t
S 667 23 0 0 0 8 1607 624 5336 4 0 A 0 0 0 0 B 0 139 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 pp
S 669 23 0 0 0 6 4803 624 5353 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 my_cart_id
S 670 23 0 0 0 6 4798 624 5364 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 num_compute
S 671 23 0 0 0 6 4800 624 5376 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nboundlines
S 672 23 0 0 0 6 4787 624 5388 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ldatatypes
S 673 23 0 0 0 6 4788 624 5399 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ltime_barrier
S 674 23 0 0 0 6 4801 624 5413 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ncomm_type
S 675 23 0 0 0 6 4805 624 5424 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 my_cart_neigh
S 676 23 0 0 0 6 4819 624 5438 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 icomm_cart
S 677 23 0 0 0 6 4821 624 5449 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iexch_req
S 678 23 0 0 0 6 4822 624 5459 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 imp_reals
S 679 23 0 0 0 6 4831 624 5469 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nexch_tag
S 680 23 0 0 0 8 4846 624 5479 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 sendbuf
S 681 23 0 0 0 6 4853 624 5487 4 0 A 0 0 0 0 B 0 171 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 isendbuflen
S 683 23 0 0 0 6 4890 624 5515 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ntstep
S 684 23 0 0 0 6 4893 624 5522 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnew
S 685 23 0 0 0 6 4892 624 5527 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 nnow
S 686 23 0 0 0 6 5059 624 5532 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lperi_x
S 687 23 0 0 0 6 5060 624 5540 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lperi_y
S 688 23 0 0 0 6 5061 624 5548 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 l2dim
S 689 23 0 0 0 6 5052 624 5554 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 ltime
S 690 23 0 0 0 6 5005 624 5560 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 l2tls
S 691 23 0 0 0 6 5041 624 5566 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 l_cosmo_art
S 692 23 0 0 0 6 5042 624 5578 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 l_pollen
S 693 23 0 0 0 8 5085 624 5587 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_corr_u_bd
S 694 23 0 0 0 8 5086 624 5600 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_corr_t_bd
S 695 23 0 0 0 8 5087 624 5613 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_corr_trcr_bd
S 696 23 0 0 0 8 5088 624 5629 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_corr_p_bd
S 697 23 0 0 0 8 5089 624 5642 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_corr_u_in
S 698 23 0 0 0 8 5090 624 5655 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_corr_t_in
S 699 23 0 0 0 8 5091 624 5668 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_corr_trcr_in
S 700 23 0 0 0 8 5092 624 5684 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_corr_p_in
S 701 23 0 0 0 8 5093 624 5697 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 hd_dhmax
S 702 23 0 0 0 6 5095 624 5706 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 l_diff_smag
S 703 23 0 0 0 6 5096 624 5718 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 l_diff_cold_pools
S 704 23 0 0 0 6 5194 624 5736 4 0 A 0 0 0 0 B 0 195 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 linit_fields
S 706 23 0 0 0 8 5228 624 5764 4 0 A 0 0 0 0 B 0 236 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 r_earth
S 707 23 0 0 0 8 5207 624 5772 4 0 A 0 0 0 0 B 0 236 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 r_d
S 709 23 0 0 0 8 5945 624 5788 4 0 A 0 0 0 0 B 0 245 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_diff_id
S 710 23 0 0 0 8 5947 624 5798 4 0 A 0 0 0 0 B 0 245 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_diff_on
S 711 23 0 0 0 8 5915 624 5808 4 0 A 0 0 0 0 B 0 245 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 t_missing
S 713 23 0 0 0 8 6902 624 5829 4 0 A 0 0 0 0 B 0 249 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 trcr_get_ntrcr
S 714 23 0 0 0 8 6707 624 5844 4 0 A 0 0 0 0 B 0 249 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 trcr_get
S 715 23 0 0 0 8 6715 624 5853 4 0 A 0 0 0 0 B 0 249 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 trcr_meta_get
S 716 23 0 0 0 8 7553 624 5867 4 0 A 0 0 0 0 B 0 249 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 trcr_errorstr
S 718 23 0 0 0 8 8160 624 5893 4 0 A 0 0 0 0 B 0 254 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 exchg_boundaries
S 719 23 0 0 0 8 8404 624 5910 4 0 A 0 0 0 0 B 0 254 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 comm_barrier
S 720 23 0 0 0 6 8087 624 5923 4 0 A 0 0 0 0 B 0 254 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 model_abort
S 722 23 0 0 0 8 8948 624 5950 4 0 A 0 0 0 0 B 0 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 get_timings
S 723 23 0 0 0 6 8805 624 5962 4 0 A 0 0 0 0 B 0 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 i_dyn_computations
S 724 23 0 0 0 6 8868 624 5981 4 0 A 0 0 0 0 B 0 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 i_barrier_waiting_dyn
S 725 23 0 0 0 6 8867 624 6003 4 0 A 0 0 0 0 B 0 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 i_communications_dyn
S 726 23 0 0 0 6 8806 624 6024 4 0 A 0 0 0 0 B 0 255 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 i_horizontal_diffusion
S 728 23 0 0 0 6 9068 624 6065 4 0 A 0 0 0 0 B 0 257 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lap_4a
S 729 23 0 0 0 6 9089 624 6072 4 0 A 0 0 0 0 B 0 257 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lap_4am
S 730 23 0 0 0 6 9112 624 6080 4 0 A 0 0 0 0 B 0 257 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lap_4aml
S 731 23 0 0 0 6 9030 624 6089 4 0 A 0 0 0 0 B 0 257 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 lap_2
S 733 23 0 0 0 6 10427 624 6114 4 0 A 0 0 0 0 B 0 258 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 i_global
S 734 23 0 0 0 6 10431 624 6123 4 0 A 0 0 0 0 B 0 258 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 j_global
R 741 16 3 mo_kind wp
R 752 16 4 data_parameters iintegers
S 766 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 7 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 768 3 0 0 0 16 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 16
R 775 6 7 data_modelconfig ie
R 776 6 8 data_modelconfig je
R 777 6 9 data_modelconfig ke
R 781 6 13 data_modelconfig ke1
R 807 6 39 data_modelconfig istart
R 808 6 40 data_modelconfig iend
R 809 6 41 data_modelconfig istartu
R 810 6 42 data_modelconfig iendu
R 811 6 43 data_modelconfig istartv
R 812 6 44 data_modelconfig iendv
R 815 6 47 data_modelconfig jstart
R 816 6 48 data_modelconfig jend
R 817 6 49 data_modelconfig jstartu
R 818 6 50 data_modelconfig jendu
R 819 6 51 data_modelconfig jstartv
R 820 6 52 data_modelconfig jendv
R 821 6 53 data_modelconfig jstartpar
R 822 6 54 data_modelconfig jendpar
R 834 6 66 data_modelconfig eddlon
R 835 6 67 data_modelconfig eddlat
R 844 6 76 data_modelconfig dt
R 923 7 3 data_fields rho0
R 939 7 19 data_fields p0
R 971 7 51 data_fields t0
R 979 7 59 data_fields hhl
R 1171 7 251 data_fields hd_mask_dcoeff_p
R 1179 7 259 data_fields hd_mask_dcoeff_t
R 1187 7 267 data_fields hd_mask_dcoeff_q
R 1195 7 275 data_fields hd_mask_dcoeff_u
R 1203 7 283 data_fields ofa_hdx
R 1211 7 291 data_fields ofa_hdy
R 1275 7 355 data_fields crlat
R 1282 7 362 data_fields acrlat
R 1571 7 651 data_fields u
R 1580 7 660 data_fields v
R 1589 7 669 data_fields w
R 1598 7 678 data_fields t
R 1607 7 687 data_fields pp
R 4787 6 3 data_parallel ldatatypes
R 4788 6 4 data_parallel ltime_barrier
R 4798 6 14 data_parallel num_compute
R 4800 6 16 data_parallel nboundlines
R 4801 6 17 data_parallel ncomm_type
R 4803 6 19 data_parallel my_cart_id
R 4805 7 21 data_parallel my_cart_neigh
R 4819 6 35 data_parallel icomm_cart
R 4821 7 37 data_parallel iexch_req
R 4822 6 38 data_parallel imp_reals
R 4831 6 47 data_parallel nexch_tag
R 4846 7 62 data_parallel sendbuf
R 4853 6 69 data_parallel isendbuflen
R 4890 6 6 data_runcontrol ntstep
R 4892 6 8 data_runcontrol nnow
R 4893 6 9 data_runcontrol nnew
R 5005 6 121 data_runcontrol l2tls
R 5041 6 157 data_runcontrol l_cosmo_art
R 5042 6 158 data_runcontrol l_pollen
R 5052 6 168 data_runcontrol ltime
R 5059 6 175 data_runcontrol lperi_x
R 5060 6 176 data_runcontrol lperi_y
R 5061 6 177 data_runcontrol l2dim
R 5085 6 201 data_runcontrol hd_corr_u_bd
R 5086 6 202 data_runcontrol hd_corr_t_bd
R 5087 6 203 data_runcontrol hd_corr_trcr_bd
R 5088 6 204 data_runcontrol hd_corr_p_bd
R 5089 6 205 data_runcontrol hd_corr_u_in
R 5090 6 206 data_runcontrol hd_corr_t_in
R 5091 6 207 data_runcontrol hd_corr_trcr_in
R 5092 6 208 data_runcontrol hd_corr_p_in
R 5093 6 209 data_runcontrol hd_dhmax
R 5095 6 211 data_runcontrol l_diff_smag
R 5096 6 212 data_runcontrol l_diff_cold_pools
R 5194 6 310 data_runcontrol linit_fields
R 5207 6 5 data_constants r_d
R 5228 6 26 data_constants r_earth
R 5268 25 6 iso_c_binding c_ptr
R 5269 5 7 iso_c_binding val c_ptr
R 5271 25 9 iso_c_binding c_funptr
R 5272 5 10 iso_c_binding val c_funptr
R 5306 6 44 iso_c_binding c_null_ptr$ac
R 5308 6 46 iso_c_binding c_null_funptr$ac
R 5309 26 47 iso_c_binding ==
R 5311 26 49 iso_c_binding !=
S 5343 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 6 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 5345 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 12 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 5346 3 0 0 0 3964 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 50816 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 55 4e 44 45 46 20 20 20 20 20 20 20
S 5347 3 0 0 0 3964 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 50829 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 49 4e 54 45 47 45 52 20 20 20 20 20
S 5348 3 0 0 0 3964 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 50842 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 52 45 41 4c 20 20 20 20 20 20 20 20
S 5349 3 0 0 0 3964 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 50855 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 44 4f 55 42 4c 45 20 20 20 20 20 20
S 5350 3 0 0 0 3964 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 50868 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 53 54 52 49 4e 47 20 20 20 20 20 20
S 5351 3 0 0 0 3964 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 50881 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 4c 4f 47 49 43 41 4c 20 20 20 20 20
S 5352 3 0 0 0 3964 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 50894 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 12 50 4f 49 4e 54 45 52 20 20 20 20 20
R 5379 7 27 data_tracer_metadata y_data_type$ac
R 5385 25 33 data_tracer_metadata t_metadata
R 5386 5 34 data_tracer_metadata lisready t_metadata
R 5387 5 35 data_tracer_metadata imaxkeys t_metadata
R 5388 5 36 data_tracer_metadata inumkey t_metadata
R 5389 5 37 data_tracer_metadata iunique t_metadata
R 5390 5 38 data_tracer_metadata iidx t_metadata
R 5392 5 40 data_tracer_metadata iidx$sd t_metadata
R 5393 5 41 data_tracer_metadata iidx$p t_metadata
R 5394 5 42 data_tracer_metadata iidx$o t_metadata
R 5396 5 44 data_tracer_metadata ykey t_metadata
R 5398 5 46 data_tracer_metadata ykey$sd t_metadata
R 5399 5 47 data_tracer_metadata ykey$p t_metadata
R 5400 5 48 data_tracer_metadata ykey$o t_metadata
R 5402 5 50 data_tracer_metadata iattr t_metadata
R 5404 5 52 data_tracer_metadata iattr$sd t_metadata
R 5405 5 53 data_tracer_metadata iattr$p t_metadata
R 5406 5 54 data_tracer_metadata iattr$o t_metadata
R 5408 5 56 data_tracer_metadata itype t_metadata
R 5410 5 58 data_tracer_metadata itype$sd t_metadata
R 5411 5 59 data_tracer_metadata itype$p t_metadata
R 5412 5 60 data_tracer_metadata itype$o t_metadata
R 5414 5 62 data_tracer_metadata ipos t_metadata
R 5416 5 64 data_tracer_metadata ipos$sd t_metadata
R 5417 5 65 data_tracer_metadata ipos$p t_metadata
R 5418 5 66 data_tracer_metadata ipos$o t_metadata
R 5421 5 69 data_tracer_metadata ilen t_metadata
R 5422 5 70 data_tracer_metadata ilen$sd t_metadata
R 5423 5 71 data_tracer_metadata ilen$p t_metadata
R 5424 5 72 data_tracer_metadata ilen$o t_metadata
R 5426 5 74 data_tracer_metadata imaxbuf t_metadata
R 5427 5 75 data_tracer_metadata inumbuf t_metadata
R 5428 5 76 data_tracer_metadata imaxbuflen t_metadata
R 5429 5 77 data_tracer_metadata ydefault t_metadata
R 5431 5 79 data_tracer_metadata ydefault$sd t_metadata
R 5432 5 80 data_tracer_metadata ydefault$p t_metadata
R 5433 5 81 data_tracer_metadata ydefault$o t_metadata
R 5435 5 83 data_tracer_metadata ybuf t_metadata
R 5438 5 86 data_tracer_metadata ybuf$sd t_metadata
R 5439 5 87 data_tracer_metadata ybuf$p t_metadata
R 5440 5 88 data_tracer_metadata ybuf$o t_metadata
R 5915 16 16 data_tracer t_missing
R 5945 6 46 data_tracer t_diff_id
R 5947 16 48 data_tracer t_diff_on
R 6707 19 52 src_tracer trcr_get
R 6715 19 60 src_tracer trcr_meta_get
R 6902 14 247 src_tracer trcr_get_ntrcr
R 7553 14 898 src_tracer trcr_errorstr
R 8087 14 389 environment model_abort
R 8160 14 462 environment exchg_boundaries
R 8404 14 706 environment comm_barrier
R 8805 6 311 time_utilities i_dyn_computations
R 8806 6 312 time_utilities i_horizontal_diffusion
R 8867 6 373 time_utilities i_communications_dyn
R 8868 6 374 time_utilities i_barrier_waiting_dyn
R 8948 14 454 time_utilities get_timings
R 9030 14 80 numeric_utilities lap_2
R 9068 14 118 numeric_utilities lap_4a
R 9089 14 139 numeric_utilities lap_4am
R 9112 14 162 numeric_utilities lap_4aml
R 10427 14 736 parallel_utilities i_global
R 10431 14 740 parallel_utilities j_global
S 10696 23 5 0 0 0 10698 624 76794 0 0 A 0 0 0 0 B 0 757 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 comp_hori_diff
S 10697 1 3 1 0 6 1 10696 51523 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 itype
S 10698 14 5 0 0 0 1 10696 76794 0 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1944 1 0 0 0 0 0 0 0 0 0 0 0 0 272 0 624 0 0 0 0 comp_hori_diff
F 10698 1 10697
S 10699 23 5 0 0 0 10707 624 76809 0 0 A 0 0 0 0 B 0 962 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 smagorinsky_coeff
S 10700 7 3 2 0 5301 1 10699 76827 800204 3000 A 0 0 0 0 B 0 962 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 k_diff_smag_u
S 10701 7 3 2 0 5304 1 10699 76841 800204 3000 A 0 0 0 0 B 0 962 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 k_diff_smag_v
S 10702 7 3 1 0 5292 1 10699 76855 800204 3000 A 0 0 0 0 B 0 962 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 hd_mask_dcoeff_v
S 10703 7 3 1 0 5289 1 10699 5282 800204 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 hd_mask_dcoeff_u
S 10704 7 3 1 0 5295 1 10699 5328 800204 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 u
S 10705 7 3 1 0 5298 1 10699 5330 800204 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 v
S 10706 1 3 1 0 9 1 10699 5201 4 3000 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 dt
S 10707 14 5 0 0 0 1 10699 76809 200 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1946 7 0 0 0 0 0 0 0 0 0 0 0 0 761 0 624 0 0 0 0 smagorinsky_coeff
F 10707 7 10700 10701 10703 10702 10704 10705 10706
S 10708 6 1 0 0 6 1 10699 76872 40800006 3000 A 0 0 0 0 B 0 962 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_11565
S 10709 6 1 0 0 6 1 10699 76882 40800006 3000 A 0 0 0 0 B 0 962 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_11567
S 10710 6 1 0 0 6 1 10699 76892 40800006 3000 A 0 0 0 0 B 0 962 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_12162
S 10711 6 1 0 0 6 1 10699 76902 40800006 3000 A 0 0 0 0 B 0 962 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_12159
S 10712 6 1 0 0 6 1 10699 76912 40800006 3000 A 0 0 0 0 B 0 962 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_12165
S 10713 6 1 0 0 6 1 10699 76922 40800006 3000 A 0 0 0 0 B 0 962 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_12168
S 10714 23 5 0 0 0 10718 624 76932 0 0 A 0 0 0 0 B 0 1141 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 targeted_diffusion_cold_pools
S 10715 7 3 3 0 5307 1 10714 76962 800204 3000 A 0 0 0 0 B 0 1141 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 tp
S 10716 1 3 1 0 9 1 10714 76965 4 3000 A 0 0 0 0 B 0 1141 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 t_threshold
S 10717 1 3 1 0 6 1 10714 76977 4 3000 A 0 0 0 0 B 0 1141 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 dk
S 10718 14 5 0 0 0 1 10714 76932 200 400000 A 0 0 0 0 B 0 0 0 0 0 0 0 1954 3 0 0 0 0 0 0 0 0 0 0 0 0 966 0 624 0 0 0 0 targeted_diffusion_cold_pools
F 10718 3 10715 10716 10717
S 10719 6 1 0 0 6 1 10714 76872 40800006 3000 A 0 0 0 0 B 0 1141 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_11565
S 10720 6 1 0 0 6 1 10714 76882 40800006 3000 A 0 0 0 0 B 0 1141 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_11567
S 10721 6 1 0 0 6 1 10714 76922 40800006 3000 A 0 0 0 0 B 0 1141 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_12168
S 10722 6 1 0 0 6 1 10714 76892 40800006 3000 A 0 0 0 0 B 0 1141 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_12162
S 10723 6 1 0 0 6 1 10714 76980 40800006 3000 A 0 0 0 0 B 0 1141 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_12171
S 10724 6 1 0 0 6 1 10714 76990 40800006 3000 A 0 0 0 0 B 0 1141 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_e_12174
A 44 2 0 0 0 6 766 0 0 0 44 0 0 0 0 0 0 0 0 0 0 0
A 94 2 0 0 0 16 768 0 0 0 94 0 0 0 0 0 0 0 0 0 0 0
A 8397 1 0 0 4015 3260 5306 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 8400 1 0 0 7040 3269 5308 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 8401 2 0 0 5360 6 5345 0 0 0 8401 0 0 0 0 0 0 0 0 0 0 0
A 8419 2 0 0 7891 6 5343 0 0 0 8419 0 0 0 0 0 0 0 0 0 0 0
A 8430 2 0 0 8356 3290 5346 0 0 0 8430 0 0 0 0 0 0 0 0 0 0 0
A 8431 2 0 0 8357 3290 5347 0 0 0 8431 0 0 0 0 0 0 0 0 0 0 0
A 8432 2 0 0 8358 3290 5348 0 0 0 8432 0 0 0 0 0 0 0 0 0 0 0
A 8433 2 0 0 8359 3290 5349 0 0 0 8433 0 0 0 0 0 0 0 0 0 0 0
A 8434 2 0 0 8360 3290 5350 0 0 0 8434 0 0 0 0 0 0 0 0 0 0 0
A 8435 2 0 0 8361 3290 5351 0 0 0 8435 0 0 0 0 0 0 0 0 0 0 0
A 8436 2 0 0 8362 3290 5352 0 0 0 8436 0 0 0 0 0 0 0 0 0 0 0
A 8522 1 0 9 8376 3292 5379 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 11565 1 0 0 11392 6 775 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 11567 1 0 0 11395 6 776 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12159 1 0 0 11944 6 10713 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12160 1 0 0 11302 6 10708 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12161 1 0 0 11312 6 10709 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12162 1 0 0 11996 6 777 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12163 1 0 0 11313 6 10710 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12164 1 0 0 11115 6 10711 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12165 1 0 0 11648 6 10712 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12166 1 0 0 11660 6 10724 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12167 1 0 0 11655 6 10719 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12168 1 0 0 11949 6 10720 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12169 1 0 0 11952 6 10721 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12170 1 0 0 11658 6 10722 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
A 12171 1 0 0 11659 6 10723 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
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
V 8522 3292 7 0
R 0 3295 0 0
A 0 3290 0 0 1 8430 1
A 0 3290 0 0 1 8431 1
A 0 3290 0 0 1 8432 1
A 0 3290 0 0 1 8433 1
A 0 3290 0 0 1 8434 1
A 0 3290 0 0 1 8435 1
A 0 3290 0 0 1 8436 0
T 5385 3310 0 3 0 0
A 5386 16 0 0 1 94 0
Z
