V33 :0x4 data_parameters
62 /users/dedekind/code/yves_gesa_inp_ccn/src/data_parameters.f90 S624 0
07/07/2019  22:41:28
use mo_kind private
enduse
S 624 24 0 0 0 8 1 0 5015 5 0 A 0 0 0 0 B 0 4 0 0 0 0 0 0 0 0 0 0 194 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 data_parameters
S 626 23 0 0 0 8 634 624 5039 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 wp
S 627 23 0 0 0 8 632 624 5042 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 dp
S 628 23 0 0 0 8 633 624 5045 4 0 A 0 0 0 0 B 0 57 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 sp
S 629 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 8 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 630 3 0 0 0 6 1 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 0 4 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
R 632 16 1 mo_kind dp
R 633 16 2 mo_kind sp
R 634 16 3 mo_kind wp
S 640 16 0 0 0 6 641 624 5063 800004 400000 A 0 0 0 0 B 0 90 0 0 0 0 0 0 4 15 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iintegers
S 641 16 0 0 0 6 642 624 5073 4 400000 A 0 0 0 0 B 0 99 0 0 0 0 0 0 4 15 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 intgribf
S 642 16 0 0 0 6 643 624 5082 4 400000 A 0 0 0 0 B 0 99 0 0 0 0 0 0 4 15 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 intgribc
S 643 16 0 0 0 6 645 624 5091 800004 400000 A 0 0 0 0 B 0 99 0 0 0 0 0 0 4 15 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 irealgrib
S 644 6 4 0 0 6 1 624 5101 4 0 A 0 0 0 0 B 0 116 0 0 0 0 0 0 0 0 0 0 658 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 iwlength
S 645 16 0 0 0 6 647 624 5110 4 400000 A 0 0 0 0 B 0 145 0 0 0 0 0 0 8 13 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 int_dp
S 647 16 0 0 0 6 648 624 5117 800004 400000 A 0 0 0 0 B 0 145 0 0 0 0 0 0 4 15 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 int_ga
S 648 16 0 0 0 9 652 624 5124 4 400000 A 0 0 0 0 B 0 161 0 0 0 0 0 0 651 37 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 repsilon
S 651 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 28825476 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 652 16 0 0 0 9 0 624 5133 800004 400000 A 0 0 0 0 B 0 161 0 0 0 0 0 0 657 42 0 0 0 0 0 0 0 0 0 0 0 0 0 624 0 0 0 0 rprecision
S 657 3 0 0 0 9 0 1 0 0 0 A 0 0 0 0 B 0 0 0 0 0 0 0 0 1020396463 -1629006314 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9
S 658 11 0 0 0 8 1 624 5144 40800000 805000 A 0 0 0 0 B 0 194 0 0 0 4 0 0 644 644 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 _data_parameters$0
A 13 2 0 0 0 6 629 0 0 0 13 0 0 0 0 0 0 0 0 0 0 0
A 15 2 0 0 0 6 630 0 0 0 15 0 0 0 0 0 0 0 0 0 0 0
A 37 2 0 0 0 9 651 0 0 0 37 0 0 0 0 0 0 0 0 0 0 0
A 42 2 0 0 0 9 657 0 0 0 42 0 0 0 0 0 0 0 0 0 0 0
Z
Z
