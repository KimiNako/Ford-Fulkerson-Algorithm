Commands : 

BUILD : 
$ make   

EXECUTE & TEST (step by step):
$ ./ftest.byte graph_Minimum 0 5 TEST
$ make dot_ftest

$ ./ff_algorithm_test.byte graph_Minimum 0 5 TEST
$ make dot_ff_algo_test

$ ./bipartite_matching_test.byte graph_Medium TEST
$ make dot_bp_test



EXECUTE & TEST
$ ./ftest.byte graph_Minimum 0 5 TEST
$ ./ff_algorithm_test.byte graph_Minimum 0 5 TEST
$ ./bipartite_matching_test.byte graph_Medium TEST

CONVERT ALL GRAPHS IN DOT FILE :
make dot


CLEAR TESTS repository:
make clear
