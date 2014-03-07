-module(dna2).

-export([test/0,count_nucleotides/1]).

test() ->
	Dataset = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC",
	"20 12 17 21" == count_nucleotides(Dataset).

count_nucleotides(Dataset) -> 
	OrdDict = lists:foldl(
		fun (X, Dict) -> orddict:update_counter(X,1,Dict) end,
		orddict:new(), Dataset),
	[{$A, A},{$C, C}, {$G, G}, {$T, T}] = orddict:to_list(OrdDict),
	lists:concat([A," ",C," ",G," ",T]).
