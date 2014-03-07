-module(dna).
-export([test/0,count_nucleotides/1]).

test() ->
	Dataset = "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC",
	"20 12 17 21" == count_nucleotides(Dataset).

count_nucleotides(Dna) -> 
	{A, C, G, T} = count(Dna, {0,0,0,0}),
	lists:concat([A, " ", C, " ", G, " ", T]).

%% - Private

count([Head|Tail], Counts) ->
	count(Tail, count_helper(Head, Counts));
count([], Result) ->
	Result.

count_helper($A, {A,C,G,T}) ->
	{A+1,C,G,T};
count_helper($C, {A,C,G,T}) ->
	{A,C+1,G,T};
count_helper($G, {A,C,G,T}) ->
	{A,C,G+1,T};
count_helper($T, {A,C,G,T}) ->
	{A,C,G,T+1}.