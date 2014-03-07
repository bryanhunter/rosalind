-module(revc).

-export([test/0, reverse_complement/1]).

test() ->
	Dna = "AAAACCCGGT",
	"ACCGGGTTTT" == reverse_complement(Dna).

reverse_complement(Dna) ->
	revc_helper(Dna, []).

%% - Private

%% In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'.

revc_helper([], Acc) ->
	Acc;
revc_helper([Head|Tail], Acc) ->
	Complement = case Head of 
		$A -> $T;
		$T -> $A;
		$C -> $G;
		$G -> $C
	end,
	revc_helper(Tail, [Complement|Acc]).
