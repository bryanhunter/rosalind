-module(rna).

-export([test/0, dna_to_rna/1]).

test() ->
	Dna = "GATGGAACTTGACTACGTAAATT",
	"GAUGGAACUUGACUACGUAAAUU" == dna_to_rna(Dna).

dna_to_rna(Dna) ->
	dna_to_rna_helper(Dna, []).

%% - Private

dna_to_rna_helper([], Rna) ->
	lists:reverse(Rna);
dna_to_rna_helper([$T|DnaTail], Rna) -> 
	dna_to_rna_helper(DnaTail, [$U|Rna]);
dna_to_rna_helper([DnaHead|DnaTail], Rna) -> 
	dna_to_rna_helper(DnaTail, [DnaHead|Rna]).