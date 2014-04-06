// Rosalind #5: Mendel's First Law

let (/.) x y = float x / float y

type Allele = 
    | Dominant
    | Recessive 

let homozygous allele = [ allele; allele ]
let heterozygous = [ Dominant; Recessive ]

let offsprings alleles1 alleles2 =
    [ for allele1 in alleles1 do
      for allele2 in alleles2 do
          yield [ allele1; allele2 ] ]

let offspringProbs populations = 
    [ let totalPop = populations |> Seq.sumBy snd
      for alleles1, pop1 in populations do
      for alleles2, pop2 in populations do
      for offspring in offsprings alleles1 alleles2 do
          let pop2 = if alleles1 = alleles2 then pop2 - 1 else pop2
          let prob = (pop1 /. totalPop) * (pop2 /. (totalPop - 1)) / 4.
          yield offspring, prob ]
    
let dominantProb populations =
    offspringProbs populations
    |> Seq.filter (fst >> Seq.exists ((=) Dominant))
    |> Seq.sumBy snd

dominantProb [ homozygous Dominant, 2; 
               heterozygous, 2; 
               homozygous Recessive, 2 ]
