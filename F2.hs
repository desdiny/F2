-- Oskar Casselryd
-- Trolle Geuna

module F2 where

data Typ = PROTEIN | DNA deriving(Show)

data MolSeq = MolSeq { sekvensnamn :: String, sekvens :: String, typ :: Typ}deriving(Show)





dna =['A','C','G','T']

string2seq :: String->String->MolSeq
string2seq namn sekvens
  | checkDNA sekvens = MolSeq namn sekvens DNA
  | otherwise = MolSeq namn sekvens PROTEIN

checkDNA :: String -> Bool
checkDNA [] = True
checkDNA(h:t)
  | elem h dna = checkDNA(t)
  | otherwise = False




seqName :: MolSeq -> String
seqName m = sekvensnamn m

seqSequence :: MolSeq -> String
seqSequence m = sekvens m

seqLength :: MolSeq -> Int
seqLength m = length (sekvens m)




seqDistance :: MolSeq -> MolSeq -> Double
seqDistance n m
  | checkDNA (sekvens n) /= checkDNA (sekvens m) = error "Can't compare DNA and PROTEIN"
  | checkDNA (sekvens n) == True  = 1.0 -- kalla funktionen för dna här
  | otherwise = 2.0 -- kalla funktionen för protein här



--jukes-Cantor
--da,b=−(3/4)ln(1−4α/3)
jukes-Cantor:: Double -> Double
jukes-Cantor a
  | if a > 0.74 = 3.3
  | otherwise = -(3/4)log(1-((4a)/3)

--da,b=−(19/20)ln(1−20α/19)




seqDiff :: String -> String ->Int
seqDiff [] [] = 0
seqDiff (a,taila) (b,tailb)
  | if a ==  b = 0 + seqDiff(taila,tailb)





