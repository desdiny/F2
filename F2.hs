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
  | checkDNA n == checkDNA m = error "Can't compare DNA and PROTEIN"
  | otherwise = 5.5
