module F2 where

data Typ = PROTEIN | DNA deriving(Show)

data MolSeq = MolSeq { sekvensnamn :: String, sekvens :: String, typ :: Typ}deriving(Show)





dna =['A','C','G','T']

string2seq:: String->String->MolSeq
string2seq namn sekvens
	| checkDNA sekvens = MolSeq namn sekvens DNA
	| otherwise = MolSeq namn sekvens PROTEIN

checkDNA:: String -> Bool
checkDNA[h:t]
	| [] = True
	| elem h dna =  checkDNA(t)
 	| otherwise = False




seqName :: MolSeq -> String
seqName m = sekvensnamn m

seqSequence :: MolSeq -> String
seqName m = sekvens m

seqLength :: Molseq -> Typ
seqLength m = typ m