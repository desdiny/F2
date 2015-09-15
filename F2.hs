module F2 where

--data MolSeq = DNA String String | PROTEIN String String


data MolSeq = MolSeq { sekvensnamn :: String, sekvens :: String, typ :: String}deriving(Show)

data Typ = PROTEIN | DNA deriving(Show)




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






