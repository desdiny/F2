module F2 where

--data MolSeq = DNA String String | PROTEIN String String


data MolSeq = MolSeq { sekvensnamn :: String, sekvens :: String, typ :: String}deriving(Show)

data Typ = PROTEIN | DNA deriving(Show)





String->String->Bool
| if dna = MolSeq DNA a b
| otherwise = MolSeq PROTEIN a b





