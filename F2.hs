-- Oskar Casselryd
-- Trolle Geuna

module F2 where

data Typ = PROTEIN | DNA deriving(Show)

data MolSeq = MolSeq { sekvensnamn :: String, sekvens :: String, typ :: Typ }deriving(Show)



-- UPPGIFT 2

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
  | checkDNA (seqSequence n) == True  = jukesCantor(fromIntegral(seqDiff(seqSequence n) (seqSequence m)) / fromIntegral(seqLength n)) -- kalla funktionen för dna här
  | otherwise = poissonModellforProtein(fromIntegral(seqDiff(seqSequence n) (seqSequence m)) / fromIntegral(seqLength n)) -- kalla funktionen för protein här



jukesCantor:: Double -> Double
jukesCantor a
  | a > 0.74 = 3.3
  | otherwise = -(3/4)*log(1-((4*a)/3))


poissonModellforProtein:: Double -> Double
poissonModellforProtein a
  | a <= 0.94 = -(19/20)*log(1-((20*a)/19))
  | otherwise = 3.7


seqDiff :: String -> String -> Int
seqDiff [] [] = 0
seqDiff a b
  | head a == head b = 0 + seqDiff (tail a) (tail b)
  | otherwise = 1 + seqDiff (tail a) (tail b)



-- UPPGIFT 3

data Profile = Profile { m :: Matrix, mTyp :: Typ, antalSekvenser :: Int, namn :: String }deriving(Show)



