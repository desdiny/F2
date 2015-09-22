-- Oskar Casselryd
-- Trolle Geuna

module F2 where
import Data.List
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

seqType :: MolSeq -> Typ
seqType m = typ m



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
-- ToDO: Fixa dataprofilen
type Matrix = [[Int]]
data Profile = Profile { m :: Matrix, mTyp :: Typ, antalSekvenser :: Int, namn :: String }deriving(Show)


--TODO: se kommentarerna i funktionen
-- Är våran Matrix fel???
-- Kanske finns ett snyggare/bättre sätt

molseqs2profile:: String -> [MolSeq] -> Profile
molseqs2profile a b = Profile m mTyp antalSekvenser namn
  where
  	matrix = makeProfileMatrix b
  	mTyp = seqType (head b)
  	antalSekvenser = length b
  	namn = a
 -- 	mTyp =  TODO: fixa en returntyp till molSeq typen
 --		anal (går det att använda length bara enkelt?)
 --		namn 


nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYVX"



-- denna måste vi kommentera så vi förstår den
makeProfileMatrix :: [MolSeq] -> Matrix
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res
  where 
    t = seqType (head sl)
    defaults = 
      if (t == DNA) then
        zip nucleotides (replicate (length nucleotides) 0) -- Rad (i)
      else 
        zip aminoacids (replicate (length aminoacids) 0)   -- Rad (ii)
    strs = map seqSequence sl                              -- Rad (iii)
    tmp1 = map (map (\x -> ((head x), (length x))) . group . sort)
               (transpose strs)                            -- Rad (iv)
    equalFst a b = (fst a) == (fst b)
    res = map sort (map (\l -> unionBy equalFst l defaults) tmp1)






profileName :: Profile -> String
profileName n = namn n


-- profileFrequency :: Profile -> Int -> Char -> Double




-- profileDistance :: Profile -> Profile -> Double




