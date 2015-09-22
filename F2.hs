-- Oskar Casselryd
-- Trolle Geuna

module F2 where
import Data.List

data Typ = PROTEIN | DNA deriving(Show,Eq)

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
type Matris = [[(Char, Int)]]
data Profile = Profile { m :: Matris, mTyp :: Typ, antalSekvenser :: Int, namn :: String }deriving(Show)



--TODO: se kommentarerna i funktionen
-- Är våran Matrix fel???
-- Kanske finns ett snyggare/bättre sätt

molseqs2profile:: String -> [MolSeq] -> Profile
molseqs2profile a b = Profile m mTyp antalSekvenser namn
  where
    m = makeProfileMatrix b
    mTyp = seqType (head b)
    antalSekvenser = length b
    namn = a
 -- 	mTyp =  TODO: fixa en returntyp till molSeq typen
 --		anal (går det att använda length bara enkelt?)
 --		namn 


nucleotides = "ACGT"
aminoacids = sort "ARNDCEQGHILKMFPSTWYVX"

makeProfileMatrix :: [MolSeq] -> Matris
makeProfileMatrix [] = error "Empty sequence list"
makeProfileMatrix sl = res
  where 
    t = seqType (head sl)
    defaults = 
      if (t == DNA) then
      	-- skapar en lista utav tupler [(A,0),(B,2),.....]
        zip nucleotides (replicate (length nucleotides) 0) -- Rad (i)
      else 
      	-- samma som rad i fast med aminosyror
        zip aminoacids (replicate (length aminoacids) 0)   -- Rad (ii)
    --
    strs = map seqSequence sl                              -- Rad (iii)
    tmp1 = map (map (\x -> ((head x), (length x))) . group . sort)
               (transpose strs)                            -- Rad (iv)
    equalFst a b = (fst a) == (fst b)
    res = map sort (map (\l -> unionBy equalFst l defaults) tmp1)




profileName :: Profile -> String
profileName n = namn n



-- Eftersom våran matris är [[(Char, Int)]]
-- Bör vi väll plocka ut en [(Char, Int)] på den position vi vill undersöka
-- Sedan hämta ut det värde för rätt Char
profileFrequency :: Profile -> Int -> Char -> Double
profileFrequency (Profile m _ antalSekvenser _) position tecken = fromIntegral number / fromIntegral antalSekvenser
  where
    number = helpprofileFrequency (m !! position) tecken

-- Tecknet _MÅSTE_ finnas i sekvensen för att denna funktion ska fungera.
helpprofileFrequency :: [(Char, Int)] -> Char -> Int
helpprofileFrequency (huvud: svans) tecken
  | tecken == fst huvud = snd huvud
  | otherwise = helpprofileFrequency svans tecken




-- ANVÄND PROFILEFREQUENCY! Se till att det är rätt matriser som jämförs! Det ska vara doubles, inte int!

-- Plocka ut matriserna från profilerna och kalla hjälpfunktion.
profileDistance :: Profile -> Profile -> Double
profileDistance (Profile m1 _ _ _) (Profile m2 _ _ _) = helpDistance m1 m2

-- Kör igenom listorna i matrisen. Alltså de olika teckenpositionerna.
helpDistance :: Matris -> Matris -> Double
helpDistance (h1:t1) (h2:t2)
  | h1 == [] = 0
  | otherwise = abs (helpDistance2 h1 h2) + (helpDistance t1 t2) -- Kalla hd2

-- Jämför varje tuple som utgör matrisen. Dvs hur många av varje tecken som finns på positionen.
helpDistance2 :: [(Char, Int)] -> [(Char, Int)] -> Double
helpDistance2 (h1:t1) (h2:t2) = abs((snd h1) - (snd h2)) + helpDistance2 t1 t2



class Evol a where
	distance :: a -> a -> Double
	name :: a -> String 
	distanceMatrix :: [a] -> [(String, String, Double)]
	distanceMatrix a = helpDistanceMatrix a 0 ++ distanceMatrix (tail a)
	helpDistanceMatrix :: [a] -> Int ->[(String, String, Double)]
	helpDistanceMatrix a nummer
	  |nummer < length a = (name ett , name tva, distance ett tva) : helpDistanceMatrix a (nummer+ 1)
	  |otherwise = []
	  where
	  	ett = head a
	  	tva = a !! nummer


    





instance Evol MolSeq where
	name = seqName
	distance = seqDistance

instance Evol Profile where
	name = profileName
	distance = profileDistance



