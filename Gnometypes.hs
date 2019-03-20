import Data.List
import System.Environment
import System.Random

type Gene = (Char, Char)
type Genome = [Gene]

main :: IO ()
main = do
  args <- getArgs

  let mode = args !! 0
  let n = read (args !! 1) :: Int
  let path = args !! 2

  finalPopulation <- breedFile path n

  let output = case mode of
                 "normal" -> showPopulation finalPopulation
                 "ratios" -> showRatios $ ofEach finalPopulation
                 _        -> "That is not a valid mode"
    
  putStrLn output


ofEach population = fmap (fmap (\g -> (ratioOf $ length g, head g)) . group . sort) $ transpose population
  where
    populationSize = length population
    ratioOf :: Int -> Double
    ratioOf x = fromIntegral x / fromIntegral populationSize

genotypesFromFile path = fmap (fmap genotypify) (readFile path >>= return . lines)

breedFile path n = fmap (perform n breedPopulation) genotypesFromFile path

genotypify :: String -> Genome
genotypify [] = []
genotypify (c1:c2:s) = (c1,c2) : genotypify s

choosePartner :: [a] -> IO (Maybe a, [a])
choosePartner [] = return (Nothing, [])
choosePartner (x:xs) = do
  r <- randomRIO (1,2) :: IO Int
  case r of 
    1 -> return (Just x,xs)
    2 -> choosePartner xs >>= (return . fmap (x:))

breedPopulation :: [Genome] -> IO [Genome]
breedPopulation [] = return []
breedPopulation (x:xs) = do
  (partner, xp) <- choosePartner xs
  case partner of
    Just p -> do
      baby1 <- breed x p
      baby2 <- breed x p
      other <- breedPopulation xp
      return $ baby1 : baby2 : other
    Nothing -> do
      baby1 <- breed x x
      other <- breedPopulation xp
      return $ baby1  : other

breed :: [Gene] -> [Gene] -> IO [Gene]
breed g1 g2 = sequence $ zipWith breedGenes g1 g2

breedGenes :: Gene -> Gene -> IO Gene
breedGenes g1 g2 = do
    r1 <- randomRIO (1,2) :: IO Int
    r2 <- randomRIO (1,2) :: IO Int

    return $ sortGene (randomFunc r1 g1, randomFunc r2 g2)

randomFunc :: Int -> (a,a) -> a
randomFunc 1 = fst
randomFunc 2 = snd

sortGene g@(x1, x2)
  | x2 < x1 = (x2, x1)
  | otherwise = g

perform :: (Monad m) => Int -> (a -> m a) -> m a -> m a
perform 0 _ m = m
perform n f m = perform (n-1) f (m >>= f)

showPopulation :: [Genome] -> String
showPopulation = unlines . fmap (concat . fmap mergePair)

showRatios :: [[(Double, Gene)]] -> String
showRatios = unlines . intercalate [" "] . fmap (fmap (showRatio . fmap mergePair))

showRatio :: (Double, String) -> String
showRatio (n, g) = g ++ " - " ++ show n

mergePair :: (a, a) -> [a]
mergePair (c1, c2) = [c1, c2]
