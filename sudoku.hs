import Prelude hiding ((||),(&&),not,and,or)
import qualified Data.ByteString.Lazy as BL (writeFile)
import Data.Char
import Data.Array
import Data.Functor.Identity
import Data.Word
import Control.Monad
import Control.Monad.State
import Ersatz

fieldRange :: ((Int, Int),(Int, Int))
fieldRange = ((1,1),(9,9))

unique :: Equatable a => [a] -> Bit
unique = go [] where
  go ss (x:xs) = and (map (/==x) xs) && go (x:ss) xs
  go _ [] = true

type Board = Array (Int, Int)

sudoku :: (MonadState s m, HasSAT s) => Board Char -> Board Bit4 -> m (Board Bit4)
sudoku problem answer = do
  forM_ (range fieldRange) $ \ix ->
    assert $ if isDigit (problem ! ix)
             then answer ! ix === encode (toEnum $ digitToInt $ problem ! ix)
             else let Bit4 b3 b2 b1 b0 = answer ! ix
                  in or [ b3 && not b2 && not b1 -- 8,9
                        , not b3 && (b2 || b1 || b0) -- excepting 0
                        ]
  -- unique for row
  forM_ [1..9] $ \r -> assert $ unique [answer ! (c, r) | c <- [1..9]]
  -- unique for col
  forM_ [1..9] $ \c -> assert $ unique [answer ! (c, r) | r <- [1..9]]
  -- unique for blk
  forM_ (range (((0,0),(2,2))::((Int, Int),(Int, Int)))) $ \(sc, sr) ->
    assert $ unique [answer ! (sc * 3 + c, sr * 3 + r) | c <- [1..3], r <- [1..3]]

  return answer

showField :: (Result, Maybe (Board Word8)) -> IO ()
showField (_, Just answer) = forM_ [1..9] $ \c -> do
  forM_ [1..9] $ \r -> putChar $ intToDigit $ fromEnum $ answer ! (c, r)
  putChar '\n'
showField _ = fail "Impossible"

problem1 :: [[Char]]
problem1 = [ "-9---3---"
           , "-7----8--"
           , "---5--2--"
           , "5--------"
           , "-----7-91"
           , "2---4----"
           , "----6-4--"
           , "6--2-----"
           , "-------7-"
           ]

main :: IO ()
main = do
  forM_ problem1 putStrLn
  let p = array fieldRange $ zip (range ((1,1),(9,9))) (concat problem1)
  let s = replicateM 81 exists >>= sudoku p . listArray fieldRange
  let s' = mapStateT (return . runIdentity) s
  liftIO $ BL.writeFile "out.cnf" $ dimacsSAT s
  ans <- cryptominisat `solveWith` s'
  showField ans
