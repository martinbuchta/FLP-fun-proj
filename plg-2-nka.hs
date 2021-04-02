import System.IO
import System.Environment
import Data.List.Split ( splitOn )
import qualified Control.Monad
import Control.Monad (unless)

-- struktura reprezentujici gramatiku
data Grammar = Grammar {
    nonterminals :: [String], -- neterminaly
    terminals :: [String], -- terminaly
    productionRules :: [(String, String)], -- pravidla
    startSymbol :: String -- pocatecni symbol
} deriving (Show)

-- struktura reprezentujici nedeterministicky konecny automat
data Machine = Machine {
    alphabet :: [String], -- vstupni abeceda
    states :: [String], -- mnozina stavu
    initialState :: String, -- pocatecni stav
    transitionFunction :: [( (String, String), [String] )], -- prechodova funkce
    finalStates :: [String] -- koncove stavy
} deriving (Show)


main :: IO()
main = do
    args <- getArgs
    let inputFileName = last args
    let flags = filter (\a -> a == "-i" || a == "-1" || a == "-2") args

    -- kontrola neznamych argumentu
    unless (all (\a -> a == "-i" || a == "-1" || a == "-2" || a == inputFileName) args) $ error "Nektery z argumentu neznam"

    -- kontrola, ze je aspon jeden argument zadany
    Control.Monad.when (null flags) $ error "Prosím, dej mi alespoň jeden přepínač z množiny {\"-i\", \"-1\", \"-2\"}."

    -- precti vstup z stdin nebo ze souboru
    input <- if inputFileName /= "-i" && inputFileName /= "-1" && inputFileName /= "-2"
        then readFile inputFileName
        else getContents

    let inputLines = splitOn "\n" input
    let nonterminals = splitOn "," $ head inputLines
    print inputLines
