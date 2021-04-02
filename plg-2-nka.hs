import System.IO
import System.Environment
import Data.List.Split ( splitOn )
import qualified Control.Monad
import Control.Monad (unless)
import Data.List (nub)

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

    -- parsuj vstup a napln strukturu Grammar
    let nonterminals = nub $ splitOn "," $ head inputLines
    let terminals = nub $ splitOn "," $ inputLines !! 1
    let startSymbol = inputLines !! 2
    let productionRulesLinesAll = tail $ tail $ tail inputLines

    let productionRulesLines = if last productionRulesLinesAll == ""
        then  init productionRulesLinesAll
        else productionRulesLinesAll

    let productionRules = map (splitOn "->") productionRulesLines
    let productionRulesTuples = map (\a -> (head a, a !! 1)) productionRules

    let rlg = Grammar nonterminals terminals productionRulesTuples startSymbol

    -- validuj neterminaly
    let nonterminalsCharsValidated = validateNonterminalsChars nonterminals
    let nonterminalsLength = validateNonterminalsLength nonterminals
    Control.Monad.when (not nonterminalsCharsValidated || not nonterminalsLength) $ error "Neterminály nejsou validni"

    -- validuj terminaly
    let terminalsLength = validateNonterminalsLength terminals
    let terminalsChards = validateTerminalsChars terminals
    Control.Monad.when (not terminalsLength || not terminalsChards) $ error "Terminály nejsou validni"

    -- validuj pocatecni neterminal
    Control.Monad.when (startSymbol `notElem` nonterminals) $ error "Počáteční symbol musí náležet množině neterminálů!"


    print rlg

validateNonterminalsChars :: [String] -> Bool
validateNonterminalsChars[] = True
validateNonterminalsChars(x:xs) = if head x < 'A' || head x > 'Z' then error ("Neterminály musí být [A-Z], toto nalezeno: " ++ x) else validateNonterminalsChars xs

validateNonterminalsLength :: [String] -> Bool
validateNonterminalsLength[] = True
validateNonterminalsLength(x:xs) = if length x > 1 then error ("Neterminály i terminály musí být dlouhé 1 znak, toto nalezeno: " ++ x) else validateNonterminalsLength xs

validateTerminalsChars :: [String] -> Bool
validateTerminalsChars[] = True
validateTerminalsChars(x:xs) = if head x < 'a' || head x > 'z' then error("Terminály musí být [a-z], toto nalezeno: " ++ x) else validateTerminalsChars xs
