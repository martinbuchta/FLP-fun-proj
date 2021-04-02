import System.IO
import System.Environment
import Data.List.Split ( splitOn )
import qualified Control.Monad
import Control.Monad (unless)
import Data.List (nub)
import Debug.Trace (trace)

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

    -- validuj pravidla gramatiky
    let rulesNeterminals = validateRulesNonterminals productionRulesTuples nonterminals
    let rulesSnd = validateRulesSnd productionRulesTuples nonterminals terminals
    let rulesSndTerminals = validateRuselSndNonterminals productionRulesTuples nonterminals terminals
    Control.Monad.when (not rulesNeterminals || not rulesSnd || not rulesSndTerminals) $ error "Pravidla nejsou validni"


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

-- params: rules, nonterminals
validateRulesNonterminals :: [(String, String)] -> [String] -> Bool
validateRulesNonterminals[] _ = True
validateRulesNonterminals(x:xs) nonterminals = if fst x `notElem` nonterminals then error("Neterminál z pravidla " ++ fst x ++ "->" ++ snd x ++ " neexistuje!") else validateRulesNonterminals xs nonterminals

validateRulesSnd :: [(String, String)] -> [String] -> [String] -> Bool
validateRulesSnd [] _ _ = True
validateRulesSnd (x:xs) nonterminals terminals
    | length (snd x) == 1 = if snd x /= "#" && snd x `notElem` terminals then error ("Neplatná pravá strana pravidla " ++ snd x) else validateRulesSnd xs nonterminals terminals
    | null (snd x) = False
    | otherwise = if [last (snd x)] `notElem` nonterminals then error ("Neplatný neterminál na pravé straně pravidla " ++ snd x) else validateRulesSnd xs nonterminals terminals

validateRuselSndNonterminals :: [(String, String)] -> [String] -> [String] -> Bool
validateRuselSndNonterminals [] _ _ = True
validateRuselSndNonterminals (x:xs) nonterminals terminals
    | length (snd x) == 1 = validateRuselSndNonterminals xs nonterminals terminals
    | otherwise = not (any (\a -> [a] `notElem` terminals) (init (snd x))) && validateRuselSndNonterminals xs nonterminals terminals

debug = flip trace
