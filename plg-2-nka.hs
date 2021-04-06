import System.IO
import System.Environment
import Data.List.Split ( splitOn )
import qualified Control.Monad
import Control.Monad (unless)
import Data.List (nub, intercalate)
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

    -- over, ze neexistuje jednoduche pravidlo
    let noSimpleRule = validateNoSimpleRule productionRulesTuples nonterminals
    unless noSimpleRule $ error "Nesmí existovat jednoduché pravidlo!"

    -- validuj pravidla gramatiky
    let rulesNeterminals = validateRulesNonterminals productionRulesTuples nonterminals
    let rulesSnd = validateRulesSnd productionRulesTuples nonterminals terminals
    let rulesSndTerminals = validateRuselSndNonterminals productionRulesTuples nonterminals terminals
    Control.Monad.when (not rulesNeterminals || not rulesSnd || not rulesSndTerminals) $ error "Pravidla nejsou validni"

    -- -i flag functionality
    Control.Monad.when ("-i" `elem` flags) $ printGrammar rlg

    -- -1 functioncality
    Control.Monad.when ("-1" `elem` flags) $ printGrammar $ rightLinearGrammarToRightRegularTransoform rlg

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
    | otherwise = validateRulesSnd xs nonterminals terminals

validateRuselSndNonterminals :: [(String, String)] -> [String] -> [String] -> Bool
validateRuselSndNonterminals [] _ _ = True
validateRuselSndNonterminals (x:xs) nonterminals terminals
    | length (snd x) == 1 = validateRuselSndNonterminals xs nonterminals terminals
    | otherwise = not (any (\a -> [a] `notElem` terminals) (init (snd x))) && validateRuselSndNonterminals xs nonterminals terminals

validateNoSimpleRule :: [(String, String)] -> [String] -> Bool
validateNoSimpleRule [] _ = True
validateNoSimpleRule (x:xs) nonterminals
    | snd x `elem` nonterminals = False
    | otherwise = validateNoSimpleRule xs nonterminals

debug = flip trace


printGrammar grammar = do
    putStrLn (intercalate "," (nonterminals grammar))
    putStrLn (intercalate "," (terminals grammar))
    putStrLn (startSymbol grammar)
    mapM_ (\rule -> putStrLn (fst rule ++ "->" ++ snd rule) ) (productionRules grammar)

--                n -> orig n -> neterminal -> terminaly -> posledni neterminal
enumerateRules :: Int -> Int -> String -> String -> String -> [(String, String)]
enumerateRules n origN nonterminal terminals final
    | length terminals > 1 = (nonterminal ++ (if n /= origN then show n else ""), [head terminals] ++ (nonterminal ++ show (n+1))) : enumerateRules (n+1) n nonterminal (tail terminals) final
    | otherwise  = [(nonterminal ++ show n, terminals ++ final)]

splitRule :: Int -> (String, String) -> [(String, String)]
splitRule nextIndex rule = enumerateRules nextIndex nextIndex nonterminal terminals final
    where
        nonterminal = fst rule
        terminals = init $ snd rule
        final = [last $ snd rule]

getNextIndex :: [[(String, String)]] -> Int
getNextIndex [] = 0
getNextIndex rules = sum l - 1
    where l = map length rules

splitRuleRecursion :: [(String, String)] -> [(String, String)]
splitRuleRecursion [] = []
splitRuleRecursion (rule:rules) = splitRuleRecursion rules ++ splitRule (getNextIndex (map (splitRule 0) rules)) rule

hasNonterminal :: String -> Bool
hasNonterminal "" = False
hasNonterminal (x:xs)
    | x `elem` ['A' .. 'Z'] = True
    | otherwise = hasNonterminal xs

splitTerminalsRule :: Int -> Int-> (String, String) -> [(String, String)]
splitTerminalsRule index _ (nonterm, "") = [(nonterm ++ show index, "#")]
splitTerminalsRule index origIndex rule
    | index == origIndex = (fst rule, [head (snd rule)] ++ (fst rule ++ show (index+1))) : splitTerminalsRule (index+1) index (fst rule, tail $ snd rule)
    | otherwise = (fst rule ++ show index, [head (snd rule)] ++ (fst rule ++ show (index+1))) : splitTerminalsRule (index+1) index (fst rule, tail $ snd rule)

splitTerminals :: Int -> [(String, String)] -> [(String, String)]
splitTerminals _ [] = []
splitTerminals index (rule:rules) = splitTerminals index rules ++ splitTerminalsRule i i rule
    where i = index + getNextIndex (map (splitTerminalsRule 0 0) rules)

rightLinearGrammarToRightRegularTransoform :: Grammar -> Grammar
rightLinearGrammarToRightRegularTransoform grammar = Grammar r_nonterminals r_terminals r_productionRules r_startSymbol
    where
        r_nonterminals = [] -- nonterminals grammar
        r_terminals = [] -- terminals grammar
        r_productionRules = nub (ad1 ++ ad2 ++ ad3)
        r_startSymbol = [] -- startSymbol grammar
        ad1 = filter (\rule -> length (snd rule) <= 2 && (hasNonterminal (snd rule) || snd rule == "#")) (productionRules grammar) -- A → aB a A → # kde A, B ∈ N, a ∈ Σ
        ad2 = splitRuleRecursion $ reverse (filter (\rule -> length (snd rule) > 2 && last (snd rule) `elem` ['A'..'Z']) $ productionRules grammar) -- A → a1a2...anB; A, B ∈ N, ai ∈ Σ
        ad3 = splitTerminals (getNextIndex [ad2]) $ reverse (filter (\rule -> length (snd rule) >= 2 && not (hasNonterminal (snd rule))) $ productionRules grammar) -- A → a1...an, ai ∈ Σ, n ≥ 1 

