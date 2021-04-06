import Data.Char
import Data.List
import Data.Ord


-- return a list from the wordtext excluding the given charcters
toWordsList = map (map toLower . filter (`notElem`",.?!-:;'\"")).words
--return the sum of the number of common words within the wordlist
countCommonWords wordList = sum (map (\w -> (length w)) $ group $ sort (commonWords wordList))
--return list given the common words
commonWords wordList = filter (\w -> w `elem` ["the", "be", "to", "of", "and", "a", "in", "that", "have", "i", "it", "for", "not", "on", "with", "he", "as", "you", "do", "at"]) wordList
-- return the list without the given words 
dropCommonWords wordList = filter (\w -> w `notElem`["the", "be", "to", "of", "and", "a", "in", "that", "have", "i", "it", "for", "not", "on", "with", "he", "as", "you", "do", "at"]) wordList
-- returns the number of items in the wordlist 
countWords wordList = map (\w -> (head w, length w)) $ group $ sort wordList
--sorts alphabetically in reverse from end of alphabet starting from highest count  
sortOrder = reverse.sortBy (comparing snd)  
sortWords wordList = sortOrder wordList 
-- orders tuple placeing second tuple first repersented by aterisk then print arrow folowed by first tuple
printAsterisk x = (replicate (snd x) '*') ++ "->" ++ (fst x) ++ "\n"
--apply the first argument of the list from printAsterisk taking the first 20 in wordlist
makeHistogram wordList = concat $ map printAsterisk (take 20 wordList)
    

text = "It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way--in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only.\nThere were a king with a large jaw and a queen with a plain face, on the throne of England; there were a king with a large jaw and a queen with a fair face, on the throne of France. In both countries it was clearer than crystal to the lords of the State preserves of loaves and fishes, that things in general were settled for ever."

main = do
        let wordlist = toWordsList text
        putStrLn "Report:"
        putStrLn("\t" ++ (show $ length wordlist) ++ "words")
        putStrLn("\t" ++ (show $ countCommonWords wordlist) ++ "common words")
        putStrLn "\nHistogram of the most frequent words(excluding common words):\n"
        putStr $ makeHistogram $ sortWords $ countWords $ dropCommonWords $ wordlist
