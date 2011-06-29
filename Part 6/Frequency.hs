import Data.List
import Database.HDBC
import Database.HDBC.Sqlite3

--q = "select words.word, sum(freq) as wc, doc_freq.df as all_df, info_doc_freq.df as info_df from words inner join freqs on words.wordid = freqs.wordid left join doc_freq on words.wordid = doc_freq.wordid left join info_doc_freq on words.wordid = info_doc_freq.wordid group by words.word, doc_freq.df, info_doc_freq.df order by wc desc;"

q = "select words.word, sum(freq) as wc from words inner join freqs on words.wordid = freqs.wordid where docid in information group by words.word order by wc desc;" 

queryAndProcess conn = do
    r <- quickQuery' conn q []
    writeFile "resized.tsv" $ process (map (map fromSql) r)

process :: [[String]] -> String
process = untable . sizeUp

untable :: [[String]] -> String
untable = unlines . (map (intercalate "\t")) . ((:) header)
    where
        header = ["Word", "Word Count", "Relative Increase"]
        --header = ["Word", "Word Count", "Relative Increase", "All Doc Count", "Info Doc Count"]

sizeUp :: [[String]] -> [[String]]
sizeUp [row] = [row ++ ["1"]]
--sizeUp ([word, wc, allDf, iDf]:(next@[nextWord, nextWc]):table) = [word, wc, size, allDf, iDf] : (sizeUp (next:table))
sizeUp ([word, wc]:(next@[nextWord, nextWc]):table) = [word, wc, size] : (sizeUp (next:table))
    where
        size = show $ (1+) $ (wc' - nextWc')/nextWc'
        wc' = read wc
        nextWc' = read nextWc

main = do
    conn <- connectSqlite3 "words.db"
    queryAndProcess conn
    disconnect conn