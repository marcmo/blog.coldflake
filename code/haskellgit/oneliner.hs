.ghci:

import Text.Regex
import qualified Data.List as L
import qualified Data.Set as S
import Data.List.Split
import Maybe(isJust)
import Control.Arrow ((&&&))
import Data.Function(on)

let script f = getContents >>= return . f
let (=~) inp pat = isJust $ matchRegex (mkRegex pat) inp 
let uniq = S.toList . S.fromList
:set prompt "List,Set,Split,Regex> "

from http://blog.endpoint.com/2009/03/git-commits-per-contributor-one-liner.html
Git commits per contributor one-liner
Posted by Jon Jensen
Just for fun, in the Spree Git repository:
git log | grep ^Author: | sed 's/ <.*//; s/^Author: //' | sort | uniq -c | sort -nr

one-liners:
number of commits per committer:
git log | ghc -e 'script $ map (\x->(head x,length x)) . L.group.L.sort.map(drop 8).filter(\line-> (=~) line "^Author").lines'


git log | ghc -e 'script $ L.sortBy(compare `on` snd).map (head &&& length) . L.group.L.sort.map(drop 8).filter((flip(=~)) "^Author").lines'

boost.git(master) > git log | ghc -e 'script $ reverse.L.sortBy(compare `on` snd).map (L.intercalate " ".tail.init.words.head &&& length) . L.group.L.sort.filter((flip(=~)) "^Author").lines'
[("david_abrahams",6447),("vladimir_prus",4980),("johnmaddock",4126),("jsiek",4088),("dgregor",2786),("beman_dawes",2206),("grafik",2005),("hkaiser",1943),("jmaurer",1702),("eric_niebler",1695),("djowel",1574),("az_sw_dude",1570),("ramey",1554),("turkanis",1439),("agurtovoy",1427),("rogeeff",1397),("mistevens",1098),("pdimov",1069),("danieljames",1011),("joaquintides",856),("andreas_huber69",855),("nesotto",765),("rwgk",624),("ebf",596),("chris_kohlhoff",586),("rgarcia",546),("vesa_karvonen",374),("urzuga",344),("t_schwinger",342),("pmenso57",322),("jurko",322),("gennaro_prota",311),("igaztanaga",299),("memring",286),("bemandawes",286),("jewillco",276),("daniel_wallin",264),("anthonyw",264),("tknapen",262),("schoepflin",260),("nicoddemus",254),("glassfordm",246),("samuel_k",220),("mbergal",218),("fcacciola",214),("gmelquio",212),("biochimia",172),("bill_kempf",172),("emildotchevski",167),("martin_wille",163),("darinadler",158),("slapi",150),("steven_watanabe",142),("dlwalker",141),("vertleyb",138),("dan_marsden",124),...]

## add untracked files and folders to .gitignore
git ls-files --others --exclude-standard --directory --no-empty-directory | ghc -e 'getContents >>= appendFile ".gitignore"'
