{- STOLEN STUFF -}

import Data.List -- for sorting lists, for nub
-- import Text.EditDistance -- a package defining the Levenshtein metric; not readily useful, since its functions are not polymorphic
import Data.Array -- for the memoization in the definition of Levenshtein metric

-- Define the Levenshtein metric as given on https://www.reddit.com/r/programming/comments/w4gs6/levenshtein_distance_in_haskell/c5a6jjz/ .

levenshtein :: (Eq a) => [a] -> [a] -> Int
levenshtein xs ys = memoArray ! (n, m)
  where memoArray = array ((0,0),(n,m)) [((i,j),levAux i j) | i <- [0..n], j <- [0..m]]
        n = length xs
        m = length ys
        xa = listArray (1, n) xs
        ya = listArray (1, m) ys
        levAux 0 v = v
        levAux u 0 = u
        levAux u v
          | xa ! u == ya ! v = memoArray ! (u-1, v-1)
          | otherwise        = 1 + minimum [memoArray ! (u, v-1),
                                            memoArray ! (u-1, v),
                                            memoArray ! (u-1, v-1)]

{- examples -}

{-

> levenshtein "ABCFs" "AFsB"
3

> levenshtein "uyfrgfnocsni7ryco4nfo9weqywo489ncf8ow" "pawcfboas78tbca4ano9foa8wcgksfygrnaow"
29

-}

{-
Apart from the auxiliary preexisting datatypes, like Int and [a], the first base datatypes that come to mind regarding this project are an enumeration type for all twelve notes, and the positive reals (or maybe, the positive integers will do in practice, don't know) representing frequencies. Let's concentrate first on the notes.
-}

{-NOTES AND FUNCTIONS OVER THEM-}

data Note = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs
    deriving (Eq, Enum, Ord, Show, Read, Bounded)
    
{-
Having notes, we can define intervals, certain triads, and more, as values of appropriate higher datatypes, for example, by functions outputing lists.
-}

-- an auxiliary function that gives the note m halfsteps above the note r (the "root")

halfsteps :: Note -> Int -> Note
halfsteps r m = (toEnum (((fromEnum r) + m) `mod` 12))

-- auxiliary functions that gives the halfsteps distance between two notes; an absolute and a directed version

halfstepsDistance :: Note -> Note -> Int
halfstepsDistance r s = min (((fromEnum r) - (fromEnum s)) `mod` 12) (((fromEnum s) - (fromEnum r)) `mod` 12)

halfstepsDirectedDistance :: Note -> Note -> Int
halfstepsDirectedDistance r s = ((fromEnum s) - (fromEnum r)) `mod` 12

-- a signature is a list of integers representing intervals; given a root r and a signature sig define the list of notes that are each that many halfsteps apart from the root according to the signature -- note: this is meant to work for non-negative integers for the time being, the intended use being the generation of scales and chords, not of melody

notelistBySignature :: Note -> [Int] -> [Note]
notelistBySignature r sig = [r `halfsteps` m | m <- sig]

-- intervals

interval1p :: Note -> [Note]
interval1p r = notelistBySignature r [0,0]

interval2m :: Note -> [Note]
interval2m r = notelistBySignature r [0,1]

interval2M :: Note -> [Note]
interval2M r = notelistBySignature r [0,2]

interval3m :: Note -> [Note]
interval3m r = notelistBySignature r [0,3]

interval3M :: Note -> [Note]
interval3M r = notelistBySignature r [0,4]

interval4p :: Note -> [Note]
interval4p r = notelistBySignature r [0,5]

interval4a :: Note -> [Note]
interval4a r = notelistBySignature r [0,6]

interval5p :: Note -> [Note]
interval5p r = notelistBySignature r [0,7]

interval6m :: Note -> [Note]
interval6m r = notelistBySignature r [0,8]

interval6M :: Note -> [Note]
interval6M r = notelistBySignature r [0,9]

interval7m :: Note -> [Note]
interval7m r = notelistBySignature r [0,10]

interval7M :: Note -> [Note]
interval7M r = notelistBySignature r [0,11]

-- triads

triadM :: Note -> [Note]
triadM r = notelistBySignature r [0,4,7]

triadm :: Note -> [Note]
triadm r = notelistBySignature r [0,3,7]

triada :: Note -> [Note]
triada r = notelistBySignature r [0,4,8]

triadd :: Note -> [Note]
triadd r = notelistBySignature r [0,3,6]

-- scales

scaleM :: Note -> [Note]
scaleM r = notelistBySignature r [0,2,4,5,7,9,11]

scalemh :: Note -> [Note]
scalemh r = notelistBySignature r [0,2,3,5,7,8,11]

scalemm :: Note -> [Note]
scalemm r = notelistBySignature r [0,2,3,5,7,9,11]

scalec :: Note -> [Note]
scalec r = notelistBySignature r [0..11]

scalewt :: Note -> [Note]
scalewt r = notelistBySignature r [0,2,4,6,8,10]

scalepM :: Note -> [Note]
scalepM r = notelistBySignature r [0,2,4,7,9]

scalepm :: Note -> [Note]
scalepm r = notelistBySignature r [0,3,5,7,10]

-- define the signature of a given note list as the list of the respective directed halfstep-distances

signature :: [Note] -> [Int]
signature ns = [halfstepsDirectedDistance (head ns) n | n <- ns]

--in order to get the modes of a given scale, or the inversions of a given chord, we first define the cyclic permutations of a (finite) list

cyclicPermutation :: Int -> [a] -> [a]
cyclicPermutation m []     = []
cyclicPermutation m xs     = take l (drop m cxs)
    where cxs = cycle xs
          l = length xs

-- now, according to established terminology, the m-th mode of a scale sc with root r, should be the (m-1)-th cyclic permutation of the given scale at root r, while, if we're thinking about chords, the m-th inversion should be the m-th cyclic permutation of a given chord (the 0-th inversion is called "root position"); what the heck, we define both
          
mode :: Int -> (Note -> [Note]) -> Note -> [Note]
mode m sc r = cyclicPermutation (m-1) (sc r)

inversion :: Int -> (Note -> [Note]) -> Note -> [Note]
inversion m sc r = cyclicPermutation m (sc r)

-- we relativize the above to an arbitrary rerooting: rsc stands now for a scale rooted at r and s stands for the new root

modeAtRoot :: Int -> (Note -> [Note]) -> Note -> Note -> [Note]
modeAtRoot m rsc r s = notelistBySignature s (signature (mode m rsc r))

inversionAtRoot :: Int -> (Note -> [Note]) -> Note -> Note -> [Note]
inversionAtRoot m rsc r s = notelistBySignature s (signature (inversion m rsc r))

{- APPLICATION

INPUT a note list metric, a list of notes c and a list of lists of notes cs
OUTPUT the (c:cs) sorted by the given metric

-}

sortByMetric :: ([Note] -> [Note] -> Int) -> [[Note]] -> [Note] -> [[Note]]
sortByMetric dist cs c = c : (sortOn (\ d -> dist c d) cs)

improScale :: [Note] -> Note -> [[Note]]
improScale ns r = sortByMetric levenshtein allscales ns
    where allscales = nub $ [modeAtRoot m scaleM r r | m <- [1..7]] ++ [modeAtRoot m scalepM r r | m <- [1..5]] ++ [modeAtRoot m scalewt r r | m <- [1..6]] ++ [modeAtRoot m scalemh r r | m <- [1..7]] ++ [modeAtRoot m scalemm r r | m <- [1..7]] ++ [modeAtRoot m scalec r r | m <- [1..12]] -- this is still ugly

improTriad :: [Note] -> Note -> [[Note]]
improTriad ns r = sortByMetric levenshtein alltriads ns
    where alltriads = nub $ [inversionAtRoot m triadM r r | m <- [1..3]] ++ [inversionAtRoot m triadm r r | m <- [1..3]] ++ [inversionAtRoot m triadd r r | m <- [1..3]] ++ [inversionAtRoot m triada r r | m <- [1..3]]

{- examples -}

{-

> levenshtein (mode 6 scaleM C) (scalemh A)
1

> levenshtein (mode 6 scaleM C) (scalemm A)
2

> halfstepsDistance A B
2

> halfstepsDistance B A
2

> sortByMetric levenshtein [mode m scaleM C | m <- [1..7]] (yieldOfChord c_major_extended)
[[C,E,G],[C,D,E,F,G,A,B],[A,B,C,D,E,F,G],[B,C,D,E,F,G,A],[D,E,F,G,A,B,C],[G,A,B,C,D,E,F],[E,F,G,A,B,C,D],[F,G,A,B,C,D,E]]

> sortByMetric levenshtein [modeAtRoot m scaleM C C | m <- [1..7]] (yieldOfChord c_major_extended)
[[C,E,G],[C,D,E,F,G,A,B],[C,D,E,Fs,G,A,B],[C,D,E,F,G,A,As],[C,D,Ds,F,G,A,As],[C,Cs,Ds,F,G,Gs,As],[C,D,Ds,F,G,Gs,As],[C,Cs,Ds,F,Fs,Gs,As]]

> sortByMetric levenshtein [modeAtRoot m scalemm C C | m <- [1..7]] (yieldOfChord c_major_extended)
[[C,E,G],[C,D,E,Fs,G,A,As],[C,D,E,F,G,Gs,As],[C,D,Ds,F,G,A,B],[C,Cs,Ds,F,G,A,As],[C,D,E,Fs,Gs,A,B],[C,Cs,Ds,E,Fs,Gs,As],[C,D,Ds,F,Fs,Gs,As]]

> improScale [C,G,E] C
[[C,G,E],[C,D,E,G,A],[C,D,F,G,As],[C,D,F,G,A],[C,Ds,F,G,As],[C,Ds,F,Gs,As],[C,D,E,Fs,Gs,As],[C,D,E,F,G,A,B],[C,D,Ds,F,G,A,As],[C,Cs,Ds,F,G,Gs,As],[C,D,E,Fs,G,A,B],[C,D,E,F,G,A,As],[C,D,Ds,F,G,Gs,As],[C,D,Ds,F,G,Gs,B],[C,D,E,F,Gs,A,B],[C,D,Ds,Fs,G,A,As],[C,Cs,E,F,G,Gs,As],[C,Ds,E,Fs,G,A,B],[C,Cs,Ds,E,Fs,Gs,A],[C,D,Ds,F,G,A,B],[C,Cs,Ds,F,G,A,As],[C,D,E,Fs,Gs,A,B],[C,D,E,Fs,G,A,As],[C,D,E,F,G,Gs,As],[C,Cs,Ds,E,Fs,Gs,As],[C,Cs,Ds,F,Fs,Gs,As],[C,Cs,Ds,F,Fs,A,As],[C,D,Ds,F,Fs,Gs,As],[C,Cs,D,Ds,E,F,Fs,G,Gs,A,As,B]]

> improScale [C,G,E] A
[[C,G,E],[A,C,D,E,G],[A,B,Cs,E,Fs],[A,B,D,E,G],[A,C,D,F,G],[A,B,D,E,Fs],[A,B,C,D,E,Fs,G],[A,As,C,D,E,F,G],[A,B,C,D,E,F,G],[A,B,C,D,E,F,Gs],[A,B,C,Ds,E,Fs,G],[A,C,Cs,Ds,E,Fs,Gs],[A,B,C,D,E,Fs,Gs],[A,As,C,D,E,Fs,G],[A,B,Cs,D,E,Fs,Gs],[A,B,Cs,Ds,E,Fs,Gs],[A,B,Cs,D,E,Fs,G],[A,As,C,D,Ds,F,G],[A,B,Cs,Ds,F,G],[A,As,C,D,Ds,Fs,G],[A,As,Cs,D,E,F,G],[A,As,C,Cs,Ds,F,Fs],[A,B,Cs,Ds,E,Fs,G],[A,B,Cs,D,E,F,G],[A,B,C,D,Ds,F,G],[A,As,C,Cs,Ds,F,G],[A,B,Cs,D,F,Fs,Gs],[A,B,Cs,Ds,F,Fs,Gs],[A,As,B,C,Cs,D,Ds,E,F,Fs,G,Gs]]

> improTriad [C,G,E] C
[[C,G,E],[C,Ds,Gs],[C,F,A],[C,E,G],[C,E,A],[C,F,Gs],[C,Ds,G],[C,Ds,A],[C,Fs,A],[C,Ds,Fs],[C,E,Gs]]

> improTriad [C,G,E] A
[[C,G,E],[A,Cs,E],[A,C,E],[A,C,F],[A,D,Fs],[A,Cs,Fs],[A,D,F],[A,C,Fs],[A,Ds,Fs],[A,C,Ds],[A,Cs,F]]

Note that the Levenshtein distance needs adaptation to fit certain musical intuitions.

-}

{-REPRESENTING CHORDS BY AN ALGEBRA-}

{-
So, one way to speak about chords is to view them as values of [Note], exactly as we did above with the four basic types of triads.
-}

type ChordL = [Note]

{-
Another way to capture all chords, which tracks their actual construction (as is sometimes the case in harmonic analysis), including single notes, intervals, triads, and polychords, is to use an inductive type, making use of the previously defined Note datatype. Here we restrict the chord datatype to up to "pentads"; not sure if we need more or less.
-}

data Chord = ChordBot | Single Note | Dyad Chord Chord | Triad Chord Chord Chord | Tetrad Chord Chord Chord Chord | Pentad Chord Chord Chord Chord Chord
    deriving (Eq, Ord, Show, Read)

{- examples -}

e_minor_over_c = Dyad (Single C) (Triad (Single E) (Single G) (Single B))
a_over_c_major = Dyad (Triad (Single C) (Single E) (Single G)) (Single A)
c_major_extended = Dyad (Triad (Single C) (Single E) (Single G)) (ChordBot)

{-

> :t a_over_c_major
a_over_c_major :: Chord

> :t C
C :: Note

> :t Single C
Single C :: Chord

-}

{-
There is a bunch of functions to be defined on any inductive datatype. Note that the constructor "Single" takes values of "Note" as arguments, and it seems intuitive to view it as nullary in (some of?) the definitions that follow.
-}

-- the size and the height of a token (i.e., a chord)

sizeOfChordToken :: Chord -> Int
sizeOfChordToken ChordBot           = 0
sizeOfChordToken (Single n)         = 1
sizeOfChordToken (Dyad c d)         = 1 + (sizeOfChordToken c) + (sizeOfChordToken d)
sizeOfChordToken (Triad c d e)      = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e)
sizeOfChordToken (Tetrad c d e f)   = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e) + (sizeOfChordToken f)
sizeOfChordToken (Pentad c d e f g) = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e) + (sizeOfChordToken f) + (sizeOfChordToken g)

heightOfChordToken :: Chord -> Int
heightOfChordToken ChordBot             = 0
heightOfChordToken (Single n)           = 1
heightOfChordToken (Dyad c d)           = 1 + maximum [heightOfChordToken c, heightOfChordToken d]
heightOfChordToken (Triad c d e)        = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e]
heightOfChordToken (Tetrad c d e f)     = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e, heightOfChordToken f]
heightOfChordToken (Pentad c d e f g)   = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e, heightOfChordToken f, heightOfChordToken g]

-- the size of the chord, that is, its number of notes

sizeOfChord :: Chord -> Int
sizeOfChord ChordBot            = 0
sizeOfChord (Single n)          = 1
sizeOfChord (Dyad c d)          = (sizeOfChord c) + (sizeOfChord d)
sizeOfChord (Triad c d e)       = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e)
sizeOfChord (Tetrad c d e f)    = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e) + (sizeOfChord f)
sizeOfChord (Pentad c d e f g)  = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e) + (sizeOfChord f) + (sizeOfChord g)

-- the yield of a token, that is, its leaves, that is, its actual notes

yieldOfChord :: Chord -> [Note]
yieldOfChord ChordBot           = []
yieldOfChord (Single n)         = [n]
yieldOfChord (Dyad c d)         = (yieldOfChord c) ++ (yieldOfChord d)
yieldOfChord (Triad c d e)      = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e)
yieldOfChord (Tetrad c d e f)   = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e) ++ (yieldOfChord f)
yieldOfChord (Pentad c d e f g) = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e) ++ (yieldOfChord f) ++ (yieldOfChord g)

{- examples -}

{-

> sizeOfChordToken e_minor_over_c
6

> heightOfChordToken e_minor_over_c
3

> sizeOfChord e_minor_over_c
4

> yieldOfChord e_minor_over_c
[C,E,G,B]

> yieldOfChord a_over_c_major
[C,E,G,A]

> sort (yieldOfChord a_over_c_major)
[A,C,E,G]

> :t Triad (Single C) (Single E) (Single G)
(Triad (Single C) (Single E) (Single G)) :: Chord

> :t yieldOfChord ((Triad (Single C) (Single E) (Single G)))
yieldOfChord ((Triad (Single C) (Single E) (Single G))) :: [Note]

The size of a chord is just the length of its yield.

> (sizeOfChord (Triad (Single C) (Single E) (Single G))) == length (yieldOfChord (Triad (Single C) (Single E) (Single G)))
True


-}

-- define the head constructor C of a token C a1 ... ar as the token C * ... *

headToken :: Chord -> Chord
headToken ChordBot = ChordBot
headToken (Single n) = Single n
headToken (Dyad c d) = Dyad ChordBot ChordBot
headToken (Triad c d e) = Triad ChordBot ChordBot ChordBot
headToken (Tetrad c d e f) = Tetrad ChordBot ChordBot ChordBot ChordBot
headToken (Pentad c d e f g) = Pentad ChordBot ChordBot ChordBot ChordBot ChordBot

-- define the i-th component token of a given token

componentToken :: Chord -> Int -> Maybe Chord
componentToken ChordBot _   = Nothing
componentToken (Single n) _ = Nothing
componentToken (Dyad c d) m
    | m < 1 || m > 2        = Nothing
    | m == 1                = Just c
    | m == 2                = Just d
componentToken (Triad c d e) m
    | m < 1 || m > 3        = Nothing
    | m == 1                = Just c
    | m == 2                = Just d
    | m == 3                = Just e
componentToken (Tetrad c d e f) m
    | m < 1 || m > 4        = Nothing
    | m == 1                = Just c
    | m == 2                = Just d
    | m == 3                = Just e
    | m == 4                = Just f
componentToken (Pentad c d e f g) m
    | m < 1 || m > 5        = Nothing
    | m == 1                = Just c
    | m == 2                = Just d
    | m == 3                = Just e
    | m == 4                = Just f
    | m == 5                = Just g

-- define the arity of a token; more general, and easier than declaring the arity of a constructor. In particular, what is here defined is the arity of the head constructor of a token

arity :: Chord -> Int
arity ChordBot              = 0
arity (Single n)            = 1 -- here Single is treated as a unary constructor!
arity (Dyad c d)            = 2
arity (Triad c d e)         = 3
arity (Tetrad c d e f)      = 4
arity (Pentad c d e f g)    = 5

-- define the list of component tokens of a given token

componentTokenList :: Chord -> [Chord]
componentTokenList ChordBot             = []
componentTokenList (Single n)           = []
componentTokenList (Dyad c d)           = [c, d]
componentTokenList (Triad c d e)        = [c, d, e]
componentTokenList (Tetrad c d e f)     = [c, d, e, f]
componentTokenList (Pentad c d e f g)   = [c, d, e, f, g]

-- define the list of all subtokens of a given token

subtokens :: Chord -> [Chord]
subtokens ChordBot              = [ChordBot]
subtokens (Single n)            = [Single n]
subtokens (Dyad c d)            = [Dyad c d] ++ (subtokens c) ++ (subtokens d)
subtokens (Triad c d e)         = [Triad c d e] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e)
subtokens (Tetrad c d e f)      = [Tetrad c d e f] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e) ++ (subtokens f)
subtokens (Pentad c d e f g)    = [Pentad c d e f g] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e) ++ (subtokens f) ++ (subtokens g)


{- examples -}

{-

> componentToken a_over_c_major 1
Just (Triad (Single C) (Single E) (Single G))

> componentToken a_over_c_major 2
Just (Single A)

> componentToken a_over_c_major 3
Nothing

> componentTokenList a_over_c_major
[Triad (Single C) (Single E) (Single G),Single A]

> componentTokenList e_minor_over_c
[Single C,Triad (Single E) (Single G) (Single B)]

> subtokens e_minor_over_c 
[Dyad (Single C) (Triad (Single E) (Single G) (Single B)),Single C,Triad (Single E) (Single G) (Single B),Single E,Single G,Single B]

-}

consistent :: Chord -> Chord -> Bool
consistent c ChordBot   = True
consistent ChordBot c   = True
consistent c d          = (headToken c == headToken d) && (consistent_lift (componentTokenList c) (componentTokenList d))

consistent_lift :: [Chord] -> [Chord] -> Bool
consistent_lift [] []           = True
consistent_lift (c:cs) (d:ds)   = (consistent c d) && (consistent_lift cs ds)
consistent_lift _ _             = False

{-examples-}

{-

> c_major_extended `consistent` a_over_c_major
True

> c_major_extended `consistent` e_minor_over_c
False

-}

consistentList :: [Chord] -> Bool
consistentList []       = True
consistentList [c]      = True
consistentList (c:d:cs) = (consistent c d) && (consistentList (c:cs))

{-examples-}

{-

> consistentList [c_major_extended, a_over_c_major, e_minor_over_c]
False

> consistentList [c_major_extended, a_over_c_major]
True

> consistentList [Single A, ChordBot, Single B]
False

-}