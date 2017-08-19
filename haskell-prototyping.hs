{----------------------------------}
{- 0 -- PREAMBLE AND STOLEN STUFF -}
{----------------------------------}

import Data.List -- for sorting lists, for nub
import Data.Array -- for the memoization in the definition of Levenshtein metric

-- Define the Levenshtein metric as given on https://www.reddit.com/r/programming/comments/w4gs6/levenshtein_distance_in_haskell/c5a6jjz/ . It is polymorphic, so readily applicable to our case, in contrast to the existing package Text.EditDistance.

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

{- EXAMPLES

> levenshtein "ABCFs" "AFsB"
3

> levenshtein "uyfrgfnocsni7ryco4nfo9weqywo489ncf8ow" "pawcfboas78tbca4ano9foa8wcgksfygrnaow"
29

-}

{--------------------------------------}
{- 1 -- AN ENUMERATION TYPE FOR NOTES -}
{--------------------------------------}

{-

Apart from the auxiliary preexisting datatypes, like Int and [a], the first base datatypes that come to mind regarding this project are an enumeration type for all twelve notes, and the positive reals (or maybe, the positive integers will do in practice, don't know) representing frequencies. Let's concentrate first on the notes. Having notes, we can proceed to define intervals, triads, scales, as values of appropriate higher datatypes.

-}

data Note = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs
    deriving (Eq, Enum, Ord, Show, Read, Bounded)
    
-- An auxiliary function that gives the note m halfsteps above the note r (the "root").

halfsteps :: Note -> Int -> Note
halfsteps r m = (toEnum (((fromEnum r) + m) `mod` 12))

-- Auxiliary functions that gives the halfsteps distance between two notes; an absolute and a directed version.

halfstepsDistance :: Note -> Note -> Int
halfstepsDistance r s = min (((fromEnum r) - (fromEnum s)) `mod` 12) (((fromEnum s) - (fromEnum r)) `mod` 12)

halfstepsDirectedDistance :: Note -> Note -> Int
halfstepsDirectedDistance r s = ((fromEnum s) - (fromEnum r)) `mod` 12

-- A signature is a list of integers representing intervals. given a root r and a signature sig define the list of notes that are each that many halfsteps apart from the root according to the signature. NB: this is meant to work for non-negative integers for the time being, the intended use being the generation of scales and chords, not of melody.

type Sig = [Int]

notesBySignature :: Note -> [Int] -> [Note]
notesBySignature r sig = [r `halfsteps` m | m <- sig]

-- Hardcoding signatures

sigInterval1p :: [Int] -- unison interval signature
sigInterval1p = [0,0]

sigInterval2m :: [Int] -- minor second interval signature
sigInterval2m = [0,1]

sigInterval2M :: [Int] -- major second interval signature
sigInterval2M = [0,2]

sigInterval3m :: [Int] -- minor third interval signature
sigInterval3m = [0,3]

sigInterval3M :: [Int] -- major third interval signature
sigInterval3M = [0,4]

sigInterval4p :: [Int] -- perfect fourth interval signature
sigInterval4p = [0,5]

sigInterval4a :: [Int] -- augmented fourth interval signature
sigInterval4a = [0,6]

sigInterval5p :: [Int] -- perfect fifth interval signature
sigInterval5p = [0,7]

sigInterval6m :: [Int] -- minor sixth interval signature
sigInterval6m = [0,8]

sigInterval6M :: [Int] -- major sixth interval signature
sigInterval6M = [0,9]

sigInterval7m :: [Int] -- minor seventh interval signature
sigInterval7m = [0,10]

sigInterval7M :: [Int] -- major seventh interval signature
sigInterval7M = [0,11]

sigTriadM :: [Int] -- major triad signature
sigTriadM = [0,4,7]

sigTriadm :: [Int] -- minor triad signature
sigTriadm = [0,3,7]

sigTriada :: [Int] -- augmented triad signature
sigTriada = [0,4,8]

sigTriadd :: [Int] -- diminished triad signature
sigTriadd = [0,3,6]

sigScaleM :: [Int] -- major scale signature
sigScaleM = [0,2,4,5,7,9,11]

sigScalemh :: [Int] -- harmonic minor scale signature
sigScalemh = [0,2,3,5,7,8,11]

sigScalemm :: [Int] -- melodic minor scale signature
sigScalemm = [0,2,3,5,7,9,11]

sigScalec :: [Int] -- chromatic scale signature
sigScalec = [0..11]

sigScalewt :: [Int] -- whole tone scale signature
sigScalewt = [0,2,4,6,8,10]

sigScalepM :: [Int] -- pentatonic major scale signature
sigScalepM = [0,2,4,7,9]

sigScalepm :: [Int] -- pentatonic minor scale signature
sigScalepm = [0,3,5,7,10]

-- Hardcoding intervals

interval1p :: Note -> [Note] -- unison
interval1p r = notesBySignature r [0,0]

interval2m :: Note -> [Note] -- minor second
interval2m r = notesBySignature r [0,1]

interval2M :: Note -> [Note] -- major second
interval2M r = notesBySignature r [0,2]

interval3m :: Note -> [Note] -- minor third
interval3m r = notesBySignature r [0,3]

interval3M :: Note -> [Note] -- major third
interval3M r = notesBySignature r [0,4]

interval4p :: Note -> [Note] -- perfect fourth
interval4p r = notesBySignature r [0,5]

interval4a :: Note -> [Note] -- augmented fourth
interval4a r = notesBySignature r [0,6]

interval5p :: Note -> [Note] -- perfect fifth
interval5p r = notesBySignature r [0,7]

interval6m :: Note -> [Note] -- minor sixth
interval6m r = notesBySignature r [0,8]

interval6M :: Note -> [Note] -- major sixth
interval6M r = notesBySignature r [0,9]

interval7m :: Note -> [Note] -- minor seventh
interval7m r = notesBySignature r [0,10]

interval7M :: Note -> [Note] -- major seventh
interval7M r = notesBySignature r [0,11]

-- triads

triadM :: Note -> [Note] -- major triad
triadM r = notesBySignature r [0,4,7]

triadm :: Note -> [Note] -- minor triad
triadm r = notesBySignature r [0,3,7]

triada :: Note -> [Note] -- augmented triad
triada r = notesBySignature r [0,4,8]

triadd :: Note -> [Note] -- diminished triad
triadd r = notesBySignature r [0,3,6]

-- and scales

scaleM :: Note -> [Note] -- major scale
scaleM r = notesBySignature r [0,2,4,5,7,9,11]

scalemh :: Note -> [Note] -- harmonic minor scale
scalemh r = notesBySignature r [0,2,3,5,7,8,11]

scalemm :: Note -> [Note] -- melodic minor scale
scalemm r = notesBySignature r [0,2,3,5,7,9,11]

scalec :: Note -> [Note] -- chromatic scale
scalec r = notesBySignature r [0..11]

scalewt :: Note -> [Note] -- whole tone scale
scalewt r = notesBySignature r [0,2,4,6,8,10]

scalepM :: Note -> [Note] -- pentatonic major scale
scalepM r = notesBySignature r [0,2,4,7,9]

scalepm :: Note -> [Note] -- pentatonic minor scale
scalepm r = notesBySignature r [0,3,5,7,10]

-- Define the signature of a given note-list as the list of the respective directed halfstep-distances.

signatureByNotes :: [Note] -> [Int]
signatureByNotes ns = [halfstepsDirectedDistance (head ns) n | n <- ns]

-- In order to get the modes of a given scale, or the inversions of a given chord, we first define the cyclic permutations of a (finite) list.

cyclicPermutation :: Int -> [a] -> [a]
cyclicPermutation m []     = []
cyclicPermutation m xs     = take l (drop m cxs)
    where cxs = cycle xs
          l = length xs

-- Now, according to established terminology, the m-th mode of a scale sc with root r, should be the (m-1)-th cyclic permutation of the given scale at root r, while, if we're thinking about chords, the m-th inversion should be the m-th cyclic permutation of a given chord (the 0-th inversion is called "root position"); what the heck, we define both.
          
mode :: Int -> (Note -> [Note]) -> Note -> [Note]
mode m sc r = cyclicPermutation (m-1) (sc r)

inversion :: Int -> (Note -> [Note]) -> Note -> [Note]
inversion m sc r = cyclicPermutation m (sc r)

-- We relativize the above to an arbitrary rerooting: rsc stands now for a scale rooted at r and s stands for the new root.

modeAtRoot :: Int -> (Note -> [Note]) -> Note -> Note -> [Note]
modeAtRoot m rsc r s = notesBySignature s (signatureByNotes (mode m rsc r))

inversionAtRoot :: Int -> (Note -> [Note]) -> Note -> Note -> [Note]
inversionAtRoot m rsc r s = notesBySignature s (signatureByNotes (inversion m rsc r))

{-----------------------------------------}
{- 1.1 -- APPLICATION: SCALES FROM NOTES -}
{-----------------------------------------}

{-

Suggest scales fitting to a set of notes.

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

{- EXAMPLES

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

Note that the Levenshtein distance needs adaptation to fit some musical intuitions.

This is for the song "Etymology":

> improScale (sort $ nub $ [E,G,D,C,Ds,G,A,C,E,G,Ds,G]) E
[[A,C,D,Ds,E,G],[E,G,A,C,D],[E,Fs,Gs,B,Cs],[E,Fs,A,B,D],[E,Fs,A,B,Cs],[E,G,A,B,D],[E,Fs,Gs,As,C,D],[E,Fs,Gs,A,C,Cs,Ds],[E,Fs,Gs,A,B,Cs,Ds],[E,Fs,G,A,B,Cs,D],[E,F,G,A,B,C,D],[E,Fs,Gs,As,B,Cs,Ds],[E,Fs,Gs,A,B,Cs,D],[E,Fs,G,A,B,C,D],[E,F,G,A,As,C,D],[E,Fs,G,A,B,C,Ds],[E,F,G,A,As,Cs,D],[E,Fs,G,As,B,Cs,D],[E,F,Gs,A,B,C,D],[E,G,Gs,As,B,Cs,Ds],[E,F,G,Gs,As,C,Cs],[E,Fs,G,A,B,Cs,Ds],[E,F,G,A,B,Cs,D],[E,Fs,Gs,As,C,Cs,Ds],[E,Fs,Gs,As,B,Cs,D],[E,Fs,Gs,A,B,C,D],[E,Fs,G,A,As,C,D],[E,F,G,Gs,As,C,D],[E,F,Fs,G,Gs,A,As,B,C,Cs,D,Ds]]

-}

{------------------------------------}
{- 1.2 -- APPLICATION: SCALE CHORDS -}
{------------------------------------}

{-

Produce the chords of a given scale, according to a given signature.

INPUT a scale and a signature
OUTPUT a list of lists of notes from the given scale that fit the given signature

-}

scalechords :: [Note] -> [Int] -> [[Note]]
scalechords [] _ = []
scalechords _ [] = []
scalechords sc sig = [ notesBySignature (sc !! 0) c | c <- memoArray, all (\ n -> elem n scsig) c]
    where
        scsig = signatureByNotes sc
        memoArray = map (\ m -> (map (\ n -> (n + m) `mod` 12) sig)) scsig

{- EXAMPLES -}

c_major_scale = scaleM C -- alternatively, c_major_scale = notesBySignature C sigScaleM

{-

The following find all major, minor, augmented, and diminished triads to be found in the key of C major.

> scalechords c_major_scale sigTriadM
[[C,E,G],[F,A,C],[G,B,D]]

> scalechords c_major_scale sigTriadm
[[D,F,A],[E,G,B],[A,C,E]]

> scalechords c_major_scale sigTriada
[]

> scalechords c_major_scale sigTriadd
[[B,D,F]]

We can find all triads to be found in the key of C major

> map (\ sigs -> scalechords c_major_scale sigs) [sigTriadM, sigTriadm, sigTriada, sigTriadd]
[[[C,E,G],[F,A,C],[G,B,D]],[[D,F,A],[E,G,B],[A,C,E]],[],[[B,D,F]]]

or in the scale of E minor pentatonic

> map (\ sigs -> scalechords (notesBySignature E sigScalepm) sigs) [sigTriadM, sigTriadm, sigTriada, sigTriadd]
[[[G,B,D]],[[E,G,B]],[],[]]

Similarly, we can use the function scalechords to find all perfect fourth intervals, say, of A minor pentatonic.

> map (\ sigs -> scalechords (notesBySignature E sigScalepm) sigs) [ sigInterval4p ]
[[[E,A],[A,D],[B,E],[D,G]]]

and, to contrast, all perfect fourth intervals of C major:

> map (\ sigs -> scalechords c_major_scale sigs) [ sigInterval4p ]
[[[C,F],[D,G],[E,A],[G,C],[A,D],[B,E]]]

Finally, all tritones in A melodic minor.

> map (\ sigs -> scalechords (notesBySignature A sigScalemm) sigs) [ sigInterval4a ]
[[[C,Fs],[D,Gs],[Fs,C],[Gs,D]]]

Also, we can find existing instances of a scale or mode within a given scale. For example, this is how we find which notes of E major key constitute its locrian mode (the seventh mode of a major scale).

> scalechords (scaleM E) (signatureByNotes [B, C, D, E, F, G, A])

-}

{--------------------------------------------------}
{- 2 -- AN INDUCTIVE TYPE FOR CHORD CONSTRUCTIONS -}
{--------------------------------------------------}

{-
So, one way to speak about chords is to view them as values of [Note], exactly as we did above with the four basic types of triads.
-}

type ChordL = [Note]

{-
Another way to capture all chords, which tracks their actual construction (as is sometimes the case in harmonic analysis), including single notes, intervals, triads, and polychords, is to use an inductive type, making use of the previously defined Note datatype. Here we restrict the chord datatype to up to "pentads"; not sure if we need more or less.
-}

data Chord = ChordBot | Single Note | Dyad Chord Chord | Triad Chord Chord Chord | Tetrad Chord Chord Chord Chord | Pentad Chord Chord Chord Chord Chord
    deriving (Eq, Ord, Show, Read)

{- EXAMPLES -}

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

-- The size and the height of a token (i.e., a chord).

sizeOfChordToken :: Chord -> Int
sizeOfChordToken ChordBot           = 0
sizeOfChordToken (Single _)         = 1
sizeOfChordToken (Dyad c d)         = 1 + (sizeOfChordToken c) + (sizeOfChordToken d)
sizeOfChordToken (Triad c d e)      = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e)
sizeOfChordToken (Tetrad c d e f)   = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e) + (sizeOfChordToken f)
sizeOfChordToken (Pentad c d e f g) = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e) + (sizeOfChordToken f) + (sizeOfChordToken g)

heightOfChordToken :: Chord -> Int
heightOfChordToken ChordBot             = 0
heightOfChordToken (Single _)           = 1
heightOfChordToken (Dyad c d)           = 1 + maximum [heightOfChordToken c, heightOfChordToken d]
heightOfChordToken (Triad c d e)        = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e]
heightOfChordToken (Tetrad c d e f)     = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e, heightOfChordToken f]
heightOfChordToken (Pentad c d e f g)   = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e, heightOfChordToken f, heightOfChordToken g]

-- The size of the chord, that is, its number of notes.

sizeOfChord :: Chord -> Int
sizeOfChord ChordBot            = 0
sizeOfChord (Single _)          = 1
sizeOfChord (Dyad c d)          = (sizeOfChord c) + (sizeOfChord d)
sizeOfChord (Triad c d e)       = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e)
sizeOfChord (Tetrad c d e f)    = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e) + (sizeOfChord f)
sizeOfChord (Pentad c d e f g)  = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e) + (sizeOfChord f) + (sizeOfChord g)

-- The yield of a token, that is, its leaves, that is, its actual notes.

yieldOfChord :: Chord -> [Note]
yieldOfChord ChordBot           = []
yieldOfChord (Single n)         = [n]
yieldOfChord (Dyad c d)         = (yieldOfChord c) ++ (yieldOfChord d)
yieldOfChord (Triad c d e)      = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e)
yieldOfChord (Tetrad c d e f)   = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e) ++ (yieldOfChord f)
yieldOfChord (Pentad c d e f g) = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e) ++ (yieldOfChord f) ++ (yieldOfChord g)

{- EXAMPLES

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

-- Define the head constructor C of a token C a1 ... ar as the token C * ... *.

headToken :: Chord -> Chord
headToken ChordBot = ChordBot
headToken (Single n) = Single n
headToken (Dyad c d) = Dyad ChordBot ChordBot
headToken (Triad c d e) = Triad ChordBot ChordBot ChordBot
headToken (Tetrad c d e f) = Tetrad ChordBot ChordBot ChordBot ChordBot
headToken (Pentad c d e f g) = Pentad ChordBot ChordBot ChordBot ChordBot ChordBot

-- Define the arity of a token; more general, and easier than declaring the arity of a constructor. In particular, what is here defined is the arity of the head constructor of a token.

arity :: Chord -> Int
arity ChordBot              = 0
arity (Single _)            = 0 -- here Single is treated as a nullary constructor!
arity (Dyad _ _)            = 2
arity (Triad _ _ _)         = 3
arity (Tetrad _ _ _ _)      = 4
arity (Pentad _ _ _ _ _)    = 5

-- Define the list of component tokens of a given token.

tokenComponents :: Chord -> [Chord]
tokenComponents ChordBot             = []
tokenComponents (Single _)           = []
tokenComponents (Dyad c d)           = [c, d]
tokenComponents (Triad c d e)        = [c, d, e]
tokenComponents (Tetrad c d e f)     = [c, d, e, f]
tokenComponents (Pentad c d e f g)   = [c, d, e, f, g]

-- Define the list of all subtokens of a given token.

subtokens :: Chord -> [Chord]
subtokens ChordBot              = [ChordBot]
subtokens (Single n)            = [Single n]
subtokens (Dyad c d)            = [Dyad c d] ++ (subtokens c) ++ (subtokens d)
subtokens (Triad c d e)         = [Triad c d e] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e)
subtokens (Tetrad c d e f)      = [Tetrad c d e f] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e) ++ (subtokens f)
subtokens (Pentad c d e f g)    = [Pentad c d e f g] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e) ++ (subtokens f) ++ (subtokens g)


{- EXAMPLES

> componentToken a_over_c_major 1
Just (Triad (Single C) (Single E) (Single G))

> componentToken a_over_c_major 2
Just (Single A)

> componentToken a_over_c_major 3
Nothing

> tokenComponents a_over_c_major
[Triad (Single C) (Single E) (Single G),Single A]

> tokenComponents e_minor_over_c
[Single C,Triad (Single E) (Single G) (Single B)]

> subtokens e_minor_over_c 
[Dyad (Single C) (Triad (Single E) (Single G) (Single B)),Single C,Triad (Single E) (Single G) (Single B),Single E,Single G,Single B]

-}

-- Define the usual consistency predicates.

consistent :: Chord -> Chord -> Bool
consistent _ ChordBot   = True
consistent ChordBot _   = True
consistent c d          = (headToken c == headToken d) && (consistent_lift (tokenComponents c) (tokenComponents d))

consistent_lift :: [Chord] -> [Chord] -> Bool
consistent_lift [] []           = True
consistent_lift (c:cs) (d:ds)   = (consistent c d) && (consistent_lift cs ds)
consistent_lift _ _             = False

consistentL :: [Chord] -> Bool
consistentL []       = True
consistentL [_]      = True
consistentL (c:d:cs) = (consistent c d) && (consistentL (c:cs))

{- EXAMPLES

> c_major_extended `consistent` a_over_c_major
True

> c_major_extended `consistent` e_minor_over_c
False

> consistentL [c_major_extended, a_over_c_major, e_minor_over_c]
False

> consistentL [c_major_extended, a_over_c_major]
True

> consistentL [Single A, ChordBot, Single B]
False

-}

-- An auxiliary function for the notion of sufficiency: a neighborhood U is sufficient for the constructor C of arity r on the arguments U1, ..., Ur, when for each i = 1, ..., r and each ai in Ui there exists an a in U with head constructor C and i-th component token ai.

sufficient :: [Chord] -> Chord -> [[Chord]] -> Bool -- arg1: U, arg2: C, arg3: U1, 6..., Ur
sufficient u ctr us
    | not (consistentL u) = error "sufficient: inconsistent list"
    | ctr /= (headToken ctr) = error "sufficient: not a constructor"
    | (length us) /= r = error "sufficient: too few or too many arguments"
    | otherwise = all (\ i -> all (\ b -> any (\ a -> headToken a == ctr && ((tokenComponents a) !! (i - 1)) == b) u) (us !!(i-1))) indices
    where
        r = arity ctr
        indices = [1..r]
        
-- An auxiliary function for the constructor application.

ctrapply :: Chord -> [[Chord]] -> [Chord]
ctrapply ctr us
    | ctr /= ctrhead                                                    = error "ctrapply: not a constructor"
--     | any (\ u -> u == []) us                                           = error "ctrapply: empty argument" -- if you include this line the pathform below crashes; in any case, you would need the line in the definition of entails, if you were to use ctrapply there
    | any (\ u -> not (consistentL u)) us                               = error "ctrapply: inconsistent argument"
    | (length us) /= (arity ctr)                                        = error "ctrapply: too few or too many arguments"
    | ctrhead == ChordBot                                               = [ChordBot]
    | ctrhead == Single A                                               = [Single A]
    | ctrhead == Single As                                              = [Single As]
    | ctrhead == Single B                                               = [Single B]
    | ctrhead == Single C                                               = [Single C]
    | ctrhead == Single Cs                                              = [Single Cs]
    | ctrhead == Single D                                               = [Single D]
    | ctrhead == Single Ds                                              = [Single Ds]
    | ctrhead == Single E                                               = [Single E]
    | ctrhead == Single F                                               = [Single F]
    | ctrhead == Single Fs                                              = [Single Fs]
    | ctrhead == Single G                                               = [Single G]
    | ctrhead == Single Gs                                              = [Single Gs]
    | ctrhead == Dyad ChordBot ChordBot                                 = [Dyad a b | a <- (us !! 0), b <- (us !! 1)]
    | ctrhead == Triad ChordBot ChordBot ChordBot                       = [Triad a b c | a <- (us !! 0), b <- (us !! 1), c <- (us !! 2)]
    | ctrhead == Tetrad ChordBot ChordBot ChordBot ChordBot             = [Tetrad a b c d | a <- (us !! 0), b <- (us !! 1), c <- (us !! 2), d <- (us !! 3)]
    | ctrhead == Pentad ChordBot ChordBot ChordBot ChordBot ChordBot    = [Pentad a b c d e | a <- (us !! 0), b <- (us !! 1), c <- (us !! 2), d <- (us !! 3), e <- (us !! 4)]
    where
        ctrhead = headToken ctr

-- The following strips a neighborhood of its bottom tokens.

stripBot :: [Chord] -> [Chord]
stripBot u = filter (\ a -> a /= ChordBot) u

-- Define the predicate of entailment; interesting that it works without ctrapply or stripBot -- this needs testing.
        
entails :: [Chord] -> Chord -> Bool
entails u a
    | not (consistentL u)    = error "inconsistent list"
    | otherwise                 = case a of
                                        ChordBot         -> True
                                        Single n         -> sufficient u (headToken a) [[Single n]]
                                        Dyad b c         -> sufficient u (headToken a) [[b], [c]]
                                        Triad b c d      -> sufficient u (headToken a) [[b], [c], [d]]
                                        Tetrad b c d e   -> sufficient u (headToken a) [[b], [c], [d], [e]]
                                        Pentad b c d e f -> sufficient u (headToken a) [[b], [c], [d], [e], [f]]

-- Finally, define the lift of entails between lists.

entailsL :: [Chord] -> [Chord] -> Bool
entailsL u v = all (\ b -> u `entails` b) v

{- EXAMPLES

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad ChordBot ChordBot)
True

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad e_minor_over_c ChordBot)
True

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad e_minor_over_c (Dyad (Triad (Single C) (Single E) (Single G)) (Single A)))
True

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad e_minor_over_c (Dyad (Triad (Single C) (Single E) (Single G)) ChordBot))
True

> entails [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] (Dyad e_minor_over_c (Dyad (Triad (Single C) (Single E) (Single G)) (Single B)))
False

> [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entails` (Dyad e_minor_over_c (Dyad (Triad (Single C) (Single E) (Single G)) (Single B)))
False

> [ChordBot, Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entails` (Dyad e_minor_over_c ChordBot)
True

> [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entails` (Triad e_minor_over_c ChordBot ChordBot)
False

> [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entailsL` [ChordBot,Triad e_minor_over_c ChordBot ChordBot]
False

> [Dyad ChordBot c_major_extended, Dyad ChordBot a_over_c_major, Dyad e_minor_over_c ChordBot] `entailsL` [ChordBot,Dyad e_minor_over_c ChordBot]
True

-}

-- Now to some normal forms of neighborhoods. The eigentoken (or supremum) of a neighborhood is defined with the help of an auxiliary function computing suprema of two tokens.

sup :: Chord -> Chord -> Chord
sup ChordBot a = a
sup a ChordBot = a
sup (Single A) (Single A) = (Single A)
sup (Single As) (Single As) = (Single As)
sup (Single B) (Single B) = (Single B)
sup (Single C) (Single C) = (Single C)
sup (Single Cs) (Single Cs) = (Single Cs)
sup (Single D) (Single D) = (Single D)
sup (Single Ds) (Single Ds) = (Single Ds)
sup (Single E) (Single E) = (Single E)
sup (Single F) (Single F) = (Single F)
sup (Single Fs) (Single Fs) = (Single Fs)
sup (Single G) (Single G) = (Single G)
sup (Single Gs) (Single Gs) = (Single Gs)
sup (Dyad a b) (Dyad a' b') = Dyad (sup a a') (sup b b')
sup (Triad a b c) (Triad a' b' c') = Triad (sup a a') (sup b b') (sup c c')
sup (Tetrad a b c d) (Tetrad a' b' c' d') = Tetrad (sup a a') (sup b b') (sup c c') (sup d d')
sup (Pentad a b c d e) (Pentad a' b' c' d' e') = Pentad (sup a a') (sup b b') (sup c c') (sup d d') (sup e e')
sup _ _ = error "sup: inconsistent tokens"

supL :: [Chord] -> Chord
supL [] = ChordBot
supL [a] = a
supL (a:as) = sup a (supL as)

-- The path form of a neighborhood is defined with the help of an auxiliary function computing the path form of a token.

paths :: Chord -> [Chord]
paths ChordBot            = [ChordBot]
paths (Single A)          = [Single A]
paths (Single As)         = [Single As]
paths (Single B)          = [Single B]
paths (Single C)          = [Single C]
paths (Single Cs)         = [Single Cs]
paths (Single D)          = [Single D]
paths (Single Ds)         = [Single Ds]
paths (Single E)          = [Single E]
paths (Single F)          = [Single F]
paths (Single Fs)         = [Single Fs]
paths (Single G)          = [Single G]
paths (Single Gs)         = [Single Gs]
paths (Dyad a b)          = nub $ (ctrapply (Dyad ChordBot ChordBot) [stripBot $ paths a, [ChordBot]]) ++ (ctrapply (Dyad ChordBot ChordBot) [[ChordBot], stripBot $ paths b])
paths (Triad a b c)       = nub $ (ctrapply (Triad ChordBot ChordBot ChordBot) [stripBot $ paths a, [ChordBot], [ChordBot]]) ++ (ctrapply (Triad ChordBot ChordBot ChordBot) [[ChordBot], stripBot $ paths b, [ChordBot]]) ++ (ctrapply (Triad ChordBot ChordBot ChordBot)  [[ChordBot], [ChordBot], stripBot $ paths c])
paths (Tetrad a b c d)    = nub $ (ctrapply (Tetrad ChordBot ChordBot ChordBot ChordBot) [stripBot $ paths a, [ChordBot], [ChordBot], [ChordBot]]) ++ (ctrapply (Tetrad ChordBot ChordBot ChordBot ChordBot) [[ChordBot], stripBot $ paths b, [ChordBot], [ChordBot]]) ++ (ctrapply (Tetrad ChordBot ChordBot ChordBot ChordBot)  [[ChordBot], [ChordBot], stripBot $ paths c, [ChordBot]]) ++ (ctrapply (Tetrad ChordBot ChordBot ChordBot ChordBot) [[ChordBot], [ChordBot], [ChordBot], stripBot $ paths d])
paths (Pentad a b c d e)  = nub $ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot) [stripBot $ paths a, [ChordBot], [ChordBot], [ChordBot], [ChordBot]]) ++ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot) [[ChordBot], stripBot $ paths b, [ChordBot], [ChordBot], [ChordBot]]) ++ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot)  [[ChordBot], [ChordBot], stripBot $ paths c, [ChordBot], [ChordBot]]) ++ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot) [[ChordBot], [ChordBot], [ChordBot], stripBot $ paths d, [ChordBot]]) ++ (ctrapply (Pentad ChordBot ChordBot ChordBot ChordBot ChordBot) [[ChordBot], [ChordBot], [ChordBot], [ChordBot], stripBot $ paths e])

pathsL :: [Chord] -> [Chord]
pathsL u = paths (supL u)

{- EXAMPLES

> sup (Dyad (Single Cs) (ChordBot)) (Dyad (ChordBot) (Single D))
Dyad (Single Cs) (Single D)

> sup (Dyad (Single Cs) (ChordBot)) (Single D)
*** Exception: sup: inconsistent tokens

> supL [Triad (Single C) ChordBot ChordBot, Triad ChordBot (Single E) ChordBot, Triad ChordBot ChordBot (Single G), ChordBot, Triad (Single C) (Single E) ChordBot]
Triad (Single C) (Single E) (Single G)

> supL []
ChordBot

> supL [Triad (Single C) ChordBot ChordBot, Triad ChordBot (Single E) ChordBot, Triad ChordBot ChordBot (Single G), ChordBot, Triad (Single F) ChordBot ChordBot]
Triad *** Exception: sup: inconsistent tokens

> paths ChordBot
[ChordBot]

> paths (Single A)
[Single A]

> paths (Dyad ChordBot (Single A))
[Dyad ChordBot (Single A)]

> paths (Pentad (Single E) (Single G) (Single B) (Single D) (Pentad (Single E) (Single G) (Single B) (Single D) ChordBot))
[Pentad (Single E) ChordBot ChordBot ChordBot ChordBot,Pentad ChordBot (Single G) ChordBot ChordBot ChordBot,Pentad ChordBot ChordBot (Single B) ChordBot ChordBot,Pentad ChordBot ChordBot ChordBot (Single D) ChordBot,Pentad ChordBot ChordBot ChordBot ChordBot (Pentad (Single E) ChordBot ChordBot ChordBot ChordBot),Pentad ChordBot ChordBot ChordBot ChordBot (Pentad ChordBot (Single G) ChordBot ChordBot ChordBot),Pentad ChordBot ChordBot ChordBot ChordBot (Pentad ChordBot ChordBot (Single B) ChordBot ChordBot),Pentad ChordBot ChordBot ChordBot ChordBot (Pentad ChordBot ChordBot ChordBot (Single D) ChordBot)]

> paths (Dyad (Triad (Single E) (Single G) (Single B)) (Pentad (Single E) (Single G) (Single B) (Single D) ChordBot))
[Dyad (Triad (Single E) ChordBot ChordBot) ChordBot,Dyad (Triad ChordBot (Single G) ChordBot) ChordBot,Dyad (Triad ChordBot ChordBot (Single B)) ChordBot,Dyad ChordBot (Pentad (Single E) ChordBot ChordBot ChordBot ChordBot),Dyad ChordBot (Pentad ChordBot (Single G) ChordBot ChordBot ChordBot),Dyad ChordBot (Pentad ChordBot ChordBot (Single B) ChordBot ChordBot),Dyad ChordBot (Pentad ChordBot ChordBot ChordBot (Single D) ChordBot)]

-}

-- it's high time we had some pretty-printing...

ppn :: Note -> String
ppn n = show n

ppnL :: [Note] -> [String]
ppnL ns = map ppn ns

ppc :: Chord -> String
ppc ChordBot           = "*"
ppc (Single n)         = ppn n
ppc (Dyad c d)         = "(2" ++ (ppc c) ++ (ppc d) ++ ")"
ppc (Triad c d e)      = "(3" ++ (ppc c) ++ (ppc d) ++ (ppc e) ++ ")"
ppc (Tetrad c d e f)   = "(4" ++ (ppc c) ++ (ppc d) ++ (ppc e) ++ (ppc f) ++ ")"
ppc (Pentad c d e f g) = "(5" ++ (ppc c) ++ (ppc d) ++ (ppc e) ++ (ppc f) ++ (ppc g) ++ ")"

ppcL :: [Chord] -> [String]
ppcL as = map ppc as

{- EXAMPLES

> ppcL $ paths ChordBot
["*"]

> ppcL $ paths (Single A)
["A"]

> ppcL $ paths (Dyad ChordBot (Single A))
["(2*A)"]

> ppcL $ paths (Pentad (Single E) (Single G) (Single B) (Single D) (Pentad (Single E) (Single G) (Single B) (Single D) ChordBot))
["(5E****)","(5*G***)","(5**B**)","(5***D*)","(5****(5E****))","(5****(5*G***))","(5****(5**B**))","(5****(5***D*))"]

> ppcL $ paths (Dyad (Triad (Single E) (Single G) (Single B)) (Pentad (Single E) (Single G) (Single B) (Single D) ChordBot))
["(2(3E**)*)","(2(3*G*)*)","(2(3**B)*)","(2*(5E****))","(2*(5*G***))","(2*(5**B**))","(2*(5***D*))"]

-}