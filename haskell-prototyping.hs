import Data.List -- for sorting lists

{-
The first base datatypes that come to mind regarding this project are an enumeration type for all twelve notes, and the positive reals (or maybe, the positive integers will do in practice, don't know) representing frequencies. Let's concentrate first on the notes
-}

{-NOTES AND FUNCTIONS OVER THEM-}

data Note = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs
    deriving (Eq, Enum, Ord, Show, Read, Bounded)
    
{-
Having notes, we can define intervals, certain triads, and more, as values of appropriate higher datatypes, for example, by functions outputing lists.
-}

-- an auxiliary function that gives the note m halfsteps above the note x

halfsteps :: Note -> Int -> Note
halfsteps x m = (toEnum (((fromEnum x) + m) `mod` 12))    

-- intervals

int1p :: Note -> [Note]
int1p x = [x, x `halfsteps` 0]

int2m :: Note -> [Note]
int2m x = [x, x `halfsteps` 1]

int2M :: Note -> [Note]
int2M x = [x, x `halfsteps` 2]

int3m :: Note -> [Note]
int3m x = [x, x `halfsteps` 3]

int3M :: Note -> [Note]
int3M x = [x, x `halfsteps` 4]

int4p :: Note -> [Note]
int4p x = [x, x `halfsteps` 5]

int4a :: Note -> [Note]
int4a x = [x, x `halfsteps` 6]

int5p :: Note -> [Note]
int5p x = [x, x `halfsteps` 7]

int6m :: Note -> [Note]
int6m x = [x, x `halfsteps` 8]

int6M :: Note -> [Note]
int6M x = [x, x `halfsteps` 9]

int7m :: Note -> [Note]
int7m x = [x, x `halfsteps` 10]

int7M :: Note -> [Note]
int7M x = [x, x `halfsteps` 11]

-- triads

majTriad :: Note -> [Note]
majTriad x = [x , x `halfsteps` 4 , x `halfsteps` 7]

minTriad :: Note -> [Note]
minTriad x = [x , x `halfsteps` 3 , x `halfsteps` 7]

augTriad :: Note -> [Note]
augTriad x = [x , x `halfsteps` 4 , x `halfsteps` 8]

dimTriad :: Note -> [Note]
dimTriad x = [x , x `halfsteps` 3 , x `halfsteps` 6]

-- scales

majorScale :: Note -> [Note]
majorScale x = [x, x `halfsteps` 2, x `halfsteps` 4, x `halfsteps` 5, x `halfsteps` 7, x `halfsteps` 9, x `halfsteps` 11]

minorHarmonicScale :: Note -> [Note]
minorHarmonicScale x = [x, x `halfsteps` 2, x `halfsteps` 3, x `halfsteps` 5, x `halfsteps` 7, x `halfsteps` 8, x `halfsteps` 11]

minorMelodicScale :: Note -> [Note]
minorMelodicScale x = [x, x `halfsteps` 2, x `halfsteps` 3, x `halfsteps` 5, x `halfsteps` 7, x `halfsteps` 9, x `halfsteps` 11]

chromaticScale :: Note -> [Note]
chromaticScale x = [x, x `halfsteps` 1, x `halfsteps` 2, x `halfsteps` 3, x `halfsteps` 4, x `halfsteps` 5, x `halfsteps` 6, x `halfsteps` 7, x `halfsteps` 8, x `halfsteps` 9, x `halfsteps` 10, x `halfsteps` 11]

wholeToneScale :: Note -> [Note]
wholeToneScale x = [x, x `halfsteps` 2, x `halfsteps` 4, x `halfsteps` 6, x `halfsteps` 8, x `halfsteps` 10]

{-REPRESENTING CHORDS-}

{-
So, one way to speak about chords is to view them as values of [Note].
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
There is a bunch of functions to be defined on any inductive datatype. Note that the constructor "Single" takes values of "Note" as arguments, and it seems intuitive to view it as nullary in the definitions that follow.
-}

-- the size of a token (i.e., a chord)

sizeOfChordToken :: Chord -> Int
sizeOfChordToken ChordBot = 0
sizeOfChordToken (Single n) = 1
sizeOfChordToken (Dyad c d) = 1 + (sizeOfChordToken c) + (sizeOfChordToken d)
sizeOfChordToken (Triad c d e) = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e)
sizeOfChordToken (Tetrad c d e f) = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e) + (sizeOfChordToken f)
sizeOfChordToken (Pentad c d e f g) = 1 + (sizeOfChordToken c) + (sizeOfChordToken d) + (sizeOfChordToken e) + (sizeOfChordToken f) + (sizeOfChordToken g)

-- the height of a token

heightOfChordToken :: Chord -> Int
heightOfChordToken ChordBot = 0
heightOfChordToken (Single n) = 1
heightOfChordToken (Dyad c d) = 1 + maximum [heightOfChordToken c, heightOfChordToken d]
heightOfChordToken (Triad c d e) = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e]
heightOfChordToken (Tetrad c d e f) = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e, heightOfChordToken f]
heightOfChordToken (Pentad c d e f g) = 1 + maximum [heightOfChordToken c, heightOfChordToken d, heightOfChordToken e, heightOfChordToken f, heightOfChordToken g]

-- the size of the chord, that is, its number of notes

sizeOfChord :: Chord -> Int
sizeOfChord ChordBot = 0
sizeOfChord (Single n) = 1
sizeOfChord (Dyad c d) = (sizeOfChord c) + (sizeOfChord d)
sizeOfChord (Triad c d e) = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e)
sizeOfChord (Tetrad c d e f) = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e) + (sizeOfChord f)
sizeOfChord (Pentad c d e f g) = (sizeOfChord c) + (sizeOfChord d) + (sizeOfChord e) + (sizeOfChord f) + (sizeOfChord g)

-- the yield of a token, that is, its leaves, that is, its actual notes

yieldOfChord :: Chord -> [Note]
yieldOfChord ChordBot = []
yieldOfChord (Single n) = [n]
yieldOfChord (Dyad c d) = (yieldOfChord c) ++ (yieldOfChord d)
yieldOfChord (Triad c d e) = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e)
yieldOfChord (Tetrad c d e f) = (yieldOfChord c) ++ (yieldOfChord d) ++ (yieldOfChord e) ++ (yieldOfChord f)
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
componentToken ChordBot _ = Nothing
componentToken (Single n) _ = Nothing
componentToken (Dyad c d) m
    | m < 1 || m > 2 = Nothing
    | m == 1 = Just c
    | m == 2 = Just d
componentToken (Triad c d e) m
    | m < 1 || m > 3 = Nothing
    | m == 1 = Just c
    | m == 2 = Just d
    | m == 3 = Just e
componentToken (Tetrad c d e f) m
    | m < 1 || m > 4 = Nothing
    | m == 1 = Just c
    | m == 2 = Just d
    | m == 3 = Just e
    | m == 4 = Just f
componentToken (Pentad c d e f g) m
    | m < 1 || m > 5 = Nothing
    | m == 1 = Just c
    | m == 2 = Just d
    | m == 3 = Just e
    | m == 4 = Just f
    | m == 5 = Just g

-- define the arity of a token; more general, and easier than declaring the arity of a constructor

arity :: Chord -> Int
arity ChordBot = 0
arity (Single n) = 1
arity (Dyad c d) = 2
arity (Triad c d e) = 3
arity (Tetrad c d e f) = 4
arity (Pentad c d e f g) = 5

-- define the list of component tokens of a given token

componentTokenList :: Chord -> [Chord]
componentTokenList ChordBot = []
componentTokenList (Single n) = []
componentTokenList (Dyad c d) = [c, d]
componentTokenList (Triad c d e) = [c, d, e]
componentTokenList (Tetrad c d e f) = [c, d, e, f]
componentTokenList (Pentad c d e f g) = [c, d, e, f, g]

-- define the list of all subtokens of a given token

subtokens :: Chord -> [Chord]
subtokens ChordBot = [ChordBot]
subtokens (Single n) = [Single n]
subtokens (Dyad c d) = [Dyad c d] ++ (subtokens c) ++ (subtokens d)
subtokens (Triad c d e) = [Triad c d e] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e)
subtokens (Tetrad c d e f) = [Tetrad c d e f] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e) ++ (subtokens f)
subtokens (Pentad c d e f g) = [Pentad c d e f g] ++ (subtokens c) ++ (subtokens d) ++ (subtokens e) ++ (subtokens f) ++ (subtokens g)


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
consistent c ChordBot = True
consistent ChordBot c = True
consistent c d = (headToken c == headToken d) && (consistent_lift (componentTokenList c) (componentTokenList d))

consistent_lift :: [Chord] -> [Chord] -> Bool
consistent_lift [] [] = True
consistent_lift (c:cs) (d:ds) = (consistent c d) && (consistent_lift cs ds)
consistent_lift _ _ = False

{-examples-}

{-

> c_major_extended `consistent` a_over_c_major
True

> c_major_extended `consistent` e_minor_over_c
False

-}

consistentList :: [Chord] -> Bool
consistentList [] = True
consistentList [c] = True
consistentList (c:d:cs) = (consistent c d) && (consistentList (d:cs))

{-examples-}

{-

> consistentList [c_major_extended, a_over_c_major, e_minor_over_c]
False

> consistentList [c_major_extended, a_over_c_major]
True

-}