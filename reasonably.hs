data Note = A | As | B | C | Cs | D | Ds | E | F | Fs | G | Gs 
    deriving (Eq, Enum, Ord, Show, Read, Bounded)

steps :: Note -> Int -> Note
steps x m = (toEnum (((fromEnum x) + m) `mod` 12))    

majTriad :: Note -> (Note , Note , Note)
majTriad x = (x , x `steps` 4 , x `steps` 7)

minTriad :: Note -> (Note , Note , Note)
minTriad x = (x , x `steps` 3 , x `steps` 7)

augTriad :: Note -> (Note , Note , Note)
augTriad x = (x , x `steps` 4 , x `steps` 8)

dimTriad :: Note -> (Note , Note , Note)
dimTriad x = (x , x `steps` 3 , x `steps` 6)

majorScale :: Note -> (Note, Note, Note, Note, Note, Note, Note)
majorScale x = (x, x `steps` 2, x `steps` 4, x `steps` 5, x `steps` 7, x `steps` 9, x `steps` 11)

minorHarmonicScale :: Note -> (Note, Note, Note, Note, Note, Note, Note)
minorHarmonicScale x = (x, x `steps` 2, x `steps` 3, x `steps` 5, x `steps` 7, x `steps` 8, x `steps` 11)

minorMelodicScale :: Note -> (Note, Note, Note, Note, Note, Note, Note)
minorMelodicScale x = (x, x `steps` 2, x `steps` 3, x `steps` 5, x `steps` 7, x `steps` 9, x `steps` 11)

chromaticScale :: Note -> (Note, Note, Note, Note, Note, Note, Note, Note, Note, Note, Note, Note)
chromaticScale x = (x, x `steps` 1, x `steps` 2, x `steps` 3, x `steps` 4, x `steps` 5, x `steps` 6, x `steps` 7, x `steps` 8, x `steps` 9, x `steps` 10, x `steps` 11)

wholeToneScale :: Note -> (Note, Note, Note, Note, Note, Note)
wholeToneScale x = (x, x `steps` 2, x `steps` 4, x `steps` 6, x `steps` 8, x `steps` 10)
