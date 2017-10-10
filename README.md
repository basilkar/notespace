# notespace

Next steps:

~~(1)~~ implement some string metric (possibly the Levenshtein one): haskell-prototyping.hs:levenshtein  
(2) figure out how to set up the threepenny gui  
(3) implement spaces such as the circle of fifths and the tonnetz  
(4) figure out applications for the type of chord constructions  

Features:

~~(1)~~ input a set of notes, output fitting scales: haskell-prototyping.hs: improScale  
~~(2)~~ input a scale, output its chords: haskell-prototyping.hs: scalechords  
(3) input a melody, output a harmonization (hard)  
(4) input keys X, Y and an n, output n keys on a tonnetz/circle-of-fifths/... geodesic of X & Y  
(5) input a chord progression, output its negative (in the sense of Ernst Levy)  
~~(6)~~ recognize known chords and scales: haskell-prototyping.hs: icsRecognizer  