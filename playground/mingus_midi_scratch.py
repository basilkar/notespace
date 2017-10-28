from mingus.midi import fluidsynth
from mingus.containers import Note
import time

a = fluidsynth.init("../soundfonts/GeneralUser GS v1.471.SF2")
print(a)

b = fluidsynth.play_Note(Note("C-5"))
time.sleep(1.0)
print(b)
c = fluidsynth.play_Note(Note("E-5"))
time.sleep(1.0)
print(c)