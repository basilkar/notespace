class TonnetzLogicalGrid:

    root = 0
    tones = ["A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#"]

    def node(self, i, j):
        index = (self.root + i*7 + j*8) % 12
        return self.tones[index]
