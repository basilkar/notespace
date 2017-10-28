import mingus.core.notes as notes
import mingus.core.keys as keys
import mingus.core.intervals as intervals


print(notes.is_valid_note('C#'))
print(notes.int_to_note(0)) # [0,..,11]


print(keys.get_notes("C"))

print(intervals.second("E", "C"))
print(intervals.determine("Gbb", "Ab"))
print(intervals.determine("Gbb", "Ab", True))
print(intervals.measure("C", "D"))
print(intervals.measure("D", "C"))