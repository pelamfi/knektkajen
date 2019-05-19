open RelativeNotesState
open Note

let notesBoxNotes  = (state: state): list(note) => Note.range(state.currentNote, 12)

