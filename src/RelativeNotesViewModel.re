open RelativeNotesState
open Note

let notesBoxNotes  = (state: state): list(note) => range(state.currentNote, -11, 12)

