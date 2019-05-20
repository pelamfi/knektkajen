open RelativeNotesState;
open Note;

let notesBoxNotes = (state: state): list(note) =>
  range(state.currentNote, (-12) * 2, 12 * 2);