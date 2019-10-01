
let noteChangeListenerEffect = (setCurrentNote: ((Note.note) => unit)): ((RelativeNotesState.acceptEvent) => (unit => option(unit => unit))) => {
  RelativeNotesState.listenerEffect(stateChange => {
      switch (stateChange) {
      | CurrentNoteChanged(currentNote) => setCurrentNote(currentNote);
      | _ => ()
      };
  })
};

[@react.component]
let make = () => {
  let (currentNote, setCurrentNote) = React.useReducer((_, x) => x, RelativeNotesState.initialState.currentNote);

  React.useEffect0(noteChangeListenerEffect(setCurrentNote, RelativeNotesState.dispatch));

  React.useEffect0(Synth.effect(RelativeNotesState.dispatch));

  React.useEffect0(Keyboard.listenerEffect(RelativeNotesState.dispatch));

  <>
    <IntervalsComponent />
    <NoteNamesComponent currentNote/>
    <NoteNumberComponent currentNote/>
    
  </>;
};