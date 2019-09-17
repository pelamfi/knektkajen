open ReactUtil;

let noteChangeListenerEffect = (setCurrentNote: ((Note.note) => unit)): ((RelativeNotesState.acceptEvent) => (unit => option(unit => unit))) => {
  RelativeNotesState.listenerEffect(stateChange => {
      switch (stateChange) {
      | CurrentNoteChanged(currentNote) => setCurrentNote(currentNote);
      | _ => ()
      };
  })
};

let noteNameFactory = (dispatch: RelativeNotesState.acceptEvent, i: int, current: int, id: string): reactComponent => {
  let current = current == i;
  let note: Note.note = RelativeNotesState.loopOctaves({offset: i});
  <RelativeNoteComponent
    current
    acceptEvent=dispatch
    key={string_of_int(note.offset)}
    cssClass="noteNameCell"
    renderContent=Note.nameOfNoteInCMajor
    id
    note
  />;
};

let sliderConfig = (dispatch: RelativeNotesState.acceptEvent): InfiniteSlider.config => {
  componentFactory: noteNameFactory(dispatch),
  styleBaseName: "relativeNotes",
  componentBaseName: "relativeNotes",
  itemSelectedDispatch: i => {
    dispatch(NoteTrigger(NoteClick({offset: i}, MouseClick)));
  },
  itemsWindow: RelativeNotesState.intervalStepsInUi,
  maxJump: 12,
};

[@react.component]
let make = () => {
  let (currentNote, setCurrentNote) = React.useReducer((_, x) => x, RelativeNotesState.initialState.currentNote);
    
  React.useEffect0(noteChangeListenerEffect(setCurrentNote, RelativeNotesState.dispatch));

  React.useEffect0(Synth.effect(RelativeNotesState.dispatch));

  React.useEffect0(Keyboard.listenerEffect(RelativeNotesState.dispatch));

  <InfiniteSlider config={sliderConfig(RelativeNotesState.dispatch)} selected={currentNote.offset} />;
};