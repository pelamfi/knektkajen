open ReactUtil;

open Synth;


let noteChangeListenerEffect = (setCurrentNote: ((Note.note) => unit)): ((RelativeNotesState.acceptEvent) => (unit => option(unit => unit))) => {
  RelativeNotesState.listenerEffect(stateChange => {
      switch (stateChange) {
      | CurrentNoteChanged(currentNote) => setCurrentNote(currentNote);
      };
  })
};

let synthEffect: ((RelativeNotesState.acceptEvent) => (unit => option(unit => unit))) = {
    let synthRef: ref(option(synth)) = ref(None);
    RelativeNotesState.listenerEffect(stateChange => {
      switch (stateChange) {
      | CurrentNoteChanged(currentNote) =>
        switch (synthRef^) {
        | None =>
          // Webaudio can be initialized only after user input
          let synth = makeSynth();
          synthRef := Some(synth);
          play(synth, Note.frequency(currentNote));
        | Some(synth) => play(synth, Note.frequency(currentNote))
        };
      };
    })
  };

let noteNameFactory = (dispatch: RelativeNotesState.acceptEvent, i: int, current: int, id: string): reactComponent => {
  let current = current == i;
  let note: Note.note = {offset: i};
  <RelativeNoteComponent
    current
    acceptEvent=dispatch
    key={string_of_int(note.offset)}
    id
    note
  />;
};

let sliderConfig = (dispatch: RelativeNotesState.acceptEvent): InfiniteSlider.config => {
  componentFactory: noteNameFactory(dispatch),
  styleBaseName: "relativeNotes",
  componentBaseName: "relativeNotes",
  itemSelectedDispatch: i => {
    let note: Note.note = {offset: i};
    dispatch(ClickNote(note));
  },
  itemsWindow: RangeOfInt.make((-12) * 2, 12 * 2),
  maxJump: 12,
};

[@react.component]
let make = () => {
  let (currentNote, setCurrentNote) = React.useReducer((_, x) => x, RelativeNotesState.initialState.currentNote);
    
  React.useEffect0(noteChangeListenerEffect(setCurrentNote, RelativeNotesState.dispatch));

  React.useEffect0(synthEffect(RelativeNotesState.dispatch));

  React.useEffect0(Keyboard.listenerEffect(RelativeNotesState.dispatch));

  <InfiniteSlider config={sliderConfig(RelativeNotesState.dispatch)} selected={currentNote.offset} />;
};