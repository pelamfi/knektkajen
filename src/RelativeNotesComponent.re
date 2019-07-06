open RelativeNotesState;
open ReactUtil;
open Synth;

let noteChangeListenerEffect = (dispatch: RelativeNotesState.acceptEvent, setCurrentNote: ((Note.note) => unit), _): option(unit => unit) => {
    let synthRef: ref(option(synth)) = ref(None);
    let listener = (stateChange): unit => {
      switch (stateChange) {
      | CurrentNoteChanged(currentNote) =>
        setCurrentNote(currentNote);
        switch (synthRef^) {
        | None =>
          // Webaudio can be initialized only after user input
          let synth = makeSynth();
          synthRef := Some(synth);
          play(synth, Note.frequency(currentNote));
        | Some(synth) => play(synth, Note.frequency(currentNote))
        };
      };
    };
    dispatch(RegisterListener(listener));
    Some(() => dispatch(UnregisterListener(listener)));
  };

let componentFactory = (dispatch: RelativeNotesState.acceptEvent, i: int, current: int, id: string): reactComponent => {
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

[@react.component]
let make = () => {

  let sliderConfig: InfiniteSlider.config = {
    componentFactory: componentFactory(dispatch),
    styleBaseName: "relativeNotes",
    componentBaseName: "relativeNotes",
    itemSelectedDispatch: i => {
      let note: Note.note = {offset: i};
      dispatch(ClickNote(note));
    },
    itemsWindow: RangeOfInt.make((-12) * 2, 12 * 2),
    maxJump: 12,
  };

  let (currentNote, setCurrentNote) = React.useReducer((_, x) => x, initialState.currentNote);
    
  React.useEffect0(noteChangeListenerEffect(dispatch, setCurrentNote));

  React.useEffect0(Keyboard.listenerEffect(dispatch));

  <InfiniteSlider config=sliderConfig selected={currentNote.offset} />;
};