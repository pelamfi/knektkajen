
let noteChangeListenerEffect = (setCurrentNote: ((Note.note) => unit)): ((RelativeNotesState.acceptEvent) => (unit => option(unit => unit))) => {
  RelativeNotesState.listenerEffect(stateChange => {
      switch (stateChange) {
      | CurrentNoteChanged(currentNote) => setCurrentNote(currentNote);
      | _ => ()
      };
  })
};

[@react.component]
let make = (~debugModes: DebugMode.debugModes) => {
  let (currentNote, setCurrentNote) = React.useReducer((_, x) => x, RelativeNotesState.initialState.currentNote);

  React.useEffect0(noteChangeListenerEffect(setCurrentNote, RelativeNotesState.dispatch));

  React.useEffect0(Synth.effect(RelativeNotesState.dispatch));

  React.useEffect0(Keyboard.listenerEffect(RelativeNotesState.dispatch));

  let (style, styleChild) = if (Belt.Set.has(debugModes, NoteInfoStrips2xZoom)) {
    (ReactDOMRe.Style.make(~transform="scale(0.5)", ()), ReactDOMRe.Style.make(~overflow="visible", ()))
  } else {
    (ReactUtil.emptyStyle, ReactUtil.emptyStyle)
  };

  let foo = ReactDOMRe.Style.make(());

  <div className="noteInfoStrips" style>
    <IntervalsComponent />
    <NoteNamesComponent style=styleChild currentNote/>
    <NoteNumberComponent style=styleChild currentNote/>
    
  </div>;
};