open Belt

let noteChangeListenerEffect = (setCurrentNote: ((Note.note) => unit)): ((RelativeNotesState.acceptEvent) => (unit => option(unit => unit))) => {
  RelativeNotesState.listenerEffect(stateChange => {
      switch (stateChange) {
      | CurrentNoteChanged(currentNote) => setCurrentNote(currentNote);
      | _ => ()
      };
  })
};

let intervalClickHandler = (layoutGridInfo: option(LayoutGridProbe.layoutGridInfo), click: ReactEvent.Mouse.t) => {
  Option.map(
    layoutGridInfo,
    layoutGridInfo => {
      let clickX = float_of_int(ReactEvent.Mouse.clientX(click));

      let gridCell = int_of_float((clickX -. layoutGridInfo.leftX) /. layoutGridInfo.pitchX);

      List.get(RelativeNotesState.intervalsInUi, gridCell) |> 
        Option.map(_, interval => {RelativeNotesState.dispatch(NoteTrigger(IntervalClick(interval, MouseClick)))})
        |> ignore;
    }
  )
  |> ignore;
};


[@react.component]
let make = (~debugModes: DebugMode.debugModes) => {
  let (currentNote, setCurrentNote) = React.useReducer((_, x) => x, RelativeNotesState.initialState.currentNote);

  React.useEffect0(noteChangeListenerEffect(setCurrentNote, RelativeNotesState.dispatch));

  React.useEffect0(Synth.effect(RelativeNotesState.dispatch));

  React.useEffect0(Keyboard.listenerEffect(RelativeNotesState.dispatch));

  let (layoutGridInfo: option(LayoutGridProbe.layoutGridInfo), setLayoutGridInfo: LayoutGridProbe.layoutGridInfoCallback) = React.useReducer((_, x) => {Some(x)}, None);

  let intervalClickHandler = intervalClickHandler(layoutGridInfo)

  let (style, styleChild) = if (Set.has(debugModes, NoteInfoStrips2xZoom)) {
    (ReactDOMRe.Style.make(~transform="scale(0.5)", ()), ReactDOMRe.Style.make(~overflow="visible", ()))
  } else {
    (ReactUtil.emptyStyle, ReactUtil.emptyStyle)
  };

  <div onClick=intervalClickHandler className="noteInfoStrips" style>
    <IntervalsComponent />
    <NoteNamesComponent style=styleChild currentNote/>
    <NoteNumberComponent style=styleChild currentNote/>
    <LayoutGridProbe infoCallback=setLayoutGridInfo debugModes/>
  </div>;
};