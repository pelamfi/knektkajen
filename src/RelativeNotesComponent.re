open RelativeNotesState

let component = ReasonReact.reducerComponent("RelativeNotesComponent");


let make = (_children) => {
  ...component,

  initialState: () => initialState,
  reducer: (event, state) => ReasonReact.Update(updateState(event, state)),

  render: self => {
    let acceptEvent: acceptEvent = self.send
    let notes = Note.(scale(middleC, Chromatic));
    let noteElems = Belt.List.map(notes, x => {
      <RelativeNoteComponent state={self.state} acceptEvent={acceptEvent} key={string_of_int(x.offset)} note={x}/>
    });
    <Fragment>
      <div className="relativeNotesRow">
      (ReasonReact.array(Belt.List.toArray(noteElems)))
      </div>
    </Fragment>
  },
};
