
let component = ReasonReact.statelessComponent("RelativeNotesComponent");

let make = (_children) => {
  ...component,
  render: _ => {
    let notes = Note.(scale(middleC, Chromatic));
    let noteElems = Belt.List.map(notes, x => {
      <RelativeNoteComponent key={string_of_int(x.offset)} note={x}/>
    });
    <Fragment>
      <div className="relativeNotesRow">
      (ReasonReact.array(Belt.List.toArray(noteElems)))
      </div>
    </Fragment>
  },
};
