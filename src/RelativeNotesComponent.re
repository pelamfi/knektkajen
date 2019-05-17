
let component = ReasonReact.statelessComponent("RelativeNotesComponent");

let make = (_children) => {
  ...component,
  render: _ => {
    let notes = Note.(scale(middleC, Chromatic));
    let noteElems =Belt.List.map(notes, x => {
      <div className="noteCell">{ReasonReact.string(string_of_int(x.offset))}</div>
    });
    <Fragment>
      <div className="relativeNotesRow">
      (ReasonReact.array(Belt.List.toArray(noteElems)))
      </div>
    </Fragment>
  },
};
