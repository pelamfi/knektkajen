open Note;
open RelativeNotesState;

let component = ReasonReact.statelessComponent("RelativeNoteComponent");

let make = (~note: note, ~current: bool, ~acceptEvent: acceptEvent, _children) => {
  ...component,

  render: _ => {
    let className =
      current ? "noteCell current" : "noteCell";
    <div className onClick={_ => acceptEvent(ClickNote(note))}>
      {ReasonReact.string(asString(name(note, cMajorName)))}
    </div>
  },
}
