
open Note
open RelativeNotesState

let resultComponent = (contents) => <div className="testCell">
    contents
  </div>

/* Action declaration */
type action =
  | Click
  | Toggle;

let component = ReasonReact.statelessComponent("RelativeNoteComponent");

let make = (~note: note, ~state: state, ~ acceptEvent: acceptEvent, _children) => {
  ...component,

  render: _ => { 
    let className = state.currentNote == note ? "noteCell current" :  "noteCell notCurrent";
    <div className={className} onClick={_ => acceptEvent(ClickNote(note))}>
    {ReasonReact.string(asString(name(note, cMajorName)))}
    </div>
  },
};
