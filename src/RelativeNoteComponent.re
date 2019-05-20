
open Note
open RelativeNotesState

let component = ReasonReact.statelessComponent("RelativeNoteComponent");

let make = (~note: note, ~state: state, ~acceptEvent: acceptEvent, _children) => {
  ...component,

  render: _ => { 
    let className = state.currentNote == note ? "noteCell current" :  "noteCell";
    <div className={className} onClick={_ => acceptEvent(ClickNote(note))}>
    {ReasonReact.string(asString(name(note, cMajorName)))}
    </div>
  },
};
