open Note;
open RelativeNotesState;

[@react.component]
let make = (~note: note, ~current: bool, ~acceptEvent: acceptEvent, ~id: string) => {
  let className = current ? "noteCell current" : "noteCell";
  <div id={id} className onClick={_ => acceptEvent(ClickNote(note))}>
    {ReasonReact.string(asString(name(note, cMajorName)))}
  </div>;
};