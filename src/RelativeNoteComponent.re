open Note;
open RelativeNotesState;

[@react.component]
let make =
    (~note: note, ~current: bool, ~acceptEvent: acceptEvent, ~id: string) => {
  let className = current ? "noteCell current" : "noteCell";
  let noteName: string = nameOfNoteInCMajor(note);
  <div id className> {ReasonReact.string(noteName)} </div>;
};