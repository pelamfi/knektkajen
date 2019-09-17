open Note;
open RelativeNotesState;

[@react.component]
let make =
    (
      ~note: note,
      ~current: bool,
      ~acceptEvent as _: acceptEvent,
      ~id: string,
      ~cssClass: string,
      ~renderContent: note => string,
    ) => {
  let className =
    current
      ? cssClass ++ " noteInfoCell current" : cssClass ++ " noteInfoCell";
  let renderedNote: string = renderContent(note);
  <div id className> {ReasonReact.string(renderedNote)} </div>;
};