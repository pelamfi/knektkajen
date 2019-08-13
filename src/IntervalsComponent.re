open Note;
open RelativeNotesState;
open ReactUtil;

let makeIntervalOffsetComponent = (acceptEvent: acceptEvent, interval: interval): reactComponent => {
  <div className="intervalCell" onClick={_ => acceptEvent(NoteTrigger(IntervalClick(interval, MouseClick)))}>{ReasonReact.string(string_of_int(interval.steps))}</div>
};

let makeIntervalNameComponent = (acceptEvent: acceptEvent, interval: interval): reactComponent => {
  <div className="intervalCell" onClick={_ => acceptEvent(NoteTrigger(IntervalClick(interval, MouseClick)))}>{ReasonReact.string(Note.nameOfInterval(interval))}</div>
};

[@react.component]
let make =
    () => {

  let intervalOffsetElements: list(reactComponent) = RelativeNotesState.intervalsInUi |> Belt.List.map(_, makeIntervalOffsetComponent(RelativeNotesState.dispatch));
  let intervalNameElements: list(reactComponent) = RelativeNotesState.intervalsInUi |> Belt.List.map(_, makeIntervalNameComponent(RelativeNotesState.dispatch));

  <div id="intervalsRowWrapper" className="intervalsRowWrapper">
  <div id="intervalsRow" className="intervalsRow">{asReact(intervalOffsetElements)}
  </div>
  <div id="intervalsRow" className="intervalsRow">{asReact(intervalNameElements)}
  </div>
  </div>;
};