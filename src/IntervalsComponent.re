open Note;
open RelativeNotesState;
open ReactUtil;

let makeIntervalComponent = (acceptEvent: acceptEvent, interval: interval): reactComponent => {
  <div className="intervalCell" onClick={_ => acceptEvent(NoteTrigger(IntervalClick(interval, MouseClick)))}>{ReasonReact.string(string_of_int(interval.steps))}</div>
};

[@react.component]
let make =
    () => {

  let elements: list(reactComponent) = RelativeNotesState.intervalsInUi |> Belt.List.map(_, makeIntervalComponent(RelativeNotesState.dispatch));

  <div id="intervalsRowWrapper" className="intervalsRowWrapper">
  <div id="intervalsRow" className="intervalsRow">{asReact(elements)}
  </div>
  </div>;
};