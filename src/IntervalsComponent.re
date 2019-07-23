open Note;
open RelativeNotesState;
open ReactUtil;

let makeIntervalComponent = (interval: interval): reactComponent => {
  <div className="intervalCell">{ReasonReact.string(string_of_int(interval.steps))}</div>
};

[@react.component]
let make =
    () => {

  let elements: list(reactComponent) = RelativeNotesState.intervalsInUi |> Belt.List.map(_, makeIntervalComponent);

  <div id="intervalsRowWrapper" className="intervalsRowWrapper">
  <div id="intervalsRow" className="intervalsRow">{asReact(elements)}
  </div>
  </div>;
};