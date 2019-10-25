open Note;
open RelativeNotesState;
open ReactUtil;

let makeIntervalOffsetComponent = (acceptEvent: acceptEvent, interval: interval): reactComponent => {
  <div className="intervalStepsCell" key={string_of_int(interval.steps)}>{ReasonReact.string(MathUtil.stringOfIntWithSign(interval.steps))}</div>
};

let makeIntervalNameComponent = (acceptEvent: acceptEvent, interval: interval): reactComponent => {
  <div className="intervalNameCell" key={string_of_int(interval.steps)}>
  <div className="intervalNameRotate">
  <div className="intervalName">
  {ReasonReact.string(Note.nameOfInterval(interval))}
  </div></div></div>
};

[@react.component]
let make =
    () => {

  let intervalOffsetElements: list(reactComponent) = RelativeNotesState.intervalsInUi |> Belt.List.map(_, makeIntervalOffsetComponent(RelativeNotesState.dispatch));
  let intervalNameElements: list(reactComponent) = RelativeNotesState.intervalsInUi |> Belt.List.map(_, makeIntervalNameComponent(RelativeNotesState.dispatch));

  <>

  <div className="intervalOffsetsStrip">
  <div className="intervalOffsetsRow">
  {asReact(intervalOffsetElements)}
  </div>
  </div>

  <div className="intervalNamesStrip">
  <div className="intervalNamesRow">
  {asReact(intervalNameElements)}
  </div>
  </div>

  </>;
};