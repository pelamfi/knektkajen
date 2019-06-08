type state = {
  ticks: int,
  intervalId: option(Js.Global.intervalId),
};

let resultComponent = _greeting =>
  <div className="testCell"> {ReasonReact.string(_greeting)} </div>;

type action =
  | Tick
  | Mount(Js.Global.intervalId);

[@react.component]
let make = (~greeting: string) => {
  let (state, dispatch) =
    React.useReducer(
      (state: state, action: action) =>
        // Saved here as a mememto: self.state.intervalId |> Belt.Option.map(_, Js.Global.clearInterval) |> ignore;
        switch (action) {
        | Tick => {...state, ticks: state.ticks + 1}
        | Mount(id) => {...state, intervalId: Some(id)}
        },
      {ticks: 0, intervalId: None},
    );

  React.useEffect0(() => {
    Js.log("set interval");
    let intervalId =
      Js.Global.setInterval(
        () => {
          Js.log("tick");
          dispatch(Tick);
        },
        500,
      );
    dispatch(Mount(intervalId));
    Some(() => Js.Global.clearInterval(intervalId));
  });

  let message =
    greeting ++ string_of_int(state.ticks) ++ " ticks have passed";

  ReasonReact.string(message);
};