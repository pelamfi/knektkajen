type state = {
  ticks: int,
  intervalId: option(Js.Global.intervalId),
};

let resultComponent = _greeting =>
  <div className="testCell"> {ReasonReact.string(_greeting)} </div>;

type action =
  | Tick
  | Mount(Js.Global.intervalId);

let component = ReasonReact.reducerComponent("Ticker");

let make = (~greeting, _children) => {
  ...component,

  initialState: () => {ticks: 0, intervalId: None},

  didMount: self => {
    let intervalId =
      Js.Global.setInterval(
        () => {
          Js.log("Tick");
          self.send(Tick);
        },
        500,
      );

    self.send(Mount(intervalId));
  },
  willUnmount: self => {
    self.state.intervalId
    |> Belt.Option.map(_, Js.Global.clearInterval)
    |> ignore;
  },

  reducer: (action, state) =>
    switch (action) {
    | Tick => ReasonReact.Update({...state, ticks: state.ticks + 1})
    | Mount(id) => ReasonReact.Update({...state, intervalId: Some(id)})
    },

  render: self => {
    let message =
      greeting ++ string_of_int(self.state.ticks) ++ " ticks have passed";
    <Fragment> {ReasonReact.string(message)} </Fragment>;
  },
};