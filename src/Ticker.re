type state = {
  ticks: int,
  intervalId: option(Js.Global.intervalId),
};

let resultComponent = (_greeting) => <div className="testCell">
    {ReasonReact.string(_greeting)}
  </div>

type action =
  | Tick
  | Mount(Js.Global.intervalId);

let component = ReasonReact.reducerComponent("Ticker");

let make = (~greeting, _children) => {
  ...component,

  initialState: () => {ticks: 0, intervalId: None},

  didMount: self => {self.send(Mount(Js.Global.setInterval({() => {Js.log("Tick");self.send(Tick)}}, 2000)))},
  willUnmount: self => {
      self.state.intervalId |> Belt.Option.map(_, Js.Global.clearInterval) |> ignore
      
      Js.log("clearInterval")
    },

  reducer: (action, state) =>
    switch (action) {
    | Tick => ReasonReact.Update({...state, ticks: state.ticks + 1})
    | Mount(id) => ReasonReact.Update({...state, intervalId: Some(id)})
    },

  render: self => {
    let message = greeting ++ string_of_int(self.state.ticks) ++ " ticks have passed";
    <Fragment>
      {ReasonReact.string(message)}
    </Fragment>
  },
};
