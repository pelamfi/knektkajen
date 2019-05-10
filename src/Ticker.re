type state = {
  ticks: int,
  intervalId: option(Js_global.intervalId),
};

let resultComponent = (_greeting) => <div className="testCell">
    {ReasonReact.string(_greeting)}
  </div>

type action =
  | Tick
  | Mount(Js_global.intervalId)
  | UnMount;

let component = ReasonReact.reducerComponent("Ticker");

let make = (~greeting, _children) => {
  ...component,

  initialState: () => {ticks: 0, intervalId: None},

  didMount: self => {self.send(Mount(Js_global.setInterval({() => {Js.log("Tick");self.send(Tick)}}, 2000)))},
  willUnmount: self => {
      switch (self.state.intervalId) {
        |Some(id) => 
        Js.log("clearInterval")
        Js_global.clearInterval(id)
        |None => ()
      }
    },

  reducer: (action, state) =>
    switch (action) {
    | Tick => ReasonReact.Update({...state, ticks: state.ticks + 1})
    | Mount(id) => ReasonReact.Update({...state, intervalId: Some(id)})
    | UnMount => {

      ReasonReact.Update({...state, intervalId: None})
      }
    },

  render: self => {
    let message = greeting ++ string_of_int(self.state.ticks) ++ " ticks have passed";
    <Fragment>
      {ReasonReact.string(message)}
    </Fragment>
  },
};
