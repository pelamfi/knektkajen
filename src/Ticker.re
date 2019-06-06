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
let make = (~greeting) => {  
  let (state, dispatch) =
    React.useReducer(
      (state: state, action: action) =>
      switch (action) {
      | Tick => {...state, ticks: state.ticks + 1}
      | Mount(id) => {...state, intervalId: Some(id)}
      }, {ticks: 0, intervalId: None});

/*
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
  },*/
  let message =
    greeting ++ string_of_int(state.ticks) ++ " ticks have passed";
   {ReasonReact.string(message)}
};