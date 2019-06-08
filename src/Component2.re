type state = {
  count: int,
  showTicker: bool,
};

let resultComponent = contents => <div className="testCell"> contents </div>;

type action =
  | Click
  | Toggle;

[@react.component]
let make = (~greeting) => {
  let (state, dispatch) =
    React.useReducer(
      (state: state, action: action) =>
        switch (action) {
        | Click => {...state, count: state.count + 1}
        | Toggle => {...state, showTicker: !state.showTicker}
        },
      {count: 0, showTicker: false},
    );

  let message =
    greeting
    ++ " You've clicked this div "
    ++ string_of_int(state.count)
    ++ " times(s)\n";

  <>
    <button className="testCell" onClick={_event => dispatch(Click)}>
      {ReasonReact.string(message)}
    </button>
    <button className="testCell" onClick={_event => dispatch(Toggle)}>
      {ReasonReact.string("Toggle ticker")}
    </button>
    {state.showTicker
       ? resultComponent(<Ticker greeting="A ticker! " />)
       : resultComponent(ReasonReact.string("Hi! No ticker!"))}
  </>;
};