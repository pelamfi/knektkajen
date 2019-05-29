type state = {
  count: int,
  showTicker: bool,
};

let resultComponent = contents => <div className="testCell"> contents </div>;

/* Action declaration */
type action =
  | Click
  | Toggle;


[@react.component]
let make = (~greeting) => {

  <div/>
  /*
  initialState: () => {count: 0, showTicker: false},

  reducer: (action, state) =>
    switch (action) {
    | Click => ReasonReact.Update({...state, count: state.count + 1})
    | Toggle => ReasonReact.Update({...state, showTicker: !state.showTicker})
    },

  render: self => {
    let message =
      "You've clicked this div "
      ++ string_of_int(self.state.count)
      ++ " times(s)\n";

    <fragment>
      <button className="testCell" onClick={_event => self.send(Click)}>
        {ReasonReact.string(message)}
      </button>
      <button className="testCell" onClick={_event => self.send(Toggle)}>
        {ReasonReact.string("Toggle ticker")}
      </button>
      {self.state.showTicker
         ? resultComponent(<Ticker greeting="A ticker! " />)
         : resultComponent(ReasonReact.string("Hi! No ticker!"))}
    </fragment>
  },
  */
};