/* State declaration */
type state = {
  count: int,
  show: bool,
};

let resultComponent = (_greeting) => <div className="testCell">
    {ReasonReact.string(_greeting)}
  </div>


/* Action declaration */
type action =
  | Click
  | Toggle;

/* Component template declaration.
   Needs to be **after** state and action declarations! */
let component = ReasonReact.reducerComponent("Example");

/* greeting and children are props. `children` isn't used, therefore ignored.
   We ignore it by prepending it with an underscore */
let make = (~greeting, _children) => {
  /* spread the other default fields of component here and override a few */
  ...component,

  initialState: () => {count: 0, show: true},

  /* State transitions */
  reducer: (action, state) =>
    switch (action) {
    | Click => ReasonReact.Update({...state, count: state.count + 1})
    | Toggle => ReasonReact.Update({...state, show: ! state.show})
    },

  render: self => {
    let message =
      "You've clicked this div " ++ string_of_int(self.state.count) ++ " times(s)";
    
      <Fragment>
      <button className="testCell" onClick={_event => self.send(Click)}>
        {ReasonReact.string(message)}
      </button>
      <button className="testCell" onClick={_event => self.send(Toggle)}>
        {ReasonReact.string("Toggle greeting")}
      </button>
      {self.state.show ? resultComponent(greeting) : ReasonReact.null}
      </Fragment>
  },
};
