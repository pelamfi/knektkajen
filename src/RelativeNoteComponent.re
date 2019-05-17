
open Note

type state = {
  count: int,
  showTicker: bool,
};

let resultComponent = (contents) => <div className="testCell">
    contents
  </div>

/* Action declaration */
type action =
  | Click
  | Toggle;

let component = ReasonReact.reducerComponent("Example");

let make = (~note: note, _children) => {
  ...component,

  initialState: () => {count: 0, showTicker: false},

  reducer: (action, state) =>
    switch (action) {
    | Click => ReasonReact.Update({...state, count: state.count + 1})
    | Toggle => ReasonReact.Update({...state, showTicker: ! state.showTicker})
    },

  render: self => { 
    
    <div className="noteCell">
    {ReasonReact.string(asString(name(note, cMajorName)))}
    </div>
  },
};
