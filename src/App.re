type mainUiMode = Game | RelativeNotes;

type state = {
  mainUiMode: mainUiMode,
};

type action =
  | ChangeMode(mainUiMode)


let component = ReasonReact.reducerComponent("App");

let make = (_children) => {
  ...component,

  initialState: () => {mainUiMode: RelativeNotes},

  reducer: (action, _) =>
    switch (action) {
    | ChangeMode(mode)  => ReasonReact.Update({mainUiMode: mode})
    },

  render: self =>
    <Fragment>
      <div className="mainMenuRow">
        <button className="mainMenuItem" onClick={_event => self.send(ChangeMode(RelativeNotes))}>
            {ReasonReact.string("Play With Interval")}
        </button>
        <button className="mainMenuItem" onClick={_event => self.send(ChangeMode(Game))}>
            {ReasonReact.string("Notes Quiz (placeholder)")}
        </button>
      </div>
      {switch (self.state.mainUiMode) {
         | Game => <GameComponent/>
         | RelativeNotes  => <RelativeNotesComponent/>
      }
      }
    </Fragment>,
};


