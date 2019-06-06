open Belt.List;
open ReactUtil;

type mainUiMode =
  | Game
  | RelativeNotes;

type state = {mainUiMode};

type action =
  | ChangeMode(mainUiMode);

type menuItem = {
  title: string,
  mode: mainUiMode,
};

let menuItems: list(menuItem) = [
  {title: "Play With Intervals (wip)", mode: RelativeNotes},
  {title: "Notes Quiz (placeholder)", mode: Game},
];

let menuButton =
    (menuItem: menuItem, currentMode: mainUiMode, send: action => unit) => {
  let className =
    currentMode == menuItem.mode ? "mainMenuItem current" : "mainMenuItem";
  <button className key={menuItem.title} onClick={_event => send(ChangeMode(menuItem.mode))}>
    {ReasonReact.string(menuItem.title)}
  </button>;
};

[@react.component]
let make = () => {
  let (state, dispatch) =
    React.useReducer(
      (state: state, action: action) =>
      switch (action) {
      | ChangeMode(mode) => {mainUiMode: mode}
      }, {mainUiMode: RelativeNotes});

  <>
      <div className="mainMenuRow">
        {asReact(
           menuItems
           |> map(_, menuButton(_, state.mainUiMode, dispatch)),
         )}
      </div>
      {switch (state.mainUiMode) {
       | Game => <GameComponent />
       | RelativeNotes => <RelativeNotesComponent />
       }}
   </>;
};