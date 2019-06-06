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
  //initialState: () => {mainUiMode: RelativeNotes},
  
  /*
  reducer: (action, _) =>
    switch (action) {
    | ChangeMode(mode) => ReasonReact.Update({mainUiMode: mode})
    },
*/
  <>
      /*<div className="mainMenuRow">
        {asReact(
           menuItems
           |> map(_, menuButton(_, self.state.mainUiMode, self.send)),
         )}
      </div>*/
      <GameComponent />
      <RelativeNotesComponent />
      /*
      {switch (self.state.mainUiMode) {
       | Game => <GameComponent />
       | RelativeNotes => <RelativeNotesComponent />
       }} */
   </>;
};