open ReactUtil;
open Belt;
open Webapi;

type config = {
  id0: string,
  id1: string
};

type animationState =
  | Idle

type state = {
  animationState,
};

type itemSlotPlacement = {
  centeredLeftX: float,
  width: float,
};

let initialState: state = {animationState: Idle};

type event =
  | SlotPlacement(option(itemSlotPlacement));

let stateMachine = (config: config, state: state, action: event): state => {
  state
};

let getItemSlotPlacement =
    (config: config, state: state): option(itemSlotPlacement) => {
  let doc = Webapi.Dom.document;
  let id0 = config.id0;
  let id1 = config.id1;
  let left = (id: string): option(float) => {
    let e = Webapi.Dom.Document.getElementById(id, doc);
    // https://stackoverflow.com/a/18053642/1148030
    let boundingClientRect =
      Option.map(e, Webapi.Dom.Element.getBoundingClientRect);
    Option.map(boundingClientRect, Dom.DomRect.left);
  };
  Option.flatMap(left(id0), left0 =>
    Option.map(left(id1), left1 =>
      {centeredLeftX: left0, width: left1 -. left0}
    )
  );
};

let config: config = {id0: "foo", id1: "bar"};

[@react.component]
let make = () => {
  
  let (state, dispatch) =
    React.useReducer(stateMachine(config), initialState);

  React.useLayoutEffect2(
    () => {
      if (state.animationState == Idle) {
        dispatch(SlotPlacement(getItemSlotPlacement(config, state)));
      };
      None;
    },
    ((), state.animationState),
  );


  <div></div>;
};