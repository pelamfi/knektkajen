open ReactUtil;
open Belt.List;
open Belt.Option;
open Webapi;
open Webapi.Dom.Element;

type componentFactory = (int, int, string) => reactComponent;

type animating = {
  fromIndex: int,
  toIndex: int,
};

type slideState =
  | Idle(int)
  | Animating(animating); // CSS animation slide and a timer running

type itemSlotPlacement = {currentLeftX: int, width: int}

type state = {
  current: int,
  animationToIndexQueued: option(animating),
  slideState,
  itemSlotPlacement: option(itemSlotPlacement)
};

let string_of_animating = (a: animating): string => {
  "{"
    ++ string_of_int(a.fromIndex)
    ++ ", "
    ++ string_of_int(a.toIndex)
    ++ "}"
}

let string_of_slide_state = (state: slideState): string => {
  switch (state) {
  | Idle(currentAt) => "Idle(" ++ string_of_int(currentAt) ++ ")"
  | Animating(animating) =>
    "Animating(" ++ string_of_animating(animating) ++ ")"
  };
};

let string_of_state = (state: state): string => {
  "[current:"
  ++ string_of_int(state.current)
  ++ " slideState: "
  ++ string_of_slide_state(state.slideState)
  ++ " animationToIndexQueued: "
  ++ (
    state.animationToIndexQueued |> mapWithDefault(_, "none", string_of_animating)
  )
  ++ "]";
};

type event =
  | AnimationComplete
  | ChangeCurrent(int)
  | CheckNextAnimation
  | SlotPlacement(option(itemSlotPlacement));

let string_of_event = (event: event): string => {
  switch (event) {
  | AnimationComplete => "AnimationComplete"
  | CheckNextAnimation => "CheckNextAnimation"
  | ChangeCurrent(current) =>
    "ChangeCurrent(" ++ string_of_int(current) ++ ")"
  | SlotPlacement(_) =>
    "SlotPlacement(...)"
  };
};

type config = {
  componentFactory,
  styleBaseName: string,
  componentBaseName: string,
  itemSelectedDispatch: int => unit,
  itemsWindow: RangeOfInt.range_of_int, // current is at 0
  maxJump: int,
};

let id = (config: config, i: int): string => {
  "inf-slider-item-" ++ config.componentBaseName ++ "-" ++ string_of_int(i)
}

let paddingCount = (slideState: slideState, maxJump: int): int => {
  switch (slideState) {
  | Idle(_) => 1 + maxJump
  | Animating({fromIndex, toIndex}) =>
    min(max(1, 1 + maxJump - (toIndex - fromIndex)), 1 + 2 * maxJump)
  };
};

let elems = (state: state, config: config): list(reactComponent) => {
  let offset =
    switch (state.slideState) {
    | Idle(currentAt) => currentAt
    | Animating({fromIndex}) => fromIndex
    };
  config.itemsWindow
  |> RangeOfInt.map(_, i => config.componentFactory(i + offset, state.current, id(config, i+offset)));
};


let getItemSlotPlacement = (state: state, config: config): option(itemSlotPlacement) => {
    let doc = Webapi.Dom.document;
    let id0 = id(config, state.current);
    let id1 = id(config, state.current + 1);
    let left = (id: string): option(int) => {
      let e = Webapi.Dom.Document.getElementById(id, doc);
      let boundingClientRect = map(e, Webapi.Dom.Element.getBoundingClientRect);
      map(boundingClientRect, Dom.DomRect.left) |> map(_, int_of_float);
    };
    flatMap(left(id0), left0 => map(left(id1), left1 => {
        {currentLeftX: left0, width: left1 - left0}
    }))
}

let handleClick = (state: state, config: config, click: ReactEvent.Mouse.t): unit => {
    map(state.itemSlotPlacement, placement => {
      let clickX = ReactEvent.Mouse.clientX(click);
      let slot = (clickX - placement.currentLeftX) / placement.width;
      config.itemSelectedDispatch(state.current + slot);

      Js.logMany(toArray(["dist", string_of_int(placement.width), 
      "clickX", string_of_int(clickX),
      "slot", string_of_int(slot),
      "cur", string_of_int(state.current)]));
      
    }) |> ignore
  };

[@react.component]
let make = (~config: config, ~current: int) => {
  let rowClassName = config.styleBaseName ++ "Row";

  let (state, dispatch) =
    React.useReducer(
      (state: state, action: event) => {
        let newState =
          switch (action) {
          | AnimationComplete =>
            switch(state.slideState) {
              | Animating({toIndex}) =>
              {...state, slideState: Idle(toIndex)}
              | Idle(_) =>
              {...state, slideState: Idle(state.current)}
            }
          | CheckNextAnimation =>
            switch (state.animationToIndexQueued) {
            | Some(queued) when queued.fromIndex != queued.toIndex => {
                ...state,
                animationToIndexQueued: None,
                slideState: Animating(queued),
              }
            | _ => {...state, animationToIndexQueued: None, slideState: Idle(state.current)}
            }
          | ChangeCurrent(newCurrent) =>
            switch (state.slideState) {
            | Idle(currentAt) => {
                ...state,
                current: newCurrent,
                slideState:
                  Animating({fromIndex: currentAt, toIndex: newCurrent}),
              }
            | Animating({toIndex}) => {
                ...state,
                current: newCurrent,
                animationToIndexQueued: Some({fromIndex: toIndex, toIndex: newCurrent}),
              }
            }
          | SlotPlacement(info) => {
            ...state,
            itemSlotPlacement: info
          }
          };

        Js.log( 
          "state "
          ++ string_of_state(state)
          ++ "->"
          ++ string_of_state(newState)
          ++ " on "
          ++ string_of_event(action),
        );

        newState;
      },
      {current: 0, animationToIndexQueued: None, slideState: Idle(0), itemSlotPlacement: None},
    );

  Js.log(string_of_slide_state(state.slideState));
  React.useEffect2(
    () => {
      Js.log("FOOOOO");
      switch (state.slideState) {
      | Animating(_) =>
        let timeoutId =
          Js.Global.setTimeout(() => dispatch(AnimationComplete), 333);
        Some(() => Js.Global.clearTimeout(timeoutId));
      | Idle(_) => 
        dispatch(SlotPlacement(getItemSlotPlacement(state, config)))
        dispatch(CheckNextAnimation)
        None
      }},
  (0, string_of_slide_state(state.slideState)),
  );

  if (current !== state.current) {
    dispatch(ChangeCurrent(current));
  };

  //ReactSwipeable.swipeTest();
  //ReactSwipeable.useSwipeableInternal("foo");

  let paddingAnimClass =
    switch (state.slideState) {
    | Animating(_) => " " ++ config.styleBaseName ++ "Padding-anim"
    | Idle(_) => ""
    };

  let paddingClass =
    config.styleBaseName
    ++ "Padding-"
    ++ string_of_int(paddingCount(state.slideState, config.maxJump))
    ++ paddingAnimClass;

  let e: list(reactComponent) = elems(state, config);

  // Js.log("paddingclass " ++ paddingClass);

  // let jsonStringify: ('a) => string = [%bs.raw {|function(x){return JSON.stringify(x)}|}];

  
  <div className="infiniteSlider" onClick={event => handleClick(state, config, event)}>
    <div id="notesRow" className=rowClassName>
      <div className=paddingClass />
      {asReact(e)}
    </div>
  </div>;
};