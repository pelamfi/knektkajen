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
  | Idle
  | Animating(animating); // CSS animation slide and a timer running

type state = {
  current: int,
  animationToIndexQueued: option(int),
  slideState,
};

let string_of_slide_state = (state: slideState): string => {
  switch (state) {
  | Idle => "Idle"
  | Animating({fromIndex, toIndex}) =>
    "Animating("
    ++ string_of_int(fromIndex)
    ++ ", "
    ++ string_of_int(toIndex)
    ++ ")"
  };
};

let string_of_state = (state: state): string => {
  "[current:"
  ++ string_of_int(state.current)
  ++ " slideState: "
  ++ string_of_slide_state(state.slideState)
  ++ " animationToIndexQueued: "
  ++ (
    state.animationToIndexQueued |> mapWithDefault(_, "none", string_of_int)
  )
  ++ "]";
};

type event =
  | AnimationComplete
  | ChangeCurrent(int);

let string_of_event = (event: event): string => {
  switch (event) {
  | AnimationComplete => "AnimationComplete"
  | ChangeCurrent(current) =>
    "ChangeCurrent(" ++ string_of_int(current) ++ ")"
  };
};

type config = {
  componentFactory,
  styleBaseName: string,
  componentBaseName: string,
  itemsWindow: RangeOfInt.range_of_int, // current is at 0
  maxJump: int,
};

let id = (config: config, i: int): string => {
  "inf-slider-item-" ++ config.componentBaseName ++ "-" ++ string_of_int(i)
}

let paddingCount = (slideState: slideState, maxJump: int): int => {
  switch (slideState) {
  | Idle => 1 + maxJump
  | Animating({fromIndex, toIndex}) =>
    min(max(1, 1 + maxJump - (toIndex - fromIndex)), 1 + 2 * maxJump)
  };
};

let elems = (state: state, config: config): list(reactComponent) => {
  let offset =
    switch (state.slideState) {
    | Idle => state.current
    | Animating({fromIndex}) => fromIndex
    };
  config.itemsWindow
  |> RangeOfInt.map(_, i => config.componentFactory(i + offset, state.current, id(config, i+offset)));
};

let handleClick = (state: state, config: config, click: ReactEvent.Mouse.t): unit => {
    let doc = Webapi.Dom.document;
    let id0 = id(config, state.current);
    let id1 = id(config, state.current + 1);
    let xRange = (id: string): option(RangeOfInt.range_of_int) => {
      let e = Webapi.Dom.Document.getElementById(id, doc);
      let boundingClientRect = map(e, Webapi.Dom.Element.getBoundingClientRect);
      map(boundingClientRect, r => {
        let left = int_of_float(Dom.DomRect.left(r));
        let width = int_of_float(Dom.DomRect.width(r));
        RangeOfInt.make(left, left + width)
      })
    };
    let item0XRange = xRange(id0)
    let item1XRange = xRange(id1)
    //Js.log(mapWithDefault(width, "no width", Js.Float.toString));
    Js.log(string_of_int(ReactEvent.Mouse.clientX(click)));
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
            switch (state.slideState) {
            | Idle => state
            | Animating({fromIndex, toIndex}) =>
              switch (state.animationToIndexQueued) {
              | Some(queued) => {
                  ...state,
                  slideState:
                    Animating({fromIndex: toIndex, toIndex: queued}),
                }
              | None => {...state, slideState: Idle}
              }
            }
          | ChangeCurrent(newCurrent) =>
            switch (state.slideState) {
            | Idle => {
                ...state,
                current: newCurrent,
                slideState:
                  Animating({fromIndex: state.current, toIndex: newCurrent}),
              }
            | Animating(_) => {
                ...state,
                current: newCurrent,
                animationToIndexQueued: Some(newCurrent),
              }
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
      {current: 0, animationToIndexQueued: None, slideState: Idle},
    );

  React.useEffect1(
    () => {
      Js.log("foo");
      switch (state.slideState) {
      | Animating(_) =>
        Js.log("bar");
        let timeoutId =
          Js.Global.setTimeout(() => dispatch(AnimationComplete), 333);
        Some(() => Js.Global.clearTimeout(timeoutId));
      | Idle => None
      }},
    toArray([state.slideState]),
  );

  if (current !== state.current) {
    dispatch(ChangeCurrent(current));
  };

  ReactSwipeable.swipeTest();
  ReactSwipeable.useSwipeableInternal("foo");

  let paddingAnimClass =
    switch (state.slideState) {
    | Animating(_) => " " ++ config.styleBaseName ++ "Padding-anim"
    | Idle => ""
    };

  let paddingClass =
    config.styleBaseName
    ++ "Padding-"
    ++ string_of_int(paddingCount(state.slideState, config.maxJump))
    ++ paddingAnimClass;

  let e: list(reactComponent) = elems(state, config);

  Js.log("paddingclass " ++ paddingClass);

  // let jsonStringify: ('a) => string = [%bs.raw {|function(x){return JSON.stringify(x)}|}];

  
  <div className="infiniteSlider" onClick={event => handleClick(state, config, event)}>
    <div id="notesRow" className=rowClassName>
      <div className=paddingClass />
      {asReact(e)}
    </div>
  </div>;
};