open ReactUtil;
open Belt.List;
open Belt;
open Webapi;

let slideAnimationDurationMs = 333.0;

type componentFactory = (int, int, string) => reactComponent;

type animation = {
  fromIndex: int,
  toIndex: int,
};

type animationState =
  | Idle
  | Animating(animation); // CSS animation slide and a timer running

type itemSlotPlacement = {
  centeredLeftX: float,
  width: float,
};

type state = {
  selected: int,
  centered: int, // if animation is running, this is where the last centering animation was aiming for (toIndex)
  queuedAnimation: option(int),
  animationState,
  paddingCommand: InfiniteSliderPadding.command,
  itemSlotPlacement: option(itemSlotPlacement),
};

let stringOfAnimation = (a: animation): string => {
  "{"
  ++ string_of_int(a.fromIndex)
  ++ ", "
  ++ string_of_int(a.toIndex)
  ++ "}";
};

let stringOfAnimationState = (state: animationState): string => {
  switch (state) {
  | Idle => "Idle"
  | Animating(animation) =>
    "Animating(" ++ stringOfAnimation(animation) ++ ")"
  };
};

let stringOfState = (state: state): string => {
  "[ selected:"
  ++ string_of_int(state.selected)
  ++ " centered:"
  ++ string_of_int(state.centered)
  ++ " animationState:"
  ++ stringOfAnimationState(state.animationState)
  ++ " paddingCommand:"
  ++ InfiniteSliderPadding.stringOfCommand(state.paddingCommand)
  ++ " queuedAnimation:"
  ++ (
    state.queuedAnimation |> Option.mapWithDefault(_, "None", string_of_int)
  )
  ++ "]";
};

type event =
  | AnimationComplete(InfiniteSliderPadding.animationState)
  | ChangeSelected(int)
  | SlotPlacement(option(itemSlotPlacement));

let stringOfEvent = (event: event): string => {
  switch (event) {
  | AnimationComplete(paddingAnimationState) =>
    "AnimationComplete("
    ++ InfiniteSliderPadding.stringOfAnimationState(paddingAnimationState)
    ++ ")"
  | ChangeSelected(newSelected) =>
    "ChangeSelected(" ++ string_of_int(newSelected) ++ ")"
  | SlotPlacement(_) => "SlotPlacement(...)"
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
  "inf-slider-item-" ++ config.componentBaseName ++ "-" ++ string_of_int(i);
};

let id_for_string = (config: config, s: string): string => {
  "inf-slider-" ++ config.componentBaseName ++ "-" ++ s;
};

//   Js.log("FOOO " ++ string_of_int(prevSpanItems) ++ " " ++ Js.Float.toString(tInsideItem) ++ " "++ string_of_int(nextSpanItems))

let switchAnimation =
    (
      prevPaddingState: InfiniteSliderPadding.animationState,
      prevAnimation: animation,
      queuedAnimationToIndex: int,
    )
    : (float, animation, option(int)) => {
  let prevItemStep = prevAnimation.toIndex - prevAnimation.fromIndex;
  let (currentItemInPrevAnimation, tInsideItem) =
    if (prevItemStep < 0) {
      let prevItemStepAbs = float_of_int(Js.Math.abs_int(prevItemStep));
      // was going left
      (
        int_of_float(prevPaddingState.t *. float_of_int(prevItemStep)) - 1,
        JsUtil.fmod(prevPaddingState.t, 1.0 /. prevItemStepAbs)
        *. prevItemStepAbs,
      );
    } else {
      (
        // was going right
        int_of_float(prevPaddingState.t *. float_of_int(prevItemStep)),
        JsUtil.fmod(prevPaddingState.t, 1.0 /. float_of_int(prevItemStep))
        *. float_of_int(prevItemStep),
      );
    };
  let fromIndexNew = prevAnimation.fromIndex + currentItemInPrevAnimation;
  let nextItemStep: int = queuedAnimationToIndex - fromIndexNew;
  Js.log(
    "switchAnimation prevAnimation: "
    ++ stringOfAnimation(prevAnimation)
    ++ " prevPaddingState.t: "
    ++ Js.Float.toString(prevPaddingState.t),
  );
  Js.log(
    "switchAnimation fromIndexNew: "
    ++ string_of_int(fromIndexNew)
    ++ " currentItemInPrevAnimation:"
    ++ string_of_int(currentItemInPrevAnimation)
    ++ " prevItemStep: "
    ++ string_of_int(prevItemStep)
    ++ " tInsideItem:"
    ++ Js.Float.toString(tInsideItem)
    ++ " nextItemStep:"
    ++ string_of_int(nextItemStep)
    ++ " queuedAnimationToIndex: "
    ++ string_of_int(queuedAnimationToIndex),
  );
  if (prevPaddingState.t >= 1.0) {
    Js.log("PREVIOUS ANIMATION COMPLETE");
    (
      0.0,
      {fromIndex: prevAnimation.toIndex, toIndex: queuedAnimationToIndex},
      None,
    );
  } else if (Js.Math.sign_int(prevItemStep) == Js.Math.sign_int(nextItemStep)) {
    if (prevItemStep < 0) {
      // going left
      let tSwitched = tInsideItem /. float_of_int(- nextItemStep + 1);
      let nextAnimation = {
        fromIndex: fromIndexNew + 1,
        toIndex: queuedAnimationToIndex,
      };
      Js.log(
        "NORMAL LEFT "
        ++ Js.Float.toString(tSwitched)
        ++ " nextAnimation: "
        ++ stringOfAnimation(nextAnimation),
      );
      (tSwitched, nextAnimation, None);
    } else {
      // going right. Very simple, just scale the tInsideItem to the next animation
      let tSwitched = tInsideItem /. float_of_int(nextItemStep);
      let nextAnimation = {
        fromIndex: fromIndexNew,
        toIndex: queuedAnimationToIndex,
      };
      Js.log(
        "NORMAL RIGHT "
        ++ Js.Float.toString(tSwitched)
        ++ " nextAnimation: "
        ++ stringOfAnimation(nextAnimation),
      );
      (tSwitched, nextAnimation, None);
    };
  } else if
    // We are changing direction. Insert a "shim" animation to change direction and go back as far
    // as current item has not completely been animated
    (prevAnimation.toIndex > queuedAnimationToIndex) {
    // was going right, now going left
    let tNextAnimation = 1.0 -. tInsideItem;
    let nextAnimation = {fromIndex: fromIndexNew + 1, toIndex: fromIndexNew};
    Js.log(
      "GOING RIGHT TO GOING LEFT "
      ++ Js.Float.toString(tInsideItem)
      ++ " nextAnimation: "
      ++ stringOfAnimation(nextAnimation)
      ++ " tNextAnimation:"
      ++ Js.Float.toString(tNextAnimation),
    );
    (tNextAnimation, nextAnimation, Some(queuedAnimationToIndex));
  } else {
    // was going left, now going right, back right "back" to next element on right
    let nextAnimation = {fromIndex: fromIndexNew, toIndex: fromIndexNew + 1};
    let tNextAnimation = 1.0 -. tInsideItem;
    Js.log(
      "GOING LEFT TO GOING RIGHT "
      ++ Js.Float.toString(tInsideItem)
      ++ " nextAnimation: "
      ++ stringOfAnimation(nextAnimation)
      ++ " tNextAnimation:"
      ++ Js.Float.toString(tNextAnimation),
    );
    (tNextAnimation, nextAnimation, Some(queuedAnimationToIndex));
  };
};

let animationPaddingState =
    (
      prevState: InfiniteSliderPadding.animationState,
      itemSlotPlacement: option(itemSlotPlacement),
      animation,
    )
    : InfiniteSliderPadding.animationState => {
  let {fromIndex, toIndex} = animation;
  let (fromItems, toItems) =
    if (fromIndex < toIndex) {
      (
        // going right
        float_of_int(toIndex - fromIndex),
        0.0 // replace some elements & shrink to create illusion of scrolling right
      );
    } else {
      (
        0.0,
        float_of_int(fromIndex - toIndex) // grow
      );
    };
  Option.mapWithDefault(itemSlotPlacement, prevState, isp =>
    {
      ...prevState,
      timer: InfiniteSliderPadding.restart(prevState.timer),
      durationMs: slideAnimationDurationMs,
      startWidth: fromItems *. isp.width,
      endWidth: toItems *. isp.width,
    }
  );
};

let replacedItems = (animation: animation): int =>
  if (animation.fromIndex < animation.toIndex) {
    // going right
    animation.toIndex - animation.fromIndex;
  } else {
    0;
  };

let elems =
    (
      state: state,
      config: config,
      dispatchCompleted: InfiniteSliderPadding.dispatchCompleted,
    )
    : list(reactComponent) => {
  switch (state.animationState) {
  | Idle =>
    let index = i => i + state.centered;
    config.itemsWindow
    |> RangeOfInt.map(_, i =>
         config.componentFactory(
           index(i),
           state.selected,
           id(config, index(i)),
         )
       );
  | Animating(animation) =>
    let paddingItem: reactComponent =
      <InfiniteSliderPadding
        key="padding"
        command={state.paddingCommand}
        dispatchCompleted
        id={id_for_string(config, "padding")}
      />;

    let index = i => animation.fromIndex + i;
    let normalItems =
      config.itemsWindow
      |> RangeOfInt.drop(_, replacedItems(animation))
      |> RangeOfInt.map(_, i =>
           config.componentFactory(
             index(i),
             state.selected,
             id(config, index(i)),
           )
         );

    [paddingItem, ...normalItems];
  };
};

let getItemSlotPlacement =
    (state: state, config: config): option(itemSlotPlacement) => {
  let doc = Webapi.Dom.document;
  let id0 = id(config, state.centered);
  let id1 = id(config, state.centered + 1);
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

let handleClick =
    (state: state, config: config, click: ReactEvent.Mouse.t): unit => {
  Option.map(
    state.itemSlotPlacement,
    placement => {
      let clickX = float_of_int(ReactEvent.Mouse.clientX(click));

      let slot =
        clickX > placement.centeredLeftX
          ? (clickX -. placement.centeredLeftX) /. placement.width
          : (clickX -. placement.centeredLeftX) /. placement.width -. 1.0;

      let newSelected = state.selected + int_of_float(slot);
      config.itemSelectedDispatch(newSelected);

      Js.logMany(
        toArray([
          "handleClick item width:",
          Js.Float.toString(placement.width),
          "clickX",
          Js.Float.toString(clickX),
          "slot",
          Js.Float.toString(slot),
          "newSelected",
          string_of_int(newSelected),
          "state.selected",
          string_of_int(state.selected),
        ]),
      );
    },
  )
  |> ignore;
};

let stateMachine = (state: state, action: event): state => {
  switch (action, state.animationState) {
  | (AnimationComplete(prevPaddingAnimationState), Animating(prevAnimation)) =>
    switch (state.queuedAnimation) {
    | Some(queuedAnimationToIndex) =>
      let (tSwitched, switchedAnimation, queuedStill) =
        switchAnimation(
          prevPaddingAnimationState,
          prevAnimation,
          queuedAnimationToIndex,
        );
      {
        ...state,
        queuedAnimation: queuedStill,
        centered: switchedAnimation.fromIndex,
        animationState: Animating(switchedAnimation),
        paddingCommand:
          Start({
            ...
              animationPaddingState(
                prevPaddingAnimationState,
                state.itemSlotPlacement,
                switchedAnimation,
              ),
            tInitial: tSwitched,
            t: tSwitched,
          }),
      };
    | _ => {
        ...state,
        centered: prevAnimation.toIndex,
        paddingCommand: Nop,
        queuedAnimation: None,
        animationState: Idle,
      }
    }
  | (ChangeSelected(newSelected), Idle) =>
    let animation = {fromIndex: state.centered, toIndex: newSelected};
    {
      ...state,
      selected: newSelected,
      paddingCommand:
        Start(
          animationPaddingState(
            InfiniteSliderPadding.initialState,
            state.itemSlotPlacement,
            animation,
          ),
        ),
      animationState: Animating(animation),
    };
  | (ChangeSelected(newSelected), Animating(_)) =>
    let queuedAnimationToIndex = Some(newSelected);
    {
      ...state,
      selected: newSelected,
      paddingCommand: Stop,
      queuedAnimation: queuedAnimationToIndex,
    };
  | (SlotPlacement(info), _) => {...state, itemSlotPlacement: info}
  | (AnimationComplete(_), Idle) =>
    Js.log("Should be impossible transition?");
    state;
  };
};

let initialState = {
  selected: 0,
  centered: 0,
  queuedAnimation: None,
  animationState: Idle,
  itemSlotPlacement: None,
  paddingCommand: Nop,
};

let logTransition = ((state: state, dispatch: event => unit)) => {
  let wrapped: event => unit =
    (event: event) => (
      {
        Js.log(
          "transition on event "
          ++ stringOfEvent(event)
          ++ " to state "
          ++ stringOfState(stateMachine(state, event)),
        );
        dispatch(event);
      }: unit
    );
  (state, wrapped);
};

[@react.component]
let make = (~config: config, ~selected: int) => {
  let rowClassName = config.styleBaseName ++ "Row";

  let (state, dispatch) =
    logTransition(React.useReducer(stateMachine, initialState));

  React.useEffect2(
    () => {
      if (state.selected != selected) {
        dispatch(ChangeSelected(selected));
      };
      None;
    },
    ((), selected),
  );

  React.useLayoutEffect2(
    () => {
      if (state.animationState == Idle) {
        dispatch(SlotPlacement(getItemSlotPlacement(state, config)));
      };
      None;
    },
    ((), state.animationState),
  );

  //ReactSwipeable.swipeTest();
  //ReactSwipeable.useSwipeableInternal("foo");

  let e: list(reactComponent) =
    elems(state, config, paddingAnimationState =>
      dispatch(AnimationComplete(paddingAnimationState))
    );

  // let jsonStringify: ('a) => string = [%bs.raw {|function(x){return JSON.stringify(x)}|}];

  <div
    className="infiniteSlider"
    onClick={event => handleClick(state, config, event)}>
    <div className=rowClassName> {asReact(e)} </div>
  </div>;
};