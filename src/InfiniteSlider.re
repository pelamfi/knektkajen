open ReactUtil;
open Belt;

let slideAnimationDurationMs = 333.0;

type componentFactory = (int, int, string) => reactComponent;

type animation = {
  fromIndex: int,
  toIndex: int,
};

type animationState =
  | Idle
  | Animating(animation); // CSS animation slide and a timer running

type state = {
  selected: int,
  centered: int, // if animation is running, this is where the last centering animation was aiming for (toIndex)
  queuedAnimation: option(int),
  animationState,
  paddingCommand: InfiniteSliderPadding.command,
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
  | ChangeSelected(int);

let stringOfEvent = (event: event): string => {
  switch (event) {
  | AnimationComplete(paddingAnimationState) =>
    "AnimationComplete("
    ++ InfiniteSliderPadding.stringOfAnimationState(paddingAnimationState)
    ++ ")"
  | ChangeSelected(newSelected) =>
    "ChangeSelected(" ++ string_of_int(newSelected) ++ ")"
  };
};

type config = {
  componentFactory,
  styleBaseName: string,
  componentBaseName: string,
  itemSelectedDispatch: int => unit,
  itemsWindow: RangeOfInt.rangeOfInt, // current is at 0
  maxJump: int,
};

let id = (config: config, i: int): string => {
  "inf-slider-item-" ++ config.componentBaseName ++ "-" ++ string_of_int(i);
};

let id_for_string = (config: config, s: string): string => {
  "inf-slider-" ++ config.componentBaseName ++ "-" ++ s;
};

type limitedAnimationSteps = {
  itemStep: int,
  overflowToIndex: option(int),
  toIndex: int,
};

let limitAnimationSteps =
    (toIndex: int, fromIndex: int, limit: int): limitedAnimationSteps => {
  let itemStep: int = toIndex - fromIndex;
  // Js.log("itemStep " ++ string_of_int(itemStep) ++ " " ++ string_of_int(limit))
  if (Js.Math.abs_int(itemStep) > limit) {
    let itemStepLimited = limit * Js.Math.sign_int(itemStep);
    {
      itemStep: itemStepLimited,
      overflowToIndex: Some(toIndex),
      toIndex: fromIndex + itemStepLimited,
    };
  } else {
    {itemStep, overflowToIndex: None, toIndex};
  };
};

let switchAnimation =
    (
      config: config,
      prevPaddingState: InfiniteSliderPadding.animationState,
      prevAnimation: animation,
      queuedAnimationToIndexOrig: int,
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
    } else if (prevItemStep > 0) {
      (
        // was going right
        int_of_float(prevPaddingState.t *. float_of_int(prevItemStep)),
        JsUtil.fmod(prevPaddingState.t, 1.0 /. float_of_int(prevItemStep))
        *. float_of_int(prevItemStep),
      );
    } else {
      (
        // was going nowhere, give prev t 1.0 so prev animation is "completed"
        prevItemStep,
        1.0,
      );
    };

  let fromIndexNew = prevAnimation.fromIndex + currentItemInPrevAnimation;

  let limited =
    limitAnimationSteps(
      queuedAnimationToIndexOrig,
      fromIndexNew,
      config.maxJump,
    );
  // Js.log( "switchAnimation prevAnimation: " ++ stringOfAnimation(prevAnimation) ++ " prevPaddingState.t: "++ Js.Float.toString(prevPaddingState.t));
  // Js.log( "switchAnimation fromIndexNew: " ++ string_of_int(fromIndexNew) ++ " currentItemInPrevAnimation:" ++ string_of_int(currentItemInPrevAnimation) ++ " prevItemStep: " ++ string_of_int(prevItemStep) ++ " tInsideItem:" ++ Js.Float.toString(tInsideItem) ++ " limited.itemStep:" ++ string_of_int(limited.itemStep) ++ " limited.overflow…: "++ (limited.overflowToIndex |> Option.mapWithDefault(_, "None", string_of_int)));
  if (prevPaddingState.t >= 1.0) {
    (
      // Js.log("PREVIOUS ANIMATION COMPLETE");
      0.0,
      {fromIndex: prevAnimation.toIndex, toIndex: limited.toIndex},
      limited.overflowToIndex,
    );
  } else if (Js.Math.sign_int(prevItemStep)
             == Js.Math.sign_int(limited.itemStep)) {
    if (prevItemStep < 0) {
      // going left
      let tSwitched = tInsideItem /. float_of_int(- limited.itemStep + 1);
      let nextAnimation = {
        fromIndex: fromIndexNew + 1,
        toIndex: limited.toIndex,
      };
      // Js.log( "NORMAL LEFT " ++ Js.Float.toString(tSwitched) ++ " nextAnimation: " ++ stringOfAnimation(nextAnimation));
      (tSwitched, nextAnimation, limited.overflowToIndex);
    } else {
      // going right. Very simple, just scale the tInsideItem to the next animation
      let tSwitched = tInsideItem /. float_of_int(limited.itemStep);
      let nextAnimation = {fromIndex: fromIndexNew, toIndex: limited.toIndex};
      // Js.log( "NORMAL RIGHT " ++ Js.Float.toString(tSwitched) ++ " nextAnimation: " ++ stringOfAnimation(nextAnimation));
      (tSwitched, nextAnimation, limited.overflowToIndex);
    };
  } else if
    // We are changing direction. Insert a "shim" animation to change direction and go back as far
    // as current item has not completely been animated
    (prevAnimation.toIndex > queuedAnimationToIndexOrig) {
    // was going right, now going left
    let tNextAnimation = 1.0 -. tInsideItem;
    let nextAnimation = {fromIndex: fromIndexNew + 1, toIndex: fromIndexNew};
    // Js.log( "GOING RIGHT TO GOING LEFT " ++ Js.Float.toString(tInsideItem) ++ " nextAnimation: " ++ stringOfAnimation(nextAnimation) ++ " tNextAnimation:" ++ Js.Float.toString(tNextAnimation));
    (tNextAnimation, nextAnimation, Some(queuedAnimationToIndexOrig));
  } else {
    // was going left, now going right, back right "back" to next element on right
    let nextAnimation = {fromIndex: fromIndexNew, toIndex: fromIndexNew + 1};
    let tNextAnimation = 1.0 -. tInsideItem;
    // Js.log("GOING LEFT TO GOING RIGHT " ++ Js.Float.toString(tInsideItem) ++ " nextAnimation: " ++ stringOfAnimation(nextAnimation) ++ " tNextAnimation:" ++ Js.Float.toString(tNextAnimation));
    (tNextAnimation, nextAnimation, Some(queuedAnimationToIndexOrig));
  };
};

let animationPaddingState =
    (
      prevState: InfiniteSliderPadding.animationState,
      itemPitchX: float,
      animation: animation,
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
  {
    ...prevState,
    timer: InfiniteSliderPadding.restart(prevState.timer),
    durationMs: slideAnimationDurationMs,
    startWidth: fromItems *. itemPitchX,
    endWidth: toItems *. itemPitchX,
  };
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
    |> RangeOfInt.map(i =>
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
      |> RangeOfInt.drop(replacedItems(animation))
      |> RangeOfInt.dropRight(
           max(0, config.maxJump - replacedItems(animation)),
         )
      |> RangeOfInt.map(i =>
           config.componentFactory(
             index(i),
             state.selected,
             id(config, index(i)),
           )
         );

    [paddingItem, ...normalItems];
  };
};

// https://www.w3schools.com/jsref/prop_element_offsetleft.asp
let offsetLeftOfElement = (id: string): option(float) => {
  Webapi.Dom.(
    Document.getElementById(id, document)
    |> Option.flatMap(_, Element.asHtmlElement)
    |> Option.map(_, HtmlElement.offsetLeft)
    |> Option.map(_, float_of_int)
  );
};

let switchedAnimationState =
    (
      config: config,
      state: state,
      prevPaddingAnimationState: InfiniteSliderPadding.animationState,
      prevAnimation: animation,
      queuedAnimationToIndex: int,
    )
    : state => {
  let (tSwitched, switchedAnimation, queuedStill) =
    switchAnimation(
      config,
      prevPaddingAnimationState,
      prevAnimation,
      queuedAnimationToIndex,
    );

  let id0 = id(config, state.centered);
  let id1 = id(config, state.centered + 1);

  let itemPitchX =
    Option.flatMap(offsetLeftOfElement(id0), left0 =>
      Option.map(offsetLeftOfElement(id1), left1 => left1 -. left0)
    )
    |> Option.mapWithDefault(_, 20.0, x => x);

  let paddingState =
    animationPaddingState(
      prevPaddingAnimationState,
      itemPitchX,
      switchedAnimation,
    );
  {
    ...state,
    queuedAnimation: queuedStill,
    centered: switchedAnimation.fromIndex,
    animationState: Animating(switchedAnimation),
    paddingCommand:
      Start({...paddingState, tInitial: tSwitched, t: tSwitched}),
  };
};

let stateMachine = (config: config, state: state, action: event): state => {
  switch (action, state.animationState) {
  | (AnimationComplete(prevPaddingAnimationState), Animating(prevAnimation)) =>
    switch (state.queuedAnimation) {
    | Some(queuedAnimationToIndex) =>
      switchedAnimationState(
        config,
        state,
        prevPaddingAnimationState,
        prevAnimation,
        queuedAnimationToIndex,
      )
    | _ => {
        ...state,
        centered: prevAnimation.toIndex,
        paddingCommand: Nop,
        queuedAnimation: None,
        animationState: Idle,
      }
    }
  | (ChangeSelected(newSelected), Idle) => {
      ...
        switchedAnimationState(
          config,
          state,
          InfiniteSliderPadding.completedDummyState,
          {fromIndex: state.centered, toIndex: state.centered},
          newSelected,
        ),
      selected: newSelected,
    }
  | (ChangeSelected(newSelected), Animating(_)) =>
    let queuedAnimationToIndex = Some(newSelected);
    {
      ...state,
      selected: newSelected,
      paddingCommand: Stop,
      queuedAnimation: queuedAnimationToIndex,
    };
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
  paddingCommand: Nop,
};

let logTransition = (config: config, (state: state, dispatch: event => unit)) => {
  let wrapped: event => unit =
    (event: event) => (
      {
        Js.log(
          "transition on event "
          ++ stringOfEvent(event)
          ++ " to state "
          ++ stringOfState(stateMachine(config, state, event)),
        );
        dispatch(event);
      }: unit
    );
  (state, wrapped);
};

[@react.component]
let make =
    (
      ~config: config,
      ~selected: int,
      ~className: string,
      ~style: ReactDOMRe.Style.t=ReactUtil.emptyStyle,
    ) => {
  let rowClassName = config.styleBaseName ++ "Row";

  let (state, dispatch) =
    React.useReducer(stateMachine(config), initialState);
  // logTransition(config, React.useReducer(stateMachine(config), initialState));

  React.useEffect2(
    () => {
      if (state.selected != selected) {
        dispatch(ChangeSelected(selected));
      };
      None;
    },
    ((), selected),
  );

  let e: list(reactComponent) =
    elems(state, config, paddingAnimationState =>
      dispatch(AnimationComplete(paddingAnimationState))
    );

  <div style className>
    <div className=rowClassName> {asReact(e)} </div>
  </div>;
};