open ReactUtil;
open Belt.List;
open Belt
open Webapi;
//open Webapi.Dom.Element;

type componentFactory = (int, int, string) => reactComponent;

type animation = {
  fromIndex: int,
  toIndex: int
};

type animationState =
  | Idle
  | Animating(animation); // CSS animation slide and a timer running

type itemSlotPlacement = {centeredLeftX: float, width: float}

type state = {
  selected: int,
  centered: int, // if animation is running, this is where the last centering animation was aiming for (toIndex)
  queuedAnimation: option(int),
  animationState,
  paddingCommand: InfiniteSliderPadding.command,
  itemSlotPlacement: option(itemSlotPlacement)
};

let stringOfAnimation = (a: animation): string => {
  "{"
    ++ string_of_int(a.fromIndex)
    ++ ", "
    ++ string_of_int(a.toIndex)
    ++ "}"
}

let slideAnimationDurationMs = 333.0

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
  | AnimationComplete(paddingAnimationState) => "AnimationComplete(" ++ InfiniteSliderPadding.stringOfAnimationState(paddingAnimationState) ++ ")"
  | ChangeSelected(newSelected) =>
    "ChangeSelected(" ++ string_of_int(newSelected) ++ ")"
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

let id_for_string = (config: config, s: string): string => {
  "inf-slider-" ++ config.componentBaseName ++ "-" ++ s
};

let switchAnimation = (prevPaddingState: InfiniteSliderPadding.animationState, prevAnimation: animation, queuedAnimationToIndex: int): (float, animation) => {
  let prevSpanItems = Js.Math.abs_int(prevAnimation.fromIndex - prevAnimation.toIndex);
  let tPerPrevItems = prevPaddingState.t /. float_of_int(prevSpanItems);
  let tInsideItem: float = JsUtil.fmod(prevPaddingState.t, float_of_int(prevSpanItems));
  let currentItemInPrevAnimation = int_of_float(tPerPrevItems);
  let fromIndexNew = prevAnimation.fromIndex + currentItemInPrevAnimation;
  let nextSpanItems: int = Js.Math.abs_int(fromIndexNew - queuedAnimationToIndex);
  let tSwitched = tInsideItem /. float_of_int(nextSpanItems);
  (tSwitched, {fromIndex: fromIndexNew, toIndex: queuedAnimationToIndex})
}

let animationPaddingCommand = (t: float, itemSlotPlacement: option(itemSlotPlacement), animation): InfiniteSliderPadding.command => {
  let {fromIndex, toIndex} = animation;
  let (fromItems, toItems) = if (fromIndex < toIndex) { // going right
    (float_of_int(toIndex - fromIndex), 0.0) // replace some elements & shrink to create illusion of scrolling right
  } else {
    (0.0, float_of_int(fromIndex - toIndex)) // grow
  };
  let (startWidth, endWidth) = Option.mapWithDefault(itemSlotPlacement, (0.0, 0.0), isp => {(fromItems *. isp.width, toItems *. isp.width)});
  Start({t: t, 
      durationMs: slideAnimationDurationMs, startWidth, endWidth})
}

let replacedItems = (animation: animation): int => {
  if (animation.fromIndex < animation.toIndex) { // going right
    animation.toIndex - animation.fromIndex
  } else {
    0
  };
}

let elems = (state: state, config: config, dispatchCompleted: InfiniteSliderPadding.dispatchCompleted): list(reactComponent) => {

  switch (state.animationState) {
  | Idle => 
    let index = i => i + state.centered
    config.itemsWindow |> RangeOfInt.map(_, i => config.componentFactory(index(i), state.selected, id(config, index(i))));
  | Animating(animation) =>
    let paddingItem: reactComponent = 
      <InfiniteSliderPadding key="padding" command={state.paddingCommand} dispatchCompleted={dispatchCompleted} id={id_for_string(config, "padding")}/>;

    let index = i => animation.fromIndex + i
    let normalItems = config.itemsWindow
      |> RangeOfInt.drop(_, replacedItems(animation))
      |> RangeOfInt.map(_, i => config.componentFactory(index(i), state.selected, id(config, index(i))));

    [paddingItem, ...normalItems];
  };
};

let getItemSlotPlacement = (state: state, config: config): option(itemSlotPlacement) => {
    let doc = Webapi.Dom.document;
    let id0 = id(config, state.centered);
    let id1 = id(config, state.centered + 1);
    let left = (id: string): option(float) => {
      let e = Webapi.Dom.Document.getElementById(id, doc);
      // https://stackoverflow.com/a/18053642/1148030
      let boundingClientRect = Option.map(e, Webapi.Dom.Element.getBoundingClientRect);
      Option.map(boundingClientRect, Dom.DomRect.left);
    };
    Option.flatMap(left(id0), left0 => Option.map(left(id1), left1 => {
        {centeredLeftX: left0, width: left1 -. left0}
    }))
}

let handleClick = (state: state, config: config, click: ReactEvent.Mouse.t): unit => {
    Option.map(state.itemSlotPlacement, placement => {
      let clickX = float_of_int(ReactEvent.Mouse.clientX(click))

      let slot = switch (clickX > placement.centeredLeftX) {
        | true => (clickX -. placement.centeredLeftX) /. placement.width
        | false => (clickX -. placement.centeredLeftX) /. placement.width -. 1.0
      }
      
      let newSelected = state.selected + int_of_float(slot)
      config.itemSelectedDispatch(newSelected);

      Js.logMany(toArray(["handleClick item width:", Js.Float.toString(placement.width), 
      "clickX", Js.Float.toString(clickX),
      "slot", Js.Float.toString(slot),
      "newSelected", string_of_int(newSelected),
      "state.selected", string_of_int(state.selected)]));
      
    }) |> ignore
  };

let stateMachine = (state: state, action: event): state => {
  switch (action, state.animationState) {
  | (AnimationComplete(prevPaddingAnimationState), Animating(prevAnimation)) =>
    switch (state.queuedAnimation) {
    | Some(queuedAnimationToIndex) => 
      let (tSwitched, switchedAnimation) = switchAnimation(prevPaddingAnimationState, prevAnimation, queuedAnimationToIndex);
      {
        ...state,
        queuedAnimation: None,
        centered: switchedAnimation.fromIndex, 
        animationState: Animating(switchedAnimation),
        paddingCommand: animationPaddingCommand(tSwitched, state.itemSlotPlacement, switchedAnimation)
      }
    | _ => {...state, centered: prevAnimation.toIndex, queuedAnimation: None, animationState: Idle}
    }
  | (ChangeSelected(newSelected), Idle) =>
      let animation = {fromIndex: state.centered, toIndex: newSelected};
      let paddingCommand = animationPaddingCommand(0.0, state.itemSlotPlacement, animation);
      { 
        ...state,
        selected: newSelected,
        paddingCommand: paddingCommand,
        animationState: Animating(animation),
      }
  | (ChangeSelected(newSelected), Animating(_)) => 
      let queuedAnimationToIndex = if (state.centered == newSelected) {
        None
      } else {
        Some(newSelected)
      };
      {
        ...state,
        selected: newSelected,
        paddingCommand: Stop,
        queuedAnimation: queuedAnimationToIndex
      }
  | (SlotPlacement(info), _) => {
      ...state,
      itemSlotPlacement: info
    }
  | (AnimationComplete(_), Idle) =>
    Js.log("Should be impossible transition?")
    state
  }
};

let initialState = {selected: 0, centered: 0, queuedAnimation: None, animationState: Idle, itemSlotPlacement: None, paddingCommand: Nop};

let logTransition = ((state: state, dispatch: event => unit)) => {
  let wrapped: event => unit = (event: event): unit => {
    Js.log("transition on event" ++ stringOfEvent(event) ++ " to state " ++ stringOfState(stateMachine(state, event)))
    dispatch(event)
  };
  (state, wrapped)
};

[@react.component]
let make = (~config: config, ~selected: int) => {
  let rowClassName = config.styleBaseName ++ "Row";

  let (state, dispatch) = logTransition(React.useReducer(stateMachine, initialState));
  
  React.useEffect2(() => {
    if (state.selected != selected) {
      dispatch(ChangeSelected(selected))
    }
    None
    }, ((), selected))
  
  React.useLayoutEffect2(() => {
      if (state.animationState == Idle) {
        dispatch(SlotPlacement(getItemSlotPlacement(state, config)))
      }
      None
    }, ((), state.animationState))

  //ReactSwipeable.swipeTest();
  //ReactSwipeable.useSwipeableInternal("foo");

  let e: list(reactComponent) = elems(state, config, paddingAnimationState => {dispatch(AnimationComplete(paddingAnimationState))});

  // let jsonStringify: ('a) => string = [%bs.raw {|function(x){return JSON.stringify(x)}|}];
  
  <div className="infiniteSlider" onClick={event => handleClick(state, config, event)}>
    <div className=rowClassName>
      {asReact(e)}
    </div>
  </div>;
};