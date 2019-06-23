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
  queuedAnimation: option(animation),
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

let slideAnimationDurationMs = 1000.0

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
    state.queuedAnimation |> Option.mapWithDefault(_, "None", stringOfAnimation)
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

/*
type animationVars = {pxPos: float, replacedItems: int, spanItems: int, currentItem: int, tInsideItem: float, tDirectional: float}

let computeAnimationVars = (animation: animation, itemSlotPlacement: option(itemSlotPlacement)): animationVars => {
  let {fromIndex, toIndex, t} = animation;

  let (replacedItems, spanItems, tDirectional) = if (fromIndex < toIndex) { // going right
    (toIndex - fromIndex, toIndex - fromIndex, 1.0 -. t); // going right, shrink padding from full to 0, replace n items
  } else {
    (0, fromIndex - toIndex, t); // going left, grow padding from 0 to required amount
  };

  let pxPos = Option.mapWithDefault(itemSlotPlacement , 0.0, isp => {tDirectional *. float_of_int(spanItems) *. isp.width});

  let tPerItems = t /. float_of_int(spanItems)
  let tInsideItem = JsUtil.fmod(t, float_of_int(spanItems))
  let currentItem = int_of_float(tPerItems);

  {replacedItems, spanItems, pxPos, tDirectional, tInsideItem, currentItem};
}

let string_of_animationVars = (a: animationVars): string => {
  Printf.sprintf("[replacedItems:%d, spanItems:%d, pxPos:%f, tDirectional:%f, tInsideItem:%f, currentItem:%d]", 
    a.replacedItems, a.spanItems, a.pxPos, a.tDirectional, a.tInsideItem, a.currentItem)
}

let switch_animation = (itemSlotPlacement: option(itemSlotPlacement), prev: animation, next: animation): animation => {
  let prevVars = computeAnimationVars(prev, itemSlotPlacement)
  let nextVars = computeAnimationVars(next, itemSlotPlacement)
  let fromIndex = prev.fromIndex + prevVars.currentItem;
  let tSwitched = prevVars.tInsideItem /. float_of_int(nextVars.spanItems) // Option.mapWithDefault(itemSlotPlacement, 0.0, isp => {prevVars.tInsideItem *. isp.width});
  Js.log("switch_animation tSwitched:" ++ string_of_float(tSwitched) ++ " fromIndex:" ++ string_of_int(fromIndex) );
  {fromIndex, toIndex: next.toIndex, t: tSwitched}
};
*/

let switchAnimation = (itemSlotPlacement: option(itemSlotPlacement), prevPaddingState: InfiniteSliderPadding.animationState, prevAnimation: animation, animation): (InfiniteSliderPadding.command, animation) => {
  let prevSpanItems = Js.Math.abs_int(prevAnimation.fromIndex - prevAnimation.toIndex);
  let nextSpanItems = Js.Math.abs_int(animation.fromIndex - animation.toIndex);
  let tPerPrevItems = prevPaddingState.t /. float_of_int(prevSpanItems);
  let tInsideItem = JsUtil.fmod(prevPaddingState.t, float_of_int(prevSpanItems));
  let currentItemInPrevAnimation = int_of_float(tPerPrevItems);
  let fromIndexNew = prevAnimation.fromIndex + currentItemInPrevAnimation;
  //let tSwitched = tInsideItem ./ float_of_int(nextSpanItems);
  (
    Start(
      {t: 0.0, 
        durationMs: slideAnimationDurationMs, startWidth: 0.0, endWidth: 100.0
      }), 
    {fromIndex: fromIndexNew, toIndex: animation.toIndex}
  )
}

let animationPaddingCommand = (itemSlotPlacement: option(itemSlotPlacement), animation): InfiniteSliderPadding.command => {
  let {fromIndex, toIndex} = animation;
  let (fromItems, toItems) = if (fromIndex < toIndex) { // going right
    (float_of_int(toIndex - fromIndex), 0.0) // replace some elements & shrink to create illusion of scrolling right
  } else {
    (0.0, float_of_int(fromIndex - toIndex)) // grow
  };
  let (startWidth, endWidth) = Option.mapWithDefault(itemSlotPlacement, (0.0, 0.0), isp => {(fromItems *. isp.width, toItems *. isp.width)});
  Start({t: 0.0, 
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
    | Some(queuedAnimation) => 
      let (switchedPaddingCommand, switchedAnimation) = switchAnimation(state.itemSlotPlacement, prevPaddingAnimationState, prevAnimation, queuedAnimation);
      {
        ...state,
        queuedAnimation: None,
        centered: switchedAnimation.fromIndex, 
        animationState: Animating(switchedAnimation),
        paddingCommand: switchedPaddingCommand
      }
    | _ => {...state, centered: prevAnimation.toIndex, queuedAnimation: None, animationState: Idle}
    }
  | (ChangeSelected(newSelected), Idle) =>
      let animation = {fromIndex: state.centered, toIndex: newSelected};
      let paddingCommand = animationPaddingCommand(state.itemSlotPlacement, animation);
      { 
        ...state,
        selected: newSelected,
        paddingCommand: paddingCommand,
        animationState: Animating(animation),
      }
  | (ChangeSelected(newSelected), Animating(_)) => 
      let queuedAnimation = if (state.centered == newSelected) {
        None
      } else {
        Some({fromIndex: state.centered, toIndex: newSelected})
      };
      {
        ...state,
        selected: newSelected,
        paddingCommand: Stop,
        queuedAnimation: queuedAnimation
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

  let dispatch = (event) => {
    Js.log(stringOfAnimationState(state.animationState));
    dispatch(event)
  }
  
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