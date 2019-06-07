open ReactUtil
open Belt.List

type componentFactory = (int, int) => reactComponent;

type animatingWithQueue = {fromIndex: int, toIndex: int, toIndexQueued: int};
type animating = {fromIndex: int, toIndex: int};

type slideState = 
  | Idle 
  | Animating(animating) // CSS animation slide and a timer running
  | AnimatingWithQueue(animatingWithQueue) // if another prop change comes while we animate
;

type slideState2 = 
  | Idle 
  | Animating(animating) // CSS animation slide and a timer running
;

type state = {
    current: int, 
    timeoutId: option(Js.Global.timeoutId),
    slideState};

let string_of_slide_state = (state: slideState): string => {
    switch(state) {
      | Idle => "Idle"
      | Animating({fromIndex, toIndex}) => "Animating("++ string_of_int(fromIndex) ++ ", " ++ string_of_int(toIndex) ++ ")"
      | AnimatingWithQueue({fromIndex, toIndex, toIndexQueued}) => "AnimatingWithQueue("++ string_of_int(fromIndex) ++ ", " ++ string_of_int(toIndex) ++ ", " ++ string_of_int(toIndexQueued) ++ ")"
    }
}

let string_of_state = (state: state): string => {
    "[current:" ++ string_of_int(state.current) ++ " slideState: " ++ string_of_slide_state(state.slideState) ++ "]"
}

type event = 
| AnimationComplete
| ChangeCurrent(int)
| AnimationStart(int, int)

let string_of_event = (event: event): string => {
    switch(event) {
        | AnimationComplete => "AnimationComplete"
        | AnimationStart(from, toIndex) => "AnimationStart(" ++ string_of_int(from) ++ ", " ++ string_of_int(toIndex) ++ ")"
        | ChangeCurrent(current) => "CahangeCurrent(current)"
    }
}

type config = {
    componentFactory: componentFactory,
    styleBaseName: string,
    itemsWindow: Range.range, // current is at 0
    maxJump: int
};

let paddingCount = (state: state, maxJump: int): int => {
    switch(state.slideState) {
        | Idle => 1 + maxJump
        | Animating({fromIndex, toIndex}) | AnimatingWithQueue({fromIndex, toIndex}) => min(max(1, (1 + maxJump) - (toIndex - fromIndex)), 1 + 2 * maxJump)
    }
}

let elems = (state: state, config: config): list(reactComponent) => {
    let offset = switch(state.slideState) {
        | Idle => state.current
        | Animating({fromIndex}) | AnimatingWithQueue({fromIndex}) => fromIndex
    }
    config.itemsWindow |> Range.map(_, i => config.componentFactory(i + offset, state.current));
};

[@react.component]
let make = (~config: config, ~current: int) => {
  let rowClassName = config.styleBaseName ++ "Row";   
  let (slideState, setSlideState) = React.useReducer((_, newState: slideState2): slideState2 => newState, Idle);

  let (retainedCurrent, setRetainedCurrent) = React.useReducer((_, newCurrent: int): int => newCurrent, 0);

  let (state, dispatch) =
    React.useReducer(
      (state: state, action: event) => {
            let newState = switch(action) {
                | AnimationComplete => 
                switch (state.slideState) {
                    | Idle => state
                    | Animating(_) => {...state, timeoutId: None, slideState: Idle}
                    | AnimatingWithQueue({toIndex, toIndexQueued}) => 
                    //let newSlideState2: slideState2 = Animating({fromIndex: toIndex, toIndex: toIndexQueued});
                    //setSlideState(newSlideState2);
                    {...state, timeoutId: None, slideState: Idle}
                }
                | AnimationStart(fromIndex, toIndex) =>
                    {...state, slideState: Animating({fromIndex, toIndex})}
                | ChangeCurrent(newCurrent) =>
                    {...state, current: newCurrent}
            };

            Js.log("state " ++ string_of_state(newState) ++ "->" ++ string_of_state(newState) ++ " on " ++ string_of_event(action) )
            newState
      }, {current: 0, timeoutId: None, slideState: Idle});

    React.useEffect1(() => {
        switch slideState {
            | Animating({fromIndex, toIndex}) =>
            dispatch(AnimationStart(toIndex, toIndex))
            let timeoutId = Js.Global.setTimeout(() => dispatch(AnimationComplete), 333)
            Some(() => Js.Global.clearTimeout(timeoutId))
            | Idle => None
        }
    }, Belt.List.toArray([slideState]));

    if (current !== retainedCurrent) {
        dispatch(ChangeCurrent(current))
        setRetainedCurrent(current)
        Js.log("current " ++ string_of_int(retainedCurrent) ++ "->" ++ string_of_int(current));
    }

    let paddingAnimClass = switch(slideState) {
        | Animating(_) => " " ++ config.styleBaseName ++ "Padding-anim"
        | Idle => ""
    }

    let paddingClass = config.styleBaseName ++ "Padding-" ++ string_of_int(paddingCount(state, config.maxJump))
        ++ paddingAnimClass;

    let e: list(reactComponent) = elems(state, config);

    Js.log("paddingclass " ++ paddingClass);

    <div className="infiniteSlider">
        <div className={rowClassName}>
            <div className={paddingClass}/>
            {asReact(e)}
        </div>
    </div>
};
