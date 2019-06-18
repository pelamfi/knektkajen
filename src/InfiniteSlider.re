open ReactUtil;
open Belt.List;
open Belt
open Webapi;
//open Webapi.Dom.Element;

type componentFactory = (int, int, string) => reactComponent;

type animating = {
  fromIndex: int,
  toIndex: int,
  t: float,
};

type slideState =
  | Idle
  | RequestStop(animating)
  | Animating(animating); // CSS animation slide and a timer running

type itemSlotPlacement = {centeredLeftX: float, width: float}

type state = {
  selected: int,
  centered: int, // if animation is running, this is where the last centering animation was aiming for (toIndex)
  animationToIndexQueued: option(animating),
  slideState,
  itemSlotPlacement: option(itemSlotPlacement)
};

let string_of_animating = (a: animating): string => {
  "{"
    ++ string_of_int(a.fromIndex)
    ++ ", "
    ++ string_of_int(a.toIndex)
    ++ ", "
    ++ Js.Float.toString(a.t)
    ++ "}"
}

let slideAnimationDuration = 3333

let string_of_slide_state = (state: slideState): string => {
  switch (state) {
  | Idle => "Idle"
  | RequestStop(animating) => "RequestStop(" ++ string_of_animating(animating) ++ ")"
  | Animating(animating) =>
    "Animating(" ++ string_of_animating(animating) ++ ")"
  };
};

let string_of_state = (state: state): string => {
  "[ selected:"
  ++ string_of_int(state.selected)
  ++ " centered:"
  ++ string_of_int(state.centered)
  ++ " slideState:"
  ++ string_of_slide_state(state.slideState)
  ++ " animationToIndexQueued:"
  ++ (
    state.animationToIndexQueued |> Option.mapWithDefault(_, "None", string_of_animating)
  )
  ++ "]";
};

type event =
  | AnimationComplete(animating)
  | ChangeSelected(int)
  | SlotPlacement(option(itemSlotPlacement));

let string_of_event = (event: event): string => {
  switch (event) {
  | AnimationComplete(animating) => "AnimationComplete(" ++ string_of_animating(animating) ++ ")"
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
}

let paddingWidthStyle = (dist: float): string => {
  string_of_int(int_of_float(dist)) ++ "px"
} 

type animationVars = {pxPos: float, replacedItems: int, spanItems: int, currentItem: int, tInsideItem: float, tDirectional: float}

let computeAnimationVars = (animating: animating, itemSlotPlacement: option(itemSlotPlacement)): animationVars => {
  let {fromIndex, toIndex, t} = animating;

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

let switch_animation = (itemSlotPlacement: option(itemSlotPlacement), prev: animating, next: animating): animating => {
  let prevVars = computeAnimationVars(prev, itemSlotPlacement)
  let nextVars = computeAnimationVars(next, itemSlotPlacement)
  let fromIndex = prev.fromIndex + prevVars.currentItem;
  let tSwitched = prevVars.tInsideItem /. float_of_int(nextVars.spanItems) // Option.mapWithDefault(itemSlotPlacement, 0.0, isp => {prevVars.tInsideItem *. isp.width});
  Js.log("switch_animation tSwitched:" ++ string_of_float(tSwitched) ++ " fromIndex:" ++ string_of_int(fromIndex) );
  {fromIndex, toIndex: next.toIndex, t: tSwitched}
};

let elems = (state: state, config: config): list(reactComponent) => {
  let workaround = (animating: animating): list(reactComponent) => {
    let animationVars = computeAnimationVars(animating, state.itemSlotPlacement)
    Js.log("ELEMS" ++ string_of_animationVars(animationVars));
    let widthStyle = paddingWidthStyle(animationVars.pxPos);
    let style = ReactDOMRe.Style.make(~background="red", ~width = widthStyle, ());
    let paddingItem: reactComponent = <div key="padding" id={id_for_string(config, "padding")} className="infiniteSliderAnimationPadding" style={style} />;
    let index = i => animating.fromIndex + i
    let normalItems = config.itemsWindow
      |> RangeOfInt.drop(_, animationVars.replacedItems)
      |> RangeOfInt.map(_, i => config.componentFactory(index(i), state.selected, id(config, index(i))));

    [paddingItem, ...normalItems];
  };

  switch (state.slideState) {
  | Idle => 
    let index = i => i + state.centered
    config.itemsWindow |> RangeOfInt.map(_, i => config.componentFactory(index(i), state.selected, id(config, index(i))));
  | Animating(animating) | RequestStop(animating) =>
    workaround(animating) // Work around bug https://github.com/BuckleScript/bucklescript/issues/3609
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

[@react.component]
let make = (~config: config, ~selected: int) => {
  let rowClassName = config.styleBaseName ++ "Row";

  let (state, dispatch) =
    React.useReducer(
      (state: state, action: event) => {
        let newState =
          switch (action) {
          | AnimationComplete(prevAnimating) =>
            switch (state.animationToIndexQueued) {
            | Some(queuedAnimation) => 
              let switchedAnimation = switch_animation(state.itemSlotPlacement, prevAnimating, queuedAnimation);
              {
                ...state,
                animationToIndexQueued: None,
                centered: switchedAnimation.fromIndex, 
                slideState: Animating(switchedAnimation),
              }
            | _ => {...state, centered: prevAnimating.toIndex, animationToIndexQueued: None, slideState: Idle}
            }
          | ChangeSelected(newSelected) =>
            switch (state.slideState) {
            | Idle => {
                ...state,
                selected: newSelected,
                slideState:
                  Animating({fromIndex: state.centered, toIndex: newSelected, t: 0.0}),
              }
            | Animating(animating) | RequestStop(animating) => 
              let queuedAnimation = if (state.centered == newSelected) {
                None
              } else {
                Some({fromIndex: state.centered, toIndex: newSelected, t: 0.0})
              };
              {
                ...state,
                selected: newSelected,
                animationToIndexQueued: queuedAnimation,
                slideState: RequestStop(animating)
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
      {selected: 0, centered: 0, animationToIndexQueued: None, slideState: Idle, itemSlotPlacement: None},
    );

  Js.log(string_of_slide_state(state.slideState));

  React.useEffect2(
    () => {
      let tRef: ref(option(float)) = ref(None)
      switch (state.slideState) {
      | Animating(animating) =>
        let startTimestamp: ref(option(float)) = ref(None)
        let rafId: ref(option(Webapi.rafId)) = ref(None)
        let rec rafCallback = (time: float) => {
          let startTime = startTimestamp^ |> Option.mapWithDefault(_, time, x => x)
          rafId := Some(Webapi.requestCancellableAnimationFrame(rafCallback))
          startTimestamp := Some(startTime)
          let doc = Webapi.Dom.document;
          let t = (time -. startTime) /. float_of_int(slideAnimationDuration);
          tRef := Some(t)
          let newAnimating = {...animating, t: t}
          let animVars = computeAnimationVars(newAnimating, state.itemSlotPlacement);
          // Js.log(widthStyle ++ " foo " ++ (state.animationToIndexQueued |> Option.mapWithDefault(_, "-", a => string_of_int(a.toIndex))));
          let e = Webapi.Dom.Document.getElementById(id_for_string(config, "padding"), doc);
          let widthStyle = paddingWidthStyle(animVars.pxPos)
          Option.map(e, Webapi.Dom.Element.setAttribute("style", "width: " ++ widthStyle)) |> ignore;

          if (t >= 1.0) {
            Js.log("ANIMATION COMPLETE, END " ++ Js.Float.toString(t));
            tRef := None
            dispatch(AnimationComplete(newAnimating))
          }
        }
        rafId := Some(Webapi.requestCancellableAnimationFrame(rafCallback))
        Some(() => {
          if (tRef^ != None) {
            let finalT = Option.mapWithDefault(tRef^, 1.0, x => x)
            Js.log("ANIMATION COMPLETE, CLEANUP " ++ Js.Float.toString(finalT))
            dispatch(AnimationComplete({...animating, t: finalT}))
          }
          rafId^ |> Option.map(_, Webapi.cancelAnimationFrame) |> ignore
          })
      | RequestStop(_) =>
        // Not idle, but this causes the previous effect to complete, then we can start the next animation once the
        // AnimationComplete event arrives
        None
      | Idle => 
        dispatch(SlotPlacement(getItemSlotPlacement(state, config)))
        None
      }},
  (0, string_of_slide_state(state.slideState)),
  );

  if (selected !== state.selected) {
    dispatch(ChangeSelected(selected));
  };

  //ReactSwipeable.swipeTest();
  //ReactSwipeable.useSwipeableInternal("foo");

  let e: list(reactComponent) = elems(state, config);

  // let jsonStringify: ('a) => string = [%bs.raw {|function(x){return JSON.stringify(x)}|}];
  
  <div className="infiniteSlider" onClick={event => handleClick(state, config, event)}>
    <div className=rowClassName>
      {asReact(e)}
    </div>
  </div>;
};