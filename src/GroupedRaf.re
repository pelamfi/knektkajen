open Belt;
open Js.Array;

// Do just 1 requestAnimationFrame callback and call all registered functions in that.
// The idea is to ensure that all components get their callbacks called or none.
// This should help animations stay in sync.


type mplexRequest = {id: int, actionDispatch: actionDispatch}
let dispatches: array(mplexRequest) = [||];

type cancel = unit => unit;

let idCounter: ref(int) = ref(0);

let mplexRafId: ref(option(Webapi.rafId)) = ref(None);

let rec rafCallback = (timerMs: float) => {
  mplexRafId := Some(Webapi.requestCancellableAnimationFrame(rafCallback));
  let message: event = Frame(timerMs)
  Js.Array2.forEach(dispatches, dispatch => {dispatch.actionDispatch(message)})
};

let requestCancellableAnimationFrame = (actionDispatch: actionDispatch): cancel => {
  let id = idCounter^ + 1
  let mplexReq: mplexRequest = {id, actionDispatch}
  idCounter := id;
  Js.Array.push(mplexReq, dispatches) |> ignore;
  if (Js.Array.length(dispatches) == 1) {
    mplexRafId := Some(Webapi.requestCancellableAnimationFrame(rafCallback));
    ()
  } else {
    ()
  };
  () => {
    Js.Array2.findIndex(dispatches, x => {mplexReq.id == x.id}) |> Js.Array2.removeCountInPlace(dispatches, _, 1) |> ignore
    if (Js.Array.length(dispatches) == 0) {
      let id: option(Webapi.rafId) = mplexRafId^;
      id |> Option.map(_, Webapi.cancelAnimationFrame) |> ignore;
      mplexRafId := None;
      ()
    } else {
      ()
    };
  }
}
