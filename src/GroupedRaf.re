open Belt
open Js.Array2

// Do just 1 requestAnimationFrame callback and call all registered functions in that.
// The idea is to ensure that all components get their callbacks called or none.
// This should help animations stay in sync.

type callback = (float) => unit

type mplexRequest = {id: int, callback: callback}
let dispatches: array(mplexRequest) = [||];

type cancel = unit => unit;

let idCounter: ref(int) = ref(0);

let mplexRafId: ref(option(Webapi.rafId)) = ref(None);

let rec rafCallback = (timerMs: float) => {
  mplexRafId := Some(Webapi.requestCancellableAnimationFrame(rafCallback));
  forEach(dispatches, dispatch => {dispatch.callback(timerMs)})
};

let register = (callback: callback): cancel => {
  let id = idCounter^ + 1
  let mplexReq: mplexRequest = {id, callback}
  idCounter := id;
  push(dispatches, mplexReq) |> ignore;
  if (length(dispatches) == 1) {
    mplexRafId := Some(Webapi.requestCancellableAnimationFrame(rafCallback));
    ()
  } else {
    ()
  };
  () => {
    findIndex(dispatches, x => {mplexReq.id == x.id}) |> Js.Array2.removeCountInPlace(dispatches, _, 1) |> ignore
    if (length(dispatches) == 0) {
      let id: option(Webapi.rafId) = mplexRafId^;
      id |> Option.map(_, Webapi.cancelAnimationFrame) |> ignore;
      mplexRafId := None;
      ()
    } else {
      ()
    };
  }
}
