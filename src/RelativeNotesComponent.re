open RelativeNotesState;
open ReactUtil;

[@react.component]
let make = () => {
  let (state, dispatch) = React.useReducer(updateState, initialState);

  let componentFactory = (i: int, current: int, id: string): reactComponent => {
    let current = current == i;
    let note: Note.note = {offset: i};
    <RelativeNoteComponent
      current
      acceptEvent=dispatch
      key={string_of_int(note.offset)}
      id={id}
      note
    />;
  };

  let sliderConfig: InfiniteSlider.config = {
    componentFactory,
    styleBaseName: "relativeNotes",
    componentBaseName: "relativeNotes",
    itemSelectedDispatch: i => {
      let note: Note.note = {offset: i};
      dispatch(ClickNote(note))
    },
    itemsWindow: RangeOfInt.make((-12) * 1, 12 * 1),
    maxJump: 12,
  };


/*
  let (foo, fooSet) = React.useReducer((_, x) => {x}, true);
  React.useEffect(() => {
    if (foo) {
      fooSet(false);
      Js.Global.setTimeout(
        () => {
          dispatch(ClickNote({offset: -1})) // 100ms -2
        },
        100
      ) |> ignore;
      Js.Global.setTimeout(
        () => {
          dispatch(ClickNote({offset: -2})) // 200ms 1
        },
        500
      ) |> ignore;
    }
    None;
  });*/

  <InfiniteSlider config=sliderConfig selected={state.currentNote.offset} />;
};