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
    itemsWindow: RangeOfInt.make((-12) * 2, 12 * 2),
    maxJump: 12,
  };

  <InfiniteSlider config=sliderConfig selected={state.currentNote.offset} />;
};