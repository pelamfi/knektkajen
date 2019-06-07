open RelativeNotesState
open RelativeNotesViewModel
open ReactUtil
open Belt.List

[@react.component]
let make = () => {

  let (state, dispatch) = React.useReducer(updateState, initialState)

  let componentFactory = (i: int, current: int): reactComponent => { 
    let current = current == i;
    let note: Note.note = {offset: i};
    <RelativeNoteComponent current={current} acceptEvent={dispatch} key={string_of_int(note.offset)} note={note}/>
  }

  let sliderConfig: InfiniteSlider.config = {
    componentFactory: componentFactory,
    styleBaseName: "relativeNotes",
    itemsWindow: Range.make(-12*2, 12*2),
    maxJump: 12
  };

  <InfiniteSlider config={sliderConfig} current={state.currentNote.offset}/>
}
