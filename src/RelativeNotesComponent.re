open RelativeNotesState
open RelativeNotesViewModel
open ReactUtil
open Belt.List

[@react.component]
let make = () => {
  //initialState: () => initialState,
  //reducer: (event, state) => ReasonReact.Update(updateState(event, state)),

  let componentFactory = (i: int, current: int): reactComponent => { 
    let current = current == i;
    //let acceptEvent/*: acceptEvent*/ = Js.log("dispatch TODO"); // self.send;
    let note: Note.note = {offset: i};
    <RelativeNoteComponent current={current} /*acceptEvent={acceptEvent}*/ key={string_of_int(note.offset)} note={note}/>
  }

  let sliderConfig: InfiniteSlider.config = {
    componentFactory: componentFactory,
    styleBaseName: "relativeNotes",
    itemsWindow: Range.make(-12*2, 12*2),
    maxJump: 12
  };

  //<fragment>
    <InfiniteSlider config={sliderConfig} current={0/*self.state.currentNote.offset*/}/>
  //</fragment>
}
