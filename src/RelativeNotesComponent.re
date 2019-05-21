open RelativeNotesState
open RelativeNotesViewModel
open ReactUtil
open Belt.List

let component = ReasonReact.reducerComponent("RelativeNotesComponent");




let make = (_children) => {
  {
    ...component,

    initialState: () => initialState,
    reducer: (event, state) => ReasonReact.Update(updateState(event, state)),
    
    render: self => {
      let componentFactory = (i: int, current: int) => { 
        let current = current == i;
        let acceptEvent: acceptEvent = self.send;
        let note: Note.note = {offset: i};
        <RelativeNoteComponent current={current} acceptEvent={acceptEvent} key={string_of_int(note.offset)} note={note}/>
      }
      
      let sliderConfig: InfiniteSlider.config = {
        componentFactory: componentFactory,
        styleBaseName: "relativeNotes",
        itemsWindow: Range.make(-12*2, 12*2),
        maxJump: 12
      };
      
      <Fragment>
        <InfiniteSlider config={sliderConfig} current={self.state.currentNote.offset}/>
      </Fragment>
    },
  }
}
