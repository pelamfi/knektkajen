open RelativeNotesState
open RelativeNotesViewModel
open ReactUtil
open Belt.List

let component = ReasonReact.reducerComponent("RelativeNotesComponent");


let make = (_children) => {
  ...component,

  initialState: () => initialState,
  reducer: (event, state) => ReasonReact.Update(updateState(event, state)),

  render: self => {
    let acceptEvent: acceptEvent = self.send
    let noteElems = notesBoxNotes(self.state) |> map(_, note =>
      <RelativeNoteComponent state={self.state} acceptEvent={acceptEvent} key={string_of_int(note.offset)} note={note}/>
    );

    let paddingClass = "relativeNotesPadding-" ++ string_of_int(12 - Note.moduloOffset(self.state.currentNote));
    
    <Fragment>
      <div className="relativeNotesRow">
        <div className={paddingClass}/>
        {asReact(noteElems)}
      </div>
    </Fragment>
  },
};
