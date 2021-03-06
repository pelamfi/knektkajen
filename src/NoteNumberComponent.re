open ReactUtil;
open Note

let noteNameFactory = (dispatch: RelativeNotesState.acceptEvent, i: int, current: int, id: string): reactComponent => {
  let current = current == i;
  let octaveLoopedNote: Note.note = RelativeNotesState.loopOctaves({offset: i});
  let note: Note.note = Note.oneLinedNoteOfNote(octaveLoopedNote);
  <RelativeNoteComponent
    current
    acceptEvent=dispatch
    key={string_of_int(i)}
    cssClass="noteNumberCell"
    renderContent={note => {string_of_int(note.offset)}}
    id
    note
  />;
};

let sliderConfig = (dispatch: RelativeNotesState.acceptEvent): InfiniteSlider.config => {
  componentFactory: noteNameFactory(dispatch),
  styleBaseName: "noteNumbers",
  componentBaseName: "noteNumbers",
  itemSelectedDispatch: i => {
    dispatch(NoteTrigger(NoteClick({offset: i}, MouseClick)));
  },
  itemsWindow: RelativeNotesState.intervalStepsInUi,
  maxJump: 8,
};

[@react.component]
let make = (~currentNote: note, ~style: ReactDOMRe.Style.t = ReactUtil.emptyStyle) => {
  <InfiniteSlider style className="noteNumbersStrip" config={sliderConfig(RelativeNotesState.dispatch)} selected={currentNote.offset} />;
};