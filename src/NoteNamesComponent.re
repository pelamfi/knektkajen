open ReactUtil;
open Note

let noteNameFactory = (dispatch: RelativeNotesState.acceptEvent, i: int, current: int, id: string): reactComponent => {
  let current = current == i;
  let note: Note.note = RelativeNotesState.loopOctaves({offset: i});
  <RelativeNoteComponent
    current
    acceptEvent=dispatch
    key={string_of_int(i)}
    cssClass="noteNameCell"
    renderContent=Note.nameOfNoteInCMajor
    id
    note
  />;
};

let sliderConfig = (dispatch: RelativeNotesState.acceptEvent): InfiniteSlider.config => {
  componentFactory: noteNameFactory(dispatch),
  styleBaseName: "relativeNotes",
  componentBaseName: "relativeNotes",
  itemSelectedDispatch: i => {
    dispatch(NoteTrigger(NoteClick({offset: i}, MouseClick)));
  },
  itemsWindow: RelativeNotesState.intervalStepsInUi,
  maxJump: 8,
};

[@react.component]
let make = (~currentNote: note, ~style: ReactDOMRe.Style.t = ReactUtil.emptyStyle) => {
  <InfiniteSlider style config={sliderConfig(RelativeNotesState.dispatch)} selected={currentNote.offset} />;
};