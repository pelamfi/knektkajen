open Note;
type state = {currentNote: note};

type event =
  | ClickNote(note)
  | ClickInterval(interval);

type acceptEvent = event => unit;

let initialState: state = {currentNote: middleC};

let updateState = (current: state, event: event): state => {
  switch (event) {
  | ClickNote(newCurrentNote) => {currentNote: newCurrentNote}
  | ClickInterval(interval) => {currentNote: apply(current.currentNote, interval)}
  };
};