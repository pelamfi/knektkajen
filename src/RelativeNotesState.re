open Note;
type state = {currentNote: note};

type event =
  | ClickNote(note);

type acceptEvent = event => unit;

let initialState: state = {currentNote: middleC};

let updateState = (current: state, event: event): state => {
  switch (event) {
  | ClickNote(newCurrentNote) => {currentNote: newCurrentNote}
  };
};