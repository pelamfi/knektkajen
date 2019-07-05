open Note;
open Belt;

type stateChange =  
  | CurrentNoteChanged(note);

type state = {
  currentNote: note,
  listeners: list((stateChange) => unit)
  };

type listener = (stateChange) => unit

type event =
  | ClickNote(note)
  | ClickInterval(interval)
  | RegisterListener(listener)
  | UnregisterListener(listener);


type acceptEvent = event => unit;

let initialState: state = {currentNote: middleC, listeners: []};

let emit = (state: state, stateChange: stateChange) => {
  List.forEach(state.listeners, listener => {
    listener(stateChange)
  })
}

let updateState = (state: state, event: event): state => {
  let newState: state = switch (event) {
  | ClickNote(newCurrentNote) => 
    let newState = {...state, currentNote: newCurrentNote}
    emit(state, CurrentNoteChanged(newState.currentNote))
    newState
  | ClickInterval(interval) => 
    let newState = {...state, currentNote: apply(state.currentNote, interval)}
    emit(state, CurrentNoteChanged(newState.currentNote))
    newState
  | RegisterListener(listener) => {...state, listeners: [listener, ...state.listeners]}
  | UnregisterListener(listener) => {...state, listeners: List.keep(state.listeners, registered => {registered !== listener})}
  };

  newState
};

let stateRef: ref(state) = ref(initialState)

let dispatch = (event: event): unit => {
  stateRef := updateState(stateRef^, event)
}
