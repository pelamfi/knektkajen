open Note;
open Belt;

// Determines which voices can trigger again without stopping previous voices
type voiceKey =   
  | Single(note)

type trigger =
  | IntervalAttack(interval)
  | IntervalRelease(interval)
  | Click;

type voiceState =
  | Idle
  | AttackRelease
  | Attack
  | Release;

type voice = {key: voiceKey, updateIndex: int, trigger, state: voiceState, prevState: voiceState};

type stateChange =
  | CurrentNoteChanged(note)
  | Voice(voice);

type state = {
  currentNote: note,
  updateIndex: int,
  voices: list(voice),
  listeners: list(stateChange => unit),
  lastUpdate: list(stateChange)
};

type listener = stateChange => unit;

type event =
  | ClickNote(note)
  | ClickInterval(interval)
  | KeyDownInterval(interval)
  | KeyUpInterval(interval)
  | RegisterListener(listener)
  | UnregisterListener(listener);

type acceptEvent = event => unit;

let initialState: state = {currentNote: middleC, updateIndex: 0, voices: [], listeners: [], lastUpdate: []};

let emit = (state: state, stateChange: stateChange) => {
  List.forEach(state.listeners, listener => listener(stateChange));
};

let voiceStateOfTrigger = (trigger: trigger): voiceState => {
  switch(trigger) {
    | IntervalAttack(_) => Attack
    | IntervalRelease(_) => Release
    | Click => AttackRelease
  }
};

let triggerVoice = (voiceKey: voiceKey, trigger: trigger, state: state): state => {

  let (updatedVoice, otherVoices: list(voice)) = switch (List.partition(state.voices, voice => voice.key == voiceKey)) {
    | ([existing, ..._], others) => // there should be only 1 matching the key
      ({key: voiceKey, updateIndex: state.updateIndex, trigger, state: voiceStateOfTrigger(trigger), prevState: existing.state}, others)
    | ([], others) =>
      ({key: voiceKey, updateIndex: state.updateIndex, trigger, state: voiceStateOfTrigger(trigger), prevState: Idle}, others)
  };

  {...state, voices: [updatedVoice, ...otherVoices], lastUpdate: [Voice(updatedVoice), ...state.lastUpdate]}
};

let updateState = (prevState: state, event: event): state => {
  let state = {...prevState, updateIndex: prevState.updateIndex + 1, lastUpdate: []}
  let newState: state =
    switch (event) {
    | ClickNote(newCurrentNote) =>
      {...triggerVoice(Single(newCurrentNote), Click, state), 
        currentNote: newCurrentNote, 
        lastUpdate: [CurrentNoteChanged(state.currentNote), ...state.lastUpdate]}
    | ClickInterval(interval) =>
      let newCurrentNote: note = noteApplyInterval(state.currentNote, interval);
      {...triggerVoice(Single(newCurrentNote), Click, state), 
        currentNote: newCurrentNote, 
        lastUpdate: [CurrentNoteChanged(state.currentNote), ...state.lastUpdate]}
    | KeyDownInterval(interval) =>
      let newCurrentNote: note = noteApplyInterval(state.currentNote, interval);
      {...triggerVoice(Single(newCurrentNote), IntervalAttack(interval), state), 
        currentNote: newCurrentNote, 
        lastUpdate: [CurrentNoteChanged(state.currentNote), ...state.lastUpdate]}
    | KeyUpInterval(interval) =>
      let newCurrentNote: note = noteApplyInterval(state.currentNote, interval);
      {...triggerVoice(Single(newCurrentNote), IntervalRelease(interval), state), 
        currentNote: newCurrentNote, 
        lastUpdate: [CurrentNoteChanged(state.currentNote), ...state.lastUpdate]}
    | RegisterListener(listener) => {
        ...state,
        listeners: [listener, ...state.listeners],
      }
    | UnregisterListener(listener) => {
        ...state,
        listeners:
          List.keep(state.listeners, registered => registered !== listener),
      }
    };

  List.forEach(newState.lastUpdate, emit(state))

  newState;
};

let stateRef: ref(state) = ref(initialState);

let dispatch = (event: event): unit => {
  stateRef := updateState(stateRef^, event);
};

// To help wrapping in react effects
let listenerEffect = (listener: (stateChange) => unit, dispatch: acceptEvent, _): option(unit => unit) => {
    dispatch(RegisterListener(listener));
    Some(() => dispatch(UnregisterListener(listener)));
};
