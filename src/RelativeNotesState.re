open Note;
//open Belt;

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
  Belt.List.forEach(state.listeners, listener => listener(stateChange));
};

let voiceStateOfTrigger = (trigger: trigger): voiceState => {
  switch(trigger) {
    | IntervalAttack(_) => Attack
    | IntervalRelease(_) => Release
    | Click => AttackRelease
  }
};

// https://stackoverflow.com/questions/49944067/join-array-of-strings
let commaSeparated = (items: list(string)): string => {
  switch(items) {
    | [] => ""
    | [single] =>  single
    | [head, ...tail] => List.fold_left((a, b) => {a ++ ", " ++ b}, head, tail)
  }
}

let stringOfVoiceKey = (voiceKey: voiceKey): string => {
  switch(voiceKey) {
    | Single(note) => "Single(" ++ Note.nameOfNoteInCMajor(note) ++ ")"
  }
}

let stringOfVoiceState = (voiceState: voiceState): string => {
  switch(voiceState) {
    | Idle => "Idle"
    | AttackRelease => "AttackRelease"
    | Attack => "Attack"
    | Release => "Release"
  }
}

let stringOfVoice = (voice: voice): string => {
  Printf.sprintf("[%s, state: %s]", stringOfVoiceKey(voice.key), stringOfVoiceState(voice.state))
}

let stringOfStateChange = (stateChange: stateChange): string => {
  switch(stateChange) {
    | CurrentNoteChanged(note) => "CurrentNoteChanged(" ++ Note.nameOfNoteInCMajor(note) ++ ")"
    | Voice(voice) => "Voice(" ++ stringOfVoice(voice) ++ ")"
  }
}

let stringOfState = (state: state): string => {
  Printf.sprintf("[currentNote: %s, updateIndex:%d, voices: [%s], lastUpdate: [%s]]", 
    Note.nameOfNoteInCMajor(state.currentNote), state.updateIndex,
    List.map(stringOfVoice, state.voices) |> commaSeparated,
    List.map(stringOfStateChange, state.lastUpdate) |> commaSeparated)
}

let stringOfEvent = (event: event): string => {
  switch(event){
    | ClickNote(note) => "ClickNote(" ++ Note.nameOfNoteInCMajor(note) ++ ")"
    | ClickInterval(interval) => "ClickInterval(" ++ string_of_int(interval.steps) ++ ")"
    | KeyDownInterval(interval) => "KeyDownInterval(" ++ string_of_int(interval.steps) ++ ")"
    | KeyUpInterval(interval) => "KeyUpInterval(" ++ string_of_int(interval.steps) ++ ")"
    | RegisterListener(_) => "RegisterListener(" ++ "..." ++ ")"
    | UnregisterListener(_) => "UnregisterListener(" ++ "..." ++ ")"
  }
}

let triggerVoice = (voiceKey: voiceKey, trigger: trigger, state: state): state => {

  let (updatedVoice, otherVoices: list(voice)) = switch (Belt.List.partition(state.voices, voice => voice.key == voiceKey)) {
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
      let stateWithVoice = triggerVoice(Single(newCurrentNote), Click, state);
      {...stateWithVoice, 
        currentNote: newCurrentNote, 
        lastUpdate: [CurrentNoteChanged(state.currentNote), ...stateWithVoice.lastUpdate]}
    | ClickInterval(interval) =>
      let newCurrentNote: note = noteApplyInterval(state.currentNote, interval);
      let stateWithVoice = triggerVoice(Single(newCurrentNote), Click, state);
      {...stateWithVoice, 
        currentNote: newCurrentNote, 
        lastUpdate: [CurrentNoteChanged(state.currentNote), ...stateWithVoice.lastUpdate]}
    | KeyDownInterval(interval) =>
      let newCurrentNote: note = noteApplyInterval(state.currentNote, interval);
      let stateWithVoice = triggerVoice(Single(newCurrentNote), IntervalAttack(interval), state);
      {...stateWithVoice, 
        currentNote: newCurrentNote, 
        lastUpdate: [CurrentNoteChanged(state.currentNote), ...stateWithVoice.lastUpdate]}
    | KeyUpInterval(interval) =>
      let newCurrentNote: note = noteApplyInterval(state.currentNote, interval);
      // TODO: newCurrentNote is wrong...
      let stateWithVoice = triggerVoice(Single(newCurrentNote), IntervalRelease(interval), state);
      stateWithVoice
    | RegisterListener(listener) => {
        ...state,
        listeners: [listener, ...state.listeners],
      }
    | UnregisterListener(listener) => {
        ...state,
        listeners:
          Belt.List.keep(state.listeners, registered => registered !== listener),
      }
    };

  Belt.List.forEach(newState.lastUpdate, emit(state));

  Js.log(Printf.sprintf("%s -> %s on %s", stringOfState(prevState), stringOfState(newState), stringOfEvent(event)));

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
