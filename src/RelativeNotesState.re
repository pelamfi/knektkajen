open Note;
//open Belt;

// Determines which voices can trigger again without stopping previous voices
type voiceKey =   
  | Single(note)

type trigger =
  | IntervalAttack(interval)
  | IntervalRelease(interval)
  | NoteClick(note);

type voiceState =
  | Idle
  | AttackRelease
  | Attack
  | Release;

type voice = {key: voiceKey, updateIndex: int, trigger, state: voiceState, prevState: voiceState, allocated: int};

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

let stringOfTrigger = (trigger: trigger): string => {
  switch(trigger) {
    | IntervalAttack(interval) => "IntervalAttack(" ++ string_of_int(interval.steps) ++ ")"
    | IntervalRelease(interval) => "IntervalRelease(" ++ string_of_int(interval.steps) ++ ")"
    | NoteClick(note) => "NoteClick(" ++ Note.nameOfNoteInCMajor(note) ++ ")"
  }
}

let stringOfVoice = (voice: voice): string => {
  Printf.sprintf("[%s, state: %s, trigger: %s]", stringOfVoiceKey(voice.key), stringOfVoiceState(voice.state), stringOfTrigger(voice.trigger))
}

let stringOfStateChange = (stateChange: stateChange): string => {
  switch(stateChange) {
    | CurrentNoteChanged(note) => "CurrentNoteChanged(" ++ Note.nameOfNoteInCMajor(note) ++ ")"
    | Voice(voice) => "Voice(" ++ stringOfVoice(voice) ++ ")"
  }
}

let voices = 6

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

let sameKeyVoice = (voiceKey: voiceKey, state: state): (list(voice), list(voice)) => {Belt.List.partition(state.voices, voice => {voice.key == voiceKey})};
  
let isVoiceActive = (voice: voice): bool => {
  switch(voice.state) {
    | Release => false
    | AttackRelease => false // TODO: Not correct really. Should track duration
    | _ => true
  }
}

let triggerClickVoice = (state: state, note: note): (voice, list(voice))  => {
  let trigger = NoteClick(note)
  switch (sameKeyVoice(Single(note), state)) {
    | ([previous, ..._], others) => // should only be 1 matching
    ({key: previous.key /*Single(note)*/, updateIndex: state.updateIndex, trigger, state: AttackRelease, prevState: previous.state, allocated: 0}, others)
    | ([], others) =>
    ({key: Single(note), updateIndex: state.updateIndex, trigger, state: AttackRelease, prevState: Idle, allocated: 0}, others)
  }
}

let triggerIntervalKeyDownVoice = (state: state, interval: interval): (voice, list(voice)) => {
  let trigger = IntervalAttack(interval)
  let note = Note.noteApplyInterval(state.currentNote, interval);
  switch (sameKeyVoice(Single(note), state)) {
    | ([previous, ..._], others) => // should only be 1 matching
    ({key: previous.key /*Single(note)*/, updateIndex: state.updateIndex, trigger, state: Attack, prevState: previous.state, allocated: 0}, others)
    | ([], others) =>
    ({key: Single(note), updateIndex: state.updateIndex, trigger, state: Attack, prevState: Idle, allocated: 0}, others)
  }
}

let triggerIntervalKeyUpVoice = (state: state, interval: interval): (option(voice), list(voice)) => {
  let trigger = IntervalRelease(interval)
  switch (Belt.List.partition(state.voices, voice => {voice.trigger == IntervalAttack(interval)})) {
    | ([previous, ..._], others) =>
    (Some({key: previous.key /*Single(note)*/, updateIndex: state.updateIndex, trigger, state: Release, prevState: previous.state, allocated: 0}), others)
    | ([], others) =>
    Js.log(Printf.sprintf("Unexpected voice trigger: trigger:%s, state: %s",
      stringOfTrigger(trigger), stringOfState(state)));
    (None, state.voices)
  }
}

let updateState = (prevState: state, event: event): state => {
  let state = {...prevState, updateIndex: prevState.updateIndex + 1, lastUpdate: []};

  let newState: state =
    switch (event) {
    | ClickNote(newCurrentNote) =>
      let (updatedVoice, otherVoices) = triggerClickVoice(state, newCurrentNote);
      {...state, 
        currentNote: newCurrentNote, 
        voices: [updatedVoice, ...Belt.List.keep(otherVoices, isVoiceActive)],
        lastUpdate: [CurrentNoteChanged(newCurrentNote), Voice(updatedVoice)]}
    | ClickInterval(interval) =>
      let newCurrentNote = Note.noteApplyInterval(state.currentNote, interval)
      let (updatedVoice, otherVoices) = triggerClickVoice(state, newCurrentNote);
      {...state, 
        currentNote: newCurrentNote, 
        voices: [updatedVoice, ...Belt.List.keep(otherVoices, isVoiceActive)],
        lastUpdate: [CurrentNoteChanged(newCurrentNote), Voice(updatedVoice)]}
    | KeyDownInterval(interval) =>
      let (updatedVoice, otherVoices) = triggerIntervalKeyDownVoice(state, interval);
      {...state,
        voices: [updatedVoice, ...Belt.List.keep(otherVoices, isVoiceActive)],
        lastUpdate: [Voice(updatedVoice)]}
    | KeyUpInterval(interval) =>
      let (updatedVoice, otherVoices) = triggerIntervalKeyUpVoice(state, interval);
      switch(updatedVoice) {
        | Some(updatedVoice) =>
        switch(updatedVoice.key) {
          | Single(newCurrentNote) =>
          {...state, 
          currentNote: newCurrentNote, 
          voices: [updatedVoice, ...Belt.List.keep(otherVoices, isVoiceActive)],
          lastUpdate: [CurrentNoteChanged(newCurrentNote), Voice(updatedVoice)]}
        }
        | None =>
        state
      }
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

  Js.log(Printf.sprintf("%s on %s", stringOfState(newState), stringOfEvent(event)));

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
