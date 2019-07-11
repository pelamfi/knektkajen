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

let idleVoice = (voiceNumber: int): voice => {
   {key: Single(Note.middleC), updateIndex: 0, trigger: NoteClick(Note.middleC), state: Idle, prevState: Idle, allocated: voiceNumber}
}

let voices = 6

let initialVoices = RangeOfInt.make(0, voices) |> RangeOfInt.map(_, idleVoice)

let initialState: state = {currentNote: middleC, updateIndex: 0, voices: initialVoices, listeners: [], lastUpdate: []};

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

let matchingVoiceOrLRU = (pred: ((voice) => bool), state: state): (option(voice), list(voice)) => {
  switch (Belt.List.partition(state.voices, pred)) {
    | ([], others) =>
      // no match, take last to get LRU scheme
      switch(Belt.List.reverse(others)) {
        | [last, ...tail] =>
          (Some(last), Belt.List.reverse(tail))
        | [] =>
          Js.log("sameKeyVoice: no voices?");
          (None, [])
      }
    | ([matching], others) =>
    (Some(matching), others)  
    | ([firstMatching, ...moreThan1Matching], others) =>
    Js.log("sameKeyVoice: more than 1 matching?");
      (Some(firstMatching), Belt.List.concat(moreThan1Matching, others))
  }
}
  
let isVoiceActive = (voice: voice): bool => {
  switch(voice.state) {
    | Release => false
    | AttackRelease => false // TODO: Not correct really. Should track duration
    | _ => true
  }
}

let triggerClickVoice = (state: state, note: note): (option(voice), list(voice))  => {
  let trigger = NoteClick(note);
  switch (matchingVoiceOrLRU((voice) => {voice.key == Single(note)}, state)) {
    | (Some(previous), others) => // should only be 1 matching
    (Some({...previous, key: Single(note), updateIndex: state.updateIndex, trigger, state: AttackRelease, prevState: previous.state}), others)
    | (None, others) =>
    Js.log("triggerClickVoice: no voices?");
    (None, state.voices)
  }
}

let triggerIntervalKeyDownVoice = (state: state, interval: interval): (option(voice), list(voice)) => {
  let trigger = IntervalAttack(interval);
  let note = Note.noteApplyInterval(state.currentNote, interval);
  switch (matchingVoiceOrLRU((voice) => {voice.key == Single(note)}, state)) {
    | (Some(previous), others) => // should only be 1 matching
    (Some({...previous, key: Single(note), updateIndex: state.updateIndex, trigger, state: Attack, prevState: previous.state}), others)
    | (None, others) =>
    Js.log("triggerClickVoice: no voices?");
    (None, state.voices)
  }
}

let triggerIntervalKeyUpVoice = (state: state, interval: interval): (option(voice), list(voice)) => {
  let trigger = IntervalRelease(interval)
  switch (Belt.List.partition(state.voices, voice => {voice.trigger == IntervalAttack(interval)})) {
    | ([previous], others) =>
      (Some({...previous, updateIndex: state.updateIndex, trigger, state: Release, prevState: previous.state}), others)
    | (_, _) =>
      Js.log(Printf.sprintf("triggerIntervalKeyUpVoice: Unexpected voice trigger: trigger:%s, state: %s",
      stringOfTrigger(trigger), stringOfState(state)));
      (None, state.voices)
  }
}

let updateState = (prevState: state, event: event): state => {
  let state = {...prevState, updateIndex: prevState.updateIndex + 1, lastUpdate: []};

  let newState: state =
    switch (event) {
    | ClickNote(newCurrentNote) =>
      switch (triggerClickVoice(state, newCurrentNote)) {
        | (Some(updatedVoice), otherVoices) =>
          {...state, 
            currentNote: newCurrentNote, 
            voices: [updatedVoice, ...otherVoices],
            lastUpdate: [CurrentNoteChanged(newCurrentNote), Voice(updatedVoice)]}
        | (_, _) => state
      }
    | ClickInterval(interval) =>
      let newCurrentNote = Note.noteApplyInterval(state.currentNote, interval)
      switch (triggerClickVoice(state, newCurrentNote)) { // DRY!
        | (Some(updatedVoice), otherVoices) =>
          {...state, 
            currentNote: newCurrentNote, 
            voices: [updatedVoice, ...otherVoices],
            lastUpdate: [CurrentNoteChanged(newCurrentNote), Voice(updatedVoice)]}
        | (_, _) => state
      }
    | KeyDownInterval(interval) =>
      switch(triggerIntervalKeyDownVoice(state, interval)) {
        | (Some(updatedVoice), otherVoices) =>
          {...state,
            voices: [updatedVoice, ...otherVoices],
            lastUpdate: [Voice(updatedVoice)]}
        | (_, _) => state
      }
    | KeyUpInterval(interval) =>
      switch(triggerIntervalKeyUpVoice(state, interval)) {
        | (Some(updatedVoice), otherVoices) =>
          switch(updatedVoice.key) {
            | Single(newCurrentNote) =>
            {...state, 
            currentNote: newCurrentNote, 
            voices: [updatedVoice, ...otherVoices],
            lastUpdate: [CurrentNoteChanged(newCurrentNote), Voice(updatedVoice)]}
            | _ =>
            state
          }
        | (_, _) =>
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
