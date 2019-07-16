open Note;
//open Belt;

// Determines which voices can trigger again without stopping previous voices
type voiceKey =   
  | Single(note)

type triggerId =
  | Idle
  | MouseClick
  | Keyboard(string)
  | ChordInterval(interval)

type trigger =
  | IntervalAttack(interval, triggerId)
  | Release(triggerId)
  | NoteClick(note, triggerId)
  | ChordPrime(interval, triggerId)
  | ChordRelease(triggerId)
  | IntervalClick(interval, triggerId);

type voiceState =
  | Idle
  | AttackRelease
  | Attack
  | Release;

type voice = {key: voiceKey, updateIndex: int, triggerId, state: voiceState, prevState: voiceState, allocated: int};

type chordInterval  = {interval: interval, triggerId: triggerId}

type stateChange =
  | CurrentNoteChanged(note)
  | Voice(voice);

type listener = stateChange => unit;

type event =
  | NoteTrigger(trigger)
  | RegisterListener(listener)
  | UnregisterListener(listener);

type state = {
  currentNote: note,
  chordIntervals: list(chordInterval),
  updateIndex: int,
  voices: list(voice),
  listeners: list(stateChange => unit),
  lastUpdate: list(stateChange)
};

type acceptEvent = event => unit;

let minOctave: octave = {number: 1} // A 55 hz
let maxOctave: octave = {number: 8}
let minNote: note = noteOfOctaveAndChromaticNote(minOctave, C)
let maxNote: note = noteOfOctaveAndChromaticNote(maxOctave, B)
let numberOfNotes: int = (maxNote.offset - minNote.offset) + 1

let loopOctaves = (n: note): note => {
  let loopedOffset = MathUtil.flooredDivisionRemainder(n.offset - minNote.offset, numberOfNotes) + minNote.offset;
  // Js.log(Printf.sprintf("n.offset:%d minNote.offset:%d numberOfNotes:%d loopedOffset: %d", n.offset, minNote.offset, numberOfNotes, loopedOffset));
  {offset: loopedOffset}
}

let idleVoice = (voiceNumber: int): voice => {
   {key: Single(Note.middleC), updateIndex: 0, triggerId: Idle, state: Idle, prevState: Idle, allocated: voiceNumber}
}

let voices = 6

let initialVoices = RangeOfInt.make(0, voices) |> RangeOfInt.map(_, idleVoice)

let initialState: state = {currentNote: middleC, chordIntervals: [], updateIndex: 0, voices: initialVoices, listeners: [], lastUpdate: []};

let emit = (state: state, stateChange: stateChange) => {
  Belt.List.forEach(state.listeners, listener => listener(stateChange));
};

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

let stringOfTriggerId = (triggerId: triggerId): string => {
  switch(triggerId) {
    | Idle => "NotTriggered"
    | MouseClick => "MouseClick"
    | Keyboard(id) => "Keyboard(" ++ id ++")"
    | ChordInterval(interval) => "ChordInterval(" ++ string_of_int(interval.steps) ++")"
  }
}

let stringOfTrigger = (trigger: trigger): string => {
  switch(trigger) {
    | IntervalAttack(interval, triggerId) => "IntervalAttack(" ++ string_of_int(interval.steps) ++ ", " ++ stringOfTriggerId(triggerId) ++ ")"
    | Release(triggerId) => "IntervalRelease(" ++ stringOfTriggerId(triggerId) ++ ")"
    | NoteClick(note, triggerId) => "NoteClick(" ++ Note.nameOfNoteInCMajor(note) ++ ", " ++ stringOfTriggerId(triggerId) ++ ")"
    | IntervalClick(interval, triggerId) => "IntervalClick(" ++ string_of_int(interval.steps) ++ ", " ++ stringOfTriggerId(triggerId) ++ ")"
    | ChordPrime(interval, triggerId) => "ChordPrime(" ++ string_of_int(interval.steps) ++ ", " ++ stringOfTriggerId(triggerId) ++ ")"
    | ChordRelease(triggerId) => "ChordRelease(" ++ stringOfTriggerId(triggerId) ++ ")"
  }
}

let stringOfVoice = (voice: voice): string => {
  Printf.sprintf("[%s, state: %s, triggerId: %s]", stringOfVoiceKey(voice.key), stringOfVoiceState(voice.state), stringOfTriggerId(voice.triggerId))
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
    List.map(stringOfVoice, state.voices) |> StringUtil.commaSeparated,
    List.map(stringOfStateChange, state.lastUpdate) |> StringUtil.commaSeparated)
}

let stringOfEvent = (event: event): string => {
  switch(event){
    | NoteTrigger(trigger) => "NoteTrigger(" ++ stringOfTrigger(trigger) ++ ")"
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

let triggerClickVoice = (state: state, note: note, triggerId: triggerId): (option(voice), list(voice))  => {
  switch (matchingVoiceOrLRU((voice) => {voice.key == Single(note)}, state)) {
    | (Some(previous), others) => // should only be 1 matching
    (Some({...previous, key: Single(note), updateIndex: state.updateIndex, triggerId, state: AttackRelease, prevState: previous.state}), others)
    | (None, _) =>
    Js.log("triggerClickVoice: no voices?");
    (None, state.voices)
  }
}

let triggerNote = (note: note, triggerId: triggerId, state: state): state => {
  switch (matchingVoiceOrLRU((voice) => {voice.key == Single(note)}, state)) {
    | (Some(previous), others) => // should only be 1 matching
    let newVoice: voice = {...previous, key: Single(note), updateIndex: state.updateIndex, triggerId, state: Attack, prevState: previous.state};
    {...state, voices: [newVoice, ...others], lastUpdate: [Voice(newVoice), ...state.lastUpdate]}
    | (None, _) =>
    Js.log("triggerNote: no voices available?");
    state
  }
}

let releaseNote = (triggerId: triggerId, state: state): state => {
  switch (Belt.List.partition(state.voices, voice => {voice.triggerId == triggerId})) {
    | ([previous], others) =>
      let updatedVoice: voice = {...previous, updateIndex: state.updateIndex, triggerId: Idle, state: Release, prevState: previous.state};
      {...state, 
        voices: [updatedVoice, ...others],
        lastUpdate: [Voice(updatedVoice), ...state.lastUpdate]
      }
    | (_, _) =>
      Js.log(Printf.sprintf("releaseNote: Unexpected voice trigger: state: %s", stringOfState(state)));
      state
  }
}

let handleSingleShotNote = (state: state, note: note, triggerId: triggerId): state => {
  switch (triggerClickVoice(state, note, triggerId)) {
    | (Some(updatedVoice), otherVoices) =>
      {...state, 
        currentNote: note, 
        voices: [updatedVoice, ...otherVoices],
        lastUpdate: [CurrentNoteChanged(note), Voice(updatedVoice)]}
    | (_, _) => state
  }
}

let attackChordItervalVoices = (state: state, note: note): state => {
  //List.onstate.voices state.chordIntervals
  state
}

let releaseChordItervalVoices = (state: state, note: note): state => {
  state
}

let noteOfVoice = (voice: voice): note => {
  switch(voice.key) {
    | Single(note) =>  note
  }
}

let lastTriggeredReduce = (prev: option(voice), voice: voice): option(voice) => {
  switch(voice.state) {
    | Attack | AttackRelease => 
      switch(prev) {
        | Some(prev) =>
          if (prev.updateIndex > voice.updateIndex) {
            Some(prev)
          } else {
            Some(voice)
          }
        | None => Some(voice)
      }
    | _ => prev
  }
}

let lastTriggered = (voices: list(voice)): option(voice) => {
  Belt.List.reduce(voices, None, lastTriggeredReduce)
}


let isVoicePlaying = (voice: voice): bool => {
  switch(voice.state) {
    | Attack => true // TODO: Track whether single shots should be playing or not... or manage them with own timers
    | _ => false
  }
}

let releaseRemovedChordIntervals = (state: state): state => {
  let voices = Belt.List.map(state.voices, (voice: voice): voice => {
    voice
  })
  state
}

let attackAddedChordIntervals = (state: state): state => {
  switch(lastTriggered(state.voices)) {
    | Some(rootVoice) =>
      let rootNote = noteOfVoice(rootVoice)
      Belt.List.reduce(state.chordIntervals, state, (state: state, chordInterval: chordInterval): state => {
        let triggerId = ChordInterval(chordInterval.interval)
        let note = Note.noteApplyInterval(rootNote, chordInterval.interval)
        switch(Belt.List.getBy(state.voices, (voice: voice): bool => {
          voice.triggerId == triggerId
        })) {
          | Some(voice) => state // already playing
          | None => triggerNote(note, triggerId, state)
        }
      })
    | None => state
  }
}

let changeCurrentNote = (state: state, note: note): state => {
  {...state,
   currentNote: note,
   lastUpdate: [CurrentNoteChanged(note), ...state.lastUpdate]}
}

let handleChordIntervalsChange = (state: state): state => {
  state 
  |> releaseRemovedChordIntervals 
  |> attackAddedChordIntervals
}

let updateState = (prevState: state, event: event): state => {
  let state = {...prevState, updateIndex: prevState.updateIndex + 1, lastUpdate: []};

  let newState: state =
    switch (event) {
    | NoteTrigger(NoteClick(note, triggerId)) => handleSingleShotNote(state, note, triggerId)
    | NoteTrigger(IntervalClick(interval, triggerId)) =>
      let note  = Note.noteApplyInterval(state.currentNote, interval)
      handleSingleShotNote(state, note, triggerId)
    | NoteTrigger(IntervalAttack(interval, triggerId)) =>
      let note = Note.noteApplyInterval(state.currentNote, interval);
      state |> triggerNote(note, triggerId) 
        |> changeCurrentNote(_, note)
    | NoteTrigger(Release(triggerId)) =>
      releaseNote(triggerId, state)
    | NoteTrigger(ChordPrime(interval, triggerId)) =>
      let stateWithChordIntervalAdded = {
        ...state,
        chordIntervals: [{interval, triggerId}, ...state.chordIntervals]
      }

      handleChordIntervalsChange(stateWithChordIntervalAdded)

    | NoteTrigger(ChordRelease(triggerId)) =>
      state
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
