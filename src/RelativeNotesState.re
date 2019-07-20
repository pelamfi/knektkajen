open Note;
//open Belt;

// Determines which voices can trigger again without stopping previous voices
type voiceKey =   
  | Single(note)


type externalTriggerId =
  | MouseClick
  | Keyboard(string)

type triggerId =
  | Idle
  | DirectNote(externalTriggerId)
  | ChordInterval(interval, externalTriggerId)
  

type trigger =
  | IntervalAttack(interval, externalTriggerId)
  | Release(externalTriggerId)
  | NoteClick(note, externalTriggerId)
  | ChordPrime(interval, externalTriggerId)
  | ChordRelease(externalTriggerId)
  | IntervalClick(interval, externalTriggerId);

type voiceState =
  | Idle
  | AttackRelease
  | Attack
  | Release;

type voice = {key: voiceKey, updateIndex: int, triggerId, state: voiceState, prevState: voiceState, allocated: int};

type chordInterval  = {interval: interval, externalTriggerId: externalTriggerId}

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

let stringOfExternalTriggerId = (externalTriggerId: externalTriggerId): string => {
  switch(externalTriggerId) {
    | MouseClick => "MouseClick"
    | Keyboard(id) => "Keyboard(" ++ id ++")"
  }
}

let stringOfTriggerId = (triggerId: triggerId): string => {
  switch(triggerId) {
    | Idle => "NotTriggered"
    | DirectNote(externalId) => "DirectNote(" ++ stringOfExternalTriggerId(externalId) ++ ")"
    | ChordInterval(interval, externalId) => "ChordInterval(" ++ string_of_int(interval.steps) ++ ", " ++ stringOfExternalTriggerId(externalId) ++ ")"
  }
}

let stringOfTrigger = (trigger: trigger): string => {
  switch(trigger) {
    | IntervalAttack(interval, triggerId) => "IntervalAttack(" ++ string_of_int(interval.steps) ++ ", " ++ stringOfExternalTriggerId(triggerId) ++ ")"
    | Release(triggerId) => "IntervalRelease(" ++ stringOfExternalTriggerId(triggerId) ++ ")"
    | NoteClick(note, triggerId) => "NoteClick(" ++ Note.nameOfNoteInCMajor(note) ++ ", " ++ stringOfExternalTriggerId(triggerId) ++ ")"
    | IntervalClick(interval, triggerId) => "IntervalClick(" ++ string_of_int(interval.steps) ++ ", " ++ stringOfExternalTriggerId(triggerId) ++ ")"
    | ChordPrime(interval, triggerId) => "ChordPrime(" ++ string_of_int(interval.steps) ++ ", " ++ stringOfExternalTriggerId(triggerId) ++ ")"
    | ChordRelease(triggerId) => "ChordRelease(" ++ stringOfExternalTriggerId(triggerId) ++ ")"
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

let stringOfChordInterval = (chordInterval: chordInterval): string => {
  Printf.sprintf("[%d, %s]", chordInterval.interval.steps, stringOfExternalTriggerId(chordInterval.externalTriggerId))
}

let stringOfState = (state: state): string => {
  Printf.sprintf("[currentNote: %s, updateIndex:%d, voices: [%s], lastUpdate: [%s], chordIntervals: [%s]", 
    Note.nameOfNoteInCMajor(state.currentNote), state.updateIndex,
    List.map(stringOfVoice, state.voices) |> StringUtil.commaSeparated,
    List.map(stringOfStateChange, state.lastUpdate) |> StringUtil.commaSeparated,
    List.map(stringOfChordInterval, state.chordIntervals) |> StringUtil.commaSeparated)
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
  let (voicesToUpdate, otherVoices) = Belt.List.partition(state.voices, voice => {voice.triggerId == triggerId});

  let updatedVoices = Belt.List.map(voicesToUpdate, voice => {
    {...voice, updateIndex: state.updateIndex, triggerId: Idle, state: Release, prevState: voice.state}
    }
  );
  
  let voiceUpdates = Belt.List.map(updatedVoices, voice => {Voice(voice)})

  let voices = Belt.List.concat(otherVoices, updatedVoices); // put ended voices at end of list so they get reused first

  {...state, 
    voices,
    lastUpdate: Belt.List.concat(voiceUpdates, state.lastUpdate)
  }
}

let handleSingleShotNote = (state: state, note: note, externalTriggerId: externalTriggerId): state => {
  switch (triggerClickVoice(state, note, DirectNote(externalTriggerId))) {
    | (Some(updatedVoice), otherVoices) =>
      {...state, 
        currentNote: note, 
        voices: [updatedVoice, ...otherVoices],
        lastUpdate: [CurrentNoteChanged(note), Voice(updatedVoice)]}
    | (_, _) => state
  }
}

let attackChordItervalVoices = (externalTriggerId: externalTriggerId, note: note, state: state): state => {
  Belt.List.reduce(state.chordIntervals, state, (state, chordInterval: chordInterval): state => {
    let chordNote = Note.noteApplyInterval(note, chordInterval.interval)
    triggerNote(chordNote, ChordInterval(chordInterval.interval, externalTriggerId), state)
  })
}

let releaseChordIntervalVoices = (externalTriggerId: externalTriggerId, state: state): state => {
  Belt.List.reduce(state.chordIntervals, state, (state, chordInterval: chordInterval): state => {
    releaseNote(ChordInterval(chordInterval.interval, externalTriggerId), state)
  })
}

let noteOfVoice = (voice: voice): note => {
  switch(voice.key) {
    | Single(note) =>  note
  }
}

let lastTriggeredReduce = (prev: option(voice), voice: voice): option(voice) => {
  switch(voice.state, voice.triggerId) {
    | (_, ChordInterval(_)) => prev
    | (Attack, _) | (AttackRelease, _) => 
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

let lastExternallyTriggered = (voices: list(voice)): option((voice, externalTriggerId)) => {
  let voice = Belt.List.reduce(voices, None, lastTriggeredReduce);
  switch(voice) {
    | Some({triggerId: DirectNote(externalTriggerId)} as v) =>
      Some((v, externalTriggerId))
    | _ => None
  }
}

let isVoicePlaying = (voice: voice): bool => {
  switch(voice.state) {
    | Attack => true // TODO: Track whether single shots should be playing or not... or manage them with own timers
    | _ => false
  }
}

let releaseRemovedChordIntervals = (state: state): state => {
  Belt.List.reduce(state.voices, state, (state, voice: voice): state => {
    switch(voice.triggerId, voice.state) {
    | (ChordInterval(interval, _), Attack) =>
      if (Belt.List.every(state.chordIntervals, (chordInterval) => {chordInterval.interval != interval})) {
        releaseNote(voice.triggerId, state)
      } else {
        state
      }
    | _ => state
    }
  })
}

let attackAddedChordIntervals = (state: state): state => {
  switch(lastExternallyTriggered(state.voices)) {
    | Some((rootVoice, externalTriggerId)) =>
      let rootNote = noteOfVoice(rootVoice)
      Belt.List.reduce(state.chordIntervals, state, (state: state, chordInterval: chordInterval): state => {
        let triggerId = ChordInterval(chordInterval.interval, externalTriggerId)
        let note = Note.noteApplyInterval(rootNote, chordInterval.interval)
        switch(Belt.List.getBy(state.voices, (voice: voice): bool => {
          voice.triggerId == triggerId
        })) {
          | Some(_) => state // already playing
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

let updateState = (prevState: state, event: event): state => {
  let state = {...prevState, updateIndex: prevState.updateIndex + 1, lastUpdate: []};

  let newState: state =
    switch (event) {
    | NoteTrigger(NoteClick(note, extTriggerId)) => handleSingleShotNote(state, note, extTriggerId)
    | NoteTrigger(IntervalClick(interval, triggerId)) =>
      let note  = Note.noteApplyInterval(state.currentNote, interval)
      handleSingleShotNote(state, note, triggerId)
    | NoteTrigger(IntervalAttack(interval, extTriggerId)) =>
      let note = Note.noteApplyInterval(state.currentNote, interval);
      state |> triggerNote(note, DirectNote(extTriggerId)) |> changeCurrentNote(_, note) |> attackChordItervalVoices(extTriggerId, note)
    | NoteTrigger(Release(extTriggerId)) =>
      state |> releaseNote(DirectNote(extTriggerId)) |> releaseChordIntervalVoices(extTriggerId)
    | NoteTrigger(ChordPrime(interval, externalTriggerId)) =>
      let stateWithChordIntervalAdded = {
        ...state,
        chordIntervals: [{interval, externalTriggerId}, ...state.chordIntervals]
      }

      attackAddedChordIntervals(stateWithChordIntervalAdded)

    | NoteTrigger(ChordRelease(extTriggerId)) =>
      let stateWithChordIntervalRemoved = {
        ...state,
        chordIntervals: Belt.List.keep(state.chordIntervals, chordInterval => {chordInterval.externalTriggerId != extTriggerId})
      }

      releaseRemovedChordIntervals(stateWithChordIntervalRemoved)
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
