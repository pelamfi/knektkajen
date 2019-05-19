open Note
type state = {currentNote: note}

type event = ClickNote(note)

type acceptEvent = event => unit

let initialState: state = {currentNote: middleC}

let updateState = (event: event, current: state): state => {
    switch event {
        | ClickNote(newCurrentNote) =>  {currentNote: newCurrentNote}
    }
}