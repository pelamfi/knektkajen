// Link to this somewhere https://www.youtube.com/watch?v=mdEcLQ_RQPY&pbjreload=10
// get ideas from

// scale patterns?
// At least same kind of visualization as in video

type questionType =
  | NoteNameStepsFromAUp
  | NoteNameStepsFromNotAUp
  | NoteNameStepsFromADown
  | NoteNameStepsFromNotADown
  | IntervalName
  | NoteASmallNUp
  | NoteASmallNDown
  | NoteNameSmallNUp
  | NoteNameSmallNDown
  | BarsMajScaleName
  | BarsMinScaleName
  | BarsCMajNoteNFromA
  | BarsMajNoteNFromA
  | BarsCMajNoteName
  | BarsMajNoteName;

type knektkajenState =
  | Welcome
  | ContinueGame // continue at last best level
  | StartGame // start at easiest level
  | StartRound
  | Question
  | WrongAnswer
  | CorrectAnswer
  | EndRound
  | EndGame
  | HighScore; // local storage of highest levels reached

type questionMode =
  | LowerLevelConfirmation // ask something that should be easy from lower leve
  | LowerLevelToughOne // Ask something from lower level that had wrong answers last time
  | ActiveLevelToughOne
  | ActiveLevelRandom;

type levelProgress =
  | Begin
  | LevelUp
  | LevelDown
  | FastUp; // you can get back to previous highest level by answering easier level questions correctly successively