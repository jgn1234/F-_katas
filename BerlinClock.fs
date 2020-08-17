// module BerlinClock

type Hour = int
type Minute = int
type Second = int

type TimeValue = Hour | Minute | Second

type Time = {
  Hour: Hour
  Minute: Minute
  Second: Second
}

type ClockRowGenerator = {
  RowFields: int List   
  RowAction: int -> int -> string
  RowTimeType: TimeValue
}

let modulo x y = y % x

let onOrOff trueValue falseValue measure elem = 
  match (>=) measure elem with
  | true -> trueValue 
  | false -> falseValue

// each element of the clockRows list represents one row on the clock
// the RowFields list represents the fields on a given row of the clock
// the RowAction defines the action that should be taken to generate each field on the row
// the RowTimeType defines the part of the 24-hour time that is used to calculate the fields on the row
//    using the first row as an example: the seconds of the current time are passed to modulo 2, 
//    the result of which is passed to the onOrOff calcuation and compared against the RowField list element for the current field.  
//    This result is used to determine whether the first or second value is returned to represent the field color
// This data structure can easily be extended to add new rows with unique attributes to represent time
// (e.g., another row where each field represents 10 seconds)
let clockRows = [
  {
    RowFields = [1]; 
    RowAction = modulo 2 >> onOrOff "O" "Y"; 
    RowTimeType = Second
  };
  {
    RowFields = [5;10;15;20]; 
    RowAction = onOrOff "R" "O"; 
    RowTimeType = Hour
  };
  {
    RowFields = [1;2;3;4]; 
    RowAction = modulo 5 >> onOrOff "R" "O"; 
    RowTimeType = Hour
  };
  {
    RowFields = [5;10;15;20;25;30;35;40;45;50;55]; 
    RowAction = onOrOff "Y" "O"; 
    RowTimeType = Minute
  };
  {
    RowFields = [1;2;3;4]; 
    RowAction = modulo 5 >> onOrOff "Y" "O"; 
    RowTimeType = Minute
  };
]

let rowLights list action = 
  let foldAction lightTracker elem = lightTracker + action elem
  list |> List.fold foldAction ""

let extractTimeValue inputTime timeValue = 
    match timeValue with
    | Second -> inputTime.Second
    | Minute -> inputTime.Minute
    | Hour ->   inputTime.Hour

let generateClockRows inputTime = 
  let mapAction (elem: ClockRowGenerator) = 
    elem.RowTimeType |> extractTimeValue inputTime |> elem.RowAction |> rowLights elem.RowFields
  clockRows |> List.map mapAction

let arrayToTime (timeArray: string[]) = {
  Hour = int timeArray.[0]; 
  Minute = int timeArray.[1]; 
  Second = int timeArray.[2]
  }

let timeStringToArray (timeStr: string) = timeStr.Split ':'
let timeStringToTime = timeStringToArray >> arrayToTime

let generateAllClockRows = timeStringToTime >> generateClockRows

let timeString = "21:57:21"
let result = timeString |> generateAllClockRows