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
  RowList: int List
  RowAction: int -> int -> string
  RowTimeType: TimeValue
}

let modulo x y = y % x

let onOrOff trueValue falseValue measure elem = 
  match (>=) measure elem with
  | true -> trueValue 
  | false -> falseValue

let clockRows = [
  {
    RowList = [1]; 
    RowAction = modulo 2 >> onOrOff "O" "Y"; 
    RowTimeType = Second
  };
  {
    RowList = [5;10;15;20]; 
    RowAction = onOrOff "R" "O"; 
    RowTimeType = Hour
  };
  {
    RowList = [1;2;3;4]; 
    RowAction = modulo 5 >> onOrOff "R" "O"; 
    RowTimeType = Hour
  };
  {
    RowList = [5;10;15;20;25;30;35;40;45;50;55]; 
    RowAction = onOrOff "Y" "O"; 
    RowTimeType = Minute
  };
  {
    RowList = [1;2;3;4]; 
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
    elem.RowTimeType |> extractTimeValue inputTime |> elem.RowAction |> rowLights elem.RowList
  clockRows |> List.map mapAction

let arrayToTime (timeArray: string[]) = {
  Hour = int timeArray.[0]; 
  Minute = int timeArray.[1]; 
  Second = int timeArray.[2]
  }

let timeStringToArray (timeStr: string) = timeStr.Split ':'
let timeStringToTime = timeStringToArray >> arrayToTime

let allRows inputTime = inputTime |> timeStringToTime |> generateClockRows

let timeString = "21:57:21"
let result = timeString |> allRows