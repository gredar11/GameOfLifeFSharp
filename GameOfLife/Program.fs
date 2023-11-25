module GameOfLife.Program
// #r "nuget:DIKU.Canvas, 2.0.1"
open Canvas
open GameOfLife.GameOfLife
open Color

let w, h = (800, 800)

type State = LiveStatus array2d

let defineRectColorByCellLiveStatus cellStatus =
    match cellStatus with
    | LiveStatus.Alive -> green
    | LiveStatus.Dead -> black
    | c -> failwith $"No color for such status {c.ToString()}"

let draw (state: State) =
    let seqOfRect = seq {
        for i = 0 to state.GetUpperBound(0) do
            for j = 0 to state.GetUpperBound(1) do
                let cell = state[i, j]
                let k = 10
                let x = i * k
                let y = j * k
                let cellColor = cell |> defineRectColorByCellLiveStatus
                let rect = filledRectangle cellColor k k |> translate x y
                rect
    }
    let tree =  seqOfRect |> Seq.reduce onto
    tree |> make

let react (state: State) (ev: Event) =
    match ev with
    |Event.TimerTick -> Some (getNextGenTable state)
    | _ -> None

let initialState : State = Array2D.init 50 50 (fun _ _ -> createRandomCell() )

let interval = Some 50

// render "Game of life demo" w h cellsPicture

interact "Game of life" w h interval draw react initialState
