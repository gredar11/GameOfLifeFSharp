module GameOfLife.GameOfLife

open System

type LiveStatus =
    | Alive = 0
    | Dead = 1

type Point = { X: int; Y: int }

type Cell =
    { LiveStatus: LiveStatus
      Coordinate: Point }

let random = Random()

let pointsToFindNeighbours =
    seq {
        (1, 0)
        (1, 1)
        (0, 1)
        (-1, 1)
        (-1, 0)
        (-1, -1)
        (0, -1)
        (1, -1)
    }

let createRandomCell () =
    let isAlive = random.Next(0, 2)
    let status = enum<LiveStatus> isAlive
    status

let getNextGenCell cellStatus neighbours =
    let numberOfLiveNeighbours =
        neighbours
        |> Array.where (fun arrayCell -> arrayCell = LiveStatus.Alive)
        |> Array.length

    match cellStatus with
    | LiveStatus.Alive ->
        match numberOfLiveNeighbours with
        | 2
        | 3 -> LiveStatus.Alive
        | _ -> LiveStatus.Dead
    | LiveStatus.Dead ->
        match numberOfLiveNeighbours with
        | 3 -> LiveStatus.Alive
        | _ -> LiveStatus.Dead
    | _ -> ArgumentOutOfRangeException() |> raise

let findNeighbours (x, y) (array2D: LiveStatus array2d) =
    let w = array2D.GetLength 0
    let h = array2D.GetLength 1

    let neighbours =
        pointsToFindNeighbours
        |> Seq.map (fun point -> { X = x - fst point; Y = y - snd point })
        |> Seq.where (fun point ->
            point.X >= 0
            && point.Y >= 0
            && point.X < w
            && point.Y < h)
        |> Seq.map (fun point ->
            let neighbour = array2D[point.X, point.Y]
            neighbour)
        |> Array.ofSeq

    neighbours

let getNextGenTable cellTable : LiveStatus array2d =
    let initFunction x y =
        let neighbours = findNeighbours (x, y) cellTable
        let cell = cellTable[x, y]
        let cellLiveStatus = getNextGenCell cell neighbours
        cellLiveStatus

    let nextGenTable =
        Array2D.init (cellTable.GetLength(0)) (cellTable.GetLength(1)) initFunction

    nextGenTable
