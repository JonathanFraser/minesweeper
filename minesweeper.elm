import Html
import Dict
import List 
import Html.Events exposing(..)
import Json.Decode as Json
import Maybe
import Html.Attributes as Attrs
import Random
import Html.Lazy
import Time
import Task

type NewTable = RandomTable (List Pos)
type ClickType = MarkCmd | UncoverCmd | FlushCmd
type Covered = Marked | Unmarked | Uncovered
type GameState = Playing | Won | Lost

type alias Square = {covered: Covered, isMine: Bool, state: GameState}
type alias Pos = (Int,Int)
type alias GameTable = Dict.Dict Pos Square

outer : (a->b->c) -> List a -> List b -> List c
outer f xs ys =
  List.concatMap
    ( \x -> List.map ( \y -> f x y ) ys )
    xs

gridList (h,w) =
     outer (,) (List.range 0 (h-1)) (List.range 0 (w-1))

initTable (h,w) initVal = 
        gridList (h,w)
            |> List.foldl (\pos prev -> Dict.insert pos initVal prev) Dict.empty

operate : (Square -> Square) -> (Int, Int) -> GameTable -> GameTable 
operate f loc = Dict.update loc (\v -> 
    case v of 
        Nothing -> Nothing 
        Just a -> Just (f a))

addMine: Pos -> GameTable -> GameTable
addMine pos table = 
    operate (\s -> {s|isMine = True}) pos table 

getNeighbours : (Int,Int) -> List (Int,Int)
getNeighbours (r,c) = [(r-1,c-1),(r-1,c),(r-1,c+1),(r,c-1),(r,c+1),(r+1,c-1),(r+1,c),(r+1,c+1)] 

uncoverSquare : (Int,Int) -> GameTable -> GameTable
uncoverSquare pos table = 
    let 
        value = Dict.get pos table 
        uncovered = operate uncover pos table 
    in 
        case value of 
            Nothing -> table
            Just v -> 
                if v.covered == Marked then
                    table 
                else
                    if v.isMine || getUnFlaggedCount table pos /= 0 || v.covered == Uncovered then
                        uncovered
                    else
                        List.foldl uncoverSquare uncovered (getNeighbours pos)
                    

anyTable: (Square->Bool) -> GameTable -> Bool
anyTable f = Dict.foldl (\ _ v p -> p || f v) False 

allTable: (Square->Bool) -> GameTable -> Bool
allTable f = Dict.foldl (\ _ v p -> p && f v ) True 

hasHit : Square -> Bool
hasHit square = square.covered == Uncovered && square.isMine

isValidMark : Square -> Bool
isValidMark s = not (xor (s.covered == Marked) s.isMine)

isLost: GameTable -> Bool
isLost = anyTable hasHit

isWon : GameTable -> Bool
isWon table = (allTable isValidMark table) && not (isLost table)

toggleMark : Square -> Square
toggleMark v = case v.covered of 
        Marked -> {v | covered = Unmarked}
        Unmarked -> {v | covered = Marked}
        Uncovered -> v

uncover : Square -> Square
uncover v = case v.covered of 
    Marked -> v 
    Unmarked -> {v | covered = Uncovered}
    Uncovered -> v

getCount : GameTable -> Pos -> Int
getCount = getNeighbourCount .isMine

getFlagCount : GameTable -> Pos -> Int 
getFlagCount = getNeighbourCount (\s -> s.covered == Marked)

getUnFlaggedCount : GameTable -> Pos -> Int
getUnFlaggedCount = getNeighbourCount (\s -> s.isMine && (not (s.covered == Marked)))

getNeighbourCount : (Square -> Bool) -> GameTable -> Pos -> Int
getNeighbourCount f table pos = getNeighbours pos
                        |> List.map  (\x -> Dict.get x table)
                        |> List.filter (\x -> case x of 
                            Nothing -> False
                            Just v -> f v)
                        |> List.length

minesLeft : GameTable -> Int
minesLeft  table =  
    let
     mines = Dict.size (Dict.filter (\_ s -> s.isMine) table)
     flags = Dict.size (Dict.filter (\_ s -> s.covered == Marked) table) 
    in 
        if flags > mines then
            0
        else
            mines - flags


nullCell : Square
nullCell = {covered=Unmarked, isMine=False, state=Playing}

randomTable : Pos -> Int -> Random.Generator (List Pos)
randomTable (w,h) count = 
    Random.list count (Random.pair (Random.int 0 (w-1)) (Random.int 0 (h-1)))


makeInitModel size = 
    {loc=Nothing, table=initTable size nullCell, state=Playing, size=size, start = 0,duration=0}


type MouseStatus = Entered Pos | Left Pos
type Msg  = MouseMove MouseStatus | Click ClickType | NewTable (List Pos) | Tick Time.Time | ResetTimer Time.Time | Reset

onRightClick : a -> Html.Attribute a
onRightClick message =
  onWithOptions
    "contextmenu"
    { stopPropagation = True
    , preventDefault = True
    }
    (Json.succeed message)

update msg model = 
         case msg of 
            Reset ->
                ({model | state = Playing},Random.generate NewTable (randomTable (initModel.size) 8))
            NewTable mines ->
                ({model | table = List.foldl addMine (initTable model.size nullCell) mines},Task.perform ResetTimer (Time.now))
            Tick  time ->
                ({model | duration = time - model.start},Cmd.none)
            ResetTimer time -> 
                ({model | start = time },Cmd.none)
            MouseMove move -> case move of 
                Entered a ->
                    ({model | loc=Just a}, Cmd.none)
                Left a -> 
                    ({model | loc=Nothing},Cmd.none)
            Click clicktype -> case model.loc of 
                Nothing -> (model,Cmd.none)
                Just pos -> 
                let 
                    new = case clicktype of 
                        MarkCmd -> {model | table = operate toggleMark pos model.table}
                        UncoverCmd -> {model |  table = uncoverSquare pos model.table}
                        FlushCmd -> if (getFlagCount model.table pos) == (getCount model.table pos) then
                            {model | table = List.foldl uncoverSquare model.table (getNeighbours pos)}
                        else
                            model
                    latch = if model.state /= Playing then
                                model
                            else
                                if isLost new.table then
                                    {new | state = Lost}
                                else 
                                    if isWon new.table then
                                        {new | state = Won}
                                    else 
                                        new
                in 
                (latch,Cmd.none)

messages point = 
    [onDoubleClick (Click FlushCmd),onClick (Click UncoverCmd), onRightClick (Click MarkCmd), onMouseEnter (MouseMove (Entered point)),onMouseLeave (MouseMove (Left point))]

getDiv pos style text =
    Html.div ((Attrs.style (style++[("width","20px"),("height","20px")]))::(messages pos)) [Html.text text]

type Sprite = Covered | IncorrectlyFlagged |CorrectlyFlagged | Empty Int | ExplodedMine | RevealedMine | Error 

toSprite mode s count =
    case s.covered of 
        Uncovered -> 
            if s.isMine then
                ExplodedMine
            else 
                Empty count
        Marked ->
            if mode == Playing then
                CorrectlyFlagged
            else 
                if s.isMine then
                    CorrectlyFlagged
                else 
                    IncorrectlyFlagged
        Unmarked ->
            if mode == Playing then
                Covered
            else
                if s.isMine then
                    RevealedMine
                else
                    if mode == Won then
                        Empty count
                    else
                        Covered
colors = 
    ["blue","blueViolet","brown","burlyWood","cadetBlue","chartreuse","chocolate","coral"] 

getColor count = 
    case (List.head (List.drop count colors)) of 
        Nothing -> "black"
        Just col -> col



mine = "\x1F4A3"
flag = "\x1F6A9"
error = "\x203C"
empty = " "
background = ("background-color","grey")
tagged = ("background-color","red")

spriteProps sprite = 
    case sprite of 
        Covered ->
            ([background],empty)
        CorrectlyFlagged ->
            ([background],flag)
        IncorrectlyFlagged ->
            ([tagged],flag)
        Empty count -> 
            if count == 0 then
                ([],empty)
            else
                ([("color",getColor (count-1))],toString count)
        ExplodedMine ->
            ([tagged],mine)
        RevealedMine ->
            ([background],mine)
        Error ->
            ([],error)

makeSpriteTable state table =
    Dict.map (\pos s -> toSprite state s (getCount table pos)) table

getSprite table pos = 
    case Dict.get pos table of 
        Nothing -> Error
        Just a -> a

tableElement table pos =
    let 
        (attrs, str) = spriteProps (getSprite table pos)
    in
        getDiv pos attrs str

renderRow y f w = 
    List.map (\x -> Html.td [Attrs.style [("text-align","center"),("border","1px solid black")]] [f (y,x)]) (List.range 0 (w-1))

renderTable f (h,w) = 
    Html.table [Attrs.style [("border-collapse","collapse")]] (List.map (\y -> Html.tr [] (renderRow y f w)) (List.range 0 (h-1)))

renderMineCount table = 
    Html.div [Attrs.style [("text-align","left")]] [Html.text (toString (minesLeft table))]

thinking = "\x1F914"
sunglasses = "\x1F60E"
sick = "\x1F915"
renderRestart state =
    Html.div [Attrs.style [("text-align","center"),("border","1px solid black")], onClick Reset] [
        case state of 
        Playing ->
            Html.text thinking
        Won ->
            Html.text sunglasses
        Lost ->
            Html.text sick]

renderTimer time =
   Html.div [Attrs.style [("text-align","right")]] [Html.text (toString (floor (Time.inSeconds time)))]

subscriptions model =
    if model.state == Playing then
        Time.every Time.second Tick
    else
        Sub.none

initModel = makeInitModel (9,9)
main = Html.program {init=(initModel,Random.generate NewTable (randomTable (initModel.size) 8) ),subscriptions = subscriptions, view = Html.Lazy.lazy view, update = (\ msg model -> update msg model)}

view model = Html.table [Attrs.style [("background-color","lightgrey")]] [ Html.tr [] [ Html.th [Attrs.style [("width","40px")]] [renderMineCount model.table]
                                         ,Html.th [] [renderRestart model.state]
                                         ,Html.th [Attrs.style [("width","40px")]] [renderTimer model.duration]]
                            ,Html.tr [] [Html.td [Attrs.colspan 3] [renderTable (tableElement (makeSpriteTable model.state model.table)) model.size]]]