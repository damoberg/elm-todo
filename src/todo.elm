import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

main = Browser.sandbox { init=init, update=update, view=view }

-- Model
type alias Todo =
  { label: String
  , completed: Bool
  , id: Int
  }

type alias Model =
  { todos: List Todo
  , input: String
  , uid: Int
  }

type Msg
  = UpdateInput String
  | AddTodo
  | RemoveTodo Int
  | ToggleCompleteTodo Int

init : Model
init =
  { todos =
    [ { label = "Lorem"
      , completed = False
      , id = 1
      }
    , { label = "Ipsum"
      , completed = False
      , id = 2
      }
    ]
  , input = ""
  , uid = 3 -- Next ID to be assigned
  }

-- Update
update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateInput string
      -> { model
            | input = string
          }

    AddTodo
      -> { model
            | todos = createTodo model.input model.uid :: model.todos
            , input = ""
            , uid = model.uid + 1
          }

    RemoveTodo toRemove
      -> { model
            | todos = List.filter (\ todo -> todo.id /= toRemove) model.todos
          }

    ToggleCompleteTodo toComplete
      -> { model
            | todos = List.map (\ todo -> completeTodo todo toComplete) model.todos
          }


completeTodo : Todo -> Int -> Todo
completeTodo todo toComplete =
  if todo.id == toComplete
  then { todo | completed = not todo.completed }
  else todo

createTodo : String -> Int -> Todo
createTodo label id =
  { label = label
  , completed = False
  , id = id
  }

todoListItem : Todo -> Html Msg
todoListItem todo =
  let
    itemStyle = if todo.completed then "line-through" else ""
  in
  li [ onDoubleClick (RemoveTodo todo.id), onClick (ToggleCompleteTodo todo.id) ]
    [ if todo.completed then span [ style "margin-left" "-15px"] [ text "✓ " ] else text ""
    , span [ style "text-decoration" itemStyle ] [ text todo.label ]
    ]

-- View
view : Model -> Html Msg
view model =
  Html.form [ onSubmit AddTodo ]
    [ h1 [] [ text "Todos" ]
    , label [ for "input" ] [ text "Add todo: " ]
    , input [ type_ "text", id "input", onInput UpdateInput, value model.input, autofocus True, autocomplete False ] []
    , ul [ style "list-style" "none" ] (List.map todoListItem model.todos)
    ]
