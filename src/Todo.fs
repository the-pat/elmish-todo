module Todo

open Elmish
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
open Feliz.UseElmish

open FontAwesome

type private State =
    { TodoList: string list
      NewTodo: string }

type private Msg =
    | SetNewTodo of string
    | AddNewTodo

let private init () =
    { TodoList = [ "Learn F#" ]
      NewTodo = "" },
    Cmd.none

let private update msg state =
    match msg with
    | SetNewTodo todoText -> { state with NewTodo = todoText }, Cmd.none
    | AddNewTodo when state.NewTodo = "" -> state, Cmd.none
    | AddNewTodo ->
        { state with
              TodoList = List.append state.TodoList [ state.NewTodo ]
              NewTodo = "" },
        Cmd.none

let private appTitle =
    Bulma.title [ prop.innerHtml "Elmish To-Do List" ]

let private inputField state dispatch =
    Bulma.field.div [ field.hasAddons
                      prop.children [ Bulma.control.div [ control.isExpanded
                                                          prop.children [ Bulma.input.text [ input.isMedium
                                                                                             prop.valueOrDefault
                                                                                                 state.NewTodo
                                                                                             prop.onChange (
                                                                                                 SetNewTodo >> dispatch
                                                                                             ) ] ] ]
                                      Bulma.control.div [ prop.children [ Bulma.button.button [ color.isPrimary
                                                                                                ++ button.isMedium
                                                                                                prop.onClick
                                                                                                    (fun _ ->
                                                                                                        dispatch
                                                                                                            AddNewTodo)
                                                                                                prop.children [ Html.i [ prop.classes [ FA.fas
                                                                                                                                        FA.fa_plus ] ] ] ] ] ] ] ]

let private todoList state dispatch =
    Html.ul [ prop.children [ for todo in state.TodoList ->
                                  Html.li [ prop.classes [ "box"; "subtitle" ]
                                            prop.text todo ] ] ]

[<ReactComponent>]
let App () =
    let state, dispatch = React.useElmish (init, update, [||])

    Html.div [ prop.style [ style.padding 20 ]
               prop.children [ appTitle
                               inputField state dispatch
                               todoList state dispatch ] ]
