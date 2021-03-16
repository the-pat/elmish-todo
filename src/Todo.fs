module Todo

open Elmish
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
open Feliz.UseElmish
open System

open FontAwesome

type private Todo =
    { Id: Guid
      Description: string
      CompletedOn: Option<DateTime> }

type private State =
    { TodoList: Todo list
      NewTodo: string }

type private Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of Guid
    | DeleteTodo of Guid

let private init () =
    { NewTodo = ""
      TodoList =
          [ { Id = Guid.NewGuid()
              Description = "Learn F#"
              CompletedOn = Some(DateTime.UtcNow.AddDays(-1.)) }
            { Id = Guid.NewGuid()
              Description = "Learn Elmish"
              CompletedOn = None } ] },
    Cmd.none

let private update msg state =
    match msg with
    | SetNewTodo todoText -> { state with NewTodo = todoText }, Cmd.none
    | DeleteTodo todoId ->
        let nextTodoList =
            state.TodoList
            |> List.filter (fun todo -> todo.Id <> todoId)

        { state with TodoList = nextTodoList }, Cmd.none
    | ToggleCompleted todoId ->
        let nextTodoList =
            state.TodoList
            |> List.map
                (fun todo ->
                    if todo.Id = todoId then
                        { todo with
                              CompletedOn =
                                  if todo.CompletedOn.IsNone then
                                      Some(DateTime.UtcNow)
                                  else
                                      None }
                    else
                        todo)

        { state with TodoList = nextTodoList }, Cmd.none
    | AddNewTodo when state.NewTodo = "" -> state, Cmd.none
    | AddNewTodo ->
        let nextTodo =
            { Id = Guid.NewGuid()
              Description = state.NewTodo
              CompletedOn = None }

        { state with
              TodoList = List.append state.TodoList [ nextTodo ]
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

let private renderTodo todo dispatch =
    Bulma.box [ Bulma.columns [ columns.isMobile ++ columns.isVCentered
                                prop.children [ Bulma.column [ Bulma.text.p [ prop.className "subtitle"
                                                                              prop.text todo.Description ] ]
                                                Bulma.column [ column.isNarrow
                                                               prop.children [ Bulma.buttons [ Bulma.button.button [ if todo.CompletedOn.IsSome then
                                                                                                                         color.isSuccess
                                                                                                                     prop.onClick
                                                                                                                         (fun _ ->
                                                                                                                             dispatch (
                                                                                                                                 ToggleCompleted
                                                                                                                                     todo.Id
                                                                                                                             ))
                                                                                                                     prop.children [ Html.i [ prop.classes [ FA.fas
                                                                                                                                                             FA.fa_check ] ] ] ]
                                                                                               Bulma.button.button [ color.isDanger
                                                                                                                     prop.onClick
                                                                                                                         (fun _ ->
                                                                                                                             dispatch (
                                                                                                                                 DeleteTodo
                                                                                                                                     todo.Id
                                                                                                                             ))
                                                                                                                     prop.children [ Html.i [ prop.classes [ FA.fas
                                                                                                                                                             FA.fa_trash ] ] ] ] ] ] ] ] ] ]

let private todoList state dispatch =
    Html.ul [ prop.children [ for todo in state.TodoList -> renderTodo todo dispatch ] ]

[<ReactComponent>]
let App () =
    let state, dispatch = React.useElmish (init, update, [||])

    Html.div [ prop.style [ style.padding 20 ]
               prop.children [ appTitle
                               inputField state dispatch
                               todoList state dispatch ] ]
