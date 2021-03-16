module Todo

open Elmish
open Feliz
open Feliz.Bulma
open Feliz.Bulma.Operators
open Feliz.UseElmish
open System

open FontAwesome

type private Filter =
    | All
    | Completed
    | NotCompleted

type private Todo =
    { Id: Guid
      Description: string
      CompletedOn: DateTime option
      CreatedOn: DateTime }

type private TodoBeingEdited = { Id: Guid; Description: string }

type private State =
    { TodoList: Todo list
      NewTodo: string
      TodoBeingEdited: TodoBeingEdited option
      Filter: Filter }

type private Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of Guid
    | DeleteTodo of Guid
    | CancelEdit
    | ApplyEdit
    | StartEditingTodo of Guid
    | SetEditedDescription of string
    | SetFilter of Filter

let private init () =
    { TodoList =
          [ { Id = Guid.NewGuid()
              Description = "Learn F#"
              CompletedOn = Some(DateTime.UtcNow.AddDays(-1.))
              CreatedOn = DateTime.UtcNow.AddMonths(-1) }
            { Id = Guid.NewGuid()
              Description = "Learn Elmish"
              CompletedOn = None
              CreatedOn = DateTime.UtcNow.AddHours(-3.) } ]
      NewTodo = ""
      TodoBeingEdited = None
      Filter = NotCompleted },
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
              CompletedOn = None
              CreatedOn = DateTime.UtcNow }

        { state with
              TodoList = List.append state.TodoList [ nextTodo ]
              NewTodo = "" },
        Cmd.none
    | StartEditingTodo todoId ->
        let nextEditModel =
            state.TodoList
            |> List.tryFind (fun todo -> todo.Id = todoId)
            |> Option.map
                (fun todo ->
                    { Id = todo.Id
                      Description = todo.Description })

        { state with
              TodoBeingEdited = nextEditModel },
        Cmd.none
    | CancelEdit -> { state with TodoBeingEdited = None }, Cmd.none
    | ApplyEdit ->
        match state.TodoBeingEdited with
        | None -> state, Cmd.none
        | Some todoBeingEdited when todoBeingEdited.Description = "" -> state, Cmd.none
        | Some todoBeingEdited ->
            let nextTodoList =
                state.TodoList
                |> List.map
                    (fun todo ->
                        if todo.Id = todoBeingEdited.Id then
                            { todo with
                                  Description = todoBeingEdited.Description }
                        else
                            todo)

            { state with
                  TodoList = nextTodoList
                  TodoBeingEdited = None },
            Cmd.none
    | SetEditedDescription newDescription ->
        let nextEditModel =
            state.TodoBeingEdited
            |> Option.map
                (fun todoBeingEdited ->
                    { todoBeingEdited with
                          Description = newDescription })

        { state with
              TodoBeingEdited = nextEditModel },
        Cmd.none
    | SetFilter newFilter -> { state with Filter = newFilter }, Cmd.none

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

let private renderEditForm todoBeingEdited dispatch =
    Bulma.box [ Bulma.field.div [ field.isGrouped
                                  prop.children [ Bulma.control.div [ control.isExpanded
                                                                      prop.children [ Bulma.input.text [ input.isMedium
                                                                                                         prop.valueOrDefault
                                                                                                             todoBeingEdited.Description
                                                                                                         prop.onTextChange (
                                                                                                             SetEditedDescription
                                                                                                             >> dispatch
                                                                                                         ) ] ] ]
                                                  Bulma.buttons [ Bulma.button.button [ color.isPrimary
                                                                                        prop.onClick
                                                                                            (fun _ ->
                                                                                                dispatch ApplyEdit)
                                                                                        prop.children [ Html.i [ prop.classes [ FA.fas
                                                                                                                                FA.fa_save ] ] ] ]
                                                                  Bulma.button.button [ color.isWarning
                                                                                        prop.onClick
                                                                                            (fun _ ->
                                                                                                dispatch CancelEdit)
                                                                                        prop.children [ Html.i [ prop.classes [ FA.fas
                                                                                                                                FA.fa_arrow_right ] ] ] ] ] ]

                                   ]

                 ]

let private renderTodo todo dispatch =
    Bulma.box [ Bulma.columns [ columns.isMobile ++ columns.isVCentered
                                prop.children [ Bulma.column [ Bulma.text.p [ if todo.CompletedOn.IsSome then
                                                                                  color.hasTextGreyLight
                                                                              prop.className "subtitle"
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
                                                                                               Bulma.button.button [ color.isPrimary
                                                                                                                     if todo.CompletedOn.IsSome then
                                                                                                                         prop.disabled
                                                                                                                             true
                                                                                                                     if todo.CompletedOn.IsSome then
                                                                                                                         prop.onClick
                                                                                                                             (fun _ ->
                                                                                                                                 dispatch (
                                                                                                                                     StartEditingTodo
                                                                                                                                         todo.Id
                                                                                                                                 ))
                                                                                                                     prop.children [ Html.i [ prop.classes [ FA.fas
                                                                                                                                                             FA.fa_edit ] ] ] ]
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
    let sortedTodoList =
        state.TodoList
        |> List.filter
            (fun todo ->
                match state.Filter with
                | All -> true
                | Completed -> todo.CompletedOn.IsSome
                | NotCompleted -> todo.CompletedOn.IsNone)
        |> List.sortBy (fun todo -> todo.CompletedOn, todo.CreatedOn)

    Html.ul [ prop.children [ for todo in sortedTodoList ->
                                  match state.TodoBeingEdited with
                                  | Some todoBeingEdited when todoBeingEdited.Id = todo.Id ->
                                      renderEditForm todoBeingEdited dispatch
                                  | _ -> renderTodo todo dispatch ] ]

let private renderFilterTabs state dispatch =
    Bulma.tabs [ tabs.isToggle ++ tabs.isFullWidth
                 prop.children [ Html.ul [ Bulma.tab [ if state.Filter = All then tab.isActive
                                                       prop.onClick (fun _ -> dispatch (SetFilter All))
                                                       prop.children [ Html.a [ prop.text "All" ] ] ]
                                           Bulma.tab [ if state.Filter = Completed then
                                                           tab.isActive
                                                       prop.onClick (fun _ -> dispatch (SetFilter Completed))
                                                       prop.children [ Html.a [ prop.text "Completed" ] ] ]
                                           Bulma.tab [ if state.Filter = NotCompleted then
                                                           tab.isActive
                                                       prop.onClick (fun _ -> dispatch (SetFilter NotCompleted))
                                                       prop.children [ Html.a [ prop.text "Not Completed" ] ] ] ] ] ]

[<ReactComponent>]
let App () =
    let state, dispatch = React.useElmish (init, update, [||])

    Html.div [ prop.style [ style.padding 20 ]
               prop.children [ appTitle
                               inputField state dispatch
                               renderFilterTabs state dispatch
                               todoList state dispatch ] ]
