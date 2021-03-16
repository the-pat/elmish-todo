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

type private TodoBeingEdited = { Id: Guid; Description: string }

type private Todo =
    { Id: Guid
      Description: string
      CompletedOn: DateTime option
      CreatedOn: DateTime
      TodoBeingEdited: TodoBeingEdited option }

type private State =
    { TodoList: Todo list
      NewTodo: string
      Filter: Filter }

type private Msg =
    | SetNewTodo of string
    | AddNewTodo
    | ToggleCompleted of Guid
    | DeleteTodo of Guid
    | CancelEdit of Guid
    | ApplyEdit of Guid
    | StartEditingTodo of Guid
    | SetEditedDescription of Guid * string
    | SetFilter of Filter

let private init () =
    { TodoList =
          [ { Id = Guid.NewGuid()
              Description = "Learn F#"
              CompletedOn = Some(DateTime.UtcNow.AddDays(-1.))
              CreatedOn = DateTime.UtcNow.AddMonths(-1)
              TodoBeingEdited = None }
            { Id = Guid.NewGuid()
              Description = "Learn Elmish"
              CompletedOn = None
              CreatedOn = DateTime.UtcNow.AddHours(-3.)
              TodoBeingEdited = None } ]
      NewTodo = ""
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
              CreatedOn = DateTime.UtcNow
              TodoBeingEdited = None }

        { state with
              TodoList = List.append state.TodoList [ nextTodo ]
              NewTodo = "" },
        Cmd.none
    | StartEditingTodo todoId ->
        let nextTodoList =
            state.TodoList
            |> List.map
                (fun todo ->
                    if todo.Id = todoId then
                        { todo with
                              TodoBeingEdited =
                                  Some(
                                      { Id = todo.Id
                                        Description = todo.Description }
                                  ) }
                    else
                        todo)

        { state with TodoList = nextTodoList }, Cmd.none
    | CancelEdit todoId ->
        let nextTodoList =
            state.TodoList
            |> List.map
                (fun todo ->
                    if todo.Id = todoId then
                        { todo with TodoBeingEdited = None }
                    else
                        todo)

        { state with TodoList = nextTodoList }, Cmd.none
    | ApplyEdit todoId ->
        let nextTodoList =
            state.TodoList
            |> List.map
                (fun todo ->
                    if todo.Id = todoId then
                        match todo.TodoBeingEdited with
                        | None -> todo
                        | Some todoBeingEdited when todoBeingEdited.Description = "" -> todo
                        | Some todoBeingEdited ->
                            { todo with
                                  Description = todoBeingEdited.Description
                                  TodoBeingEdited = None }
                    else
                        todo)

        { state with TodoList = nextTodoList }, Cmd.none
    | SetEditedDescription (todoId, newDescription) ->
        let nextTodoList =
            state.TodoList
            |> List.map
                (fun todo ->
                    if todo.Id = todoId then
                        { todo with
                              TodoBeingEdited =
                                  Some(
                                      { Id = todo.Id
                                        Description = newDescription }
                                  ) }
                    else
                        todo)

        { state with TodoList = nextTodoList }, Cmd.none
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

let private renderEditForm uneditedTodoDescription (todoBeingEdited: TodoBeingEdited) (dispatch: Msg -> unit) =
    Bulma.box [ Bulma.field.div [ field.isGrouped
                                  prop.children [ Bulma.control.div [ control.isExpanded
                                                                      prop.children [ Bulma.input.text [ input.isMedium
                                                                                                         prop.valueOrDefault
                                                                                                             todoBeingEdited.Description
                                                                                                         prop.onTextChange
                                                                                                             (fun newDescription ->
                                                                                                                 dispatch (
                                                                                                                     SetEditedDescription(
                                                                                                                         todoBeingEdited.Id,
                                                                                                                         newDescription
                                                                                                                     )
                                                                                                                 )) ] ] ]
                                                  Bulma.buttons [ Bulma.button.button [ color.isPrimary
                                                                                        if todoBeingEdited.Description = uneditedTodoDescription then
                                                                                            prop.disabled true
                                                                                        if todoBeingEdited.Description
                                                                                           <> uneditedTodoDescription then
                                                                                            prop.onClick
                                                                                                (fun _ ->
                                                                                                    dispatch (
                                                                                                        ApplyEdit
                                                                                                            todoBeingEdited.Id
                                                                                                    ))
                                                                                        prop.children [ Html.i [ prop.classes [ FA.fas
                                                                                                                                FA.fa_save ] ] ] ]
                                                                  Bulma.button.button [ color.isWarning
                                                                                        prop.onClick
                                                                                            (fun _ ->
                                                                                                dispatch (
                                                                                                    CancelEdit
                                                                                                        todoBeingEdited.Id
                                                                                                ))
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
                                                                                                                     if todo.CompletedOn.IsNone then
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
                                  match todo.TodoBeingEdited with
                                  | Some todoBeingEdited when todoBeingEdited.Id = todo.Id ->
                                      renderEditForm todo.Description todoBeingEdited dispatch
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
