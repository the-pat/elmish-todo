module Persistance

open Browser.WebStorage
open Fable.SimpleJson

let inline set (key: string) (value: 't) =
    let json = Json.serialize<'t> value

    localStorage.setItem (key, json)

let inline get<'t> (key: string) =
    key |> localStorage.getItem |> Json.tryParseAs<'t>
