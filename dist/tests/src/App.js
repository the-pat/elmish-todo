import { Interop_reactApi } from "../.fable/Feliz.1.37.0/Interop.fs.js";
import { createElement } from "react";
import { join } from "../.fable/fable-library.3.1.5/String.js";
import { singleton, ofArray } from "../.fable/fable-library.3.1.5/List.js";
import { createObj } from "../.fable/fable-library.3.1.5/Util.js";
import { Helpers_combineClasses } from "../.fable/Feliz.Bulma.2.11.0/ElementBuilders.fs.js";

export function HelloWorld() {
    let props_4, elms;
    const elms_1 = singleton((props_4 = ofArray([["className", "has-text-centered"], ["children", Interop_reactApi.Children.toArray([(elms = ofArray([createElement("i", {
        className: join(" ", ["fas", "fa-globe-europe"]),
    }), createElement("span", {
        children: ["Hello world"],
    }), createElement("i", {
        className: join(" ", ["fas", "fa-globe-americas"]),
    })]), createElement("h1", {
        className: "title",
        children: Interop_reactApi.Children.toArray(Array.from(elms)),
    }))])]]), createElement("div", createObj(Helpers_combineClasses("container", props_4)))));
    return createElement("section", {
        className: "section",
        children: Interop_reactApi.Children.toArray(Array.from(elms_1)),
    });
}

