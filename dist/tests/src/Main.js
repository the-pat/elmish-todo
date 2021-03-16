import * as global$ from "../../../src/styles/global.scss";
import { render } from "react-dom";
import { createElement } from "react";
import { HelloWorld } from "./App.js";


render(createElement(HelloWorld, null), document.getElementById("feliz-app"));

