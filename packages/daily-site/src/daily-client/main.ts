// @ts-ignore
import { Elm } from "./elm/Main.elm";

import view_state from "./test/view_state.json";

Elm.Main.init({
    flags: view_state,
    node: document.getElementById("app"),
});
