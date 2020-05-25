import { Elm } from "./src/Main";

import "normalize.css/normalize.css";
import "milligram/dist/milligram.css";
import "./src/styles/index.scss";

try {
  const app = Elm.Main.init({ node: document.getElementById("elm") });
} catch (e) {
  // display initialization errors (e.g. bad flags, infinite recursion)
  const header = document.createElement("h1");
  header.style.fontFamily = "monospace";
  header.innerText = "Initialization Error";
  const pre = document.getElementById("elm");
  document.body.insertBefore(header, pre);
  pre.innerText = e;
  throw e;
}
