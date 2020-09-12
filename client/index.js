import { Elm } from './src/Main';
import './css/index.scss';

const lsKey = 'sheet';

try {
  const node = document.getElementById('elm');
  const flags = JSON.parse(localStorage.getItem(lsKey));
  const app = Elm.Main.init({ node, flags });
  app.ports.storeSheet.subscribe((sheet) => {
    localStorage.setItem(lsKey, JSON.stringify(sheet));
  });
} catch (e) {
  // display initialization errors (e.g. bad flags, infinite recursion)
  const header = document.createElement('h1');
  header.style.fontFamily = 'monospace';
  header.innerText = 'Initialization Error';
  const pre = document.getElementById('elm');
  document.body.insertBefore(header, pre);
  pre.innerText = e;
  throw e;
}
