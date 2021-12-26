// const {src, dist, publicDir} = require("./../paths");
import "./css/main.css";
import "./Main.elm";

const body     = document.body;
const html     = document.getElementsByTagName("html")[0]
const title    = document.title;
const protocol = location.protocol
const host     = location.host
const widthPx  = window.innerWidth;
const heightPx = window.innerHeight;

var socket;

// {{{ DARK MODE 
var darkMode;
if (window.matchMedia('(prefers-color-scheme: dark)').matches) {
  darkMode = true;
} else {
  darkMode = false;
}
// }}}

// {{{ APP INITIALIZATION 
const {Elm} = require("./Main.elm");

const app = Elm.Main.init
  ( { node  : document.getElementById("main")
    , flags : 
        { title    : title
        , protocol : protocol
        , host     : host
        , widthPx  : widthPx
        , heightPx : heightPx
        , darkMode : darkMode
        , seed     : Date.now()
        }
    }
  );
// }}}

// {{{ ELM INTEROP 
app.ports.setBackgroundColor.subscribe(function(bgClass) {
  // {{{
  html.classList = ["no-js " + bgClass];
  body.classList = [bgClass];
  // }}}
});


function connectWS(gameCode) {
  // {{{
  const wsProtocol = protocol === "https" ? "wss" : "ws";
  const wsHost     = wsProtocol + "//" ++ host;
  socket = new WebSocket(wsHost + "/play/" + gameCode);
  console.log("WebSocket to " + gameCode + " is now open.");
  // }}}
}


app.ports.openSocket.subscribe(function(gameCode) {
  // {{{
  if (socket) {
    if (socket.readyState === WebSocket.CONNECTING || socket.readyState === WebSocket.OPEN) {
      socket.close();
    }
    socket = undefined;
    connectWS(gameCode);
  } else {
    connectWS(gameCode);
  }
  // }}}
});


app.ports.closeSocket.subscribe(function() {
  // {{{
  try {
    socket.close();
  } catch(e) {
  }
  socket = undefined;
  // }}}
});


app.ports.sendThroughSocket.subscribe(function(jsonData) {
  // {{{
  try {
    socket.send(jsonData);
  } catch(e) {
  }
  // }}}
}
// }}}
