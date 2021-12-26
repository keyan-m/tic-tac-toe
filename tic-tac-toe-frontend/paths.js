const fs   = require("fs");
const path = require("path");
const src  = path.resolve(__dirname, "src");
const dist = path.resolve(__dirname, "dist");
//                        ^-------^
// __dirname means relative to the script itself.
// Something like "./consts.txt" would mean relative
// to the executaion path.


module.exports =
  { src       : path.resolve(__dirname, "src")
  , dist      : path.resolve(__dirname, "dist")
  }
