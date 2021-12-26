const fs   = require("fs");
const path = require("path");
const {src, dist} = require("./paths");
const CopyPlugin = require("copy-webpack-plugin");

module.exports =
  { entry :
      { main: path.resolve(src, "webpackEntry.js")
      }
  , output :
      { path: dist
      , filename: path.join("js", "[name].[contenthash].js")
      }
  , plugins :
      [ new CopyPlugin
          ( { patterns :
                [ { from  : path.resolve
                    ( src
                    , "**"
                    , "*.(svg|webmanifest|woff|woff2|ttf|otf|eot)"
                    )
                  , to({context, absoluteFilename}) {
                      let output = path.join
                        ( dist
                        , absoluteFilename.slice(src.length)
                        );
                      return output;
                    }
                  , force : true
                  }
                ]
            }
          )
      ]
  }
