const path = require("path");
const {merge} = require("webpack-merge");
const {CleanWebpackPlugin} = require("clean-webpack-plugin");
const HtmlWebpackPlugin    = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const {src, dist} = require("./paths");


module.exports = merge
  ( require("./webpack.common")
  , { mode    : "development"
    , plugins :
        [ new MiniCssExtractPlugin
            ( { filename : path.join("css", "[name].[contenthash].css")
              }
            )
        , new CleanWebpackPlugin
            ( { dry : false // true for a test run (no removals)
              , cleanOnceBeforeBuildPatterns :
                  [ "**/*.js"
                  , "**/*.css"
                  ]
              }
            )
        , new HtmlWebpackPlugin
            ( { template   : path.join(src, "index.html")
              , filename   : path.join("index.html")
              , inject     : "body"
              , publicPath : "/"
              }
            )
        ]
    , module  :
        { rules :
            [ { test    : /\.elm$/
              , exclude : [/node_modules/, /elm-stuff/]
              , loader  : "elm-webpack-loader"
              }
            , { test    : /\.css$/
              , include : /css/
              , use     :
                  [ MiniCssExtractPlugin.loader
                  , { loader  : "css-loader"
                    , options : {url : false}
                    }
                  , "postcss-loader"
                  ]
              }
            ]
        }
    }
  );
