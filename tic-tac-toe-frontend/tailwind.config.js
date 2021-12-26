module.exports =
  { content  :
      [ "./src/*.elm"
      , "./src/*.js"
      ]
  , darkMode : "class"
  , theme    :
      { extend :
          { maxWidth :
              { "1/2" : "50%"
              }
          , transitionProperty :
              { "transform-opacity" : "transform, opacity"
              , "margin-padding"    : "margin, padding"
              , "padding"           : "padding"
              }
          , zIndex :
              { "100" : "100"
              }
          }
      }
  , plugins : []
  }
