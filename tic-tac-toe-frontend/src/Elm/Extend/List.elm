module Extend.List exposing (..)


last : List a -> Maybe a
last = List.reverse >> List.head
