{-# LANGUAGE OverloadedStrings #-}

module Interface.Bulma (containerBox, block, title, button, input, bulmaCSS, containerBoxCentered, hasTextCentered) where

import qualified Concur.Replica.DOM.Props as P
import Data.Text (Text)

bulmaCSS :: Text
bulmaCSS = "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"

input :: P.Props a
input = P.className "input"

button :: P.Props a
button = P.className "button"

-- buttonsCentered :: P.Props a
-- buttonsCentered = P.className "buttons is-centered"

title :: P.Props a
title = P.className "title"

hasTextCentered :: P.Props a
hasTextCentered = P.className "has-text-centered"

-- container :: P.Props a
-- container = P.className "container"

-- columns :: P.Props a
-- columns = P.className "columns"

-- column :: P.Props a
-- column = P.className "column"

block :: P.Props a
block = P.className "block"

containerBox :: P.Props a
containerBox = P.className "container box"

containerBoxCentered :: P.Props a
containerBoxCentered = P.className "container box has-text-centered"

-- section :: P.Props a
-- section = P.className "section"
