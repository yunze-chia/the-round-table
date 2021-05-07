{-# LANGUAGE OverloadedStrings #-}

module Interface.Bulma where

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

widthWrapper :: P.Props a
widthWrapper = P.className "container is-max-desktop has-text-centered"

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
containerBoxCentered = P.className "container box has-text-centered is-max-desktop"

tableFullWidth :: P.Props a
tableFullWidth = P.className "table is-fullwidth"

header4 :: P.Props a
header4 = P.className "is-size-4 has-text-weight-semibold"

bolded :: P.Props a
bolded = P.className "has-text-weight-bold has-text-primary"

-- section :: P.Props a
-- section = P.className "section"
