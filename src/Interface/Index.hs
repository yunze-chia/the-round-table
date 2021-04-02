{-# LANGUAGE OverloadedStrings #-}

module Interface.Index where

import Data.Map (fromList)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Interface.Bulma (bulmaCSS)
import Replica.VDOM (Attr (ABool, AText), HTML, VDOM (VLeaf, VNode, VRawText, VText), clientDriver)

index :: Text -> HTML
index title =
  [ VLeaf "!doctype" (fromList [("html", ABool True)]) Nothing,
    VNode
      "html"
      mempty
      Nothing
      [ VNode
          "head"
          mempty
          Nothing
          [ VLeaf "meta" (fromList [("charset", AText "utf-8")]) Nothing,
            VNode "title" mempty Nothing [VText title],
            VLeaf "link" (fromList [("rel", AText "stylesheet"), ("href", AText bulmaCSS)]) Nothing
          ],
        VNode
          "body"
          mempty
          Nothing
          [ VNode "script" (fromList [("language", AText "javascript")]) Nothing [VRawText $ decodeUtf8 clientDriver]
          ]
      ]
  ]
