{-# LANGUAGE OverloadedStrings #-}

module Interface.Index where

import Data.Map           (fromList)
import Data.Text          (Text)
import Data.Text.Encoding (decodeUtf8)
import Replica.VDOM       (Attr (ABool, AText), HTML, VDOM (VLeaf, VNode, VRawText, VText), clientDriver)

indexWithBootstrap :: Text -> HTML
indexWithBootstrap title =
  [ VLeaf "!doctype" (fl [("html", ABool True)]) Nothing,
    VNode "html" mempty Nothing
      [ VNode "head" mempty Nothing
          [ VLeaf "meta" (fl
              [ ("charset", AText "utf-8"),
                  ("name", AText "viewport"),
                  ("content", AText "width=device-width, initial-scale=1, shrink-to-fit=no")
              ]) Nothing,
            VNode "title" mempty Nothing [VText title],
            VLeaf "link" (fl
              [ ("rel", AText "stylesheet"),
                ("href", AText "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/css/bootstrap.min.css"),
                ("integrity", AText "sha384-Vkoo8x4CGsO3+Hhxv8T/Q5PaXtkKtu6ug5TOeNV6gBiFeWPGFN9MuhOf23Q9Ifjh"),
                ("crossorigin", AText "anonymous")
              ]) Nothing,
            customCss
          ],
        VNode "body" mempty Nothing
          [ VNode "script" (fl [("language", AText "javascript")]) Nothing
              [VRawText $ decodeUtf8 clientDriver],
            VNode "script" (fl
              [ ("src", AText "https://code.jquery.com/jquery-3.4.1.slim.min.js"),
                ("integrity", AText "sha384-J6qa4849blE2+poT4WnyKhv5vZF5SrPo0iEjwBvKU7imGFAV0wwj1yYfoRSJoZ+n"),
                ("crossorigin", AText "anonymous")
              ]) Nothing [],
            VNode "script" (fl
              [ ("src", AText "https://cdn.jsdelivr.net/npm/popper.js@1.16.0/dist/umd/popper.min.js"),
                ("integrity", AText "sha384-Q6E9RHvbIyZFJoft+2mJbHaEWldlvI9IOYy5n3zV9zzTtmI3UksdQRVvoxMfooAo"),
                ("crossorigin", AText "anonymous")
              ]) Nothing [],
            VNode "script" (fl
              [ ("src", AText "https://stackpath.bootstrapcdn.com/bootstrap/4.4.1/js/bootstrap.min.js"),
                ("integrity", AText "sha384-wfSDF2E50Y2D1uUdj0O3uMBJnjuUD4Ih7YwaYd1iqfktj0Uod8GCExl3Og8ifwB6"),
                ("crossorigin", AText "anonymous")
              ]) Nothing []
          ]
      ]
  ]
  where
    fl = fromList

customCss :: VDOM
customCss =
  VNode "style" (fl [("type", AText "text/css")]) Nothing
    [VText ".window { margin-right: auto; margin-left: auto; max-width: 640px;}"]
  where
    fl = fromList
