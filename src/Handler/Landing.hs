{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Landing where

import Import
import Text.Hamlet          (hamletFile)

emptyLayout :: Widget -> Handler Html
emptyLayout widget = do
    pc <- widgetToPageContent $ do
        $(widgetFile "empty-layout")
    withUrlRenderer $(hamletFile "templates/empty-layout-wrapper.hamlet")

getLandingR :: Handler Html
getLandingR = do
  emptyLayout $ do
    $(widgetFile "landing")
