{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Landing where

import Import

getLandingR :: Handler Html
getLandingR = do
  defaultLayout $ do
    let interpolated = "interpolated string" :: Text
    $(widgetFile "landing")
