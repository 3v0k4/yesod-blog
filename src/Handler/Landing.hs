{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Landing where

import Import

data Login =
  Login { username :: Text, password :: Text }
  deriving Show

loginForm :: Form Login
loginForm =
  renderDivs $
    Login <$> areq textField "Username" Nothing <*> areq textField "Password" Nothing

getLandingR :: Handler Html
getLandingR = do
  (widget, enctype) <- generateFormPost loginForm
  emptyLayout $ do
    $(widgetFile "landing")

postLandingR :: Handler Html
postLandingR = do
  ((result, widget), enctype) <- runFormPost loginForm
  case result of
    FormSuccess _ ->
      redirect PostsR
    _ ->
      emptyLayout $ do
        $(widgetFile "landing")
