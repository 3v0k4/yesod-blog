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
    Login <$> areq textField "Username" Nothing <*> areq passwordField "Password" Nothing


getLandingR :: Handler Html
getLandingR = do
  maybeUser <- maybeAuthPair
  case maybeUser of
    Just _ ->
      redirect $ PostsR
    Nothing ->
      redirect $ AuthR LoginR

postLandingR :: Handler Html
postLandingR = do
  ((result, widget), enctype) <- runFormPost loginForm
  case result of
    FormSuccess login -> do
      user <- runDB $ selectList [ UserIdent ==. username login, UserPassword ==. Just (password login) ] []
      case user of
        [] ->
          emptyLayout $ do
            $(widgetFile "landing")

        _ ->
          redirect PostsR
    _ ->
      emptyLayout $ do
        $(widgetFile "landing")
