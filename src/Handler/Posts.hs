{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Handler.Posts where

import           Import
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import Data.Aeson.Types (Result(..), Parser, parseEither, withObject)

postForm :: UserId -> Form Post
postForm userId =
  renderDivs $
    Post <$>
    areq textField "Title" Nothing <*>
    areq textareaField "Text" Nothing <*>
    areq hiddenField "" (Just userId)

getPostsR :: Handler Html
getPostsR = do
  userId <- fmap fst $ requireAuthPair
  (widget, enctype) <- generateFormPost $ postForm userId
  allPosts <- runDB
         $ E.select
         $ E.from $ \(post `E.InnerJoin` user) -> do
              E.on $ post ^. PostUserId E.==. user ^. UserId
              E.orderBy [E.desc (post ^. PostId)]
              return
                  ( post ^. PostId
                  , post ^. PostUserId
                  , post ^. PostTitle
                  , post ^. PostText
                  , user ^. UserIdent
                  )
  emptyLayout $ do
    $(widgetFile "posts")

data PostData = PostData (Entity Post, Entity User)

instance ToJSON PostData where
  toJSON (PostData (postEntity, userEntity)) =
    let
      post = entityVal postEntity
      postId = entityKey postEntity
      user = entityVal userEntity
      userId = entityKey userEntity
    in
    object
      [ "id" .= postId
      , "title" .= postTitle post
      , "text" .= postText post
      , "user" .= object
        [ "id" .= userId
        , "username" .= userIdent user
        ]
      ]

getApiPostsR :: Handler Value
getApiPostsR = do
  userId <- fmap fst $ requireAuthPair
  (widget, enctype) <- generateFormPost $ postForm userId
  allPosts <- runDB
         $ E.select
         $ E.from $ \(post `E.InnerJoin` user) -> do
              E.on $ post ^. PostUserId E.==. user ^. UserId
              E.orderBy [E.desc (post ^. PostId)]
              return (post, user)
  let allPosts' = PostData <$> allPosts
  return $ object [ "posts" .= allPosts' ]

postApiPostsR :: Handler Value
postApiPostsR = do
  (result :: Result Value) <- parseCheckJsonBody
  case result of
    Success val -> do
      let mPost = parseEither postParser val
      case mPost of
        Right post -> do
          postId <- runDB $ insert post
          return $ object [ "post" .= post, "id" .= postId ]
        Left err ->
          invalidArgs [pack err]
    Error err ->
      invalidArgs [pack err]

postParser :: Value -> Parser Post
postParser = withObject "Post" (\obj -> do
                title <- obj .: "title"
                text <- obj .: "text"
                userId <- obj .: "userId"
                return $ Post title text userId)

postPostsR :: Handler Html
postPostsR = do
  userId <- fmap fst $ requireAuthPair
  ((result, widget), enctype) <- runFormPost $ postForm userId
  allPosts <- runDB
         $ E.select
         $ E.from $ \(post `E.InnerJoin` user) -> do
              E.on $ post ^. PostUserId E.==. user ^. UserId
              E.orderBy [E.desc (post ^. PostId)]
              return
                  ( post ^. PostId
                  , post ^. PostUserId
                  , post ^. PostTitle
                  , post ^. PostText
                  , user ^. UserIdent
                  )
  case result of
    FormSuccess post -> do
      _ <- runDB $ insert post
      redirect PostsR
    _ ->
      emptyLayout $ do
        $(widgetFile "posts")

deletePostR :: PostId -> Handler Html
deletePostR postId = do
  _ <- runDB $ delete postId
  redirect PostsR

deleteApiPostR :: PostId -> Handler Value
deleteApiPostR postId = do
  _ <- runDB $ delete postId
  return Null
