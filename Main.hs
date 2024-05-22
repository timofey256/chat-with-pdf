#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import           Data.Aeson                 (Value, object, (.=), encode, FromJSON)
import qualified Data.ByteString.Char8      as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TIO
import           Network.HTTP.Simple
import           Pdftotext
import           System.IO
import           Data.Maybe
import           GHC.Generics
import           Text.Regex.PCRE            ((=~))
import System.Environment ( getArgs )
import System.Environment (lookupEnv)
import Config

systemBasePrompt :: String
systemBasePrompt = "You are a system that is designed to comprehend and memorize PDF files. It must accurately respond to user queries about these files, providing summaries, explanations of concepts, or examples of methods used in the research. Responses should be concise yet detailed, covering all necessary information. Do not use \" brackets in your response \n"

modelName :: String
modelName = "mistralai/Mixtral-8x7B-Instruct-v0.1"

sendRequest :: String -> String -> IO (Response Value)
sendRequest systemPrompt userPrompt = do
    -- Parse the request URL
    request' <- parseRequest "POST https://api.together.xyz/v1/chat/completions"

    -- Define the JSON payload
    let jsonPayload = encode $ object
            [ "model" .= (modelName :: String)
            , "messages" .=
                [ object
                    [ "role" .= ("system" :: String)
                    , "content" .= (systemPrompt :: String)
                    ]
                , object
                    [ "role" .= ("user" :: String)
                    , "content" .= (userPrompt :: String)
                    ]
                ]
            ]

    -- Create the request with the necessary modifications
    let request = setRequestMethod "POST"
                $ setRequestHeader "Authorization" ["Bearer " <> apiKey]
                -- $ setRequestHeader "Authorization" ["Bearer " <> apiKey]
                $ setRequestHeader "Content-Type" ["application/json"]
                $ setRequestBodyLBS jsonPayload
                $ request'

    -- Send the request and get the response
    httpJSON request :: IO (Response Value)

pattern :: String
pattern = "\"content\":\"([^\"]*)\""

-- Function to convert Maybe String to Text
maybeStringToText :: Maybe String -> T.Text
maybeStringToText maybeStr = T.pack (fromMaybe "" maybeStr)

-- Function to extract the "content" field from the JSON string and trim leading whitespaces
extractContent :: String -> Maybe String
extractContent json =
    let matches = json =~ pattern :: (String, String, String, [String])
    in case matches of
        (_, _, _, [content]) -> Just (T.unpack $ T.stripStart $ T.pack content)
        _ -> Nothing

-- Function to process user input and print response
processUserInput :: String -> IO ()
processUserInput systemPrompt = do
    putStr "Enter question: "
    hFlush stdout
    userQuery <- getLine

    response <- sendRequest systemPrompt userQuery
    let stringResponse = T.pack $ L8.unpack $ encode $ getResponseBody response
    
    putStr "Response: "
    TIO.putStrLn $ maybeStringToText $ extractContent (T.unpack stringResponse)
    putStrLn ""
    
    processUserInput systemPrompt

-- initialInformation :: String
-- initialInformation = 

convertIOMaybeStringToByteString :: IO (Maybe String) -> IO S8.ByteString
convertIOMaybeStringToByteString ioMaybeStr = do
    maybeStr <- ioMaybeStr
    let str = fromMaybe "" maybeStr
    return $ S8.pack str

main :: IO ()
main = do
    args <- getArgs
    case args of
          ["-f", file] ->
               do
                -- printIOMaybeString $ lookupEnv "API_KEY"
         
                Just pdf <- Pdftotext.openFile file
                let fileText = (pdftotext Physical pdf)
                let systemPrompt = (systemBasePrompt ++ T.unpack fileText)

                processUserInput systemPrompt

          _ -> putStrLn ("Error: You need to specify file path with -f flag!")