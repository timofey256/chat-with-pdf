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

sendRequest :: String -> String -> IO (Response Value)
sendRequest systemPrompt userPrompt = do
    -- Parse the request URL
    request' <- parseRequest ("POST " ++ apiEndpoint) 

    -- Define the JSON payload
    let jsonPayload = encode $ object
            [ "model" .= (defaultModel :: String)
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

apiResponseContentExtractionPattern :: String
apiResponseContentExtractionPattern = "\"content\":\"([^\"]*)\""

-- Function to convert Maybe String to Text
maybeStringToText :: Maybe String -> T.Text
maybeStringToText maybeStr = T.pack (fromMaybe failedResponseErrorMessage maybeStr)

replaceNewline :: T.Text -> T.Text
replaceNewline = T.replace "\\n" "\n"

-- Function to extract the "content" field from the JSON string and trim leading whitespaces
extractContent :: String -> Maybe String
extractContent json =
    let matches = json =~ apiResponseContentExtractionPattern :: (String, String, String, [String])
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
    
    TIO.putStrLn $ replaceNewline $ maybeStringToText $ extractContent (T.unpack stringResponse)
    putStrLn ""
    
    processUserInput systemPrompt

main :: IO ()
main = do
    TIO.putStrLn $ T.pack initMessage

    args <- getArgs
    case args of
          ["-f", file] ->
               do
                -- printIOMaybeString $ lookupEnv "API_KEY"
         
                Just pdf <- Pdftotext.openFile file
                let fileText = (pdftotext Physical pdf)
                let systemPrompt = (systemBasePrompt ++ T.unpack fileText)

                processUserInput systemPrompt

          _ -> putStrLn noFlagSpecifiedErrorMessage