module Config where

import Data.ByteString.Char8      as S8

apiKey :: S8.ByteString
apiKey = S8.pack "API_KEY"

initMessage :: String
initMessage = "Welcome to the Chat with PDF bot!\n\nYou can ask whatever questions you have regarding your file. For example, you might ask:\n - What the document is about?\n - What are main takeaways from this paper?\n - What research methods were used?  \n\nYou are using " ++ defaultModel ++ " model.\n"

systemBasePrompt :: String
systemBasePrompt = "You are a system that is designed to comprehend and memorize PDF files. It must accurately respond to user queries about these files, providing summaries, explanations of concepts, or examples of methods used in the research. Responses should be concise yet detailed, covering all necessary information. Do not use \" brackets in your response \n"

defaultModel :: String
defaultModel = "mistralai/Mixtral-8x7B-Instruct-v0.1"

failedResponseErrorMessage :: String
failedResponseErrorMessage = "[Error] Couldn't retrieve a response from API."

apiEndpoint :: String
apiEndpoint = "https://api.together.xyz/v1/chat/completions"

noFlagSpecifiedErrorMessage :: String
noFlagSpecifiedErrorMessage = "Error: You need to specify file path with -f flag!"