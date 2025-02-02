library(httr)
library(jsonlite)

# Replace with your actual OpenAI API key
OPENAI_API_KEY <- Sys.getenv("OPENAI_API_KEY")  

# Define the API endpoint
api_url <- "https://api.openai.com/v1/chat/completions"

# Prepare the request body (as a list)
request_body <- list(
  model = "gpt-4o-mini",
  messages = list(
    list(role = "system", content = "You are a helpful assistant."),
    list(role = "user", content = "Write a haiku about recursion in programming.")
  )
)

# Convert the list to JSON
json_body <- toJSON(request_body, auto_unbox = TRUE)

# Make the POST request
response <- POST(
  url = api_url,
  add_headers("Authorization" = paste("Bearer", OPENAI_API_KEY)),
  add_headers("Content-Type" = "application/json"),
  body = json_body
)

# Check for a successful response
if (http_status(response)$category == "Success") {
  # Parse the JSON response
  content <- content(response, "text", encoding = "UTF-8")
  parsed_content <- fromJSON(content)
  
  # Extract the text from the res
}