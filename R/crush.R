library(httr)

login <- function(username, password) {
  # Define the URL and parameters
  url <- "http://crushftp.concertzender.nl:9090"
  crush_body <- list(
    command = "login",
    username = username,
    password = password
  )

  # Make the POST request
  response <- POST(url, body = crush_body, encode = "form")

  # Check the response status
  if (http_status(response)$category != "Success") {
    stop("Login failed with status: ", http_status(response)$message)
  }

  headers <- response$headers
  headers[names(headers) == "set-cookie"] |> unlist() |>
    keep(\(x) str_detect(x, "current")) |> str_extract("=(.*); ", group = 1)
}

c2f <- login(username = "lon", password = "lonatcrush.18")
