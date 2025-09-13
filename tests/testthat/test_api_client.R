library(testthat)
library(httr)

# Mock the httr::PUT function
mock_put <- function(...) {
  args <- list(...)
  # Store the arguments for inspection
  assign("put_args", args, envir = .GlobalEnv)
  # Return a dummy response
  structure(
    list(
      status_code = 200,
      content = charToRaw('{"status":"OK"}'),
      headers = list("Content-Type" = "application/json")
    ),
    class = "response"
  )
}

test_that("http_timeout is not duplicated in PUT requests", {
  # Create an instance of the ApiClient
  api_client <- ApiClient$new()

  # Use with_mock to replace httr::PUT with our mock function
  with_mock(
    `httr::PUT` = mock_put,
    {
      # Call the Execute method which should call httr::PUT
      api_client$Execute(
        url = "http://localhost/test",
        method = "PUT",
        query_params = list(),
        header_params = list(),
        form_params = list(),
        file_params = list(),
        accepts = list("application/json"),
        content_types = list("application/json"),
        body = "test body"
      )
    }
  )

  # Check the arguments passed to our mock PUT function
  # The timeout object is a list, so we search for that
  timeout_count <- sum(sapply(put_args, function(arg) inherits(arg, "timeout")))

  # This should fail before the fix (timeout_count will be 2)
  # and pass after the fix (timeout_count will be 1)
  expect_equal(timeout_count, 1, info = "http_timeout should only be passed once.")
})
