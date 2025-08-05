
#Creates mock api with httptest
# camr_post_form_JSON_to_dtf = camr post form

with_mock_api({
  test_that("camr_post_form handles successful responses", {
    JSON_data = camr_post_form_JSON_to_dtf("exampleAPI.com/correct", list(param1 = "p"))
    expect_type(JSON_data, "list")
    expect_s3_class(JSON_data$data, "data.frame")
    expect_equal(JSON_data$status, 200)
    expect_equal(dim(JSON_data$data), c(3, 4))
  })


  test_that("camr_post_form handles responses with tabs and newlines", {
    JSON_data = camr_post_form_JSON_to_dtf("exampleAPI.com/formatting", list(param1 = "p"))
    expect_type(JSON_data, "list")
    expect_s3_class(JSON_data$data, "data.frame")
    expect_equal(JSON_data$status, 200)
    expect_equal(dim(JSON_data$data), c(3, 4))
  })

  test_that("camr_post_form handles empty responses", {
    JSON_data = camr_post_form_JSON_to_dtf("exampleAPI.com/empty", list(param1 = "p"))
    expect_type(JSON_data, "list")
    expect_s3_class(JSON_data$data, "data.frame")
    expect_equal(JSON_data$status, -1)
    expect_equal(dim(JSON_data$data), c(0, 0))
  })

  test_that("camr_post_form handles malformed responses", {
    JSON_data = camr_post_form_JSON_to_dtf("exampleAPI.com/malformed", list(param1 = "p"))
    expect_type(JSON_data, "list")
    expect_s3_class(JSON_data$data, "data.frame")
    expect_equal(JSON_data$status, -1)
    expect_equal(dim(JSON_data$data), c(0, 0))
  })

})
