


test_that("camr_name_file recognizes invalid project, description, or extension", {
  expect_error(camr_name_file(description = "invalid description", project = "valid"),
               "Assertion on 'description' failed")
  expect_error(camr_name_file(description = "valid_description", project = "invalid-name"),
               "Assertion on 'project' failed")
  expect_error(camr_name_file(description = "valid_description", project = "valid",
                              extension = ".invalid"),
               "Assertion on 'extension' failed")
})
