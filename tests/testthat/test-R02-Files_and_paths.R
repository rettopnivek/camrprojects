
#Calls camr_name_file with default valid project, description, and datetime, with git disabled
test_name_file = function(description = "valid_description", project = "valid_project",
                          extension = NULL, date = NULL,
                          time = NULL, datetime = NULL, git = FALSE){

  #Checks if date, time, and datetime are all null, if so sets default datetime
  if(is.null(date) & is.null(time) & is.null(datetime)){
    datetime = "2024-12-03 20:04"
  }

  camr_name_file(description = description,
                 project = project,
                 extension = extension,
                 date = date,
                 time = time,
                 datetime = datetime,
                 git = git)
}

test_that("camr_name_file has consistent naming practices", {
  filename = "valid_project-valid_description-2024_12_03-20_04_00.RData"
  expect_equal(test_name_file(date = "2024-12-03", time = "20:04"), filename)
  expect_equal(test_name_file(extension = "RData"), filename)

})

test_that("camr_name_file recognizes invalid project, description, or extension", {
  expect_error(test_name_file(description = "invalid description"),
               "Assertion on 'description' failed")
  expect_error(test_name_file(project = "invalid-name"),
               "Assertion on 'project' failed")
  expect_error(test_name_file(extension = ".invalid"),
               "Assertion on 'extension' failed")
})

test_that("camr_name_file recognizes malformed dates/times", {
  expect_error(test_name_file(date = "2024:13:03"),
               "Invalid date format")
  expect_error(test_name_file(time = "25:04"),
               "Invalid time format")
  expect_error(test_name_file(datetime = "2024:12:003:20:04"),
               "Invalid datetime format")
})
