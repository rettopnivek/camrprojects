
#Testing camr_name_file

#Calls camr_name_file with default valid project, description, and datetime, with git disabled
test_name_file = function(description = "valid_description", project = "valid_project",
                          extension = NULL, date = NULL,
                          time = NULL, datetime = NULL, git = FALSE){

  #Checks if date, time, and datetime are all null, if so sets default datetime
  if(is.null(date) & is.null(time) & is.null(datetime)){
    datetime = "2024-12-03 20:04:00"
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
  expect_equal(test_name_file(date = "2024-12-03", time = "20:04:00"), filename)
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


#Testing camr_file_path
test_that("camr_file_path works with partial filename match", {
  temp_dir = tempdir()
  file.create(file.path(temp_dir, "examplefile.txt"))
  withr::local_dir(temp_dir)

  result = camr_file_path("examplefile")
  expect_true(file.exists(result))
  expect_equal(result, normalizePath("examplefile.txt", winslash = "/"))
})

test_that("camr_file_path can find latest created file", {
  temp_dir = tempdir()
  file1 = file.path(temp_dir, "valid_project-valid_description-2024_12_03-20_04_00.txt")
  file2 = file.path(temp_dir, "valid_project-valid_description-2024_12_03-20_05_00.txt")
  file.create(file1)
  #Create file2 later
  Sys.sleep(1)
  file.create(file2)
  withr::local_dir(temp_dir)

  result =  camr_file_path("valid_project-valid_description", latest = TRUE)
  expect_equal(result,
               normalizePath("valid_project-valid_description-2024_12_03-20_05_00.txt", winslash = "/"))
})

test_that("camr_file_path uses env_variable correctly", {
  temp_dir = tempdir()
  #Fixes problem with Windows matching
  temp_dir = normalizePath(temp_dir, winslash = "/")

  file.create(file.path(temp_dir, "examplefile.txt"))
  withr::local_envvar(FOLDER = temp_dir)

  result = camr_file_path("examplefile", env_variable = "FOLDER")
  withr::local_dir(temp_dir)
  expect_equal(result, normalizePath("examplefile.txt", winslash = "/"))
})


test_that("camr_file_path uses folder correctly", {
  temp_dir = tempdir()
  temp_dir = normalizePath(temp_dir, winslash = "/")

  subfolder = file.path(temp_dir, "subfolder")
  dir.create(subfolder)
  file.create(file.path(subfolder, "examplefile.txt"))
  withr::local_dir(temp_dir)


  # test succeeds if folder argument is a uniquely identifiable
  # substring of folder(e.g, "subfold), but not if folder = "subfolder"

  #This results in an error("Unable to locate any folders containing the label")
  result = camr_file_path("examplefile.txt", folder = "subfolder")
  expect_equal(result, normalizePath("subfolder/examplefile.txt", winslash = "/"))
})


