
#Testing %??%
test_that("%??% works", {
  expect_equal(1 %??% 2 , 1)
  expect_equal(NA %??% 3, 3)
  expect_equal(NULL %??% 3, 3)
})


#Testing %p%
test_that("%p% works with strings", {
  expect_equal("an" %p% " apple", "an apple")
  expect_equal(3 %p% "p", "3p")
  expect_equal(3 %p% 3, "33")
})

test_that("%p% works with string vectors", {
  expect_equal("an" %p% c(" apple", " orange", " apricot"), c("an apple", "an orange", "an apricot"))
  expect_equal(3 %p% c(" dollars", " cents"), c("3 dollars", "3 cents"))
  expect_equal(4 %p% c(3, 4, 5), c("43", "44", "45"))
})

#Testing %+=% and %-=%


test_that("%+=% works normally", {
  #Testing that variable assigns, doesn't just output result
  x = 0
  z = x %+=% 7

  expect_equal(x, 7)
  expect_equal(z, 7)
  expect_error(7 %+=% 7, "left-hand side to assignment")
})



test_that("%-=% works normally", {
  #Testing that variable assigns, doesn't just output result
  x = 0
  z = x %-=% 7

  expect_equal(x, -7)
  expect_equal(z, -7)
  expect_error(7 %-=% 7, "left-hand side to assignment")
})


test_that("%+=% works with vectors", {
  #Testing that variable assigns, doesn't just output result
  x = 1:3
  z = x %+=% 7

  expect_equal(x, c(8, 9, 10))
  expect_equal(z, c(8, 9, 10))
})


test_that("%-=% works with vectors", {
  #Testing that variable assigns, doesn't just output result
  x = c(7, 8, 9)
  z = x %-=% 7

  expect_equal(x, c(0, 1, 2))
  expect_equal(z, c(0, 1, 2))
})


#Testing %not_in%
test_that("%not_in% works", {
  expect_equal(1 %not_in% 2, TRUE)
  expect_equal(1 %not_in% c(1, 2), FALSE)
  expect_equal(c(1, 2) %not_in% c(3, 4, 1), c(FALSE, TRUE))
  expect_equal(c(1, 3, 4, 5) %not_in% c(1, 2), c(FALSE, TRUE, TRUE, TRUE))
})


#Testing %pm% and %em%
test_that("%pm% works", {
  expect_equal(1:3 %pm% 1, c(TRUE, FALSE, FALSE))
  expect_equal(c("hey", "hello", "goodbye") %pm% "he", c(TRUE, TRUE, FALSE))
})

test_that("%pm% is robust against NAs", {
  expect_equal(c("hello", NA, "world") %pm% "he", c(TRUE, FALSE, FALSE))
  expect_equal(c("test", "tester", NA) %pm% NA, c(FALSE, FALSE, TRUE))
})

test_that("%em% works", {
  expect_equal(1:3 %em% 1, c(TRUE, FALSE, FALSE))
  expect_equal(c("hey", "he", "goodbye") %em% "he", c(FALSE, TRUE, FALSE))
})

test_that("%em% is robust against NAs", {
  expect_equal(c("hello", NA, "he") %em% "he", c(FALSE, FALSE, TRUE))
  expect_equal(c("test", "NAtrium", NA) %em% NA, c(FALSE, FALSE, TRUE))
})


#Testing %row%
dtf = data.frame(X1 = 1:4, X2 = LETTERS[1:4], X3 = c( TRUE, TRUE, FALSE, FALSE ))
attributes( dtf$X1 ) = list( Example_attr = "Integer" )
attributes( dtf$X2 ) = list( Example_attr = "Character" )
attributes( dtf$X3 ) = list( Example_attr = "Logical" )
dtf_subset_1 = dtf %rows% 1:2
dtf_subset_2 = dtf %rows% c("X2", "X3")


test_that("%row% works", {
  expect_equal(attributes(dtf_subset_1$X1), attributes(dtf$X1))
  expect_equal(attributes(dtf_subset_2$X2), attributes(dtf$X2))
  #Checks whether contents are the same by removing attributes
  expect_equal(dtf_subset_1[1:2,], dtf[1:2,])
  expect_equal(dtf_subset_2[c("X2", "X3"),], dtf[c("X2","X3"),])
})

#Testing camr_commit_number
test_that("camr_commit_number has 7-digits and is alphanumerical", {
  expect_equal(nchar(camr_commit_number()), 7)
  expect_true(grepl("^[A-Za-z0-9]+$", camr_commit_number()))
})

#Testing functions that extract dates and times
test_that("Functions follow expected format", {
  expect_true(grepl("^\\d{4}_\\d{2}_\\d{2}$", camr_ymd()))
  expect_true(grepl("^\\d{4}_\\d{2}_\\d{2}-\\d{2}_\\d{2}$", camr_ymd_hm()))
  expect_true(grepl("^\\d{4}_\\d{2}_\\d{2}-\\d{2}_\\d{2}_\\d{2}$", camr_ymd_hms()))
  expect_true(grepl("^\\d{2}_\\d{2}_\\d{2}$", camr_hms()))
})

test_that("Functions follow expected format with input", {
  expect_true(grepl("^\\d{4}:\\d{2}:\\d{2}$", camr_ymd(":")))
  expect_true(grepl("^\\d{4}:\\d{2}:\\d{2}-\\d{2}:\\d{2}$", camr_ymd_hm(":")))
  expect_true(grepl("^\\d{4}:\\d{2}:\\d{2}-\\d{2}:\\d{2}:\\d{2}$", camr_ymd_hms(":")))
  expect_true(grepl("^\\d{2}:\\d{2}:\\d{2}$", camr_hms(":")))
})

#Testing camr_list_idx
#Have to load purrr as it doesn't load with devtools(not added in Description?)
library("purrr")
test_that("camr_list_idx works", {
  df = data.frame(IDX.name = c("Alice", "Bob", "Carl"),
                  IDX.age = c(25, 30, 5),
                  nonindex = 1:3)
  expect_equal(camr_list_idx(df), c("IDX.name", "IDX.age"))
})

#Testing camr_join_on_idx
test_that("camr_list_on_idx works with valid dataframes", {
  df1 = data.frame(IDX.id = 1:3, name = c("Alice", "Bob", "Carl"))
  df2 = data.frame(IDX.id = 1:3, age = c(42, 54, 32))
  df3 = data.frame(IDX.id = 1:3, name = c("Alice", "Bob", "Carl"), age = c(42, 54, 32))
  df_joined = camr_join_on_idx(df1, df2)
  expect_equal(colnames(df_joined), c("IDX.id", "name", "age"))
  expect_equal(df_joined, df3)
})

test_that("camr_list_on_idx correctly joins even if index column values are swapped", {
  df1 = data.frame(IDX.id = 1:3, name = c("Alice", "Bob", "Carl"))
  #ids and ages are reverse order
  df2 = data.frame(IDX.id = 3:1, age = c(32, 54, 42))
  df3 = data.frame(IDX.id = 1:3, name = c("Alice", "Bob", "Carl"), age = c(42, 54, 32))
  df_joined = camr_join_on_idx(df1, df2)
  expect_equal(colnames(df_joined), c("IDX.id", "name", "age"))
  expect_equal(df_joined, df3)
})

test_that("camr_list_on_idx recognizes if index columns don't match", {
  df1 = data.frame(IDX.id = 1:3, name = c("Alice", "Bob", "Carl"))
  df2 = data.frame(IDX.id2 = 101:103, age = c(42, 54, 32))

  expect_error(camr_join_on_idx(df1, df2), "expects that the IDX columns of df_y will be present")
})

#Testing camr_write_csv

test_that("camr_write_csv writes a file with given name", {
  tmp_dir = tempdir()
  withr::local_dir(tmp_dir)

  df = data.frame(x = 1:3, y = c("a", "b", "c"))
  camr_write_csv(df, "examplefile.csv", lgl_literal = TRUE)

  expect_true(file.exists("examplefile.csv"))
  written = read.csv("examplefile.csv", stringsAsFactors = FALSE)
  expect_equal(written, df)
})

test_that("camr_write_csv errors for bad input", {
  expect_error(
    camr_write_csv(list(x = 1:3), "examplefile.csv", lgl_literal = TRUE),
    "Assertion on 'df_x' failed"
  )
  expect_error(
    camr_write_csv(data.frame(x = 1:3), "examplefile.txt", lgl_literal = TRUE),
    "Assertion on 'chr_filename' failed"
  )
  expect_error(
    camr_write_csv(data.frame(x = 1:3), "examplefile.csv", lgl_literal = "yes"),
    "Assertion on 'lgl_literal' failed"
  )
})

#Testing camr_assert

test_that("camr_assert outputs data if condition is true", {
  df = data.frame(x = 1:5)
  result = camr_assert(df, all(x > 0), "All x must be positive")
  expect_equal(result, df)
})

test_that("camr_assert errors if condition is false", {
  df = data.frame(x = c(1, -1))
  expect_error(
    camr_assert(df, all(x > 0), "All x must be positive"),
    "All x must be positive")
})

test_that("camr_assert errors if condition is not logical", {
  df = data.frame(x = 1:3)
  expect_error(
    camr_assert(df, sum(x), "Not logical"),
    "Must be of type 'logical'")
})

test_that("camr_assert errors if condition is NA", {
  df = data.frame(x = c(TRUE, NA))
  expect_error(
    camr_assert(df, all(x), "Contains missing values"),
    "Contains missing values")
})



