test_df <- data.frame(SBJ.INT.Age = c(11, 17, 19),
                 SBJ.FCT.Sex = factor(c("M", "F", "M"),
                                      levels = c("M", "F"),
                                      labels = c("Male", "Female")),
                 SBJ.FCT.Ethnicity = factor(c(1,0,0),
                                            levels = c(0,1),
                                            labels = c("Hispanic", "Not Hispanic")))

test_that("camr_make_table1 runs with numeric and factors", {
  expect_no_error(camr_make_table1(test_df,
                                   list(SBJ.INT.Age = "Age",
                                        SBJ.FCT.Sex = "Sex",
                                        SBJ.FCT.Ethnicity = "Ethnicity")))
})

test_that("camr_make_table1 runs with numeric vars only", {
  expect_no_error(camr_make_table1(test_df,
                                   list(SBJ.INT.Age = "Age")))
})

test_that("camr_make_table1 runs with no numeric vars", {
  expect_no_error(camr_make_table1(test_df,
                                   list(SBJ.FCT.Sex = "Sex",
                                        SBJ.FCT.Ethnicity = "Ethnicity")))
})
