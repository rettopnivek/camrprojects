test_that("Variable name has at least 3 parts", {
  expect_equal(validate_var_name("FOO"), FALSE)
  expect_equal(validate_var_name("FOO.BAZ"), FALSE)
  expect_equal(validate_var_name("FOO.CHR.BAZ"), TRUE)
  expect_equal(validate_var_name("FOO.CHR.BAZ.Bar"), TRUE)
})

test_that("Variable name begins with 3 uppercase letters", {
  expect_equal(validate_var_name("FO.CHR.Baz"), FALSE)
  expect_equal(validate_var_name("foo.CHR.Baz"), FALSE)
  expect_equal(validate_var_name("FOOO.CHR.Baz"), FALSE)
  expect_equal(validate_var_name("FOO.CHR.Baz"), TRUE)
})

test_that("Variable name must match R type", {
  expect_equal(validate_var_name("FOO.INT.Baz"), TRUE)
  expect_equal(validate_var_name("BAZ.BAR.Foo"), FALSE)
})

test_that("Variable name must match REDCap type", {
  expect_equal(validate_var_name("FOO.MCQ.Baz", "REDCap"), TRUE)
  expect_equal(validate_var_name("FOO.MCQ.Baz"), FALSE)
})

test_that("Variable name optional part follows casing guidelines", {
  expect_equal(validate_var_name("FOO.INT.QTNNAME.Item1"), TRUE)
  expect_equal(validate_var_name("FOO.INT.Qtnname.Item1"), TRUE)
  expect_equal(validate_var_name("FOO.INT.Qtn_name.Item1"), FALSE)
  expect_equal(validate_var_name("FOO.INT.qtnbad.Item1"), FALSE)
  expect_equal(validate_var_name("FOO.INT.qTnBaD.Item1"), FALSE)
})

test_that("Variable name last part follows casing guidelines", {
  expect_equal(validate_var_name("FOO.INT.Var_name"), TRUE)
  expect_equal(validate_var_name("FOO.INT.VAR_NAME"), TRUE)
  expect_equal(validate_var_name("FOO.INT.Var_NAME_test"), TRUE)
  expect_equal(validate_var_name("FOO.INT.VAR_name"), TRUE)
  expect_equal(validate_var_name("FOO.INT.var_name"), FALSE)
  expect_equal(validate_var_name("FOO.INT.Var_Name"), FALSE)
})

test_that("Variable name validation is vectorized", {
  expect_equal(validate_var_name(c("FOO", "BAZ", "FOO.CHR.Baz")), c(F, F, T))
})
