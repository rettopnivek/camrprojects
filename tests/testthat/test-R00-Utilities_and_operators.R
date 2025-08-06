
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

#Testing that variable assigns, doesn't just output result
x = 0
z = x %+=% 7
test_that("%+=% works normally", {
  expect_equal(x, 7)
  expect_equal(z, 7)
  expect_error(7 %+=% 7, "left-hand side to assignment")
})

#Testing that variable assigns, doesn't just output result
x = 0
z = x %-=% 7
test_that("%-=% works normally", {
  expect_equal(x, -7)
  expect_equal(z, -7)
  expect_error(7 %-=% 7, "left-hand side to assignment")
})

x = c(1, 2, 3)
z = x %+=% 7
test_that("%+=% works with vectors", {
  expect_equal(x, c(8, 9, 10))
  expect_equal(z, c(8, 9, 10))
})

x = c(7, 8, 9)
z = x %-=% 7
test_that("%-=% works with vectors", {
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
  expect_equal(c(1, 2, 3) %pm% 1, c(TRUE, FALSE, FALSE))
  expect_equal(c("hey", "hello", "goodbye") %pm% "he", c(TRUE, TRUE, FALSE))
})

test_that("%pm% is robust against NAs", {
  expect_equal(c("hello", NA, "world") %pm% "he", c(TRUE, FALSE, FALSE))
  expect_equal(c("test", "tester", NA) %pm% NA, c(FALSE, FALSE, TRUE))
})

test_that("%em% works", {
  expect_equal(c(1, 2, 3) %em% 1, c(TRUE, FALSE, FALSE))
  expect_equal(c("hey", "he", "goodbye") %em% "he", c(FALSE, TRUE, FALSE))
})

test_that("%em% is robust against NAs", {
  expect_equal(c("hello", NA, "he") %em% "he", c(FALSE, FALSE, TRUE))
  expect_equal(c("test", "NAtrium", NA) %em% NA, c(FALSE, FALSE, TRUE))
})


#Testing %row%
dtf <- data.frame(X1 = 1:4, X2 = LETTERS[1:4], X3 = c( TRUE, TRUE, FALSE, FALSE ))
attributes( dtf$X1 ) <- list( Example_attr = "Integer" )
attributes( dtf$X2 ) <- list( Example_attr = "Character" )
attributes( dtf$X3 ) <- list( Example_attr = "Logical" )
dtf_subset_1 = dtf %rows% 1:2
dtf_subset_2 = dtf %rows% c("X2", "X3")


test_that("%row% works", {
  expect_equal(attributes(dtf_subset_1$X1), attributes(dtf$X1))
  expect_equal(attributes(dtf_subset_2$X2), attributes(dtf$X2))
  #Checks whether contents are the same by removing attributes
  expect_equal(dtf_subset_1[1:2,], dtf[1:2,])
  expect_equal(dtf_subset_2[c("X2", "X3"),], dtf[c("X2","X3"),])
})
