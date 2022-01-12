dirs <- list(
  project = fs::path_temp('project'),
  a = fs::path_temp('example_dir_a'),
  b = fs::path_temp('example_dir_b'),
  c = fs::path_temp('example_dir_c'),
  d = fs::path_temp('example_dir_d'),
  e = fs::path_temp('example_dir_e'),
  f = fs::path_temp('example_dir_f')
)

unlink(unlist(dirs), recursive = TRUE)
fs::dir_create(dirs[1:5]) # Do not create directories e or f.

path_config <- fs::path_join(c(dirs$project, 'config.yml'))
file_config <- file(path_config, 'wt')

example_config <- stringr::str_glue('
default:
  input-root:
    a: {dirs$a}
    b: {dirs$b}
  input-root-default: a
  output-root:
    c: {dirs$c}
    d: {dirs$d}
    e: {dirs$e}
    f: {dirs$f}
  output-root-default: d
')

writeLines(example_config, file_config)
close(file_config)

# Set the path to the config.yml such that config::get() can find it.
Sys.setenv('R_CONFIG_FILE'=path_config)

test_that('camr_path constructs paths as given in config.', {
  expect_identical(camr_path('input'), fs::path_real(dirs$a))
  expect_identical(camr_path('input', 'a'), fs::path_real(dirs$a))
  expect_identical(camr_path('input', 'b'), fs::path_real(dirs$b))

  expect_identical(camr_path('output'), fs::path_real(dirs$d))
  expect_identical(camr_path('output', 'c'), fs::path_real(dirs$c))
  expect_identical(camr_path('output', 'd'), fs::path_real(dirs$d))
})

test_that('camr_path throws error on invalid prefix or invalid root.', {
  expect_error(camr_path())
  expect_error(camr_path(''))

  expect_error(camr_path('input', 'c'))
  expect_error(camr_path('input', 'd'))
  expect_error(camr_path('input', ''))

  expect_error(camr_path('output', 'a'))
  expect_error(camr_path('output', 'b'))
  expect_error(camr_path('output', ''))
})


test_that('camr_path handles nonexistant paths.', {
  expect_error(camr_path('output', 'e'))
  expect_error(camr_path('output', 'f'))

  expect_identical(camr_path('output', 'e', real=FALSE), dirs$e)
  expect_identical(camr_path('output', 'f', real=FALSE, create=TRUE), dirs$f)
  expect_true(fs::dir_exists(dirs$f))
})

