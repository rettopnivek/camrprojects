
chr_temp <- tempfile('testthat-', tmpdir=normalizePath(tempdir(check=TRUE)))
dir.create(chr_temp)

dirs <- list(
  a = file.path(chr_temp, 'example_dir_a'),
  b = file.path(chr_temp, 'example_dir_b'),
  c = file.path(chr_temp, 'example_dir_c'),
  d = file.path(chr_temp, 'example_dir_d'),
  e = file.path(chr_temp, 'example_dir_e'),
  f = file.path(chr_temp, 'example_dir_f')
)

for (dir in dirs[c('a', 'b', 'c')]) {
  dir.create(dir)
}

path_config <- file.path(chr_temp, 'config.yml')
file_config <- file(path_config, 'wt')

example_config <- stringr::str_glue('
default:
  local-directories:
    a: {dirs$a}
    b: {dirs$b}
    c: {dirs$c}
    d: {dirs$d}
    e: {dirs$e}
    f: {dirs$f}
')

writeLines(example_config, file_config)
close(file_config)

# Set the path to the config.yml such that config::get() can find it.
# Sys.setenv('R_CONFIG_FILE'=path_config)
setwd(chr_temp)

test_that('camr_path defaults to the working directory.', {
  expect_identical(camr_build_path(), getwd())
  expect_identical(camr_build_path('config.yml'), file.path(getwd(), 'config.yml'))
})

test_that('camr_path constructs paths as given in config.', {
  expect_identical(camr_build_path(root='a'), dirs$a)
  expect_identical(camr_build_path(root='b'), dirs$b)
  expect_identical(camr_build_path(root='c'), dirs$c)
  expect_identical(camr_build_path(root='d', lgl_verify=F), dirs$d)
  expect_identical(camr_build_path(root='e', lgl_verify=F), dirs$e)
  expect_identical(camr_build_path(root='f', lgl_verify=F), dirs$f)
})

test_that('camr_path handles nonexistant paths.', {
  expect_error(camr_build_path(root='d'))
  expect_error(camr_build_path(root='e'))
  expect_error(camr_build_path(root='f'))

  expect_identical(camr_build_path(root='d', lgl_verify=F, lgl_create=T), dirs$d)
  expect_identical(camr_build_path(root='e', lgl_verify=F, lgl_create=T), dirs$e)
  expect_identical(camr_build_path(root='f', lgl_verify=F, lgl_create=T), dirs$f)

  expect_identical(camr_build_path(root='d'), dirs$d)
  expect_identical(camr_build_path(root='e'), dirs$e)
  expect_identical(camr_build_path(root='f'), dirs$f)

  expect_true(dir.exists(dirs$d))
  expect_true(dir.exists(dirs$e))
  expect_true(dir.exists(dirs$f))
})

