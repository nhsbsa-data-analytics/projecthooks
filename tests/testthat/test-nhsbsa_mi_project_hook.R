test_that("mi_dash works", {
  tmpdir <- tempdir()
  path <- fs::dir_create(tmpdir, "testdir")
  withr::local_dir(path)

  fs::dir_create("dev")
  fs::dir_create("inst")
  fs::dir_create("man")
  fs::dir_create("R")
  fs::file_create(".Rbuildignore")
  fs::file_create(".gitignore")
  fs::file_create("DESCRIPTION")
  fs::file_create("NAMESPACE")

  mi_dash(path = "", package_name = "")

  expect_equal(file.exists("dev"), FALSE)
  expect_equal(file.exists("inst"), FALSE)
  expect_equal(file.exists("man"), FALSE)
  expect_equal(file.exists("R"), FALSE)
  expect_equal(file.exists(".Rbuildignore"), FALSE)
  expect_equal(file.exists(".gitignore"), FALSE)
  # Temporarily do not delete DESCRIPTION due to error when trying to set the
  # active project. May need to handle this by customising the create_golem and/or
  # create_package functions :(
  # expect_equal(file.exists("DESCRIPTION"), FALSE)
  expect_equal(file.exists("NAMESPACE"), FALSE)
  expect_equal(file.exists("data"), TRUE)
  expect_equal(file.exists("ui"), TRUE)
  expect_equal(file.exists("data-raw"), TRUE)
  expect_equal(file.exists("global"), TRUE)
  expect_equal(file.exists("server"), TRUE)
  expect_equal(file.exists("www"), TRUE)
})
