#' Project hook to create a base dashboard app for the NHSBSA MI team
#'
#' This will create an app skeleton as a starting point for new dashboards built
#' by the Management Information team. Since it is the function itself which is
#' provided to `golem`, should never need to call explicitly - the arguments are
#' set as required by `golem::create_golem`. Even if the arguments are not actually
#' used in the project hook, they must be available for `golem`.
#'
#' @param path Path to the package
#' @param package_name Name of the package (aka the app or dashboard)
#' @param ... Never used
#'
#' @import R.utils
#' @import fs
#' @import withr
#'
#' @export
nhsbsa_mi_project_hook <- function(path, package_name, ...) {
  # Delete all files except .Rproj
  fs::dir_delete("dev")
  fs::dir_delete("inst")
  fs::dir_delete("man")
  fs::dir_delete("R")
  fs::file_delete(".Rbuildignore")
  fs::file_delete(".gitignore")
  fs::file_delete("NAMESPACE")
  # Temporarily do not delete DESCRIPTION due to error when trying to set the
  # active project. May need to handle this by customising the create_golem and/or
  # create_package functions :(
  # fs::file_delete("DESCRIPTION") # Exclude Linting

  fs::dir_create("data")
  fs::dir_create("ui")

  # Copy standard files into new project folder
  R.utils::copyDirectory(
    system.file(
      "projects", "mi_dash", "standard_files",
      package = "projecthooks"
    ),
    "."
  )
}
