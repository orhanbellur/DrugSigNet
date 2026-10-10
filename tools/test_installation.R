package_ref <- Sys.getenv("DRUGSIGNET_PACKAGE_REF", "orhanbellur/DrugSigNet")
python <- Sys.which("python")
if (!nzchar(python)) stop("Pixi environment does not contain Python.")

Sys.setenv(
  RETICULATE_PYTHON = python,
  DRUGSIGNET_SKIP_RUNTIME_SETUP = "true",
  PKG_SYSREQS = "false",
  PKG_BUILD_VIGNETTES = "true"
)
options(
  timeout = max(1200, getOption("timeout", 60)),
  pkg.sysreqs = FALSE,
  pkg.build_vignettes = TRUE
)

pak::pkg_install(package_ref, dependencies = NA, upgrade = FALSE, ask = FALSE)
check <- DrugSigNet::check_drugsignet_installation(
  check_python = TRUE,
  check_optional = FALSE,
  stop_on_error = FALSE
)
if (!isTRUE(check$ok)) {
  print(check$packages[!check$packages$available, , drop = FALSE])
  print(check$functions[!check$functions$available, , drop = FALSE])
  print(check$python[!check$python$available, , drop = FALSE])
  stop("DrugSigNet Pixi validation failed; missing components are printed above.")
}
message("DrugSigNet installation and its Pixi runtime were validated successfully.")
