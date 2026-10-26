test_that("configured graph-tool preflight never embeds Python in R", {
  local_mocked_bindings(
    .drugsignet_select_python = function(...) stop("must not initialize reticulate"),
    .drugsignet_python_module_probe = function(python, modules) {
      expect_identical(python, "/configured/python")
      expect_identical(modules, "graph_tool")
      list(available = TRUE, output = character())
    }, .package = "DrugSigNet"
  )
  expect_true(DrugSigNet:::.drugsignet_use_configured_graph_tool("/configured/python"))
})

test_that("failed standalone graph import reports the configured runtime diagnostic", {
  local_mocked_bindings(.drugsignet_python_module_probe = function(...) {
    list(available = FALSE, output = "ImportError: missing GLIBCXX_test")
  }, .package = "DrugSigNet")
  expect_error(DrugSigNet:::.drugsignet_use_configured_graph_tool("/broken/python"),
    "own runtime: /broken/python.*GLIBCXX_test")
})

test_that("worker transport preserves identifiers, nonfinite values and native target lists", {
  python <- tempfile(); file.create(python)
  on.exit(unlink(python), add = TRUE)
  withr::local_envvar(c(PYTHONHASHSEED = NA))
  request_path <- NULL
  label <- paste0("0001-NA-", paste(rep("long", 100), collapse = ""), "-\u03b2")
  local_mocked_bindings(
    .drugsignet_discover_pixi_python = function() python,
    .drugsignet_python_system2 = function(python, args, extra_env, ...) {
      expect_identical(extra_env, "PYTHONHASHSEED=0")
      # system2 arguments are shell-quoted by the bridge.
      unquote <- function(x) gsub("^['\"]|['\"]$", "", x)
      request_path <<- unquote(args[3])
      request <- jsonlite::fromJSON(request_path, simplifyVector = FALSE)
      input <- utils::read.csv(request$args$input$path, colClasses = "character",
        check.names = FALSE, na.strings = request$na_value, fileEncoding = "UTF-8")
      expect_identical(input$Drug, c("NA", "0001", NA_character_))
      path <- file.path(dirname(request_path), "output.csv")
      result <- data.frame(Drug = c("NA", label, "0001"), Score = c("Inf", NA_character_, "NaN"),
        Targets = c('["001","NA"]', '[]', '["G1"]'))
      utils::write.table(result, path, sep = ",", quote = c(1L, 3L), qmethod = "double", row.names = FALSE,
        na = request$na_value, fileEncoding = "UTF-8")
      jsonlite::write_json(list(ok = TRUE, kind = "data.frame", path = path,
        types = list(Drug = "string", Score = "number", Targets = "list")),
        unquote(args[5]), auto_unbox = TRUE)
      character()
    }, .package = "DrugSigNet"
  )
  result <- DrugSigNet:::.drugsignet_network_python_call("test", "fixture", list(
    input = data.frame(Drug = c("NA", "0001", NA_character_))))
  expect_identical(result$Drug, c("NA", label, "0001"))
  expect_true(is.infinite(result$Score[1]))
  expect_true(is.na(result$Score[2]))
  expect_false(is.nan(result$Score[2]))
  expect_true(is.nan(result$Score[3]))
  expect_identical(result$Targets[[1]], c("001", "NA"))
  expect_length(result$Targets[[2]], 0)
  expect_identical(result$Targets[[3]], "G1")
  expect_false(file.exists(request_path))
})

test_that("worker errors cannot return a stale or partial result", {
  python <- tempfile(); file.create(python)
  on.exit(unlink(python), add = TRUE)
  local_mocked_bindings(
    .drugsignet_discover_pixi_python = function() python,
    .drugsignet_python_system2 = function(...) structure("native module failure", status = 1L),
    .package = "DrugSigNet"
  )
  expect_error(DrugSigNet:::.drugsignet_network_python_call("test", "fixture", list()),
    "Network Python worker failed.*native module failure")
})

test_that("malformed worker responses fail with context instead of partial tables", {
  python <- tempfile(); file.create(python)
  on.exit(unlink(python), add = TRUE)
  local_mocked_bindings(
    .drugsignet_discover_pixi_python = function() python,
    .drugsignet_python_system2 = function(python, args, ...) {
      response <- gsub("^['\"]|['\"]$", "", args[5])
      writeLines("{", response)
      character()
    }, .package = "DrugSigNet"
  )
  expect_error(DrugSigNet:::.drugsignet_network_python_call("test", "fixture", list()),
    "Network Python worker failed.*Invalid worker response")
})

test_that("an already initialized interpreter is probed outside R as well", {
  local_mocked_bindings(
    .drugsignet_discover_pixi_python = function() "",
    .drugsignet_python_has_modules = function(python, modules) {
      expect_identical(python, "/already/initialized/python")
      expect_identical(modules, "graph_tool")
      TRUE
    },
    setup_python_dependencies = function(...) stop("must not provision another runtime"),
    .package = "DrugSigNet"
  )
  local_mocked_bindings(
    py_available = function(...) TRUE,
    py_config = function() list(python = "/already/initialized/python"),
    py_module_available = function(...) stop("must not import graph_tool into host R"),
    .package = "reticulate"
  )
  expect_true(DrugSigNet:::.drugsignet_require_graph_tool())
})
