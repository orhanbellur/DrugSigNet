test_that("core S4 constructors enforce validity", {
  expect_error(
    methods::new(
      "DrugSearchingPipeline",
      DrugSearching = list(Raw = list(), Processed = list()),
      RankAggregation = list(),
      type = ""
    ),
    "type"
  )

  obj <- PlotObject(
    input_data = data.frame(x = 1:3),
    file_type = "pdf",
    file_name = "plot.pdf",
    width = 4,
    height = 3,
    units = "in"
  )
  expect_s4_class(obj, "PlotObject")
  expect_true(all(c("input_data", "file_type", "file_name", "width", "height", "units") %in% names(obj@parameters)))
})

test_that("manual PlotObject creation without required entries fails validity", {
  expect_error(
    methods::new("PlotObject", parameters = list(file_name = "x.pdf")),
    "required"
  )
})

test_that("pipeline subclasses support base-class coercion and replacement", {
  variants <- list(
    DrugSearchingPipeline(DrugAnnotation = list(source = "test")),
    DrugSearchingPipeline(Visualization = list(plot = "test")),
    DrugSearchingPipeline(
      DrugAnnotation = list(source = "test"),
      Visualization = list(plot = "test")
    )
  )

  replacement <- DrugSearchingPipeline(
    DrugSearching = list(Raw = list(replaced = TRUE), Processed = list()),
    RankAggregation = list(method = "test"),
    type = "network"
  )

  for (object in variants) {
    expect_true(methods::is(object, "DrugSearchingPipeline"))
    expect_s4_class(methods::as(object, "DrugSearchingPipeline"), "DrugSearchingPipeline")

    methods::as(object, "DrugSearchingPipeline") <- replacement
    expect_identical(object@DrugSearching, replacement@DrugSearching)
    expect_identical(object@RankAggregation, replacement@RankAggregation)
    expect_identical(object@type, replacement@type)
  }
})
