test_that("native Windows network guard warns and reports unavailable", {
  expect_warning(
    available <- .drugsignet_network_methods_available(os_type = "windows"),
    paste(
      "Network-based methods require the Python graph-tool library",
      "Please use the DrugSigNet Docker image or Windows Subsystem",
      sep = ".*"
    )
  )
  expect_false(available)
  expect_true(.drugsignet_network_methods_available(os_type = "unix"))
})

test_that("all graph-tool network entry points apply the Windows guard", {
  pipeline_body <- paste(deparse(body(drugNetworkPipeline)), collapse = "\n")
  expect_match(pipeline_body, ".drugsignet_network_methods_available", fixed = TRUE)

  methods <- c(
    "TrustRank", "Degree_centrality", "Harmonic_centrality",
    "Diffusion", "Network_proximity"
  )
  for (method_name in methods) {
    generic_body <- paste(deparse(body(getGeneric(method_name))), collapse = "\n")
    expect_match(
      generic_body,
      ".drugsignet_network_methods_available",
      fixed = TRUE,
      info = method_name
    )
  }
})
