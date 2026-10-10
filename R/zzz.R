.onAttach <- function(libname, pkgname) {
  logo <- "
  ***********************************************************
   _____                    _____ _       _   _      _
  |  __ \\                  / ____(_)     | \\ | |    | |
  | |  | |_ __ _   _  __ _| (___  _  __ _|  \\| | ___| |_
  | |  | | '__| | | |/ _` |\\___ \\| |/ _` | . ` |/ _ \\ __|
  | |__| | |  | |_| | (_| |____) | | (_| | |\\  |  __/ |_
  |_____/|_|   \\__,_|\\__, |_____/|_|\\__, |_| \\_|\\___|\\__|
                      __/ |          __/ |
                     |___/          |___/
  ***********************************************************
  "

  message <- "
  ***********************************************************
  Welcome to DrugSigNet!

  This package provides tools for drug repurposing using
  signature- and network-based approaches. Start with:
    - ?drugSignaturePipeline for signature-based analysis
    - ?drugNetworkPipeline for network-based analysis

  Visit https://github.com/compneurobio/DrugSigNet for more.
  ***********************************************************
  "

  packageStartupMessage(paste(logo, message, sep = "\n"))

  if (.drugsignet_auto_install_enabled()) {
    if (.drugsignet_is_windows()) {
      packageStartupMessage(
        "DrugSigNet will ensure its supported conda Python environment is available; ",
        "graph_tool and network-based methods are unavailable on native Windows."
      )
    } else {
      packageStartupMessage(
        "DrugSigNet will ensure its conda Python environment, including graph_tool, is available."
      )
    }
    .drugsignet_maybe_auto_install_python()
  } else {
    packageStartupMessage(
      "Python dependency auto setup has been disabled. ",
      "Run setup_python_dependencies() manually when you need Python-backed methods, ",
      "or set options(DrugSigNet.auto_install_python = TRUE) before library(DrugSigNet)."
    )
  }

  packageStartupMessage(
    "Synapse operations use the isolated Python client in the DrugSigNet conda environment."
  )
}
