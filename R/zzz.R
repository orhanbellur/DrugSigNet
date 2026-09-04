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
    packageStartupMessage("DrugSigNet will attempt automatic Python dependency setup on attach (graph_tool is skipped by default).")
    .drugsignet_maybe_auto_install_python()
  } else {
    packageStartupMessage(
      "Python dependency auto setup is disabled by default. ",
      "Run setup_python_dependencies() manually when you need Python-backed methods, ",
      "or set options(DrugSigNet.auto_install_python = TRUE) before library(DrugSigNet)."
    )
  }

  if (!.drugsignet_synapser_available()) {
    packageStartupMessage(
      "Synapse support will be installed on first use. Run setup_synapser() now, ",
      "or set options(DrugSigNet.auto_install_synapser = FALSE) to disable this."
    )
  }
}
