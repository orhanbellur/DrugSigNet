#' @title Download and Cache DrugSigNet Network Data
#'
#' @description
#' Downloads DrugSigNet network resources from Synapse and caches them in the
#' user cache directory.
#'
#' @details
#' `load_drugsignet_network()` retrieves DrugSigNet network resources used by
#' network-based drug repurposing methods, including the drug-target interaction
#' network and the integrated gene-gene interaction network.
#'
#' Cached files are stored under `tools::R_user_dir("DrugSigNet", "cache")`.
#' When a Synapse authentication token is available, the function checks the
#' remote Synapse file version and updates the local cache when needed. If no
#' token is available but a cached file already exists, the cached file is used
#' without a remote version check.
#'
#' Use `force = TRUE` to redownload the selected network from Synapse. This
#' requires a valid Synapse authentication token.
#'
#' @param network Network resource to load. One of `"drug_target"`,
#'   `"gene_gene"`, or `"all"`.
#' @param force Logical; if `TRUE`, redownload the selected network even when a
#'   cached version is available or current. Default is `FALSE`.
#' @param auth_token Optional Synapse authentication token. If `NULL`, the
#'   `SYNAPSE_AUTH_TOKEN` environment variable is used.
#'
#' @return
#' A data frame when a single network is requested, or a named list of data
#' frames when `network = "all"`.
#'
#' @examples
#' \dontrun{
#' # Load the default drug-target network
#' drug_target <- load_drugsignet_network(
#'   network = "drug_target",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' # Load the default gene-gene interaction network
#' gene_gene <- load_drugsignet_network(
#'   network = "gene_gene",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' # Load all available network resources
#' networks <- load_drugsignet_network(
#'   network = "all",
#'   auth_token = Sys.getenv("SYNAPSE_AUTH_TOKEN")
#' )
#'
#' # Use an existing local cache without providing a token
#' cached_drug_target <- load_drugsignet_network("drug_target")
#' }
#'
#' @export
load_drugsignet_network <- function(network = c("drug_target", "gene_gene", "all"),
                                    force = FALSE,
                                    auth_token = NULL) {
  network <- match.arg(network)
  force <- .pipeline_flag(force, "force")

  synapse_ids <- c(
    drug_target = "syn75862040",
    gene_gene = "syn75862041"
  )

  file_names <- c(
    drug_target = "drug_target_network.rds",
    gene_gene = "integrated_gene_network.rds"
  )

  networks_to_load <- if (identical(network, "all")) names(synapse_ids) else network

  cache_dir <- file.path(
    tools::R_user_dir("DrugSigNet", which = "cache"),
    "network_data"
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  if (is.null(auth_token)) {
    auth_token <- Sys.getenv("SYNAPSE_AUTH_TOKEN")
  }
  has_auth_token <- !identical(auth_token, "")

  if (isTRUE(force) && !has_auth_token) {
    stop(
      "force = TRUE requires a Synapse authentication token.\n\n",
      "Use load_drugsignet_network(network = '", network, "', auth_token = 'your_token_here')\n",
      "or save the token in ~/.Renviron as SYNAPSE_AUTH_TOKEN=your_token_here.",
      call. = FALSE
    )
  }

  if (has_auth_token) {
    .drugsignet_require_synapser("download DrugSigNet network data")
    .drugsignet_synapser_function("synLogin")(authToken = auth_token)
  }

  load_one_network <- function(network_name) {
    synapse_data_id <- unname(synapse_ids[[network_name]])
    local_file <- file.path(cache_dir, unname(file_names[[network_name]]))
    meta_file <- file.path(
      cache_dir,
      paste0(tools::file_path_sans_ext(unname(file_names[[network_name]])), "_metadata.rds")
    )

    if (!has_auth_token && file.exists(local_file) && !force) {
      message(
        "Using cached DrugSigNet network data without remote version check: ",
        local_file,
        " | network: ", network_name
      )
      return(readRDS(local_file))
    }

    if (!has_auth_token) {
      stop(
        "No Synapse authentication token was provided and no local cache exists for '",
        network_name,
        "'.\n\nUse:\n",
        "  load_drugsignet_network(network = '", network_name, "', auth_token = 'your_token_here')\n\n",
        "or save the token in ~/.Renviron as:\n",
        "  SYNAPSE_AUTH_TOKEN=your_token_here",
        call. = FALSE
      )
    }

    syn_meta <- .drugsignet_synapser_function("synGet")(synapse_data_id, downloadFile = FALSE)
    remote_version <- as.integer(syn_meta$properties$versionNumber)
    remote_id <- as.character(syn_meta$properties$id)
    remote_name <- as.character(syn_meta$properties$name)

    local_meta <- NULL
    if (file.exists(meta_file)) {
      local_meta <- readRDS(meta_file)
    }

    cache_is_current <-
      file.exists(local_file) &&
      !is.null(local_meta) &&
      identical(local_meta$synapse_id, remote_id) &&
      identical(as.integer(local_meta$versionNumber), remote_version)

    if (cache_is_current && !force) {
      message(
        "Using cached DrugSigNet network data: ", local_file,
        " | network: ", network_name,
        " | Synapse version: ", remote_version
      )
      return(readRDS(local_file))
    }

    if (!file.exists(local_file)) {
      message("No local cache found. Downloading ", network_name, " network from Synapse.")
    } else if (!cache_is_current) {
      message(
        "Synapse network data has changed. Updating local cache from version ",
        if (!is.null(local_meta$versionNumber)) local_meta$versionNumber else "unknown",
        " to version ", remote_version, "."
      )
    } else if (force) {
      message("force = TRUE. Redownloading ", network_name, " network from Synapse.")
    }

    if (file.exists(local_file)) {
      unlink(local_file)
    }

    syn_file <- .drugsignet_synapser_function("synGet")(
      synapse_data_id,
      downloadLocation = cache_dir,
      ifcollision = "overwrite.local"
    )

    downloaded_path <- tryCatch(syn_file$path, error = function(e) NULL)
    if (is.null(downloaded_path) || !file.exists(downloaded_path)) {
      stop(
        "The Synapse file was not downloaded.\n\n",
        "Check that ", synapse_data_id,
        " is the correct file ID and that your account has DOWNLOAD permission.",
        call. = FALSE
      )
    }

    downloaded_path_norm <- normalizePath(downloaded_path, mustWork = TRUE)
    local_file_norm <- normalizePath(local_file, mustWork = FALSE)
    if (!identical(downloaded_path_norm, local_file_norm)) {
      ok <- file.copy(downloaded_path, local_file, overwrite = TRUE)
      if (!ok || !file.exists(local_file)) {
        stop(
          "Downloaded file could not be copied to the local cache path:\n",
          local_file,
          call. = FALSE
        )
      }
    }

    saveRDS(
      list(
        network = network_name,
        synapse_id = remote_id,
        synapse_name = remote_name,
        versionNumber = remote_version,
        downloaded_at = as.character(Sys.time()),
        local_file = local_file,
        file_size = file.info(local_file)$size
      ),
      meta_file
    )

    message(
      "DrugSigNet network data cached at: ", local_file,
      " | network: ", network_name,
      " | Synapse version: ", remote_version,
      " | size: ", round(file.info(local_file)$size / 1024^2, 2), " MB"
    )

    readRDS(local_file)
  }

  res <- lapply(networks_to_load, load_one_network)
  names(res) <- networks_to_load

  if (!identical(network, "all")) {
    return(res[[1]])
  }

  res
}
