#' List-or-matrix class union
#'
#' An internal S4 class union allowing a slot to contain either a list or a
#' matrix.
#'
#' @importFrom methods setClass
#' @exportClass listOrMat

# Define a union for list and matrix types
setClassUnion("listOrMat", c("list", "matrix"))
setClassUnion("listOrNULL", c("list", "NULL"))

# Define the DrugSearching class
setClass("DrugSearching",
         slots = list(result = "data.frame")
)

setValidity("DrugSearching", function(object) {
  if (!is.data.frame(object@result)) {
    return("`result` slot must be a data.frame.")
  }
  TRUE
})

# Define the SignatureBased class that extends DrugSearching
setClass(
  "SignatureBased",
  contains = "DrugSearching",
  slots = list(
    result = "data.frame", # Results from the signature-based analysis
    parameters = "list" # Parameters for the signature-based method
  )
)

setValidity("SignatureBased", function(object) {
  if (!is.list(object@parameters)) {
    return("`parameters` slot must be a list.")
  }
  TRUE
})


# Constructor function for SignatureBased
SignatureBased <- function(
    result = data.frame(), # Analysis result
    query = NULL, # Query data (list or matrix)
    signature_method = "CMAP", # Signature-based method
    refdb = "", # Reference database
    higher = NULL, # Optional higher cutoff
    lower = NULL, # Optional lower cutoff
    padj = NULL, # Optional adjusted p-value threshold
    tau = FALSE, # Logical: use tau or not
    sortby = "WTCS", # Sorting criterion (default: "WTCS")
    GeneType = "reference", # Gene type (default: "reference")
    chunk_size = 5000, # Chunk size for processing
    method = "" # Additional method-specific parameter
) {
  # Validate `result`
  if (!is.data.frame(result)) {
    stop("`result` must be a data frame.")
  }

  # Validate `query`
  if (!is.null(query) && !is.list(query) && !is.matrix(query)) {
    stop("`query` must be NULL, a list, or a matrix.")
  }

  # Validate `signature_method`
  if (!is.character(signature_method) || length(signature_method) != 1) {
    stop("`signature_method` must be a single character string.")
  }

  # Validate `refdb`
  if (!is.character(refdb) || length(refdb) != 1) {
    stop("`refdb` must be a single character string.")
  }

  # Validate `chunk_size`
  if (!is.numeric(chunk_size) || length(chunk_size) != 1 || chunk_size <= 0) {
    stop("`chunk_size` must be a positive numeric value.")
  }

  # Validate additional parameters
  if (!is.null(higher) && (!is.numeric(higher) || length(higher) != 1)) {
    stop("`higher` must be a single numeric value if provided.")
  }
  if (!is.null(lower) && (!is.numeric(lower) || length(lower) != 1)) {
    stop("`lower` must be a single numeric value if provided.")
  }
  if (!is.null(padj) && (!is.numeric(padj) || length(padj) != 1 || padj <= 0 || padj > 1)) {
    stop("`padj` must be a numeric value between 0 and 1 if provided.")
  }
  if (!is.logical(tau) || length(tau) != 1) {
    stop("`tau` must be a single logical value (TRUE or FALSE).")
  }
  if (!is.character(sortby) || length(sortby) != 1) {
    stop("`sortby` must be a single character string.")
  }
  if (!is.character(GeneType) || length(GeneType) != 1) {
    stop("`GeneType` must be a single character string.")
  }
  if (!is.character(method) || length(method) != 1) {
    stop("`method` must be a single character string.")
  }

  # Prepare the parameters list
  parameters <- list(
    query = query,
    signature_method = signature_method,
    refdb = refdb,
    higher = higher,
    lower = lower,
    padj = padj,
    tau = tau,
    sortby = sortby,
    GeneType = GeneType,
    chunk_size = chunk_size,
    method = method
  )

  # Create and return the SignatureBased object
  new(
    "SignatureBased",
    result = result,
    parameters = parameters
  )
}

filterSignatureParameters <- function(object) {
  # Extract the signature method
  method <- object@parameters$signature_method

  # Define fields to keep in the `parameters` slot for each signature method
  parameter_fields <- list(
    CMAP = c("query", "signature_method", "refdb", "chunk_size"),
    LINCS = c("query", "signature_method", "refdb", "tau", "sortby", "GeneType", "chunk_size"),
    gCMAP = c("query", "signature_method", "refdb", "higher", "lower", "padj", "chunk_size"),
    Correlation = c("query", "signature_method", "refdb", "chunk_size", "method")
  )

  # Check if the method exists
  if (!method %in% names(parameter_fields)) {
    stop(sprintf("Unknown signature method: %s", method))
  }

  # Filter the `parameters` slot based on the signature method
  filtered_parameters <- object@parameters[names(object@parameters) %in% parameter_fields[[method]]]

  return(filtered_parameters)
}


#' Create a SignatureBasedDrugSearching Object
#'
#' A helper function to create an instance of the `SignatureBasedDrugSearching` class.
#'
#' @param signature_result A `data.frame` containing the results of the search.
#' @param query A `list` containing the up-regulated (`upset`) and down-regulated (`downset`) gene sets.
#' @param signature_method A `character` string representing the method used (e.g., "CMAP").
#' @param refdb A `character` string specifying the reference database used (e.g., "cmap").
#'
#' @return A `SignatureBasedDrugSearching` object.
#' @importFrom methods new
#' @export
setClass(
  "NetworkBased",
  contains = "DrugSearching",
  slots = list(
    result = "data.frame", # Analysis result
    parameters = "list" # Parameters for the analysis
  )
)

setValidity("NetworkBased", function(object) {
  if (!is.list(object@parameters)) {
    return("`parameters` slot must be a list.")
  }
  TRUE
})


NetworkBased <- function(
    result = data.frame(), # Analysis result
    ppi_network = data.frame(), # Protein-Protein Interaction network
    drug_target_network = data.frame(), # Drug-target interaction network
    disease_genes = data.frame(), # Disease-associated genes
    # Centrality-specific parameters
    target = "drug",
    include_indirect_drugs = TRUE,
    include_non_approved_drugs = TRUE,
    filter_paths = TRUE,
    hub_penalty = 0.01,
    damping_factor = 0.95,
    max_deg = NULL,
    result_size = NULL,
    # RandomWalk-specific parameters
    n_simulations = 1000L,
    ncpus = 1L,
    start = 0L,
    end = NULL,
    random_seed = 42L,
    ties_method = "max",
    output_dir = tempdir(),
    # Additional method parameter
    method = "") {
  # Validate inputs
  if (!is.data.frame(result)) stop("`result` must be a data frame.")
  if (!is.data.frame(ppi_network)) stop("`ppi_network` must be a data frame.")
  if (!is.data.frame(drug_target_network)) stop("`drug_target_network` must be a data frame.")
  if (!is.data.frame(disease_genes)) stop("`disease_genes` must be a data frame.")
  if (!is.character(method) || length(method) != 1) stop("`method` must be a single character string.")

  # Prepare parameters as a list
  parameters <- list(
    ppi_network = ppi_network,
    drug_target_network = drug_target_network,
    disease_genes = disease_genes,
    target = target,
    include_indirect_drugs = include_indirect_drugs,
    include_non_approved_drugs = include_non_approved_drugs,
    filter_paths = filter_paths,
    hub_penalty = hub_penalty,
    damping_factor = damping_factor,
    max_deg = if (is.null(max_deg)) NULL else as.integer(max_deg),
    result_size = if (is.null(result_size)) NULL else as.integer(result_size),
    n_simulations = as.integer(n_simulations),
    ncpus = as.integer(ncpus),
    start = as.integer(start),
    end = if (is.null(end)) NULL else as.integer(end),
    random_seed = random_seed,
    ties_method = ties_method,
    output_dir = output_dir,
    method = method
  )

  # Create the object
  new(
    "NetworkBased",
    result = result,
    parameters = parameters
  )
}


filterNetworkParameters <- function(object) {
  # Extract the method from the object parameters
  method <- object@parameters$method

  # Define fields to keep for each method
  parameter_fields <- list(
    TrustRank = c(
      "ppi_network", "drug_target_network", "disease_genes", "method", "target",
      "include_indirect_drugs", "include_non_approved_drugs", "filter_paths",
      "hub_penalty", "damping_factor", "max_deg", "result_size"
    ),
    Harmonic_centrality = c(
      "ppi_network", "drug_target_network", "disease_genes", "method", "target",
      "include_indirect_drugs", "include_non_approved_drugs", "filter_paths",
      "hub_penalty", "max_deg", "result_size"
    ),
    Degree_centrality = c(
      "ppi_network", "drug_target_network", "disease_genes", "method", "target",
      "include_indirect_drugs", "include_non_approved_drugs", "filter_paths",
      "max_deg", "result_size"
    ),
    Network_proximity = c(
      "ppi_network", "drug_target_network", "disease_genes", "method",
      "include_indirect_drugs", "include_non_approved_drugs", "n_simulations",
      "ncpus", "start", "end", "random_seed"
    ),
    Diffusion = c(
      "ppi_network", "drug_target_network", "disease_genes", "method",
      "include_indirect_drugs", "include_non_approved_drugs", "ties_method", "output_dir"
    )
  )

  # Check if the method exists
  if (!method %in% names(parameter_fields)) {
    stop(sprintf("Unknown method: %s", method))
  }

  # Filter the `parameters` slot based on the method
  filtered_parameters <- object@parameters[names(object@parameters) %in% parameter_fields[[method]]]

  return(filtered_parameters)
}




#' RankAggregation Class
#'
#' A class to store results of rank aggregation analysis.
#'
setClass(
  "RankAggregation",
  slots = list(
    result = "data.frame", # Aggregated ranking results
    parameters = "list" # Parameters for the CRank algorithm
  )
)

setValidity("RankAggregation", function(object) {
  if (!is.list(object@parameters)) {
    return("`parameters` slot must be a list.")
  }
  TRUE
})
RankAggregation <- function(
    result = data.frame(), # Aggregation result
    input_data = data.frame(), # Input data for aggregation
    rank_aggregation = "CRank", # Aggregation method
    ties_method = "max", # Handling of ties
    prior = 0.093, # Prior probability
    num_bin = 200, # Number of bins
    num_iter = 1000, # Number of iterations
    reverse = FALSE, # Reverse ranking order
    full = TRUE, # Return full ranking details
    exact = FALSE # Exact ranking computation
) {
  # Validate inputs
  if (!is.data.frame(result)) stop("`result` must be a data frame.")
  if (!is.data.frame(input_data)) stop("`input_data` must be a data frame.")
  if (!is.character(rank_aggregation) || length(rank_aggregation) != 1) {
    stop("`rank_aggregation` must be a single character string.")
  }
  if (!is.character(ties_method) || length(ties_method) != 1) {
    stop("`ties_method` must be a single character string.")
  }
  if (!is.numeric(prior) || length(prior) != 1) stop("`prior` must be a single numeric value.")
  if (!is.numeric(num_bin) || length(num_bin) != 1) stop("`num_bin` must be a single numeric value.")
  if (!is.numeric(num_iter) || length(num_iter) != 1) stop("`num_iter` must be a single numeric value.")
  if (!is.logical(reverse) || length(reverse) != 1) stop("`reverse` must be a single logical value.")
  if (!is.logical(full) || length(full) != 1) stop("`full` must be a single logical value.")
  if (!is.logical(exact) || length(exact) != 1) stop("`exact` must be a single logical value.")

  # Prepare parameters
  parameters <- list(
    input_data = input_data,
    rank_aggregation = rank_aggregation,
    ties_method = ties_method,
    prior = prior,
    num_bin = num_bin,
    num_iter = num_iter,
    reverse = reverse,
    full = full,
    exact = exact,
    converged = NULL,
    correlation = NULL
  )

  # Create the object
  new(
    "RankAggregation",
    result = result,
    parameters = parameters
  )
}
filterParametersByMethod <- function(object) {
  # Extract the rank_aggregation method
  method <- object@parameters$rank_aggregation

  # Define fields to keep in the `parameters` slot for each method
  parameter_fields <- list(
    CRank = c("input_data", "rank_aggregation", "ties_method", "prior", "num_bin", "num_iter", "reverse", "converged", "correlation"),
    RRA = c("input_data", "rank_aggregation", "ties_method", "full", "exact", "reverse"),
    Dowdall = c("input_data", "rank_aggregation", "ties_method", "reverse")
  )

  # Check if the method exists
  if (!method %in% names(parameter_fields)) {
    stop(sprintf("Unknown method: %s", method))
  }

  # Filter the `parameters` slot based on the method
  filtered_parameters <- object@parameters[names(object@parameters) %in% parameter_fields[[method]]]

  return(filtered_parameters)
}




setClass(
  "DrugAnnotation",
  slots = list(
    result = "data.frame",
    parameters = "list"
  )
)

setValidity("DrugAnnotation", function(object) {
  if (!is.list(object@parameters)) {
    return("`parameters` slot must be a list.")
  }
  TRUE
})


DrugAnnotation <- function(
    result = data.frame(), # The result data frame for annotations
    input_data = character(), # Input data for the annotation process (vector of gene symbols)
    source = "GO", # Source for enrichment analysis (default: "GO")
    ont = "BP", # Ontology type (default: Biological Process "BP")
    Adjusted.P.value = 0.05, # Adjusted p-value threshold (default: 0.05)
    target_id_from = NULL # Optional target identifier type for TSEA conversion
) {
  # Validate inputs
  if (!is.data.frame(result)) stop("`result` must be a data frame.")
  if (!is.vector(input_data) || !is.character(input_data)) {
    stop("`input_data` must be a character vector.")
  }
  if (!is.character(source) || length(source) != 1) {
    stop("`source` must be a single character string.")
  }
  if (!is.character(ont) || length(ont) != 1) {
    stop("`ont` must be a single character string.")
  }
  if (!is.numeric(Adjusted.P.value) || length(Adjusted.P.value) != 1 || Adjusted.P.value <= 0 || Adjusted.P.value > 1) {
    stop("`Adjusted.P.value` must be a numeric value between 0 and 1.")
  }
  if (!is.null(target_id_from) && (!is.character(target_id_from) || length(target_id_from) != 1 || is.na(target_id_from) || !nzchar(trimws(target_id_from)))) {
    stop("`target_id_from` must be NULL or a single non-empty character string.")
  }

  # Prepare the parameters list
  parameters <- list(
    input_data = input_data,
    source = source,
    ont = ont,
    Adjusted.P.value = Adjusted.P.value,
    target_id_from = target_id_from
  )

  # Create and return the DrugAnnotation object
  new(
    "DrugAnnotation",
    result = result,
    parameters = parameters
  )
}


#' @title DrugSearchingPipeline Class Definition
#' @description A class to manage drug signature searching and processing.
#' @export
setClass("DrugSearchingPipeline",
         slots = c(
           DrugSearching = "list",
           RankAggregation = "list",
           type = "character"
         ),
         prototype = list(
           DrugSearching = list(Raw = list(), Processed = list()),
           RankAggregation = list(),
           type = "signature"
         )
)

#' Annotated Drug-Searching Pipeline Class
#'
#' Stores raw and processed searches, rank aggregations, and drug annotations.
#'
#' @slot DrugSearching A list containing `Raw` and `Processed` search results.
#' @slot RankAggregation A list of rank-aggregation results.
#' @slot DrugAnnotation A list of drug-annotation results.
#' @slot type A character string identifying the pipeline type.
#' @exportClass DrugSearchingPipelineAnnotated
setClass(
  "DrugSearchingPipelineAnnotated",
  slots = c(
    DrugSearching = "list",
    RankAggregation = "list",
    DrugAnnotation = "list",
    type = "character"
  ),
  prototype = list(
    DrugSearching = list(Raw = list(), Processed = list()),
    RankAggregation = list(),
    DrugAnnotation = list(),
    type = "signature"
  )
)

#' Visualized Drug-Searching Pipeline Class
#'
#' Stores raw and processed searches, rank aggregations, and visualizations.
#'
#' @slot DrugSearching A list containing `Raw` and `Processed` search results.
#' @slot RankAggregation A list of rank-aggregation results.
#' @slot Visualization A list of visualization results.
#' @slot type A character string identifying the pipeline type.
#' @exportClass DrugSearchingPipelineVisualized
setClass(
  "DrugSearchingPipelineVisualized",
  slots = c(
    DrugSearching = "list",
    RankAggregation = "list",
    Visualization = "list",
    type = "character"
  ),
  prototype = list(
    DrugSearching = list(Raw = list(), Processed = list()),
    RankAggregation = list(),
    Visualization = list(),
    type = "signature"
  )
)

#' Annotated and Visualized Drug-Searching Pipeline Class
#'
#' Stores search, rank-aggregation, annotation, and visualization results.
#'
#' @slot DrugSearching A list containing `Raw` and `Processed` search results.
#' @slot RankAggregation A list of rank-aggregation results.
#' @slot DrugAnnotation A list of drug-annotation results.
#' @slot Visualization A list of visualization results.
#' @slot type A character string identifying the pipeline type.
#' @exportClass DrugSearchingPipelineAnnotatedVisualized
setClass(
  "DrugSearchingPipelineAnnotatedVisualized",
  slots = c(
    DrugSearching = "list",
    RankAggregation = "list",
    DrugAnnotation = "list",
    Visualization = "list",
    type = "character"
  ),
  prototype = list(
    DrugSearching = list(Raw = list(), Processed = list()),
    RankAggregation = list(),
    DrugAnnotation = list(),
    Visualization = list(),
    type = "signature"
  )
)

.as_base_drug_searching_pipeline <- function(from) {
  methods::new(
    "DrugSearchingPipeline",
    DrugSearching = from@DrugSearching,
    RankAggregation = from@RankAggregation,
    type = from@type
  )
}

# Complete the non-structural inheritance relationship in both directions.
# `setIs()` otherwise asks methods to synthesize a replacement coercion and
# warns at package load because the derived classes contain additional slots.
.replace_base_drug_searching_pipeline <- function(from, value) {
  from@DrugSearching <- value@DrugSearching
  from@RankAggregation <- value@RankAggregation
  from@type <- value@type
  methods::validObject(from)
  from
}

# Register non-structural inheritance so existing
# `is(x, "DrugSearchingPipeline")` checks remain valid without changing slots.
methods::setIs(
  "DrugSearchingPipelineAnnotated",
  "DrugSearchingPipeline",
  coerce = .as_base_drug_searching_pipeline,
  replace = .replace_base_drug_searching_pipeline
)
methods::setIs(
  "DrugSearchingPipelineVisualized",
  "DrugSearchingPipeline",
  coerce = .as_base_drug_searching_pipeline,
  replace = .replace_base_drug_searching_pipeline
)
methods::setIs(
  "DrugSearchingPipelineAnnotatedVisualized",
  "DrugSearchingPipeline",
  coerce = .as_base_drug_searching_pipeline,
  replace = .replace_base_drug_searching_pipeline
)

.validate_drug_searching_pipeline <- function(object) {
  if (!is.list(object@DrugSearching) ||
      !all(c("Raw", "Processed") %in% names(object@DrugSearching)) ||
      !is.list(object@DrugSearching$Raw) ||
      !is.list(object@DrugSearching$Processed)) {
    return("`DrugSearching` slot must be a list containing list entries `Raw` and `Processed`.")
  }
  if (!is.list(object@RankAggregation)) {
    return("`RankAggregation` slot must be a list.")
  }
  if (length(object@type) != 1L || !nzchar(object@type)) {
    return("`type` slot must be a single non-empty character value.")
  }
  TRUE
}

setValidity("DrugSearchingPipeline", .validate_drug_searching_pipeline)
setValidity("DrugSearchingPipelineAnnotated", .validate_drug_searching_pipeline)
setValidity("DrugSearchingPipelineVisualized", .validate_drug_searching_pipeline)
setValidity("DrugSearchingPipelineAnnotatedVisualized", .validate_drug_searching_pipeline)

.drug_searching_pipeline_classes <- c(
  "DrugSearchingPipeline",
  "DrugSearchingPipelineAnnotated",
  "DrugSearchingPipelineVisualized",
  "DrugSearchingPipelineAnnotatedVisualized"
)

.is_drug_searching_pipeline <- function(object) {
  any(vapply(
    .drug_searching_pipeline_classes,
    function(class_name) methods::is(object, class_name),
    logical(1)
  ))
}

#' Construct a DrugSearchingPipeline object
#'
#' @param DrugSearching List containing `Raw` and `Processed` entries.
#' @param RankAggregation List of rank-aggregation results.
#' @param DrugAnnotation Optional list of annotation results, or NULL to omit the annotation slot.
#' @param Visualization Optional list of visualization outputs, or NULL to omit the visualization slot.
#' @param type Pipeline type label (for example `"signature"` or `"network"`).
#'
#' @return A valid pipeline S4 object whose slots follow the order
#'   `DrugSearching`, `RankAggregation`, optional `DrugAnnotation`, optional
#'   `Visualization`, and `type`.
#' @export
DrugSearchingPipeline <- function(
    DrugSearching = list(Raw = list(), Processed = list()),
    RankAggregation = list(),
    DrugAnnotation = NULL,
    Visualization = NULL,
    type = "signature"
) {
  if (!is.list(DrugSearching)) {
    stop("`DrugSearching` must be a list containing `Raw` and `Processed`.", call. = FALSE)
  }
  if (!all(c("Raw", "Processed") %in% names(DrugSearching))) {
    stop("`DrugSearching` must contain `Raw` and `Processed` entries.", call. = FALSE)
  }
  if (!is.list(DrugSearching$Raw) || !is.list(DrugSearching$Processed)) {
    stop("`DrugSearching$Raw` and `DrugSearching$Processed` must both be lists.", call. = FALSE)
  }
  if (!is.list(RankAggregation)) {
    stop("`RankAggregation` must be a list.", call. = FALSE)
  }
  if (!is.null(DrugAnnotation) && !is.list(DrugAnnotation)) {
    stop("`DrugAnnotation` must be NULL or a list.", call. = FALSE)
  }
  if (!is.null(Visualization) && !is.list(Visualization)) {
    stop("`Visualization` must be NULL or a list.", call. = FALSE)
  }
  if (!is.character(type) || length(type) != 1L || is.na(type) || !nzchar(type)) {
    stop("`type` must be a single non-empty character string.", call. = FALSE)
  }

  pipeline_class <- if (!is.null(DrugAnnotation) && !is.null(Visualization)) {
    "DrugSearchingPipelineAnnotatedVisualized"
  } else if (!is.null(DrugAnnotation)) {
    "DrugSearchingPipelineAnnotated"
  } else if (!is.null(Visualization)) {
    "DrugSearchingPipelineVisualized"
  } else {
    "DrugSearchingPipeline"
  }

  pipeline_args <- list(
    DrugSearching = DrugSearching,
    RankAggregation = RankAggregation,
    type = type
  )
  if (!is.null(DrugAnnotation)) {
    pipeline_args$DrugAnnotation <- DrugAnnotation
  }
  if (!is.null(Visualization)) {
    pipeline_args$Visualization <- Visualization
  }

  do.call(methods::new, c(pipeline_class, pipeline_args))
}




# Define the PlotObject class
setClass(
  "PlotObject",
  slots = list(
    parameters = "list" # Parameters for the PlotObject
  )
)

setValidity("PlotObject", function(object) {
  if (!is.list(object@parameters)) {
    return("`parameters` slot must be a list.")
  }
  required_names <- c("input_data", "file_type", "file_name", "width", "height", "units")
  if (!all(required_names %in% names(object@parameters))) {
    return("`parameters` slot is missing one or more required entries.")
  }
  TRUE
})

# Constructor function for PlotObject
PlotObject <- function(input_data = data.frame(),
                       file_type = c("pdf", "png", "svg", "jpeg"),
                       file_name = NULL,
                       width = 30,
                       height = 19,
                       units = c("in", "cm", "mm", "px")) {
  # Validate inputs
  if (missing(input_data) || !is.data.frame(input_data)) stop("`input_data` must be a data frame and cannot be missing.")

  file_type <- match.arg(file_type) # Ensures a valid file type
  units <- match.arg(units) # Ensures valid units

  if (is.null(file_name) || !is.character(file_name) || nchar(file_name) == 0) {
    stop("`file_name` must be a non-empty character string.")
  }

  if (!is.numeric(width) || length(width) != 1) stop("`width` must be a single numeric value.")

  if (!is.numeric(height) || length(height) != 1) stop("`height` must be a single numeric value.")


  # Prepare the parameters list
  parameters <- list(
    input_data = input_data,
    file_type = file_type,
    file_name = file_name,
    width = width,
    height = height,
    units = units
  )

  # Return the new object
  new("PlotObject", parameters = parameters)
}
