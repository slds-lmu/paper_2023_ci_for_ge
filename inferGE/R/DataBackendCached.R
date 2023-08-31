#' @title Cached Data Backend
#' @description
#' This backend wraps another backend -- usually an out of memory backend -- and when first accessing
#' the data field, it creates a [`mlr3::DataBackendDataTable`] for the IDs provided
#' during construction.
#' @export
DataBackendCached = R6Class("DataBackendCached",
  inherit = mlr3::DataBackend,
  public = list(
    initialize = function(backend, ids) {
      private$.ids = ids
      private$.colnames = backend$colnames
      super$initialize(
        backend,
        backend$primary_key,
        backend$data_formats
      )
    },
    data = function(rows, cols, data_format = "data.table") {
      stopifnot(data_format == "data.table")
      self$cached_backend$data(rows, cols, data_format)
    },
    missings = function(rows, cols) {
      self$cached_backend$missings(rows, cols)
    }, 
    distinct = function(rows, cols, na_rm = TRUE) {
      self$cached_backend$distinct(rows, cols, na_rm)
    },
    head = function(n = 6L) {
      self$cached_backend$head(n)
    }
  ),
  active = list(
    rownames = function() {
      private$.ids
    },
    colnames = function() {
      private$.colnames
    }, 
    nrow = function() {
      length(private$.ids)
    },
    ncol = function() {
      length(private$.colnames)
    },
    cached_backend = function() {
      if (is.null(private$.cache)) {
        data = private$.data$data(
          private$.ids,private$.data$colnames,
          data_format = "data.table"
        )
        private$.cache = DataBackendDataTable$new(
          data,
          primary_key = private$.data$primary_key
        )
      }

      private$.cache
    }
  ),
  private = list(
    .cache = NULL,
    .ids = NULL,
    .colnames = NULL,
    .calculate_hash = function() {
      mlr3misc::calculate_hash(self$backend$hash, private$.ids)
    }
  )
)