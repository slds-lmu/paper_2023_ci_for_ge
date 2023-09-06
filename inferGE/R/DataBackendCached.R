#' @title Cached Data Backend
#' @description
#' This backend wraps another backend -- usually an out of memory backend -- and when first accessing
#' the data field, it creates a [`mlr3::DataBackendDataTable`] for the IDs provided
#' during construction.
#' @param backend The data backend whose `$data()` we want to cache upon first access.
#' @param ids The cached backend represens only a row-wise subset of the wrapped backend specified
#'   by this parameter.a
#' @param do_caching (logical) whether to cache. 
#' 
#' @export
DataBackendCached = R6Class("DataBackendCached",
  inherit = mlr3::DataBackend,
  public = list(
    do_caching = NULL,
    initialize = function(backend, ids, do_caching = FALSE) {
      private$.ids = ids
      private$.colnames = backend$colnames
      self$do_caching = do_caching
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
      # During task construction head() is called, we want to avoid loading the cache there
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
      if (!self$do_caching) {
        return(private$.data)
      } 
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