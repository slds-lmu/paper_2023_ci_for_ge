repeat {
    res = try(renv::restore(prompt = FALSE, exclude = "inferGE"), silent = TRUE)
    if (!inherits(res, "try-error")) break

    package_name <- c(sub(".*'([^']+)'.*", "\\1", res))
    renv::install(package_name)
    renv::record(package_name)
}