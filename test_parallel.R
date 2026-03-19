# -*- coding: utf-8 -*-
# ---
# jupyter:
#   jupytext:
#     cell_metadata_filter: -all
#     formats: ipynb,R:percent
#     text_representation:
#       extension: .R
#       format_name: percent
#       format_version: '1.3'
#       jupytext_version: 1.15.0
#   kernelspec:
#     display_name: R
#     language: R
#     name: ir
# ---

# %%
library(doParallel)
library(foreach)

# %%
# ── 1. Detect cores ────────────────────────────────────────────────────────────
n_cores <- detectCores(logical = FALSE)
cat(sprintf("Physical cores detected: %d\n", n_cores))
cat(sprintf("Logical cores detected:  %d\n", detectCores(logical = TRUE)))

# %%
# ── 2. Register cluster ────────────────────────────────────────────────────────
n_workers <- max(1L, n_cores - 1L)
cl <- makeCluster(n_workers)
registerDoParallel(cl)
cat(sprintf("Workers registered:      %d\n\n", getDoParWorkers()))

# %%
# ── 3. Basic correctness test ──────────────────────────────────────────────────
# Each worker returns its own PID; we should see n_workers distinct PIDs.
cat("── Test 1: worker identity ──────────────────────────────────────────────\n")
pids <- foreach(i = seq_len(n_workers * 2), .combine = c) %dopar% Sys.getpid()
n_unique <- length(unique(pids))
cat(sprintf("  Unique worker PIDs seen: %d  (expected ~%d)\n", n_unique, n_workers))
if (n_unique >= n_workers) {
    cat("  PASS\n\n")
} else {
    cat("  WARN – fewer workers than expected (may still be correct on small machines)\n\n")
}

# %%
# ── 4. Numeric correctness test ───────────────────────────────────────────────
# Parallel sum of squares vs known formula n*(n+1)*(2n+1)/6
cat("── Test 2: numeric correctness ──────────────────────────────────────────\n")
n <- 1000L
parallel_result <- foreach(i = seq_len(n), .combine = `+`) %dopar% (i^2)
expected        <- n * (n + 1L) * (2L * n + 1L) / 6L
cat(sprintf("  parallel sum of squares: %d\n", parallel_result))
cat(sprintf("  expected:                %d\n", expected))
if (identical(parallel_result, expected)) {
    cat("  PASS\n\n")
} else {
    cat(sprintf("  FAIL – difference: %d\n\n", parallel_result - expected))
}

# %%
# ── 5. Speed test ─────────────────────────────────────────────────────────────
# Serial vs parallel on a CPU-bound task (computing row means of random matrices).
cat("── Test 3: parallel is faster than serial ───────────────────────────────\n")
n_tasks  <- 200L
task_fn  <- function(i) mean(rowMeans(matrix(rnorm(5000L), nrow = 100L)))

# %%
t_serial   <- system.time(
    serial_res <- sapply(seq_len(n_tasks), task_fn)
)[["elapsed"]]

# %%
t_parallel <- system.time(
    par_res <- foreach(i = seq_len(n_tasks), .combine = c) %dopar% task_fn(i)
)[["elapsed"]]

# %%
cat(sprintf("  serial time:   %.3f s\n", t_serial))
cat(sprintf("  parallel time: %.3f s  (%d workers)\n", t_parallel, n_workers))

# %%
if (n_workers > 1L && t_parallel < t_serial) {
    speedup <- t_serial / t_parallel
    cat(sprintf("  speedup: %.2fx\n", speedup))
    cat("  PASS\n\n")
} else if (n_workers == 1L) {
    cat("  SKIP – only 1 worker available; no parallelism possible\n\n")
} else {
    cat("  WARN – parallel was not faster (overhead may dominate for this task size)\n\n")
}

# %%
# ── 6. Variable export test ───────────────────────────────────────────────────
# Workers must receive exported variables correctly.
cat("── Test 4: variable export to workers ───────────────────────────────────\n")
multiplier <- 42L
par_mult <- foreach(i = 1:5, .combine = c, .export = "multiplier") %dopar% {
    i * multiplier
}
expected_mult <- (1:5) * 42L
if (identical(par_mult, expected_mult)) {
    cat(sprintf("  Results: %s\n", paste(par_mult, collapse = ", ")))
    cat("  PASS\n\n")
} else {
    cat(sprintf("  FAIL – got: %s  expected: %s\n\n",
        paste(par_mult, collapse = ", "),
        paste(expected_mult, collapse = ", ")))
}

# %%
# ── 7. Error handling test ────────────────────────────────────────────────────
# A foreach that includes one failing task should surface an error.
cat("── Test 5: error propagation ────────────────────────────────────────────\n")
tryCatch({
    foreach(i = 1:4, .combine = c) %dopar% {
        if (i == 3L) stop("intentional error on task 3")
        i * 10L
    }
    cat("  FAIL – error was not propagated\n\n")
}, error = function(e) {
    cat(sprintf("  Error caught: \"%s\"\n", conditionMessage(e)))
    cat("  PASS\n\n")
})

# %%
# ── 8. Cleanup ────────────────────────────────────────────────────────────────
stopCluster(cl)
cat("Cluster stopped. All tests complete.\n")

# %%
