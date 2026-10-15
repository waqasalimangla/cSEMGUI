# ==============================================================================
#  cSEM GUI - a cSEM-style Shiny interface built on the cSEM package
#
#  Run:       shiny::runApp("app_csem.R")
#  Requires:  shiny, cSEM, writexl  (jsonlite ships with shiny)
#
#  File layout
#    1. Setup & dependencies
#    2. Presentation helpers (HTML tables)
#    3. Statistical engine   - pure functions, no Shiny state. Shipped to
#                              parallel workers during bootstrapping.
#    4. Front end assets     - CSS / JavaScript for the SVG model canvas
#    5. User interface
#    6. Server
# ==============================================================================

# ---- 1. Setup & dependencies -------------------------------------------------
local({
  needed  <- c("shiny", "cSEM", "writexl", "jsonlite", "parallel")
  missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing))
    stop("Missing package(s): ", paste(missing, collapse = ", "),
         ".\nInstall with: install.packages(c(",
         paste0("\"", missing, "\"", collapse = ", "), "))", call. = FALSE)
})
suppressPackageStartupMessages({ library(shiny); library(cSEM) })

options(shiny.maxRequestSize = 100 * 1024^2)     # allow CSV uploads up to 100 MB

# ---- 2. Presentation helpers -------------------------------------------------
# turn ANY cSEM output (list / matrix / vector / data.frame) into a named list of data.frames
flat <- function(x, nm = "Result", depth = 0) {
  if (is.null(x) || is.function(x) || depth > 4) return(list())
  if (is.data.frame(x)) return(setNames(list(x), nm))
  if (is.matrix(x)) {
    d <- as.data.frame(unclass(x), check.names = FALSE)
    if (!is.null(rownames(x))) d <- cbind(` ` = rownames(x), d)
    return(setNames(list(d), nm))
  }
  if (is.list(x)) {
    out <- list(); nms <- names(x); if (is.null(nms)) nms <- seq_along(x)
    for (i in seq_along(x))
      out <- c(out, flat(x[[i]], if (nm == "Result") as.character(nms[i]) else paste(nm, "-", nms[i]), depth + 1))
    return(out)
  }
  if (is.atomic(x)) {
    d <- if (!is.null(names(x))) data.frame(Name = names(x), Value = unname(x)) else data.frame(Value = x)
    return(setNames(list(d), nm))
  }
  list()
}
# Quality criteria (change here if you prefer other cut-offs)
THR <- list(rel = 0.70,   # Cronbach's alpha, rho_a, rho_c  >= 0.70
            ave = 0.50,   # AVE                             >= 0.50
            load = 0.70,  # outer loadings                  >= 0.70
            htmt = 0.90,  # HTMT                            <  0.90
            vif = 5,      # VIF                             <= 5
            srmr = 0.08,  # SRMR                            <  0.08
            nfi = 0.90)   # NFI                             >= 0.90

# Decide per cell whether a value meets its threshold: "sp-ok" (green), "sp-bad" (red) or "" (no rule).
# `raw` = the untouched numeric data.frame, `sec` = report section the table belongs to (NULL = no colouring).
judge <- function(raw, title, sec) {
  cl <- matrix("", nrow(raw), ncol(raw)); lg <- NULL
  if (is.null(sec) || !nrow(raw) || ncol(raw) < 2L) return(list(cl = cl, legend = lg))
  isn <- vapply(raw, is.numeric, logical(1))
  mark <- function(j, ok) cl[, j] <<- ifelse(is.na(ok), "", ifelse(ok, "sp-ok", "sp-bad"))
  num_cols <- setdiff(which(isn), 1L)
  eps <- 1e-9
  if (identical(sec, "Outer loadings")) {
    for (j in num_cols) mark(j, raw[[j]] >= THR$load - eps); lg <- "Loading \u2265 0.70"
  } else if (identical(sec, "Construct reliability and validity")) {
    for (j in num_cols) {
      nm <- names(raw)[j]
      if (grepl("Cronbach|rho_", nm)) mark(j, raw[[j]] >= THR$rel - eps)
      else if (grepl("AVE|Average variance", nm)) mark(j, raw[[j]] >= THR$ave - eps) }
    lg <- "\u03b1, \u03c1 \u2265 0.70 | AVE \u2265 0.50"
  } else if (identical(sec, "Discriminant validity")) {
    if (grepl("HTMT", title)) { for (j in num_cols) mark(j, raw[[j]] < THR$htmt); lg <- "HTMT < 0.90" }
    else if (grepl("Fornell", title)) {                       # diagonal (sqrt AVE) must exceed all correlations of its construct
      M <- as.matrix(raw[, -1, drop = FALSE]); n <- min(nrow(M), ncol(M)); Fm <- M
      for (i in seq_len(nrow(Fm))) for (j in seq_len(ncol(Fm))) if (is.na(Fm[i, j]) && j <= nrow(Fm) && i <= ncol(Fm)) Fm[i, j] <- Fm[j, i]
      for (i in seq_len(n)) { d0 <- M[i, i]; ot <- abs(Fm[i, -i]); ot <- ot[is.finite(ot)]
        if (is.finite(d0)) cl[i, i + 1L] <- if (all(d0 > ot)) "sp-ok" else "sp-bad" }
      lg <- "\u221aAVE > correlations" }
  } else if (identical(sec, "Collinearity statistics (VIF)")) {
    for (j in num_cols) mark(j, raw[[j]] <= THR$vif + eps); lg <- "VIF \u2264 5"
  } else if (identical(sec, "Model fit")) {
    lab <- trimws(as.character(raw[[1]]))
    for (j in num_cols) { v <- raw[[j]]; ok <- rep(NA, length(v))
      ok[lab == "SRMR"] <- v[lab == "SRMR"] < THR$srmr
      ok[lab == "NFI"]  <- v[lab == "NFI"] >= THR$nfi - eps
      mark(j, ok) }
    lg <- "SRMR < 0.08 | NFI \u2265 0.90"
  } else if (identical(sec, "PLSpredict")) {
    for (j in num_cols) { nm <- names(raw)[j]
      if (grepl("^Q2", nm)) mark(j, raw[[j]] > 0)
      else if (grepl("difference", nm)) mark(j, raw[[j]] < 0) }
    lg <- "Q2 > 0 | PLS - benchmark < 0"
  }
  list(cl = cl, legend = if (any(cl != "")) lg else NULL)
}

html_tab <- function(d, title, sec = NULL) {
  d <- as.data.frame(d, check.names = FALSE); if (!ncol(d)) return(NULL)
  raw <- d; jd <- judge(raw, title, sec); cl <- jd$cl                # threshold colours (green = met, red = not met)
  is_num <- vapply(d, is.numeric, logical(1))            # numbers are right-aligned, text left-aligned
  d[] <- lapply(d, function(v) {
    v <- if (is.numeric(v)) {
      f <- v[is.finite(v)]
      if (all(f == round(f))) as.character(v) else formatC(v, format = "f", digits = 3)
    } else as.character(v)
    v[is.na(v) | trimws(v) %in% c("NA", "NaN", "Inf", "-Inf", "<NA>")] <- ""   # missing values stay blank, never "NA"
    v })
  al <- function(j) if (j == 1L || !is_num[j]) "sp-l" else "sp-n"
  tags$div(class = "sp-tabwrap", `data-title` = title,
    tags$div(class = "sp-pls",
      tags$div(class = "sp-pls-title",
               tags$span(class = "sp-pls-t", title = title, title),
               if (!is.null(jd$legend)) tags$span(class = "sp-legend", jd$legend),
               tags$button(type = "button", class = "btn btn-default btn-xs", onclick = "spCopyTab(this)",
                           title = "Copy the full table; paste into MS Word or MS Excel", HTML("&#128203; Copy"))),
      tags$div(class = "sp-pls-scroll",
        tags$table(class = "sp-pls-tab", style = sprintf("min-width:%dpx;", 150L + 90L * (ncol(d) - 1L)),
          tags$thead(tags$tr(lapply(seq_along(d), function(j) tags$th(class = al(j), names(d)[j])))),
          tags$tbody(lapply(seq_len(nrow(d)), function(i)
            tags$tr(lapply(seq_along(d), function(j)
              tags$td(class = trimws(paste(if (j == 1L) "sp-rl" else al(j), cl[i, j])), d[i, j])))))))))
}
tabs_out <- function(l) if (is.list(l) && length(l)) tagList(lapply(names(l), function(n) html_tab(l[[n]], n))) else NULL
tm <- function(m) if (is.matrix(m)) t(m) else m

# ---- 3. Statistical engine ---------------------------------------------------
# Everything in this section is a pure function of its arguments (no reactive
# state), so that it can be copied to parallel worker processes.

# Report sections shown in the bootstrapping tab
BS <- c("Path coefficients", "Total indirect effects", "Specific indirect effects", "Total effects",
        "Outer loadings", "Outer weights", "R-square", "R-square adjusted", "Cronbach's alpha",
        "Composite reliability (rho_a)", "Composite reliability (rho_c)", "Average variance extracted (AVE)",
        "HTMT", "SRMR", "d_ULS", "d_G")

# Single fit statistic from a cSEM result (saturated or estimated model)
fit_one <- function(r, fname, sat) {
  fn <- tryCatch(getExportedValue("cSEM", fname), error = function(e) NULL)
  if (is.null(fn)) return(NA_real_)
  # some cSEM fit functions (NFI, CFI, GFI, IFI, NNFI, RMSEA) have no `.saturated` argument: they exist for the estimated model only
  if (!(".saturated" %in% names(formals(fn)))) { if (sat) return(NA_real_); return(tryCatch(as.numeric(unlist(fn(r)))[1], error = function(e) NA_real_)) }
  tryCatch(as.numeric(unlist(fn(r, .saturated = sat)))[1], error = function(e) NA_real_)
}

# Named vector of the entries of matrix m at the non-NA positions of ref
mvec <- function(m, ref, sep) {
  ix <- which(!is.na(ref), arr.ind = TRUE)
  if (!nrow(ix)) return(numeric(0))
  rn <- rownames(ref)[ix[, 1]]; cn <- colnames(ref)[ix[, 2]]
  v <- tryCatch(m[cbind(rn, cn)], error = function(e) rep(NA_real_, length(rn)))
  v <- setNames(as.numeric(v), paste(cn, sep, rn)); v[order(names(v))]
}

# Bootstrap confidence interval (percentile or bias-corrected)
ci_fun <- function(x, o, lo, hi, bc) {
  x <- x[is.finite(x)]
  if (length(x) < 2 || !is.finite(o)) return(c(NA_real_, NA_real_))
  if (bc) {
    e  <- 1 / (2 * length(x)); z0 <- qnorm(min(max(mean(x < o), e), 1 - e))
    lo <- pnorm(2 * z0 + qnorm(lo)); hi <- pnorm(2 * z0 + qnorm(hi))
  }
  unname(quantile(x, c(lo, hi), na.rm = TRUE))
}

# Resolve the sign indeterminacy of a bootstrap run against the original outer weights `Wo`
align_signs <- function(rb, Wo) {
  Wb <- rb$Estimates$Weight_estimates
  ks <- intersect(rownames(Wb), rownames(Wo)); cs <- intersect(colnames(Wb), colnames(Wo))
  s  <- setNames(rep(1, length(ks)), ks)
  for (k in ks) { v <- sum(Wb[k, cs] * Wo[k, cs], na.rm = TRUE); if (is.finite(v) && v < 0) s[k] <- -1 }
  if (all(s == 1)) return(rb)
  e <- rb$Estimates
  for (nm in c("Weight_estimates", "Loading_estimates")) { m <- e[[nm]]; m[ks, ] <- m[ks, , drop = FALSE] * s; e[[nm]] <- m }
  Sc <- e$Construct_scores; Sc[, ks] <- sweep(Sc[, ks, drop = FALSE], 2, s, `*`); e$Construct_scores <- Sc
  Pm <- e$Path_estimates; sa <- setNames(rep(1, nrow(Pm)), rownames(Pm)); sa[ks] <- s[ks]
  for (k in setdiff(rownames(Pm), ks)) {
    pt <- strsplit(k, ".", fixed = TRUE)[[1]]
    if (all(pt %in% ks)) sa[k] <- prod(s[pt])
  }
  Pm[] <- Pm * outer(sa, sa); e$Path_estimates <- Pm
  rb$Estimates <- e; rb
}

# Set tiny values to NA (cSEM shows "no effect" as empty cells)
nz <- function(m) { m[!is.na(m) & abs(m) < 1e-12] <- NA; m }

# Embed matrix m into a square nms x nms matrix
sq <- function(m, nms) {
  o <- matrix(NA_real_, length(nms), length(nms), dimnames = list(nms, nms))
  if (!is.null(m) && !is.null(rownames(m))) {
    rr <- intersect(rownames(m), nms); cc <- intersect(colnames(m), nms); o[rr, cc] <- m[rr, cc]
  }
  nz(o)
}

# Everything derived from one cSEM result: scores, paths, effects, R2 ...
calc_fun <- function(r, fb = NULL) {
  e <- r$Estimates
  S <- as.matrix(e$Construct_scores); S0 <- S
  # interaction terms (A.B) are not part of cSEM's construct scores: add the standardised product of their parts
  for (k in setdiff(colnames(e$Path_estimates), colnames(S0))) {
    pt <- strsplit(k, ".", fixed = TRUE)[[1]]
    if (all(pt %in% colnames(S0))) {
      S <- cbind(S, as.vector(scale(Reduce(`*`, lapply(pt, function(z) S0[, z]))))); colnames(S)[ncol(S)] <- k
    }
  }
  nms <- sort(colnames(S)); S <- S[, nms, drop = FALSE]
  P <- sq(e$Path_estimates, nms); L <- e$Loading_estimates; W <- e$Weight_estimates
  nmc <- nms[nms %in% rownames(L)]                       # constructs with indicators (no interaction terms)
  blocks <- lapply(setNames(nms, nms), function(k) {
    if (!k %in% rownames(L)) return(character(0)); v <- L[k, ]; names(v)[!is.na(v) & v != 0] })
  d <- tryCatch(as.data.frame(r$Information$Data), error = function(e) NULL); if (is.null(d)) d <- fb
  ind <- intersect(unlist(blocks), names(d))
  X <- scale(as.matrix(d[, ind, drop = FALSE])); X <- X[complete.cases(X), , drop = FALSE]
  Rm <- cor(S); preds <- lapply(setNames(nms, nms), function(y) nms[!is.na(P[y, ])])
  endo <- nms[lengths(preds) > 0]; n <- nrow(S)
  r2f <- function(y, pr) if (!length(pr)) 0 else { rr <- Rm[pr, y]; drop(t(rr) %*% solve(Rm[pr, pr, drop = FALSE], rr)) }
  R2 <- vapply(endo, function(y) r2f(y, preds[[y]]), 0)
  # consistent PLS: use cSEM's disattenuated R-square so that it matches the (corrected) path coefficients
  if (isTRUE(r$Information$Arguments$.disattenuate) && all(endo %in% names(e$R2))) R2 <- e$R2[endo]
  B <- P; B[is.na(B)] <- 0; K <- length(nms)
  Tot <- solve(diag(K) - B) - diag(K); dimnames(Tot) <- list(nms, nms)
  list(r = r, S = S, nms = nms, nmc = nmc, P = P, L = L, W = W, blocks = blocks, X = X, Rm = Rm, preds = preds,
       endo = endo, n = n, r2f = r2f, R2 = R2, B = B, Tot = Tot, Ind = Tot - B)
}

# All directed paths with >= 2 arrows (>= 3 nodes) through the structural matrix B.
# Returned as index vectors; the structure is fixed, so this is computed once per model
# and re-used for every bootstrap resample.
spec_paths <- function(B) {
  nm <- rownames(B); out <- list()
  walk <- function(path) {
    cur <- path[length(path)]
    for (nx in setdiff(which(B[, cur] != 0), path)) {
      p2 <- c(path, nx)
      if (length(p2) >= 3) out[[length(out) + 1]] <<- p2
      walk(p2)
    }
  }
  for (s0 in seq_along(nm)) walk(s0)
  list(idx = out, names = vapply(out, function(p) paste(nm[p], collapse = " -> "), ""))
}
# Values of the specific indirect effects along pre-computed paths
spec_vals <- function(B, sp) {
  if (!length(sp$idx)) return(numeric(0))
  v <- vapply(sp$idx, function(p) prod(B[cbind(p[-1], p[-length(p)])]), 0)
  setNames(v, sp$names)[order(sp$names)]
}
# Report table of specific indirect effects
spec <- function(B) {
  sp <- spec_paths(B)
  if (!length(sp$idx)) return(data.frame(Note = "No specific indirect effects in this model"))
  v <- vapply(sp$idx, function(p) prod(B[cbind(p[-1], p[-length(p)])]), 0)
  data.frame(` ` = sp$names, `Specific indirect effects` = v, check.names = FALSE)
}

# Reliability and convergent validity
rel_fun <- function(cc) with(cc, {
  rel <- do.call(rbind, lapply(nmc, function(k) {
    b <- blocks[[k]]; l <- L[k, b]; w <- W[k, b]; Rb <- cor(X[, b, drop = FALSE]); kk <- length(b)
    if (kk < 2) return(data.frame(` ` = k, `Cronbach's alpha` = 1, `Composite reliability (rho_a)` = 1,
                                  `Composite reliability (rho_c)` = 1, `Average variance extracted (AVE)` = 1, check.names = FALSE))
    rbar <- mean(Rb[lower.tri(Rb)]); w <- w / sqrt(drop(t(w) %*% Rb %*% w)); ww <- w %*% t(w)
    rho_a <- drop(t(w) %*% w)^2 * drop(t(w) %*% (Rb - diag(diag(Rb))) %*% w) / drop(t(w) %*% (ww - diag(diag(ww))) %*% w)
    data.frame(` ` = k, `Cronbach's alpha` = kk * rbar / (1 + (kk - 1) * rbar), `Composite reliability (rho_a)` = rho_a,
               `Composite reliability (rho_c)` = sum(l)^2 / (sum(l)^2 + sum(1 - l^2)),
               `Average variance extracted (AVE)` = mean(l^2), check.names = FALSE)
  }))
  rel
})

# Heterotrait-monotrait ratio (v2 = TRUE gives HTMT2, the geometric-mean version)
ht_fun <- function(cc, v2) with(cc, {
  Rx <- cor(X); gm <- function(x) exp(mean(log(x)))
  o <- matrix(NA_real_, length(nmc), length(nmc), dimnames = list(nmc, nmc))
  mono <- function(b) if (length(b) < 2) NA_real_ else { x <- abs(Rx[b, b][lower.tri(Rx[b, b])]); if (v2) gm(x) else mean(x) }
  for (i in seq_along(nmc)) for (j in seq_along(nmc)) if (i > j) {
    bi <- blocks[[nmc[i]]]; bj <- blocks[[nmc[j]]]
    h <- abs(Rx[bi, bj]); o[i, j] <- (if (v2) gm(h) else mean(h)) / sqrt(mono(bi) * mono(bj))
  }
  o
})

# ---- bootstrap statistics ----------------------------------------------------
# All statistics reported by the bootstrapping tab, as a list of named numeric vectors (one per BS section).
# `refs` carries the reference layout (which cells exist) and the pre-computed indirect-effect paths.
boot_stat <- function(cc, r, refs, fit = TRUE) {
  o <- setNames(vector("list", length(BS)), BS)
  o[[1]] <- mvec(cc$P, refs$P, " -> ")
  o[[2]] <- mvec(cc$Ind, refs$Ind, " -> ")
  o[[3]] <- spec_vals(cc$B, refs$sp)
  o[[4]] <- mvec(cc$Tot, refs$Tot, " -> ")
  o[[5]] <- mvec(cc$L[cc$nmc, , drop = FALSE], refs$L, " <- ")
  o[[6]] <- mvec(cc$W[cc$nmc, , drop = FALSE], refs$W, " <- ")
  o[[7]] <- cc$R2
  o[[8]] <- vapply(cc$endo, function(y) 1 - (1 - cc$R2[[y]]) * (cc$n - 1) / (cc$n - length(cc$preds[[y]]) - 1), 0)
  rl <- rel_fun(cc)
  for (j in 2:5) o[[7 + j]] <- setNames(rl[[j]], rl[[1]])
  H <- ht_fun(cc, FALSE); o[[13]] <- mvec(H, H, " <-> ")
  if (fit) {
    f <- function(nm) c(`Saturated model` = fit_one(r, nm, TRUE), `Estimated model` = fit_one(r, nm, FALSE))
    o[[14]] <- f("calculateSRMR"); o[[15]] <- f("calculateDL"); o[[16]] <- f("calculateDG")
  } else for (j in 14:16) o[[j]] <- numeric(0)
  o
}

# Estimate a chunk of bootstrap resamples. `idx_list` is a list of row-index vectors.
# `ctx` = list(args, Wo, refs, fit). Returns one result (or NULL for a failed resample) per element.
# Runs identically in the main process and in parallel workers.
boot_chunk <- function(idx_list, ctx) {
  d0 <- ctx$args$.data
  lapply(idx_list, function(idx) {
    tryCatch({
      a <- ctx$args; db <- d0[idx, , drop = FALSE]; a$.data <- db
      rb <- suppressWarnings(suppressMessages(do.call(csem, a)))
      rb <- align_signs(rb, ctx$Wo)
      suppressWarnings(boot_stat(calc_fun(rb, db), rb, ctx$refs, ctx$fit))
    }, error = function(e) NULL)
  })
}

# Stack the valid resamples into one (resamples x statistics) matrix per section
stack_boots <- function(orig, boots) {
  lapply(setNames(names(orig), names(orig)), function(sec) {
    keys <- names(orig[[sec]])
    if (!length(keys)) return(matrix(numeric(0), length(boots), 0))
    m <- matrix(unlist(lapply(boots, function(b) as.numeric(b[[sec]][keys])), use.names = FALSE), nrow = length(keys))
    t(m)
  })
}

# Summary table (cSEM layout) of one bootstrapped section
boot_table <- function(O, M, n, alpha, two, bc) {
  if (!length(O)) return(data.frame(Note = "Not available (not requested or no entries)"))
  lo <- if (two) alpha / 2 else alpha; hi <- if (two) 1 - alpha / 2 else 1 - alpha
  lab <- function(q) paste0(format(100 * q, trim = TRUE), "%")
  keys <- names(O); O <- unname(O)
  mu  <- colMeans(M, na.rm = TRUE); sdv <- apply(M, 2, sd, na.rm = TRUE)
  tt  <- abs(O / sdv); p <- if (two) 2 * (1 - pt(tt, n - 1)) else 1 - pt(tt, n - 1)
  ci  <- t(vapply(seq_along(O), function(j) ci_fun(M[, j], O[j], lo, hi, bc), c(0, 0)))
  d <- data.frame(` ` = keys, `Original sample (O)` = O, `Sample mean (M)` = mu, `Standard deviation (STDEV)` = sdv,
                  `T statistics (|O/STDEV|)` = tt, `P values` = p, check.names = FALSE)
  d[[lab(lo)]] <- ci[, 1]; d[[lab(hi)]] <- ci[, 2]
  d$Significant <- ifelse(is.na(p), "", ifelse(p < alpha, "Yes", "No"))
  d
}

# ---- confirmatory tetrad analysis (Bollen & Ting 1993; Gudergan et al. 2008) --
tet_all <- function(p) {            # all tetrads: 3 per quadruple, as lists of two products of index pairs
  q <- combn(p, 4); T <- list()
  for (j in seq_len(ncol(q))) {
    i <- q[, j]
    pr <- function(a, b) c(min(i[a], i[b]), max(i[a], i[b]))
    T[[length(T) + 1]] <- list(i = i, a = list(pr(1, 2), pr(3, 4)), b = list(pr(1, 3), pr(2, 4)))
    T[[length(T) + 1]] <- list(i = i, a = list(pr(1, 2), pr(3, 4)), b = list(pr(1, 4), pr(2, 3)))
    T[[length(T) + 1]] <- list(i = i, a = list(pr(1, 3), pr(2, 4)), b = list(pr(1, 4), pr(2, 3)))
  }
  T
}
tet_jac <- function(S, t, p) {      # gradient of a tetrad w.r.t. the p(p-1)/2 unique covariances
  nm <- function(a) paste(a, collapse = "_")
  g <- setNames(numeric(0), character(0))
  add2 <- function(pa, other, sg) { k <- nm(pa); g[k] <<- (if (is.na(g[k])) 0 else g[k]) + sg * S[other[1], other[2]] }
  add2(t$a[[1]], t$a[[2]], 1); add2(t$a[[2]], t$a[[1]], 1); add2(t$b[[1]], t$b[[2]], -1); add2(t$b[[2]], t$b[[1]], -1)
  g
}
tet_set <- function(S, p) {         # greedy choice of linearly independent (non-redundant) tetrads
  T <- tet_all(p); keys <- apply(combn(p, 2), 2, paste, collapse = "_")
  J <- matrix(0, 0, length(keys), dimnames = list(NULL, keys)); keep <- integer(0)
  target <- p * (p - 3) / 2
  for (j in seq_along(T)) {
    g <- tet_jac(S, T[[j]], p); r <- setNames(numeric(length(keys)), keys); r[names(g)] <- g
    J2 <- rbind(J, r)
    if (qr(J2, tol = 1e-9)$rank > nrow(J)) { J <- J2; keep <- c(keep, j) }
    if (length(keep) >= target) break
  }
  T[keep]
}
tet_names <- function(T, v) vapply(T, function(t) {
  f <- function(a) paste0("(", v[a[1]], ",", v[a[2]], ")")
  sprintf("%s*%s - %s*%s", paste0("s", f(t$a[[1]])), paste0("s", f(t$a[[2]])), paste0("s", f(t$b[[1]])), paste0("s", f(t$b[[2]])))
}, "")
# Linear (column-major) indices into the p x p covariance matrix, so that all tetrads of a
# resample are evaluated with one vectorised expression instead of a loop over tetrads.
tet_index <- function(T, p) {
  lin <- function(sel, k) vapply(T, function(t) { pr <- t[[sel]][[k]]; (pr[2] - 1L) * p + pr[1] }, 0)
  list(a1 = lin("a", 1), a2 = lin("a", 2), b1 = lin("b", 1), b2 = lin("b", 2))
}
tet_vals <- function(S, ix) S[ix$a1] * S[ix$a2] - S[ix$b1] * S[ix$b2]

# ---- PLSpredict & CVPAT (k-fold cross-validated out-of-sample prediction) ------
# Follows cSEM::predict(): for every fold the model is re-estimated on the training part, the test part is
# standardised with the TRAINING mean / sd, scores of the exogenous constructs are predicted from the
# indicators (weights), structural prediction gives the endogenous construct scores (earliest antecedent:
# via exogenous constructs only; direct antecedent: via all direct predecessors), and the indicators of the
# endogenous constructs are predicted from the loadings and rescaled. Benchmarks: LM (each endogenous
# indicator regressed on all exogenous indicators) and IA (training mean of the indicator).
pred_metrics <- function(P, A, Mu) {          # P = predictions, A = actual, Mu = naive (training-mean) predictions
  ok <- stats::complete.cases(P) & stats::complete.cases(A)
  P <- P[ok, , drop = FALSE]; A <- A[ok, , drop = FALSE]; Mu <- Mu[ok, , drop = FALSE]
  R <- A - P; Rm <- A - Mu; cn <- colnames(A)
  mse2 <- vapply(cn, function(x) (mean(P[, x]) - mean(A[, x]))^2 + stats::var(R[, x]), 0)
  cs <- function(M) colSums(sweep(M, 2, colMeans(M))^2)
  uni <- function(x) { v <- suppressWarnings(x); if (is.finite(v)) v else NA_real_ }
  data.frame(
    MAE  = colMeans(abs(R)), RMSE = sqrt(colMeans(R^2)),
    Q2   = 1 - cs(R) / cs(Rm),
    MAPE = colMeans(abs(R / A)), MSE2 = mse2,
    U1   = sqrt(mse2) / sqrt(colMeans(A^2)),
    U2   = sqrt(colSums(R^2)) / sqrt(colSums(A^2)),
    UM   = (colMeans(P) - colMeans(A))^2 / mse2,
    UR   = vapply(cn, function(x) uni((stats::sd(P[, x]) - stats::cor(P[, x], A[, x]) * stats::sd(A[, x]))^2) / mse2[[x]], 0),
    UD   = vapply(cn, function(x) uni((1 - stats::cor(P[, x], A[, x])^2) * stats::var(A[, x])^2) / mse2[[x]], 0),
    row.names = cn)
}

# res = cSEM result of the full sample, args = the csem() arguments that produced it
pred_cv <- function(res, args, k = 10, reps = 1, seed = 123, approach = "earliest", inadm = "stop", tick = NULL) {
  Mo  <- res$Information$Model
  ind <- colnames(res$Information$Data)
  d   <- as.data.frame(args$.data)
  X   <- as.matrix(d[, ind, drop = FALSE]); X <- X[stats::complete.cases(X), , drop = FALSE]
  N   <- nrow(X)
  if (k < 2 || k > N) stop("The number of folds must be between 2 and the number of observations (", N, ").")
  msr  <- Mo$measurement
  endo <- colnames(msr)[colSums(msr[Mo$cons_endo, , drop = FALSE]) != 0]
  exo  <- colnames(msr)[colSums(msr[Mo$cons_exo,  , drop = FALSE]) != 0]
  blocks <- lapply(setNames(Mo$cons_endo, Mo$cons_endo), function(cn) intersect(colnames(msr)[msr[cn, ] != 0], endo))
  nE <- length(endo)
  S <- list(PLS = 0, LM = 0, IA = 0)                   # squared prediction errors, averaged over the repetitions
  M <- list(PLS = list(), LM = list(), IA = list())
  A <- X[, endo, drop = FALSE]
  for (r in seq_len(reps)) {
    set.seed(seed + r - 1L)
    fold <- integer(N); fold[sample.int(N)] <- rep_len(seq_len(k), N)
    Pt <- Pl <- Pi <- matrix(NA_real_, N, nE, dimnames = list(NULL, endo))
    for (j in seq_len(k)) {
      te <- which(fold == j); tr <- which(fold != j)
      Xtr <- X[tr, , drop = FALSE]; Xte <- X[te, , drop = FALSE]
      a <- args; a$.data <- as.data.frame(Xtr, check.names = FALSE)
      Est <- tryCatch(suppressWarnings(suppressMessages(do.call(csem, a))), error = function(e) NULL)
      bad <- is.null(Est) || sum(unlist(cSEM::verify(Est))) != 0
      if (bad && inadm == "stop")
        stop("Estimation in repetition ", r, ", fold ", j, " failed or gave an inadmissible result. Choose 'Ignore' or 'Set predictions to NA' for inadmissible results.", call. = FALSE)
      mu <- colMeans(Xtr); sdv <- apply(Xtr, 2, stats::sd)
      if (!is.null(Est) && (!bad || inadm == "ignore")) {
        Me <- Est$Information$Model
        W <- Est$Estimates$Weight_estimates; L <- Est$Estimates$Loading_estimates; Pm <- Est$Estimates$Path_estimates
        Xs <- sweep(sweep(Xte, 2, mu, "-"), 2, sdv, "/")[, colnames(W), drop = FALSE]
        ce <- Me$cons_endo; cx <- Me$cons_exo
        eta <- if (approach == "direct") {
          (Xs %*% t(W)) %*% t(Pm)[, ce, drop = FALSE]
        } else {
          B <- Pm[ce, ce, drop = FALSE]; G <- Pm[ce, cx, drop = FALSE]
          (Xs %*% t(W[cx, , drop = FALSE])) %*% t(G) %*% t(solve(diag(nrow(B)) - B))
        }
        Xh <- (eta %*% L[ce, , drop = FALSE])[, endo, drop = FALSE]
        Xh <- sweep(sweep(Xh, 2, sdv[endo], "*"), 2, mu[endo], "+")
        Pt[te, ] <- Xh
      }
      X1 <- cbind(1, Xtr[, exo, drop = FALSE])
      cf <- qr.coef(qr(X1), Xtr[, endo, drop = FALSE]); cf <- matrix(cf, ncol = nE); cf[is.na(cf)] <- 0
      Pl[te, ] <- cbind(1, Xte[, exo, drop = FALSE]) %*% cf
      Pi[te, ] <- matrix(colMeans(Xtr[, endo, drop = FALSE]), length(te), nE, byrow = TRUE)
      if (is.function(tick)) tick(((r - 1) * k + j) / (reps * k), sprintf("repetition %d / %d, fold %d / %d", r, reps, j, k))
    }
    for (nm in c("PLS", "LM", "IA")) {
      Pp <- switch(nm, PLS = Pt, LM = Pl, IA = Pi)
      S[[nm]] <- S[[nm]] + (A - Pp)^2 / reps
      M[[nm]][[r]] <- as.matrix(pred_metrics(Pp, A, Pi))
    }
  }
  met <- lapply(M, function(L) as.data.frame(Reduce(`+`, L) / length(L)))
  list(met = met, SE = S, blocks = blocks, endo = endo, N = N)
}

# Report tables of PLSpredict
pred_tables <- function(pr) {
  m <- pr$met; ep <- pr$endo
  T1 <- data.frame(` ` = ep, `Q2 predict` = m$PLS$Q2, `RMSE PLS` = m$PLS$RMSE, `RMSE LM` = m$LM$RMSE, `RMSE IA` = m$IA$RMSE,
                   `MAE PLS` = m$PLS$MAE, `MAE LM` = m$LM$MAE, `MAE IA` = m$IA$MAE, check.names = FALSE)
  T2 <- data.frame(` ` = ep,
                   `RMSE difference (PLS - LM)` = m$PLS$RMSE - m$LM$RMSE, `MAE difference (PLS - LM)` = m$PLS$MAE - m$LM$MAE,
                   `RMSE difference (PLS - IA)` = m$PLS$RMSE - m$IA$RMSE, `MAE difference (PLS - IA)` = m$PLS$MAE - m$IA$MAE, check.names = FALSE)
  T3 <- do.call(rbind, lapply(names(pr$blocks), function(cn) {
    b <- pr$blocks[[cn]]; n <- length(b); q <- sum(m$PLS[b, "Q2"] > 0, na.rm = TRUE); w <- sum(m$PLS[b, "RMSE"] < m$LM[b, "RMSE"], na.rm = TRUE)
    data.frame(` ` = cn, `Number of indicators` = n, `Indicators with Q2 predict > 0` = q, `Indicators with RMSE PLS < RMSE LM` = w,
               Conclusion = if (q == 0) "No predictive power (Q2 predict <= 0)" else if (w == n) "High predictive power"
                            else if (w > n / 2) "Medium predictive power" else if (w > 0) "Low predictive power" else "No predictive power",
               check.names = FALSE) }))
  T4 <- data.frame(` ` = ep, `MAPE PLS` = m$PLS$MAPE, `MAPE LM` = m$LM$MAPE, `U1 PLS` = m$PLS$U1, `U1 LM` = m$LM$U1,
                   `U2 PLS` = m$PLS$U2, `U2 LM` = m$LM$U2, `UM PLS` = m$PLS$UM, `UR PLS` = m$PLS$UR, `UD PLS` = m$PLS$UD, check.names = FALSE)
  list(`Prediction metrics (PLS vs LM and IA benchmarks)` = T1,
       `Prediction errors: PLS minus benchmark (negative = PLS predicts better)` = T2,
       `Predictive power assessment (Shmueli et al., 2019)` = T3,
       `Further prediction metrics (cSEM)` = T4)
}

# CVPAT (Liengaard et al., 2021): paired t test on the per-observation losses; same test statistic as cSEM::testCVPAT().
# Loss of an observation = squared prediction error averaged over the indicators of the group (all / one construct).
cvpat_tables <- function(pr, two = TRUE, alpha = 0.05) {
  grp <- c(list(Overall = pr$endo), pr$blocks)
  one <- function(SEb, bn) {
    d <- do.call(rbind, lapply(names(grp), function(nm) {
      v  <- grp[[nm]]
      Lt <- rowMeans(pr$SE$PLS[, v, drop = FALSE]); Lb <- rowMeans(SEb[, v, drop = FALSE])
      ok <- is.finite(Lt) & is.finite(Lb); Lt <- Lt[ok]; Lb <- Lb[ok]
      D <- Lb - Lt; n <- length(D); tt <- mean(D) / sqrt(stats::var(D) / n)
      p <- if (two) 2 * (1 - stats::pt(abs(tt), n - 1)) else 1 - stats::pt(tt, n - 1)
      cc <- if (!is.finite(p)) "" else if (p < alpha) { if (mean(D) > 0) "PLS predicts significantly better" else "PLS predicts significantly worse" } else "No significant difference"
      data.frame(nm, mean(Lt), mean(Lb), mean(D), tt, p, cc, stringsAsFactors = FALSE)
    }))
    names(d) <- c(" ", "PLS average loss", paste(bn, "average loss"), "Average loss difference", "T statistics", "P values", "Conclusion")
    d
  }
  list(`CVPAT: PLS vs indicator average (IA)` = one(pr$SE$IA, "IA"), `CVPAT: PLS vs linear model (LM)` = one(pr$SE$LM, "LM"))
}

# ---- parallel back end -------------------------------------------------------
# Self-contained environment holding the engine functions. Its parent is the global
# environment, so shipping it to a worker never drags the Shiny app along.
ENGINE_FUNS <- c("BS", "fit_one", "mvec", "ci_fun", "align_signs", "nz", "sq", "calc_fun", "spec_paths",
                 "spec_vals", "rel_fun", "ht_fun", "boot_stat", "boot_chunk")
make_engine <- function(src = environment(make_engine)) {
  env <- new.env(parent = globalenv())
  for (nm in ENGINE_FUNS) {
    obj <- get(nm, envir = src)
    if (is.function(obj)) environment(obj) <- env
    assign(nm, obj, envir = env)
  }
  env
}
on_global <- function(f) { environment(f) <- globalenv(); f }

default_workers <- function() {
  n <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)
  if (is.na(n)) n <- 2L
  max(1L, min(n - 1L, 8L))
}

# Start a PSOCK cluster with cSEM loaded and the engine installed. Returns NULL on failure.
start_workers <- function(n, engine) {
  tryCatch({
    cl <- parallel::makeCluster(n)
    parallel::clusterCall(cl, on_global(function() { suppressPackageStartupMessages(library(cSEM)); NULL }))
    parallel::clusterCall(cl, on_global(function(e) { assign(".eng", e, envir = globalenv()); NULL }), engine)
    cl
  }, error = function(e) NULL)
}

ENGINE <- make_engine()   # worker-safe copy of the engine, shipped to parallel workers

# ---- 4. Front-end assets -----------------------------------------------------
# ---------------- interactive model canvas (SVG, drag & drop) ----------------
# Used by the "Model builder" and "Results diagram" tabs. Constructs and indicators can be dragged,
# nudged with arrow buttons / keys, and the indicators of a construct can be put left / right / above / below.
# Both tabs share one layout, so the arrangement made in the builder carries over to the results.
SEM_CSS <- r"---(
.sp-ctr th, .sp-ctr td { text-align:center !important; vertical-align:middle !important; }
.sp-ctr th:first-child, .sp-ctr td:first-child { text-align:left !important; }
/* Result tables: every table has the SAME compact size (half page width, fixed height, same title bar / header / row height / font).
   Green = threshold met, red = threshold not met (reliability, validity, VIF, model fit). */
.sp-pls { display:block; width:50%; min-width:480px; max-width:100%; box-sizing:border-box; margin:8px 0 14px; border:1px solid #d9d9d9; background:#fff; font-family:"Segoe UI",Calibri,Arial,sans-serif; }
.sp-pls-title { display:flex; align-items:center; gap:10px; height:34px; box-sizing:border-box; background:#a8e6b4; color:#222; font-size:13px; font-weight:400; padding:0 10px; }
.sp-pls-title .sp-pls-t { flex:1 1 auto; min-width:0; overflow:hidden; text-overflow:ellipsis; white-space:nowrap; }
.sp-pls-title .sp-legend { flex:none; font-size:10.5px; font-weight:600; color:#1b5e46; background:#fff; border-radius:9px; padding:1px 8px; white-space:nowrap; }
.sp-pls-title .btn { flex:none; font-size:10.5px; padding:0 6px; line-height:18px; }
.sp-pls-scroll { overflow:auto; height:230px; }
.sp-pls-tab { border-collapse:collapse; table-layout:fixed; width:100%; font-size:12px; line-height:16px; }
.sp-pls-tab th { position:sticky; top:0; z-index:1; height:32px; box-sizing:border-box; background:#fff; color:#666; font-weight:700; padding:5px 8px; vertical-align:middle; white-space:normal; overflow-wrap:anywhere; border-bottom:1px solid #d9d9d9; }
.sp-pls-tab td { color:#000; background:#fff; padding:5px 8px; height:28px; box-sizing:border-box; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; border-bottom:0; }
.sp-pls-tab .sp-n { text-align:right; font-variant-numeric:tabular-nums; }
.sp-pls-tab .sp-l { text-align:left; }
.sp-pls-tab th:first-child, .sp-pls-tab td.sp-rl { width:150px; text-align:left; font-weight:700; color:#000; white-space:normal; overflow-wrap:anywhere; border-right:1px solid #d9d9d9; }
.sp-pls-tab td.sp-rl { background:#fff; }
.sp-pls-tab th:first-child { background:#fff; color:#666; z-index:2; }
.sp-pls-tab tbody tr:nth-child(even) td { background:#f2f2f2; }
.sp-pls-tab tbody tr:nth-child(even) td.sp-rl { background:#f2f2f2; }
.sp-pls-tab tbody tr:hover td { background:#e6f4ea; }
.sp-pls-tab tbody tr td.sp-ok  { color:#0a8a2a !important; font-weight:700; }
.sp-pls-tab tbody tr td.sp-bad { color:#d00000 !important; font-weight:700; }
.sp-top { display:flex; align-items:center; gap:10px; padding:8px 14px; margin:0 -15px 10px; background:#1f3a5f; color:#fff; }
.sp-brand { font-size:18px; font-weight:700; } .sp-sub { opacity:.75; font-size:12px; flex:1; }
.sp-settings { background:#f6f7f9; border:1px solid #d9dde3; border-radius:6px; padding:8px 14px 2px; margin-bottom:10px; }
.sp-work { display:flex; gap:10px; align-items:flex-start; }
.sp-left { width:250px; flex:none; border:1px solid #cfd6df; border-radius:6px; background:#fff; display:flex; flex-direction:column; height:720px; overflow:hidden; }
.sp-lhead { background:#99c9ff; padding:9px 10px; font-size:16px; font-weight:600; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
.sp-ldata { padding:6px 8px 0; border-bottom:1px solid #e3e6ea; font-size:12px; }
.sp-ldata .form-group { margin-bottom:4px; } .sp-ldata .checkbox { margin:2px 0 6px; }
.sp-left input.form-control { border-radius:0; border-left:0; border-right:0; }
.sv-list { flex:1; overflow:auto; }
.sv-row { display:flex; padding:5px 10px; font-size:13px; cursor:grab; user-select:none; }
.sv-row:nth-child(even) { background:#f3f3f3; } .sv-row.sel { background:#cfe3ff; }
.sv-row.used { color:#9aa0a8; } .sv-row.cur { outline:1px dotted #0d6efd; outline-offset:-2px; } .sv-list:focus { outline:none; box-shadow:inset 0 0 0 2px rgba(59,130,196,.3); } .sv-i { width:26px; color:#777; } .sv-n { flex:1; } .sv-t { font-size:11px; color:#555; }
.sv-empty { padding:14px 12px; color:#8a93a0; font-size:13px; }
.sv-foot { padding:6px 10px; font-size:11px; color:#666; border-top:1px solid #e3e6ea; }
.sp-main { flex:1; min-width:0; }
.sem-canvas { margin-bottom:12px; }
.sp-head { background:#99c9ff; padding:8px 12px; font-size:16px; font-weight:600; border-radius:6px 6px 0 0; border:1px solid #cfd6df; border-bottom:0; }
.sp-tools { display:flex; flex-wrap:wrap; gap:2px; padding:6px 8px; background:#fff; border:1px solid #cfd6df; border-bottom:0; }
.sp-tools:empty { display:none; }
.sp-tool { border:0; background:none; display:flex; flex-direction:column; align-items:center; width:92px; cursor:pointer; font-size:12px; color:#222; padding:2px 0; }
.sp-ico { width:38px; height:38px; border-radius:50%; background:#b8b8b8; color:#fff; font-size:20px; font-weight:700; display:flex; align-items:center; justify-content:center; margin-bottom:3px; }
.sp-tool:hover .sp-ico { background:#8f8f8f; } .sp-tool.on .sp-ico { background:#0d6efd; }
.sp-tool.warn .sp-ico { background:#c9a0a0; } .sp-sepv { width:1px; background:#d9dde3; margin:4px 8px; }
.sp-body { display:flex; border:1px solid #cfd6df; border-radius:0 0 6px 6px; background:#fff; }
.sem-stage { position:relative; flex:1; min-width:0; background:#fff; overflow:hidden; outline:none; }
.sem-stage:focus { box-shadow:inset 0 0 0 2px rgba(59,130,196,.25); }
.sem-stage[data-tool=lv] { cursor:crosshair; } .sem-stage[data-tool=connect] [data-id], .sem-stage[data-tool=mod] [data-id] { cursor:alias !important; }
.sem-svg { display:block; width:100%; height:100%; touch-action:none; user-select:none; -webkit-user-select:none; }
.sem-empty { position:absolute; inset:0; display:flex; align-items:center; justify-content:center; color:#8a93a0; font-size:14px; pointer-events:none; text-align:center; padding:0 20px; }
.sem-status { font-size:11px; color:#666; margin-top:4px; } .sem-status:empty { display:none; }
html, body { scrollbar-width:none; -ms-overflow-style:none; } html::-webkit-scrollbar, body::-webkit-scrollbar { display:none; width:0; height:0; }
.sp-panel { width:176px; flex:none; border-left:1px solid #e3e6ea; padding:8px 10px; overflow:auto; background:#fff; }
.sp-st { font-weight:700; font-size:14px; margin:6px 0 6px; } .sp-sec { margin-bottom:10px; }
.sp-cross { display:grid; grid-template-columns:repeat(3,44px); grid-template-rows:repeat(3,34px); gap:2px 4px; justify-content:center; margin-bottom:8px; }
.sp-rbtn { width:44px; height:36px; padding:0; border-radius:10px; border:1px solid #e3e6ea; background:#fff; box-shadow:0 1px 4px rgba(0,0,0,.28); display:flex; align-items:center; justify-content:center; cursor:pointer; }
.sp-rbtn:hover { background:#eaf2ff; box-shadow:0 1px 5px rgba(13,110,253,.5); } .sp-rbtn:active { transform:translateY(1px); box-shadow:0 0 2px rgba(0,0,0,.4); }
.sp-rbtn svg { display:block; } .sp-rot { width:100%; gap:8px; font-size:12.5px; font-weight:600; color:#111; margin-bottom:4px; height:38px; }
.sp-rot .sp-dot { width:24px; height:24px; border-radius:50%; background:#000; color:#fff; display:flex; align-items:center; justify-content:center; font-size:15px; line-height:1; }
.sp-in { width:100%; font-size:12px; padding:3px 5px; border:1px solid #b9c0ca; border-radius:4px; margin-bottom:4px; }
.sp-note { font-size:11px; color:#666; margin-top:2px; }
.sem-btn { border:1px solid #b9c0ca; background:#fff; border-radius:4px; padding:2px 8px; font-size:12px; line-height:18px; cursor:pointer; color:#222; margin:0 3px 4px 0; }
.sem-btn:hover { background:#eef3fa; } .sem-btn:disabled { opacity:.45; cursor:default; }
.sem-step { width:56px; font-size:12px; padding:1px 4px; border:1px solid #b9c0ca; border-radius:4px; height:24px; }
.sem-chk { display:flex; align-items:center; margin:0 0 3px; font-weight:400; font-size:12px; color:#333; cursor:pointer; }
.sem-chk input { margin:0 4px 0 0; }

.sp-btn-calc { font-size:15px !important; font-weight:700; padding:7px 18px !important; border-radius:8px !important; box-shadow:0 2px 6px rgba(0,0,0,.35); }
.sp-menu { min-width:310px; padding:8px 10px 10px; border-radius:12px; border:1px solid #cfd6df; box-shadow:0 12px 34px rgba(0,0,0,.35); }
.sp-menu .sp-mh { padding:6px 4px 4px; font-size:11.5px; font-weight:700; color:#6b7280; text-transform:uppercase; letter-spacing:.6px; }
.sp-menu .divider { margin:8px 2px; }
.sp-menu > li > a { display:flex !important; align-items:center; gap:10px; white-space:nowrap !important; padding:6px 10px !important; margin-bottom:5px; border:1px solid #d5dbe3; border-radius:9px; background:linear-gradient(#fff,#f3f6fa); color:#111; box-shadow:0 1px 2px rgba(0,0,0,.12); transition:all .12s; }
.sp-menu > li > a:hover, .sp-menu > li > a:focus { background:#0d6efd; border-color:#0a58ca; color:#fff; box-shadow:0 3px 8px rgba(13,110,253,.45); transform:translateY(-1px); }
.sp-menu > li > a strong { font-size:13.5px; font-weight:600; display:block; }
.sp-mi { flex:none; width:28px; height:28px; border-radius:8px; color:#fff; display:flex; align-items:center; justify-content:center; font-size:16px; line-height:1; box-shadow:inset 0 -2px 0 rgba(0,0,0,.18); }
.sp-dlg-bg { position:fixed; top:0; left:0; right:0; bottom:0; background:rgba(0,0,0,.45); z-index:2000; align-items:center; justify-content:center; }
.sp-dlg { background:#fff; width:680px; max-width:95vw; max-height:90vh; border-radius:8px; display:flex; flex-direction:column; box-shadow:0 10px 40px rgba(0,0,0,.35); }
.sp-dlg-h { background:#99c9ff; padding:10px 16px; font-size:17px; font-weight:600; border-radius:8px 8px 0 0; display:flex; justify-content:space-between; align-items:center; }
.sp-dlg-d { padding:10px 16px 4px; color:#444; font-size:13px; }
.sp-dlg-b { overflow:auto; padding:0 16px; display:flex; flex-direction:column; }
.sp-grp { border-top:1px solid #e3e6ea; padding-top:6px; margin-top:8px; }
.sp-grp h5 { margin:2px 0 8px; font-weight:700; } .sp-grp .help-block { font-size:11.5px; margin:-4px 0 10px; }
.sp-dlg-f { padding:10px 16px; border-top:1px solid #e3e6ea; text-align:right; } .sp-dlg-f .btn { margin-left:6px; }

/* ---- toolbar states: active tool = blue, Delete / Clear = red while hovered, pressed or just used ---- */
.sp-tool .sp-ico { transition:background .15s, transform .1s; }
.sp-tool.on .sp-ico { background:#0d6efd !important; box-shadow:0 0 0 3px rgba(13,110,253,.25); }
.sp-tool.on span:last-child { color:#0d6efd; font-weight:600; }
.sp-tool.warn:hover .sp-ico, .sp-tool.warn:active .sp-ico, .sp-tool.warn.hit .sp-ico { background:#dc3545 !important; box-shadow:0 0 0 3px rgba(220,53,69,.25); }
.sp-tool.warn:hover span:last-child, .sp-tool.warn.hit span:last-child { color:#dc3545; font-weight:600; }
.sp-tool:active .sp-ico { transform:scale(.94); }

/* ---- compact header buttons ---- */
.sp-btn-calc { font-size:13px !important; font-weight:600; padding:5px 14px !important; border-radius:6px !important; box-shadow:0 1px 3px rgba(0,0,0,.3); }

/* ---- Calculate menu ---- */
.sp-menu { min-width:270px; padding:6px 8px 8px; border-radius:10px; border:1px solid #d5dbe3; box-shadow:0 8px 24px rgba(0,0,0,.28); }
.sp-menu .sp-mh { padding:4px 4px 3px; font-size:10.5px; font-weight:700; color:#8a93a0; letter-spacing:.7px; }
.sp-menu .divider { margin:5px 2px; }
.sp-menu > li > a { gap:9px; padding:4px 8px !important; margin-bottom:3px; border-radius:7px; font-size:12.5px; box-shadow:none; }
.sp-menu > li > a:hover, .sp-menu > li > a:focus { box-shadow:0 2px 6px rgba(13,110,253,.35); transform:none; }
.sp-menu > li > a strong { font-size:12.5px; font-weight:600; }
.sp-mi { width:22px; height:22px; border-radius:6px; font-size:13px; box-shadow:none; }

/* ---- settings dialog ---- */
.sp-dlg { width:520px; max-width:94vw; border-radius:10px; font-size:13px; }
.sp-dlg-h { padding:9px 16px; font-size:15px; border-radius:10px 10px 0 0; }
.sp-dlg-d { padding:10px 16px 6px; font-size:12px; line-height:1.45; color:#4b5563; }
.sp-dlg-b { padding:0 16px 6px; }
.sp-grp { padding-top:8px; margin-top:6px; }
.sp-grp h5 { font-size:13px; font-weight:700; margin:0 0 8px; color:#1f3a5f; text-transform:uppercase; letter-spacing:.4px; }
.sp-dlg .form-group { margin-bottom:6px; }
.sp-dlg label.control-label, .sp-dlg .control-label { font-size:12.5px; font-weight:600; margin-bottom:2px; color:#222; }
.sp-dlg .form-control, .sp-dlg select { height:30px; padding:3px 9px; font-size:13px; border-radius:5px; box-shadow:none; max-width:260px; }
.sp-dlg .help-block, .sp-grp .help-block { font-size:11px; line-height:1.35; color:#7b8494; margin:2px 0 10px; }
.sp-dlg .checkbox { margin:4px 0 2px; } .sp-dlg .checkbox label { font-size:12.5px; font-weight:600; }
.sp-dlg-f { padding:8px 16px; } .sp-dlg-f .btn { font-size:12.5px; padding:4px 14px; border-radius:5px; }
)---"

SEM_JS <- r"---(
(function () {
  'use strict';

  var NS = 'http://www.w3.org/2000/svg';
  var ITEM_H = 22, ITEM_GAP = 8, V_GAP = 56, H_GAP = 70, COL_PAD = 70, ROW_PAD = 34, PER_ROW = 4, GRID = 10;
  var C_FILL = '#3399ff', C_STROKE = '#3399ff', I_FILL = '#ffff66', I_STROKE = '#ffff66', SEL = '#d9480f', LBL = 18;
  var FONT = 'Helvetica, Arial, sans-serif';

  // one layout shared by every canvas: pos[id] = {x, y}; side[constructId] = left|right|up|down
  var layout = { pos: {}, side: {} };
  var canvases = {}, latest = {}, uid = 0;
  var hidden = {};   // constructs whose indicators are hidden (Alt+X hides, Alt+C shows the selected constructs; shared by every canvas)

  // ---------- small helpers ----------
  function arr(x) { return x === null || x === undefined ? [] : (Array.isArray(x) ? x : [x]); }
  function snapv(v) { return Math.round(v / GRID) * GRID; }
  function mk(tag, attrs, parent, text) {
    var e = document.createElementNS(NS, tag);
    if (attrs) Object.keys(attrs).forEach(function (k) { if (attrs[k] !== null && attrs[k] !== undefined) e.setAttribute(k, attrs[k]); });
    if (text !== undefined && text !== null) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }
  function h(tag, props, parent, text) {
    var e = document.createElement(tag);
    if (props) Object.keys(props).forEach(function (k) { if (k === 'class') e.className = props[k]; else e.setAttribute(k, props[k]); });
    if (text !== undefined) e.textContent = text;
    if (parent) parent.appendChild(e);
    return e;
  }

  // ---------- model -> graph ----------
  function buildGraph(model) {
    var g = { cons: {}, order: [], paths: [], results: model.mode === 'results' };
    function ensure(name) {
      name = String(name);
      if (!g.cons[name]) { g.cons[name] = { id: name, type: 'cf', sub: '', items: [] }; g.order.push(name); }
      return g.cons[name];
    }
    arr(model.cons).forEach(function (k) {
      var c = ensure(k.name);
      c.type = k.type || 'cf';
      c.sub = k.sub ? String(k.sub) : '';
      c.parts = k.parts ? arr(k.parts).map(String) : (c.type === 'int' ? c.id.split('.') : undefined);
      c.items = arr(k.items).map(function (it) {
        var o = (it !== null && typeof it === 'object') ? it : { n: it };
        return { id: c.id + '|' + o.n, name: String(o.n), label: (o.l === null || o.l === undefined) ? '' : String(o.l), con: c.id };
      });
    });
    arr(model.paths).forEach(function (p) {
      ensure(p.from); ensure(p.to);
      g.paths.push({ from: String(p.from), to: String(p.to), label: (p.l === null || p.l === undefined) ? '' : String(p.l) });
    });
    return g;
  }

  function conSize(c) { if (c.type === 'int') return { rx: Math.max(34, 4.2 * c.id.length + 14), ry: 20 }; return { rx: 28, ry: 28 }; }
  function itemSize(it) { return { w: Math.max(56, it.name.length * 6 + 12), h: ITEM_H }; }

  // offsets (relative to the construct centre) of all indicators of a construct for a given side
  function blockOffsets(c, side) {
    var n = c.items.length, out = new Array(n), s = conSize(c), sz = c.items.map(itemSize), i;
    if (!n) return out;
    if (side === 'up' || side === 'down') {
      var dir = side === 'down' ? 1 : -1, rows = Math.ceil(n / PER_ROW), per = Math.ceil(n / rows);
      for (var r = 0; r < rows; r++) {
        var from = r * per, to = Math.min(n, from + per), total = ITEM_GAP * (to - from - 1);
        for (i = from; i < to; i++) total += sz[i].w;
        var x = -total / 2, dy = dir * (s.ry + V_GAP + ITEM_H / 2 + r * (ITEM_H + ITEM_GAP)) + (dir > 0 ? LBL : 0);
        for (i = from; i < to; i++) { out[i] = { dx: x + sz[i].w / 2, dy: dy, w: sz[i].w, h: ITEM_H }; x += sz[i].w + ITEM_GAP; }
      }
    } else {
      var d2 = side === 'right' ? 1 : -1, mw = 0;
      sz.forEach(function (z) { mw = Math.max(mw, z.w); });
      var tot = n * ITEM_H + (n - 1) * ITEM_GAP;
      for (i = 0; i < n; i++) out[i] = { dx: d2 * (s.rx + H_GAP + mw / 2), dy: -tot / 2 + ITEM_H / 2 + i * (ITEM_H + ITEM_GAP), w: sz[i].w, h: ITEM_H };
    }
    return out;
  }
  function bboxOf(c, side) {
    var s = conSize(c), b = { l: -s.rx, r: s.rx, t: -s.ry, b: s.ry + (c.type === 'int' ? 0 : LBL) };
    blockOffsets(c, side).forEach(function (o) {
      b.l = Math.min(b.l, o.dx - o.w / 2); b.r = Math.max(b.r, o.dx + o.w / 2);
      b.t = Math.min(b.t, o.dy - o.h / 2); b.b = Math.max(b.b, o.dy + o.h / 2);
    });
    return b;
  }
  function placeBlock(c, side) {
    var p = layout.pos[c.id]; if (!p) return;
    layout.side[c.id] = side;
    blockOffsets(c, side).forEach(function (o, i) { layout.pos[c.items[i].id] = { x: p.x + o.dx, y: p.y + o.dy }; });
  }

  // ---------- automatic layout: columns by path depth, indicators around each construct ----------
  function autoLayout(g) {
    var ids = g.order.filter(function (id) { return g.cons[id].type !== 'int'; }), n = ids.length, rank = {}, side = {}, pos = {};
    var gpaths = g.paths.filter(function (p) { return g.cons[p.from] && g.cons[p.to] && g.cons[p.from].type !== 'int' && g.cons[p.to].type !== 'int'; });
    ids.forEach(function (id) { rank[id] = 0; });
    for (var it = 0; it < n; it++) {
      var ch = false;
      gpaths.forEach(function (p) { if (rank[p.from] < n && rank[p.to] < rank[p.from] + 1) { rank[p.to] = rank[p.from] + 1; ch = true; } });
      if (!ch) break;
    }
    var hasPaths = false;
    ids.forEach(function (id) { if (rank[id] > 0) hasPaths = true; });
    if (!hasPaths) ids.forEach(function (id, i) { rank[id] = Math.floor(i / 3); });   // no paths yet: columns of three
    var last = 0; ids.forEach(function (id) { last = Math.max(last, rank[id]); });
    var cols = [];
    ids.forEach(function (id) { (cols[rank[id]] = cols[rank[id]] || []).push(id); });
    var cursor = 0, infos = [], maxH = 0, ncols = cols.filter(Boolean).length;
    cols.forEach(function (col, r) {
      if (!col) return;
      col.forEach(function (id, i) {
        side[id] = !hasPaths ? 'down' : (r === 0 ? 'left' : (r === last ? 'right' : (col.length === 1 ? (ncols >= 3 && infos.length % 2 === 0 ? 'up' : 'down') : (i < col.length / 2 ? 'up' : 'down'))));
      });
      var bbs = col.map(function (id) { return bboxOf(g.cons[id], side[id]); });
      var colL = 0, colR = 0;
      bbs.forEach(function (b) { colL = Math.max(colL, -b.l); colR = Math.max(colR, b.r); });
      var X = cursor + colL; cursor += colL + colR + COL_PAD;
      var y = 0, ys = [];
      bbs.forEach(function (b) { var cy = y - b.t; ys.push(cy); y = cy + b.b + ROW_PAD; });
      var H = y - ROW_PAD; maxH = Math.max(maxH, H);
      infos.push({ col: col, X: X, ys: ys, H: H });
    });
    infos.forEach(function (inf, ci) {
      var off = (maxH - inf.H) / 2;
      if (infos.length >= 3 && inf.col.length === 1) off += (ci % 2 ? 1 : -1) * 55;   // zig-zag chains so long paths stay visible
      inf.col.forEach(function (id, i) {
        pos[id] = { x: inf.X, y: inf.ys[i] + off };
        var c = g.cons[id];
        blockOffsets(c, side[id]).forEach(function (o, k) { pos[c.items[k].id] = { x: pos[id].x + o.dx, y: pos[id].y + o.dy }; });
      });
    });
    return { pos: pos, side: side };
  }

  function conRect(c, p, side) { var b = bboxOf(c, side); return { x0: p.x + b.l, x1: p.x + b.r, y0: p.y + b.t, y1: p.y + b.b }; }
  function overlaps(a, b, m) { return a.x0 < b.x1 + m && a.x1 > b.x0 - m && a.y0 < b.y1 + m && a.y1 > b.y0 - m; }

  // make sure every node of the graph has a position; optionally drop positions of vanished nodes
  function sync(g, prune) {
    if (prune) {
      var valid = {};
      g.order.forEach(function (id) { valid[id] = 1; g.cons[id].items.forEach(function (it) { valid[it.id] = 1; }); });
      Object.keys(layout.pos).forEach(function (k) { if (!valid[k]) delete layout.pos[k]; });
      Object.keys(layout.side).forEach(function (k) { if (!g.cons[k]) delete layout.side[k]; });
    }
    var fresh = g.order.filter(function (id) { return !layout.pos[id] && g.cons[id].type !== 'int'; });
    if (fresh.length) {
      var auto = autoLayout(g), placed = g.order.filter(function (id) { return layout.pos[id]; });
      fresh.forEach(function (id) {
        var c = g.cons[id], p = { x: auto.pos[id].x, y: auto.pos[id].y }, sd = auto.side[id];
        if (placed.length) {                                  // model already has nodes: slide the new one into free space
          var others = placed.map(function (o) { return conRect(g.cons[o], layout.pos[o], layout.side[o] || 'down'); }), tries = 0;
          while (tries++ < 120 && others.some(function (o) { return overlaps(conRect(c, p, sd), o, 12); })) p.y += 40;
        }
        layout.pos[id] = p; layout.side[id] = sd; placeBlock(c, sd); placed.push(id);
      });
    }
    g.order.forEach(function (id) {
      var c = g.cons[id];
      if (c.items.some(function (it) { return !layout.pos[it.id]; })) placeBlock(c, layout.side[id] || 'down');
    });
  }

  function worldBox(g) {
    var b = { x0: Infinity, y0: Infinity, x1: -Infinity, y1: -Infinity };
    function upd(x0, y0, x1, y1) { b.x0 = Math.min(b.x0, x0); b.y0 = Math.min(b.y0, y0); b.x1 = Math.max(b.x1, x1); b.y1 = Math.max(b.y1, y1); }
    g.order.forEach(function (id) {
      var p = layout.pos[id]; if (!p || g.cons[id].type === 'int') return;
      var s = conSize(g.cons[id]); upd(p.x - s.rx, p.y - s.ry, p.x + s.rx, p.y + s.ry + LBL);
      if (!hidden[id]) g.cons[id].items.forEach(function (it) {
        var q = layout.pos[it.id]; if (!q) return;
        var z = itemSize(it); upd(q.x - z.w / 2, q.y - z.h / 2, q.x + z.w / 2, q.y + z.h / 2);
      });
    });
    return b.x0 === Infinity ? null : b;
  }

  // ---------- drawing ----------
  function ellipseEdge(s, p, tx, ty) {
    var dx = tx - p.x, dy = ty - p.y;
    if (!dx && !dy) return { x: p.x, y: p.y };
    var k = 1 / Math.sqrt(dx * dx / (s.rx * s.rx) + dy * dy / (s.ry * s.ry));
    return { x: p.x + dx * k, y: p.y + dy * k };
  }
  function rectEdge(z, p, tx, ty) {
    var dx = tx - p.x, dy = ty - p.y;
    if (!dx && !dy) return { x: p.x, y: p.y };
    var k = Math.min(dx ? (z.w / 2) / Math.abs(dx) : Infinity, dy ? (z.h / 2) / Math.abs(dy) : Infinity);
    return { x: p.x + dx * k, y: p.y + dy * k };
  }
  function addDefs(svg, mid) {
    var d = mk('defs', null, svg);
    [['m', '#111', 7], ['p', '#111', 9]].forEach(function (m) {
      var mm = mk('marker', { id: mid + m[0], viewBox: '0 0 10 10', refX: '10', refY: '5', markerWidth: m[2], markerHeight: m[2], markerUnits: 'userSpaceOnUse', orient: 'auto' }, d);
      mk('path', { d: 'M0,0 L10,5 L0,10 z', fill: m[1] }, mm);
    });
    var pt = mk('pattern', { id: mid + 'grid', width: GRID * 2, height: GRID * 2, patternUnits: 'userSpaceOnUse' }, d);
    mk('circle', { cx: 0, cy: 0, r: 1, fill: '#c4cad3' }, pt);
  }

  function draw(root, g, sel, mid) {
    while (root.firstChild) root.removeChild(root.firstChild);
    var eg = mk('g', null, root), lg = mk('g', null, root), ng = mk('g', null, root);

    function conNode(c) {
      var p = layout.pos[c.id], s = conSize(c);
      return p ? { p: p, edge: function (tx, ty) { return ellipseEdge(s, p, tx, ty); } } : null;
    }
    function itemNode(it) {
      var q = layout.pos[it.id], z = itemSize(it);
      return q ? { p: q, edge: function (tx, ty) { return rectEdge(z, q, tx, ty); } } : null;
    }
    function line(A, B, label, kind, t, pid) {
      if (!A || !B) return;
      var a = A.edge(B.p.x, B.p.y), b = B.edge(A.p.x, A.p.y), pth = kind === 'p';
      if (Math.abs(a.x - b.x) + Math.abs(a.y - b.y) < 2) return;
      var psel = pid && sel[pid];
      mk('line', { x1: a.x, y1: a.y, x2: b.x, y2: b.y, stroke: psel ? SEL : '#111', 'stroke-width': psel ? 3 : (pth ? 1.8 : 1), 'marker-end': 'url(#' + mid + kind + ')' }, eg);
      if (pid) mk('line', { x1: a.x, y1: a.y, x2: b.x, y2: b.y, stroke: 'transparent', 'stroke-width': 14, 'data-id': pid, style: 'cursor:pointer' }, eg);
      if (label) mk('text', {
        x: a.x + (b.x - a.x) * (t || 0.5), y: a.y + (b.y - a.y) * (t || 0.5), 'text-anchor': 'middle', 'dominant-baseline': 'central',
        'font-size': pth ? 12 : 10, 'font-family': FONT, 'font-weight': pth ? 700 : 400, fill: pth ? '#111' : '#444',
        stroke: '#fff', 'stroke-width': 3, 'stroke-linejoin': 'round', 'paint-order': 'stroke', 'pointer-events': 'none'
      }, lg, label);
    }

    g.order.forEach(function (id) {
      var c = g.cons[id], cn = conNode(c);
      if (!cn || hidden[id]) return;
      c.items.forEach(function (it) {
        var inode = itemNode(it);
        if (c.type === 'cf') line(cn, inode, it.label, 'm', 0.7); else line(inode, cn, it.label, 'm', 0.3);   // labels sit near the indicator, where the lines fan out
      });
    });
    function modLine(p) {
      var ic = g.cons[p.from], A = conNode(g.cons[ic.parts[0]]), M = conNode(g.cons[ic.parts[1]]), B = conNode(g.cons[p.to]);
      if (!A || !M || !B) return;
      var a = A.edge(B.p.x, B.p.y), b = B.edge(A.p.x, A.p.y), mx = (a.x + b.x) / 2, my = (a.y + b.y) / 2, s0 = M.edge(mx, my), pid = 'path:' + p.from + '>' + p.to, on = sel[pid];
      if (ic.parts[0] === ic.parts[1]) {      // quadratic effect: curved dashed arrow from the construct onto its own path
        var dx = b.x - a.x, dy = b.y - a.y, Ln = Math.sqrt(dx * dx + dy * dy) || 1, cx = (a.x + mx) / 2 - dy / Ln * 50, cy = (a.y + my) / 2 + dx / Ln * 50;
        var q0 = A.edge(cx, cy), dd = 'M' + q0.x + ' ' + q0.y + ' Q' + cx + ' ' + cy + ' ' + mx + ' ' + my;
        mk('path', { d: dd, fill: 'none', stroke: on ? SEL : '#2f6060', 'stroke-width': on ? 3 : 1.6, 'stroke-dasharray': '5 3', 'marker-end': 'url(#' + mid + 'm)' }, eg);
        mk('path', { d: dd, fill: 'none', stroke: 'transparent', 'stroke-width': 14, 'data-id': pid, style: 'cursor:pointer' }, eg);
        mk('circle', { cx: mx, cy: my, r: 4.5, fill: on ? SEL : '#9a9a9a', stroke: '#fff', 'stroke-width': 1, 'pointer-events': 'none' }, lg);
        mk('text', { x: 0.25 * q0.x + 0.5 * cx + 0.25 * mx, y: 0.25 * q0.y + 0.5 * cy + 0.25 * my, 'text-anchor': 'middle', 'dominant-baseline': 'central', 'font-size': 12, 'font-family': FONT,
          'font-weight': 700, fill: '#1d4444', stroke: '#fff', 'stroke-width': 3, 'stroke-linejoin': 'round', 'paint-order': 'stroke', 'pointer-events': 'none' }, lg, p.label || (ic.parts[0] + '\u00B2'));
        return;
      }
      mk('line', { x1: s0.x, y1: s0.y, x2: mx, y2: my, stroke: on ? SEL : '#2f6060', 'stroke-width': on ? 3 : 1.6, 'stroke-dasharray': '5 3', 'marker-end': 'url(#' + mid + 'm)' }, eg);
      mk('line', { x1: s0.x, y1: s0.y, x2: mx, y2: my, stroke: 'transparent', 'stroke-width': 14, 'data-id': pid, style: 'cursor:pointer' }, eg);
      mk('circle', { cx: mx, cy: my, r: 4.5, fill: on ? SEL : '#9a9a9a', stroke: '#fff', 'stroke-width': 1, 'pointer-events': 'none' }, lg);
      if (p.label) mk('text', { x: s0.x + (mx - s0.x) * 0.5, y: s0.y + (my - s0.y) * 0.5, 'text-anchor': 'middle', 'dominant-baseline': 'central', 'font-size': 12, 'font-family': FONT, 'font-weight': 700, fill: '#1d4444',
        stroke: '#fff', 'stroke-width': 3, 'stroke-linejoin': 'round', 'paint-order': 'stroke', 'pointer-events': 'none' }, lg, p.label);
    }
    function moderated(p) { return g.paths.some(function (q) { var ic = g.cons[q.from]; return ic && ic.type === 'int' && ic.parts && ic.parts[0] === p.from && q.to === p.to; }); }
    g.paths.forEach(function (p) {
      var fc = g.cons[p.from];
      if (fc && fc.type === 'int') { if (fc.parts && fc.parts.length === 2) modLine(p); return; }
      line(conNode(g.cons[p.from]), conNode(g.cons[p.to]), p.label, 'p', moderated(p) ? 0.3 : undefined, 'path:' + p.from + '>' + p.to);
    });

    g.order.forEach(function (id) {
      var c = g.cons[id], p = layout.pos[id]; if (!p || c.type === 'int') return;
      var s = conSize(c), on = !!sel[id];
      mk('ellipse', { cx: p.x, cy: p.y, rx: s.rx, ry: s.ry, fill: c.type === 'int' ? '#e6dcff' : C_FILL, stroke: on ? SEL : 'none', 'stroke-width': 3, 'data-id': id, style: 'cursor:move' }, ng);
      mk('text', { x: p.x, y: p.y + s.ry + 11, 'text-anchor': 'middle', 'dominant-baseline': 'central', 'font-size': 12, 'font-weight': 700, 'font-family': FONT, fill: '#000', 'pointer-events': 'none' }, ng, c.id);
      if (c.sub) mk('text', { x: p.x, y: p.y, 'text-anchor': 'middle', 'dominant-baseline': 'central', 'font-size': 10, 'font-family': FONT, fill: '#fff', 'pointer-events': 'none' }, ng, c.sub);
      if (!hidden[id]) c.items.forEach(function (it) {
        var q = layout.pos[it.id]; if (!q) return;
        var z = itemSize(it), on2 = !!sel[it.id];
        mk('rect', { x: q.x - z.w / 2, y: q.y - z.h / 2, width: z.w, height: z.h, rx: 4, fill: I_FILL, stroke: on2 ? SEL : 'none', 'stroke-width': 2.5, 'data-id': it.id, style: 'cursor:move' }, ng);
        mk('text', { x: q.x, y: q.y, 'text-anchor': 'middle', 'dominant-baseline': 'central', 'font-size': 11, 'font-family': FONT, fill: '#000', 'pointer-events': 'none' }, ng, it.name);
      });
    });
  }


  // ---------- variable list (left panel) ----------
  var vars = [], vsel = {}, vlast = -1, vcur = -1, dragNames = [];
  var SIDES = ['left', 'up', 'right', 'down'];
  var HINTS = {
    select: 'Drag to move \u2022 Shift+click selects several \u2022 arrow keys nudge \u2022 Del removes \u2022 double-click a construct to rename \u2022 wheel zooms \u2022 drag the background to pan',
    lv: 'Click on the canvas to place a new latent variable',
    connect: 'Drag from one construct to another \u2014 or click the source, then the target',
    mod: 'Drag from the moderating construct onto a path (arrow) \u2014 or click the path, then the moderator'
  };
  function usedSet() {
    var u = {}, cv = canvases.mb;
    if (cv) cv.model.cons.forEach(function (c) { c.items.forEach(function (i) { u[i] = c.name; }); });
    return u;
  }
  function visIdx() {
    var fi = document.getElementById('sem_vfilter'), q = fi ? fi.value.toLowerCase() : '', out = [];
    vars.forEach(function (v, i) { if (!q || v.n.toLowerCase().indexOf(q) >= 0) out.push(i); });
    return out;
  }
  function rangeSel(a, b, keep) {
    var vis = visIdx(), pa = vis.indexOf(a), pb = vis.indexOf(b);
    if (pa < 0) pa = pb; if (pb < 0) pb = pa; if (pa < 0) return;
    if (!keep) vsel = {};
    for (var k = Math.min(pa, pb); k <= Math.max(pa, pb); k++) vsel[vars[vis[k]].n] = 1;
  }
  function varKey(e) {
    var k = e.key, ctrl = e.ctrlKey || e.metaKey, vis = visIdx();
    if (!vars.length || !vis.length) return;
    if (k === 'ArrowDown' || k === 'ArrowUp') {
      var pos = vis.indexOf(vcur), np = pos < 0 ? 0 : Math.max(0, Math.min(vis.length - 1, pos + (k === 'ArrowDown' ? 1 : -1))), ni = vis[np];
      if (e.shiftKey) { if (vlast < 0 || vis.indexOf(vlast) < 0) vlast = vcur >= 0 ? vcur : ni; rangeSel(vlast, ni, ctrl); }
      else if (ctrl) { /* move the cursor only, selection stays (Space toggles) */ }
      else { vsel = {}; vsel[vars[ni].n] = 1; vlast = ni; }
      vcur = ni;
    } else if (k === ' ' && ctrl) {
      if (vcur < 0) vcur = vis[0];
      var nm = vars[vcur].n; if (vsel[nm]) delete vsel[nm]; else vsel[nm] = 1; vlast = vcur;
    } else if ((k === 'a' || k === 'A') && ctrl) {
      vsel = {}; vis.forEach(function (i) { vsel[vars[i].n] = 1; });
    } else if (k === 'Home' || k === 'End') {
      var ni2 = k === 'Home' ? vis[0] : vis[vis.length - 1];
      if (e.shiftKey) { if (vlast < 0) vlast = vcur >= 0 ? vcur : ni2; rangeSel(vlast, ni2, ctrl); } else { vsel = {}; vsel[vars[ni2].n] = 1; vlast = ni2; }
      vcur = ni2;
    } else if (k === 'Escape') { vsel = {}; }
    else return;
    e.preventDefault(); renderVars();
    var cur = document.querySelector('#sem_vars .sv-row.cur'); if (cur && cur.scrollIntoView) cur.scrollIntoView({ block: 'nearest' });
  }
  function renderVars() {
    var box = document.getElementById('sem_vars'); if (!box) return false;
    if (!box._kb) { box._kb = true; box.setAttribute('tabindex', '0'); box.addEventListener('keydown', varKey); }
    var fi = document.getElementById('sem_vfilter'), q = fi ? fi.value.toLowerCase() : '', used = usedSet();
    box.innerHTML = '';
    if (!vars.length) { h('div', { class: 'sv-empty' }, box, 'Upload a CSV file or tick "Use cSEM demo data" to list the variables here.'); return true; }
    vars.forEach(function (v, i) {
      if (q && v.n.toLowerCase().indexOf(q) < 0) return;
      var row = h('div', { class: 'sv-row' + (vsel[v.n] ? ' sel' : '') + (used[v.n] ? ' used' : '') + (i === vcur ? ' cur' : ''), draggable: 'true',
                           title: used[v.n] ? 'Used in ' + used[v.n] : 'Drag onto the canvas (Shift/Ctrl+click or Shift/Ctrl+arrow keys select several)' }, box);
      h('span', { class: 'sv-i' }, row, String(i)); h('span', { class: 'sv-n' }, row, v.n); h('span', { class: 'sv-t' }, row, v.t);
      row.addEventListener('click', function (e) {
        var ctrl = e.ctrlKey || e.metaKey;
        if (e.shiftKey && vlast >= 0) rangeSel(vlast, i, ctrl);
        else if (ctrl) { if (vsel[v.n]) delete vsel[v.n]; else vsel[v.n] = 1; vlast = i; }
        else { vsel = {}; vsel[v.n] = 1; vlast = i; }
        vcur = i; renderVars(); box.focus({ preventScroll: true });
      });
      row.addEventListener('dragstart', function (e) {
        if (!vsel[v.n]) { vsel = {}; vsel[v.n] = 1; vlast = i; vcur = i; }
        dragNames = vars.filter(function (x) { return vsel[x.n]; }).map(function (x) { return x.n; });
        e.dataTransfer.setData('text/plain', dragNames.join(',')); e.dataTransfer.effectAllowed = 'copy';
      });
      row.addEventListener('dragend', function () { setTimeout(function () { dragNames = []; }, 0); });
    });
    return true;
  }
  function emit(model) {
    if (!(window.Shiny && typeof window.Shiny.setInputValue === 'function')) return;
    window.Shiny.setInputValue('sem_state', JSON.stringify({
      cons: model.cons.map(function (k) { return { name: k.name, type: k.type, items: k.items }; }),
      paths: model.paths.map(function (p) { return { from: p.from, to: p.to }; })
    }), { priority: 'event' });
  }
  function renameKey(o, n) {
    ['pos', 'side'].forEach(function (w) {
      var src = layout[w], dst = {};
      Object.keys(src).forEach(function (k) { dst[k === o ? n : (k.indexOf(o + '|') === 0 ? n + k.slice(o.length) : k)] = src[k]; });
      layout[w] = dst;
    });
  }

  // ---------- canvas component ----------
  function Canvas(target, host) {
    this.target = target; this.host = host; this.editable = target === 'mb';
    this.model = { cons: [], paths: [] };
    this.g = { cons: {}, order: [], paths: [] };
    this.sel = {}; this.follow = true; this.snap = false; this.gsnap = true; this.guides = []; this.autoFit = !this.editable; this.drag = null;
    this.tool = 'select'; this.pending = null; this.rub = null; this.undo = []; this.redo = [];
    this.view = { x: 0, y: 0, w: 1000, h: 500 };
    this.mid = 'semarr' + (++uid) + '_';
    this.build();
  }
  var P = Canvas.prototype;

  P.build = function () {
    var self = this, host = this.host, ed = this.editable;
    host.innerHTML = '';
    h('div', { class: 'sp-head' }, host, host.getAttribute('data-title') || '');
    var tl = h('div', { class: 'sp-tools' }, host);
    this.toolBtns = {};
    function tool(label, ico, title, fn, cls) {
      var b = h('button', { type: 'button', class: 'sp-tool ' + (cls || ''), title: title }, tl);
      h('span', { class: 'sp-ico' }, b, ico); h('span', null, b, label);
      b.addEventListener('click', function (e) {
        e.preventDefault(); fn();
        if (cls === 'warn') { b.classList.add('hit'); setTimeout(function () { b.classList.remove('hit'); }, 450); }   // red flash for Delete / Clear
      });
      return b;
    }
    if (ed) {
      [['select', 'Select', '\u2196'], ['lv', 'Latent variable', '\u25CB'], ['connect', 'Connect', '\u2192'], ['mod', 'Moderator', '\u00D7']].forEach(function (d) {
        self.toolBtns[d[0]] = tool(d[1], d[2], HINTS[d[0]], function () { self.setTool(d[0]); });
      });
      h('div', { class: 'sp-sepv' }, tl);
      tool('Undo', '\u21B6', 'Undo (Ctrl+Z)', function () { self.undoOp(); });
      tool('Redo', '\u21B7', 'Redo (Ctrl+Y)', function () { self.redoOp(); });
      tool('Delete', '\u2715', 'Delete the selected construct, indicator or path (Del)', function () { self.delSel(); }, 'warn');
      tool('Clear', '\u2205', 'Remove the whole model', function () { self.clearAll(); }, 'warn');
      if (self.toolBtns.select) self.toolBtns.select.classList.add('on');   // default tool is highlighted from the start
    }
    var body = h('div', { class: 'sp-body' }, host), sh = (host.getAttribute('data-height') || '520') + 'px';
    this.stage = h('div', { class: 'sem-stage', tabindex: '0', 'data-tool': 'select' }, body);
    this.stage.style.height = sh;
    var pn = h('div', { class: 'sp-panel' }, body); pn.style.height = sh;
    function sec(t) { var d = h('div', { class: 'sp-sec' }, pn); h('div', { class: 'sp-st' }, d, t); return d; }
    function rb(parent, txt, title, fn, cls) {
      var b = h('button', { type: 'button', class: cls || 'sem-btn', title: title || '' }, parent, txt);
      b.addEventListener('click', function (e) { e.preventDefault(); fn(); });
      return b;
    }
    function chk(parent, text, init, fn) {
      var lab = h('label', { class: 'sem-chk' }, parent), cb = h('input', { type: 'checkbox' }, lab);
      cb.checked = init; lab.appendChild(document.createTextNode(text));
      cb.addEventListener('change', function () { fn(cb.checked); });
    }
    var s1 = sec('Indicators'), cr = h('div', { class: 'sp-cross' }, s1);
    function chev(deg) {
      var sv = mk('svg', { viewBox: '0 0 28 28', width: 26, height: 26 });
      mk('circle', { cx: 14, cy: 14, r: 12, fill: '#000' }, sv);
      mk('polyline', { points: '16.5,8.5 11,14 16.5,19.5', fill: 'none', stroke: '#fff', 'stroke-width': 3, 'stroke-linecap': 'round', 'stroke-linejoin': 'round', transform: 'rotate(' + deg + ' 14 14)' }, sv);
      return sv;
    }
    [['up', 90, 2, 1, 'Indicators above the construct'], ['left', 0, 1, 2, 'Indicators on the left'], ['right', 180, 3, 2, 'Indicators on the right'], ['down', 270, 2, 3, 'Indicators below the construct']].forEach(function (d) {
      var b = rb(cr, '', d[4], function () { self.setSide(d[0]); }, 'sp-rbtn');
      b.style.gridColumn = d[2]; b.style.gridRow = d[3]; b.appendChild(chev(d[1]));
    });
    var rot = rb(s1, '', 'Rotate the indicators of the selected construct(s) clockwise (all constructs if nothing is selected)', function () { self.rotate(); }, 'sp-rbtn sp-rot');
    h('span', { class: 'sp-dot' }, rot, '\u27F3'); h('span', null, rot, 'Rotate');
    if (ed) {
      var s2 = sec('Selected construct');
      this.pName = h('input', { type: 'text', class: 'sp-in', placeholder: '(select a construct)' }, s2);
      this.pType = h('select', { class: 'sp-in' }, s2);
      [['cf', 'Reflective (common factor)'], ['A', 'Composite - Mode A'], ['B', 'Composite - Mode B (formative)']].forEach(function (o) { h('option', { value: o[0] }, self.pType, o[1]); });
      this.pInfo = h('div', { class: 'sp-note' }, s2);
      this.pName.addEventListener('change', function () { var k = self.selCon(); if (k) self.rename(k, self.pName.value); });
      this.pType.addEventListener('change', function () { var k = self.selCon(); if (k) self.setType(k, self.pType.value); });
    }
    var s3 = sec('Move selected');
    rb(s3, '\u2190', 'Move left', function () { self.nudge(-1, 0); }); rb(s3, '\u2191', 'Move up', function () { self.nudge(0, -1); });
    rb(s3, '\u2193', 'Move down', function () { self.nudge(0, 1); }); rb(s3, '\u2192', 'Move right', function () { self.nudge(1, 0); });
    this.stepIn = h('input', { type: 'number', class: 'sem-step', value: '10', min: '1', max: '500', title: 'Step in pixels (Shift + arrow key = 5x)' }, s3);
    var sa = sec('Align constructs');
    rb(sa, 'Align vertically', 'Selected constructs get the same x position (one vertical line)', function () { self.align('vcenter'); });
    rb(sa, 'Align horizontally', 'Selected constructs get the same y position (one horizontal line)', function () { self.align('hmiddle'); });
    rb(sa, '\u21E4', 'Align to the left-most construct', function () { self.align('left'); });
    rb(sa, '\u21E5', 'Align to the right-most construct', function () { self.align('right'); });
    rb(sa, '\u2912', 'Align to the top-most construct', function () { self.align('top'); });
    rb(sa, '\u2913', 'Align to the bottom-most construct', function () { self.align('bottom'); });
    rb(sa, 'Spread \u2194', 'Equal horizontal spacing (3+ constructs)', function () { self.align('distH'); });
    rb(sa, 'Spread \u2195', 'Equal vertical spacing (3+ constructs)', function () { self.align('distV'); });
    h('div', { class: 'sp-note' }, sa, 'Shift+click several constructs first. Indicators move along.');
    chk(sa, 'Alignment guides while dragging', true, function (v) { self.gsnap = v; });
    var s4 = sec('View');
    rb(s4, 'Auto-arrange', 'Reset all positions to the automatic layout', function () { self.autoArrange(); });
    rb(s4, 'Fit', 'Fit the whole model into the window', function () { self.autoFit = !ed; self.fit(); });
    rb(s4, '+', 'Zoom in', function () { var v = self.view; self.zoomAt(1.25, v.x + v.w / 2, v.y + v.h / 2); });
    rb(s4, '\u2212', 'Zoom out', function () { var v = self.view; self.zoomAt(1 / 1.25, v.x + v.w / 2, v.y + v.h / 2); });
    rb(s4, 'Hide indicators', 'Hide the indicators of the selected construct(s) (Alt+X)', function () { setItems(false); });
    rb(s4, 'Show indicators', 'Show the indicators of the selected construct(s) (Alt+C)', function () { setItems(true); });
    rb(s4, 'Save SVG', 'Download the diagram as an SVG file', function () { self.exportSVG(); });
    this.zoomLbl = h('div', { class: 'sp-note' }, s4);
    chk(s4, 'Indicators follow construct', true, function (v) { self.follow = v; });
    chk(s4, 'Snap to grid', false, function (v) { self.snap = v; self.render(); });

    this.svg = mk('svg', { class: 'sem-svg', width: '100%', height: '100%', viewBox: '0 0 1000 500', preserveAspectRatio: 'xMidYMid meet' }, this.stage);
    addDefs(this.svg, this.mid);
    this.grid = mk('rect', { x: -20000, y: -20000, width: 40000, height: 40000, fill: 'url(#' + this.mid + 'grid)', 'pointer-events': 'none', style: 'display:none' }, this.svg);
    this.root = mk('g', null, this.svg);
    this.empty = h('div', { class: 'sem-empty' }, this.stage, host.getAttribute('data-empty') || '');
    this.status = h('div', { class: 'sem-status' }, host);

    this.svg.addEventListener('pointerdown', function (e) { self.onDown(e); });
    this.svg.addEventListener('pointermove', function (e) {
      if (self.pending && (self.pending.from || self.pending.mfrom) && !self.drag) { self.rub = self.toWorld(e); self.render(); }
    });
    this.svg.addEventListener('wheel', function (e) {
      e.preventDefault();
      var pt = self.toWorld(e); self.zoomAt(e.deltaY < 0 ? 1.15 : 1 / 1.15, pt.x, pt.y);
    }, { passive: false });
    this.svg.addEventListener('dblclick', function (e) {
      if (!ed) return;
      var t = e.target && e.target.closest ? e.target.closest('[data-id]') : null; if (!t) return;
      var id = t.getAttribute('data-id'); if (id.indexOf('path:') === 0) return;
      var k = id.indexOf('|') > 0 ? id.slice(0, id.indexOf('|')) : id, c = self.findCon(k);
      if (!c || c.type === 'int') return;
      var nn = window.prompt('Rename latent variable', k); if (nn) self.rename(k, nn);
    });
    this.stage.addEventListener('keydown', function (e) {
      if (e.target && /^(INPUT|SELECT|TEXTAREA)$/.test(e.target.tagName)) return;
      var m = e.shiftKey ? 5 : 1, k = e.key, mod = e.ctrlKey || e.metaKey;
      if (k === 'ArrowLeft') self.nudge(-m, 0);
      else if (k === 'ArrowRight') self.nudge(m, 0);
      else if (k === 'ArrowUp') self.nudge(0, -m);
      else if (k === 'ArrowDown') self.nudge(0, m);
      else if (ed && (k === 'Delete' || k === 'Backspace')) self.delSel();
      else if (ed && mod && (k === 'z' || k === 'Z')) { if (e.shiftKey) self.redoOp(); else self.undoOp(); }
      else if (ed && mod && (k === 'y' || k === 'Y')) self.redoOp();
      else if (k === 'Escape') { self.pending = null; self.rub = null; self.sel = {}; self.render(); }
      else return;
      e.preventDefault();
    });
    if (ed) {
      this.stage.addEventListener('dragover', function (e) { e.preventDefault(); if (e.dataTransfer) e.dataTransfer.dropEffect = 'copy'; });
      this.stage.addEventListener('drop', function (e) {
        e.preventDefault();
        var names = dragNames.length ? dragNames.slice() : String(e.dataTransfer.getData('text/plain') || '').split(',').filter(Boolean);
        dragNames = [];
        if (!names.length) return;
        var pt = self.toWorld(e), k = self.conAt(pt), c = k && self.findCon(k);
        if (c && c.type !== 'int') self.addItems(k, names); else self.addCon(names, pt);
      });
    }
    if (window.ResizeObserver) new ResizeObserver(function () { self.resize(); }).observe(this.stage);
    this.render();
  };

  P.stepVal = function () { var v = parseFloat(this.stepIn.value); return isFinite(v) && v > 0 ? v : 10; };
  P.setStatus = function (t) { this.status.textContent = t; };
  P.setTool = function (t) {
    var self = this;
    this.tool = t; this.pending = null; this.rub = null;
    Object.keys(this.toolBtns).forEach(function (k) { self.toolBtns[k].classList.toggle('on', k === t); });
    this.stage.setAttribute('data-tool', t); this.render();
  };

  P.applyView = function () {
    var v = this.view;
    this.svg.setAttribute('viewBox', [v.x, v.y, v.w, v.h].join(' '));
    var r = this.svg.getBoundingClientRect();
    if (this.zoomLbl && r.width) this.zoomLbl.textContent = 'Zoom ' + Math.round(100 * r.width / v.w) + '%';
  };
  P.resize = function () {
    var r = this.svg.getBoundingClientRect();
    if (!r.width || !r.height) return;
    if (this.autoFit) this.fit();
    else {
      if (!this._init) { this._init = true; this.view.x = 0; this.view.y = 0; this.view.w = r.width; }
      else if (this._rw && Math.abs(this._rw - r.width) > 0.5) this.view.w = this.view.w * r.width / this._rw;   // keep the zoom when the window / panel is resized
      this._rw = r.width;
      this.view.h = this.view.w * r.height / r.width; this.applyView();
    }
  };
  P.fit = function () {
    var b = worldBox(this.g), r = this.svg.getBoundingClientRect(), asp = (r.width && r.height) ? r.height / r.width : 0.5;
    if (!b) { var w0 = r.width || 1000; this.view = { x: 0, y: 0, w: w0, h: w0 * asp }; this.applyView(); return; }
    var pad = 30, bw = b.x1 - b.x0 + 2 * pad, bh = b.y1 - b.y0 + 2 * pad, w = Math.max(bw, bh / asp);
    if (r.width) w = Math.max(w, r.width / 1.0);               // never zoom in beyond 100%
    var hh = w * asp;
    this.view = { x: (b.x0 + b.x1) / 2 - w / 2, y: (b.y0 + b.y1) / 2 - hh / 2, w: w, h: hh };
    this._init = true; this._rw = r.width; this.applyView();
  };
  P.zoomAt = function (f, cx, cy) {
    var v = this.view, nw = Math.min(Math.max(v.w / f, 150), 30000), k = nw / v.w;
    v.x = cx - (cx - v.x) * k; v.y = cy - (cy - v.y) * k; v.w = nw; v.h = v.h * k;
    this.autoFit = false; this._rw = this.svg.getBoundingClientRect().width; this.applyView();
  };
  P.toWorld = function (e) {
    var r = this.svg.getBoundingClientRect(), s = this.view.w / (r.width || 1);
    return { x: this.view.x + (e.clientX - r.left) * s, y: this.view.y + (e.clientY - r.top) * s };
  };

  // ---------- model access / editing (model builder only) ----------
  P.findCon = function (n) { for (var i = 0; i < this.model.cons.length; i++) if (this.model.cons[i].name === n) return this.model.cons[i]; return null; };
  P.taken = function (n) { return !!this.findCon(n) || vars.some(function (v) { return v.n === n; }); };
  P.uniq = function (base) { var n = base, i = 1; while (this.taken(n)) n = base + (++i); return n; };
  P.nameFor = function (items) {
    if (!items.length) return 'LV';
    var b = String(items[0]).replace(/[0-9]+$/, '').replace(/[^A-Za-z0-9_]/g, '_');
    return /^[A-Za-z][A-Za-z0-9_]*$/.test(b) ? b : 'LV';
  };
  P.snapshot = function () { return JSON.stringify({ m: this.model, p: layout.pos, s: layout.side }); };
  P.pushUndo = function () { this.undo.push(this.snapshot()); if (this.undo.length > 60) this.undo.shift(); this.redo = []; };
  P.restore = function (s) { var o = JSON.parse(s); this.model = o.m; layout.pos = o.p; layout.side = o.s; this.sel = {}; this.pending = null; this.commit(); };
  P.undoOp = function () { if (!this.undo.length) return; this.redo.push(this.snapshot()); this.restore(this.undo.pop()); };
  P.redoOp = function () { if (!this.redo.length) return; this.undo.push(this.snapshot()); this.restore(this.redo.pop()); };
  P.edit = function (fn) { this.pushUndo(); fn.call(this); this.commit(); };
  P.commit = function () {
    this.g = buildGraph(this.model);
    sync(this.g, true);
    renderAll();
    if (this.autoFit) this.fit();
    emit(this.model); renderVars();
  };
  P.stripItems = function (items) { this.model.cons.forEach(function (c) { c.items = c.items.filter(function (i) { return items.indexOf(i) < 0; }); }); };
  P.growView = function () {
    var b = worldBox(this.g), v = this.view, r = this.svg.getBoundingClientRect(); if (!b || !r.width) return;
    var pad = 30, asp = r.height / r.width;
    if (b.x0 - pad >= v.x && b.x1 + pad <= v.x + v.w && b.y0 - pad >= v.y && b.y1 + pad <= v.y + v.h) return;
    var x0 = Math.min(v.x, b.x0 - pad), y0 = Math.min(v.y, b.y0 - pad), x1 = Math.max(v.x + v.w, b.x1 + pad), y1 = Math.max(v.y + v.h, b.y1 + pad);
    var w = Math.max(x1 - x0, (y1 - y0) / asp), cx = (x0 + x1) / 2, cy = (y0 + y1) / 2;
    this.view = { x: cx - w / 2, y: cy - w * asp / 2, w: w, h: w * asp }; this.autoFit = false; this.applyView();
  };
  P.addCon = function (items, pt) {
    this.edit(function () {
      var name = this.uniq(this.nameFor(items));
      this.stripItems(items);
      this.model.cons.push({ name: name, type: 'cf', items: items.slice() });
      layout.pos[name] = { x: this.snap ? snapv(pt.x) : pt.x, y: this.snap ? snapv(pt.y) : pt.y }; layout.side[name] = 'left';
      this.sel = {}; this.sel[name] = 1;
    });
  };
  P.addItems = function (k, items) {
    this.edit(function () {
      this.stripItems(items);
      var c = this.findCon(k); items.forEach(function (i) { if (c.items.indexOf(i) < 0) c.items.push(i); });
      this.sel = {}; this.sel[k] = 1;
    });
  };
  P.connect = function (a, b) {
    var ca = this.findCon(a), cb = this.findCon(b);
    if (!ca || !cb || a === b) return;
    if (cb.type === 'int') { this.setStatus('A moderator term cannot be the target of a path.'); return; }
    if (this.model.paths.some(function (p) { return p.from === a && p.to === b; })) { this.setStatus('These constructs are already connected.'); return; }
    this.edit(function () { this.model.paths.push({ from: a, to: b }); });
  };
  // moderator: internally an interaction term (a.m) that predicts b; on the canvas only a dashed line from m to the middle of path a -> b is drawn
  P.addMod = function (pk, m) {
    var i = pk.indexOf('>'), a = pk.slice(0, i), b = pk.slice(i + 1), cm = this.findCon(m), ca = this.findCon(a);
    if (!cm || !ca || cm.type === 'int' || ca.type === 'int' || m === a || m === b) { this.setStatus('Pick a construct that is neither end of the selected path.'); return; }
    if (!this.model.paths.some(function (p) { return p.from === a && p.to === b; })) { this.setStatus('Attach the moderator to a direct path between two constructs.'); return; }
    var nm = a + '.' + m, ex = this.findCon(nm);
    if (ex && this.model.paths.some(function (p) { return p.from === nm && p.to === b; })) { this.setStatus('This moderator is already attached to that path.'); return; }
    this.edit(function () {
      if (!ex) this.model.cons.push({ name: nm, type: 'int', items: [], parts: [a, m] });
      this.model.paths.push({ from: nm, to: b });
    });
  };
  // quadratic effect: internally the interaction term a.a (cSEM syntax) that predicts b, in addition to the path a -> b
  P.addQuad = function (pk) {
    var i = pk.indexOf('>'), a = pk.slice(0, i), b = pk.slice(i + 1), ca = this.findCon(a), cb = this.findCon(b);
    if (!ca || !cb || ca.type === 'int' || cb.type === 'int') return 'Pick a path between two constructs.';
    if (!this.model.paths.some(function (p) { return p.from === a && p.to === b; })) return 'The path ' + a + ' -> ' + b + ' does not exist.';
    var nm = a + '.' + a, ex = this.findCon(nm);
    if (ex && this.model.paths.some(function (p) { return p.from === nm && p.to === b; })) return 'The quadratic effect of ' + a + ' on ' + b + ' already exists.';
    this.edit(function () {
      if (!ex) this.model.cons.push({ name: nm, type: 'int', items: [], parts: [a, a] });
      this.model.paths.push({ from: nm, to: b });
    });
    return '';
  };
  P.hasQuad = function (pk) {
    var i = pk.indexOf('>'), a = pk.slice(0, i), b = pk.slice(i + 1), nm = a + '.' + a;
    return this.model.paths.some(function (p) { return p.from === nm && p.to === b; });
  };
  P.delQuad = function (pk) {
    var i = pk.indexOf('>'), a = pk.slice(0, i), b = pk.slice(i + 1), nm = a + '.' + a;
    if (!this.hasQuad(pk)) return 'There is no quadratic effect on this path.';
    this.edit(function () {
      this.model.paths = this.model.paths.filter(function (p) { return !(p.from === nm && p.to === b); });
      if (!this.model.paths.some(function (p) { return p.from === nm; })) this.model.cons = this.model.cons.filter(function (c) { return c.name !== nm; });
    });
    return '';
  };
  P.delSel = function () {
    var ids = Object.keys(this.sel); if (!ids.length) return;
    this.edit(function () {
      var m = this.model, rmC = {}, rmI = {}, rmP = {};
      ids.forEach(function (id) {
        if (id.indexOf('path:') === 0) rmP[id.slice(5)] = 1; else if (id.indexOf('|') > 0) rmI[id] = 1; else rmC[id] = 1;
      });
      m.cons.forEach(function (c) { if (c.type === 'int' && c.parts.some(function (p) { return rmC[p]; })) rmC[c.name] = 1; });
      m.cons = m.cons.filter(function (c) { return !rmC[c.name]; });
      m.cons.forEach(function (c) { c.items = c.items.filter(function (i) { return !rmI[c.name + '|' + i]; }); });
      m.paths = m.paths.filter(function (p) { return !rmC[p.from] && !rmC[p.to] && !rmP[p.from + '>' + p.to]; });
      // moderator terms whose moderated path (or moderator line) is gone disappear as well
      var byName = {}; m.cons.forEach(function (c) { byName[c.name] = c; });
      var pp = m.paths;
      m.paths = pp.filter(function (p) { var c = byName[p.from]; return !(c && c.type === 'int') || pp.some(function (q) { return q.from === c.parts[0] && q.to === p.to; }); });
      m.cons = m.cons.filter(function (c) { return c.type !== 'int' || m.paths.some(function (p) { return p.from === c.name; }); });
      this.sel = {};
    });
  };
  P.clearAll = function () {
    if (!this.model.cons.length) return;
    if (!window.confirm('Remove the whole model?')) return;
    this.edit(function () { this.model = { cons: [], paths: [] }; layout.pos = {}; layout.side = {}; this.sel = {}; });
    if (window.Shiny && window.Shiny.setInputValue) window.Shiny.setInputValue('sem_cleared', Math.random(), { priority: 'event' });
  };
  P.rename = function (old, nw) {
    nw = String(nw || '').trim();
    if (!this.findCon(old) || nw === old) return;
    if (!/^[A-Za-z][A-Za-z0-9_]*$/.test(nw) || this.taken(nw)) { this.setStatus('Invalid or already used name (letters, digits and _ only; must not equal a data column).'); this.updateProps(); return; }
    this.edit(function () {
      var map = {}; map[old] = nw;
      this.model.cons.forEach(function (k) { if (k.type === 'int' && k.parts.indexOf(old) >= 0) map[k.name] = k.parts.map(function (p) { return p === old ? nw : p; }).join('.'); });
      this.model.cons.forEach(function (k) { if (k.parts) k.parts = k.parts.map(function (p) { return map[p] || p; }); if (map[k.name]) k.name = map[k.name]; });
      this.model.paths.forEach(function (p) { p.from = map[p.from] || p.from; p.to = map[p.to] || p.to; });
      Object.keys(map).forEach(function (o) { renameKey(o, map[o]); });
      this.sel = {}; this.sel[nw] = 1;
    });
  };
  P.setType = function (k, tp) { var c = this.findCon(k); if (!c || c.type === 'int' || c.type === tp) return; this.edit(function () { c.type = tp; }); };
  P.conAt = function (pt) {
    for (var i = this.g.order.length - 1; i >= 0; i--) {
      var id = this.g.order[i], c = this.g.cons[id], p = layout.pos[id]; if (!p || c.type === 'int') continue;
      var s = conSize(c), dx = (pt.x - p.x) / s.rx, dy = (pt.y - p.y) / s.ry;
      if (dx * dx + dy * dy <= 1) return id;
      for (var j = 0; !hidden[id] && j < c.items.length; j++) {
        var q = layout.pos[c.items[j].id], z = itemSize(c.items[j]);
        if (q && Math.abs(pt.x - q.x) <= z.w / 2 && Math.abs(pt.y - q.y) <= z.h / 2) return id;
      }
    }
    return null;
  };

  P.setModel = function (msg) {
    var self = this, arrS = function (x) { return arr(x).map(String); };
    if (this.editable) {                                       // model pushed by the server (example model)
      if (msg.reset) { layout.pos = {}; layout.side = {}; }
      this.model = {
        cons: arr(msg.cons).map(function (k) { return { name: String(k.name), type: k.type || 'cf', items: arrS(k.items), parts: k.parts ? arrS(k.parts) : undefined }; }),
        paths: arr(msg.paths).map(function (p) { return { from: String(p.from), to: String(p.to) }; })
      };
      this.undo = []; this.redo = []; this.sel = {}; this.pending = null;
      this.commit(); this.fit();
      return;
    }
    this.g = buildGraph(msg);
    sync(this.g, false);
    Object.keys(this.sel).forEach(function (id) { if (!layout.pos[id]) delete self.sel[id]; });
    renderAll();
    if (this.autoFit) this.fit();
  };

  // ---------- selection helpers ----------
  P.selCon = function () {
    var ks = {}, self = this;
    Object.keys(this.sel).forEach(function (id) {
      if (self.g.cons[id]) ks[id] = 1; else if (id.indexOf('|') > 0) ks[id.slice(0, id.indexOf('|'))] = 1;
    });
    var n = Object.keys(ks); return n.length === 1 ? n[0] : null;
  };
  P.selectedCons = function () {
    var ks = {}, self = this;
    Object.keys(this.sel).forEach(function (id) {
      if (self.g.cons[id]) ks[id] = 1; else if (id.indexOf('|') > 0) ks[id.slice(0, id.indexOf('|'))] = 1;
    });
    var n = Object.keys(ks); return n.length ? n : this.g.order.filter(function (id) { return self.g.cons[id].type !== 'int'; });
  };
  P.updateProps = function () {
    if (!this.pName) return;
    var k = this.selCon(), m = k && this.findCon(k), ok = !!(m && m.type !== 'int');
    this.pName.disabled = !ok; this.pType.disabled = !ok;
    if (document.activeElement !== this.pName) this.pName.value = ok ? k : '';
    this.pType.value = ok ? m.type : 'cf';
    this.pInfo.textContent = !m ? 'Select a construct to edit its name and measurement mode (or double-click it).' :
      (m.type === 'int' ? 'Moderator term' : m.items.length + ' indicator(s)' + (m.items.length ? '' : ' - drag variables onto it'));
  };

  P.render = function () {
    var self2 = this, s = this.sel, pend = this.pending;
    var psrc = pend && (pend.from || pend.mfrom);
    if (psrc) { s = {}; for (var k in this.sel) s[k] = 1; s[psrc] = 1; }
    draw(this.root, this.g, s, this.mid);
    if (psrc && this.rub && layout.pos[psrc]) {
      var p = layout.pos[psrc];
      mk('line', { x1: p.x, y1: p.y, x2: this.rub.x, y2: this.rub.y, stroke: pend.mfrom ? '#2f6060' : C_STROKE, 'stroke-width': 2, 'stroke-dasharray': '6 4', 'marker-end': 'url(#' + this.mid + 'p)', 'pointer-events': 'none' }, this.root);
    }
    (this.guides || []).forEach(function (gd) {
      var a = gd.t === 'v' ? { x1: gd.c, x2: gd.c, y1: gd.a, y2: gd.b } : { y1: gd.c, y2: gd.c, x1: gd.a, x2: gd.b };
      a.stroke = '#e0218a'; a['stroke-width'] = 1.4; a['stroke-dasharray'] = '6 4'; a['pointer-events'] = 'none';
      mk('line', a, self2.root);
    });
    if (this.quadMode) this.drawQuadBadges();
    this.grid.style.display = this.snap ? '' : 'none';
    this.empty.style.display = this.g.order.length ? 'none' : '';
    var ids = Object.keys(this.sel);
    if (this.quadMode) this.setStatus(this.quadMsg || 'Quadratic effect: click "Click" on a path to add the quadratic effect of its source construct, or "Remove" to delete an existing one. Esc or the button finishes.');
    else if (this.editable && this.tool !== 'select') this.setStatus('');
    else if (!ids.length) this.setStatus('');
    else this.setStatus('');
    this.updateProps();
  };

  // ids that move together: the selection (+ the indicators of selected constructs when "follow" is on)
  P.moveIds = function () {
    var seen = {}, out = [], self = this;
    function add(id) { if (!seen[id] && layout.pos[id]) { seen[id] = 1; out.push(id); } }
    Object.keys(this.sel).forEach(function (id) {
      if (id.indexOf('|') > 0 && hidden[id.slice(0, id.indexOf('|'))]) return;
      add(id);
      var c = self.g.cons[id];
      if (c && self.follow) c.items.forEach(function (it) { add(it.id); });
    });
    return out;
  };

  P.listen = function () {
    var self = this;
    this._mv = function (ev) { self.onMove(ev); };
    this._up = function (ev) { self.onUp(ev); };
    document.addEventListener('pointermove', this._mv);
    document.addEventListener('pointerup', this._up);
    document.addEventListener('pointercancel', this._up);
  };

  P.drawQuadBadges = function () {
    var g = this.g, root = this.root;
    g.paths.forEach(function (p) {
      var f = g.cons[p.from], t = g.cons[p.to]; if (!f || !t || f.type === 'int' || t.type === 'int') return;
      var pf = layout.pos[f.id], pt = layout.pos[t.id]; if (!pf || !pt) return;
      var a = ellipseEdge(conSize(f), pf, pt.x, pt.y), b = ellipseEdge(conSize(t), pt, pf.x, pf.y), mx = (a.x + b.x) / 2, my = (a.y + b.y) / 2;
      var done = g.paths.some(function (q) { var c = g.cons[q.from]; return c && c.type === 'int' && c.parts && c.parts[0] === c.parts[1] && c.parts[0] === p.from && q.to === p.to; });
      mk('rect', { x: mx - 23, y: my - 12, width: 46, height: 24, rx: 2, fill: done ? '#c0392b' : '#3f51c1', 'data-id': 'quad:' + p.from + '>' + p.to, style: 'cursor:pointer' }, root);
      mk('text', { x: mx, y: my, 'text-anchor': 'middle', 'dominant-baseline': 'central', 'font-size': 12, 'font-family': FONT, fill: '#fff', 'pointer-events': 'none' }, root, done ? 'Remove' : 'Click');
    });
  };
  P.onDown = function (e) {
    if (e.button !== undefined && e.button !== 0) return;
    if (this.quadMode) {
      var qt = e.target && e.target.closest ? e.target.closest('[data-id]') : null, qid = qt ? qt.getAttribute('data-id') : null;
      if (qid && qid.indexOf('quad:') === 0) { var qk = qid.slice(5);
        if (this.hasQuad(qk)) this.quadMsg = this.delQuad(qk) || 'Quadratic effect removed.';
        else this.quadMsg = this.addQuad(qk) || 'Quadratic effect added \u2014 click another label or finish.';
        this.render(); }
      e.preventDefault(); return;
    }
    var tool = this.editable ? this.tool : 'select';
    var t = e.target && e.target.closest ? e.target.closest('[data-id]') : null, multi = e.shiftKey || e.ctrlKey || e.metaKey;
    var id = t ? t.getAttribute('data-id') : null, pt = this.toWorld(e), isCon = !!(id && this.g.cons[id]);
    try { this.stage.focus({ preventScroll: true }); } catch (err) { /* ignore */ }

    if (tool === 'lv') { if (!t) this.addCon([], pt); e.preventDefault(); return; }
    if (tool === 'connect') {
      if (isCon && this.pending && this.pending.from && this.pending.from !== id) {
        var f = this.pending.from; this.pending = null; this.rub = null; this.connect(f, id); this.render();
      } else if (isCon) {
        this.pending = { from: id }; this.rub = pt; this.drag = { type: 'connect', from: id }; this.listen(); this.render();
      } else { this.pending = null; this.rub = null; this.render(); }
      e.preventDefault(); return;
    }
    if (tool === 'mod') {
      if (id && id.indexOf('path:') === 0) {
        if (this.pending && this.pending.mfrom) { var mf = this.pending.mfrom; this.pending = null; this.rub = null; this.sel = {}; this.addMod(id.slice(5), mf); this.render(); }
        else { this.pending = { path: id.slice(5) }; this.sel = {}; this.sel[id] = 1; this.render(); }
      } else if (isCon && this.pending && this.pending.path) { var pk = this.pending.path; this.pending = null; this.sel = {}; this.render(); this.addMod(pk, id); }
      else if (isCon) { this.pending = { mfrom: id }; this.rub = pt; this.sel = {}; this.drag = { type: 'modline', from: id }; this.listen(); this.render(); }
      else { this.pending = null; this.rub = null; this.sel = {}; this.render(); }
      e.preventDefault(); return;
    }
    if (t) {
      if (multi) { if (this.sel[id]) delete this.sel[id]; else this.sel[id] = 1; }
      else if (!this.sel[id]) { this.sel = {}; this.sel[id] = 1; }
      this.render();
      if (!this.sel[id]) { e.preventDefault(); return; }
      var ids = this.moveIds(), orig = {};
      ids.forEach(function (i) { orig[i] = { x: layout.pos[i].x, y: layout.pos[i].y }; });
      this.drag = { type: 'nodes', id: id, start: pt, ids: ids, orig: orig, pre: this.editable ? this.snapshot() : null, moved: false };
    } else {
      if (!multi) { this.sel = {}; this.render(); }
      this.drag = { type: 'pan', sx: e.clientX, sy: e.clientY, vx: this.view.x, vy: this.view.y };
    }
    this.listen();
    e.preventDefault();
  };
  P.onMove = function (e) {
    var d = this.drag; if (!d) return;
    if (d.type === 'connect' || d.type === 'modline') { this.rub = this.toWorld(e); this.render(); return; }
    if (d.type === 'pan') {
      var r = this.svg.getBoundingClientRect(), s = this.view.w / (r.width || 1);
      this.view.x = d.vx - (e.clientX - d.sx) * s; this.view.y = d.vy - (e.clientY - d.sy) * s;
      this.autoFit = false; this.applyView(); return;
    }
    var pt = this.toWorld(e), dx = pt.x - d.start.x, dy = pt.y - d.start.y, o = d.orig[d.id];
    if (this.snap && o) { dx = snapv(o.x + dx) - o.x; dy = snapv(o.y + dy) - o.y; }
    this.guides = [];
    if (!this.snap && this.gsnap && o && this.g.cons[d.id] && this.g.cons[d.id].type !== 'int') {
      var rr = this.svg.getBoundingClientRect(), T = 6 * this.view.w / (rr.width || 1), self = this, mv = {}, nx = o.x + dx, ny = o.y + dy, bx = null, by = null;
      d.ids.forEach(function (i) { mv[i] = 1; });
      this.g.order.forEach(function (id) {
        if (mv[id] || !layout.pos[id] || self.g.cons[id].type === 'int') return;
        var q = layout.pos[id];
        if (Math.abs(q.x - nx) < T && (bx === null || Math.abs(q.x - nx) < Math.abs(bx - nx))) bx = q.x;
        if (Math.abs(q.y - ny) < T && (by === null || Math.abs(q.y - ny) < Math.abs(by - ny))) by = q.y;
      });
      if (bx !== null) dx = bx - o.x; if (by !== null) dy = by - o.y;
      nx = o.x + dx; ny = o.y + dy;
      var vy = [ny], hx = [nx];
      this.g.order.forEach(function (id) {
        if (mv[id] || !layout.pos[id] || self.g.cons[id].type === 'int') return;
        var q = layout.pos[id];
        if (bx !== null && Math.abs(q.x - nx) < 0.5) vy.push(q.y);
        if (by !== null && Math.abs(q.y - ny) < 0.5) hx.push(q.x);
      });
      if (bx !== null) this.guides.push({ t: 'v', c: nx, a: Math.min.apply(null, vy) - 50, b: Math.max.apply(null, vy) + 50 });
      if (by !== null) this.guides.push({ t: 'h', c: ny, a: Math.min.apply(null, hx) - 50, b: Math.max.apply(null, hx) + 50 });
    }
    if (dx || dy) d.moved = true;
    d.ids.forEach(function (id) { layout.pos[id].x = d.orig[id].x + dx; layout.pos[id].y = d.orig[id].y + dy; });
    renderAll();
  };
  P.onUp = function (e) {
    document.removeEventListener('pointermove', this._mv);
    document.removeEventListener('pointerup', this._up);
    document.removeEventListener('pointercancel', this._up);
    var d = this.drag; this.drag = null;
    if (this.guides && this.guides.length && !(d && d.type === 'nodes' && false)) { this.guides = []; this.render(); }
    if (!d) return;
    if (d.type === 'nodes' && d.moved && d.pre) { this.undo.push(d.pre); this.redo = []; }
    if (d.type === 'modline') {
      var el2 = e && document.elementFromPoint(e.clientX, e.clientY), tg2 = el2 && el2.closest ? el2.closest('[data-id]') : null, tid2 = tg2 && tg2.getAttribute('data-id');
      this.rub = null;
      if (tid2 && tid2.indexOf('path:') === 0) { this.pending = null; this.sel = {}; this.addMod(tid2.slice(5), d.from); this.render(); }
      else if (tid2 === d.from) this.render();                // plain click: now click the path
      else { this.pending = null; this.render(); }
      return;
    }
    if (d.type === 'connect') {
      var el = e && document.elementFromPoint(e.clientX, e.clientY), tg = el && el.closest ? el.closest('[data-id]') : null, tid = tg && tg.getAttribute('data-id');
      this.rub = null;
      if (tid && this.g.cons[tid] && tid !== d.from) { this.pending = null; this.connect(d.from, tid); this.render(); }
      else if (tid === d.from) this.render();                 // plain click: wait for the target construct
      else { this.pending = null; this.render(); }
    }
  };

  P.nudge = function (mx, my) {
    var ids = this.moveIds();
    if (!ids.length) { this.setStatus('Click a construct or an indicator first, then use the arrows.'); return; }
    if (this.editable) this.pushUndo();
    var st = this.stepVal();
    ids.forEach(function (id) { layout.pos[id].x += mx * st; layout.pos[id].y += my * st; });
    renderAll();
  };

  // put the indicators of the selected constructs (or of all constructs) on one side / rotate them clockwise
  P.setSide = function (side) {
    var self = this, names = this.selectedCons();
    if (this.editable) this.pushUndo();
    names.forEach(function (k) { var c = self.g.cons[k]; if (c && layout.pos[k]) placeBlock(c, side); });
    renderAll();
    this.setStatus('Indicators of ' + (Object.keys(this.sel).length ? names.join(', ') : 'all constructs') + ' placed ' + side + '.');
  };
  P.rotate = function () {
    var self = this, names = this.selectedCons();
    if (this.editable) this.pushUndo();
    names.forEach(function (k) {
      var c = self.g.cons[k]; if (!c || !layout.pos[k]) return;
      placeBlock(c, SIDES[(SIDES.indexOf(layout.side[k] || 'down') + 1) % 4]);
    });
    renderAll();
  };

  P.selConNames = function () {
    var ks = {}, self = this;
    Object.keys(this.sel).forEach(function (id) {
      var k = self.g.cons[id] ? id : (id.indexOf('|') > 0 ? id.slice(0, id.indexOf('|')) : null);
      if (k && self.g.cons[k] && self.g.cons[k].type !== 'int') ks[k] = 1;
    });
    return Object.keys(ks);
  };
  P.shiftCon = function (k, dx, dy) {
    var c = this.g.cons[k]; if (!layout.pos[k] || (!dx && !dy)) return;
    layout.pos[k].x += dx; layout.pos[k].y += dy;
    c.items.forEach(function (it) { if (layout.pos[it.id]) { layout.pos[it.id].x += dx; layout.pos[it.id].y += dy; } });
  };
  P.align = function (mode) {
    var self = this, ks = this.selConNames().filter(function (k) { return layout.pos[k]; });
    if (ks.length < 2) { this.setStatus('Select two or more constructs first (Shift+click), then align.'); return; }
    if ((mode === 'distH' || mode === 'distV') && ks.length < 3) { this.setStatus('Spreading needs three or more constructs.'); return; }
    if (this.editable) this.pushUndo();
    var xs = ks.map(function (k) { return layout.pos[k].x; }), ys = ks.map(function (k) { return layout.pos[k].y; });
    var mean = function (a) { return a.reduce(function (p, v) { return p + v; }, 0) / a.length; };
    var tx = {}, ty = {}, msg = '', gd = null, minx = Math.min.apply(null, xs), maxx = Math.max.apply(null, xs), miny = Math.min.apply(null, ys), maxy = Math.max.apply(null, ys), v;
    if (mode === 'left' || mode === 'vcenter' || mode === 'right') {
      v = mode === 'left' ? minx : (mode === 'right' ? maxx : mean(xs)); ks.forEach(function (k) { tx[k] = v; });
      gd = { t: 'v', c: v, a: miny - 50, b: maxy + 50 }; msg = 'Aligned vertically (same x position).';
    } else if (mode === 'top' || mode === 'hmiddle' || mode === 'bottom') {
      v = mode === 'top' ? miny : (mode === 'bottom' ? maxy : mean(ys)); ks.forEach(function (k) { ty[k] = v; });
      gd = { t: 'h', c: v, a: minx - 50, b: maxx + 50 }; msg = 'Aligned horizontally (same y position).';
    } else if (mode === 'distH') {
      var sx = ks.slice().sort(function (a, b) { return layout.pos[a].x - layout.pos[b].x; });
      sx.forEach(function (k, i) { tx[k] = minx + i * (maxx - minx) / (sx.length - 1); }); msg = 'Spread evenly (horizontal).';
    } else if (mode === 'distV') {
      var sy = ks.slice().sort(function (a, b) { return layout.pos[a].y - layout.pos[b].y; });
      sy.forEach(function (k, i) { ty[k] = miny + i * (maxy - miny) / (sy.length - 1); }); msg = 'Spread evenly (vertical).';
    }
    ks.forEach(function (k) { var p = layout.pos[k]; self.shiftCon(k, tx[k] !== undefined ? tx[k] - p.x : 0, ty[k] !== undefined ? ty[k] - p.y : 0); });
    this.guides = gd ? [gd] : [];
    renderAll(); this.setStatus(msg);
    clearTimeout(this._gt); this._gt = setTimeout(function () { self.guides = []; self.render(); }, 2500);
  };

  P.autoArrange = function () {
    var a = autoLayout(this.g);
    if (this.editable) this.pushUndo();
    Object.keys(a.pos).forEach(function (id) { layout.pos[id] = { x: a.pos[id].x, y: a.pos[id].y }; });
    Object.keys(a.side).forEach(function (id) { layout.side[id] = a.side[id]; });
    renderAll(); this.fit();
  };

  P.exportSVG = function () {
    var b = worldBox(this.g); if (!b) return;
    var pad = 20, w = b.x1 - b.x0 + 2 * pad, hh = b.y1 - b.y0 + 2 * pad, mid = this.mid + 'x';
    var svg = mk('svg', { xmlns: NS, width: Math.round(w), height: Math.round(hh), viewBox: [b.x0 - pad, b.y0 - pad, w, hh].join(' ') });
    addDefs(svg, mid);
    mk('rect', { x: b.x0 - pad, y: b.y0 - pad, width: w, height: hh, fill: '#ffffff' }, svg);
    draw(mk('g', null, svg), this.g, {}, mid);
    var blob = new Blob(['<?xml version="1.0" encoding="UTF-8"?>\n' + new XMLSerializer().serializeToString(svg)], { type: 'image/svg+xml' });
    var a = h('a', { href: URL.createObjectURL(blob), download: (this.target === 'rs' ? 'sem_results' : 'sem_model') + '.svg' }, document.body);
    a.click(); document.body.removeChild(a);
    setTimeout(function () { URL.revokeObjectURL(a.href); }, 2000);
  };

  // ---------- hide / show indicators of the selected constructs (Alt+X = hide, Alt+C = show) ----------
  function activeCanvas() {
    var ae = document.activeElement, best = null;
    Object.keys(canvases).forEach(function (k) {
      var cv = canvases[k];
      if (!cv.stage || !cv.stage.offsetParent) return;           // only the canvas that is visible (current tab)
      if (cv.stage.contains(ae)) best = cv; else if (!best) best = cv;
    });
    return best;
  }
  function setItems(show) {
    var cv = activeCanvas(); if (!cv) return;
    var ks = cv.selConNames();
    if (!ks.length) { cv.setStatus('Select one or more constructs first (click / Shift+click), then press Alt+X to hide or Alt+C to show their indicators.'); return; }
    ks.forEach(function (k) {
      if (show) delete hidden[k]; else hidden[k] = 1;
    });
    Object.keys(canvases).forEach(function (n) {
      var c = canvases[n];
      if (!show) Object.keys(c.sel).forEach(function (id) { var i = id.indexOf('|'); if (i > 0 && hidden[id.slice(0, i)]) delete c.sel[id]; });
      c.pending = null; c.rub = null;
    });
    renderAll();
    Object.keys(canvases).forEach(function (n) {
      var c = canvases[n];
      if (c.autoFit) c.fit(); else if (show) c.growView();
    });
    cv.setStatus('Indicators ' + (show ? 'shown' : 'hidden') + ' for ' + ks.join(', ') + '.');
  }
  document.addEventListener('keydown', function (e) {
    if (!e.altKey || e.ctrlKey || e.metaKey) return;
    var c = e.code, k = (e.key || '').toLowerCase();
    if (c === 'KeyX' || k === 'x') { e.preventDefault(); setItems(false); }
    else if (c === 'KeyC' || k === 'c') { e.preventDefault(); setItems(true); }
  });

  // ---------- wiring ----------
  function renderAll() { Object.keys(canvases).forEach(function (k) { canvases[k].render(); }); }
  function getCanvas(target) {
    if (canvases[target]) return canvases[target];
    var host = document.getElementById('sem_canvas_' + target);
    if (!host) return null;
    canvases[target] = new Canvas(target, host);
    return canvases[target];
  }
  function apply(target, tries) {
    var cv = getCanvas(target);
    if (!cv) { if (tries < 100) setTimeout(function () { apply(target, tries + 1); }, 100); return; }
    cv.setModel(latest[target]);
  }
  function boot(tries) {
    var ok = !!getCanvas('mb') && renderVars();
    if (!ok && tries < 100) setTimeout(function () { boot(tries + 1); }, 100);
  }
  function register() {
    if (window.Shiny && typeof window.Shiny.addCustomMessageHandler === 'function') {
      window.Shiny.addCustomMessageHandler('sem_model', function (msg) { latest[msg.target] = msg; apply(msg.target, 0); });
      window.Shiny.addCustomMessageHandler('sem_vars', function (msg) {
        vars = arr(msg.vars).map(function (v) { return { n: String(v.n), t: String(v.t) }; });
        vsel = {}; vlast = -1; vcur = -1;
        var el = document.getElementById('sem_dsname'); if (el) el.textContent = msg.name || 'No data';
        renderVars();
      });
      if (window.jQuery) window.jQuery(document).on('shown.bs.tab shown', function () {
        setTimeout(function () { Object.keys(canvases).forEach(function (k) { canvases[k].resize(); }); }, 30);
      });
      boot(0);
    } else setTimeout(register, 50);
  }
  register();
  window.semCanvas = { setItems: setItems, layout: layout, canvases: canvases, autoLayout: autoLayout, buildGraph: buildGraph, refreshVars: renderVars };
})();
)---"

SECS <- c("Path coefficients", "Total indirect effects", "Specific indirect effects", "Total effects",
          "Outer loadings", "Outer weights", "Latent variables", "Residuals", "R-square", "f-square",
          "Construct reliability and validity", "Discriminant validity", "Collinearity statistics (VIF)", "Model fit")
sp_item <- function(mode, title, desc, icon = "&#9881;&#xFE0E;", col = "#2b7de9")
  tags$li(tags$a(href = "#", title = desc, onclick = sprintf("spOpen('%s'); return false;", mode),
                 span(class = "sp-mi", style = paste0("background:", col, ";"), HTML(icon)),
                 strong(title)))
sp_head <- function(txt) tags$li(class = "dropdown-header sp-mh", txt)
SP_JS <- r"---(
(function () {
  var CFG = {
    pls:   { t: 'PLS-SEM algorithm', b: 'Start calculation', g: ['alg'],
             d: 'Estimates the model with the partial least squares algorithm. Common factors are handled like composites, so loadings are slightly overestimated and path coefficients slightly underestimated; use the consistent algorithm to correct this.' },
    plsc:  { t: 'Consistent PLS-SEM algorithm (PLSc)', b: 'Start calculation', g: ['alg', 'cf'],
             d: 'Runs the PLS algorithm and then corrects the correlations of the common-factor constructs for measurement error, which gives consistent loadings, path coefficients and R-square.' },
    boot:  { t: 'Bootstrapping', b: 'Start bootstrapping', g: ['boot', 'alg'],
             d: 'Re-estimates the model on random resamples of the data to obtain standard errors, t values, p values and confidence intervals for paths, effects (incl. moderating effects), loadings, weights, reliability, HTMT and fit. The algorithm settings below are used in every resample.' },
    ipma:  { t: 'Importance-performance map analysis (IPMA)', b: 'Run IPMA', g: ['ipma'],
             d: 'Importance = unstandardized total effect on the target construct, performance = mean of the 0-100 rescaled scores. The current PLS result is used (the model is estimated first if that has not been done yet).' },
    cta:   { t: 'Confirmatory tetrad analysis (CTA)', b: 'Run CTA', g: ['cta'],
             d: 'Tests whether the indicators of a construct can be modelled as a reflective (common factor) measurement model. A bootstrap test checks if the non-redundant model-implied vanishing tetrads of each construct are zero; if at least one tetrad differs significantly from zero, a reflective specification is rejected and a formative / composite specification should be considered. Needs constructs with at least four indicators. The current PLS result is used (the model is estimated first if that has not been done yet).' },
    pred:  { t: 'PLSpredict & CVPAT', b: 'Start', g: ['pred'],
             d: 'Runs one k-fold cross-validation (as in cSEM::predict()) and fills two tabs. PLSpredict: Q2 predict, RMSE and MAE of the PLS model for every indicator of the endogenous constructs, compared with a linear-model (LM) and an indicator-average (IA) benchmark. CVPAT (Liengaard et al., 2021): tests whether the PLS model has a significantly lower prediction loss than these benchmarks (same t statistic as cSEM::testCVPAT()). Uses the current PLS result (estimated first if needed). Only for linear models (no moderators / quadratic effects).' },
    bootc: { t: 'Consistent PLS bootstrapping', b: 'Start bootstrapping', g: ['boot', 'cf', 'alg'],
             d: 'Same as bootstrapping, but every resample is estimated with consistent PLS (PLSc).' }
  };
  var cur = null;
  window.spOpen = function (m) {
    cur = m; var c = CFG[m];
    document.getElementById('sp_dlg_t').textContent = c.t;
    document.getElementById('sp_dlg_d').textContent = c.d;
    document.getElementById('sp_go').textContent = c.b;
    ['alg', 'cf', 'boot', 'ipma', 'cta', 'pred'].forEach(function (g) {
      var el = document.getElementById('sp_g_' + g), i = c.g.indexOf(g);
      el.style.display = i < 0 ? 'none' : ''; el.style.order = i;
    });
    document.getElementById('sp_dlg').style.display = 'flex';
  };
  window.spClose = function () { document.getElementById('sp_dlg').style.display = 'none'; };
  window.spStart = function () {
    if (!cur) return;
    window.Shiny.setInputValue('start', { mode: cur, t: Date.now() }, { priority: 'event' });
    window.spClose();
  };
  document.addEventListener('keydown', function (e) { if (e.key === 'Escape') { window.spClose(); var qc = window.semCanvas && window.semCanvas.canvases && window.semCanvas.canvases.mb; if (qc && qc.quadMode) window.spQuadSet(false); } });
  // ---- quadratic effect mode: every path gets a "Click" label; clicking it adds the quadratic effect of the source construct ----
  function quadCanvas() { var c = window.semCanvas && window.semCanvas.canvases && window.semCanvas.canvases.mb; return c && c.model ? c : null; }
  window.spQuadSet = function (on) {
    var cv = quadCanvas(), b = document.getElementById('sp_quad_btn'); if (!cv) return;
    cv.quadMode = !!on; cv.quadMsg = ''; cv.render();
    if (b) { b.innerHTML = on ? '&#10003; Finish quadratic effect' : '&#178; Quadratic effect'; b.className = 'btn sp-btn-calc ' + (on ? 'btn-warning' : 'btn-default'); }
  };
  window.spQuad = function () {
    var cv = quadCanvas(); if (!cv) return;
    if (!cv.quadMode && window.jQuery) window.jQuery('a[data-value="Model builder"]').tab('show');
    window.spQuadSet(!cv.quadMode);
  };
})();
)---"
COPY_JS <- r"---(
window.spCopyTab = function (btn) {
  var w = btn.closest ? btn.closest('.sp-tabwrap') : null, t = w ? w.querySelector('table') : null;
  if (!t) return;
  var c = t.cloneNode(true), nc = t.rows[0].children.length, ttl = w.getAttribute('data-title') || '';
  c.removeAttribute('class'); c.removeAttribute('style'); c.setAttribute('cellspacing', '0'); c.setAttribute('cellpadding', '4');
  c.style.cssText = 'border-collapse:collapse;font-family:"Segoe UI",Calibri,Arial,sans-serif;font-size:11pt;';
  var cell_css = function (cell, i, bg, bold, color, bb) {
    var left = /sp-l|sp-rl/.test(cell.className || '') || i === 0;
    var cls = cell.className || '';
    if (/sp-ok/.test(cls)) { color = '#0a8a2a'; bold = true; } else if (/sp-bad/.test(cls)) { color = '#d00000'; bold = true; }
    cell.removeAttribute('class');
    cell.style.cssText = 'padding:4px 12px;text-align:' + (left ? 'left' : 'right') + ';' + (bold ? 'font-weight:bold;' : '') +
      'color:' + color + ';' + (bg ? 'background:' + bg + ';' : '') + (bb ? 'border-bottom:1px solid #d9d9d9;' : '') + (i === 0 ? 'border-right:1px solid #d9d9d9;' : '');
  };
  Array.prototype.forEach.call(c.querySelectorAll('thead th'), function (th, i) { cell_css(th, i, '', true, '#666666', true); });
  Array.prototype.forEach.call(c.querySelectorAll('tbody tr'), function (tr, r) {
    Array.prototype.forEach.call(tr.children, function (td, i) { cell_css(td, i, r % 2 ? '#f2f2f2' : '', i === 0, '#000000', false); });
  });
  var tr0 = document.createElement('tr'), th0 = document.createElement('th');
  th0.setAttribute('colspan', nc); th0.textContent = ttl;
  th0.style.cssText = 'background:#a8e6b4;color:#222222;text-align:left;font-weight:normal;font-size:13pt;padding:6px 12px;';
  tr0.appendChild(th0); c.querySelector('thead').insertBefore(tr0, c.querySelector('thead').firstChild);
  var box = document.createElement('div'); box.style.cssText = 'position:fixed;left:-10000px;top:0;background:#fff;'; box.appendChild(c); document.body.appendChild(box);
  var sel = window.getSelection(), r = document.createRange(), ok = false; r.selectNodeContents(box); sel.removeAllRanges(); sel.addRange(r);
  try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
  sel.removeAllRanges(); document.body.removeChild(box);
  var old = btn.getAttribute('data-lbl') || btn.innerHTML; btn.setAttribute('data-lbl', old);
  btn.innerHTML = ok ? '&#10003; Copied' : 'Copy failed'; setTimeout(function () { btn.innerHTML = old; }, 1600);
};
)---"

# ---- 5. User interface -------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$title("cSEM GUI"), tags$style(HTML(SEM_CSS)), tags$script(HTML(SEM_JS)), tags$script(HTML(SP_JS)), tags$script(HTML(COPY_JS))),
  div(class = "sp-top",
      span(class = "sp-brand", "cSEM GUI"), span(class = "sp-sub", "cSEM PLS-SEM"),
      tags$button(type = "button", id = "sp_quad_btn", class = "btn btn-default sp-btn-calc", style = "margin-right:6px;", onclick = "spQuad()",
                  title = "Add or remove quadratic (curvilinear) effects: click the labels shown on the paths", HTML("&#178; Quadratic effect")),
      div(class = "dropdown",
          tags$button(type = "button", class = "btn btn-primary dropdown-toggle sp-btn-calc", `data-toggle` = "dropdown",
                      HTML("&#9881; Calculate &nbsp;<span class='caret'></span>")),
          tags$ul(class = "dropdown-menu dropdown-menu-right sp-menu",
                  sp_head("Standard algorithms"),
                  sp_item("pls",   "PLS-SEM algorithm",            "Estimate the model with the PLS algorithm", "&#9881;&#xFE0E;", "#2b7de9"),
                  sp_item("plsc",  "Consistent PLS-SEM algorithm", "PLS algorithm + correction for measurement error (PLSc)", "&#9881;&#xFE0E;", "#1a9c8c"),
                  tags$li(class = "divider"),
                  sp_head("Resampling"),
                  sp_item("boot",  "Bootstrapping",                "Standard errors, t / p values and confidence intervals", "&#8635;", "#f08a24"),
                  sp_item("bootc", "Consistent PLS bootstrapping", "Bootstrapping with PLSc in every resample", "&#8635;", "#8e5bd6"),
                  tags$li(class = "divider"),
                  sp_head("Analysis"),
                  sp_item("ipma",  "Importance-performance map analysis (IPMA)", "Importance and performance of the constructs for a target", "&#8862;", "#d6455d"),
                  sp_item("cta",   "Confirmatory tetrad analysis (CTA)", "Test whether a reflective measurement model is supported (vanishing tetrads)", "&#9651;", "#2e8b57"),
                  tags$li(class = "divider"),
                  sp_head("Prediction"),
                  sp_item("pred",  "PLSpredict & CVPAT", "Cross-validated out-of-sample prediction (PLSpredict) and predictive ability test (CVPAT)", "&#8776;", "#0f8b8d")))),
  div(id = "sp_dlg", class = "sp-dlg-bg", style = "display:none;",
      div(class = "sp-dlg",
          div(class = "sp-dlg-h", span(id = "sp_dlg_t"), tags$button(type = "button", class = "close", onclick = "spClose()", HTML("&times;"))),
          div(id = "sp_dlg_d", class = "sp-dlg-d"),
          div(id = "sp_dlg_b", class = "sp-dlg-b",
              div(id = "sp_g_alg", class = "sp-grp", h5("PLS-SEM algorithm settings"),
                  selectInput("appr", "Weighting approach", c("PLS-PM")),
                  helpText("PLS-PM: the iterative PLS algorithm that estimates the outer weights of every construct."),
                  selectInput("inner", "Inner weighting scheme", c("path", "centroid", "factorial")),
                  helpText("How neighbouring constructs are combined in each iteration. path: regression / correlation based (cSEM default); centroid: sign of the correlations; factorial: correlations."),
                  selectInput("apath", "Path estimator", c("OLS", "2SLS")),
                  helpText("Estimator of the structural equations. OLS is the standard; 2SLS (two-stage least squares) is for endogeneity problems and cannot be used with moderators."),
                  numericInput("tol", "Stop criterion (tolerance)", value = 1e-5, min = 1e-10, step = 1e-5),
                  helpText("The algorithm stops when the largest change of any outer weight falls below this value."),
                  numericInput("iterm", "Maximum iterations", value = 100, min = 10, max = 5000, step = 10),
                  helpText("Upper limit of iterations if the stop criterion is not reached.")),
              div(id = "sp_g_cf", class = "sp-grp", h5("Consistent PLS (PLSc) settings"),
                  selectInput("cfapp", "Correction factor approach",
                              c("Squared Euclidean distance (default)" = "dist_squared_euclid", "Weighted Euclidean distance" = "dist_euclid_weighted",
                                "Fourth root" = "fourth_root", "Arithmetic mean" = "mean", "Geometric mean" = "geometric_mean",
                                "Harmonic mean" = "harmonic_mean")),
                  helpText("PLSc removes the attenuation caused by measurement error: the correlations of common-factor constructs are divided by an estimate of their reliability. This option sets how the correction factor is aggregated over the indicators. Composite constructs (Mode A / B) are not corrected.")),
              div(id = "sp_g_ipma", class = "sp-grp", h5("IPMA settings"),
                  selectInput("ipma_tgt", "Target construct", choices = NULL),
                  helpText("The endogenous construct whose drivers are analysed. Map level (constructs / indicators) can be switched in the IPMA tab. IPMA is only calculated when you start it here.")),
              div(id = "sp_g_cta", class = "sp-grp", h5("Confirmatory tetrad analysis settings"),
                  numericInput("cta_R", "Number of bootstrap resamples", value = 499, min = 100, max = 20000, step = 100),
                  helpText("Bootstrap samples used to obtain the variance of the tetrads. 499 is the cSEM default; use 4999 or more for final results."),
                  numericInput("cta_seed", "Seed", value = 123, step = 1),
                  helpText("Fixes the random numbers so the result can be reproduced."),
                  selectInput("cta_alpha", "Significance level", c("0.10" = 0.10, "0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001), selected = 0.05),
                  helpText("Overall error probability. The confidence interval of each tetrad uses alpha divided by the number of tetrads of the construct when an adjustment is chosen."),
                  selectInput("cta_adj", "p-value adjustment",
                              c("None (default)" = "none", "Bonferroni" = "bonferroni", "Holm" = "holm", "Hochberg" = "hochberg",
                                "Hommel" = "hommel", "Benjamini-Hochberg (BH)" = "BH", "Benjamini-Yekutieli (BY)" = "BY")),
                  helpText("Correction for testing several tetrads of the same construct at once. Bonferroni / Holm are conservative; BH controls the false discovery rate.")),
              div(id = "sp_g_pred", class = "sp-grp", h5("PLSpredict / CVPAT settings"),
                  numericInput("p_folds", "Number of folds", value = 10, min = 2, max = 200, step = 1),
                  helpText("k-fold cross-validation: the sample is split into k parts; each part is predicted by the model estimated on the other k - 1 parts."),
                  numericInput("p_reps", "Number of repetitions", value = 10, min = 1, max = 100, step = 1),
                  helpText("Repeats the random fold assignment; the prediction metrics are averaged over the repetitions (CVPAT uses the averaged losses). More repetitions = slower."),
                  numericInput("p_seed", "Seed", value = 123, step = 1),
                  helpText("Fixes the random fold assignment so the result can be reproduced."),
                  selectInput("p_appr", "Prediction approach", c("Earliest antecedent (cSEM default)" = "earliest", "Direct antecedent" = "direct")),
                  helpText("Earliest antecedent: endogenous scores are predicted from the exogenous constructs only. Direct antecedent: from all direct predecessors of each construct."),
                  selectInput("p_inadm", "Inadmissible results in a fold", c("Stop" = "stop", "Ignore (use anyway)" = "ignore", "Set PLS predictions to NA" = "set_NA")),
                  helpText("What to do if the model estimated on a training part is inadmissible (e.g. Heywood case, non-convergence)."),
                  selectInput("cv_test", "CVPAT test type", c("Two-tailed (cSEM default)" = "twosided", "One-tailed (PLS better than benchmark)" = "onesided")),
                  helpText("Applies to the CVPAT tab and can be changed afterwards without recalculating."),
                  selectInput("cv_alpha", "CVPAT significance level", c("0.10" = 0.10, "0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001), selected = 0.05),
                  helpText("Error probability for the conclusion column of the CVPAT tab.")),
              div(id = "sp_g_boot", class = "sp-grp", h5("Bootstrapping settings"),
                  numericInput("bR", "Number of resamples", value = 500, min = 50, max = 20000, step = 50),
                  helpText("Random samples drawn with replacement, each as large as the data set. 500 for a quick look; 5000 for final results."),
                  numericInput("bseed", "Seed", value = 123, step = 1),
                  helpText("Fixes the random numbers so the result can be reproduced."),
                  selectInput("btail", "Test type", c("Two-tailed" = "two", "One-tailed" = "one")),
                  helpText("Two-tailed: the effect differs from zero in either direction. One-tailed: the direction was hypothesised in advance."),
                  selectInput("balpha", "Significance level", c("0.10" = 0.10, "0.05" = 0.05, "0.01" = 0.01, "0.001" = 0.001), selected = 0.05),
                  helpText("Error probability used for the confidence intervals and the Yes / No significance flag."),
                  selectInput("bci", "Confidence interval method", c("Percentile" = "perc", "Bias-corrected (BC)" = "bc")),
                  helpText("Percentile: quantiles of the bootstrap distribution. Bias-corrected: shifts the quantiles for the bias between the original estimate and the bootstrap distribution."),
                  checkboxInput("bfit", "Include model fit (SRMR, d_ULS, d_G) - slower", TRUE),
                  helpText("Also bootstraps the fit measures. This is the most expensive part of every resample; switch off for a much faster run."),
                  numericInput("bcores", "Parallel workers", value = default_workers(), min = 1, max = 32, step = 1),
                  helpText("Number of CPU cores used for the resamples (1 = no parallelisation). The default leaves one core free. Results are identical for any number of workers."))),
          div(class = "sp-dlg-f",
              tags$button(type = "button", class = "btn btn-default btn-sm", onclick = "spClose()", "Cancel"),
              tags$button(type = "button", id = "sp_go", class = "btn btn-primary btn-sm", onclick = "spStart()", "Start calculation")))),
  tabsetPanel(id = "main",
      tabPanel("Model builder", br(),
               div(class = "sp-work",
                   div(class = "sp-left",
                       div(class = "sp-lhead", span(id = "sem_dsname", "No data")),
                       div(class = "sp-ldata",
                           fileInput("file", NULL, accept = ".csv", buttonLabel = "Upload CSV", placeholder = "no file selected", width = "100%"),
                           checkboxInput("demo", "Use cSEM demo data (satisfaction)", FALSE),
                           conditionalPanel("input.demo", actionButton("preset", "Load example model", class = "btn-default btn-xs"))),
                       tags$input(id = "sem_vfilter", type = "text", class = "form-control input-sm", placeholder = "Filter variables...",
                                  oninput = "window.semCanvas && window.semCanvas.refreshVars()"),
                       div(id = "sem_vars", class = "sv-list")),
                   div(class = "sp-main",
                       div(id = "sem_canvas_mb", class = "sem-canvas", `data-height` = "540", `data-title` = "PLS-SEM: model",
                           `data-empty` = "Drag variables from the list onto the canvas, or pick \"Latent variable\" and click here")))),
      tabPanel("Results diagram", br(),
               div(id = "sem_canvas_rs", class = "sem-canvas", `data-height` = "600", `data-title` = "PLS-SEM: results",
                   `data-empty` = "Run cSEM to see the results diagram")),
      tabPanel("cSEM report", br(),
               fluidRow(column(6, selectInput("sec", "Report selection", SECS)),
                        column(4, br(), downloadButton("xlsx", "Download Excel report"))),
               uiOutput("rep")),
      tabPanel("Bootstrapping report", br(),
               fluidRow(column(6, selectInput("bsec", "Report selection", BS)),
                        column(4, br(), downloadButton("bxlsx", "Download Excel (bootstrapping)"))),
               uiOutput("btrep")),
      tabPanel("Confirmatory tetrad analysis", br(),
               fluidRow(column(8, uiOutput("cta_info")),
                        column(4, br(), downloadButton("cta_xlsx", "Download Excel (CTA)"))),
               uiOutput("cta_tab")),
      tabPanel("PLSpredict", br(),
               fluidRow(column(8, uiOutput("pred_info")),
                        column(4, br(), downloadButton("pred_xlsx", "Download Excel (PLSpredict)"))),
               uiOutput("pred_tab")),
      tabPanel("CVPAT", br(),
               fluidRow(column(8, uiOutput("cv_info")),
                        column(4, br(), downloadButton("cv_xlsx", "Download Excel (CVPAT)"))),
               uiOutput("cv_tab")),
      tabPanel("IPMA", br(),
               fluidRow(column(4, selectInput("ipma_t", "Target construct", choices = NULL)),
                        column(4, radioButtons("ipma_lvl", "Map level", c("Constructs" = "con", "Indicators" = "ind"), inline = TRUE)),
                        column(4, br(), downloadButton("ipma_xlsx", "Download Excel (IPMA)"))),
               plotOutput("ipma_plot", height = "480px"), br(),
               uiOutput("ipma_tab"))
  )
)


# ---- 6. Server ---------------------------------------------------------------
server <- function(input, output, session) {
  rv <- reactiveValues(cons = list(), paths = data.frame(from = character(), to = character()),
                       res = NULL, args = NULL, bt = NULL, empty = character(), ipma_on = FALSE, ipma_tg = NULL, cta = NULL, pred = NULL)

  # ----- parallel workers: created on demand, reused for the whole session, stopped on exit -----
  pool <- new.env(parent = emptyenv()); pool$cl <- NULL; pool$n <- 0L
  stop_pool <- function() {
    if (!is.null(pool$cl)) try(parallel::stopCluster(pool$cl), silent = TRUE)
    pool$cl <- NULL; pool$n <- 0L
  }
  get_pool <- function(n) {
    if (!is.null(pool$cl) && pool$n != n) stop_pool()
    if (is.null(pool$cl)) { pool$cl <- start_workers(n, ENGINE); pool$n <- if (is.null(pool$cl)) 0L else n }
    pool$cl
  }
  session$onSessionEnded(stop_pool)

  data_r <- reactive({
    if (isTRUE(input$demo)) {
      d <- tryCatch(cSEM::satisfaction, error = function(e) cSEM::threecommonfactors)
      return(as.data.frame(d))
    }
    req(input$file)
    d <- read.csv(input$file$datapath, check.names = FALSE)
    names(d) <- make.names(names(d), unique = TRUE)
    d
  })
  # variable list for the left panel
  observe({
    d <- tryCatch(data_r(), error = function(e) NULL)
    nm <- if (isTRUE(input$demo)) "satisfaction" else if (!is.null(input$file)) input$file$name else "No data"
    session$sendCustomMessage("sem_vars", list(name = nm, vars = if (is.null(d)) list() else
      lapply(names(d), function(v) list(n = v, t = if (is.numeric(d[[v]])) "MET" else "CAT"))))
  })

  # the model is edited on the canvas (JS); the server just receives its description
  observeEvent(input$sem_state, {
    s  <- jsonlite::fromJSON(input$sem_state, simplifyVector = FALSE)
    cs <- Filter(function(k) !identical(k$type, "int"), s$cons)
    rv$cons  <- lapply(Filter(function(k) length(k$items) > 0, cs),
                       function(k) list(name = k$name, items = unlist(k$items), type = k$type))
    rv$empty <- vapply(Filter(function(k) !length(k$items), cs), function(k) k$name, "")
    rv$paths <- if (length(s$paths)) data.frame(from = vapply(s$paths, function(p) p$from, ""),
                                                to = vapply(s$paths, function(p) p$to, ""), stringsAsFactors = FALSE)
                else data.frame(from = character(), to = character())
  })
  observeEvent(input$sem_cleared, { rv$res <- NULL; rv$bt <- NULL; rv$cta <- NULL; rv$pred <- NULL })

  observeEvent(input$preset, {
    d <- data_r(); pre <- sub("[0-9]+$", "", names(d)); need <- c("imag", "expe", "qual", "val", "sat", "loy")
    if (!all(need %in% pre)) { showNotification("Example needs the 'satisfaction' dataset.", type = "error"); return() }
    from <- c("IMAG", "IMAG", "IMAG", "EXPE", "EXPE", "EXPE", "QUAL", "QUAL", "VAL", "SAT")
    to   <- c("EXPE", "SAT", "LOY", "QUAL", "VAL", "SAT", "VAL", "SAT", "SAT", "LOY")
    session$sendCustomMessage("sem_model", list(target = "mb", mode = "builder", reset = TRUE,
      cons  = lapply(need, function(p) list(name = toupper(p), type = "cf", items = as.list(names(d)[pre == p]))),
      paths = lapply(seq_along(from), function(i) list(from = from[i], to = to[i]))))
  })

  # ----- syntax & preview -----
  syntax <- reactive({
    pth <- rv$paths
    # a moderator (drawn only as a line onto a path) is internally an interaction term A.M; make sure M also enters the model
    # as a predictor of the same target (main effect), otherwise cSEM would not see M in the structural model
    for (i in which(grepl(".", pth$from, fixed = TRUE))) {
      mo <- strsplit(pth$from[i], ".", fixed = TRUE)[[1]][2]
      if (!any(pth$from == mo & pth$to == pth$to[i]) && !any(pth$to == mo) && !any(pth$from == mo))
        pth <- rbind(pth, data.frame(from = mo, to = pth$to[i], stringsAsFactors = FALSE))
    }
    st <- if (nrow(pth)) vapply(unique(pth$to), function(t)
      sprintf("%s ~ %s", t, paste(pth$from[pth$to == t], collapse = " + ")), "") else character(0)
    ms <- vapply(rv$cons, function(k) sprintf("%s %s %s", k$name, if (k$type == "cf") "=~" else "<~",
                                              paste(k$items, collapse = " + ")), "")
    paste(c("# Structural model", st, "", "# Measurement model", ms), collapse = "\n")
  })

  # ----- estimation -----
  estimate <- function(plsc = FALSE) {
    if (length(rv$empty)) {
      showNotification(paste("Drag indicators onto these constructs first:", paste(rv$empty, collapse = ", ")), type = "error"); return(FALSE) }
    if (!(length(rv$cons) > 0 && nrow(rv$paths) > 0)) {
      showNotification("Define constructs and paths first.", type = "error"); return(FALSE) }
    tol <- suppressWarnings(as.numeric(input$tol)); if (is.na(tol) || tol <= 0) tol <- 1e-5
    itm <- suppressWarnings(as.integer(input$iterm)); if (is.na(itm) || itm < 10) itm <- 100L
    args <- list(.data = data_r(), .model = syntax(), .approach_weights = input$appr,
                 .disattenuate = isTRUE(plsc), .approach_paths = input$apath, .tolerance = tol, .iter_max = itm)
    if (isTRUE(plsc)) args$.PLS_approach_cf <- input$cfapp
    if (input$appr == "PLS-PM") {
      args$.PLS_weight_scheme_inner <- input$inner
      args$.PLS_modes <- setNames(lapply(rv$cons, function(k) if (k$type == "B") "modeB" else "modeA"),
                                  vapply(rv$cons, `[[`, "", "name"))
    }
    withProgress(message = "Estimating...", {
      out <- tryCatch(do.call(csem, args), error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 12); NULL })
    })
    rv$res <- out; rv$args <- args; rv$bt <- NULL; rv$ipma_on <- FALSE; rv$cta <- NULL; rv$pred <- NULL
    !is.null(out)
  }

  observe({
    if (is.null(rv$res)) { hideTab("main", "Results diagram"); hideTab("main", "cSEM report") }
    else { showTab("main", "Results diagram"); showTab("main", "cSEM report") }
    if (is.null(rv$res) || !isTRUE(rv$ipma_on)) hideTab("main", "IPMA") else showTab("main", "IPMA")   # IPMA only on request
    if (is.null(rv$res) || is.null(rv$cta)) hideTab("main", "Confirmatory tetrad analysis") else showTab("main", "Confirmatory tetrad analysis")
    if (is.null(rv$bt)) hideTab("main", "Bootstrapping report") else showTab("main", "Bootstrapping report")
    if (is.null(rv$res) || is.null(rv$pred)) { hideTab("main", "PLSpredict"); hideTab("main", "CVPAT") } else { showTab("main", "PLSpredict"); showTab("main", "CVPAT") }
  })
  observeEvent(rv$bt, { if (!is.null(rv$bt)) updateTabsetPanel(session, "main", "Bootstrapping report") }, ignoreNULL = TRUE)
  observeEvent(rv$res, { if (!is.null(rv$res)) updateTabsetPanel(session, "main", "Results diagram") }, ignoreNULL = TRUE)

  # ================= all calculations (cSEM conventions) =================
  dlist <- function(m, col, sep = " -> ") {
    ix <- which(!is.na(m), arr.ind = TRUE)
    if (!nrow(ix)) return(data.frame(Note = "No entries"))
    d <- data.frame(` ` = paste(colnames(m)[ix[, 2]], sep, rownames(m)[ix[, 1]]), v = m[ix], check.names = FALSE)
    names(d)[2] <- col; d[order(d[[1]]), , drop = FALSE] }
  desc <- function(M) {
    f <- function(v) { v <- v[!is.na(v)]; z <- v - mean(v); m2 <- mean(z^2)
      data.frame(Mean = mean(v), Median = median(v), `Observed min` = min(v), `Observed max` = max(v),
                 `Number of observations used` = length(v), `Standard deviation` = sd(v),
                 `Excess kurtosis` = mean(z^4) / m2^2 - 3, Skewness = mean(z^3) / m2^1.5, check.names = FALSE) }
    d <- do.call(rbind, lapply(as.data.frame(M), f)); d <- cbind(` ` = colnames(M), d); rownames(d) <- NULL; d }
  idx <- function(M) cbind(` ` = seq_len(nrow(M)), as.data.frame(M, check.names = FALSE))

  calc <- reactive({ req(rv$res); calc_fun(rv$res, data_r()) })

  # ----- results diagram (same canvas and layout as the model builder, plus loadings / weights / path coefficients / R2) -----
  observe({
    msg <- tryCatch({
      if (is.null(rv$res)) list(target = "rs", mode = "results", cons = list(), paths = list())
      else {
        cc <- calc(); typ <- setNames(vapply(rv$cons, `[[`, "", "type"), vapply(rv$cons, `[[`, "", "name"))
        cons <- lapply(cc$nms, function(k) {
          cf <- !is.na(typ[k]) && typ[k] == "cf"
          items <- lapply(cc$blocks[[k]], function(it)
            list(n = it, l = sprintf("%.3f", if (cf) cc$L[k, it] else cc$W[k, it])))
          list(name = k, type = if (!(k %in% cc$nmc)) "int" else if (cf) "cf" else "comp", items = unname(items),
               sub = if (k %in% names(cc$R2)) sprintf("R2 = %.3f", cc$R2[[k]]) else "")
        })
        ix <- which(!is.na(cc$P), arr.ind = TRUE)
        paths <- lapply(seq_len(nrow(ix)), function(i)
          list(from = colnames(cc$P)[ix[i, 2]], to = rownames(cc$P)[ix[i, 1]],
               l = sprintf("%.3f", cc$P[ix[i, 1], ix[i, 2]])))
        list(target = "rs", mode = "results", cons = cons, paths = paths)
      }
    }, error = function(e) NULL)
    if (!is.null(msg)) session$sendCustomMessage("sem_model", msg)
  })

  # ----- cSEM report -----
  # ================= IPMA (importance-performance map analysis) =================
  # Importance  = unstandardized total effect of a construct (indicator) on the target construct
  # Performance = mean of the rescaled (0-100) latent variable score (indicator)
  # Indicators are rescaled to 0-100 (min/max); outer weights are converted to the 0-100 scale and
  # normalised to sum to 1, so that the latent variable scores are themselves on a 0-100 scale.
  ipma_fun <- function(cc, target, d) {
    d <- as.data.frame(d)
    ind <- intersect(unique(unlist(cc$blocks)), names(d))
    X0 <- as.matrix(d[, ind, drop = FALSE]); X0 <- X0[complete.cases(X0), , drop = FALSE]
    rg <- apply(X0, 2, function(v) diff(range(v))); rg[rg == 0] <- NA
    X100 <- sweep(sweep(X0, 2, apply(X0, 2, min), "-"), 2, rg, "/") * 100
    # per construct: normalised unstandardized weights u, scale factor G (LV_0-100 = mean + score / G)
    info <- lapply(setNames(cc$nms, cc$nms), function(k) {
      b <- cc$blocks[[k]]
      if (!length(b) || !all(b %in% colnames(X100)) || anyNA(X100[, b, drop = FALSE])) return(NULL)  # e.g. interaction terms
      g <- cc$W[k, b] / apply(X100[, b, drop = FALSE], 2, sd); G <- sum(g)
      if (!is.finite(G) || G == 0) return(NULL)
      u <- g / G
      list(b = b, u = u, G = G, Y = drop(X100[, b, drop = FALSE] %*% u))
    })
    others <- setdiff(cc$nms, target)
    preds <- others[abs(cc$Tot[target, others]) > 1e-12]       # constructs with a total effect on the target
    preds <- preds[!vapply(info[preds], is.null, TRUE)]
    it <- info[[target]]
    con <- data.frame(Construct = character(), Importance = numeric(), Performance = numeric())
    idf <- data.frame(Construct = character(), Indicator = character(), Importance = numeric(), Performance = numeric())
    if (!is.null(it)) for (k in preds) {
      imp <- cc$Tot[target, k] * info[[k]]$G / it$G        # unstandardized total effect k -> target
      con <- rbind(con, data.frame(Construct = k, Importance = imp, Performance = mean(info[[k]]$Y)))
      b <- info[[k]]$b
      idf <- rbind(idf, data.frame(Construct = k, Indicator = b, Importance = imp * info[[k]]$u,
                                   Performance = unname(colMeans(X100[, b, drop = FALSE])), row.names = NULL))
    }
    ks <- cc$nms[!vapply(info, is.null, TRUE)]
    neg <- ks[vapply(info[ks], function(z) any(z$u < 0), TRUE)]
    list(constructs = con, indicators = idf, target = target,
         target_perf = if (!is.null(it)) mean(it$Y) else NA_real_,
         neg = neg, skipped = setdiff(cc$nms, ks))
  }
  ipma_r <- reactive({
    req(rv$res, input$ipma_t); cc <- calc(); req(input$ipma_t %in% cc$endo)
    ipma_fun(cc, input$ipma_t, data_r())
  })
  observe({
    req(rv$res); cc <- calc()
    fin <- setdiff(cc$endo, unlist(cc$preds))               # default target: an endogenous construct that predicts nothing
    cur <- isolate(rv$ipma_tg); if (is.null(cur) || !nzchar(cur)) cur <- isolate(input$ipma_t)
    updateSelectInput(session, "ipma_t", choices = cc$endo,
                      selected = if (!is.null(cur) && cur %in% cc$endo) cur else if (length(fin)) fin[length(fin)] else tail(cc$endo, 1))
  })
  observeEvent(input$ipma_t, { if (!is.null(input$ipma_t) && nzchar(input$ipma_t)) rv$ipma_tg <- input$ipma_t })
  observe({                                                 # targets offered in the IPMA dialog = constructs that receive a path
    tg <- unique(rv$paths$to)
    updateSelectInput(session, "ipma_tgt", choices = tg, selected = isolate(input$ipma_tgt))
  })
  output$ipma_plot <- renderPlot({
    I <- ipma_r(); con <- input$ipma_lvl != "ind"
    d <- if (con) I$constructs else I$indicators
    validate(need(nrow(d) > 0, "No predecessor constructs with a total effect on the target (or indicators not found in the data)."))
    lab <- if (con) d$Construct else paste0(d$Indicator, " (", d$Construct, ")")
    xr <- range(c(0, d$Importance)); xp <- diff(xr) * 0.12; if (xp == 0) xp <- 0.1
    yr <- range(d$Performance); yp <- max(diff(yr) * 0.15, 3)
    op <- par(mar = c(4.5, 4.5, 3, 1)); on.exit(par(op))
    plot(d$Importance, d$Performance, pch = 19, cex = 1.6, col = "#2c7fb8",
         xlim = c(xr[1] - xp * 0.3, xr[2] + xp), ylim = c(max(0, yr[1] - yp), min(100, yr[2] + yp)),
         xlab = paste0("Importance (unstandardized total effect on ", I$target, ")"),
         ylab = "Performance (0-100)",
         main = sprintf("Importance-performance map - target: %s (performance %.2f)", I$target, I$target_perf))
    abline(v = mean(d$Importance), h = mean(d$Performance), lty = 2, col = "grey40")
    text(d$Importance, d$Performance, lab, pos = 3, cex = 0.9)
  })
  output$ipma_tab <- renderUI({
    I <- ipma_r()
    tagList(
      p(em(sprintf("Target construct: %s (average performance %.3f). Importance = unstandardized total effect; performance = mean of 0-100 rescaled scores. Dashed lines in the map are the averages.",
                   I$target, I$target_perf))),
      html_tab(I$constructs, "Construct level"),
      html_tab(I$indicators, "Indicator level"),
      if (length(I$neg)) p(em(paste("Warning: negative outer weights in", paste(I$neg, collapse = ", "),
                                    "- performance values of these constructs may fall outside 0-100 and should be interpreted with caution."))),
      if (length(I$skipped)) p(em(paste("Not included (indicators not found in the data, e.g. moderator terms):", paste(I$skipped, collapse = ", ")))))
  })
  output$ipma_xlsx <- downloadHandler(
    filename = function() paste0("cSEM_IPMA_", input$ipma_t, "_", Sys.Date(), ".xlsx"),
    content = function(file) {
      I <- ipma_r()
      writexl::write_xlsx(list(`Construct level` = I$constructs, `Indicator level` = I$indicators), file) })

  # ================= confirmatory tetrad analysis (CTA-PLS) =================
  # Own implementation (Bollen & Ting 1993; Gudergan et al. 2008): for each common-factor construct with >= 4 indicators
  # the non-redundant tetrads are tested against zero with a bootstrap.
  do_cta <- function() {
    if (is.null(rv$res) && !estimate(FALSE)) return()
    cons <- Filter(function(k) identical(k$type, "cf") && length(k$items) >= 4, rv$cons)
    skipped <- vapply(Filter(function(k) !(identical(k$type, "cf") && length(k$items) >= 4), rv$cons), function(k) k$name, "")
    if (!length(cons)) {
      showNotification("CTA needs at least one common-factor (reflective) construct with four or more indicators.", type = "error", duration = 10); return() }
    R <- suppressWarnings(as.integer(input$cta_R)); if (is.na(R) || R < 100) { showNotification("Use at least 100 resamples.", type = "error"); return() }
    seed <- suppressWarnings(as.integer(input$cta_seed)); if (is.na(seed)) seed <- 123L
    al <- as.numeric(input$cta_alpha); adj <- input$cta_adj
    d0 <- as.data.frame(rv$args$.data)
    res <- list()
    withProgress(message = "Confirmatory tetrad analysis...", value = 0, {
      for (ci in seq_along(cons)) {
        k <- cons[[ci]]; v <- k$items; X <- as.matrix(d0[, v, drop = FALSE]); X <- X[complete.cases(X), , drop = FALSE]; N <- nrow(X)
        out <- tryCatch({
          S <- cov(X); T <- tet_set(S, length(v)); tix <- tet_index(T, length(v)); est <- tet_vals(S, tix)
          set.seed(seed + ci)
          M <- matrix(NA_real_, R, length(T))
          for (b in seq_len(R)) { Sb <- cov(X[sample.int(N, N, replace = TRUE), , drop = FALSE]); M[b, ] <- tet_vals(Sb, tix)
            if (b %% 50 == 0) setProgress(value = (ci - 1 + b / R) / length(cons), detail = sprintf("%s: %d / %d", k$name, b, R)) }
          se <- apply(M, 2, sd, na.rm = TRUE); tt <- est / se
          p <- 2 * (1 - pnorm(abs(tt))); padj <- p.adjust(p, adj)
          kk <- length(T); a2 <- if (adj == "none") al else al / kk
          lo <- apply(M, 2, quantile, probs = a2 / 2, na.rm = TRUE); hi <- apply(M, 2, quantile, probs = 1 - a2 / 2, na.rm = TRUE)
          list(tab = data.frame(Tetrad = tet_names(T, v), `Original sample (O)` = est, `Standard deviation (STDEV)` = se,
                                `T statistics (|O/STDEV|)` = abs(tt), `P values` = p, `P values (adjusted)` = padj,
                                `CI low` = lo, `CI high` = hi,
                                `Reject H0 (not reflective)` = ifelse(padj < al, "Yes", "No"), check.names = FALSE),
               n = N, rej = sum(padj < al))
        }, error = function(e) { showNotification(paste0("CTA failed for ", k$name, ": ", conditionMessage(e)), type = "error", duration = 12); NULL })
        if (!is.null(out)) res[[k$name]] <- out
      }
    })
    if (!length(res)) return()
    rv$cta <- list(res = res, alpha = al, adj = adj, R = R, seed = seed, skipped = skipped)
    updateTabsetPanel(session, "main", "Confirmatory tetrad analysis")
    showNotification("Confirmatory tetrad analysis finished.", type = "message", duration = 3)
  }
  cta_tabs <- reactive({
    ct <- rv$cta; req(ct)
    summ <- do.call(rbind, lapply(names(ct$res), function(k) { z <- ct$res[[k]]
      data.frame(Construct = k, `Number of tetrads` = nrow(z$tab), `Significant tetrads` = z$rej,
                 Conclusion = if (z$rej > 0) "Reflective model rejected - consider composite / formative" else "Reflective model not rejected",
                 check.names = FALSE) }))
    c(list(Summary = summ), setNames(lapply(ct$res, function(z) z$tab), paste("Construct:", names(ct$res))))
  })
  output$cta_info <- renderUI({
    ct <- rv$cta; req(ct)
    tagList(h4("Confirmatory tetrad analysis (CTA-PLS)"),
      p(em(sprintf("%d bootstrap resamples (seed %d); significance level %s; p-value adjustment: %s (the confidence interval uses alpha/number of tetrads unless no adjustment is chosen). H0: the non-redundant model-implied tetrads are zero (reflective specification). A significant tetrad rejects H0.",
                   ct$R, ct$seed, format(ct$alpha), ct$adj))),
      if (length(ct$skipped)) p(em(paste("Not tested (composite or fewer than four indicators):", paste(ct$skipped, collapse = ", ")))))
  })
  output$cta_tab <- renderUI({ tabs_out(cta_tabs()) })
  output$cta_xlsx <- downloadHandler(
    filename = function() paste0("cSEM_CTA_", Sys.Date(), ".xlsx"),
    content = function(file) {
      L <- lapply(cta_tabs(), as.data.frame)
      nm <- sprintf("%02d %s", seq_along(L), gsub("[][*?/\\\\:]", "", substr(names(L), 1, 25)))
      writexl::write_xlsx(setNames(L, nm), file) })

  # ================= PLSpredict & CVPAT =================
  do_pred <- function(mode) {
    if (is.null(rv$res) && !estimate(FALSE)) return()
    if (!identical(rv$res$Information$Model$model_type, "Linear")) {
      showNotification("PLSpredict / CVPAT work only for linear models (remove moderators and quadratic effects).", type = "error", duration = 10); return() }
    k  <- suppressWarnings(as.integer(input$p_folds)); if (is.na(k) || k < 2) { showNotification("Use at least 2 folds.", type = "error"); return() }
    R  <- suppressWarnings(as.integer(input$p_reps));  if (is.na(R) || R < 1) { showNotification("Use at least 1 repetition.", type = "error"); return() }
    sd <- suppressWarnings(as.integer(input$p_seed));  if (is.na(sd)) sd <- 123L
    t0 <- proc.time()[["elapsed"]]
    pr <- withProgress(message = "Cross-validated prediction...", value = 0,
      tryCatch(pred_cv(rv$res, rv$args, k, R, sd, input$p_appr, input$p_inadm,
                       tick = function(v, detail) setProgress(value = v, detail = detail)),
               error = function(e) { showNotification(conditionMessage(e), type = "error", duration = 12); NULL }))
    if (is.null(pr)) return()
    rv$pred <- c(pr, list(k = k, R = R, seed = sd, approach = input$p_appr, secs = proc.time()[["elapsed"]] - t0))
    updateTabsetPanel(session, "main", "PLSpredict")
    showNotification(sprintf("Cross-validation finished in %.1f s.", rv$pred$secs), type = "message", duration = 4)
  }
  pred_tabs <- reactive({ pr <- rv$pred; req(pr); pred_tables(pr) })
  cv_tabs   <- reactive({ pr <- rv$pred; req(pr); cvpat_tables(pr, input$cv_test != "onesided", as.numeric(input$cv_alpha)) })
  output$pred_info <- renderUI({
    pr <- rv$pred; req(pr)
    tagList(h4("PLSpredict"),
      p(em(sprintf("%d-fold cross-validation, %d repetition(s), seed %d, %s approach (%.1f s); n = %d. Benchmarks: LM = each endogenous indicator regressed on all exogenous indicators (with intercept); IA = training-sample mean of the indicator. Q2 predict as in cSEM. Guideline: if all (most / few / no) indicators have a lower RMSE than the LM benchmark, the predictive power is high (medium / low / none).",
                   pr$k, pr$R, pr$seed, if (pr$approach == "direct") "direct antecedent" else "earliest antecedent", pr$secs, pr$N))))
  })
  output$pred_tab <- renderUI({ L <- pred_tabs(); tagList(lapply(names(L), function(n) html_tab(L[[n]], n, "PLSpredict"))) })
  output$cv_info <- renderUI({
    pr <- rv$pred; req(pr)
    tagList(h4("Cross-validated predictive ability test (CVPAT)"),
      p(em(sprintf("%d-fold cross-validation, %d repetition(s), seed %d, n = %d. Loss of an observation = squared prediction error averaged over the indicators (overall, or those of one endogenous construct); losses are averaged over the repetitions. Average loss difference = benchmark loss minus PLS loss (positive = PLS predicts better). T = mean difference / sqrt(var / n), df = n - 1, %s test, significance level %s.",
                   pr$k, pr$R, pr$seed, pr$N, if (input$cv_test == "onesided") "one-tailed" else "two-tailed", input$cv_alpha))))
  })
  output$cv_tab <- renderUI({ L <- cv_tabs(); tagList(lapply(names(L), function(n) html_tab(L[[n]], n))) })
  xl_names <- function(L) sprintf("%02d %s", seq_along(L), gsub("[][*?/\\\\:]", "", substr(names(L), 1, 25)))
  output$pred_xlsx <- downloadHandler(
    filename = function() paste0("cSEM_PLSpredict_", Sys.Date(), ".xlsx"),
    content = function(file) { L <- lapply(pred_tabs(), as.data.frame); writexl::write_xlsx(setNames(L, xl_names(L)), file) })
  output$cv_xlsx <- downloadHandler(
    filename = function() paste0("cSEM_CVPAT_", Sys.Date(), ".xlsx"),
    content = function(file) { L <- lapply(cv_tabs(), as.data.frame); writexl::write_xlsx(setNames(L, xl_names(L)), file) })

  # ================= bootstrapping (parallel) =================
  # Resamples are estimated in waves: the main process draws the row indices (so results are reproducible
  # for a given seed, whatever the number of workers) and the workers estimate them with cSEM.
  do_boot <- function(plsc = FALSE) {
    if (!estimate(plsc)) return()
    B <- suppressWarnings(as.integer(input$bR))
    if (is.na(B) || B < 50) { showNotification("Use at least 50 resamples.", type = "error"); return() }
    seed <- suppressWarnings(as.integer(input$bseed)); if (is.na(seed)) seed <- 123L
    w <- suppressWarnings(as.integer(input$bcores)); if (is.na(w) || w < 1L) w <- 1L
    w <- min(w, B, 32L)
    fitf <- isTRUE(input$bfit)
    r0 <- rv$res; a0 <- rv$args; a0$.data <- as.data.frame(a0$.data); N <- nrow(a0$.data)
    cc0 <- calc()
    refs <- list(P = cc0$P, Ind = nz(cc0$Ind), Tot = nz(cc0$Tot),
                 L = nz(cc0$L[cc0$nmc, , drop = FALSE]), W = nz(cc0$W[cc0$nmc, , drop = FALSE]),
                 sp = spec_paths(cc0$B))
    orig <- tryCatch(boot_stat(cc0, r0, refs, fitf), error = function(e) {
      showNotification(paste("Bootstrap setup failed:", conditionMessage(e)), type = "error", duration = 12); NULL })
    if (is.null(orig)) return()
    ctx <- list(args = a0, Wo = r0$Estimates$Weight_estimates, refs = refs, fit = fitf)

    cl <- NULL
    if (w > 1L) {
      cl <- withProgress(message = "Starting parallel workers...", get_pool(w))
      if (is.null(cl)) showNotification("Parallel workers could not be started; running on a single core.", type = "warning", duration = 8)
      else parallel::clusterCall(cl, on_global(function(x) { assign(".ctx", x, envir = globalenv()); NULL }), ctx)
    }
    nw    <- if (is.null(cl)) 1L else w
    chunk <- if (is.null(cl)) 10L else max(2L, min(25L, ceiling(B / (w * 20))))
    boots <- vector("list", B); done <- 0L; t0 <- proc.time()[["elapsed"]]
    set.seed(seed)
    withProgress(message = if (nw > 1L) sprintf("Bootstrapping (%d workers)...", nw) else "Bootstrapping...", value = 0, {
      while (done < B) {
        m   <- min(B - done, nw * chunk)
        idx <- lapply(seq_len(m), function(i) sample.int(N, N, replace = TRUE))
        res <- NULL
        if (!is.null(cl)) {
          parts <- unname(split(idx, cut(seq_along(idx), min(nw, m), labels = FALSE)))
          res <- tryCatch(
            do.call(c, parallel::clusterApply(cl, parts, on_global(function(ch) .eng$boot_chunk(ch, .ctx)))),
            error = function(e) { stop_pool(); NULL })
          if (is.null(res)) {
            cl <- NULL; nw <- 1L
            showNotification("A parallel worker failed; continuing on a single core.", type = "warning", duration = 8)
          }
        }
        if (is.null(res)) res <- boot_chunk(idx, ctx)
        boots[done + seq_len(m)] <- res
        done <- done + m
        setProgress(value = done / B, detail = sprintf("%d / %d", done, B))
      }
    })
    ok <- Filter(Negate(is.null), boots)
    if (length(ok) < 10) { showNotification("Too few valid resamples. Check the model and data.", type = "error"); return() }
    secs <- proc.time()[["elapsed"]] - t0
    rv$bt <- list(orig = orig, M = stack_boots(orig, ok), B = B, ok = length(ok), seed = seed, n = cc0$n,
                  plsc = plsc, workers = nw, secs = secs)
    showNotification(sprintf("Bootstrapping finished: %d valid resamples in %.1f s (%d worker%s).",
                             length(ok), secs, nw, if (nw > 1L) "s" else ""), type = "message", duration = 6)
  }
  observeEvent(input$start, {
    m <- input$start$mode
    if (identical(m, "pls") || identical(m, "plsc")) {
      if (estimate(identical(m, "plsc"))) showNotification(if (m == "plsc") "Consistent PLS-SEM finished." else "PLS-SEM finished.", type = "message", duration = 3)
    } else if (identical(m, "boot") || identical(m, "bootc")) do_boot(identical(m, "bootc"))
    else if (identical(m, "cta")) do_cta()
    else if (identical(m, "pred")) do_pred(m)
    else if (identical(m, "ipma")) {
      if (is.null(rv$res) && !estimate(FALSE)) return()
      tg <- input$ipma_tgt; if (!is.null(tg) && nzchar(tg)) rv$ipma_tg <- tg
      rv$ipma_on <- TRUE
      if (!is.null(tg) && nzchar(tg)) updateSelectInput(session, "ipma_t", selected = tg)
      updateTabsetPanel(session, "main", "IPMA")
    }
  })

  # one bootstrap section on demand (the display only needs the selected table)
  bt_one <- function(sec) {
    bt <- rv$bt
    boot_table(bt$orig[[sec]], bt$M[[sec]], bt$n, as.numeric(input$balpha), input$btail == "two", input$bci == "bc")
  }
  bt_all <- reactive({ req(rv$bt); lapply(setNames(BS, BS), bt_one) })
  output$btrep <- renderUI({
    req(input$bsec, rv$bt); bt <- rv$bt
    tagList(h4(input$bsec),
            p(em(paste0(if (isTRUE(bt$plsc)) "Consistent PLS (PLSc): " else "PLS: ",
                        sprintf("%d valid resamples of %d requested (seed %d, %.1f s on %d worker%s); %s test, significance level %s; %s confidence interval.",
                                bt$ok, bt$B, bt$seed, bt$secs, bt$workers, if (bt$workers > 1L) "s" else "",
                                if (input$btail == "two") "two-tailed" else "one-tailed", input$balpha,
                                if (input$bci == "bc") "bias-corrected" else "percentile")))),
            html_tab(bt_one(input$bsec), input$bsec))
  })
  output$bxlsx <- downloadHandler(
    filename = function() paste0("cSEM_bootstrap_", Sys.Date(), ".xlsx"),
    content = function(file) {
      L <- lapply(bt_all(), as.data.frame)
      nm <- sprintf("%02d %s", seq_along(L), gsub("[][*?/\\\\:]", "", substr(names(L), 1, 20)))
      writexl::write_xlsx(setNames(L, nm), file) })

  rep_l <- reactive({
    cc <- calc(); list2env(cc, environment()); OUT <- list()
    add <- function(sec, name, x) { f <- flat(x); for (nn in names(f)) OUT[[length(OUT) + 1]] <<- list(sec = sec, name = name, df = f[[nn]]) }
    safe <- function(sec, expr) tryCatch(expr, error = function(e) add(sec, "Error", data.frame(Message = conditionMessage(e))))

    safe(SECS[1], { add(SECS[1], "Matrix", tm(P)); add(SECS[1], "List", dlist(P, "Path coefficients")) })
    safe(SECS[2], { add(SECS[2], "Matrix", tm(nz(Ind))); add(SECS[2], "List", dlist(nz(Ind), "Total indirect effects")) })
    safe(SECS[3], add(SECS[3], "List", spec(B)))
    safe(SECS[4], { add(SECS[4], "Matrix", tm(nz(Tot))); add(SECS[4], "List", dlist(nz(Tot), "Total effects")) })
    safe(SECS[5], { Lm <- nz(L[nmc, , drop = FALSE]); add(SECS[5], "Matrix", t(Lm)); add(SECS[5], "List", dlist(Lm, "Outer loadings", " <- ")) })
    safe(SECS[6], { Wm <- nz(W[nmc, , drop = FALSE]); add(SECS[6], "Matrix", t(Wm)); add(SECS[6], "List", dlist(Wm, "Outer weights", " <- ")) })
    safe(SECS[7], { add(SECS[7], "Scores", idx(S)); add(SECS[7], "Correlations", Rm)
                    add(SECS[7], "Covariances", cov(S)); add(SECS[7], "Descriptives", desc(S)) })
    safe(SECS[8], {
      Eo <- do.call(cbind, lapply(nmc, function(k) { b <- blocks[[k]]; X[, b, drop = FALSE] - outer(S[, k], L[k, b]) }))
      add(SECS[8], "Outer model - Scores", idx(Eo)); add(SECS[8], "Outer model - Correlations", cor(Eo))
      add(SECS[8], "Outer model - Descriptives", desc(Eo))
      if (length(endo)) {
        Ei <- do.call(cbind, lapply(setNames(endo, endo), function(y) drop(S[, y] - S[, preds[[y]], drop = FALSE] %*% P[y, preds[[y]]])))
        add(SECS[8], "Inner model - Scores", idx(Ei)); add(SECS[8], "Inner model - Correlations", cor(Ei))
        add(SECS[8], "Inner model - Descriptives", desc(Ei)) } })
    safe(SECS[9], add(SECS[9], "Overview", data.frame(` ` = endo, `R-square` = unname(R2),
          `R-square adjusted` = vapply(endo, function(y) 1 - (1 - R2[[y]]) * (n - 1) / (n - length(preds[[y]]) - 1), 0), check.names = FALSE)))
    safe(SECS[10], { F2 <- P; F2[] <- NA
      for (y in endo) for (pp in preds[[y]]) F2[y, pp] <- (R2[[y]] - r2f(y, setdiff(preds[[y]], pp))) / (1 - R2[[y]])
      add(SECS[10], "Matrix", tm(F2)); add(SECS[10], "List", dlist(F2, "f-square")) })

    rel <- NULL
    safe(SECS[11], {
      rel <- rel_fun(cc)
      add(SECS[11], "Overview", rel) })

    safe(SECS[12], {
      FL <- Rm; fd <- sqrt(rel[[5]][match(nms, rel[[1]])]); fd[is.na(fd)] <- 1; diag(FL) <- fd; FL[upper.tri(FL)] <- NA
      add(SECS[12], "Fornell-Larcker criterion", FL)
      H1 <- ht_fun(cc, FALSE); H2 <- ht_fun(cc, TRUE)
      add(SECS[12], "Heterotrait-monotrait ratio (HTMT) - Matrix", H1)
      add(SECS[12], "Heterotrait-monotrait ratio (HTMT) - List", dlist(H1, "Heterotrait-monotrait ratio (HTMT)", " <-> "))
      add(SECS[12], "HTMT2 - Matrix", H2); add(SECS[12], "HTMT2 - List", dlist(H2, "HTMT2", " <-> ")) })

    safe(SECS[13], {
      ov <- do.call(rbind, lapply(nmc, function(k) { b <- blocks[[k]]
        data.frame(` ` = b, VIF = if (length(b) < 2) 1 else diag(solve(cor(X[, b, drop = FALSE]))), check.names = FALSE) }))
      add(SECS[13], "Outer model - List", ov)
      V <- P; V[] <- NA
      for (y in endo) { pr <- preds[[y]]; V[y, pr] <- if (length(pr) < 2) 1 else diag(solve(Rm[pr, pr])) }
      add(SECS[13], "Inner model - Matrix", tm(V)); add(SECS[13], "Inner model - List", dlist(V, "VIF")) })

    safe(SECS[14], {
      fit <- function(nm) vapply(c(TRUE, FALSE), function(sat) fit_one(r, nm, sat), 0)
      ft <- list(SRMR = "calculateSRMR", d_ULS = "calculateDL", d_G = "calculateDG", `Chi-square` = "calculateChiSquare", NFI = "calculateNFI")
      m <- do.call(rbind, lapply(ft, fit))
      add(SECS[14], "Fit summary", data.frame(` ` = names(ft), `Saturated model` = m[, 1], `Estimated model` = m[, 2], check.names = FALSE)) })
    OUT
  })
  output$rep <- renderUI({
    req(input$sec); L <- Filter(function(z) z$sec == input$sec, rep_l())
    tagList(h4(input$sec), lapply(L, function(z) html_tab(z$df, z$name, z$sec))) })
  output$xlsx <- downloadHandler(
    filename = function() paste0("cSEM_report_", Sys.Date(), ".xlsx"),
    content = function(file) {
      L <- rep_l(); sh <- lapply(L, function(z) as.data.frame(z$df))
      nm <- sprintf("%02d %s", seq_along(L), gsub("[][*?/\\\\:]", "", substr(vapply(L, `[[`, "", "sec"), 1, 20)))
      writexl::write_xlsx(setNames(sh, nm), file) })
}

shinyApp(ui, server)

