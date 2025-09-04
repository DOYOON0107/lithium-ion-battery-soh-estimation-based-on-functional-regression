fdasrvf::elastic.pcr.regression

elastic.pcr.regression.cv <- function(
    f, y, time,
    pca.method = "combined",
    no = 5,                 # k 탐색의 최대 개수(upper bound)
    smooth_data = FALSE, sparam = 25,
    parallel = FALSE, C = NULL
){
  # --- 입력 체크 & 전처리 ---
  stopifnot(is.matrix(f), length(y) == ncol(f))
  n  <- length(y)         # 샘플 수 (곡선 개수)
  Tn <- nrow(f)           # 시간 샘플 길이
  
  # pca.method 파싱
  pca.method.str <- pca.method
  pca.method.idx <- pmatch(pca.method, c("combined", "vert", "horiz"))
  if (is.na(pca.method.idx)) stop("invalid method selection")
  
  # 스무딩(옵션)
  if (smooth_data) {
    f <- smooth.data(f, sparam)
  }
  
  # --- 1) time warping (정렬) ---
  warp_out <- time_warping(f, time, parallel = parallel)
  
  # --- 2) FPCA (일단 no 성분까지 분해) ---
  fpca_fit <- switch(
    as.character(pca.method.idx),
    `1` = jointFPCA(warp_out, no = no, showplot = FALSE, C = C),
    `2` = vertFPCA (warp_out, no = no, showplot = FALSE),
    `3` = horizFPCA(warp_out, no = no, showplot = FALSE)
  )
  # scores: N x no  (샘플별 PC 점수)
  scores_full <- fpca_fit$coef
  stopifnot(is.matrix(scores_full), nrow(scores_full) == n)
  
  # --- 3) k = 1..no 범위로 디자인행렬 만들고 OLS 적합, GCV 계산 ---
  #     자유도(df)=k+1(절편 포함). df < n 보장 위해 k ≤ n-1 로 제한
  k_max_eff <- min(no, n - 1L)
  if (k_max_eff < 1L) stop("not enough samples to fit even 1 PC (need n >= 2)")
  
  lam <- 0      # 릿지 계수(현재 0 → OLS)
  R   <- 0      # 패널티 행렬 placeholder
  
  fit_k <- function(k){
    Phi <- cbind(Intercept = 1, scores_full[, seq_len(k), drop = FALSE])  # N x (k+1)
    xx  <- crossprod(Phi)     # t(Phi)%*%Phi
    xy  <- crossprod(Phi, y)  # t(Phi)%*%y
    beta <- tryCatch(solve(xx + lam * R, xy), error = function(e){
      if (!requireNamespace("MASS", quietly = TRUE)) stop(e)
      MASS::ginv(xx + lam * R) %*% xy
    })
    alpha <- as.numeric(beta[1])
    b     <- as.numeric(beta[-1])
    yhat  <- as.numeric(Phi[, -1, drop = FALSE] %*% b + alpha)
    sse   <- sum((y - yhat)^2)
    
    df    <- k + 1L
    denom <- (n - df)
    gcv   <- if (denom > 0) n * sse / (denom^2) else Inf
    
    list(k = k, alpha = alpha, b = b, SSE = sse, GCV = gcv)
  }
  
  fits <- lapply(1:k_max_eff, fit_k)
  cv_table <- data.frame(
    k   = vapply(fits, `[[`, integer(1), "k"),
    SSE = vapply(fits, `[[`, numeric(1), "SSE"),
    GCV = vapply(fits, `[[`, numeric(1), "GCV")
  )
  best_idx <- which.min(cv_table$GCV)
  best_k   <- cv_table$k[best_idx]
  alpha    <- fits[[best_idx]]$alpha
  b        <- fits[[best_idx]]$b
  SSE_best <- fits[[best_idx]]$SSE
  
  # --- 4) 선택된 best_k로 FPCA를 "다시" 적합하여 predict와 차원 일치 보장 ---
  fpca_best <- switch(
    as.character(pca.method.idx),
    `1` = jointFPCA(warp_out, no = best_k, showplot = FALSE, C = C),
    `2` = vertFPCA (warp_out, no = best_k, showplot = FALSE),
    `3` = horizFPCA(warp_out, no = best_k, showplot = FALSE)
  )
  
  # (선택) 일관성 위해 b를 best_k 점수로 한 번 더 재적합
  scores_best <- fpca_best$coef                       # N x best_k
  Phi_best    <- cbind(1, scores_best)                # N x (best_k+1)
  xx_best     <- crossprod(Phi_best)
  xy_best     <- crossprod(Phi_best, y)
  beta_best   <- tryCatch(solve(xx_best + lam * R, xy_best), error = function(e){
    if (!requireNamespace("MASS", quietly = TRUE)) stop(e)
    MASS::ginv(xx_best + lam * R) %*% xy_best
  })
  alpha_best  <- as.numeric(beta_best[1])
  b_best      <- as.numeric(beta_best[-1])
  yhat_best   <- as.numeric(Phi_best[, -1, drop = FALSE] %*% b_best + alpha_best)
  SSE_best    <- sum((y - yhat_best)^2)
  
  # --- 5) 결과 패키징 ---
  out <- list(
    alpha       = alpha_best,
    b           = b_best,             # 길이 = best_k
    y           = y,
    warp_data   = warp_out,
    pca         = fpca_best,          # 'predict'가 best_k 점수를 내도록 보장
    SSE         = SSE_best,
    pca.method  = pca.method.str,
    best_k      = best_k,
    gcv_table   = cv_table
  )
  class(out) <- "pcr"
  return(out)
}
