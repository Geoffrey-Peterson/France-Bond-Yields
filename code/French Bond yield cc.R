install.packages(c("readr","dplyr","zoo","lmtest","sandwich"))
library(readr); library(dplyr); library(zoo); library(lmtest); library(sandwich)

library(readr)
france_panel_quarterly <- read_csv("C:/Users/geoff/Downloads/france_panel_quarterly.csv")
View(france_panel_quarterly)

# 1) Load
france <- read_csv("C:/Users/geoff/Downloads/france_panel_quarterly.csv")

# 2) Time index + sort
france <- france %>% mutate(q = as.yearqtr(q)) %>% arrange(q)

# 3) Lags for RHS
rhs <- c("debt_gdp","deficit_gdp","g_minus_r","hicp_yoy","gdp_yoy","epu","dlog_eurusd")
france <- france %>%
  mutate(across(all_of(rhs), ~ dplyr::lag(.x, 1), .names = "{.col}_L1"))

# 4) Models + HAC
m0 <- lm(yield_10y ~ debt_gdp_L1 + deficit_gdp_L1, data = france)
m1 <- lm(yield_10y ~ debt_gdp_L1 + deficit_gdp_L1 + g_minus_r_L1 + hicp_yoy_L1 + gdp_yoy_L1, data = france)
m2 <- lm(yield_10y ~ debt_gdp_L1 + deficit_gdp_L1 + g_minus_r_L1 + hicp_yoy_L1 + gdp_yoy_L1 + epu_L1 + dlog_eurusd_L1, data = france)

coeftest(m0, vcov = NeweyWest(m0, prewhite = FALSE, adjust = TRUE))
coeftest(m1, vcov = NeweyWest(m1, prewhite = FALSE, adjust = TRUE))
coeftest(m2, vcov = NeweyWest(m2, prewhite = FALSE, adjust = TRUE))

#Second test run
# assuming your current variable is deficit_gdp  (negative when in deficit)

france <- france %>% mutate(deficit_pos_L1 = -deficit_gdp_L1)
m2_defpos <- lm(yield_10y ~ debt_gdp_L1 + deficit_pos_L1 + g_minus_r_L1 +
                  hicp_yoy_L1 + gdp_yoy_L1 + epu_L1 + dlog_eurusd_L1,
                data = france)
coeftest(m2_defpos, vcov = NeweyWest(m2_defpos, prewhite = FALSE, adjust = TRUE))

install.packages("broom")
library(broom)

library(dplyr)
library(lmtest)
library(sandwich)
library(broom)

# ---- 1) Create the "no-GDP" datasets (drop column; drop NAs consistently) ----
fr1_nogdp <- france %>%
  select(yield_10y, debt_gdp_L1, deficit_gdp_L1, g_minus_r_L1, hicp_yoy_L1) %>%
  na.omit()

fr2_nogdp <- france %>%
  select(yield_10y, debt_gdp_L1, deficit_gdp_L1, g_minus_r_L1, hicp_yoy_L1,
         epu_L1, dlog_eurusd_L1) %>%
  na.omit()

# ---- 2) Regressions without GDP growth ----
m1_nogdp <- lm(yield_10y ~ debt_gdp_L1 + deficit_gdp_L1 + g_minus_r_L1 + hicp_yoy_L1,
               data = fr1_nogdp)
m2_nogdp <- lm(yield_10y ~ debt_gdp_L1 + deficit_gdp_L1 + g_minus_r_L1 + hicp_yoy_L1 +
                 epu_L1 + dlog_eurusd_L1,
               data = fr2_nogdp)

# ---- 3) HAC (Newey–West) SEs + t-tests ----
nw1n <- NeweyWest(m1_nogdp, prewhite = FALSE, adjust = TRUE)
nw2n <- NeweyWest(m2_nogdp, prewhite = FALSE, adjust = TRUE)

cat("\n=== M1_noGDP: (g - r), inflation + fiscal ===\n")
print(coeftest(m1_nogdp, vcov = nw1n))

cat("\n=== M2_noGDP: add uncertainty + FX ===\n")
print(coeftest(m2_nogdp, vcov = nw2n))

# ---- 4) Fit stats (for the paper) ----
fits_nogdp <- tibble(
  model = c("M1_noGDP","M2_noGDP"),
  n_obs = c(nobs(m1_nogdp), nobs(m2_nogdp)),
  r2    = c(summary(m1_nogdp)$r.squared, summary(m2_nogdp)$r.squared),
  r2_adj= c(summary(m1_nogdp)$adj.r.squared, summary(m2_nogdp)$adj.r.squared)
)
print(fits_nogdp)

# ---- 5) (Optional) Export tidy table with HAC SEs ----

tidy_hac <- function(mod){
  se <- sqrt(diag(NeweyWest(mod, prewhite = FALSE, adjust = TRUE)))
  out <- broom::tidy(mod)
  out$std.error <- se
  out$statistic <- out$estimate / out$std.error
  out$p.value   <- 2*pnorm(-abs(out$statistic))
  out
}
tab1n <- tidy_hac(m1_nogdp) %>% mutate(model = "M1_noGDP")
tab2n <- tidy_hac(m2_nogdp) %>% mutate(model = "M2_noGDP")
results_nogdp <- bind_rows(tab1n, tab2n) %>%
  select(model, term, estimate, std.error, statistic, p.value)

# write.csv(results_nogdp, "reg_results_HAC_noGDP.csv", row.names = FALSE)
library(broom)
tidy_table <- tidy(model)

tidy_table          # prints in console
View(tidy_table)    # opens a spreadsheet-style viewer in RStudio

library(dplyr)
library(broom)

# your HAC-tidy function (as you wrote)
tidy_hac <- function(mod){
  se <- sqrt(diag(NeweyWest(mod, prewhite = FALSE, adjust = TRUE)))
  out <- broom::tidy(mod)
  out$std.error <- se
  out$statistic <- out$estimate / out$std.error
  out$p.value   <- 2*pnorm(-abs(out$statistic))
  out
}

# build the combined tidy table for your models
tab1n <- tidy_hac(m1_nogdp) %>% mutate(model = "M1_noGDP")
tab2n <- tidy_hac(m2_nogdp) %>% mutate(model = "M2_noGDP")

results_nogdp <- bind_rows(tab1n, tab2n) %>%
  select(model, term, estimate, std.error, statistic, p.value)

# THIS is the table you want to see:
print(results_nogdp)
View(results_nogdp)

install.packages("modelsummary")

library(sandwich)
library(lmtest)
library(modelsummary)

install.packages("rstudioapi")
library(rstudioapi)

# Newey–West vcov matrices (match what you used in coeftest)
V_m1 <- NeweyWest(m1_nogdp, prewhite = FALSE, adjust = TRUE)
V_m2 <- NeweyWest(m2_nogdp, prewhite = FALSE, adjust = TRUE)

msummary(
  list("M1_noGDP" = m1_nogdp, "M2_noGDP" = m2_nogdp),
  statistic = "({std.error}) [{p.value}]",
  vcov = list(V_m1, V_m2),
  gof_omit = "IC|Log|F|RMSE",   # keep this or adjust as you like
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = "Determinants of French Bond Yields (HAC SEs)"
)

library(modelsummary)
library(sandwich)

library(modelsummary)
library(sandwich)

# Newey–West variance-covariance matrices
V_m1 <- NeweyWest(m1_nogdp, prewhite = FALSE, adjust = TRUE)
V_m2 <- NeweyWest(m2_nogdp, prewhite = FALSE, adjust = TRUE)

# Generate table and write directly to a file
modelsummary(
  list("M1_noGDP" = m1_nogdp, "M2_noGDP" = m2_nogdp),
  statistic = "({std.error}) [{p.value}]",
  vcov = list(V_m1, V_m2),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = "IC|Log|F|RMSE",
  title = "Determinants of French Bond Yields (HAC SEs)",
  output = "french_bond_table.tex"
)

modelsummary(
  list("M1_noGDP" = m1_nogdp, "M2_noGDP" = m2_nogdp),
  statistic = "({std.error}) [{p.value}]",
  vcov = list(NeweyWest(m1_nogdp, prewhite = FALSE, adjust = TRUE),
              NeweyWest(m2_nogdp, prewhite = FALSE, adjust = TRUE)),
  stars = c('*'=.1,'**'=.05,'***'=.01),
  gof_omit = "IC|Log|F|RMSE",
  title = "Determinants of French Bond Yields (HAC SEs)",
  output = "french_bond_table.html"   # or "french_bond_table.docx"
)



# =========================
# SAFE REFINEMENT SETUP
# =========================

# 0) Freeze the original cleaned dataset (after you create q + sort)
france_core <- france  # do not modify france_core later

# Optional: save a snapshot so you can always revert exactly
dir.create("outputs", showWarnings = FALSE)
saveRDS(france_core, "outputs/france_core.rds")

# A list to store "refinement models" so nothing overwrites m0/m1/m2
ref_models <- list()


###Step 1 SIMPLE LAG EQUATION####
# 1a) AR(1) baseline (persistence only)
ref_models[["AR1"]] <- lm(yield_10y ~ dplyr::lag(yield_10y, 1), data = france_core)

# 1b) Dynamic baseline: lagged Y + your fiscal vars (L1)
ref_models[["Dyn_fiscal_AR1"]] <- lm(
  yield_10y ~ dplyr::lag(yield_10y, 1) + debt_gdp_L1 + deficit_gdp_L1,
  data = france_core
)

###Choosing Optimal Lags####
# =========================
# 2) Lag selection via IC
# =========================

# helper for HQ (Hannan–Quinn)
HQ <- function(mod){
  n <- stats::nobs(mod)
  k <- length(stats::coef(mod))
  stats::AIC(mod) + 2*k*(log(log(n)) - 1)  # HQ = -2LL + 2k log(log(n))
  # This uses the relationship between AIC and -2LL; good for comparing within same data.
}

# choose max lags (quarterly)
p_max <- 8   # try 8 first
q_max <- 8

# Keep a consistent estimation sample for all candidate models:
df_ic <- france_core %>%
  dplyr::select(
    yield_10y, debt_gdp, deficit_gdp, g_minus_r,
    hicp_yoy, gdp_yoy, epu, dlog_eurusd, q
  ) %>%
  na.omit()


df_ic <- france_core %>%
  dplyr::select(
    yield_10y, debt_gdp, deficit_gdp, g_minus_r,
    hicp_yoy, gdp_yoy, epu, dlog_eurusd, q
  ) %>%
  tidyr::drop_na()


# We'll use dynlm so we don't have to permanently create a bunch of lag columns
install.packages("dynlm")
library(dynlm)
library(zoo)

# Ensure q is yearqtr and data is ordered (you already do this)
df_ic <- df_ic %>% dplyr::mutate(q = as.yearqtr(q)) %>% dplyr::arrange(q)

# Evaluate candidate models: ARDL(p, q) style (lagged y + lags of selected x’s)
nrow(france_core)
nrow(df_ic)
sapply(df_ic, length)
df_ic <- france_core %>%
  dplyr::select(
    yield_10y, debt_gdp, deficit_gdp, g_minus_r,
    hicp_yoy, gdp_yoy, epu, dlog_eurusd, q
  ) %>%
  na.omit() %>%
  dplyr::mutate(q = zoo::as.yearqtr(q)) %>%
  dplyr::arrange(q)
###Error Check
length(france_core$q)
length(df_ic$q)

###Fix:Conver df_ic to a zoo object indexed by a q before the loop
library(zoo)
library(dynlm)
library(dplyr)

# Ensure q is yearqtr and sorted (you already do this)
df_ic <- df_ic %>% arrange(q)

# Convert to zoo (this is the key)
z_ic <- zoo(
  df_ic %>% dplyr::select(-q),
  order.by = df_ic$q
)

##Run loop using data= z_ic
ic_grid <- list()
idx <- 1

for(p in 1:p_max){
  for(qL in 0:q_max){
    
    mod <- dynlm(
      yield_10y ~ L(yield_10y, 1:p) +
        debt_gdp + L(debt_gdp, 1:qL) +
        deficit_gdp + L(deficit_gdp, 1:qL) +
        g_minus_r + L(g_minus_r, 1:qL) +
        hicp_yoy + L(hicp_yoy, 1:qL) +
        gdp_yoy + L(gdp_yoy, 1:qL) +
        epu + L(epu, 1:qL) +
        dlog_eurusd + L(dlog_eurusd, 1:qL),
      data = z_ic
    )
    
    ic_grid[[idx]] <- data.frame(
      p = p,
      q = qL,
      AIC = AIC(mod),
      BIC = BIC(mod),
      HQ  = HQ(mod)
    )
    idx <- idx + 1
  }
}

ic_table <- dplyr::bind_rows(ic_grid) %>% dplyr::arrange(BIC)
head(ic_table, 15)

# Pick winners:
best_BIC <- ic_table[which.min(ic_table$BIC), ]
best_AIC <- ic_table[which.min(ic_table$AIC), ]
best_HQ  <- ic_table[which.min(ic_table$HQ ), ]

best_BIC; best_AIC; best_HQ

##Numerically Degenerate estimated error variance is 0
mod_test <- dynlm(
  yield_10y ~ L(yield_10y, 1:1) +
    debt_gdp + L(debt_gdp, 1:6) +
    deficit_gdp + L(deficit_gdp, 1:6) +
    g_minus_r + L(g_minus_r, 1:6) +
    hicp_yoy + L(hicp_yoy, 1:6) +
    gdp_yoy + L(gdp_yoy, 1:6) +
    epu + L(epu, 1:6) +
    dlog_eurusd + L(dlog_eurusd, 1:6),
  data = z_ic
)

summary(mod_test)
sigma(mod_test)
alias(mod_test)

# =========================
#### STEP 3A: DL (Debt + Deficit)
# =========================

library(dynlm)

q_dl <- 4  # quarterly: 1 year of lags

ref_models[["DL_debt_deficit"]] <- dynlm(
  yield_10y ~
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl) +
    g_minus_r + hicp_yoy + gdp_yoy + epu + dlog_eurusd,
  data = z_ic
)

summary(ref_models[["DL_debt_deficit"]])

###Check Immediately
df.residual(ref_models[["DL_debt_deficit"]])
sigma(ref_models[["DL_debt_deficit"]])
bgtest(ref_models[["DL_debt_deficit"]], order = 4)


###Long Run Fiscal Effects
library(car)
names(coef(ref_models[["DL_debt_deficit"]]))


library(car)

m <- ref_models[["DL_debt_deficit"]]
cn <- names(coef(m))

debt_terms <- c(
  "debt_gdp",
  "L(debt_gdp, 1:q_dl)1",
  "L(debt_gdp, 1:q_dl)2",
  "L(debt_gdp, 1:q_dl)3",
  "L(debt_gdp, 1:q_dl)4"
)

# Build restriction matrix: each row picks one coefficient
R_debt <- diag(length(cn))[match(debt_terms, cn), , drop = FALSE]
R_debt <- R_debt[complete.cases(R_debt), , drop = FALSE]  # safety

linearHypothesis(m, R_debt, rhs = rep(0, nrow(R_debt)))

###Sanity check
print(setdiff(debt_terms, cn))

R_debt <- diag(length(cn))[match(debt_terms, cn), , drop = FALSE]
R_debt <- R_debt[complete.cases(R_debt), , drop = FALSE]

test_debt <- linearHypothesis(m, R_debt, rhs = rep(0, nrow(R_debt)))
print(test_debt)





####Deficit Block(Current+4LAGS)

def_terms <- c(
  "deficit_gdp",
  "L(deficit_gdp, 1:q_dl)1",
  "L(deficit_gdp, 1:q_dl)2",
  "L(deficit_gdp, 1:q_dl)3",
  "L(deficit_gdp, 1:q_dl)4"
)

R_def <- diag(length(cn))[match(def_terms, cn), , drop = FALSE]
R_def <- R_def[complete.cases(R_def), , drop = FALSE]

linearHypothesis(m, R_def, rhs = rep(0, nrow(R_def)))

###Sanity Check
print(setdiff(def_terms, cn))

R_def <- diag(length(cn))[match(def_terms, cn), , drop = FALSE]
R_def <- R_def[complete.cases(R_def), , drop = FALSE]

test_def <- linearHypothesis(m, R_def, rhs = rep(0, nrow(R_def)))
print(test_def)

####Residual Serial Correlation(Main purpose of Step 3A)
bgtest(ref_models[["DL_debt_deficit"]], order = 4)
dwtest(ref_models[["DL_debt_deficit"]])


####RUN CUMULATIVE EFFECT
coef_debt <- coef(ref_models[["DL_debt_deficit"]])[c(
  "debt_gdp",
  "L(debt_gdp, 1:q_dl)1",
  "L(debt_gdp, 1:q_dl)2",
  "L(debt_gdp, 1:q_dl)3",
  "L(debt_gdp, 1:q_dl)4"
)]

coef_def <- coef(ref_models[["DL_debt_deficit"]])[c(
  "deficit_gdp",
  "L(deficit_gdp, 1:q_dl)1",
  "L(deficit_gdp, 1:q_dl)2",
  "L(deficit_gdp, 1:q_dl)3",
  "L(deficit_gdp, 1:q_dl)4"
)]

sum(coef_debt)
sum(coef_def)

###Parameter Economy Check
df.residual(ref_models[["DL_debt_deficit"]])


# =========================
# STEP 3B: Distributed Lag Model
# Add distributed lags of g_minus_r
# (Does NOT alter Step 3A)
# =========================

# Optional: confirm Step 3A model is still there
stopifnot("DL_debt_deficit" %in% names(ref_models))

# Fit Step 3B as a NEW model object
ref_models[["DL_debt_deficit_gr"]] <- dynlm(
  yield_10y ~
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl) +
    g_minus_r + L(g_minus_r, 1:q_dl) +
    hicp_yoy + gdp_yoy + epu + dlog_eurusd,
  data = z_ic
)

# View results
summary(ref_models[["DL_debt_deficit_gr"]])

# Quick diagnostics (recommended)
df.residual(ref_models[["DL_debt_deficit_gr"]])
sigma(ref_models[["DL_debt_deficit_gr"]])

# Serial correlation check (quarterly: order=4 is common)
library(lmtest)
bgtest(ref_models[["DL_debt_deficit_gr"]], order = 4)
dwtest(ref_models[["DL_debt_deficit_gr"]])


###STEP 4 Dynamic distributed lag(ADL/ARDL) Model
# =========================
# STEP 4: Dynamic model (ARDL)
# =========================

ref_models[["ARDL_1_DL_fiscal"]] <- dynlm(
  yield_10y ~
    L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl) +
    g_minus_r + L(g_minus_r, 1:q_dl) +
    hicp_yoy + gdp_yoy + epu + dlog_eurusd,
  data = z_ic
)

summary(ref_models[["ARDL_1_DL_fiscal"]])


###Dynamic Model serial correlation tests
# Serial correlation tests
bgtest(ref_models[["ARDL_1_DL_fiscal"]], order = 4)
dwtest(ref_models[["ARDL_1_DL_fiscal"]])

# Diagnostics
df.residual(ref_models[["ARDL_1_DL_fiscal"]])
sigma(ref_models[["ARDL_1_DL_fiscal"]])


# =========================
# STEP 4 EXTENSION: ARDL(2)
# =========================

ref_models[["ARDL_2_DL_fiscal"]] <- dynlm(
  yield_10y ~
    L(yield_10y, 1:2) +
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl) +
    g_minus_r + L(g_minus_r, 1:q_dl) +
    hicp_yoy + gdp_yoy + epu + dlog_eurusd,
  data = z_ic
)

summary(ref_models[["ARDL_2_DL_fiscal"]])

###Immediate tests
bgtest(ref_models[["ARDL_2_DL_fiscal"]], order = 4)
dwtest(ref_models[["ARDL_2_DL_fiscal"]])
df.residual(ref_models[["ARDL_2_DL_fiscal"]])
sigma(ref_models[["ARDL_2_DL_fiscal"]])


###Lock in ARDL(1) as main model
final_model <- ref_models[["ARDL_1_DL_fiscal"]]

###Compute Newey-West Standard Errors
library(sandwich)
library(lmtest)

coeftest(final_model,
         vcov = NeweyWest(final_model, lag = 4, prewhite = FALSE))

###Extract the persistence parameter
final_model <- ref_models[["ARDL_1_DL_fiscal"]]
coef(final_model)["L(yield_10y, 1)"]
phi <- coef(final_model)["L(yield_10y, 1)"]
phi

###Compute Cumulatiove short-run fiscal effects
debt_terms <- c(
  "debt_gdp",
  "L(debt_gdp, 1:q_dl)1",
  "L(debt_gdp, 1:q_dl)2",
  "L(debt_gdp, 1:q_dl)3",
  "L(debt_gdp, 1:q_dl)4"
)

beta_debt <- coef(final_model)[debt_terms]
sum_beta_debt <- sum(beta_debt)
sum_beta_debt

def_terms <- c(
  "deficit_gdp",
  "L(deficit_gdp, 1:q_dl)1",
  "L(deficit_gdp, 1:q_dl)2",
  "L(deficit_gdp, 1:q_dl)3",
  "L(deficit_gdp, 1:q_dl)4"
)

beta_def <- coef(final_model)[def_terms]
sum_beta_def <- sum(beta_def)
sum_beta_def

gr_terms <- c(
  "g_minus_r",
  "L(g_minus_r, 1:q_dl)1",
  "L(g_minus_r, 1:q_dl)2",
  "L(g_minus_r, 1:q_dl)3",
  "L(g_minus_r, 1:q_dl)4"
)

beta_gr <- coef(final_model)[gr_terms]
sum_beta_gr <- sum(beta_gr)
sum_beta_gr

##STEP 3 Compute Long run fiscal effects
LR_debt <- sum_beta_debt / (1 - phi)
LR_def  <- sum_beta_def  / (1 - phi)
LR_gr   <- sum_beta_gr   / (1 - phi)

LR_debt
LR_def
LR_gr

###STEP 4 Results Table
long_run_effects <- data.frame(
  Variable = c("Debt-to-GDP", "Deficit-to-GDP", "g − r"),
  ShortRun_Cumulative = c(sum_beta_debt, sum_beta_def, sum_beta_gr),
  LongRun_Effect = c(LR_debt, LR_def, LR_gr)
)

long_run_effects

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)


write.csv(long_run_effects,
          "outputs/tables/long_run_fiscal_effects.csv",
          row.names = FALSE)
list.files("outputs/tables")

file.edit("outputs/tables/long_run_fiscal_effects.csv")

ls()
names(ref_models)
summary(ref_models[["ARDL_1_DL_fiscal"]])
summary(ref_models[["DL_debt_deficit"]])

coef(ref_models[["ARDL_1_DL_fiscal"]])

library(lmtest)

bgtest(ref_models[["ARDL_1_DL_fiscal"]], order = 4)
dwtest(ref_models[["ARDL_1_DL_fiscal"]])
df.residual(ref_models[["ARDL_1_DL_fiscal"]])
sigma(ref_models[["ARDL_1_DL_fiscal"]])


long_run_effects
file.show("outputs/tables/long_run_fiscal_effects.csv")

final_model <- ref_models[["ARDL_1_DL_fiscal"]]
summary(final_model)$r.squared
summary(final_model)$adj.r.squared
nobs(final_model)
sigma(final_model)
df.residual(final_model)
summary(final_model)$fstatistic

library(lmtest)
library(sandwich)

coeftest(
  final_model,
  vcov = NeweyWest(final_model, lag = 4, prewhite = FALSE)
)

colnames(z_ic)


##############################################
#########################################
#Final codes
# --- Packages ---
library(zoo)
library(dynlm)
library(lmtest)
library(sandwich)

# Optional (for nice tables)
install.packages(c("modelsummary", "broom", "flextable", "officer"))
library(modelsummary)
library(broom)
library(flextable)
library(officer)

# --- Confirm your data object exists ---
# Your earlier models suggest z_ic is a zoo object
stopifnot(exists("z_ic"))

# --- Set lag length for distributed lags (you used 4 in your model output) ---
q_dl <- 4

# --- Baseline ARDL model (your preferred dynamic spec) ---
m_ardl <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl) +
    g_minus_r + L(g_minus_r, 1:q_dl) +
    hicp_yoy + gdp_yoy + epu + dlog_eurusd,
  data = z_ic
)

# --- Standard summary ---
summary(m_ardl)

# --- Model fit block (like your screenshot) ---
fit <- summary(m_ardl)
cat("\nModel Fit:\n")
cat(sprintf(" • Observations: %d\n", nobs(m_ardl)))
cat(sprintf(" • R²: %.3f\n", fit$r.squared))
cat(sprintf(" • Adjusted R²: %.3f\n", fit$adj.r.squared))
cat(sprintf(" • Overall F-statistic: %.2f (p = %.4g)\n",
            fit$fstatistic[1],
            pf(fit$fstatistic[1], fit$fstatistic[2], fit$fstatistic[3], lower.tail = FALSE)))
    

V_hac <- NeweyWest(m_ardl, lag = 4, prewhite = FALSE, adjust = TRUE)
print(coeftest(m_ardl, vcov. = V_hac))

m_const <- dynlm(yield_10y ~ 1, data = z_ic)
cat("\nHAC Wald (vs constant-only):\n")
print(waldtest(m_const, m_ardl, vcov = NeweyWest(m_ardl, lag = 4, prewhite = FALSE, adjust = TRUE)))

cat("\nDurbin-Watson:\n")
print(dwtest(m_ardl))

cat("\nBreusch-Godfrey (order 4):\n")
print(bgtest(m_ardl, order = 4))

# Create a regression table with HAC SEs in parentheses
tab <- modelsummary(
  m_ardl,
  vcov = V_hac,
  statistic = c("std.error", "p.value"),
  stars = TRUE,
  output = "flextable",
  title = "Table X. Baseline Dynamic Regression Results (ARDL)"
)

# Save to a Word document
save_as_docx(tab, path = "Baseline_ARDL_Table.docx")

#######LONG-Run Multipliers
# Extract coefficient vector
b <- coef(m_ardl)

# Autoregressive coefficient
rho <- b["L(yield_10y, 1)"]

#Helper Function to compute LR Effects
lr_effect <- function(varname, coef_vec, rho) {
  idx <- grep(varname, names(coef_vec))
  sum(coef_vec[idx]) / (1 - rho)
}

####Compute Long-run effects
lr_debt    <- lr_effect("debt_gdp", b, rho)
lr_deficit<- lr_effect("deficit_gdp", b, rho)
lr_gr     <- lr_effect("g_minus_r", b, rho)

###Check long-run effects
lr_debt
lr_deficit
lr_gr

####Long-run standard errors (delta method)
install.packages("msm")
library(msm)

# Variance–covariance matrix (HAC)
V <- NeweyWest(m_ardl, lag = 4, prewhite = FALSE, adjust = TRUE)

# Function to compute LR SE using delta method
lr_se <- function(varname, coef_vec, V, rho) {
  idx <- grep(varname, names(coef_vec))
  g <- rep(0, length(coef_vec))
  g[idx] <- 1 / (1 - rho)
  g[names(coef_vec) == "L(yield_10y, 1)"] <- 
    sum(coef_vec[idx]) / (1 - rho)^2
  sqrt(t(g) %*% V %*% g)
}

se_debt     <- lr_se("debt_gdp", b, V, rho)
se_deficit <- lr_se("deficit_gdp", b, V, rho)
se_gr      <- lr_se("g_minus_r", b, V, rho)

####t-stats and p-values
lr_table <- data.frame(
  Variable = c("Debt/GDP", "Deficit/GDP", "g − r"),
  LR_Effect = c(lr_debt, lr_deficit, lr_gr),
  Std_Error = c(se_debt, se_deficit, se_gr)
)

lr_table$t_stat <- lr_table$LR_Effect / lr_table$Std_Error
lr_table$p_value <- 2 * (1 - pnorm(abs(lr_table$t_stat)))

lr_table

#Clean long-run effects table
library(flextable)

ft_lr <- flextable(lr_table)
ft_lr <- autofit(ft_lr)
ft_lr <- set_caption(ft_lr, "Table Y. Long-Run Fiscal Effects on French 10-Year Bond Yields")

save_as_docx(ft_lr, path = "Long_Run_Effects_Table.docx")

###Re-estimate ARDL With Fewer Lags (2)
# Alternative lag length: 2 lags
m_ardl_l2 <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:2) +
    deficit_gdp + L(deficit_gdp, 1:2) +
    g_minus_r + L(g_minus_r, 1:2) +
    hicp_yoy + gdp_yoy + epu + dlog_eurusd,
  data = z_ic
)

summary(m_ardl_l2)

b2   <- coef(m_ardl_l2)
rho2 <- b2["L(yield_10y, 1)"]

V2 <- NeweyWest(m_ardl_l2, lag = 4, prewhite = FALSE, adjust = TRUE)

lr_debt_2     <- lr_effect("debt_gdp", b2, rho2)
lr_deficit_2 <- lr_effect("deficit_gdp", b2, rho2)
lr_gr_2      <- lr_effect("g_minus_r", b2, rho2)

se_debt_2     <- lr_se("debt_gdp", b2, V2, rho2)
se_deficit_2 <- lr_se("deficit_gdp", b2, V2, rho2)
se_gr_2      <- lr_se("g_minus_r", b2, V2, rho2)


robust_lr <- data.frame(
  Variable = c("Debt/GDP", "Deficit/GDP", "g − r"),
  LR_4Lags = c(lr_debt, lr_deficit, lr_gr),
  LR_2Lags = c(lr_debt_2, lr_deficit_2, lr_gr_2)
)

robust_lr


####Durbin-Watson Test
library(lmtest)

dwtest(m_ardl)

###Breusch-Godfrey Test
bgtest(m_ardl, order = 4)

####Multicollinearity check (VIF)
library(car)

vif(m_ardl)

#####R Code: F stat and t stat
install.packages("ARDL")   # if needed
library(ARDL)
library(zoo)

df <- as.data.frame(z_ic)

###Step 2 — estimate an ARDL model (match your baseline structure)

######You already estimated a dynlm ARDL. For bounds testing, estimate an ARDL::ardl() version using the same regressors and lag orders.
# Example lag order: p = 1 for yield, and 4 lags for the main fiscal regressors.
# You can keep controls contemporaneous (order 0).
m_bnd <- ardl(
  yield_10y ~ debt_gdp + deficit_gdp + g_minus_r + hicp_yoy + gdp_yoy + epu + dlog_eurusd,
  data  = df,
  order = c(1, 4, 4, 4, 0, 0, 0, 0)   # (p, q_debt, q_deficit, q_gr, q_hicp, q_gdp, q_epu, q_fx)
)
####Step 3 — run PSS bounds F-test (cointegration)
##Pick the “case” consistent with your model:
###Case 3 (unrestricted intercept, no trend) is the most common in applied macro if you include a constant but no trend.
set.seed(2020)

bF_asym  <- bounds_f_test(m_bnd, case = 3, alpha = c(0.10, 0.05, 0.01), exact = FALSE)
bF_exact <- bounds_f_test(m_bnd, case = 3, alpha = c(0.10, 0.05, 0.01), exact = TRUE, R = 80000)

bF_asym$tab
bF_exact$tab
##Fix
set.seed(2020)

bF_exact <- bounds_f_test(m_bnd, case = 3, alpha = NULL, exact = TRUE, R = 80000)
bT_exact <- bounds_t_test(m_bnd, case = 3, alpha = NULL, exact = TRUE, R = 80000)

bF_exact$tab
bT_exact$tab
##Fix if you want to specify alpha explicitly
alpha_vec <- c(0.1, 0.05, 0.01)

bF_exact <- bounds_f_test(m_bnd, case = 3, alpha = alpha_vec, exact = TRUE, R = 80000)
bT_exact <- bounds_t_test(m_bnd, case = 3, alpha = alpha_vec, exact = TRUE, R = 80000)

bF_exact$tab
bT_exact$tab

##Fix 1: Omit alpha enitrely
set.seed(2020)
bF_exact <- bounds_f_test(m_bnd, case = 3, exact = TRUE, R = 80000)
bF_exact$tab

class(m_bnd)
summary(m_bnd)
##Ensure No missing Values
sum(!complete.cases(df))

bF_exact


##Table Ready Extraction
names(tabF)

tabF <- as.data.frame(bF_exact$tab)
tabT <- as.data.frame(bT_exact$tab)

# Try to filter for 0.10, 0.05, 0.01 rows (adjust column name if needed)
tabF_10_5_1 <- subset(tabF, alpha %in% c(0.1, 0.05, 0.01))
tabT_10_5_1 <- subset(tabT, alpha %in% c(0.1, 0.05, 0.01))

tabF_10_5_1
tabT_10_5_1

#Bounds t-test
set.seed(2020)
bT_exact <- bounds_t_test(m_bnd, case = 3, exact = TRUE, R = 80000)

bT_exact
bT_exact$tab

# Extract stats
F_stat <- bF_exact$tab["statistic"]
F_p    <- bF_exact$tab["p.value"]

T_stat <- bT_exact$tab["statistic"]
T_p    <- bT_exact$tab["p.value"]

bounds_results <- data.frame(
  Test = c("Bounds F-test (Wald)", "Bounds t-test"),
  Statistic = c(as.numeric(F_stat), as.numeric(T_stat)),
  `p-value (finite-sample / exact)` = c(as.numeric(F_p), as.numeric(T_p)),
  Decision_5pct = c(ifelse(F_p < 0.05, "Reject no cointegration", "Fail to reject"),
                    ifelse(T_p < 0.05, "Reject no cointegration", "Fail to reject"))
)

bounds_results

##Export Word ready table
library(flextable)
library(officer)

ft <- flextable(bounds_results)
ft <- autofit(ft)
ft <- set_caption(ft, "Table X. ARDL Bounds Tests for Cointegration (Case 3, finite-sample exact p-values, T = 52)")

save_as_docx(ft, path = "ARDL_Bounds_Tests_Table.docx")

##Run T-test Block
set.seed(2020)

bT_exact <- bounds_t_test(
  m_bnd,
  case  = 3,
  exact = TRUE,
  R     = 80000
)

bT_exact
bT_exact$tab

####ECM Table
# Long-run equilibrium residuals from your levels ARDL:
ect <- residuals(m_ardl)

# Lag it (ECT_{t-1})
ect_l1 <- stats::lag(ect, -1)  # for zoo/ts alignment; if weird, use: ect_l1 <- dplyr::lag(ect, 1)
ect_l1 <- c(NA, ect[-length(ect)])
###Step A2 — estimate an ECM in differences with ECT_{t-1}
library(dynlm)
library(zoo)

class(z_ic)
ect <- residuals(m_ardl)

# Get the index actually used by m_ardl
idx <- index(model.frame(m_ardl))

# Make ect a zoo series aligned to that index
ect_z <- zoo(as.numeric(ect), order.by = idx)

# Lag it by 1 quarter (ECT_{t-1})
ect_l1_z <- lag(ect_z, -1)

###Step 3-Build clean ECM dataset using aligned series
# Pull the same variables from z_ic and align to idx
z_ecm <- z_ic[idx, c("yield_10y", "debt_gdp", "deficit_gdp", "g_minus_r",
                     "hicp_yoy", "gdp_yoy", "epu", "dlog_eurusd")]

# Add the lagged ECT
z_ecm$ect_l1 <- ect_l1_z

# Drop rows with NAs created by differencing/lagging
z_ecm <- na.omit(z_ecm)

###Step 4-Estimating ECM
library(dynlm)

m_ecm <- dynlm(
  d(yield_10y) ~
    L(d(yield_10y), 1) +
    d(debt_gdp) + L(d(debt_gdp), 1:3) +
    d(deficit_gdp) + L(d(deficit_gdp), 1:3) +
    d(g_minus_r) + L(d(g_minus_r), 1:3) +
    d(hicp_yoy) + d(gdp_yoy) + d(epu) + d(dlog_eurusd) +
    ect_l1,
  data = z_ecm
)

summary(m_ecm)

###Bulletproof ECM code
###Step 0: Set-up
library(zoo)

ect <- residuals(m_ardl)
idx <- index(model.frame(m_ardl))

ect_z   <- zoo(as.numeric(ect), order.by = idx)
ect_l1  <- lag(ect_z, -1)

z_ecm <- merge(
  yield_10y   = z_ic[idx, "yield_10y"],
  debt_gdp    = z_ic[idx, "debt_gdp"],
  deficit_gdp = z_ic[idx, "deficit_gdp"],
  g_minus_r   = z_ic[idx, "g_minus_r"],
  hicp_yoy    = z_ic[idx, "hicp_yoy"],
  gdp_yoy     = z_ic[idx, "gdp_yoy"],
  epu         = z_ic[idx, "epu"],
  dlog_eurusd = z_ic[idx, "dlog_eurusd"],
  ect_l1      = ect_l1
)

z_ecm <- na.omit(z_ecm)

na_counts <- colSums(is.na(z_ecm_raw))
na_counts

class(index(z_ic)); class(idx)


####Build ECT and ECM dataset using yearqtr index
# Number of observations actually used in m_ardl
n_ardl <- nobs(m_ardl)

# Take the last n_ardl quarters from z_ic as the regression sample index
idx_q <- tail(index(z_ic), n_ardl)

class(idx_q)     # should be yearqtr
head(idx_q); tail(idx_q)

ect_z <- zoo(as.numeric(residuals(m_ardl)), order.by = idx_q)
ect_l1_z <- lag(ect_z, -1)

###Build aligned ECM dataset
z_sub <- z_ic[idx_q, ]   # subset z_ic to the same quarters as the ARDL sample

z_ecm_raw <- merge(
  yield_10y   = z_sub[, "yield_10y"],
  debt_gdp    = z_sub[, "debt_gdp"],
  deficit_gdp = z_sub[, "deficit_gdp"],
  g_minus_r   = z_sub[, "g_minus_r"],
  hicp_yoy    = z_sub[, "hicp_yoy"],
  gdp_yoy     = z_sub[, "gdp_yoy"],
  epu         = z_sub[, "epu"],
  dlog_eurusd = z_sub[, "dlog_eurusd"],
  ect_l1      = ect_l1_z
)

# Check missingness by column
colSums(is.na(z_ecm_raw))

# Now omit NAs
z_ecm <- na.omit(z_ecm_raw)

dim(z_ecm)     # should be > 0 rows now
head(z_ecm)

##Compute differences
Dy <- diff(z_ecm[, "yield_10y"])

n_ardl <- nobs(m_ardl)
idx_q <- tail(index(z_ic), n_ardl)
class(idx_q); head(idx_q); tail(idx_q)

ect_z <- zoo(as.numeric(residuals(m_ardl)), order.by = idx_q)
z_sub <- z_ic[idx_q, ]
z_ecm_raw <- merge(yield_10y = z_sub[, "yield_10y"], debt_gdp = z_sub[, "debt_gdp"],
                   deficit_gdp = z_sub[, "deficit_gdp"], g_minus_r = z_sub[, "g_minus_r"],
                   hicp_yoy = z_sub[, "hicp_yoy"], gdp_yoy = z_sub[, "gdp_yoy"],
                   epu = z_sub[, "epu"], dlog_eurusd = z_sub[, "dlog_eurusd"],
                   ect_l1 = lag(ect_z, -1))
colSums(is.na(z_ecm_raw))
dim(na.omit(z_ecm_raw))

###1 Build ECM regressors and estimate ECM
###Step 1: Finalize ECM dataset
z_ecm <- na.omit(z_ecm_raw)   # 51 x 9
###Step 2: Create Δ variables and lags (zoo-safe)
library(zoo)

lagz <- function(x, k) lag(x, -k)  # k=1 gives t-1 aligned at t

Dy    <- diff(z_ecm[, "yield_10y"])
Dy_l1 <- lagz(Dy, 1)

Ddebt <- diff(z_ecm[, "debt_gdp"])
Ddef  <- diff(z_ecm[, "deficit_gdp"])
Dgr   <- diff(z_ecm[, "g_minus_r"])

Dh    <- diff(z_ecm[, "hicp_yoy"])
Dg    <- diff(z_ecm[, "gdp_yoy"])
Depu  <- diff(z_ecm[, "epu"])
Dfx   <- diff(z_ecm[, "dlog_eurusd"])

# ECT_{t-1} aligned to Δy_t dates
ECT <- window(z_ecm[, "ect_l1"], start = start(Dy), end = end(Dy))
###Step 3:Put into one regression dataset (and omit NAs from differencing/lagging)
ecm_z <- na.omit(merge(
  Dy = Dy,
  Dy_l1 = Dy_l1,
  Ddebt = Ddebt, Ddebt_l1 = lagz(Ddebt, 1), Ddebt_l2 = lagz(Ddebt, 2), Ddebt_l3 = lagz(Ddebt, 3),
  Ddef  = Ddef,  Ddef_l1  = lagz(Ddef, 1),  Ddef_l2  = lagz(Ddef, 2),  Ddef_l3  = lagz(Ddef, 3),
  Dgr   = Dgr,   Dgr_l1   = lagz(Dgr, 1),   Dgr_l2   = lagz(Dgr, 2),   Dgr_l3   = lagz(Dgr, 3),
  Dh = Dh, Dg = Dg, Depu = Depu, Dfx = Dfx,
  ect_l1 = ECT
))

ecm_df <- as.data.frame(ecm_z)
dim(ecm_df)
###Step 4: Estimate ECM via lm (stable) + HAC inference
m_ecm <- lm(
  Dy ~ Dy_l1 +
    Ddebt + Ddebt_l1 + Ddebt_l2 + Ddebt_l3 +
    Ddef  + Ddef_l1  + Ddef_l2  + Ddef_l3  +
    Dgr   + Dgr_l1   + Dgr_l2   + Dgr_l3   +
    Dh + Dg + Depu + Dfx +
    ect_l1,
  data = ecm_df
)

summary(m_ecm)

library(lmtest)
library(sandwich)

V_ecm <- NeweyWest(m_ecm, lag = 4, prewhite = FALSE, adjust = TRUE)
coeftest(m_ecm, vcov. = V_ecm)["ect_l1", ]

###ECM Table
library(flextable)
library(officer)

ect_row <- coeftest(m_ecm, vcov. = V_ecm)["ect_l1", ]

ecm_tab <- data.frame(
  Term = "Error-correction term (ECT_{t-1})",
  Expected_Sign = "Negative",
  Estimate = as.numeric(ect_row[1]),
  Std_Error_HAC = as.numeric(ect_row[2]),
  t_stat = as.numeric(ect_row[3]),
  p_value = as.numeric(ect_row[4])
)

ft <- flextable(ecm_tab)
ft <- autofit(ft)
ft <- set_caption(ft, "Table X. Error-Correction Term (ECM)")

save_as_docx(ft, path = "ECM_ECT_Table.docx")

###Re-estimate Newey-West HAC SEs
###A) Baseline ARDL with HAC (formal output)
library(lmtest)
library(sandwich)

V_ardl_hac <- NeweyWest(
  m_ardl,
  lag = 4,
  prewhite = FALSE,
  adjust = TRUE
)

ardl_hac <- coeftest(m_ardl, vcov. = V_ardl_hac)
ardl_hac

###Bootstrap standard errors for long-run effects
###Bootstrap long-run effects (NO dynlm in loop; no zoo merge issues)
###Step 0:Build the fixed regression objects from your estimated ARDL
# Pull the model frame used in m_ardl (already aligned and lagged correctly)
mf <- model.frame(m_ardl)

# y and X used in the ARDL regression
y <- model.response(mf)
X <- model.matrix(m_ardl)

# Coefficients and residuals
b_hat <- coef(m_ardl)
u_hat <- residuals(m_ardl)

# sanity
dim(X)
length(y)
length(u_hat)
###Step 1: Helper to compute LR multipliers from a coefficient vector
lr_from_b <- function(b, X_colnames) {
  # Find rho (lagged dependent variable)
  rho_name <- X_colnames[grepl("^L\\(yield_10y, 1\\)$", X_colnames)]
  rho <- b[rho_name]
  
  # Sums over level + lagged terms
  s_debt <- sum(b[grepl("debt_gdp", X_colnames)])
  s_def  <- sum(b[grepl("deficit_gdp", X_colnames)])
  s_gr   <- sum(b[grepl("g_minus_r", X_colnames)])
  
  c(
    Debt_GDP    = s_debt / (1 - rho),
    Deficit_GDP = s_def  / (1 - rho),
    g_minus_r   = s_gr   / (1 - rho)
  )
}
###Step 2: Bootstrap loop
set.seed(2025)
B <- 2000   # 1000–2000 is good

boot_lr <- matrix(NA, nrow = B, ncol = 3)
colnames(boot_lr) <- c("Debt_GDP", "Deficit_GDP", "g_minus_r")

XtX_inv <- solve(t(X) %*% X)   # precompute for speed
Xty_hat <- t(X) %*% y

for (b in 1:B) {
  u_star <- sample(u_hat, replace = TRUE)
  y_star <- as.numeric(X %*% b_hat + u_star)
  
  # OLS re-estimate quickly (beta = (X'X)^-1 X'y)
  b_star <- XtX_inv %*% (t(X) %*% y_star)
  b_star <- as.numeric(b_star)
  names(b_star) <- colnames(X)
  
  boot_lr[b, ] <- lr_from_b(b_star, colnames(X))
}

boot_lr <- na.omit(boot_lr)
nrow(boot_lr)

###Step 3: Bootstrap SEs + 95% CIs + table
boot_results <- data.frame(
  Variable   = colnames(boot_lr),
  LR_Estimate = c(lr_debt, lr_deficit, lr_gr),  # your point estimates
  Boot_SE    = apply(boot_lr, 2, sd),
  CI_Lower   = apply(boot_lr, 2, quantile, 0.025),
  CI_Upper   = apply(boot_lr, 2, quantile, 0.975)
)

boot_results

library(flextable)
library(officer)

ft <- flextable(boot_results)
ft <- autofit(ft)
ft <- set_caption(ft, "Table X. Bootstrap Long-Run Effects (Residual Bootstrap, conditional on regressors)")

save_as_docx(ft, path = "Bootstrap_LongRun_Effects.docx")

nrow(boot_lr)

boot_results

###PART A — Long-Run Coefficient Comparison
###A1 Prepare the long-run variables (levels)
install.packages("cointReg")
library(cointReg)

# Construct dataframe for cointegration regressions
lr_df <- na.omit(data.frame(
  y = z_ic$yield_10y,
  debt = z_ic$debt_gdp,
  deficit = z_ic$deficit_gdp,
  gr = z_ic$g_minus_r
))
###A2 FMOLS estimation
# Dependent variable
y_lr <- lr_df$y

# Regressor matrix
X_lr <- as.matrix(lr_df[, c("debt", "deficit", "gr")])

library(cointReg)

fmols_mod <- cointRegFM(
  y = y_lr,
  x = X_lr
)

summary(fmols_mod)
coef_fmols <- fmols_mod$beta
se_fmols   <- sqrt(diag(fmols_mod$vcov))

###3 DOLS
dols_mod <- cointRegD(
  y = y_lr,
  x = X_lr,
  nLags = 2,
  nLeads = 2
)

summary(dols_mod)
coef_dols <- dols_mod$beta
se_dols   <- sqrt(diag(dols_mod$vcov))

###4 Build comparison table-ARDL vs FMOLS vs DOLS
lr_compare <- data.frame(
  Variable = c("Debt/GDP", "Deficit/GDP", "g − r"),
  ARDL  = c(lr_debt, lr_deficit, lr_gr),
  FMOLS = coef_fmols,
  DOLS  = coef_dols
)

lr_compare

###SEs in parentheses
lr_compare_se <- data.frame(
  Variable = c("Debt/GDP", "Deficit/GDP", "g − r"),
  ARDL  = paste0(round(c(lr_debt, lr_deficit, lr_gr), 3),
                 " (", round(c(se_debt, se_deficit, se_gr), 3), ")"),
  FMOLS = paste0(round(coef_fmols, 3),
                 " (", round(se_fmols, 3), ")"),
  DOLS  = paste0(round(coef_dols, 3),
                 " (", round(se_dols, 3), ")")
)

lr_compare_se
###Export table
library(flextable)
library(officer)

ft_lr <- flextable(lr_compare)
ft_lr <- autofit(ft_lr)
ft_lr <- set_caption(ft_lr, "Table X. Long-Run Coefficient Comparison: ARDL, FMOLS, and DOLS")

save_as_docx(ft_lr, path = "LR_Comparison_ARDL_FMOLS_DOLS.docx")

#####CUSM AND CUSUMSQ
install.packages("strucchange")
library(zoo)
library(strucchange)
###Step 1 — Build the regression dataset (levels)
names(lr_df)
nrow(lr_df)
lr_df <- na.omit(lr_df)   # just to be safe
###Step 2 — Fit the “stability regression”
stable_mod <- lm(y ~ debt + deficit + gr, data = lr_df)
summary(stable_mod)
###Step 3 — CUSUM (recursive residuals)
###3A) Run the test object
cusum_obj <- efp(y ~ debt + deficit + gr, data = lr_df, type = "Rec-CUSUM")
###3B) Plot it (with 5% boundaries)
plot(cusum_obj, main = "CUSUM Test (Recursive Residuals)")

###Restart
library(strucchange)
###Step 1 — Long-run stability regression (levels)
stable_mod <- lm(y ~ debt + deficit + gr, data = lr_df)
summary(stable_mod)
###Step 2 — CUSUM (recursive residuals + plot)
###2A) Create the CUSUM process
cusum_obj <- efp(
  y ~ debt + deficit + gr,
  data = lr_df,
  type = "Rec-CUSUM"
)
###2B) Plot with 5% bounds
plot(cusum_obj,
     main = "CUSUM Test (Recursive Residuals)",
     ylab = "CUSUM Statistic")
###Step 3 — CUSUMSQ (correct way)
###3A) Formal CUSUMSQ test (this is the key line)
cusumsq_test <- sctest(
  y ~ debt + deficit + gr,
  data = lr_df,
  type = "CUSUMSQ"
)

cusumsq_test

cusum_obj_rec <- efp(y ~ debt + deficit + gr, data = lr_df, type = "Rec-CUSUM")
plot(cusum_obj_rec, main = "CUSUM (Recursive)")
###Formal p-value for the CUSUM process
sctest(cusum_obj)

###2) CUSUMSQ (manual construction from recursive residuals)
###Step 2A: Get recursive residuals
stable_mod <- lm(y ~ debt + deficit + gr, data = lr_df)

rr <- recresid(stable_mod)  # recursive residuals
###Step 2B: Build the CUSUMSQ statistic (Brown–Durbin–Evans)
cusumsq_stat <- cumsum(rr^2) / sum(rr^2)
###Step 2C: Plot it
plot(cusumsq_stat, type = "l",
     main = "CUSUMSQ (Recursive Residuals Squared)",
     ylab = "CUSUMSQ",
     xlab = "Recursive step")
abline(h = c(0,1), lty = 3)
###3) Formal parameter stability (referee-friendly) using supF
###Step 3A: Compute F-stat fluctuation process (supF)
fs <- Fstats(y ~ debt + deficit + gr, data = lr_df)
plot(fs, main = "F-statistics for Structural Change")
###Step 3B: supF test (formal p-value)
supf_test <- sctest(fs, type = "supF")
supf_test

###4) Save plots
png("CUSUM_Plot.png", width = 900, height = 550)
plot(cusum_obj, main = "CUSUM (OLS-based)")
dev.off()

png("CUSUMSQ_Plot.png", width = 900, height = 550)
plot(cusumsq_stat, type = "l",
     main = "CUSUMSQ (Recursive Residuals Squared)",
     ylab = "CUSUMSQ", xlab = "Recursive step")
dev.off()

png("supF_Plot.png", width = 900, height = 550)
plot(fs, main = "F-statistics for Structural Change")
dev.off()

###Outputs
cusum_obj <- efp(y ~ debt + deficit + gr, data = lr_df, type = "OLS-CUSUM")
sctest(cusum_obj)

fs <- Fstats(y ~ debt + deficit + gr, data = lr_df)
sctest(fs, type = "supF")

###Additional plots
library(zoo)
library(ggplot2)
library(scales)

###Plot 1: Yield vs g − r over time
# Convert zoo to data.frame
plot_df <- data.frame(
  time = index(z_ic),
  yield = z_ic$yield_10y,
  g_minus_r = z_ic$g_minus_r
)

ggplot(plot_df, aes(x = time)) +
  geom_line(aes(y = yield, color = "10Y Yield"), size = 1) +
  geom_line(aes(y = g_minus_r, color = "g − r"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("10Y Yield" = "black", "g − r" = "steelblue")) +
  labs(
    title = "French 10-Year Bond Yield and Growth–Interest Rate Differential",
    x = "Year",
    y = "Percent",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

###debt_df <- data.frame(
debt_df <- data.frame(
  time = index(z_ic),
  debt_gdp = z_ic$debt_gdp
)

ggplot(debt_df, aes(x = time, y = debt_gdp)) +
  geom_line(color = "darkred", size = 1) +
  labs(
    title = "France: Public Debt-to-GDP Ratio",
    x = "Year",
    y = "Percent of GDP"
  ) +
  theme_minimal()

###EXPANDED MODEL
# Packages
install.packages("dynlm")
install.packages("sandwich")
install.packages("lmtest")
library(lmtest)
library(sandwich)
library(modelsummary)
library(dynlm)
library(sandwich)
library(lmtest)

V_hac <- NeweyWest(m_ext, lag = 4, prewhite = FALSE, adjust = TRUE)

coeftest(m_ext, vcov. = V_hac)

m_ext <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:4) +
    deficit_gdp + L(deficit_gdp, 1:4) +
    g_minus_r + L(g_minus_r, 1:4) +
    hicp_yoy + epu + dlog_eurusd,
  data = z_ic
)

# Suppose your expanded model is:
m_ext <- dynlm(yield_10y ~ L(yield_10y,1) + debt_gdp + L(debt_gdp,1:4) +
                         deficit_gdp + L(deficit_gdp,1:4) +
                        g_minus_r + L(g_minus_r,1:4) +
                         hicp_yoy + epu + dlog_eurusd, data = z_ic)

# HAC (Newey–West) vcov
V_hac <- NeweyWest(m_ext, lag = 4, prewhite = FALSE, adjust = TRUE)

# Table-ready output (Coefficient, SE, t, p)
tab_sr <- coeftest(m_ext, vcov. = V_hac)

# Pretty table for slides/paper
modelsummary(
  list("Expanded ARDL (Short-run)" = m_ext),
  vcov = list(V_hac),
  statistic = "({std.error})",
  stars = TRUE,
  output = "markdown"
)

#####MASTER DROP
library(zoo)
library(dynlm)
library(lmtest)
library(sandwich)
library(dplyr)
library(modelsummary)

## 0) Make sure z_ic exists and is sorted
stopifnot(exists("z_ic"))
z_ic <- z_ic[order(index(z_ic)), ]

## 1) Set lag length for distributed lags
q_dl <- 4

## 2) Estimate the 5 models

# Model 1: AR(1) only (yield persistence)
m1 <- dynlm(
  yield_10y ~ L(yield_10y, 1),
  data = z_ic
)

# Model 2: AR(1) + Debt (current + 4 lags)
m2 <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl),
  data = z_ic
)

# Model 3: Model 2 + Deficit (current + 4 lags)
m3 <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl),
  data = z_ic
)

# Model 4: Model 3 + g - r (current + 4 lags)
m4 <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl) +
    g_minus_r + L(g_minus_r, 1:q_dl),
  data = z_ic
)

# Model 5: Model 4 + controls (inflation, EPU, FX)
m5 <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl) +
    g_minus_r + L(g_minus_r, 1:q_dl) +
    hicp_yoy + epu + dlog_eurusd,
  data = z_ic
)

models <- list(
  "Model 1" = m1,
  "Model 2" = m2,
  "Model 3" = m3,
  "Model 4" = m4,
  "Model 5" = m5
)

## 3) Collect fit statistics for your table

fit_stats <- lapply(models, function(m) {
  s <- summary(m)
  data.frame(
    Observations        = nobs(m),
    Adj_R2              = s$adj.r.squared,
    Durbin_Watson       = as.numeric(dwtest(m)$statistic),
    BG_LM_order4_stat   = as.numeric(bgtest(m, order = 4)$statistic),
    BG_LM_order4_pvalue = as.numeric(bgtest(m, order = 4)$p.value)
  )
})

fit_table <- bind_rows(fit_stats, .id = "Model")
fit_table

####NEWEY WEST
V5 <- NeweyWest(m5, lag = 4, prewhite = FALSE, adjust = TRUE)

modelsummary(
  list("Model 5" = m5),
  vcov = list(V5),
  statistic = "({std.error}) [{p.value}]",
  stars = c('*' = .1, '**' = .05, '***' = .01),
  gof_omit = "IC|Log|F|RMSE",
  title = "Baseline ARDL Estimates for French 10-Year Bond Yields"
)

####Full Table
library(dynlm)
library(zoo)
library(lmtest)
library(sandwich)
library(modelsummary)
library(dplyr)

##Step 2 Estimate the 5 Progressive Models
q_dl <- 4   # quarterly, one year

##Model 1-Persistence Only
m1 <- dynlm(
  yield_10y ~ L(yield_10y, 1),
  data = z_ic
)

##Model 2 Add Debt
m2 <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl),
  data = z_ic
)

##Model 3 Add Deficit
m3 <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl),
  data = z_ic
)

##Model 4 Add g - r
m4 <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl) +
    g_minus_r + L(g_minus_r, 1:q_dl),
  data = z_ic
)

##Model 5 Full Expanded Model
m5 <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl) +
    g_minus_r + L(g_minus_r, 1:q_dl) +
    hicp_yoy + epu + dlog_eurusd,
  data = z_ic
)

##Step 3 Extract Fit Stats
get_stats <- function(model) {
  data.frame(
    Observations = nobs(model),
    Adj_R2 = summary(model)$adj.r.squared,
    DW = dwtest(model)$statistic,
    BG_LM_4 = bgtest(model, order = 4)$statistic,
    BG_p_value = bgtest(model, order = 4)$p.value
  )
}
stats_table <- rbind(
  Model1 = get_stats(m1),
  Model2 = get_stats(m2),
  Model3 = get_stats(m3),
  Model4 = get_stats(m4),
  Model5 = get_stats(m5)
)

stats_table

##Step 4- Build Journal Table (With HAC SEs)
V1 <- NeweyWest(m1, lag = 4, prewhite = FALSE)
V2 <- NeweyWest(m2, lag = 4, prewhite = FALSE)
V3 <- NeweyWest(m3, lag = 4, prewhite = FALSE)
V4 <- NeweyWest(m4, lag = 4, prewhite = FALSE)
V5 <- NeweyWest(m5, lag = 4, prewhite = FALSE)

##Generate Table
install.packages("pandoc")
library(pandoc)
modelsummary(
  list(
    "Model 1" = m1,
    "Model 2" = m2,
    "Model 3" = m3,
    "Model 4" = m4,
    "Model 5" = m5
  ),
  vcov = list(V1, V2, V3, V4, V5),
  statistic = "({std.error})",
  stars = TRUE,
  gof_omit = "IC|Log|F|RMSE",
  output = "progressive_models.docx"
)

library(dynlm)
library(lmtest)
library(sandwich)

q_dl <- 4

m5_clean <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl) +
    g_minus_r + L(g_minus_r, 1:q_dl) +
    hicp_yoy + epu + dlog_eurusd,
  data = z_ic
)

summary(m5_clean)



####Long Run
##Step 1
q_dl <- 4

# Model 1: AR(1) only
m1 <- dynlm(
  yield_10y ~ L(yield_10y, 1),
  data = z_ic
)

# Model 2: + Debt
m2 <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl),
  data = z_ic
)

# Model 3: + Deficit
m3 <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl),
  data = z_ic
)

# Model 4: + g − r
m4 <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:q_dl) +
    deficit_gdp + L(deficit_gdp, 1:q_dl) +
    g_minus_r + L(g_minus_r, 1:q_dl),
  data = z_ic
)

lr_effect <- function(model, varname) {
  
  b <- coef(model)
  rho <- b["L(yield_10y, 1)"]
  
  idx <- grep(varname, names(b))
  sum_b <- sum(b[idx])
  
  sum_b / (1 - rho)
}

lr_table_prog <- data.frame(
  Variable = c("Debt/GDP", "Deficit/GDP", "g − r"),
  
  Model_2 = c(
    lr_effect(m2, "debt_gdp"),
    NA,
    NA
  ),
  
  Model_3 = c(
    lr_effect(m3, "debt_gdp"),
    lr_effect(m3, "deficit_gdp"),
    NA
  ),
  
  Model_4 = c(
    lr_effect(m4, "debt_gdp"),
    lr_effect(m4, "deficit_gdp"),
    lr_effect(m4, "g_minus_r")
  )
)

lr_table_prog

###Debt-to-GDP and Deficit-to-GDP graph
library(ggplot2)
library(dplyr)
library(zoo)

fr_plot <- france %>% mutate(q = as.yearqtr(q))

ggplot(fr_plot, aes(x = q)) +
  
  # Yield: solid
  geom_line(aes(y = yield_10y, linetype = "10-Year Yield"), size = 1.1) +
  
  # Debt-to-GDP: dotted
  geom_line(aes(y = debt_gdp, linetype = "Debt-to-GDP"), size = 1.1) +
  
  # Deficit-to-GDP (flipped internally): dashed
  geom_line(aes(y = -deficit_gdp, linetype = "Deficit-to-GDP"), size = 1.1) +
  
  scale_linetype_manual(values = c(
    "10-Year Yield" = "solid",
    "Debt-to-GDP" = "dotted",
    "Deficit-to-GDP" = "dashed"
  )) +
  
  labs(
    title = "France: 10-Year Yield, Debt-to-GDP, and Deficit-to-GDP",
    x = "Quarter",
    y = "Percent",
    linetype = ""
  ) +
  
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

###g - r graph with yield
library(ggplot2)
library(dplyr)
library(zoo)

# Ensure quarterly index is yearqtr
fr_plot <- france %>% mutate(q = as.yearqtr(q))

ggplot(fr_plot, aes(x = q)) +
  
  # 10-year yield
  geom_line(aes(y = yield_10y, color = "10-Year Bond Yield"), size = 1.2) +
  
  # g - r differential
  geom_line(aes(y = g_minus_r, color = "g − r Differential"), size = 1.2) +
  
  scale_color_manual(values = c(
    "10-Year Bond Yield" = "darkgreen",
    "g − r Differential" = "purple"
  )) +
  
  labs(
    title = "France: 10-Year Bond Yield and Growth–Interest Rate Differential (g − r)",
    x = "Quarter",
    y = "Percent",
    color = ""
  ) +
  
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

###One-Pass Kalman TVP Regression with Kalman Filter
install.packages("dlm")
library(dlm)
library(dplyr)
library(dplyr)

# 1. Prepare data ---------------------------------------------------------

tvp_df <- france %>%
  select(yield_10y,
         debt_gdp, deficit_gdp, g_minus_r,
         hicp_yoy, gdp_yoy, epu, dlog_eurusd) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

y <- tvp_df$yield_10y

X <- as.matrix(cbind(
  1,
  tvp_df$debt_gdp,
  tvp_df$deficit_gdp,
  tvp_df$g_minus_r,
  tvp_df$hicp_yoy,
  tvp_df$gdp_yoy,
  tvp_df$epu,
  tvp_df$dlog_eurusd
))

colnames(X) <- c("const","debt","deficit","gmr","hicp","gdp","epu","fx")

T <- nrow(X)
k <- ncol(X)

# 2. Set Kalman filter parameters ----------------------------------------

# State equation: beta_t = beta_{t-1} + eta_t
Q <- diag(0.001, k)     # small random walk variance

# Observation equation: y_t = X_t * beta_t + eps_t
R <- 0.1                # observation noise variance

# Priors
beta_0 <- rep(0, k)
P_0 <- diag(1000, k)

# Storage
beta_filt <- matrix(0, T, k)
beta_smooth <- matrix(0, T, k)

# 3. Kalman filter --------------------------------------------------------

beta_pred <- beta_0
P_pred <- P_0

for (t in 1:T) {
  # Prediction already in beta_pred, P_pred
  
  # Update
  y_pred <- as.numeric(X[t, ] %*% beta_pred)
  e_t <- y[t] - y_pred
  S_t <- as.numeric(X[t, ] %*% P_pred %*% X[t, ] + R)
  K_t <- P_pred %*% X[t, ] / S_t
  
  beta_upd <- beta_pred + K_t * e_t
  P_upd <- (diag(k) - K_t %*% t(X[t, ])) %*% P_pred
  
  beta_filt[t, ] <- beta_upd
  
  # Predict next
  beta_pred <- beta_upd
  P_pred <- P_upd + Q
}

# 4. Kalman smoother ------------------------------------------------------

beta_smooth[T, ] <- beta_filt[T, ]
P_smooth <- P_pred

for (t in (T-1):1) {
  P_filt <- P_upd
  C_t <- P_filt %*% solve(P_filt + Q)
  beta_smooth[t, ] <- beta_filt[t, ] + C_t %*% (beta_smooth[t+1, ] - beta_filt[t, ])
}

colnames(beta_smooth) <- colnames(X)

tvp_betas <- cbind.data.frame(beta_smooth)
head(tvp_betas)

##Kalman Filter Plots
library(ggplot2)

plot_beta <- function(var) {
  ggplot(tvp_betas, aes(x = 1:nrow(tvp_betas), y = .data[[var]])) +
    geom_line() +
    labs(title = paste("TVP coefficient for", var),
         x = "Time", y = paste("beta_", var))
}

plot_beta("debt")
plot_beta("deficit")
plot_beta("gmr")

tvp_betas <- tvp_betas %>%
  rename(
    DGR = debt,
    DFG = deficit,
    IGD = gmr
  )

plot_beta("DGR")
plot_beta("DFG")
plot_beta("IGD")

tvp_betas <- tvp_betas %>%
  rename(
    Inflation = hicp,
    ExchangeRate = fx,
    
  )
tvp_betas <- tvp_betas %>%
  rename(
    EPU = epu
  ) 

plot_beta("EPU")
plot_beta("Inflation")
plot_beta("ExchangeRate")

ls()
library(dynlm)
m_ardl <- dynlm(
  yield_10y ~ L(yield_10y, 1) +
    debt_gdp + L(debt_gdp, 1:4) +
    deficit_gdp + L(deficit_gdp, 1:4) +
    g_minus_r + L(g_minus_r, 1:4) +
    hicp_yoy + gdp_yoy + epu + dlog_eurusd,
  data = z_ic
)

ls()
objects()
class(m_ardl)
class(final_model)
class(ref_models)
summary(m_ardl)$na.action
model.frame(m_ardl) %>% head()
###Convert ARDL MODEL INTO STANDARD LM OBJECT
mf <- as.data.frame(model.frame(m_ardl))
m_ardl_lm <- lm(yield_10y ~ ., data = mf)
###White Test Heteroskedasticity 
white_test <- bptest(m_ardl_lm, ~ fitted(m_ardl_lm) + I(fitted(m_ardl_lm)^2))
white_test

###EXTRAT NUMERIC MODEL MATRIX AND RUN WHITE TEST MANUALLY
###Step 1 — Extract the model frame as a data frame
mf <- as.data.frame(model.frame(m_ardl))
###Step 2 — Extract the dependent variable and fitted values
y  <- mf$yield_10y
yhat <- fitted(m_ardl)
u2 <- residuals(m_ardl)^2
###Step 3 — Build the White auxiliary regression
aux <- lm(u2 ~ yhat + I(yhat^2))
###Step 4 — Compute the White statistic manually
n <- nrow(mf)
R2 <- summary(aux)$r.squared
white_stat <- n * R2
p_value <- 1 - pchisq(white_stat, df = 2)

white_stat
p_value

###Rolling-Window ARDL/Rolling OLS
head(france)

library(dplyr)
library(zoo)
library(ggplot2)

# 1. Build clean dataset with NO lagged variables
roll_df <- france %>%
  select(q, yield_10y, debt_gdp, deficit_gdp, g_minus_r,
         hicp_yoy, gdp_yoy, epu, dlog_eurusd) %>%
  na.omit() %>%              # remove rows with NA
  arrange(q)                 # ensure sorted

# 2. Rolling window size
w <- 20
T <- nrow(roll_df)

# 3. Storage for rolling coefficients
roll_betas <- data.frame(
  q = roll_df$q[w:T],
  DGR = NA,
  DFG = NA,
  IGD = NA
)

# 4. Rolling regression loop
for (i in w:T) {
  
  sub <- roll_df[(i - w + 1):i, ]
  
  # skip if any NA (safety)
  if (anyNA(sub)) next
  
  m <- lm(yield_10y ~ debt_gdp + deficit_gdp + g_minus_r +
            hicp_yoy + gdp_yoy + epu + dlog_eurusd,
          data = sub)
  
  b <- coef(m)
  
  roll_betas$DGR[i - w + 1] <- b["debt_gdp"]
  roll_betas$DFG[i - w + 1] <- b["deficit_gdp"]
  roll_betas$IGD[i - w + 1] <- b["g_minus_r"]
}

# 5. Plot rolling coefficients
ggplot(roll_betas, aes(x = q, y = DGR)) +
  geom_line() +
  labs(title = "Rolling 20-Quarter Coefficient on Debt/GDP (DGR)",
       x = "Quarter", y = "Coefficient")

ggplot(roll_betas, aes(x = q, y = DFG)) +
  geom_line() +
  labs(title = "Rolling 20-Quarter Coefficient on Deficit/GDP (DFG)",
       x = "Quarter", y = "Coefficient")

ggplot(roll_betas, aes(x = q, y = IGD)) +
  geom_line() +
  labs(title = "Rolling 20-Quarter Coefficient on g − r (IGD)",
       x = "Quarter", y = "Coefficient")

##Extract Summary Statistics for Each rolling Coefficient
summary_stats <- roll_betas %>%
  summarise(
    mean_DGR = mean(DGR, na.rm = TRUE),
    min_DGR  = min(DGR, na.rm = TRUE),
    max_DGR  = max(DGR, na.rm = TRUE),
    
    mean_DFG = mean(DFG, na.rm = TRUE),
    min_DFG  = min(DFG, na.rm = TRUE),
    max_DFG  = max(DFG, na.rm = TRUE),
    
    mean_IGD = mean(IGD, na.rm = TRUE),
    min_IGD  = min(IGD, na.rm = TRUE),
    max_IGD  = max(IGD, na.rm = TRUE)
  )

summary_stats

###Bai-Perron Structural Break Test
library(strucchange)

# 1. Clean dataset
bp_df <- france %>%
  select(q, yield_10y, debt_gdp, deficit_gdp, g_minus_r) %>%
  na.omit() %>%
  arrange(q)

# 2. Bai–Perron breakpoints on the regression formula
bp <- breakpoints(
  yield_10y ~ debt_gdp + deficit_gdp + g_minus_r,
  data = bp_df,
  h = 0.15
)

# 3. Summary of breakpoints
summary(bp)

# 4. Extract break dates
break_dates <- bp_df$q[bp$breakpoints]
break_dates

# 5. Plot
plot(bp)
lines(fitted(bp), col = "blue")

###Jordà‑style Local Projections
###Step 1-3: Build shocks and run local projections
library(dplyr)
library(zoo)
library(ggplot2)

# 1. Start from clean data
lp_df <- lp_df %>%
  arrange(q) %>%
  mutate(
    debt_l1 = lag(debt_gdp, 1),
    debt_l2 = lag(debt_gdp, 2),
    debt_l3 = lag(debt_gdp, 3),
    debt_l4 = lag(debt_gdp, 4),
    
    def_l1 = lag(deficit_gdp, 1),
    def_l2 = lag(deficit_gdp, 2),
    def_l3 = lag(deficit_gdp, 3),
    def_l4 = lag(deficit_gdp, 4),
    
    gr_l1 = lag(g_minus_r, 1),
    gr_l2 = lag(g_minus_r, 2),
    gr_l3 = lag(g_minus_r, 3),
    gr_l4 = lag(g_minus_r, 4),
    
    y_l1 = lag(yield_10y, 1),
    y_l2 = lag(yield_10y, 2),
    y_l3 = lag(yield_10y, 3),
    y_l4 = lag(yield_10y, 4)
  ) %>%
  na.omit()

##Build fiscal shocks using new lag variables
mod_debt <- lm(debt_gdp ~ debt_l1 + debt_l2 + debt_l3 + debt_l4 +
                 gdp_yoy + hicp_yoy + epu,
               data = lp_df)

lp_df$shock_debt <- resid(mod_debt)


mod_def <- lm(deficit_gdp ~ def_l1 + def_l2 + def_l3 + def_l4 +
                gdp_yoy + hicp_yoy + epu,
              data = lp_df)

lp_df$shock_def <- resid(mod_def)


mod_gr <- lm(g_minus_r ~ gr_l1 + gr_l2 + gr_l3 + gr_l4 +
               gdp_yoy + hicp_yoy + epu,
             data = lp_df)

lp_df$shock_gr <- resid(mod_gr)

###Step 3-Local Prjoections Loop (Horizons 0-8)
library(dplyr)
library(ggplot2)

H <- 8   # horizons 0–8
irf_list <- list()

for (h in 0:H) {
  
  # Lead of yield at horizon h
  lp_df <- lp_df %>%
    mutate(y_lead = dplyr::lead(yield_10y, n = h))
  
  # Drop NA rows created by the lead
  sub <- lp_df %>% filter(!is.na(y_lead))
  
  # (a) IRF to debt shock
  m_debt <- lm(
    y_lead ~ shock_debt +
      y_l1 + y_l2 + y_l3 + y_l4 +
      debt_l1 + debt_l2 + debt_l3 + debt_l4 +
      def_l1 + def_l2 + def_l3 + def_l4 +
      gr_l1 + gr_l2 + gr_l3 + gr_l4 +
      gdp_yoy + hicp_yoy + epu,
    data = sub
  )
  
  # (b) IRF to deficit shock
  m_def <- lm(
    y_lead ~ shock_def +
      y_l1 + y_l2 + y_l3 + y_l4 +
      debt_l1 + debt_l2 + debt_l3 + debt_l4 +
      def_l1 + def_l2 + def_l3 + def_l4 +
      gr_l1 + gr_l2 + gr_l3 + gr_l4 +
      gdp_yoy + hicp_yoy + epu,
    data = sub
  )
  
  # (c) IRF to g - r shock
  m_gr <- lm(
    y_lead ~ shock_gr +
      y_l1 + y_l2 + y_l3 + y_l4 +
      debt_l1 + debt_l2 + debt_l3 + debt_l4 +
      def_l1 + def_l2 + def_l3 + def_l4 +
      gr_l1 + gr_l2 + gr_l3 + gr_l4 +
      gdp_yoy + hicp_yoy + epu,
    data = sub
  )
  
  irf_list[[h + 1]] <- data.frame(
    h = h,
    beta_debt = coef(m_debt)["shock_debt"],
    se_debt   = sqrt(vcov(m_debt)["shock_debt", "shock_debt"]),
    beta_def  = coef(m_def)["shock_def"],
    se_def    = sqrt(vcov(m_def)["shock_def", "shock_def"]),
    beta_gr   = coef(m_gr)["shock_gr"],
    se_gr     = sqrt(vcov(m_gr)["shock_gr", "shock_gr"])
  )
}

# Combine into a single IRF dataframe
irf_df <- bind_rows(irf_list) %>%
  mutate(
    ci_debt_low = beta_debt - 1.96 * se_debt,
    ci_debt_hi  = beta_debt + 1.96 * se_debt,
    ci_def_low  = beta_def  - 1.96 * se_def,
    ci_def_hi   = beta_def  + 1.96 * se_def,
    ci_gr_low   = beta_gr   - 1.96 * se_gr,
    ci_gr_hi    = beta_gr   + 1.96 * se_gr
  )

###Step 4-Plot the IRFS
# Debt shock IRF
ggplot(irf_df, aes(x = h, y = beta_debt)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_debt_low, ymax = ci_debt_hi), alpha = 0.2) +
  labs(title = "IRF of Yield to Debt Shock",
       x = "Horizon (quarters)", y = "Response of yield_10y")

# Deficit shock IRF
ggplot(irf_df, aes(x = h, y = beta_def)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_def_low, ymax = ci_def_hi), alpha = 0.2) +
  labs(title = "IRF of Yield to Deficit Shock",
       x = "Horizon (quarters)", y = "Response of yield_10y")

# g - r shock IRF
ggplot(irf_df, aes(x = h, y = beta_gr)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_gr_low, ymax = ci_gr_hi), alpha = 0.2) +
  labs(title = "IRF of Yield to g − r Shock",
       x = "Horizon (quarters)", y = "Response of yield_10y")

irf_df
