## 为实证报告准备可复现的图表 / Prepare reproducible figures and tables for the empirical report

setwd("/Users/shuai/Documents/Projects/PA-16-Guangzhou_Residential_Rental_Market")

figures_dir <- file.path("outputs", "figures")
tables_dir <- file.path("outputs", "tables")
models_dir <- file.path("outputs", "models")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

rent <- read.csv(file.path("data", "processed", "Rent data_GZ.csv"))

## 样本描述 / Sample description
summary_vars <- c("Rent", "Area", "B", "L", "R", "Floor")
descriptive_statistics <- do.call(
  rbind,
  lapply(summary_vars, function(variable) {
    values <- rent[[variable]]
    data.frame(
      Variable = variable,
      N = sum(!is.na(values)),
      Mean = mean(values, na.rm = TRUE),
      SD = sd(values, na.rm = TRUE),
      Minimum = min(values, na.rm = TRUE),
      P25 = unname(quantile(values, 0.25, na.rm = TRUE)),
      Median = median(values, na.rm = TRUE),
      P75 = unname(quantile(values, 0.75, na.rm = TRUE)),
      Maximum = max(values, na.rm = TRUE)
    )
  })
)
write.csv(
  descriptive_statistics,
  file.path(tables_dir, "sample_descriptive_statistics.csv"),
  row.names = FALSE
)

district_summary <- do.call(
  rbind,
  lapply(split(rent$Rent, rent$District), function(values) {
    data.frame(
      N = length(values),
      Mean_rent = mean(values),
      Median_rent = median(values)
    )
  })
)
district_summary$District <- row.names(district_summary)
district_summary <- district_summary[, c("District", "N", "Mean_rent", "Median_rent")]
district_summary <- district_summary[order(-district_summary$N), ]
row.names(district_summary) <- NULL
write.csv(district_summary, file.path(tables_dir, "district_summary.csv"), row.names = FALSE)

## 回归设定 / Regression specifications
rent_preferred <- subset(rent, District != "从化")
rent_preferred$District <- relevel(factor(rent_preferred$District), ref = "白云")

benchmark_model <- lm(
  Rent ~ Type + District + Area + B + L + R + Floor + HF + Brand,
  data = rent
)
preferred_levels_model <- lm(
  Rent ~ Type + District + Area + B + L + R + Floor + HF,
  data = rent_preferred
)
preferred_log_model <- lm(
  log(Rent) ~ Type + District + log(Area) + B + L + R + Floor + HF,
  data = rent_preferred
)

## 计算 HC1 异方差稳健标准误 / Calculate HC1 heteroskedasticity-robust standard errors
hc1_results <- function(model) {
  x <- model.matrix(model)
  residual <- residuals(model)
  n <- nrow(x)
  k <- ncol(x)
  bread <- solve(crossprod(x))
  vcov_hc1 <- (n / (n - k)) * bread %*%
    crossprod(x, x * as.numeric(residual^2)) %*% bread
  standard_error <- sqrt(diag(vcov_hc1))
  estimate <- coef(model)
  statistic <- estimate / standard_error
  p_value <- 2 * pt(abs(statistic), df = n - k, lower.tail = FALSE)
  data.frame(
    Term = names(estimate),
    Estimate = unname(estimate),
    Robust_SE = unname(standard_error),
    Statistic = unname(statistic),
    P_value = unname(p_value),
    CI_low = unname(estimate - qt(0.975, n - k) * standard_error),
    CI_high = unname(estimate + qt(0.975, n - k) * standard_error)
  )
}

log_model_coefficients <- hc1_results(preferred_log_model)
log_model_coefficients$Percent_effect <- 100 * (exp(log_model_coefficients$Estimate) - 1)
log_model_coefficients$Percent_CI_low <- 100 * (exp(log_model_coefficients$CI_low) - 1)
log_model_coefficients$Percent_CI_high <- 100 * (exp(log_model_coefficients$CI_high) - 1)
write.csv(
  log_model_coefficients,
  file.path(tables_dir, "preferred_log_model_coefficients.csv"),
  row.names = FALSE
)

model_fit <- do.call(
  rbind,
  lapply(
    list(
      "Benchmark levels model" = benchmark_model,
      "Preferred levels model" = preferred_levels_model,
      "Preferred log model" = preferred_log_model
    ),
    function(model) {
      data.frame(
        N = nobs(model),
        R_squared = summary(model)$r.squared,
        Adjusted_R_squared = summary(model)$adj.r.squared,
        Residual_SE = sigma(model)
      )
    }
  )
)
model_fit$Model <- row.names(model_fit)
model_fit <- model_fit[, c("Model", "N", "R_squared", "Adjusted_R_squared", "Residual_SE")]
row.names(model_fit) <- NULL
write.csv(model_fit, file.path(tables_dir, "model_fit_comparison.csv"), row.names = FALSE)

## 联合显著性与数据诊断 / Joint-significance and data diagnostics
district_test <- anova(update(preferred_levels_model, . ~ . - District), preferred_levels_model)
floor_category_test <- anova(update(preferred_levels_model, . ~ . - HF), preferred_levels_model)

simple_bp_test <- function(model) {
  auxiliary_model <- lm(residuals(model)^2 ~ fitted(model))
  statistic <- nobs(model) * summary(auxiliary_model)$r.squared
  c(statistic = statistic, p_value = pchisq(statistic, df = 1, lower.tail = FALSE))
}

diagnostic_summary <- data.frame(
  Diagnostic = c(
    "Correlation between Type and Brand",
    "District fixed effects: partial F-statistic",
    "District fixed effects: p-value",
    "Floor-category indicators: partial F-statistic",
    "Floor-category indicators: p-value",
    "Levels model: Breusch-Pagan statistic",
    "Levels model: Breusch-Pagan p-value",
    "Log model: Breusch-Pagan statistic",
    "Log model: Breusch-Pagan p-value"
  ),
  Value = c(
    cor(rent$Type, rent$Brand),
    district_test$F[2],
    district_test$`Pr(>F)`[2],
    floor_category_test$F[2],
    floor_category_test$`Pr(>F)`[2],
    simple_bp_test(preferred_levels_model)["statistic"],
    simple_bp_test(preferred_levels_model)["p_value"],
    simple_bp_test(preferred_log_model)["statistic"],
    simple_bp_test(preferred_log_model)["p_value"]
  )
)
write.csv(diagnostic_summary, file.path(tables_dir, "report_diagnostics.csv"), row.names = FALSE)

## 调整后的区域租金差异 / Adjusted district rent differentials
district_effects <- subset(log_model_coefficients, grepl("^District", Term))
district_effects$District <- sub("^District", "", district_effects$Term)
district_effects <- district_effects[
  order(district_effects$Percent_effect),
  c("District", "Percent_effect", "Percent_CI_low", "Percent_CI_high", "P_value")
]
write.csv(
  district_effects,
  file.path(tables_dir, "adjusted_district_rent_differentials.csv"),
  row.names = FALSE
)

district_plot <- ggplot2::ggplot(
  district_effects,
  ggplot2::aes(x = stats::reorder(District, Percent_effect), y = Percent_effect)
) +
  ggplot2::geom_hline(yintercept = 0, color = "grey55", linewidth = 0.5) +
  ggplot2::geom_errorbar(
    ggplot2::aes(ymin = Percent_CI_low, ymax = Percent_CI_high),
    width = 0.18,
    color = "#355C7D"
  ) +
  ggplot2::geom_point(size = 2.8, color = "#C44E52") +
  ggplot2::coord_flip() +
  ggplot2::labs(
    x = NULL,
    y = "Conditional rent difference relative to Baiyun (%)",
    title = "Adjusted district rent differentials",
    subtitle = "Preferred log-rent model; 95% HC1 confidence intervals"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  file.path(figures_dir, "adjusted_district_rent_differentials.png"),
  district_plot,
  width = 8,
  height = 5.5,
  dpi = 300
)

## 原始租金与对数租金分布 / Raw-rent and log-rent distributions
png(file.path(figures_dir, "rent_and_log_rent_distributions.png"), width = 2200, height = 1000, res = 200)
par(mfrow = c(1, 2), mar = c(4.2, 4.2, 2.5, 1))
hist(rent$Rent, breaks = 50, col = "#9ECAE1", border = "white", main = "Monthly asking rent", xlab = "Rent")
hist(log(rent$Rent), breaks = 35, col = "#F4A582", border = "white", main = "Log monthly asking rent", xlab = "log(Rent)")
dev.off()

## 聚类画像，仅作为补充探索 / Cluster profiles for supplementary exploration only
kmeans_assignments <- read.csv(file.path(tables_dir, "rent_gz_kmeans_clusters.csv"))$cluster
rent$cluster <- kmeans_assignments
cluster_profiles <- do.call(
  rbind,
  lapply(split(rent, rent$cluster), function(group) {
    data.frame(
      N = nrow(group),
      Mean_rent = mean(group$Rent),
      Median_rent = median(group$Rent),
      Mean_area = mean(group$Area),
      Mean_bedrooms = mean(group$B),
      Mean_living_rooms = mean(group$L),
      Mean_bathrooms = mean(group$R),
      Mean_floor = mean(group$Floor),
      Share_Type_1 = mean(group$Type)
    )
  })
)
cluster_profiles$Cluster <- row.names(cluster_profiles)
cluster_profiles <- cluster_profiles[, c("Cluster", setdiff(names(cluster_profiles), "Cluster"))]
row.names(cluster_profiles) <- NULL
write.csv(cluster_profiles, file.path(tables_dir, "kmeans_cluster_profiles.csv"), row.names = FALSE)

saveRDS(
  list(
    benchmark_model = benchmark_model,
    preferred_levels_model = preferred_levels_model,
    preferred_log_model = preferred_log_model
  ),
  file.path(models_dir, "report_regression_models.rds")
)
