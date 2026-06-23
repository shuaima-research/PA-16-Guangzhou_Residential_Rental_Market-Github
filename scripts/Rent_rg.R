library(olsrr)
library(car)
library(psych)
library(factoextra)


setwd("/Users/shuai/Documents/Projects/PA-16-Guangzhou_Residential_Rental_Market")

## 输出路径 / Output paths
figures_dir <- file.path("outputs", "figures")
tables_dir <- file.path("outputs", "tables")
models_dir <- file.path("outputs", "models")
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

## 数据导入 / Data import
rent_gz <- read.csv(file.path("data", "processed", "Rent data_GZ.csv"))

## 变量说明 / Variable descriptions
# rent_gz$Type：整租或合租 / Entire-unit or shared rental
# rent_gz$District：行政区 / District
# rent_gz$Bai/Cong/Pan/Hai/Hua/Huang/Li/Nan/Tian/Yue/Zeng：行政区虚拟变量 / District dummy variables
# 白云、从化、番禺、海珠、花都、黄埔、荔湾、南沙、天河、越秀、增城 / Baiyun, Conghua, Panyu, Haizhu, Huadu, Huangpu, Liwan, Nansha, Tianhe, Yuexiu, and Zengcheng
# rent_gz$Area：面积 / Floor area
# rent_gz$B/L/R：户型中的室、厅、卫 / Bedrooms, living rooms, and bathrooms
# rent_gz$Floor：所在楼层 / Floor number
# rent_gz$HF：楼层所属类别（高、中、低） / Floor category (high, middle, or low)
# rent_gz$HH/HM/HL：高、中、低楼层虚拟变量 / High-, middle-, and low-floor dummy variables
# rent_gz$Brand：品牌 / Brand
# rent_gz$Rent：租金 / Rent

## 线性拟合 1：地区使用定性信息 / Linear model 1: district as a categorical variable
rent_rg_1 <- lm(Rent ~ Type + District + Area + B + L + R + Floor + HF + Brand, data = rent_gz)
sum_rg_1 <- summary(rent_rg_1)
sum_rg_1

# 检验残差是否服从正态分布 / Test whether residuals follow a normal distribution
error_1 <- rent_rg_1$residuals
ks_1 <- ks.test(error_1, "pnorm", mean = mean(error_1), sd = sqrt(var(error_1)))
shapiro_1 <- shapiro.test(error_1)
mean_error_1 <- mean(error_1)

## 线性拟合 2：地区使用虚拟变量 / Linear model 2: district as dummy variables
rent_rg_2 <- lm(
  Rent ~ Type + Bai + Cong + Pan + Hai + Hua + Huang + Li + Nan + Tian + Yue + Zeng +
    Area + B + L + R + Floor + HH + HM + HL + Brand,
  data = rent_gz
)
sum_rg_2 <- summary(rent_rg_2)
sum_rg_2

# 检验残差是否服从正态分布 / Test whether residuals follow a normal distribution
error_2 <- rent_rg_2$residuals
png(file.path(figures_dir, "rent_rg_2_residual_histogram.png"), width = 1800, height = 1200, res = 200)
hist(error_2, main = "Residuals of linear model 2", xlab = "Residual")
dev.off()
ks_2 <- ks.test(error_2, "pnorm", mean = mean(error_2), sd = sqrt(var(error_2)))
shapiro_2 <- shapiro.test(error_2)
mean_error_2 <- mean(error_2)

## 使用三种方法筛选关键变量 / Select key variables using three methods
# 后向逐步回归 / Backward stepwise regression
trial_1 <- ols_step_backward_p(rent_rg_1, pent = 0.05, prem = 0.05)
trial_1

# 第二个回归模型含有无法估计的系数，因此去掉对应变量 / Model 2 contains non-estimable coefficients, so the corresponding variables are removed
rent_rg_2new <- lm(
  Rent ~ Type + Bai + Cong + Pan + Hai + Hua + Huang + Li + Nan + Tian + Yue +
    Area + B + L + R + Floor + HH + HM + Brand,
  data = rent_gz
)
trial_2 <- ols_step_backward_p(rent_rg_2new, pent = 0.05, prem = 0.05)
trial_2

# 前向逐步回归 / Forward stepwise regression
# 模型 1 / Model 1
trial_3 <- ols_step_forward_p(rent_rg_1, pent = 0.05, prem = 0.05)
trial_3
# 模型 2 / Model 2
trial_4 <- ols_step_forward_p(rent_rg_2new, pent = 0.05, prem = 0.05)
trial_4

# 双向逐步回归 / Bidirectional stepwise regression
# 模型 1 / Model 1
trial_5 <- ols_step_both_p(rent_rg_1, pent = 0.05, prem = 0.05)
trial_5
# 模型 2 / Model 2
trial_6 <- ols_step_both_p(rent_rg_2new, pent = 0.05, prem = 0.05)
trial_6

## 改变模型中的变量顺序，检验结果是否受顺序影响 / Change variable order to test whether results depend on ordering
rent_rg_2ord <- lm(
  Rent ~ Type + Bai + Cong + Pan + Hai + Huang + Li + Tian + Yue + Hua +
    Nan + Area + B + L + R + Floor + HH + HM + Brand,
  data = rent_gz
)
trial_7 <- ols_step_forward_p(rent_rg_2ord, pent = 0.05, prem = 0.05)
trial_7

## 检验多重共线性 / Test for multicollinearity
## 计算方差膨胀因子 / Calculate variance inflation factors
# 模型 1 / Model 1
vif_1 <- vif(rent_rg_1)
# 模型 2 / Model 2
vif_2 <- vif(rent_rg_2new)

## Kappa 检验 / Kappa tests
# 模型 1 / Model 1
kappa_1 <- kappa(rent_rg_1)
# 模型 2 / Model 2
kappa_2 <- kappa(rent_rg_2new)
# 剔除 Type 和 Brand / Remove Type and Brand
rent_rg_1test <- lm(Rent ~ Area + R + B + L, data = rent_gz)
vif_1test <- vif(rent_rg_1test)

## 变量相关系数 / Variable correlations
rent_gz_num <- rent_gz[, -2]
rent_gz_num <- rent_gz_num[, -18]
rent_gz_num <- rent_gz_num[, -22] # 去掉定性变量和因变量 / Remove categorical variables and the dependent variable
f_cor <- cor(rent_gz_num)
write.csv(f_cor, file.path(tables_dir, "rent_gz_correlation_matrix.csv"), row.names = TRUE)
rent_gz_num <- rent_gz_num[, -12]
rent_gz_num <- rent_gz_num[, -19] # 去掉前期线性回归中无法估计的变量 / Remove variables that could not be estimated in the earlier regressions

## 排除共线性变量：逐步回归 / Exclude collinear variables using stepwise regression
step_rg_1 <- step(rent_rg_1, trace = 0)
step_rg_2 <- step(rent_rg_2new, trace = 0)

## 处理共线性变量：主成分分析 / Address collinearity using principal component analysis
set.seed(20260730) # 设置随机种子以保证结果可复现 / Set a random seed for reproducibility
png(file.path(figures_dir, "pca_parallel_analysis.png"), width = 1800, height = 1200, res = 200)
fa_parallel_result <- fa.parallel(rent_gz_num, fa = "pc", n.iter = 100)
dev.off()
pc <- principal(rent_gz_num, nfactors = 12) # 提取主成分 / Extract principal components
pc_weights <- round(unclass(pc$weights), 2) # 获取主成分得分权重 / Obtain principal-component score weights
write.csv(pc_weights, file.path(tables_dir, "pca_component_weights.csv"), row.names = TRUE)

## 聚类分析 / Cluster analysis
# K-means 聚类 / K-means clustering
df <- scale(rent_gz_num) # 数据标准化 / Standardize the data
wss_plot <- fviz_nbclust(df, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)
ggplot2::ggsave(
  filename = file.path(figures_dir, "kmeans_wss.png"),
  plot = wss_plot,
  width = 8,
  height = 6,
  dpi = 300
)

set.seed(20260730) # 设置随机种子以保证聚类结果可复现 / Set a random seed for reproducible clusters
km_result <- kmeans(df, 4, nstart = 10) # 进行聚类 / Run the clustering

rent_gz_cl <- cbind(rent_gz_num, cluster = km_result$cluster) # 提取聚类结果并与原始数据对应保存 / Append cluster assignments to the original observations
write.csv(
  rent_gz_cl,
  file = file.path(tables_dir, "rent_gz_kmeans_clusters.csv"),
  row.names = FALSE
)

# 层次聚类 / Hierarchical clustering
rent_gz_num_2 <- cbind(rent_gz_num, Rent = rent_gz$Rent) # 加入租金信息进行聚类 / Add rent to the clustering variables
result <- dist(rent_gz_num_2, method = "euclidean") # 计算样本间的欧氏距离 / Calculate Euclidean distances between observations

result_hc_1 <- hclust(d = result, method = "ward.D2") # 使用 Ward 法进行层次聚类 / Run hierarchical clustering using Ward's method
result_hc_2 <- hclust(d = result, method = "complete") # 使用完全连接法进行层次聚类 / Run hierarchical clustering using complete linkage
result_hc_3 <- hclust(d = result, method = "average") # 使用平均连接法进行层次聚类 / Run hierarchical clustering using average linkage

# 可视化：Ward 树使用宽幅画布、细线和平方根高度轴，以展示密集的底层分支 / Visualization: use a wide canvas, thin lines, and a square-root height axis to reveal dense lower branches in the Ward tree
dend_ward <- fviz_dend(
  result_hc_1,
  k = 5,
  color_labels_by_k = FALSE,
  k_colors = c("#0072B2", "#E69F00", "#009E73", "#CC79A7", "#D55E00"),
  show_labels = FALSE,
  type = "rectangle",
  lwd = 0.12
) +
  ggplot2::scale_y_continuous(
    trans = "sqrt",
    breaks = c(0, 10000, 25000, 50000, 100000, 150000, 200000),
    labels = scales::label_number(big.mark = ",")
  ) +
  ggplot2::labs(
    title = "Ward Hierarchical Clustering of Guangzhou Rental Listings",
    subtitle = "Five-cluster solution; 3,000 leaf labels omitted for readability",
    x = "Rental listings",
    y = "Merge height (square-root scale)",
    caption = "Branch colors indicate membership at the five-cluster cut."
  ) +
  ggplot2::guides(color = "none", linewidth = "none") +
  ggplot2::theme_minimal(base_size = 13) +
  ggplot2::theme(
    legend.position = "none",
    panel.grid.major.x = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.caption = ggplot2::element_text(color = "grey35")
  )
dend_complete <- fviz_dend(result_hc_2, k = 5, color_labels_by_k = TRUE, k_colors = rainbow(5), show_labels = FALSE, type = "rectangle", cex = 0.6)
dend_average <- fviz_dend(result_hc_3, k = 5, color_labels_by_k = TRUE, k_colors = rainbow(5), show_labels = FALSE, type = "rectangle", cex = 0.6)

ggplot2::ggsave(
  file.path(figures_dir, "hierarchical_ward.png"),
  dend_ward,
  width = 14,
  height = 7.5,
  dpi = 400,
  bg = "white"
)
ggplot2::ggsave(file.path(figures_dir, "hierarchical_complete.png"), dend_complete, width = 8, height = 6, dpi = 300)
ggplot2::ggsave(file.path(figures_dir, "hierarchical_average.png"), dend_average, width = 8, height = 6, dpi = 300)

rent_gz_hc <- cutree(result_hc_1, k = 5) # 提取五类聚类结果 / Extract the five-cluster solution
write.csv(
  data.frame(cluster = rent_gz_hc),
  file = file.path(tables_dir, "rent_gz_hierarchical_clusters.csv"),
  row.names = FALSE
)

## 保存模型与诊断结果 / Save models and diagnostic results
saveRDS(
  list(
    rent_rg_1 = rent_rg_1,
    rent_rg_2 = rent_rg_2,
    rent_rg_2new = rent_rg_2new,
    rent_rg_2ord = rent_rg_2ord,
    rent_rg_1test = rent_rg_1test,
    step_rg_1 = step_rg_1,
    step_rg_2 = step_rg_2
  ),
  file.path(models_dir, "rent_regression_models.rds")
)

saveRDS(
  list(
    backward_model_1 = trial_1,
    backward_model_2 = trial_2,
    forward_model_1 = trial_3,
    forward_model_2 = trial_4,
    both_model_1 = trial_5,
    both_model_2 = trial_6,
    reordered_model_2 = trial_7
  ),
  file.path(models_dir, "variable_selection_results.rds")
)

saveRDS(
  list(kmeans = km_result, ward = result_hc_1, complete = result_hc_2, average = result_hc_3),
  file.path(models_dir, "clustering_models.rds")
)

capture.output(
  list(
    model_1_summary = sum_rg_1,
    model_2_summary = sum_rg_2,
    model_1_ks_test = ks_1,
    model_1_shapiro_test = shapiro_1,
    model_1_mean_residual = mean_error_1,
    model_2_ks_test = ks_2,
    model_2_shapiro_test = shapiro_2,
    model_2_mean_residual = mean_error_2,
    model_1_vif = vif_1,
    model_2_vif = vif_2,
    reduced_model_vif = vif_1test,
    model_1_kappa = kappa_1,
    model_2_kappa = kappa_2
  ),
  file = file.path(tables_dir, "regression_diagnostics.txt")
)
