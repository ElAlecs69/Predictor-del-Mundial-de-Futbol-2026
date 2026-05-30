# =========================================================
# EDA PROFESIONAL PARA DATASETS GRANDES (~1M FILAS)
# =========================================================

set.seed(123)

# -----------------------------
# 1. Librerías
# -----------------------------
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(janitor)
library(skimr)
library(naniar)
library(corrplot)
library(GGally)
library(DataExplorer)
library(stringr)
library(lubridate)
library(scales)
library(gridExtra)

# -----------------------------
# 2. Cargar dataset
# -----------------------------
df <- fread("Millon.csv")

df <- clean_names(df)

# muestra para gráficas
sample_df <- df %>% slice_sample(n = 50000)

# -----------------------------
# 3. Información básica
# -----------------------------
cat("DIMENSIONES\n")
print(dim(df))

cat("ESTRUCTURA\n")
str(df)

cat("TIPOS DE DATOS\n")
print(sapply(df,class))

cat("MEMORIA USADA\n")
print(format(object.size(df), units="MB"))

# -----------------------------
# 4. Estadísticas generales
# -----------------------------
summary(df)
skim(df)

# -----------------------------
# 5. Valores faltantes
# -----------------------------
missing <- colSums(is.na(df))
print(missing)

missing_percent <- colMeans(is.na(df))*100
print(missing_percent)

vis_miss(sample_df)

# -----------------------------
# 6. Variables numéricas
# -----------------------------
numeric_vars <- df %>% select(where(is.numeric))

# -----------------------------
# 7. Histogramas
# -----------------------------
sample_df %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols=everything()) %>%
  ggplot(aes(value)) +
  geom_histogram(bins=40,fill="steelblue") +
  facet_wrap(~name,scales="free") +
  theme_minimal()

# -----------------------------
# 8. Boxplots
# -----------------------------
sample_df %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols=everything()) %>%
  ggplot(aes(x=name,y=value)) +
  geom_boxplot(fill="orange") +
  coord_flip() +
  theme_minimal()

# -----------------------------
# 9. Densidad
# -----------------------------
sample_df %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols=everything()) %>%
  ggplot(aes(value)) +
  geom_density(fill="purple",alpha=0.5) +
  facet_wrap(~name,scales="free")

# -----------------------------
# 10. Correlación
# -----------------------------
if(ncol(numeric_vars)>1){
  corr_matrix <- cor(numeric_vars,use="complete.obs")
  
  corrplot(
    corr_matrix,
    method="color",
    type="upper"
  )
}

# -----------------------------
# 11. Pairplot
# -----------------------------
if(ncol(numeric_vars)>=3){
  
  sample_vars <- sample_df %>%
    select(where(is.numeric)) %>%
    select(1:min(5,ncol(.)))
  
  GGally::ggpairs(sample_vars)
  
}

# -----------------------------
# 12. Outliers (IQR)
# -----------------------------
detect_outliers <- function(x){
  
  Q1 <- quantile(x,0.25,na.rm=TRUE)
  Q3 <- quantile(x,0.75,na.rm=TRUE)
  
  IQR <- Q3-Q1
  
  sum(x < (Q1-1.5*IQR) | x > (Q3+1.5*IQR),na.rm=TRUE)
  
}

outliers <- sapply(numeric_vars,detect_outliers)
print(outliers)

# -----------------------------
# 13. Variables categóricas
# -----------------------------
cat_vars <- df %>% select(where(is.character))
print(names(cat_vars))

# frecuencias
for(col in names(cat_vars)){
  
  print(
    df %>%
      count(.data[[col]]) %>%
      arrange(desc(n))
  )
  
}

# -----------------------------
# 14. Gráficas categóricas
# -----------------------------
for(col in names(cat_vars)){
  
  p <- ggplot(sample_df,aes(x=.data[[col]]))+
    geom_bar(fill="darkgreen")+
    coord_flip()+
    theme_minimal()+
    labs(title=paste("Distribución:",col))
  
  print(p)
  
}

# -----------------------------
# 15. Duplicados
# -----------------------------
dup <- sum(duplicated(df))
cat("Duplicados:",dup,"\n")

# -----------------------------
# 16. Scatterplots multivariados
# -----------------------------
if(ncol(numeric_vars)>=2){
  
  comb <- combn(names(numeric_vars),2)
  
  for(i in 1:ncol(comb)){
    
    p <- ggplot(sample_df,
                aes(x=.data[[comb[1,i]]],
                    y=.data[[comb[2,i]]]))+
      
      geom_point(alpha=0.2)+
      theme_minimal()
    
    print(p)
    
  }
  
}

# -----------------------------
# 17. PCA
# -----------------------------
if(ncol(numeric_vars)>=3){
  
  pca <- prcomp(
    sample_df %>% select(where(is.numeric)),
    scale.=TRUE
  )
  
  print(summary(pca))
  
  biplot(pca)
  
}

# -----------------------------
# 18. Normalidad
# -----------------------------
for(col in names(numeric_vars)){
  
  sample_vals <- sample(numeric_vars[[col]],
                        min(5000,length(numeric_vars[[col]])))
  
  print(shapiro.test(sample_vals))
  
}

# -----------------------------
# 19. ECDF
# -----------------------------
for(col in names(numeric_vars)){
  
  p <- ggplot(sample_df,
              aes(x=.data[[col]]))+
    
    stat_ecdf()+
    theme_minimal()+
    labs(title=paste("ECDF:",col))
  
  print(p)
  
}

# -----------------------------
# 20. Percentiles
# -----------------------------
percentiles <- sapply(
  numeric_vars,
  function(x)
    quantile(x,
             c(.01,.05,.25,.5,.75,.95,.99),
             na.rm=TRUE)
)

print(percentiles)

# -----------------------------
# 21. Skewness
# -----------------------------
skewness <- sapply(
  numeric_vars,
  function(x)
    mean((x-mean(x,na.rm=TRUE))^3,
         na.rm=TRUE) /
    sd(x,na.rm=TRUE)^3
)

print(skewness)

# -----------------------------
# 22. Curtosis
# -----------------------------
kurtosis <- sapply(
  numeric_vars,
  function(x)
    mean((x-mean(x,na.rm=TRUE))^4,
         na.rm=TRUE) /
    sd(x,na.rm=TRUE)^4
)

print(kurtosis)

# -----------------------------
# 23. Top correlaciones
# -----------------------------
if(ncol(numeric_vars)>1){
  
  corr_long <- as.data.frame(as.table(corr_matrix))
  
  corr_long <- corr_long[
    corr_long$Var1!=corr_long$Var2,]
  
  top_corr <- corr_long %>%
    arrange(desc(abs(Freq))) %>%
    head(10)
  
  print(top_corr)
  
}

# -----------------------------
# 24. Balance de categorías
# -----------------------------
if(ncol(cat_vars)>0){
  
  for(col in names(cat_vars)){
    
    print(prop.table(table(df[[col]])))
    
  }
  
}

# -----------------------------
# 25. Valores extremos
# -----------------------------
extremos <- sapply(
  numeric_vars,
  function(x){
    c(min=min(x,na.rm=TRUE),
      max=max(x,na.rm=TRUE))
  }
)

print(extremos)

# -----------------------------
# 26. Medias por categoría
# -----------------------------
if(ncol(cat_vars)>0 &
   ncol(numeric_vars)>0){
  
  for(cat in names(cat_vars)){
    
    for(num in names(numeric_vars)){
      
      print(
        df %>%
          group_by(.data[[cat]]) %>%
          summarise(
            mean_val=
              mean(.data[[num]],
                   na.rm=TRUE)
          )
      )
      
    }
    
  }
  
}

# -----------------------------
# 27. Heatmap categórico
# -----------------------------
sample_df %>%
  count(
    .data[[names(cat_vars)[1]]],
    .data[[names(cat_vars)[2]]]
  ) %>%
  arrange(desc(n))

# -----------------------------
# 28. Reporte automático
# -----------------------------
create_report(
  sample_df,
  output_file="EDA_report.html",
  report_title="Exploratory Data Analysis"
)