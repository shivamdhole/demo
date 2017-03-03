library(ggplot2)
library(dplyr)
library(gbm)
library(viridis)
library(caret)
library(e1071)
library(jsonlite)
EP <- read.csv("/Users/farhanzain/Documents/Monash/Current Monash/ETC3250/Project/CUST_DATA.csv", stringsAsFactors = T)

#### Discretise REG_EP
master.df <- EP %>% filter(!is.na(REG_EP))
REG.EP.cuts <- EP %>% dplyr::select(REG_EP) %>% .[,1] %>% quantile(c(1/3,2/3))
master.df <- EP %>% mutate( 
  REG.EP.split = cut(
    master.df$REG_EP,
    c(-Inf,REG.EP.cuts[1:2],Inf),
    c("Bad","Middle","Good")
  )
)


ANALYSIS_TYPE <- "GLOBAL"

if (ANALYSIS_TYPE == "GLOBAL"){
  master.df <- master.df %>% rename(
    RESPONSE = REG.EP.split
  )
}

if (ANALYSIS_TYPE == "MATCHED"){
  master.df <- master.df %>% rename(
    RESPONSE = REG.EP.split
  )
}

if (ANALYSIS_TYPE == "GLOBALREG"){
  master.df <- master.df %>% rename(
    RESPONSE = REG_EP
  )
}



#################################################################################
#Select columns!
data.1 <- master.df %>% dplyr::select(
  
  #REG_EP,
  RESPONSE,
  # PY_N_BP,
  # PY_N_LOB,
  # PY_N_BPRegion,
  # PY_N_LOBxBP,
  # PY_MAIN_BPRegion,
  # PY_MAIN_BP,
  # PY_MAIN_NLOB,
  
  #BOOKS_HOME_MARKET,
  #BOOKS_IN_ASIA,
  #BOOKING_TYPE,
  #BOOKING_TYPE_4,
  BASEL_TREATMENT,
  #PD_DELTA,
  #contains("CLUSTER_BOOK_"),
  #contains("BP_"),
  #BOOKING_COMBO,
  
  
  
  #CIQ_n_subs,
  #CIQ_n_subs_countriesANZAsia,
  
  -YEAR,
  
  #INDUSTRY = Industry_Sector_Intermediate,
  #INDUSTRY = Industry_Sector_Intermediate_2,
  INDUSTRY,
  #INDUSTRY = INDUSTRY_OLD,
  
  #CMGGlobalBu_FIX,
  #CMGSegmentName,
  -CMGSubSegmentName,
  #FIG,
  #REI,
  #REIA,
  
  #ICRM data
  -starts_with("ICRM_"),
  -contains("_past"),
  #Survey Data
  -starts_with("PL_"),
  -starts_with("GW_"),
  #REL_TENURE#,
  
  #CapitalIQ data
  #starts_with("CIQ_"),
  #-starts_with("CIQ_TR3"),
  #-starts_with("CIQ_TR5"),
  #-starts_with("CIQ_TR10"),
  #CIQ_Accounts_Payable.Total,
  #CIQ_Accounts_Receivable.Total,
  #CIQ_Total_Assets,
  starts_with("MAIN"),
  -MAIN_BP,
  #-N_LOB,
  #-N_BP,
  N_LOBxBP,
  
  #PD_MID_POINT,
  CCR_FIX,
  -SI_FIX,
  
  -CIQ_Sector,
  -CIQ_Subsector,
  -CIQ_simpleIndustryDescription,
  
  
  -GlobalControlPoint,
  -GCPRegion,
  -CIQ_country,
  -CIQ_region,
  
  -starts_with("Asia_"),
  -starts_with("AusNZ_"),
  -starts_with("EandA_"),
  -starts_with("FCM_"),
  -starts_with("RM_"),
  -starts_with("OTHER_"),
  -starts_with("PCM_"),
  -PCM_REG_EP,
  -PCM_REG_EP_PCM_ADJ,
  -PCM_REG_EP_PCM_ONLY,
  -PCM_REG_EP_PCM_ADJ_ONLY,
  #starts_with("N_"),
  N_LOB,
  N_PRODUCT_BUSINESS,
  -contains("rating"), 
  -contains("simpleIndustry")
  #-contains("region"), 
  #-contains("country"),# -contains("_TR"),
  #-contains("SI"), 
  #-contains("_n_")
  
) 


col_types <- sapply(1:ncol(data.1), function(x) class(data.1[,x]))
data.1[,which(col_types=="logical")] <- sapply(data.1[,which(col_types=="logical")],factor) %>% data.frame()
set.seed(845)
train.idx <- sample(1:nrow(master.df),size=(0.7*nrow(master.df)))
data.test <- data.1[train.idx,]
data.train <- data.1[-train.idx,]

### Classification model

boost.EP <- gbm(RESPONSE~., 
                data=data.train,
                distribution="multinomial",
                n.trees=5000,
                interaction.depth=4
)


summary(boost.EP)
print("Shivam")
EP.pred.train <- predict(boost.EP,
                         newdata = data.train
                         ,n.trees = 5000,
                         type="response")

EP.pred.train.factor <- colnames(EP.pred.train)[apply(EP.pred.train,1, which.max)] 
EP.pred.train.factor <- factor(EP.pred.train.factor,levels=c("Bad","Middle","Good")) 


EP.pred.test <- predict(boost.EP,newdata = data.test,n.trees = 5000,type="response") 

EP.pred.test.factor <- colnames(EP.pred.test)[apply(EP.pred.test,1,which.max)] 
EP.pred.test.factor <- factor(EP.pred.test.factor,levels=c("Bad","Middle","Good")) 

h <- as.data.frame(EP.pred.train.factor)
f <- as.data.frame(EP.pred.test.factor)

### Confusion Matrix
conf_train <- confusionMatrix(EP.pred.train.factor, data.train$RESPONSE)
conf_test <- confusionMatrix(EP.pred.test.factor, data.test$RESPONSE)
overall.accuracy <- conf_test$overall['Accuracy']
if(overall.accuracy > 0.6854) {
  x = "pass"
} else {
  x = "fail"
}

cat("{","\"","result","\"",":","\"",x,"\"","}",sep="")


