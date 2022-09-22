
# Author: Akanksha Mukhriya and Rajeev Kumar

# Ensemble member selection 

# Input:
# Score_List_SET - matrix of outlier scorelists of the candidate base detectors
# DATA_LABELS - Actual groud truth
# d - drop rate
# t - threshold percentage for deciding |top-k| by each detector
# Combination Technique - In this work, we use Average for combination of scores
# ht - method_types
# k_p - parameter range
# v_or_b  - ACC: V*-Select(2) / Boost*-Select(3)
# div_num - DIV: V*-Select(5) / Boost*-Select(6)
# th_m ((as given in the paper))
# l_or_z - Normalization: Linear-scaling(1) / Z-score normalization(2)
# th_eq - Equation no (as given in the paper)


# Output:
# Selected_AnD - Indices of selected detectors for an ensemble
# ROC_AUC - ROC AUC performance of selected ensemble AnD



source("Current_prediction.R")
source("Convert_binary.R")
source("Sort_Descending.R")
source("Selected_en.R")
source("Sel_en2.R")
source("Roc_Auc.R")
source("L_Norm.R")
source("Z_Norm.R")
source("Vertical_Select.R")
source("Boosting.R")
source("Boost_Select.R")
source("ACC_DIV_Scores.R")
source("Th_Acc.R")
source("AnD_SELECT.R")

# ..........Rather than executing base detectors using ELKI-framework, you can use our scorelist as given in Score_Lists_Set.Zip in csv form.
# ...... Importing SCORE_LISTS for all datasets in the current R-workspace
# ..........Naming format for all SCORE_LIST_SET.csv -- Dataset_name(as per paper)_SCORE_LIST_SET.csv
# ..........Example for importing all 22 scorelists of SpamBase dataset named as - "SpamBase_SCORE_LIST_SET.csv" in Data.zip

# SpamBase_SCORE_LIST_SET <- as.matrix(read.csv("D:/R_bin/SpamBase_SCORE_LIST_SET.csv"), header = TRUE)
# as we use total 22 candidate detectors: 1-11 --- Average k-NN and 12-22 --- LOF
# SpamBase_DATA_LABELS <- as.matrix(read.csv("D:/R_bin/SpamBase_DATA_LABELS.csv"), header = TRUE)

#...........................................................................
#...........................................................................


# Load SCORE_LIST_SET of all candidate detectors from Score_List_Set.zip, 
# or run LOF and Avg k-NN detectors separately using ELKI-framework

# Load "DATA_LABLES" from Data_Labels.zip. Although the full data files (.csv) also contains labels 
# in their last columns, but these CSVs only contain Lables, in the form of: outlier- 1, and inlier- 0

# These two are MANDATORY INPUTS to exeute code file from Main.R, 
# for other inputs, default values as used in Main.R (this file) will be used 
SCORE_LIST_SET = as.matrix(ALOI_SCORE_LIST_SET) # after loading ALOI_SCORE_LIST_SET from Score_Lists_Set.zip
DATA_LABELS = as.matrix(ALOI_DATA_LABELS) # from Data_Labels.zip


SCORE_LIST_SET_IND = as.matrix(Sort_Descending(SCORE_LIST_SET))
SCORE_LIST_SET_NORM = as.matrix(Z_Norm(SCORE_LIST_SET))


points = dim(SCORE_LIST_SET)[1]
members = dim(SCORE_LIST_SET)[2]
out_count2 = sum(DATA_LABELS[,1])

# V-select ...................................................

Selected_Vertical = as.matrix(Vertical_Select(SCORE_LIST_SET_NORM, SCORE_LIST_SET_IND))

EN_Vertical = as.matrix(Selected_en(Selected_Vertical, SCORE_LIST_SET_NORM))
EN_Vertical_Avg = as.matrix(Current_prediction(EN_Vertical))
En_Vertical_Rocauc = as.numeric(Roc_Auc(EN_Vertical_Avg, DATA_LABELS))
round(En_Vertical_Rocauc,4)


# Boost-select ....................................................

t = 0.05    # for Boost-select
d =  0.25   # for Boost-select

Selected_Boost = as.matrix(Boost_Select(SCORE_LIST_SET_NORM, SCORE_LIST_SET_IND, t, d))

EN_Boost = as.matrix(Selected_en(Selected_Boost, SCORE_LIST_SET_NORM))
EN_Boost_Avg = as.matrix(Current_prediction(EN_Boost))
En_Boost_Rocauc = as.numeric(Roc_Auc(EN_Boost_Avg, DATA_LABELS))
round(En_Boost_Rocauc,4)

# AnD ........................................................................

k = out_count2

ACC_DIV_temp = as.matrix(ACC_DIV_Scores(SCORE_LIST_SET_NORM, SCORE_LIST_SET_IND, k, t))
ACC_temp = as.matrix(ACC_DIV_temp[,1:3])
DIV_temp = as.matrix(ACC_DIV_temp[,4:7])

acc_count = dim(ACC_temp)[2]
div_count = dim(DIV_temp)[2]
total_scores = acc_count + div_count

Final_Scores = matrix(data=0, nrow = members, ncol=total_scores)
Final_Scores[,1:acc_count] = ACC_temp[,]
Final_Scores[,(acc_count+1):total_scores] = DIV_temp[,]

k_p = as.matrix(cbind(5,10,20,30,40,50,60,70,80,90,100))
hm = dim(k_p)[2]
ht = members/hm

if(l_or_z==1)
{
  Final_Scores_Norm1 = as.matrix(L_Norm(Final_Scores))  
}else{
  Final_Scores_Norm1 = as.matrix(Z_Norm(Final_Scores))   
}

# For-th_1 ...........................................................

v_or_b = 3        # ACC: V*-Select(2) / Boost*-Select(3)
div_num = 6       # DIV: V*-Select(5) / Boost*-Select(6)
th_m = 0.7        # (as given in the paper) - 0.7 (70%) / 0.8(80%)
l_or_z = 1        # Normalization: Linear-scaling(1) / Z-score normalization(2)
th_eq = 1         # Equation no (as given in the paper) - 1/2/3


Selected_AnD = as.matrix(AnD_SELECT(Final_Scores_Norm1, ht, k_p, v_or_b, div_num, th_m, l_or_z, th_eq))
Selected_AnD
Selected_AnD_t = as.matrix(t(Selected_AnD))
EN_AnD_Avg = as.matrix(Sel_en2(Selected_AnD_t, SCORE_LIST_SET_NORM))
En_AnD_Rocauc = as.numeric(Roc_Auc(EN_AnD_Avg, DATA_LABELS))
round(En_AnD_Rocauc,4)


# For-th_2 ...........................................................

v_or_b = 3        # ACC: V*-Select(2) / Boost*-Select(3)
div_num = 6       # DIV: V*-Select(5) / Boost*-Select(6)
th_m = 0.7        # (as given in the paper) - 0.7 (70%) / 0.8(80%)
l_or_z = 1        # Normalization: Linear-scaling(1) / Z-score normalization(2)
th_eq = 2         # Equation no (as given in the paper) - 1/2/3


Selected_AnD = as.matrix(AnD_SELECT(Final_Scores_Norm1, ht, k_p, v_or_b, div_num, th_m, l_or_z, th_eq))
Selected_AnD
Selected_AnD_t = as.matrix(t(Selected_AnD))
EN_AnD_Avg = as.matrix(Sel_en2(Selected_AnD_t, SCORE_LIST_SET_NORM))
En_AnD_Rocauc = as.numeric(Roc_Auc(EN_AnD_Avg, DATA_LABELS))
round(En_AnD_Rocauc,4)


# For-th_3 ...........................................................

v_or_b = 3        # ACC: V*-Select(2) / Boost*-Select(3)
div_num = 6       # DIV: V*-Select(5) / Boost*-Select(6)
th_m = 0.7        # (as given in the paper) - 0.7 (70%) / 0.8(80%)
l_or_z = 2        # Normalization: Linear-scaling(1) / Z-score normalization(2)
th_eq = 1         # Equation no (as given in the paper) - 1/2/3


Selected_AnD = as.matrix(AnD_SELECT(Final_Scores_Norm1, ht, k_p, v_or_b, div_num, th_m, l_or_z, th_eq))
Selected_AnD
Selected_AnD_t = as.matrix(t(Selected_AnD))
EN_AnD_Avg = as.matrix(Sel_en2(Selected_AnD_t, SCORE_LIST_SET_NORM))
En_AnD_Rocauc = as.numeric(Roc_Auc(EN_AnD_Avg, DATA_LABELS))
round(En_AnD_Rocauc,4)


# For-th_4 ...........................................................

v_or_b = 3        # ACC: V*-Select(2) / Boost*-Select(3)
div_num = 6       # DIV: V*-Select(5) / Boost*-Select(6)
th_m = 0.7        # (as given in the paper) - 0.7 (70%) / 0.8(80%)
l_or_z = 2        # Normalization: Linear-scaling(1) / Z-score normalization(2)
th_eq = 3         # Equation no (as given in the paper) - 1/2/3


Selected_AnD = as.matrix(AnD_SELECT(Final_Scores_Norm1, ht, k_p, v_or_b, div_num, th_m, l_or_z, th_eq))
Selected_AnD
Selected_AnD_t = as.matrix(t(Selected_AnD))
EN_AnD_Avg = as.matrix(Sel_en2(Selected_AnD_t, SCORE_LIST_SET_NORM))
En_AnD_Rocauc = as.numeric(Roc_Auc(EN_AnD_Avg, DATA_LABELS))
round(En_AnD_Rocauc,4)





