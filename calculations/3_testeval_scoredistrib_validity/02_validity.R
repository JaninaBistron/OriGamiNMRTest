# Calculate Validity

# load expert scoring   -----------------------------------------------------------------------------------------------
    
if (location=="Senden"){
  experts <- as.data.frame(fread(file="data/senden/experts.csv"))
  totalscore <- cbind(totalscore, experts)
  totalscore_drop <- cbind(totalscore_drop, experts)
} else {
  experts <- as.data.frame(fread(file="data/duelmen/experts.csv"))
  totalscore <- cbind(totalscore, experts)
  totalscore_drop <- cbind(totalscore_drop, experts)
}

# change order of columns - first column = participants
totalscore <- totalscore[,c(7,1,2,3,4,5,6,8)]
totalscore_drop <- totalscore_drop[,c(4,1,2,3,5)]

# calculate validity (correlation coefficients)   ----------------------------------------------------------------------

  # calculate correlation coefficient
  cor_valid_loc_r <- cor(totalscore$sumloc_r, totalscore$expertscoring, method="spearman")
  cor_valid_total_r <- cor(totalscore$sumtotal_r, totalscore$expertscoring, method="spearman")
  cor_valid_loc_c <- cor(totalscore$sumloc_c, totalscore$expertscoring, method="spearman")
  cor_valid_total_c <- cor(totalscore$sumtotal_c, totalscore$expertscoring, method="spearman")
  
  # calculate correlation coefficient after dropping tasks
  cor_valid_loc_c_drop <- cor(totalscore_drop$sumloc_c, totalscore_drop$expertscoring, method="spearman")
  cor_valid_total_c_drop <- cor(totalscore_drop$sumtotal_c, totalscore_drop$expertscoring, method="spearman")