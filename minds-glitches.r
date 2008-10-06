# 3-classifly:
source("3-classifly/classifly.r")
# Error in gridz[(gridy$group - 1) * length(gridx$unique) + gridx$group] <- data$z :
#   nothing to replace with

# 4-meifly:
source("4-meifly/swiss.r")
# Error in residuals.ensemble(data) :
#   could not find function "reorder_factor"

# 5-projection-pursuit 
  # Error in if (diff(range) == 0) return(c(range[1] - 0.5, range[1] + 0.5)) : 
  #   missing value where TRUE/FALSE needed
    
# 6-nnet
source("6-nnet/nnet.r")
# Error: Continuous variable () supplied to the discrete scale_manual.