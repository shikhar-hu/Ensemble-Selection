
<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888">

# Testing Backtrack Search Optimization Algorithm as Ensemble Selection Method

```yaml

Name of Quantlet : Ensemble_testing

Published in : Numerical Introductory Course (SS17)

Description : For testing of BSA ensemble selection method against stacking and individual classifiers.

Keywords : ensemble selection, ensemble search, search optimization, stacked generalization, stacking, random forest

Author: Shikhar Srivastava

Submitted:  Sun, July 23 2017 by Shikhar Srivastava

Datafiles: 
1. Two files from (https://github.com/shikhar-hu/Ensemble-Selection/tree/master/Datasets)
2. http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/australian/australian.dat
3. http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric

Output : 'BSA performs better for 3 out 4 datasets with full library search. No improvement with pruned library.'

```

## Procedure for reproducing the results (with using R 3.3.1 in windows 64it)
1. Create empty folder using below code
```r
user=(Sys.info()[6])
Desktop=paste("C:/Users/",user,"/Desktop/",sep="")
setwd(Desktop)

dir.create(paste(Desktop,"/MEMS",sep=""))
dir.create(paste(Desktop,"/MEMS/S6/NIC",sep=""))
dir.create(paste(Desktop,"/MEMS/S6/NIC/Datasets",sep=""))

home=paste(Desktop,"MEMS/S6/NIC/Datasets/",sep="")
setwd(home)
```
2. Download this data-info [CSV](https://github.com/shikhar-hu/Ensemble-Selection/blob/master/Enemble_test_codes/data_specs.csv) in the folder. Also download these [CSVs](https://github.com/shikhar-hu/Ensemble-Selection/tree/master/Datasets) into the same folder with same name.
3. Also download all the [R-codes](https://github.com/shikhar-hu/Ensemble-Selection/tree/master/Enemble_test_codes) into the same folder. 
4. Run the codes with ordered prefixes.
5. Take care that while running the codes under "3_" (which are classifier codes), you change the number of cores accordingly as it uses parallel computing.

