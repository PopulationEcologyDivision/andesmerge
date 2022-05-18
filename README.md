# andesmerge - a utility for reformatting andes data into the ESE format
-----------------
This package exists to convert the csv files output by [andes](https://github.com/dfo-gulf-science/andes) into the older format used by the ESE.

# Description
Each time a new survey returns, the data must be QC'd and loaded into the groundfish schema. Previously, the ESE (Ecosystems Survey Entry) system output files in a specific format and loaded them into ESE_* tables within Oracle.  All of the QC and loading procedures are dependent on the structure created by the ESE.  This package is built to 

1. convert the andes data into the same format used by the ESE; and
2. load the converted data into Oracle  

Future effort will be made to incorporate existing (and improved) QC checks directly in this package, and further, load the data directly into the appropriate tables in groundfish (forgoing the step of generating ESE-specific tables altogether)

# Usage
By pointing a function (i.e. [`matchAndesToESE()`](https://github.com/PopulationEcologyDivision/andesmerge/blob/main/R/matchAndesToESE.R) at a folder of andes-generated csv files, the package will output a list object containing the following objects:

* ESE_MISSIONS
* ESE_SETS
* ESE_CATCHES
* ESE_BASKETS
* ESE_SPECIMENS
* ESE_LV1_OBSERVATIONS

With appropriate Oracle credentials, another function (i.e. [`replaceESEData()`)](https://github.com/PopulationEcologyDivision/andesmerge/blob/main/R/replaceESEData.R) can then be run to load the generated objects into the groundfish schema

Installation:
```r
# install.packages("devtools")
devtools::install_github("PopulationEcologyDivision/andesmerge", dependencies = TRUE)
```

To generate a list object containing all of the various ESE_* objects, one would run the following script, where the path points to a folder containing the following files:

* tmp_basket_data.csv
* tmp_catch_data.csv
* tmp_cruise_data.csv
* tmp_obs_types_data.csv
* tmp_set_data.csv
* tmp_specimen_data.csv

```r
library(andesmerge)
thedata <- matchAndesToESE(dataPath = "c:/<some_path_to_the_andes_csv_files>/")
```
The data can then be loaded into temporary tables within the groundfish schema using the following commands.  This will delete any existing content within the target tables, and replace it with the new information drawn from the csv files/
```r
cxn <- Mar.utils::make_oracle_cxn(fn.oracle.username = groundfish.username,
                                  fn.oracle.password = groundfish.password, 
                                  fn.oracle.dsn = "PTRAN", 
                                  usepkg = 'roracle')
replaceESEData(cxnObj = cxn, target_schema = "groundfish", target_table = "ANDESE_MISSIONS",
              source_df = thedata$ESE_MISSIONS)
replaceESEData(cxnObj = cxn, target_schema = "groundfish", target_table = "ANDESE_SETS", 
              source_df = thedata$ESE_SETS)
replaceESEData(cxnObj = cxn, target_schema = "groundfish", target_table = "ANDESE_BASKETS", 
              source_df = thedata$ESE_BASKETS)
replaceESEData(cxnObj = cxn, target_schema = "groundfish", target_table = "ANDESE_CATCHES", 
              source_df = thedata$ESE_CATCHES)
replaceESEData(cxnObj = cxn, target_schema = "groundfish", target_table = "ANDESE_SPECIMENS", 
              source_df = thedata$ESE_SPECIMENS)
replaceESEData(cxnObj = cxn, target_schema = "groundfish", target_table = "ANDESE_LV1_OBSERVATIONS", 
              source_df = thedata$ESE_LV1_OBSERVATIONS)
```

As of 2022.05.17, we do QC within these new ANDESE_* objects inside of Oracle.  Any QC issues are relayed back to this repository as [github 'issues'](https://github.com/PopulationEcologyDivision/andesmerge/issues), and are corrected using the [`tweaks.R`](https://github.com/PopulationEcologyDivision/andesmerge/blob/main/R/tweaks.R) file.  When convenient, the functions above are re-run, with the tweaks fixing the issues.  Once satisfied, the data will be loaded into the production tables in the same way it would have been done for the original ESE output.
