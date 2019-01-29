####### myPreparation.r file for Ben Rodgers (42006945) ########


##1.1 Extract the data frame and put into columns ###
cancer <- read.table("./data/breast-cancer-wisconsin.data", sep = ",")



##1.2 Assign the following names to the 11 different columns in your dataset ###
names(cancer) <- c("Sample.code.number",
                    "Clump.Thickness", 
                   "Uniformity.of.Cell.Size",
                  "Uniformity.of.Cell.Shape",
                   "Marginal.Adhesion",
                   "Single.Epithelial.Cell.Size",
                   "Bare.Nuclei",
                   "Bland.Chromatin",
                   "Normal.Nucleoli",
                   "Mitoses",
                  "Class")

### 1.3 Delete all rows that have missing data ####
cancer = subset(cancer, Bare.Nuclei!="?")

### 1.3(cont) Make all columns except Class as integer
cancer$Clump.Thickness <- as.numeric(cancer$Clump.Thickness)
cancer$Uniformity.of.Cell.Size <- as.numeric(cancer$Uniformity.of.Cell.Size)
cancer$Uniformity.of.Cell.Shape <- as.numeric(cancer$Uniformity.of.Cell.Shape)
cancer$Marginal.Adhesion <- as.numeric(cancer$Marginal.Adhesion)
cancer$Single.Epithelial.Cell.Size <- as.numeric(cancer$Single.Epithelial.Cell.Size)
cancer$Bare.Nuclei <- as.numeric(cancer$Bare.Nuclei)
cancer$Bland.Chromatin <- as.numeric(cancer$Bland.Chromatin)
cancer$Normal.Nucleoli <- as.numeric(cancer$Normal.Nucleoli)
cancer$Mitoses <- as.numeric(cancer$Mitoses)


### 1.4 Delete the first column ###
cancer$Sample.code.number <- NULL


### 1.5 Change the Class column to factor
cancer$Class <- as.factor(cancer$Class)


### 1.6 Save the dataframe into a file

saveRDS(cancer, file="./Data/bcw_processed.Rda")
