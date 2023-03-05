### This loop reads all .rds files in a folder and assigns them to the filename,
### less the extension
fileList <- list.files(path = "./data", pattern = "rds")
files <- file.path("./data", fileList)
fileList <- str_remove_all(fileList, ".rds")
for (i in 1:length(files)) {
  assign(fileList[i], as.data.frame(readRDS(files[i])))
}
