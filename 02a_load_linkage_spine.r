print("Loading linkage file")
linkage <- fread("[omitted]/linkage_file.txt", sep = "|")
setnames(linkage, names(linkage), c("person_id", "tokenid"))

linkage <- linkage[order(person_id)]
linkage[, person_id := gsub("[a-z]", "", person_id)]

linkage <- linkage[!duplicated(linkage)]