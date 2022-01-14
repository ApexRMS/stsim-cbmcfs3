# This script installs missing dependencies and loads all dependencies

depend <- c("rsyncrosim", "RODBC", "tidyverse") 
ndepend <- length(depend)
present <- installed.packages()[ , "Package"]
needed <- depend[!(depend %in% present)] 
nneeded <- length(needed)
if(nneeded > 0){
  install.packages(needed)
}
for(dep in 1:ndepend){
  suppressMessages(eval(bquote(library(.(depend[dep])))))
}