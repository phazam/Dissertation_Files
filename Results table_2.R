results <- function (target = getwd(), rep_filter = "rep", recursive = FALSE, file_filter = ".txt", 
                     residuals, nsims=NULL, plots = TRUE)
  # residuals = "high" or "low"
  # nsims = number of simulations
{
## Check if tidiverse is installed and loaded
packages <- c("tidyverse", "MplusAutomation")
package.check <- lapply(packages, FUN = function(x) {
if (!require(x, character.only = TRUE)) {
install.packages(x, dependencies = TRUE)
library(x, character.only = TRUE)
}
})
## Determine whether target is a file or a directory
if (file.exists(target)) {
if (file.info(target)$isdir == TRUE) {
## List all files in directory
files <- list.files(target, recursive=recursive, full.names=TRUE)
## Select files for the correct cell of the simulation (based on file_filter)
cell <- files[str_which(files, file_filter)]
## Select only .out files
files <- cell[str_which(cell, ".out")]
}
else {
## If target is a single output file.
if (nchar(target) >= 4 && !substr(target, nchar(target) - 3, nchar(target)) == ".out")
stop("Specified target is not an output file.\n  Target:", target)
## Assign single file to files
files <- target
}
}
## If neither
else stop("Specified target does not exist.\n  Target: ", target)
if (length(files) == 0) {
warning("No output files detected in this directory.")
return(NULL)
}
## Split each file name in 2 after the rep_filter term, save what comes after
sims <- files[grep(".txt", files, invert = FALSE)]
reps <- cell <- files[grep(".txt", files, invert = TRUE)]
## Split each file name in 2 after the rep_filter term, save what comes after
reps <- str_split_fixed(reps, rep_filter, n=2)[,1][1]

if (length(nsims) == 0) {
  nsims <- length(sims)
#  else { nsims<- nsims
#  }
}

add <- get_admissible(target = target, file_filter = file_filter, rep_filter = rep_filter, recursive = recursive,silent=TRUE)
out<-readModels(target = target, recursive = recursive, quiet = FALSE)
agg <- create_output(out, residuals = residuals, nsims = nsims)

if (plots == TRUE) {
create_plots(out, saveto=target)
}
#for(i in 1:legnth(reps))
#{
# Object = get("reps")
# Object[1] = 0
assign(paste0(reps, "add"), add , envir = .GlobalEnv)
x<-assign(paste0(reps, "out"), out , envir = .GlobalEnv)
y <- assign(paste0(reps, "agg"), agg , envir = .GlobalEnv)
#  }


    sink(paste0("F:/patrice_dissertation/Results_",format(Sys.time(),"%b%d%Y"),".txt"), append = TRUE)
    return(agg)



#print(cat(paste("done!")))

#sink(file = NULL)
#sink()
#unlink("F:/patrice_dissertation/Results.txt")
#list(x,y,z)
}
