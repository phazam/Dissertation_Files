## #########################################################################
## Function: get_admissible
## Functionality: find rep numbers of Mplus output files that do not
## contain any inadmissible warnings and categorize which warnings are
## observed.
## 
## Input: file_filter - a single string to identify relevant output files
##                      (the function already selects only .out files, so
##                      this needs to be a different string)
##         rep_filter - a single string to identify the last string in a
##                      file name before the rep number occurs.
##                      Default = rep
##           warnings - warning keywords to search for. Function comes with
##                      a default set of warning keywords to search for,
##                      see Word Doc "Default Warning Keywords"
## Output:       list - admissible: numeric vector of admissible
##                      replication numbers
##                      which_warnings: tibble with warning keywords
##                      and number of occurances
############################################################################

get_admissible <- function(target = getwd(), file_filter, rep_filter = "rep", warnings = "default",
                           recursive = FALSE, move = TRUE, silent=TRUE) {
  ## Check if tidiverse is installed and loaded
  packages <- c("tidyverse", "filesstrings")
  
  package.check <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  
  ## Check if file filter is one string
  if (length(file_filter) > 1) {stop("Error: Your file filter contains more than one term.")}
  
  ## If warnings are set to default, assign default warning key words
  if (warnings == "default") {
    warnings <- c("AVOID SINGULARITY", "NON-POSITIVE DEFINITE", 
                  "WAS NOT REPLICATED", "NOT POSITIVE DEFINITE", 
                  "SADDLE POINT", "CONVERGENCE CRITERION IS NOT SATISFIED")
  }
  
  ## Create data frame for which_warnings overview
  warnings_frame <- tibble(warning = warnings,
                           freq = rep(0, length(warnings)))
  ## Determine whether target is a file or a directory
  if (file.exists(target)) {
    if (file.info(target)$isdir == TRUE) {
      
      ## List all files in directory                             
      files <- list.files(target, recursive=recursive, full.names=TRUE)
      
      
      ## Select files for the correct cell of the simulation (based on file_filter)
      #cell <- files[grep(file_filter, files, invert = TRUE)]
      
      ## Select only .out files
      files <- files[grep(file_filter, files, invert = TRUE)] #files[str_which(cell, ".out")]
      files <- files[str_which(files, ".out")]
      # \\.out$
      
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
  reps <- str_split_fixed(files, rep_filter, n=2)[,2]
  
  ## Split each remaining string at the ".out" point, save what comes before
  ## as a numeric value (these are the rep numbers)
  reps <- as.numeric(str_split_fixed(reps, ".out", n=2)[,1])
  
  ## Create a vector to house the admissible rep numbers and names, and not admissble rep numbers
  admissible <- vector()
  admissible_name <- vector()
  not_admissible <- vector()
  not_admissible_name <- vector()
  
  ## Create a progress bar
  pb <- winProgressBar(title = "progress bar", min = 0,
                       max = length(files), width = 300)
  
  ## For each file
  for (i in 1:length(files)) {
    ## Read in the lines of the file
    output <- read_lines(files[i])
    ## Save the rep number
    rep_number <- reps[i]
    ## Search for the warning key words in the .out file and save the lines that
    ## contain warning messages
    warning_string <- output[str_which(output, paste(warnings, collapse = '|'))]
    ## If no warning_string was found
    if (length(warning_string) == 0) {
      ## Add rep_number to admissible vector
      admissible <- c(admissible, rep_number)
      ## Add file name to admissible_name vector
      admissible_name <- c(admissible_name, files[i])
      
      ## else
    } else {
      ## Add rep_number to not_admissible vector
      not_admissible <- c(not_admissible, rep_number)
      ## Add file name to admissible_name vector
      not_admissible_name <- c(not_admissible_name, files[i])
      ## For each warning key word in the warnings_frame 
      for (j in 1:nrow(warnings_frame)) {
        ## Set occurance of warning to zero
        occur <- 0
        ## If that specific warning is found in the output,
        ## change occur to 1
        if (length(str_which(output, paste(warnings_frame$warning[j]))) > 0) occur <- 1
        ## Update the counter for that warning by adding either 1 or 0 to it
        warnings_frame[j,2] <- warnings_frame[j,2] + occur
      }
    }
    ## Initialize progress bar
    setWinProgressBar(pb, i, title=paste( round(i/length(files)*100, 0),
                                          "% done"))
  }
  
  
  
  # Create new sub-folder and move inadmissible files here
  if(move == TRUE){
    if(length(not_admissible_name)!=0){
      for(i in 1:length(not_admissible_name)){
        file.move(not_admissible_name[i], paste(target, "/inadmissible", sep = "", collapse = NULL))
      }
    }
  }
  close(pb)
  
  ## Create a named list with the admissible reps vector and the which_warnings frame
  list(admissible = admissible, not_admissible = not_admissible, admissible_name = admissible_name, not_admissible_name = not_admissible_name, which_warnings = warnings_frame)
}
