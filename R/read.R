
# Function that converts mbmdr-output into a R-readable data.frame
# Parameters: 
# cut_p.value: upper bound for p-value of variable-pairs. Pairs with p-value >= cut_p.value will be ignored 
# cut_chi_square: lower bound for Chi.square value of variable-pairs. Pairs with Chi.square <= cut_chi_square will be ignored
# inputFile_tables: mbmdr-output-file, that lisis the hlo-matrices
# inputFile_list: mbmdr-output-file that lists the significance of all variable pairs
# correction: which correction method should be choosen? (Values are "CODOMINANT"(default), "ADDITIVE" and "NONE")
reading <- function(cut_p.value = NA,
                    cut_chi_square_value = NA,
                    inputFile_tables, # = "input.m_models.txt",
                    inputFile_list = "input.result",
                    correction = "CODOMINANT",
                    daten = data
                    ){
  # read input files and save theese.
  # The data.rame "output" will collect all further information as well.
  input_tables <- readLines(inputFile_tables)
  output <- read.table(inputFile_list, 
                       header = FALSE, 
                       skip = 3,
                       col.names = c("First_Marker", 
                                     "Second_Marker", 
                                     "Chi.square", 
                                     "p.value")
  )
  # Find the positions of the right matrices in the HLO-matrix-file.
  # In dependency of the given correction method the text search phrases (whole lines or distinct phrases) differ
  all_ends <- which(substr(input_tables, 1, 22) == "Categories contrasted:")
  if (correction == "CODOMINANT"){list_of_attributes <- lapply(list_of_affecteds, unique)

    output$starting_line <- which(input_tables == "HLO matrix: with CODOMINANT correction")
    output$ending_line <- all_ends[3*(1:nrow(output))]
  }
  if (correction == "ADDITIVE"){
    output$starting_line <- which(input_tables == "HLO matrix: with ADDITIVE correction")
    output$ending_line <- all_ends[3*(1:nrow(output)) - 1]
  }
  if (correction == "NONE"){
    output$starting_line <- which(input_tables == "HLO matrix: WITHOUT correction")
    output$ending_line <- all_ends[3*(1:nrow(output))-2]
  }
  # Remove all uninformative matrices, therefore select by p-value and chi-square test result.
  if (!is.na(cut_p.value)){
    output <- output[which(output$p.value < cut_p.value),]
  }
  if (!is.na(cut_chi_square_value)){
    output <- output[which(output$Chi.square > cut_chi_square_value),]
  }
  # Work on if there are matrices left
  if (nrow(output) != 0){
  # read size of each HLO-matrix and saveto output-data.frame
  output$length <- output$ending_line - output$starting_line - 1
  output$width <- -1
  for (i in 1:nrow(output)){
    output$width[i] <- length(strsplit(input_tables[output$starting_line + 1][[i]], " ")[[1]])
  }
  # cast the HLO-matrices from the text-input into a matric of type "character", using the support function "my_matrix_cast".
  output$matrix <- lapply(1:nrow(output), my_matrix_cast, output = output, input_tables = input_tables, daten = daten)
  }  
  return(output)
}


# support function that converts lines with HLO-atrix into a matrix of type "character".
# The function is written to be called by lapply in function reading.
# Input parameters are:
# line_counter: an integer value defining the index of the currently computed matrix in the output-data.frame
# output: an data.frame collecting all information from the mbmdr-output
# input_tables: a transcript of the mbmrd-output "input.m_models.txt"
my_matrix_cast <- function(line_counter, 
                           output = output,
                           input_tables = input_tables, 
                           daten = daten
                           ){
  # cutting the lines that code the current matrix and bring in list form with single letters
  teilschritt <- (strsplit(input_tables[(output$starting_line[line_counter] + 1) : (output$ending_line[line_counter] - 1)], " "))
  # creating the final matrix (in empty form)
  res <- matrix("O", output$length[line_counter], output$width[line_counter])
  rownames(res) <- sort(unique(daten[, as.character(output[line_counter, "First_Marker"])]))
  colnames(res) <- sort(unique(daten[, as.character(output[line_counter, "Second_Marker"])]))
  # fill the rresults-matrix element-wise
  for (j in 1:output$length[line_counter]){
    for (k in 1:output$width[line_counter]){
      res[j,k] <- teilschritt[[j]][[k]]   
    }
  }
  return(res)  
}
