pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  pollutant_values <- data.frame()

  for(i in id) {

    filename <- sprintf("%03d", i)
    filename_full <- paste(directory, "/", filename, ".csv", sep="")
    
    data = read.csv(filename_full, header = TRUE, sep=",")

    pollutantdata <- data[c(pollutant)]
    pollutant_values <- rbind(pollutant_values, na.omit(pollutantdata))
  }
  
  sprintf("%.3f", mean(pollutant_values[[pollutant]]))
}


complete <- function(directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases
  
  complete_cases <- data.frame(id = integer(), nobs= integer())
  
  for(i in id) {
    
    filename <- sprintf("%03d", i)
    filename_full <- paste(directory, "/", filename, ".csv", sep="")
    
    data <- read.csv(filename_full, header = TRUE, sep=",")
    
    df <- data.frame(id = i, nobs= nrow(na.omit(data)))
    complete_cases = rbind(complete_cases, df)
  }

  print(complete_cases)
}


corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations  
  corrs <- numeric()
  filenames <- list.files(directory)
  
  for(filename in filenames) {
    
    filename_full <- paste(directory, "/", filename, sep="")
    
    data <- read.csv(filename_full, header = TRUE, sep=",")
    c_cases = nrow(na.omit(data))
    
    if(c_cases > threshold) {
      corrs <- c(corrs, cor(data$sulfate, data$nitrate, use="complete.obs"))
    }
    
  }
  
  corrs
}


