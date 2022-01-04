# Wrangling Project 

rm(list=ls())

#Pull in CSV data
big10Data <- read.csv('BigTen.csv')

#Latitude/Longitude for each school
big10Data$Latitude <- c(41.662696, 40.820197, 44.976416, 43.076592, 42.701848, 42.278044, 40.101952, 40.423705, 39.176613, 40.014191, 40.798213, 40.500819, 38.986918, 42.056459)
big10Data$Longitude <- c(-91.5549, -96.700476, -93.232347, -89.412487, -84.482172, -83.738224, -88.227161, -86.921195, -86.513017, -83.030914, -77.859908, -74.447399, -76.942554, -87.675267)

big10Data$ZScore <- ((big10Data$GRAD_DEBT_MDN - mean(big10Data$GRAD_DEBT_MDN)) / sd(big10Data$GRAD_DEBT_MDN)) + 1.5

library(ggplot2)
library(dplyr)

#Debt visualization for each school on a US map
testing <- ggplot(big10Data, aes(x = Longitude, y = Latitude, colour = INSTNM)) + borders('state') + geom_point(size = big10Data$ZScore) + coord_map() + theme_void()
testing

#Pull in online data
library(xml2)
page = read_html('http://online.wsj.com/public/resources/documents/info-Salaries_for_Colleges_by_Region-sort.html')

#Get each school name
schoolName <- xml_text(xml_find_all(page, "//table[@class='sortable']/tr/td[@class='col1']"))

#Get all items within a class distinction of 'col2'
col2Items <- xml_text(xml_find_all(page, "//table[@class='sortable']/tr/td[@class='col2']"))

#READS IN EACH OF THE SPECIFIC VALUES
wsjDF <- data_frame()
run <- TRUE
schoolCount <- 0
colCount <- 0
rowCount <- 0
while (run == TRUE){
  col <- schoolCount %% 8
  if(schoolCount %% 8 == 0){
    rowCount = rowCount+1
    wsjDF[rowCount,1] <- schoolName[rowCount]
  } else{
    col = col + 1
    colCount = colCount+1
    wsjDF[rowCount,col] <- col2Items[colCount]
  }
  schoolCount = schoolCount + 1
  if(colCount == 2240){
    run <- FALSE
  }
}

colnames(wsjDF) = c('SchoolName', 'Region', 'StartingMedianSalary', 'Mid_CareerMedianSalary', 'Mid_Career10thPercentileSalary', 
                    'Mid_Career25thPercentileSalary', 'Mid_Career75thPercentileSalary', 'Mid_Career90thPercentileSalary')

#Remove whack white space stuff from values
wsjDF$SchoolName <- gsub("\u00A0", "", wsjDF$SchoolName)
wsjDF$Region <- gsub("\u00A0", "", wsjDF$Region)
wsjDF$StartingMedianSalary <- gsub("\u00A0", "", wsjDF$StartingMedianSalary)
wsjDF$Mid_CareerMedianSalary <- gsub("\u00A0", "", wsjDF$Mid_CareerMedianSalary)
wsjDF$Mid_Career10thPercentileSalary <- gsub("\u00A0", "", wsjDF$Mid_Career10thPercentileSalary)
wsjDF$Mid_Career25thPercentileSalary <- gsub("\u00A0", "", wsjDF$Mid_Career25thPercentileSalary)
wsjDF$Mid_Career75thPercentileSalary <- gsub("\u00A0", "", wsjDF$Mid_Career75thPercentileSalary)
wsjDF$Mid_Career90thPercentileSalary <- gsub("\u00A0", "", wsjDF$Mid_Career90thPercentileSalary)


schoolVector <- c('University of Illinois at Urbana-Champaign (UIUC)', 'University of Michigan', 'Purdue University', 'Northwestern University',
                  'University of Wisconsin (UW) - Madison', 'Michigan State University (MSU)', 'University of Minnesota', 'Indiana University (IU), Bloomington',
                  'University of Iowa (UI)', 'Ohio State University (OSU)', 'University of Nebraska', 'Pennsylvania State University (PSU)', 'Rutgers University', 'University of Maryland, College Park')

#Create new df with only Big 10 schools
cleanedWSJDf <- subset(wsjDF, wsjDF$SchoolName %in% schoolVector)
           
#Clean school names            
cleanedWSJDf$SchoolName <- gsub('.*University of Illinois.*', 'University of Illinois Urbana-Champaign', cleanedWSJDf$SchoolName)
cleanedWSJDf$SchoolName <- gsub('.*University of Michigan.*', 'University of Michigan-Ann Arbor', cleanedWSJDf$SchoolName)
cleanedWSJDf$SchoolName <- gsub('.*Purdue University.*', 'Purdue University-Main Campus', cleanedWSJDf$SchoolName)
cleanedWSJDf$SchoolName <- gsub('.*Wisconsin.*', 'University of Wisconsin-Madison', cleanedWSJDf$SchoolName)
cleanedWSJDf$SchoolName <- gsub('.*Michigan State.*', 'Michigan State University', cleanedWSJDf$SchoolName)
cleanedWSJDf$SchoolName <- gsub('.*Minnesota.*', 'University of Minnesota-Twin Cities', cleanedWSJDf$SchoolName)
cleanedWSJDf$SchoolName <- gsub('.*Indiana.*', 'Indiana University-Bloomington', cleanedWSJDf$SchoolName)
cleanedWSJDf$SchoolName <- gsub('.*Iowa.*', 'University of Iowa', cleanedWSJDf$SchoolName)
cleanedWSJDf$SchoolName <- gsub('.*Ohio.*', 'Ohio State University-Main Campus', cleanedWSJDf$SchoolName)
cleanedWSJDf$SchoolName <- gsub('.*Nebraska.*', 'University of Nebraska-Lincoln', cleanedWSJDf$SchoolName)
cleanedWSJDf$SchoolName <- gsub('.*Pennsylvania.*', 'Pennsylvania State University-Main Campus', cleanedWSJDf$SchoolName)
cleanedWSJDf$SchoolName <- gsub('.*Rutgers.*', 'Rutgers University-New Brunswick', cleanedWSJDf$SchoolName)
cleanedWSJDf$SchoolName <- gsub('.*Maryland.*', 'University of Maryland-College Park', cleanedWSJDf$SchoolName)

#Merge Big10 and CleanedWSJ dfs
mergedDF <- merge(big10Data, cleanedWSJDf, by.x = 'INSTNM', by.y = 'SchoolName')

mergedDF[] <- sapply(mergedDF, function(x) (gsub('[$,]', '', x)))

cols2Change <- c('ADM_RATE', 'SAT_AVG', 'GRAD_DEBT_MDN', 'FAMINC', 'MD_FAMINC', 'TUITIONFEE_IN', 
                 'TUITIONFEE_OUT', 'StartingMedianSalary', 'Mid_CareerMedianSalary', 
                 'Mid_Career10thPercentileSalary', 'Mid_Career25thPercentileSalary', 
                 'Mid_Career75thPercentileSalary', 'Mid_Career90thPercentileSalary')

#Make sure money salary columns are numeric
mergedDF[cols2Change] <- sapply(mergedDF[cols2Change], as.numeric)

#Summary statistics for Big10 salary information
#Averages
avgStartingMedian <- round(mean(mergedDF$StartingMedianSalary), 2)
avgMidCareer <- round(mean(mergedDF$Mid_CareerMedianSalary), 2)
avg10thSalary <- round(mean(mergedDF$Mid_Career10thPercentileSalary), 2)
avg25thSalary <- round(mean(mergedDF$Mid_Career25thPercentileSalary), 2)
avg75thSalary <- round(mean(mergedDF$Mid_Career75thPercentileSalary), 2)
avg90thSalary <- round(mean(mergedDF$Mid_Career90thPercentileSalary), 2)
averageVals <- c(avgStartingMedian, avgMidCareer, avg10thSalary, avg25thSalary, avg75thSalary, avg90thSalary)

#Medians
medStartingMedian <- round(median(mergedDF$StartingMedianSalary), 2)
medMidCareer <- round(median(mergedDF$Mid_CareerMedianSalary), 2)
med10thSalary <- round(median(mergedDF$Mid_Career10thPercentileSalary), 2)
med25thSalary <- round(median(mergedDF$Mid_Career25thPercentileSalary), 2)
med75thSalary <- round(median(mergedDF$Mid_Career75thPercentileSalary), 2)
med90thSalary <- round(median(mergedDF$Mid_Career90thPercentileSalary), 2)
medianVals <- c(medStartingMedian, medMidCareer, med10thSalary, med25thSalary, med75thSalary, med90thSalary)

#Min
minStartingMedian <- round(min(mergedDF$StartingMedianSalary), 2)
minMidCareer <- round(min(mergedDF$Mid_CareerMedianSalary), 2)
min10thSalary <- round(min(mergedDF$Mid_Career10thPercentileSalary), 2)
min25thSalary <- round(min(mergedDF$Mid_Career25thPercentileSalary), 2)
min75thSalary <- round(min(mergedDF$Mid_Career75thPercentileSalary), 2)
min90thSalary <- round(min(mergedDF$Mid_Career90thPercentileSalary), 2)
minimumVals <- c(minStartingMedian, minMidCareer, min10thSalary, min25thSalary, min75thSalary, min90thSalary)

#Max
maxStartingMedian <- round(max(mergedDF$StartingMedianSalary), 2)
maxMidCareer <- round(max(mergedDF$Mid_CareerMedianSalary), 2)
max10thSalary <- round(max(mergedDF$Mid_Career10thPercentileSalary), 2)
max25thSalary <- round(max(mergedDF$Mid_Career25thPercentileSalary), 2)
max75thSalary <- round(max(mergedDF$Mid_Career75thPercentileSalary), 2)
max90thSalary <- round(max(mergedDF$Mid_Career90thPercentileSalary), 2)
maximumVals <- c(maxStartingMedian, maxMidCareer, max10thSalary, max25thSalary, max75thSalary, max90thSalary)

summaryDF <- data.frame(averageVals, medianVals, minimumVals, maximumVals)
rownames(summaryDF) <- c('Starting Salary', 'Mid Career Salary', 'Mid Career 10th Percentile', 'Mid Career 25th Percentile', 'Mid Career 75th Percentile', 'Mid Career 90th Percentile')

summaryDF$Range <- summaryDF$maximumVals - summaryDF$minimumVals

#Starting salary vs average student debt
lm(mergedDF$StartingMedianSalary ~ mergedDF$GRAD_DEBT_MDN)
abline(lm(mergedDF$StartingMedianSalary ~ mergedDF$GRAD_DEBT_MDN))

# Are starting salary and debt correlated?
sdcor <- cor(mergedDF$StartingMedianSalary, mergedDF$GRAD_DEBT_MDN)
sdcor
# Interpretation: no linear correlation

#H0: Correlation is 0
#Ha: Correlation is NOT 0
ctsd<-cor.test(mergedDF$StartingMedianSalary, mergedDF$GRAD_DEBT_MDN)
class(ctsd) # Object of class "hypothesis test"
ctsd$p.value
# Interpretation: p > 0.05, thus we fail to reject
# the null hypothesis that the correlation is 0
# There is no correlation between Starting Median Salary and Median Graduation Debt


#Midwest vs non-midwest starting salary
testGroup <- group_by(mergedDF, Region, StartingMedianSalary)
summarize(testGroup)
averageMidwestMedianSalary <- subset(mergedDF[ , c(1 , 18, 19)], Region == "Midwestern")
averageNonMidwestMedianSalary <- subset(mergedDF[ , c(1 , 18, 19)], Region != "Midwestern")
t.test <- t.test(averageMidwestMedianSalary$StartingMedianSalary, averageNonMidwestMedianSalary$StartingMedianSalary, var.equal = FALSE)  
#Interpretation: p > 0.05, thus we fail to reject
#the null hypothesis that the starting median salaries are the same.
#There is statistical evidence that the starting median salaries are similar.
  
  
  
  
  