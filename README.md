# Data Analysis -Traffic-Accidents

## **Introduction**

This project has a database on traffic accidents in Brazil, which includes information such as date, weekday, state, cause, number of accidents, injured, and fatalities, among other relevant details. The database consists of a total of 64,548 records, collected by the Federal Highway Police (PRF), specifically from the year 2022. It was published on 08/05/2020 at 3:59 PM and last updated on 03/21/2023 at 8:13 PM.

## Objective

The objective of this project is to conduct an analysis to identify the main causes of traffic accidents and the factors that contribute to their occurrence. This information is crucial to assist in decision-making and the implementation of preventive measures. Using the R language, graphs and tables were created to provide a detailed view of the accident characteristics. This analysis enables us to obtain important and valuable information, allowing for a deeper understanding of the accidents and aiding in the search for effective solutions.

## Details

- In this repository, we have 3 files: the R code used for data analysis, a PDF document discussing and presenting the analysis results, and a slide presentation with the results.
- The code includes all the necessary packages for code construction and graph generation. Please ensure that you have installed all the required packages to run the program in your IDE.
- In the code, there is a line that reads the data from an Excel file. Please replace the file path there with the path to the file on your computer:

```r
data <- read_excel("C:/Users/Rafael - Notebook/Desktop/Arquivos/Trabalhos ceub/Trabalho R/acidentes_rodovias_2022.xlsx")
#Change here
```
