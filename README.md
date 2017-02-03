# Biocomb
The Biocomb software tool includes the Biocomb R package and a web application with a user-friendly and flexible web interface for accessing the package’s main functions and visualizing the results. The Biocomb R package functions include the number of standard and original data analysis methods, used for biomedical research. The feature selection methods help to mark out the informative features, which serve as biomarkers for disease diagnostics and to conduct the statistical significance analysis. The classification models with several embedded validation schemes provide the possibility to estimate both the feature informativeness and the classifier prediction power. The Biocomb R package can be installed from the CRAN site at http://cran.r-project.org/web/packages/Biocomb/.
The Biocomb repository includes a Biocomb web application, containing a user-interface definition in the “ui.R” file and a server script in the “server.R” file together with the list of files, which contain the functionality of the application and make up the Biocomb R package. The Biocomb web application is created using the Shiny R package and provides a user-friendly graphical interface with visualization of results. 

# Installation
First, download and install R:

https://cran.rstudio.com

Second, install following R packages:

install.packages("shiny")

install.packages("MASS")

install.packages("e1071")

install.packages("randomForest")

install.packages("XLConnect")
install.packages("pROC")
install.packages("ROCR")
install.packages("RWeka")
install.packages("FSelector")
install.packages("arules")
install.packages("pamr")
install.packages("class")
install.packages("nnet")
install.packages("rpart")

Finally, run following code in R console:

shiny::runGitHub("Biocomb", "novosel65")


# License

Biocomb web application as a whole is distributed under GPL-3 (General Public License Version 3).

