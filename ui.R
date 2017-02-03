library(shiny)
# library(ROCR)
# library(MASS)
# library(e1071)
# library(randomForest)
# library(XLConnect)
#library(DT)

shinyUI(navbarPage(
  #title=div(img(src="myLogo.gif"), "Biocomb"),
  title="Biocomb",                 
  tabPanel("Data Input",
           
        sidebarPanel(
		   h5("Data Input"),
	       fileInput("File","Choose Excel- or CSV-File", accept=c(".xls",".xlsx",".csv")),
	       uiOutput("Sheet.name"),#######for EXCEl-FILE
	       uiOutput("Separator"),
		   br(),
		   uiOutput("Train.items"),####Training items: people deselect any items for Data-Training
		   uiOutput("All.train.items"),
		   uiOutput("Class.item"),####maybe the last column is not for classification. We can define any item for classification
		   uiOutput("Nominal.attr"),###select nominal attribute
		   uiOutput("Check.missing"),
		   uiOutput("Missing.method"),####Training items: people select any items for Data-Training
		   uiOutput("Missing.del"),####Training items: people select any items for Data-Training
		   uiOutput("Action.Process.missing"),
		   
		   conditionalPanel(condition="$('html').hasClass('shiny-busy')", img(src="http://loadinggif.com/images/image-selection/27.gif"))
		),
		mainPanel(dataTableOutput("dataset"))),###Data input and positive class to chose
   navbarMenu("Analysis",
      tabPanel("Area Under the ROC Curve",
	        sidebarPanel(
			 ########AUC-value
			 uiOutput("Title.auc"),#####Statisticd about AUC
			     textOutput("Warn.miss"),
	         uiOutput("Size.sample"),###default 100
	         uiOutput("Prop.sick"),###0 to n-1
	         textOutput("Warn.prop.sick"),#### if proportion of sick is bigger than 1, then it should be smaller than size of sample.
	         uiOutput("Type.pauc"),###p-value or logarithmic p-value
	         uiOutput("Action.Compute.aucs"),####as.list(colnames(xdata)[-ncol(xdata)])

	         ########ROC-curve with AUC?
	         tags$hr(),
	         uiOutput("Attr.roc"),
	         uiOutput("All.attr.roc"),
	         uiOutput("Add.legend"),
	         uiOutput("Is.percent"),
	         uiOutput("Include.auc"),
	         uiOutput("Action.roc.curve"),

	         #########AUC.P-value
	         tags$hr(),
	         uiOutput("Repetition.pval"),
	         textOutput("Warn.rep.pval"),####positive integer
	         uiOutput("Compute.auc.pval.method"),###c("permutation","random")
	         uiOutput("Correction.auc.pval.art"),###c("none","bonferroniholm","bonferroni")
	         uiOutput("Action.auc.pval"),
			 
			     conditionalPanel(condition="$('html').hasClass('shiny-busy')", img(src="http://loadinggif.com/images/image-selection/27.gif"))

			),
			mainPanel(
			    tabsetPanel(id ="tabP_ROC",
			                
				     tabPanel("AUC-values", uiOutput("header.res.aucs"),####downloadButton("down.res.aucs","Download")
		                                    dataTableOutput("res.aucs"),
										    uiOutput("Action.pauclog.curve"),
											plotOutput("curve.pauclog")),	####log pauclog curve
		             tabPanel("ROC-curves", uiOutput("header.plot.roc"),####downloadButton("down.plot.roc","Download")
				               			    plotOutput("plot.roc")),
                     tabPanel("Significance",uiOutput("header.auc.sign"),
		                                     dataTableOutput("res.auc.sign"))
			)

			)),
			tabPanel("Hypervolume Under the Manifold",
			sidebarPanel(

			  uiOutput("choose_feature"),

			  uiOutput("choose_label"),

			  checkboxInput("ex", "Exhaustive search", FALSE),
			  uiOutput("choose_amount_label"),


			  actionButton("goButton", "Go HUM!"),
			  conditionalPanel(condition="$('html').hasClass('shiny-busy')", img(src="http://loadinggif.com/images/image-selection/27.gif"))



			  ),
			mainPanel(
			  tabsetPanel(id ="tabs1",
			              tabPanel(title="Result",

			                       mainPanel(


			                         uiOutput("VarSelect"),

			                         uiOutput("go_barchart"),
			                         br(),
			                         uiOutput("ROCselect"),


			                         uiOutput("ROCbutton"),
			                         textOutput("text"),
			                         br(),
			                         tableOutput("table2")



			                       )
			              ),
			              #tabPanel("Result", tableOutput("table2")),
			              tabPanel("ROC curve", plotOutput("plot1",height=672*1,width=672*1)),
			              tabPanel("Bar Chart", plotOutput("plot2",height=672*1,width=672*1))
			  )
			)
   ),

	  tabPanel("Relative Cost Curves",
	        sidebarPanel(
			  #############Plot RCC######## n?
			  uiOutput("Pos.Class"),### positive class, will be signed in RCC as 0. c("",levels(file[,ncol(file)]))
			  uiOutput("Attr.rcc"),
			  uiOutput("All.attr.rcc"),
			  uiOutput("Attr.x.llim.rcc"),###low limit in x-axis
			  uiOutput("Attr.x.ulim.rcc"),###up limit in x-axis
			  uiOutput("Attr.y.llim.rcc"),####low limit in y-axis
			  uiOutput("Attr.y.ulim.rcc"),####up limit in y -axis
			  textOutput("Warn.llim.ulim.rcc"),###up  limit> low limit
			 # uiOutput("Add.medians.rcc"),
			  uiOutput("Add.legend.rcc"),
			  uiOutput("Action.rcc.curve")
			),
			mainPanel(
			  tabsetPanel(id="tabP_RCC",
			      tabPanel("RCC-curves",uiOutput("header.plot.rcc"),
				                        plotOutput("plot.rcc"))
			    ))
            ),

			#new tabPage for Feature Selection and Cross-validation
			tabPanel("Selection and Classification",
			         sidebarPanel(
			           ########Feature selection
			           uiOutput("Title.feature"),#####feature selecton
			           uiOutput("Type.sel"),#####Feature selecton (ranking or subset)
			           uiOutput("Method.sel"),#####Feature selecton (method)
			           #Elements for information gain feature selection
			           uiOutput("Disc.method"),###default MDL

			           uiOutput("Threshold.class"),###threshold for mutual information with class
			           textOutput("Warn.threshold"), ###Between 0 and 1
			           uiOutput("Threshold.consis"),###threshold for Chi2-algorithm
			           uiOutput("Action.Compute.subset"),
			           conditionalPanel(condition="$('html').hasClass('shiny-busy')", img(src="http://loadinggif.com/images/image-selection/27.gif")),  
			           ########Cross-validation or bootstrapping
			           tags$hr(),
			           uiOutput("Title.cross"),
			           uiOutput("Include.features"),
			           uiOutput("Type.classifier"),#Nearest neighbor, Naive Bayes, Shrunken Centroids
			           uiOutput("Type.cross"), #cross-validation or bootstrapping or leaveoneout
			           uiOutput("Number.feature"),
			           textOutput("Warn.number.feat"),###positive integer
			           uiOutput("Action.cross"),

			           conditionalPanel(condition="$('html').hasClass('shiny-busy')", img(src="http://loadinggif.com/images/image-selection/27.gif"))
			         ),
			         mainPanel(
			           tabsetPanel(id ="tabP_Feature",
			                       tabPanel("Feature_select", uiOutput("header.sel.feature"),####downloadButton("down.sel.feature","Download")
			                                textOutput("Warn.sel.feat"),###wrong method,
			                                dataTableOutput("sel.feature")),
			                       tabPanel("Cross_results", uiOutput("header.plot.results"), #download
			                                uiOutput("header.cross.results"),
			                                uiOutput("Action.true.class"),
			                                tableOutput("res.true.class"),
			                                plotOutput("cross.plot")),####plot
			                       tabPanel("Class_results",uiOutput("header.class.result"),
			                                dataTableOutput("res.class.result"),
			                                uiOutput("Class.confusion"),####as.list(input$classifiers)
			                                uiOutput("Action.confusion.class"),
			                                uiOutput("header.confusion.class"),
			                                tableOutput("res.confusion.class")),
			                       tabPanel("Feature_after_cross",uiOutput("Title.after"), uiOutput("header.after.result"),
			                                dataTableOutput("res.after.result"))

			           )

			         ))


	)
))
