library(MASS)
library(e1071)
library(randomForest)
library(XLConnect)
library(pROC)
library(ROCR)

#new
library(RWeka)
library(FSelector)
library(arules)
library(pamr)
library(class)
library(nnet)
library(rpart)

library(rgl)
library(gtools)
library(Rcpp)


#setwd("C:/Natalia/Article_Klawonn_2010/Biomarker/Biomarker_shiny1")#####please give you the shiny's data-path in your Computer
source("pauc.R")
source("plotRoc.curves.R")
source("compute.auc.pvalues.R")
source("rcc.aac.R")

#new
source("select.feature.info.R")
source("class_cross.R")
source("input_miss.R")
source("plotClass.result.R")

#new
source("CalculateHUM_seq.R")
source("CalculateHUM_ROC.R")
source("ROC_Plot.R")
source("CalculateHUM_Ex.R")
sourceCpp("HUMseq.cpp")
sourceCpp("CHI2.cpp")
sourceCpp("CorrF.cpp")

seq=NULL
HUM=NULL

shinyServer(function(input,output,session){

MaxClass=10
open3d()


Sub.filename<-function(filename){
  if(grepl(".xls",filename)){
    name<-sub(".xls","",filename)
  }
  
  if(grepl(".xlsx",filename)){
    name<-sub(".xlsx","",filename)
  }
  
  if(grepl(".csv",filename)){
    name<-sub(".csv","",filename)
  }
  
  return(name)
}

Tipp<-function(dat,feature){
  Judge<-0
  for(i in 1:length(feature))
  {
    index=which(names(dat)==feature[[i]])
    if(!is.numeric(dat[,index]))
    {
      if(!is.factor(dat[,index]))
      {
      Judge<-1
      break
      }
      else
      {
        Judge<-2 
      }
    }
  }
  Judge
}

do.numeric<-function(char){

  num<-NULL

  if(grepl("^\\d+\\,\\d+$", char)){
    num<-as.numeric(sub(",",".",char))
  }

  if(grepl("^\\d+\\/\\d+$", char)){
    str<-unlist(strsplit(char,"[ /]"))
    num<-as.numeric(str[1])/as.numeric(str[2])
  }

  if(grepl("^\\d+$", char)|grepl("^\\d+\\.\\d+$",char)){
    num<-as.numeric(char)
  }

  return(num)
}

char.to.numeric<-function(char){

  if(substring(char,1,1)=="-"){
    char<-substring(char,2)
    num<-do.numeric(char)
    num<-num*(-1)
  }else{
    num<-do.numeric(char)
  }

  return(num)
}

compute.confusion.matrix <- function(true.classes,predicted.classes){
  return(table(predicted.classes,true.classes))
}

observe({
     if(is.null(input$action.compute.aucs))return()
	 if(input$action.compute.aucs==0)return()

     isolate({
	    updateTabsetPanel(session, "tabP_ROC", selected="AUC-values")#TabPanel
	})
})


observe({
     if(is.null(input$action.roc.curve))return()
	 if(input$action.roc.curve==0)return()

	 isolate({
	    updateTabsetPanel(session, "tabP_ROC", selected="ROC-curves")
	 })
})

observe({
    if(is.null(input$action.auc.pval))return()
	if(input$action.auc.pval==0)return()

	isolate({
	    updateTabsetPanel(session, "tabP_ROC", selected="Significance")
	})
})

#new lines
observe({
  if(is.null(input$action.compute.subset))return()
  if(input$action.compute.subset==0)return()

  isolate({
    updateTabsetPanel(session, "tabP_Feature", selected="Feature_select")#TabPanel
  })
})

observe({
  if(is.null(input$action.cross))return()
  if(input$action.cross==0)return()

  isolate({
    updateTabsetPanel(session, "tabP_Feature", selected="Class_results")#TabPanel
  })
})

observe({
  if(is.null(input$action.cross))return()
  if(input$action.cross==0)return()

  isolate({
    updateTabsetPanel(session, "tabP_Feature", selected="Feature_after_cross")#TabPanel
  })
})

observe({
  if(is.null(input$action.cross))return()
  if(input$action.cross==0)return()

  isolate({
    updateTabsetPanel(session, "tabP_Feature", selected="Cross_results")#TabPanel
  })
})
##
observe({

  if (is.null(input$ROCButton))
    return(NULL)
  if (input$ROCButton == 0)
    return(NULL)


  isolate({

    updateTabsetPanel(session, "tabs1", selected="ROC curve")

  })
})

observe({

  if (is.null(input$barButton))
    return(NULL)
  if (input$barButton == 0)
    return(NULL)

  isolate({

    updateTabsetPanel(session, "tabs1", selected="Bar Chart")

  })
})

observe({

  if (is.null(input$goButton))
    return(NULL)
  if (input$goButton == 0)
    return(NULL)

  isolate({

    updateTabsetPanel(session, "tabs1", selected="Result")

  })
})

######Date Input
datei<-reactive({
  inFile <- input$File
  if (is.null(inFile))return()
  dattab<-NULL

    if(grepl(".xls",inFile$name)|grepl(".xlsx",inFile$name)){
    wb<-loadWorkbook(inFile$datapath)
    sheetname<-input$sheet.name
    if(is.null(sheetname))return()
    if(sheetname=="NA")return()
    sheets <- getSheets(wb)
    if(!sheetname%in%sheets) return()
    try(lastcol<-getLastColumn(wb,sheet=sheetname),silent=T)
    if(lastcol<2)return()

    dattab<-readWorksheet(wb,sheet=sheetname)
  }

  if(grepl(".csv",inFile$name)){
    sep<-input$separator
    if(is.null(sep))return()
    if(sep==";"){
      dattab<-read.csv2(inFile$datapath)
    }

    if(sep==","){
      dattab<-read.csv(inFile$datapath)
    }
  }

  if(!is.null(dattab)){
    data.frame(dattab)
  }
})

teil.datei<-reactive({

  if(is.null(datei()))return()
  if(is.null(input$train.items))return()
  if(is.null(input$class.item))return()
  if(input$class.item=="NA")return()


  xdata<-datei()
  items<-input$train.items
  num.items<-1:ncol(xdata)
  
  ind<-numeric(length(items))
  for(i in 1:length(items)){
    vv<-which(colnames(xdata) %in% items[i])
    if(length(vv)==0) return()
    ind[i]<-vv
  }
  num.items=num.items[-ind]

  item.class<-input$class.item
  num.items<-c(num.items,which(colnames(xdata) %in% item.class))

  xdata<-xdata[,num.items]
  xdata[,ncol(xdata)]<-as.factor(xdata[,ncol(xdata)])
  

  data.frame(xdata)
})

#dataset preprocessing missing values
miss.datei<-reactive({

  if(is.null(input$action.process.missing)) return(teil.datei())
  if(input$action.process.missing==0)  return(teil.datei())


#   if(is.null(datei()))return()
#   if(is.null(teil.datei()))return()
  #if(is.null(input$check.missing)) return()
  isolate({
  xdata<-teil.datei()

  attrs.no<-numeric()
  if(!is.null(input$nominal.attr))
  {
    nom.attr<-input$nominal.attr

    for(i in 1:length(nom.attr)){
      attrs<-grep(nom.attr[i],colnames(xdata),fixed=T)[1]
      if(!is.na(attrs))
      {
        xdata[,attrs]<-as.factor(xdata[,attrs])
        attrs.no=c(attrs.no,attrs)
      }
    }
  }

  if(input$check.missing)
  {
    if(!is.null(input$missing.method))
    {
      m.method=input$missing.method

      switch(m.method,
             #there is the error when the number of features=1
             del = {


             },
             mean.value={
               out=input_miss(xdata,"mean.value",attrs.no,input$missing.del)
               if(out$flag.miss)
               {
                 xdata=out$data
               }
             },
             near.value={
               out=input_miss(xdata,"near.value",attrs.no,input$missing.del)

               if(out$flag.miss)
               {
                 xdata=out$data
               }
             }
      )
    }
  }
  data.frame(xdata)
  })
})

  output$Sheet.name<-renderUI({
  inFile <- input$File
  if (is.null(inFile))return()

  if(grepl(".xls",inFile$name)|grepl(".xlsx",inFile$name)){
    wb<-loadWorkbook(inFile$datapath)
    selectInput("sheet.name","Sheet:",as.list(c(NA,getSheets(wb))))
  }else{return()}
  })


	output$Separator<-renderUI({
	 inFile <- input$File
     if (is.null(inFile))return()

      if(grepl(".csv",inFile$name)){
		   selectInput("separator","The separator",as.list(c(";",",")))
		 }
	})
	###########Excel-file Input and show in the "Dataset"
	output$dataset<-renderDataTable({
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  xdata<-miss.datei()
	  data.frame(xdata)
	},options=list(page='enable',height=800,width=800))

	output$Train.items<-renderUI({
	  if(is.null(datei()))return()

	  xdata<-datei()
	  selectInput("train.items","Exclude Features from training",as.list(c(colnames(xdata))),multiple =T,if(!is.null(input$all.train.items)){
	    if(input$all.train.items)
	    {selected=as.list(c(colnames(xdata)))}})
	})

	output$All.train.items<-renderUI({
	  if(is.null(datei()))return()

	  checkboxInput("all.train.items","All Features",FALSE)
	})


	output$Class.item<-renderUI({
	  if(is.null(datei()))return()

	  xdata<-datei()
	  item.class<-colnames(xdata)
	  if(!is.null(input$train.items)){
	    item.class=input$train.items
	  }
	  selectInput("class.item","Classification Feature",as.list(c(NA,item.class)))
	})

	#################Compute AUC#############
	#missing values
	output$Nominal.attr<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()

	  isolate({
	    xdata<-teil.datei()
	    attrs<-colnames(xdata)[-ncol(xdata)]
	  })
	  selectInput("nominal.attr","The nominal attributes to exclude",as.list(attrs),multiple=T)
	})



	output$Check.missing<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  checkboxInput("check.missing","Preprocess missing values",FALSE)
	})

	output$Missing.method<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(input$check.missing)) return()
	  if(input$check.missing==FALSE)
	  {
	    return()
	  }

	  isolate({
	    selectInput("missing.method","Method of missing values preprocessing",as.list(c("Mean value"="mean.value","Nearest neighbors method"="near.value")),selected="mean.value")
	  })
	})

	output$Missing.del<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(input$check.missing)) return()
	  if(input$check.missing==FALSE) return()

	  sliderInput("missing.del", "Threshold for delete of missing values (%)", min=0, max=100, value =20,ticks = TRUE)
	})

	output$Action.Process.missing<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
# 	  if(is.null(input$check.missing)) return()
# 	  if(input$check.missing==FALSE) return()

	  isolate({
	    actionButton("action.process.missing","Process")
	  })
	})

	output$Title.auc<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		 h4("AUC Statistics")
	})

	output$Warn.miss<-renderText({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  
	  if(any(is.na(miss.datei()))) 
	  {
	    text<-"Missing values must be handled"
	  }
	  else
	  {
	    xdata=miss.datei()
	    judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	    
	    if(judge==1)
	    {
	      text<-"Some features are not numerical. Define them as nominal!"
	    }
	  }
	})
	
	
	output$Size.sample<-renderUI({
	    if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		textInput("size.sample", "Size of samples", value = "100")
	})

	output$Prop.sick<-renderUI({
	   if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	   textInput("prop.sick","Proportion of sick",value="0.5")
	})

	output$Warn.prop.sick<-renderText({
	    if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		if(is.null(input$size.sample))return()
		if(input$size.sample=="")return()
		if(is.null(input$prop.sick))return()
		if(input$prop.sick=="")return()

		num.prop<-char.to.numeric(input$prop.sick)
		num.size<-char.to.numeric(input$size.sample)
		if(is.null(num.prop)||is.null(num.size)) return()
		if(num.prop>1 && num.prop>=num.size){
		   text<-"Proportion of sick should be smaller than size of samples."
		}
	})


	output$Type.pauc<-renderUI({
	   if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	   if(is.null(input$size.sample))return()
	   if(input$size.sample=="")return()
	   if(is.null(input$prop.sick))return()
	   if(input$prop.sick=="")return()

		num.prop<-char.to.numeric(input$prop.sick)
		num.size<-char.to.numeric(input$size.sample)
		if(is.null(num.prop)||is.null(num.size)) return()
		if(num.prop<num.size){
		   #selectInput("type.pauc","p-value or logarithmic r-value",as.list(c(NA,"p-value","logarithmic p-value")),selected=c("p-value","logarithmic p-value"),multiple =T)
		  selectInput("type.pauc","p-value or logarithmic r-value",as.list(c(NA,"p-value","logarithmic p-value")),selected=NA,multiple =T)
		}
	})


	output$Action.Compute.aucs<-renderUI({
	    if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  
	  if(any(is.na(miss.datei()))) return()
	  
	  xdata=miss.datei()
	  judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	  
	  if(judge==1) return()
	  
		if(is.null(input$size.sample))return()
		if(input$size.sample=="")return()
		if(is.null(input$prop.sick))return()
		if(input$prop.sick=="")return()
	  
		num.prop<-char.to.numeric(input$prop.sick)
		num.size<-char.to.numeric(input$size.sample)
	    if(is.null(num.prop)||is.null(num.size)) return()
		if(num.prop<num.size){
		   actionButton("action.compute.aucs","AUC values")
		}
	})


	output$header.res.aucs<-renderUI({

		  if(is.null(input$action.compute.aucs))return()
		  if(input$action.compute.aucs==0)return()

		  isolate({
		     h3("AUC-Values",downloadButton("down.res.aucs","Download"))
		  })
	})


	output$res.aucs<-renderDataTable({

		  if(is.null(input$size.sample))return()
		  if(input$size.sample=="")return()
		  if(is.null(input$prop.sick))return()
		  if(input$prop.sick=="")return()
		  if(length(char.to.numeric(input$prop.sick))==0|length(char.to.numeric(input$size.sample))==0)
		    return()
	    
		  if(is.null(input$action.compute.aucs))return()
      if(input$action.compute.aucs==0)return()

	    #can not deal with nominal attributes
		  isolate({
		    if(is.null(input$type.pauc))return()
		     xdata<-miss.datei()

		     attrs.no=numeric()

		     if(!is.null(input$nominal.attr))
		     {
		       nom.attr<-input$nominal.attr
		       attrs.no<-numeric()
		       for(i in 1:length(nom.attr)){
		         attrs<-grep(nom.attr[i],colnames(xdata),fixed=T)[1]
		         if(!is.na(attrs))
		         {
		           xdata[,attrs]<-as.factor(xdata[,attrs])
		           attrs.no=c(attrs.no,attrs)
		         }
		       }
		     }

			 num.prop<-char.to.numeric(input$prop.sick)
		     num.size<-char.to.numeric(input$size.sample)
			 type<-input$type.pauc
			 paucv<-c()
			 pauclogv<-c()
			 #how much classes
			 val=levels(xdata[,ncol(xdata)])

			 if(length(attrs.no)>0)
			 {
			  auc.val<-compute.aucs(xdata[,-attrs.no,drop=FALSE])
			 }
			 else
			 {
			   auc.val<-compute.aucs(xdata)
			 }
			 name.aucv<-names(auc.val)

             if(length(type)>1||!type=="NA"){
               
			      vauc<-auc.val[,2]
				  judge.pv<-FALSE
				  judge.pv<-(length(grep("p-value",type))==2)|(length(grep("p-value",type))==1&length(grep("logarithmic",type))==0)

				 if(judge.pv){
				   if(length(val)==2)
				   {
				     pos=auc.val[,3]
				   }
				   else
				   {
				     pos=numeric()
				   }
					  paucv<-pauc(vauc,num.size,num.prop,xdata[,ncol(xdata)],pos)
			      }

				  if(length(grep("logarithmic p-value",type))==1){
				    if(length(val)==2)
				    {
				      pos=auc.val[,3]
				    }
				    else
				    {
				      pos=numeric()
				    }
					  pauclogv<-pauclog(vauc,num.size,num.prop,xdata[,ncol(xdata)],pos)
			      }

				  if(!is.null(paucv)){
				    auc.val<-data.frame(auc.val,paucv)
					name.aucv<-c(name.aucv,"P-values")
				  }

				  if(!is.null(pauclogv)){
				    auc.val<-data.frame(auc.val,pauclogv)
	                name.aucv<-c(name.aucv,"Logarithmic p-values")
				  }
			    }

			 names(auc.val)<-name.aucv
		     data.frame(auc.val)

		  })
	},options=list(page='enable',height=800,width=800))


	output$down.res.aucs<- downloadHandler(
	   filename=function(){
	      inFile <- input$File
		  title0<-Sub.filename(inFile$name)
		  paste(title0,"_aucs.csv",sep="")
	   },
	   content=function(con){
	     if(is.null(input$type.pauc))return()
	     xdata<-miss.datei()
	     
	     attrs.no=numeric()
	     
	     if(!is.null(input$nominal.attr))
	     {
	       nom.attr<-input$nominal.attr
	       attrs.no<-numeric()
	       for(i in 1:length(nom.attr)){
	         attrs<-grep(nom.attr[i],colnames(xdata),fixed=T)[1]
	         if(!is.na(attrs))
	         {
	           xdata[,attrs]<-as.factor(xdata[,attrs])
	           attrs.no=c(attrs.no,attrs)
	         }
	       }
	     }
	     
	     num.prop<-char.to.numeric(input$prop.sick)
	     num.size<-char.to.numeric(input$size.sample)
	     type<-input$type.pauc
	     paucv<-c()
	     pauclogv<-c()
	     #how much classes
	     val=levels(xdata[,ncol(xdata)])
	     
	     if(length(attrs.no)>0)
	     {
	       auc.val<-compute.aucs(xdata[,-attrs.no,drop=FALSE])
	     }
	     else
	     {
	       auc.val<-compute.aucs(xdata)
	     }
	     name.aucv<-names(auc.val)
	     
	     if(length(type)>1||!type=="NA"){
	       
	       vauc<-auc.val[,2]
	       judge.pv<-FALSE
	       judge.pv<-(length(grep("p-value",type))==2)|(length(grep("p-value",type))==1&length(grep("logarithmic",type))==0)
	       
	       if(judge.pv){
	         if(length(val)==2)
	         {
	           pos=auc.val[,3]
	         }
	         else
	         {
	           pos=numeric()
	         }
	         paucv<-pauc(vauc,num.size,num.prop,xdata[,ncol(xdata)],pos)
	       }
	       
	       if(length(grep("logarithmic p-value",type))==1){
	         if(length(val)==2)
	         {
	           pos=auc.val[,3]
	         }
	         else
	         {
	           pos=numeric()
	         }
	         pauclogv<-pauclog(vauc,num.size,num.prop,xdata[,ncol(xdata)],pos)
	       }
	       
	       if(!is.null(paucv)){
	         auc.val<-data.frame(auc.val,paucv)
	         name.aucv<-c(name.aucv,"P-values")
	       }
	       
	       if(!is.null(pauclogv)){
	         auc.val<-data.frame(auc.val,pauclogv)
	         name.aucv<-c(name.aucv,"Logarithmic p-values")
	       }
	     }
	     
	     names(auc.val)<-name.aucv
	     write.csv2(data.frame(auc.val),con,row.names=F)
	 })
	###########CURVE of AUC and logarithmic p-value####
	output$Action.pauclog.curve<-renderUI({
	    if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  
	  if(any(is.na(miss.datei()))) return()
	  
	  xdata=miss.datei()
	  judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	  
	  if(judge==1) return()

		if(is.null(input$size.sample))return()
		if(input$size.sample=="")return()
		if(is.null(input$prop.sick))return()
		if(input$prop.sick=="")return()
		if(length(char.to.numeric(input$prop.sick))==0|length(char.to.numeric(input$size.sample))==0)
		    return()

	    if(is.null(input$action.compute.aucs))return()
      if(input$action.compute.aucs==0)return()
	    if(is.null(input$type.pauc))return()

		isolate({
		  
		if(length(grep("logarithmic",input$type.pauc))==1){
		   actionButton("action.pauclog.curve","Log p-value curve")
		}
		})
	})


	output$curve.pauclog<-renderPlot({

	  if(is.null(input$action.pauclog.curve))return()
	  if(input$action.pauclog.curve==0)return()

	   isolate({
	     num.prop<-char.to.numeric(input$prop.sick)
		 num.size<-char.to.numeric(input$size.sample)

        pauclogcurve <- function(auc,n=num.size,n.plus=num.prop){
           return(log10(pauc(auc,n=n,n.plus=n.plus,labels=numeric(),pos=character())))
         }
         curve(pauclogcurve,from=0.7,to=0.99,xlab="AUC",ylab="log probability",lwd=3)
	   })
	})


	#######Plot.ROC###################################
	output$Attr.roc<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()

		 isolate({
		   xdata<-miss.datei()
		   attrs<-colnames(xdata)[-ncol(xdata)]
		})
		   selectInput("attr.roc","The attributes for ROC-curves",as.list(attrs),multiple=T,if(!is.null(input$all.attr.roc)){
		                                                                                        if(input$all.attr.roc)
		                                                                                            {selected=as.list(attrs)}})
	})

	output$All.attr.roc<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()

		 isolate({
		     checkboxInput("all.attr.roc","All Attributes",FALSE)
		 })
	})

	output$Add.legend<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()

		 isolate({
		     checkboxInput("add.legend","Add Legends",FALSE)
		 })
	})

	output$Is.percent<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()

		 isolate({
		     checkboxInput("is.percent","Percent",FALSE)
		 })
	})

	output$Include.auc<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()
		 if(is.null(input$add.legend))return()
		 if(!input$add.legend)return()

		 isolate({
		     checkboxInput("include.auc","Show the auc-values",FALSE)
		 })
	})


	output$Action.roc.curve<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  
	  if(any(is.na(miss.datei()))) return()
	  
	  xdata=miss.datei()
	  judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	  
	  if(judge==1) return()

		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()
		 if(is.null(input$attr.roc))return()
		 if(length(input$attr.roc)<1)return()

		 isolate({
		     actionButton("action.roc.curve","ROC curves")
		 })
	})

	output$header.plot.roc<-renderUI({

		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()
		 if(is.null(input$action.roc.curve))return()
		 if(input$action.roc.curve==0)return()

		 isolate({
		     h3("ROC-curves",downloadButton("down.plot.roc","Download"))
		 })
	})

	output$plot.roc<-renderPlot({

		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()
		 if(is.null(input$action.roc.curve))return()
		 if(input$action.roc.curve==0)return()

		 isolate({
		     xdata<-miss.datei()
		     #only for two classes
		     class=xdata[,ncol(xdata)]
		     label=levels(class)
		     if(length(label)>2) return()

			 attrs<-input$attr.roc

			 if(length(attrs)<1)return()

			 attrs.no<-numeric()
			 for(i in 1:length(attrs)){
			     if(!is.factor(xdata[,attrs[i]]))
			     {
			      attrs.no<-c(attrs.no,grep(attrs[i],colnames(xdata),fixed=T)[1])
			     }
			 }
			 if(length(attrs.no)<1)return()

			 add.legend<-input$add.legend
			 is.percent<-input$is.percent
			 include.auc<-F
			 if(add.legend){
			    include.auc<-input$include.auc
			 }

			 roc.curve<-plotRoc.curves(xdata[,c(attrs.no,ncol(xdata))],add.legend=add.legend,include.auc=include.auc,ispercent=is.percent)
		 })
		 print(roc.curve)
		 dev.off()

	},height=700,width=700)

	output$down.plot.roc<-downloadHandler(
	    filename=function(){
	      inFile <- input$File
	      title0<-Sub.filename(inFile$name)
	      all.attrs<-input$all.attr.roc
	      if(!all.attrs){
	        attrs<-input$attr.roc
	        xdata<-miss.datei()
	        title0<-paste(title0,"_col",sep="")
	        for(i in 1:length(attrs)){
	          attrs.no<-grep(attrs[i],colnames(xdata),fixed=T)[1]
	          title0<-paste(title0,"_",attrs.no,sep="")
	        }
	      }else{
	        title0<-paste(title0,"_ALL",sep="")
	      }
	      paste(title0,"_rocs.pdf",sep="")
	    },
	    content=function(file){
	      pdf(file)
	      xdata<-miss.datei()
	      #only for two classes
	      class=xdata[,ncol(xdata)]
	      label=levels(class)
	      if(length(label)>2)
	      {
	        dev.off()
	        return()
	      }

	      attrs<-input$attr.roc

	      if(length(attrs)<1){
	        dev.off()
	        return()
	      }
	      attrs.no<-numeric()
	      for(i in 1:length(attrs)){
	        if(!is.factor(xdata[,attrs[i]]))
	        {
	          attrs.no<-c(attrs.no,grep(attrs[i],colnames(xdata),fixed=T)[1])
	        }
	      }
	      if(length(attrs.no)<1)
	      {
	        dev.off()
	        return()
	      }

	      add.legend<-input$add.legend
	      is.percent<-input$is.percent
	      include.auc<-F
	      if(add.legend){
	        include.auc<-input$include.auc
	      }

	      roc.curve<-plotRoc.curves(xdata[,c(attrs.no,ncol(xdata))],add.legend=add.legend,include.auc=include.auc,ispercent=is.percent)

	      dev.off()

	    }
	)


	#################Significance
	output$Repetition.pval<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	    if(is.null(input$size.sample))return()
		if(input$size.sample=="")return()
		if(is.null(input$prop.sick))return()
		if(input$prop.sick=="")return()
		if(length(char.to.numeric(input$prop.sick))==0|length(char.to.numeric(input$size.sample))==0)
		    return()
		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()

		 isolate({
		    textInput("repetition.pval","Repetition",value="1000")
		 })
	})

	output$Warn.rep.pval<-renderText({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		if(is.null(input$size.sample))return()
		if(input$size.sample=="")return()
		if(is.null(input$prop.sick))return()
		if(input$prop.sick=="")return()
		if(length(char.to.numeric(input$prop.sick))==0|length(char.to.numeric(input$size.sample))==0)
		    return()
		 if(is.null(input$repetition.pval))return()
		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()

		 rep.val<-input$repetition.pval
		 if(rep.val==""){
		    text<-"Input a positive integer."
		 }else{
		    if((!grepl("^\\d+$", rep.val))|grepl("-", rep.val)){
		       text<-"It should be a positive integer."}else{
		       rep.num<-as.numeric(rep.val)
		      if(rep.num>100){text<-"It takes a long Time."}
            }
		}
	})

	output$Compute.auc.pval.method<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()

		 isolate({
		    selectInput("compute.auc.pval.method","Method of Significance",as.list(c("permutation","random")))
		 })
	})

	output$Correction.auc.pval.art<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()
		 if(is.null(input$compute.auc.pval.method))return()
		 if(input$compute.auc.pval.method=="random"){
			    selectInput("correction.auc.pval.art","Multiple test",as.list(c("none","bonferroniholm","bonferroni")))
		 }
	})

	output$Action.auc.pval<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  
	  if(any(is.na(miss.datei()))) return()
	  xdata=miss.datei()
	  judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	  
	  if(judge==1) return()
	  
		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()

		 if(is.null(input$repetition.pval))return()
		 rep.val<-input$repetition.pval
		 if((!grepl("^\\d+$", rep.val))|grepl("-", rep.val))return()

         isolate({
		     actionButton("action.auc.pval","Significance for AUC-values")
		 })
	})

	output$header.auc.sign<-renderUI({

		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()
		 if(is.null(input$action.auc.pval))return()
		 if(input$action.auc.pval==0)return()

		 isolate({
		     h3("Significance",downloadButton("down.auc.pval","Download"))
		 })
	})

	function.sign <- reactive ({

	  xdata<-miss.datei()

	  attrs.no=numeric()

	  if(!is.null(input$nominal.attr))
	  {
	    nom.attr<-input$nominal.attr
	    attrs.no<-numeric()
	    for(i in 1:length(nom.attr)){
	      attrs<-grep(nom.attr[i],colnames(xdata),fixed=T)[1]
	      if(!is.na(attrs))
	      {
	        xdata[,attrs]<-as.factor(xdata[,attrs])
	        attrs.no=c(attrs.no,attrs)
	      }
	    }
	  }

	  index.auc=setdiff(1:ncol(xdata),attrs.no)
	  auc.val<-compute.aucs(xdata[,index.auc,drop=FALSE])



	  rep.char<-input$repetition.pval
	  if((!grepl("^\\d+$", rep.char))|grepl("-", rep.char))return()
	  rep.num<-as.numeric(rep.char)

	  meth.auc.pval<-input$compute.auc.pval.method
	  if(meth.auc.pval=="permutation"){
	    auc.pvals<-compute.auc.permutation(auc.val[,2],xdata[,index.auc,drop=FALSE],repetitions=rep.num)
	  }

	  if(meth.auc.pval=="random"){
	    cors<-input$correction.auc.pval.art
	    auc.pvals<-compute.auc.random(auc.val[,2],xdata[,index.auc,drop=FALSE],repetitions=rep.num,correction=cors)
	  }

	  auc.pval.tab<-data.frame(colnames(xdata)[-c(attrs.no,ncol(xdata))],auc.val[,2],auc.pvals)
	  colnames(auc.pval.tab)<-c("Biomarker","AUC-value","P-value")
	  return(auc.pval.tab)
	})


	output$res.auc.sign<-renderDataTable({

		 if(is.null(input$action.compute.aucs))return()
		 if(input$action.compute.aucs==0)return()
		 if(is.null(input$action.auc.pval))return()
		 if(input$action.auc.pval==0)return()

		 isolate({

		   auc.pval.tab=function.sign()

		     data.frame(auc.pval.tab)
	    })
	},options=list(lengthMenu = c(10, 25, 50,100)))

    output$down.auc.pval<-downloadHandler(
	   filename=function(){
	      inFile <- input$File
		  title0<-Sub.filename(inFile$name)
		  meth.auc.pval<-input$compute.auc.pval.method
		  title0<-paste(title0,"_",meth.auc.pval,sep="")
		   if(meth.auc.pval=="random"){
		     cors<-input$correction.auc.pval.art
			 title0<-paste(title0,"_",cors,sep="")
		   }
		   paste(title0,"_pval.csv",sep="")
	   },
	    content=function(con){
	      auc.pval.tab=function.sign()
		    write.csv2(data.frame(auc.pval.tab),con,row.names=F)
	    }
	)

 ##########################HUM and ROC#######################################
	var_label <- reactive({
	  input$goButton
	  isolate({
	    var=input$label
	  })
	  var
	})

	var_ex <- reactive({
	  input$goButton
	  isolate({
	    var=input$ex
	  })
	  var
	})

	var_amount <- reactive({
	  input$goButton
	  isolate({
	    var=input$amount_label
	  })
	  var
	})

	var_feature <- reactive({
	  input$goButton
	  isolate({
	    var=input$feature
	  })
	  var
	})


	output$choose_feature <- renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  selectInput("feature", "Feature", as.list(names(miss.datei())), selected=NULL,multiple = TRUE)
	})

	output$choose_label <- renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  if(input$ex==FALSE)
	  {
	    xdata=miss.datei()
	    class<-xdata[,ncol(xdata)]

	    label<-levels(class)

	    if((length(label)>1)&&(length(label)<=MaxClass))
	    {
	      selectInput("label", "Label", as.list(label), selected=NULL, multiple = TRUE)
	    }
	  }
	  else
	  {
	    return()
	  }

	} )

	output$choose_amount_label<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  if(input$ex==FALSE)
	  {return()}
	  else{
	    xdata=miss.datei()
	    class<-xdata[,ncol(xdata)]

	    label<-levels(class)
	    if((length(label)>1)&&(length(label)<=MaxClass))
	    {

	      sliderInput("amount_label","Number of labels", min=2,max=length(label),value=2,step=1,ticks = TRUE)

	    }
	  }
	})

	output$VarSelect <- renderUI({

	  if (input$goButton == 0)
	    return(NULL)
	  isolate({
	    xdata<-miss.datei()
	    if(is.null(input$feature))
	    {
	      return()
	    }
	    else
	    {
# 	      for(i in 1:length(input$feature))
# 	      {
# 	        if(!is.numeric(xdata[,which(names(xdata)==input$feature[[i]])]))
# 	        {
# 	          return()
# 	          break
# 	        }
# 	      }
	    }
	    if(input$ex)
	    {

	      if(is.null(input$amount_label)) return()

	      class<-xdata[,ncol(xdata)]

	      label<-levels(class)

	      if((length(label)<input$amount_label)) return()

	      if(length(label)>1)
	      {
	        res=choose(length(label),input$amount_label)
	        numericInput("var",  "Select labels", 1, min = 1, max = res,
	                     step = 1)
	      }
	    }
	  })
	})

	output$go_barchart<-renderUI({
	  if (input$goButton == 0)
	    return(NULL)

	  isolate({

	    xdata<-miss.datei()
	    if(is.null(input$feature))
	    {
	      return()
	    }
	    else
	    {
# 	      for(i in 1:length(input$feature))
# 	      {
# 	        if(!is.numeric(xdata[,which(names(xdata)==input$feature[[i]])]))
# 	        {
# 	          return()
# 	          break
# 	        }
# 	      }
	    }

	    if(input$ex)
	    {

	      if(is.null(input$amount_label)) return()

	      class<-xdata[,ncol(xdata)]

	      label<-levels(class)

	      if((length(label)<input$amount_label)) return()

	      actionButton("barButton","Bar chart!")
	    }
	    else
	    {

	      if(length(input$label)>1)
	        actionButton("barButton","Bar chart!")
	    }

	  })
	})

	output$ROCselect <- renderUI({

	  	  input$goButton
	  isolate({
	    xdata<-miss.datei()
	    if(is.null(input$feature))
	    {
	      return()
	    }
	    else
	    {
# 	      for(i in 1:length(input$feature))
# 	      {
# 	        if(!is.numeric(xdata[,which(names(xdata)==input$feature[[i]])]))
# 	        {
# 	          return()
# 	          break
# 	        }
# 	      }
	    }
	    inp.f=input$feature
	    attrs.no=inp.f
	    if(!is.null(input$nominal.attr))
	    {
	      attrs.no<-NULL
	      nom.attr<-input$nominal.attr
	      for(i in 1:length(inp.f)){
	        if(!inp.f[i]%in%nom.attr)
	        {
	          attrs.no<-c(attrs.no,inp.f[i])
	        }
	      }
	    }
	    if(length(attrs.no)<1) return()


	    if(input$ex)
	    {
	      if(is.null(input$amount_label)) return()

	      class<-xdata[,ncol(xdata)]

	      label<-levels(class)

	      if((length(label)<input$amount_label)) return()


	      if(input$amount_label==2||input$amount_label==3)
	      {
	        selectInput("select", "Select Variable", as.list(attrs.no), selected=NULL)
	      }
	    }
	    else
	    {
	      if(length(input$label)==2||length(input$label)==3)
	      {
	        selectInput("select", "Select feature", as.list(attrs.no), selected=NULL)
	      }
	    }

	  })
	})

	output$ROCbutton <- renderUI({
	  if (input$goButton == 0)
	    return(NULL)
	  isolate({
	    xdata<-miss.datei()
	    if(is.null(input$feature))
	    {
	      return()
	    }
	    else
	    {
# 	      for(i in 1:length(input$feature))
# 	      {
# 	        if(!is.numeric(xdata[,which(names(xdata)==input$feature[[i]])]))
# 	        {
# 	          return()
# 	          break
# 	        }
# 	      }
	    }
	    if(input$ex)
	    {
	      if(is.null(input$amount_label)) return()

	      class<-xdata[,ncol(xdata)]

	      label<-levels(class)

	      if((length(label)<input$amount_label)) return()


	      if((input$amount_label==2)||(input$amount_label==3))
	        actionButton("ROCButton", "Plot ROC!")
	    }
	    else
	    {
	      if(length(input$label)==2||length(input$label)==3)
	      {
	        actionButton("ROCButton", "Plot ROC!")
	      }
	    }

	  })
	})


	output$text <- renderText({
	  if (input$goButton == 0)
	    return(NULL)
	  
	  isolate({
	    
	    if(any(is.na(miss.datei()))) 
	    {
	      text<-"Missing values must be handled"
	    }
      else
      {
	    xdata<-miss.datei()
	    
	    judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	    
	    if(judge==1)
	    {
	      text<-"Some features are not numerical. Define them as nominal!"
	    }
	    else
	    {
	    if(is.null(input$feature))
	    {
	      text<-"Select feature!"
	    }
	    else
	    {
	      judge<-Tipp(xdata,input$feature)

	      if(judge==1)
	      {
	        text<-"Some inputs are not the feature. Select right features!"
	      }
	      else
	      {
	          if(!input$ex)
	          {
	            class<-xdata[,ncol(xdata)]

	            label<-levels(class)


	            if((length(label)<=1)||(length(label)>MaxClass))
	            {
	              text<-"Class has less as two labels or more than max value. Select another class!"
	            }
	            else
	            {
	              if(length(input$label)<=1)
	              {
	                text<-"Select two or more class labels!"
	              }
	            }
	          }
	          else
	          {
	            if(is.null(input$amount_label))
	            {
	               text<-"The number of labels is not selected"
	            }
	            else
	            {

	              class<-xdata[,ncol(xdata)]

	              label<-levels(class)

	              if(length(label)<input$amount_label)
	              {
	                text<-"The number of labels is less than selected value"
	              }

	            }
	          }
	      }
	    }
	    }
      }
	  })

	})

	#show HUM result
	output$table2 <- renderTable({

	  if (input$goButton == 0)
	    return(NULL)
	  
	  
	  isolate({
	    
	    if(any(is.na(miss.datei()))) return()
	    xdata<-miss.datei()
	    
	    judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	    
	    if(judge==1) return()
	    
	    inp.f=input$feature
	    if(length(inp.f)==0)
	    {
	      return()
	    }
	    else
	    {
	      attrs.no=inp.f
	      if(!is.null(input$nominal.attr))
	      {
	        attrs.no<-NULL
	        nom.attr<-input$nominal.attr
	      for(i in 1:length(inp.f)){
	        if(!inp.f[i]%in%nom.attr)
	        {
	          attrs.no<-c(attrs.no,inp.f[i])
	        }
	      }
	      }
	      if(length(attrs.no)<1) return()

# 	      for(i in 1:length(attrs.no))
# 	      {
# 	        if(!is.numeric(xdata[,which(names(xdata)==attrs.no[i])]))
# 	        {
# 	          return()
# 	          break
# 	        }
# 	      }
	    }

	    if(input$ex==FALSE)
	    {
	      #if(flag_label()==0) return()
	      if(length(input$label)<2)
	      {
	        return()
	      }
	      inputClass=ncol(xdata)
	      out=CalculateHUM_seq(xdata,attrs.no,inputClass,input$label)
	    }
	    else
	    {
	      if(is.null(input$amount_label)) return()
	      class<-xdata[,ncol(xdata)]

	      label<-levels(class)
	      if(input$amount_label>length(label)) return()

	      inputClass=ncol(xdata)
	      out=CalculateHUM_Ex(xdata,attrs.no,inputClass,label,input$amount_label)
	    }


	    HUM<<-out$HUM
	    seq<<-out$seq
	    if(input$ex==FALSE)
	    {
	      HUM_Value=sprintf("%.3f",out$HUM)
	      names(HUM_Value)=attrs.no
	    }
	    else
	    {
	      HUM_Value=out$HUM
	    }
	    data.frame(HUM_Value)
	  })
	})

	#plot bar chart
	output$plot1 <- renderPlot({


	  if (is.null(input$ROCButton))
	    return(NULL)
	  if (input$ROCButton == 0)
	    return(NULL)
	  

	  isolate({
	    
	    if(any(is.na(miss.datei()))) return()
	    xdata<-miss.datei()
	    judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	    
	    if(judge==1) return()

	    if(!var_ex())
	    {
	      sel=input$select

	      if(length(var_label())==2)
	      {
	        out=CalculateHUM_ROC(xdata,sel,ncol(xdata),var_label(),seq)
	        print(CalculateHUM_Plot(sel,out$Sn,out$Sp,out$optSn,out$optSp,HUM))
	      }
	      else
	      {
	        if(length(var_label())==3)
	        {
	          out=CalculateHUM_ROC(xdata,sel,ncol(xdata),var_label(),seq)

	          print(Calculate3D(sel,out$Sn,out$Sp,out$S3,out$optSn,out$optSp,out$optS3,out$thresholds,HUM,var_label()[seq]))
	        }
	        else
	        {
	          return()
	        }
	      }
	    }
	    else
	    {
	      sel=input$select
	      var=input$var

	      if(var>nrow(HUM)) return()

	      HUMout=as.numeric(HUM[var,-(1:var_amount())])
	      names(HUMout)=var_feature()

	      if(var_amount()==2)
	      {
	        vremLabel=HUM[var,1:var_amount()]
	        out=CalculateHUM_ROC(xdata,sel,ncol(xdata),vremLabel,seq[[var]])
	        print(CalculateHUM_Plot(sel,out$Sn,out$Sp,out$optSn,out$optSp,HUMout))

	      }
	      else
	      {
	        if(var_amount()==3)
	        {
	          vremLabel=HUM[var,1:var_amount()]
	          out=CalculateHUM_ROC(xdata,sel,ncol(xdata),vremLabel,seq[[var]])

	          print(Calculate3D(sel,out$Sn,out$Sp,out$S3,out$optSn,out$optSp,out$optS3,out$thresholds,HUMout,as.character(vremLabel)[seq[[var]]]))

	        }
	        else
	        {
	          return()
	        }
	      }

	    }
	  })
	})

	output$plot2 <- renderPlot({
	  if (is.null(input$barButton))
	    return(NULL)
	  if (input$barButton == 0)
	    return(NULL)

	  isolate({
	    if(any(is.na(miss.datei()))) return()
	    
	    xdata<-miss.datei()
	    judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	    
	    if(judge==1) return()
	    
	    if(!var_ex())
	    {
	      if(length(var_label())>1)
	      {
	        barplot(HUM,main="HUM values-Bar Chart")
	        y<-1/factorial(length(var_label()))
	        abline(h=y,col="red",lwd=2)
	        text(1,y,paste("Threshold=",sprintf("%.3f",y),sep=""),col="red",adj = c(0, -.2))
	        dev.off()
	      }
	      else
	      {
	        return()
	      }
	    }
	    else
	    {
	      if(is.null(var_amount())) return()
	      var=input$var
	      if(var>nrow(HUM)) return()
	      out=as.numeric(HUM[var,-(1:var_amount())])
	      names(out)=var_feature()

	      barplot(out,main="HUM values-Bar Chart")
	      y<-1/factorial(var_amount())
	      abline(h=y,col="red",lwd=2)
	      text(1,y,paste("Grenz=",sprintf("%.3f",y),sep=""),col="red",adj = c(0, -.2))
	      dev.off()
	    }
	  })
	})
	##########################RCC and AAC#######################################
	output$Pos.Class<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(input$class.item))return()
	  if(input$class.item=="NA")return()

	  xdata<-datei()
	  item.class<-input$class.item
	  level<-levels(as.factor(xdata[,colnames(xdata)==item.class]))
	  selectInput("pos.class","The positive class",as.list(c(NA,level)))
	})

	output$Attr.rcc<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  isolate({
		   xdata<-miss.datei()
		   attrs<-colnames(xdata)[-ncol(xdata)]
		})
		   selectInput("attr.rcc","The attributes for RCC-curves",as.list(attrs),multiple=T,
						     if(!is.null(input$all.attr.rcc)){
		                           if(input$all.attr.rcc){selected=as.list(attrs)}}
							else{if(!is.null(input$attr.roc)){selected=input$attr.roc}})
	})

	output$All.attr.rcc<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		 isolate({
		     checkboxInput("all.attr.rcc","All Attributes",FALSE)
		 })
	})

    output$Attr.x.llim.rcc<-renderUI({
	    if(is.null(datei()))return()
      if(is.null(teil.datei()))return()
      if(is.null(miss.datei()))return()
		textInput("attr.x.llim.rcc", "Low-limit of x-axis", value = "-1")
	})

	output$Attr.x.ulim.rcc<-renderUI({
	    if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		textInput("attr.x.ulim.rcc","Up-limit of x-axis", value="2")
	})

	output$Attr.y.llim.rcc<-renderUI({
	    if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
		textInput("attr.y.llim.rcc","Low-limit of y-axis", value="30")
	})

	output$Attr.y.ulim.rcc<-renderUI({
	   if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	   textInput("attr.y.ulim.rcc","Up-limit of y-axis", value="100")
	})

	output$Warn.llim.ulim.rcc<-renderText({
	   if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  
	   if(is.null(input$attr.x.llim.rcc))return()
	   if(is.null(input$attr.x.ulim.rcc))return()
	   if(is.null(input$attr.y.llim.rcc))return()
   	   if(is.null(input$attr.y.ulim.rcc))return()

		 xllim<-char.to.numeric(input$attr.x.llim.rcc)
		 xulim<-char.to.numeric(input$attr.x.ulim.rcc)
		 yllim<-char.to.numeric(input$attr.y.llim.rcc)
		 yulim<-char.to.numeric(input$attr.y.ulim.rcc)

	    if(is.null(xllim) | is.null(xulim) | is.null(yllim) | is.null(yulim)){
		    return()
		 }

		 if(length(xllim)==0|length(xulim)==0|length(yllim)==0|yulim==0){
             return()
		 }

		 isolate({
		   
		   if(any(is.na(miss.datei()))) 
		   {
		     text<-"Missing values must be handled"
		   }
		   else
		   {
		     xdata<-miss.datei()
		     judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
		     
		     if(judge==1)
		     {
		       text<-"Some features are not numerical. Define them as nominal!"
		     }
		     else
		     {
		     
		      if(xllim>=xulim){
			     text<-"Up-limit must be larger than low-limit"
			   }

		      if(yllim>=yulim){
			     text<-"Up-limit must be larger than low-limit"
		      }
		     }
		   }
		 })
	})


	output$Add.legend.rcc<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

		 isolate({
		     checkboxInput("add.legend.rcc","Add Legends",FALSE)
		 })
	})


	output$Action.rcc.curve<-renderUI({
	     if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  if(any(is.na(miss.datei()))) return()
	  
	  xdata<-miss.datei()
	  judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	  
	  if(judge==1) return()
	  
		 if(is.null(input$attr.rcc))return()
		 if(length(input$attr.rcc)<1)return()
		 if(is.null(input$attr.x.llim.rcc))return()
		 if(is.null(input$attr.x.ulim.rcc))return()
	     if(is.null(input$attr.y.llim.rcc))return()
		 if(is.null(input$attr.y.ulim.rcc))return()

		 xllim<-char.to.numeric(input$attr.x.llim.rcc)
		 xulim<-char.to.numeric(input$attr.x.ulim.rcc)
		 yllim<-char.to.numeric(input$attr.y.llim.rcc)
		 yulim<-char.to.numeric(input$attr.y.ulim.rcc)

		 if(is.null(xllim) | is.null(xulim) | is.null(yllim) | is.null(yulim)){
		    return()
		 }

		 if(length(xllim)==0|length(xulim)==0|length(yllim)==0|yulim==0){
             return()
		 }

		if(xllim>=xulim ) return()
		if(yllim>=yulim ) return()


		 isolate({
		     actionButton("action.rcc.curve","RCC curves")
		 })
	})

	output$header.plot.rcc<-renderUI({

		 if(is.null(input$action.rcc.curve))return()
		 if(input$action.rcc.curve==0)return()

		 isolate({
		     h3("RCC-curves",downloadButton("down.plot.rcc","Download"))
		 })
	})

	output$plot.rcc<-renderPlot({

		 if(is.null(input$attr.x.llim.rcc))return()
		 if(is.null(input$attr.x.ulim.rcc))return()
	     if(is.null(input$attr.y.llim.rcc))return()
		 if(is.null(input$attr.y.ulim.rcc))return()

		 xllim<-char.to.numeric(input$attr.x.llim.rcc)
		 xulim<-char.to.numeric(input$attr.x.ulim.rcc)
		 yllim<-char.to.numeric(input$attr.y.llim.rcc)
		 yulim<-char.to.numeric(input$attr.y.ulim.rcc)

	     if(is.null(xllim) | is.null(xulim) | is.null(yllim) | is.null(yulim)){
		    return()
		 }

		 if(length(xllim)==0|length(xulim)==0|length(yllim)==0|yulim==0){
             return()
		 }

	     if(is.null(input$action.rcc.curve))return()
		 if(input$action.rcc.curve==0)return()

		 isolate({
		     xdata<-miss.datei()
			 attrs<-input$attr.rcc
			 if(length(attrs)<1)return()

			 attrs.sel=attrs
			 if(!is.null(input$nominal.attr))
			 {
			   attrs.sel<-NULL
			   nom.attr<-input$nominal.attr
			   for(i in 1:length(attrs)){
			     if(!attrs[i]%in%nom.attr)
			     {
			       attrs.sel<-c(attrs.sel,attrs[i])
			     }
			   }
			 }

			 if(length(attrs.sel)<1) return()

			 attrs.no<-rep(0,length(attrs.sel))
			 for(i in 1:length(attrs.sel)){
			     attrs.no[i]<-grep(attrs.sel[i],colnames(xdata),fixed=T)[1]
			 }

		})
		    pos.Class<-input$pos.class

			add.legend<-input$add.legend.rcc

	    aacs<-rep(0,length(attrs))
		color<-c(1:length(attrs))

	    aacs[1] <- cost.curve(xdata, attrs.no[1], pos.Class,col=color[1],add=F,xlim=c(xllim,xulim),ylim=c(yllim,yulim))
		if(length(attrs.sel)>1){
			for(i in 2:length(attrs.sel)){
		      aacs[i]<- cost.curve(xdata, attrs.no[i], pos.Class,col=color[i],add=T,xlim=c(xllim,xulim))
		    }
	    }

		if(add.legend){
	       legt <- colnames(xdata)[attrs.no]
		   for(i in 1:length(attrs.sel)){
			   legt[i] <- paste(legt[i],", AAC=",round(1000*aacs[i])/1000,sep="")
			}
			legend("bottomright",legend=legt,col=color,lwd=2)
		}
	},height=700,width=700)

	output$down.plot.rcc<-downloadHandler(
	    filename=function(){
	      inFile <- input$File
		  title0<-Sub.filename(inFile$name)
		  all.attrs<-input$all.attr.rcc
		  if(!all.attrs){
		     attrs<-input$attr.rcc
			 xdata<-miss.datei()
			 title0<-paste(title0,"_col",sep="")
		     for(i in 1:length(attrs)){
		        attrs.no<-grep(attrs[i],colnames(xdata),fixed=T)[1]
				title0<-paste(title0,"_",attrs.no,sep="")
			   }
		    }else{
			   title0<-paste(title0,"_ALL",sep="")
		    }
		  paste(title0,"_rccs.pdf",sep="")
	    },
	    content=function(file){
	      xllim<-char.to.numeric(input$attr.x.llim.rcc)
	      xulim<-char.to.numeric(input$attr.x.ulim.rcc)
	      yllim<-char.to.numeric(input$attr.y.llim.rcc)
	      yulim<-char.to.numeric(input$attr.y.ulim.rcc)
	      pdf(file)
	      xdata<-miss.datei()
	      attrs<-input$attr.rcc
	      if(length(attrs)<1)
	      {
	        dev.off()
	        return()
	      }

	      attrs.sel=attrs
	      if(!is.null(input$nominal.attr))
	      {
	        attrs.sel<-NULL
	        nom.attr<-input$nominal.attr
	        for(i in 1:length(attrs)){
	          if(!attrs[i]%in%nom.attr)
	          {
	            attrs.sel<-c(attrs.sel,attrs[i])
	          }
	        }
	      }

	      if(length(attrs.sel)<1)
	      {
	        dev.off()
	        return()
	      }

	      attrs.no<-rep(0,length(attrs.sel))
	      for(i in 1:length(attrs.sel)){
	        attrs.no[i]<-grep(attrs.sel[i],colnames(xdata),fixed=T)[1]
	      }


	pos.Class<-input$pos.class

	add.legend<-input$add.legend.rcc

	aacs<-rep(0,length(attrs))
	color<-c(1:length(attrs))

	aacs[1] <- cost.curve(xdata, attrs.no[1], pos.Class,col=color[1],add=F,xlim=c(xllim,xulim),ylim=c(yllim,yulim))
	if(length(attrs.sel)>1){
	  for(i in 2:length(attrs.sel)){
	    aacs[i]<- cost.curve(xdata, attrs.no[i], pos.Class,col=color[i],add=T,xlim=c(xllim,xulim))
	  }
	}

	if(add.legend){
	  legt <- colnames(xdata)[attrs.no]
	  for(i in 1:length(attrs.sel)){
	    legt[i] <- paste(legt[i],", AAC=",round(1000*aacs[i])/1000,sep="")
	  }
			legend("bottomright",legend=legt,col=color,lwd=2)
	}
	dev.off()
	}
	)


	#all new for feature selection

	output$Title.feature<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  h4("Feature ranking and subset selection")
	})

	output$Type.sel<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  selectInput("type.sel","Selection type",as.list(c("Feature ranking","Feature selection","Wrapper algorithms")),selected=c("Feature ranking"),multiple =F)
	})

	output$Method.sel<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  if(is.null(input$type.sel))return()



	  if(input$type.sel=="Feature ranking")
	  {
	    list.alg=c("auc","HUM","Chi-square","InformationGain","symmetrical.uncertainty","Relief")
	  }
	  if(input$type.sel=="Feature selection")
	  {
	    list.alg=c("FastFilter","CFS","CorrSF","Chi2-algorithm")
	  }
	  if(input$type.sel=="Wrapper algorithms")
	  {
	    list.alg=c("ForwardSearch")
	  }
	  selectInput("method.sel","Selection method",as.list(list.alg),selected=list.alg[1],multiple =F)
	})

	output$Disc.method<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  selectInput("disc.method","Discretization method",as.list(c("MDL","equal interval width","equal frequency")),selected=c("MDL"),multiple =F)
	})



	output$Threshold.class<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  if(is.null(input$method.sel))return()

	  if(length(grep("FastFilter",input$method.sel))==1){

	    textInput("threshold.class", "Threshold for class mutual information", value = "0.3")
	  }
	})

	output$Warn.threshold<-renderText({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  if(is.null(input$threshold.class))return()

	  isolate({
	    threshold.class<-input$threshold.class
	    if(input$threshold.class==""){
	      text<-"Input a value between 0 and 1."
	    }else{
	      threshold.class.num<-char.to.numeric(threshold.class)
	      if((is.null(threshold.class.num))|(length(threshold.class.num)==0))
	      {
	        text<-"It should be between 0 and 1."
	      }
	      else
	      {
	      if(threshold.class.num>1|threshold.class.num<0)
	      {text<-"It should be between 0 and 1."}
	      }
	    }
	  })
	})

	output$Threshold.consis<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  if(is.null(input$method.sel))return()

	  if(length(grep("Chi2-algorithm",input$method.sel))==1){
	    xdata=miss.datei()
	    d1=dim(xdata)
	    vmax=xdata[,ncol(xdata)]
	    vmax=as.numeric(format((d1[1]-max(table(vmax))-1)/d1[1],digits=2))
	    sliderInput("threshold.consis", "Threshold for inconsistensy", min=0, max=vmax, value =0,ticks = FALSE)
	  }
	})

	output$Action.Compute.subset<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  
	  if(any(is.na(miss.datei()))) return()
	  
	  xdata<-miss.datei()
	  judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	  
	  if(judge==1) return()
	  
	  
	  if(is.null(input$method.sel))return()

	  if(is.null(input$disc.method))return()

	  if(length(grep("FastFilter",input$method.sel))==1){

	    if(is.null(input$threshold.class))return()
	    if(input$threshold.class=="") return()

	    threshold.class<-input$threshold.class

	    threshold.class.num<-char.to.numeric(threshold.class)
	    if((is.null(threshold.class.num))|(length(threshold.class.num)==0)) return()
	    if(threshold.class.num>1|threshold.class.num<0)return()
	  }

	  isolate({
	    actionButton("action.compute.subset","Feature subset")
	  })
	})

	output$header.sel.feature<-renderUI({
	  if(is.null(input$action.compute.subset))return()
	  if(input$action.compute.subset==0)return()

	  isolate({

	    h3(input$method.sel,downloadButton("down.sel.feature","Download"))
	  })
	})

	output$Warn.sel.feat<-renderText({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
    
	  if(any(is.na(miss.datei()))) 
	  {
	    text<-"Missing values must be handled"
	  }
	  else
	  {
	    
	    xdata<-miss.datei()
	    judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	    
	   if(judge==1)
	   {
	     text<-"Some features are not numerical. Define them as nominal!"
	   }
	   else
	   {
	    
	  if(is.null(input$action.compute.subset))return()
	  if(input$action.compute.subset==0)return()

	  if(is.null(input$nominal.attr)) return()

	  type=input$method.sel
	  if((type=="auc")||(type=="HUM"))
	  {
	    text<-"The calculation can't be done for nominal features"
	  }
	   }
	  }
	})

	function.sel<-reactive({

	  xdata<-miss.datei()
	  type<-input$method.sel
	  disc<-input$disc.method

	  attrs.no=numeric()

	  if(!is.null(input$nominal.attr))
	  {
	    nom.attr<-input$nominal.attr
	    attrs.no<-numeric()
	    for(i in 1:length(nom.attr)){
	      attrs<-grep(nom.attr[i],colnames(xdata),fixed=T)[1]
	      if(!is.na(attrs))
	      {
	        xdata[,attrs]<-as.factor(xdata[,attrs])
	        attrs.no=c(attrs.no,attrs)
	      }
	    }
	  }

	  if(type=="FastFilter")
	  {
	    threshold.class<-input$threshold.class
	    threshold.class.num<-char.to.numeric(threshold.class)

	    info.val<-select.fast.filter(xdata,disc,threshold.class.num,attrs.no)
	  }
	  if(type=="auc")
	  {

	    index.auc=setdiff(1:(ncol(xdata)-1),attrs.no)
	    auc.val<-compute.aucs(xdata[,c(index.auc,ncol(xdata)),drop=FALSE])

	    aucs.all=rep(-1,ncol(xdata)-1)
	    aucs.all[index.auc]=auc.val[,2]
	    val <- sort(aucs.all,decreasing=T,index.return=TRUE)

	    info.val <- data.frame(names(xdata)[val$ix[1:(ncol(xdata)-1)]],val$ix,val$x)
	    names(info.val) <- c("Biomarker","Index","AUC")
	  }
	  if(type=="HUM")
	  {
	    indexF=1:(ncol(xdata)-1)
	    indexClass=ncol(xdata)
	    indexLabel=levels(xdata[,indexClass])

	    index=setdiff(indexF,attrs.no)
	    out=CalculateHUM_seq(xdata,indexF[index],indexClass,indexLabel)
	    out.all=rep(-1,ncol(xdata)-1)
	    out.all[index]=out$HUM
	    val <- sort(out.all,decreasing=T,index.return=TRUE)
	    info.val <- data.frame(names(xdata)[val$ix[1:(ncol(xdata)-1)]],val$ix,val$x)
	    names(info.val) <- c("Biomarker","Index","HUM")
	  }
	  if(type=="CFS")
	  {
	    info.val<-select.cfs(xdata)
	  }
	  if(type=="Relief")
	  {
	    info.val<-select.relief(xdata)
	  }
	  if(type=="ForwardSearch")
	  {
	    subset <- select.forward.wrapper(xdata)
	    subset<-sapply(subset, function(z) which(names(xdata)==z))
	    info.val <- data.frame(names(xdata)[subset],subset)
	    names(info.val) <- c("Biomarker","Index")
	  }
	  if(type=="Chi2-algorithm")
	  {
	    threshold.consis<-input$threshold.consis
	    out<-chi2.algorithm(xdata,attrs.no,threshold.consis)

	    subset<-sapply(out$subset, function(z) which(names(xdata)==z))
	    info.val <- data.frame(names(xdata)[subset],subset)
	    names(info.val) <- c("Biomarker","Index")
	  }
	  if(type=="CorrSF")
	  {
	    subset <- select.forward.Corr(xdata,disc,attrs.no)
	    subset<-sapply(subset, function(z) which(names(xdata)==z))
	    if(length(subset)==0)
	      info.val <- data.frame("","")
	    else
	      info.val <- data.frame(names(xdata)[subset],subset)
	    names(info.val) <- c("Biomarker","Index")
	  }
	  if(type=="InformationGain")
	  {
	    info.val<-select.inf.gain(xdata,disc,attrs.no)
	  }
	  if(type=="symmetrical.uncertainty")
	  {
	    info.val<-select.inf.symm(xdata,disc,attrs.no)
	  }
	  if(type=="Chi-square")
	  {
	    info.val<-select.inf.chi2(xdata,disc,attrs.no)
	  }
	  return(info.val)
	})


	output$sel.feature<-renderDataTable({

	  if(is.null(input$action.compute.subset))return()
	  if(input$action.compute.subset==0)return()

	  isolate({

	    info.val=function.sel()

	    data.frame(info.val)
	  })
	})


	output$down.sel.feature<- downloadHandler(
	  filename=function(){
	    inFile <- input$File
	    title0<-Sub.filename(inFile$name)
	    paste(title0,"_sel.csv",sep="")
	  },
	  content=function(con){
	    info.val=function.sel()

	    write.csv2(data.frame(info.val),con,row.names=F)

	  })

	#all new for cross-validation
	output$Title.cross<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  h4("Classification validation")
	})

	output$Include.features<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  if(is.null(input$type.sel))return()

	  type<-input$type.sel
	  if(type=="Feature ranking")
	  {
	    isolate({
	      checkboxInput("include.features","Include features",FALSE)
	    })
	  }
	})


	output$Type.cross<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  selectInput("type.cross","Validation method",as.list(c("leaveOneOut","sub-sampling","fold-crossval")),selected=c("leaveOneOut"),multiple =F)
	})

	output$Type.classifier<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  isolate({
	    
	    xdata=miss.datei()
	    judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	    
	    if(judge==2)
	    {
	      class<-c("Support Vector Machines"="svm","Linear Discriminant Analysis"="lda","Random Forest"="rf","Naive Bayes"="nbc","Multinomial Logistic Regression"="mlr")
	    }
	    else
	    {
	      class<-c("Support Vector Machines"="svm","Linear Discriminant Analysis"="lda","Random Forest"="rf","Nearest shrunken centroid"="nsc","Naive Bayes"="nbc","Nearest Neighbour"="nn","Multinomial Logistic Regression"="mlr")
	    }
	    selectInput("type.classifier","Classifiers",as.list(class),multiple=T)
	  })

	})

	output$Number.feature<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  isolate({
	    xdata=miss.datei()
	    num=ncol(xdata)-1
	    num=min(10,num)
	    textInput("number.feat","The maximum amount of selected attributes",value=num)
	  })
	})

	output$Warn.number.feat<-renderText({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  if(is.null(input$number.feat))return()

	  isolate({
	    xdata=miss.datei()
	    no.feat<-input$number.feat
	    if(no.feat==""){
	      text<-"Input a positive integer."
	    }else{
	      if((!grepl("^\\d+$", no.feat))|grepl("-", no.feat))
	      {
	        text<-"It should be a positive integer."
	      }
	      else
	      {
	        feat.num<-as.numeric(no.feat)
	        if(feat.num>=ncol(xdata))
	        {
	          text<-"It should be not more than ncol."
	        }
	      }
	    }
	  })
	})


	output$Action.cross<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  
	  if(any(is.na(miss.datei()))) return()
	  
	  xdata<-miss.datei()
	  judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	  
	  if(judge==1) return()
	  
	  if(is.null(input$method.sel))return()

	  if(is.null(input$type.classifier))return()
	  if(length(input$type.classifier)<1)return()

	  if(is.null(input$number.feat))return()
	  if(input$number.feat=="") return()

	  xdata=miss.datei()
	  no.feat<-input$number.feat
	  if((!grepl("^\\d+$", no.feat))|grepl("-", no.feat))
	  {
	    text<-"It should be a positive integer."
	    return()
	  }
	  else
	  {
	    feat.num<-as.numeric(no.feat)
	    if(feat.num>=ncol(xdata))
	    {
	      text<-"It should be not more than ncol."
	      return()
	    }
	  }

	  if(is.null(input$disc.method))return()

	  if(length(grep("FastFilter",input$method.sel))==1){

	    if(is.null(input$threshold.class))return()
	    if(input$threshold.class=="") return()


	    threshold.class<-input$threshold.class

	    threshold.class.num<-char.to.numeric(threshold.class)
	    if((is.null(threshold.class.num))|(length(threshold.class.num)==0)) return()
	    if(threshold.class.num>1|threshold.class.num<0)return()
	  }

	  #isolate({
	  actionButton("action.cross","Validation")
	  #})
	})


	currentFib <- reactive ({

	  xdata<-miss.datei()
	  type<-input$method.sel

	  attrs.no=numeric()

	  if(!is.null(input$nominal.attr))
	  {
	    nom.attr<-input$nominal.attr
	    attrs.no<-numeric()
	    for(i in 1:length(nom.attr)){
	      attrs<-grep(nom.attr[i],colnames(xdata),fixed=T)[1]
	      if(!is.na(attrs))
	      {
	        xdata[,attrs]<-as.factor(xdata[,attrs])
	        attrs.no=c(attrs.no,attrs)
	      }
	    }
	  }
	  disc<-input$disc.method
	  threshold.class.num=0.3
	  threshold.consis=0
	  if(type=="FastFilter")
	  {
	    threshold.class<-input$threshold.class
	    threshold.class.num<-char.to.numeric(threshold.class)
	  }

	  if(type=="Chi2-algorithm")
	  {
	    threshold.consis<-input$threshold.consis
	  }

	  cross.method=input$type.cross
	  class.method=input$type.classifier

	  no.feat<-input$number.feat
	  if((!grepl("^\\d+$", no.feat))|grepl("-", no.feat))
	  {
	    text<-"It should be a positive integer."
	    return()
	  }
	  else
	  {
	    feat.num<-as.numeric(no.feat)
	    if(feat.num>=ncol(xdata))
	    {
	      text<-"It should be not more than ncol."
	      return()
	    }
	  }


	  if(input$type.sel=="Feature ranking")
	  {
	    flag.feature=input$include.features
	  }
	  else
	  {
	    flag.feature=FALSE
	  }

	  out=classifier.loop(xdata,classifiers=class.method,feature.selection=type,disc.method=disc,threshold=threshold.class.num,
	                      threshold.consis=threshold.consis,attrs.nominal=attrs.no,no.feat=feat.num,flag.feature=flag.feature,method.cross=cross.method)
	  return(list(out=out,cross.method=cross.method,class.method=class.method,flag.feature=flag.feature,feat.num=feat.num))
	})

	output$header.plot.results<-renderUI({

	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()
	  isolate({
	    h3("Plots results",downloadButton("down.cross.plot","Download plots"))
	  })
	})


	output$cross.plot<-renderPlot({

	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()

	  isolate({
	    vrem=currentFib()

	    cross.method=vrem$cross.method
	    class.method=vrem$class.method
	    flag.feature=vrem$flag.feature
	    feat.num=vrem$feat.num
	    out=vrem$out

	    plotClass.result(out$true.classified, cross.method, class.method, flag.feature, feat.num)
	  })
	  print(box)
	  dev.off()
	},height=700,width=700)


	output$down.cross.plot<-downloadHandler(
	  filename=function(){
	    inFile <- input$File
	    title0<-Sub.filename(inFile$name)
	    paste(title0,"_plot.pdf",sep="")
	  },
	  content=function(file){
	    vrem=currentFib()

	    cross.method=vrem$cross.method
	    class.method=vrem$class.method
	    flag.feature=vrem$flag.feature
	    feat.num=vrem$feat.num
	    out=vrem$out

	    pdf(file)
	    plotClass.result(out$true.classified, cross.method, class.method, flag.feature, feat.num)
	    dev.off()
	  }
	)


	#result accuracy in table
	output$Action.true.class<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  
	  if(any(is.na(miss.datei()))) return()
	  
	  xdata<-miss.datei()
	  judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	  
	  if(judge==1) return()

	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()

	  isolate({
	    actionButton("action.true.class","Classification")
	  })
	})


	output$header.cross.results<-renderUI({

	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()
	  if(is.null(input$action.true.class))return()
	  if(input$action.true.class==0)return()

	  isolate({
	    h3("Validation results",downloadButton("down.cross.results","Download results"))
	  })
	})

	output$res.true.class<-renderTable({

	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()
	  if(is.null(input$action.true.class))return()
	  if(input$action.true.class==0)return()

	  isolate({
	    vrem=currentFib()

	    cross.method=vrem$cross.method
	    class.method=vrem$class.method
	    flag.feature=vrem$flag.feature
	    feat.num=vrem$feat.num
	    out=vrem$out

	    if((cross.method=="sub-sampling")||(cross.method=="fold-crossval"))
	    {

	      quant=format(apply(out$true.classified,2,quantile),digits=3)
	      res=sapply(1:ncol(out$true.classified), function(z) paste(quant[2,z],quant[3,z],sep=" ("))
	      res=sapply(1:ncol(out$true.classified), function(z) paste(res[z],quant[4,z],sep=") "))
	    }
	    if(cross.method=="leaveOneOut")
	    {
	      res=out$true.classified
	    }

	    if(flag.feature)
	    {
	      dim(res)=c(length(class.method),feat.num)
	      res=t(res)
	      dimnames(res)=c(list(seq(feat.num,1,-1)),list(class.method))
	    }
	    else
	    {
	      dim(res)=c(length(class.method),1)
	      res=t(res)
	      dimnames(res)=c(list("Accuracy"),list(class.method))
	    }

	    res
	  })
	})

	output$down.cross.results<-downloadHandler(
	  filename=function(){
	    inFile <- input$File
	    title0<-Sub.filename(inFile$name)
	    paste(title0,"_cross.csv",sep="")
	  },
	  content=function(con){
	    vrem=currentFib()
	    
	    cross.method=vrem$cross.method
	    class.method=vrem$class.method
	    flag.feature=vrem$flag.feature
	    feat.num=vrem$feat.num
	    out=vrem$out
	    
	    if((cross.method=="sub-sampling")||(cross.method=="fold-crossval"))
	    {
	      
	      quant=format(apply(out$true.classified,2,quantile),digits=3)
	      res=sapply(1:ncol(out$true.classified), function(z) paste(quant[2,z],quant[3,z],sep=" ("))
	      res=sapply(1:ncol(out$true.classified), function(z) paste(res[z],quant[4,z],sep=") "))
	    }
	    if(cross.method=="leaveOneOut")
	    {
	      res=out$true.classified
	    }
	    
	    if(flag.feature)
	    {
	      dim(res)=c(length(class.method),feat.num)
	      res=t(res)
	      dimnames(res)=c(list(seq(feat.num,1,-1)),list(class.method))
	    }
	    else
	    {
	      dim(res)=c(length(class.method),1)
	      res=t(res)
	      dimnames(res)=c(list("Accuracy"),list(class.method))
	    }
	    write.csv2(data.frame(res),con,row.names=F)
	  }
	)

	output$header.class.result<-renderUI({
	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()



	  isolate({
	    h3("Classification results",downloadButton("down.class.results","Download"))
	  })
	})

	output$res.class.result<-renderDataTable({

	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()

	  isolate({
	    vrem=currentFib()

	    out=vrem$out

	    data.frame(out$predictions)
	  })
	},options=list(lengthMenu = c(10, 25, 50,100)))


	output$down.class.results<- downloadHandler(
	  filename=function(){
	    inFile <- input$File
	    title0<-Sub.filename(inFile$name)
	    paste(title0,"_sel.csv",sep="")
	  },
	  content=function(con){
	    vrem=currentFib()

	    out=vrem$out

	    write.csv2(data.frame(out$predictions),con,row.names=F)
	  })


	#---------------------
	output$Class.confusion<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()

	  class<-input$type.classifier
	  if(length(class)>1){
	    selectInput("class.confusion","Classifier for confusion", as.list(class))
	  }
	})


	output$Action.confusion.class<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()

	  if(any(is.na(miss.datei()))) return()
	  
	  xdata<-miss.datei()
	  judge<-Tipp(xdata,colnames(xdata)[1:(ncol(xdata)-1)])
	  
	  if(judge==1) return()
	  
	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()

	  isolate({
	    actionButton("action.confusion.class","Confusion")
	  })
	})


	output$header.confusion.class<-renderUI({

	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()
	  if(is.null(input$action.confusion.class))return()
	  if(input$action.confusion.class==0)return()

	  isolate({
	    h3("Confusion",downloadButton("down.confusion.class","Download"))
	  })
	})

	output$res.confusion.class<-renderTable({

	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()
	  #if(input$type.cross=="sub-sampling") return()
	  if(is.null(input$action.confusion.class))return()
	  if(input$action.confusion.class==0)return()

	  isolate({
	    class= input$type.classifier
	    if(length(class)>1){
	      if(is.null(input$class.confusion)) return()
	    }
	    vrem=currentFib()
	    
	    out=vrem$out
	    
	    if(input$type.cross=="sub-sampling")
	    {
	      if(length(class)==1)
	      {
	        conf<-out$confus[,,class]
	      }
	      else
	      {
	        cc=input$class.confusion
	        conf<-out$confus[,,cc]
	      }
	      vrem=colSums(conf)
	      vrem[vrem==0]=1
	      conf<-t(conf)/vrem
	      conf<-t(conf)
	      conf.v=paste(round(100*conf, 2), "%", sep="")
	      dim(conf.v)=dim(conf)
	      conf=conf.v
	    }
	    else
	    {
	      if(length(class)==1)
	      {
	        conf<-compute.confusion.matrix(out$predictions[[1]],out$predictions[[2]])
	      }
	      else
	      {
	        cc=input$class.confusion
	        conf<-compute.confusion.matrix(out$predictions[[1]],out$predictions[,cc])
	      }
	    }
	    conf
	  })
	})

	output$down.confusion.class<-downloadHandler(
	  filename=function(){
	    inFile <- input$File
	    title0<-Sub.filename(inFile$name)
	    class= input$type.classifier
	    if(length(class)>1){
	      class<-input$class.confusion
	    }
	    title0<-paste(title0,"_",class,sep="")
	    paste(title0,"_confusion.csv",sep="")
	  },
	  content=function(con){
	    class= input$type.classifier
	    if(length(class)>1){
	      if(is.null(input$class.confusion)) return()
	    }
	    vrem=currentFib()
	    
	    out=vrem$out
	    
	    if(input$type.cross=="sub-sampling")
	    {
	      if(length(class)==1)
	      {
	        conf<-out$confus[,,class]
	      }
	      else
	      {
	        cc=input$class.confusion
	        conf<-out$confus[,,cc]
	      }
	      vrem=colSums(conf)
	      vrem[vrem==0]=1
	      conf<-t(conf)/vrem
	      conf<-t(conf)
	      conf.v=paste(round(100*conf, 2), "%", sep="")
	      dim(conf.v)=dim(conf)
	      conf=conf.v
	    }
	    else
	    {
	      if(length(class)==1)
	      {
	        conf<-compute.confusion.matrix(out$predictions[[1]],out$predictions[[2]])
	      }
	      else
	      {
	        cc=input$class.confusion
	        conf<-compute.confusion.matrix(out$predictions[[1]],out$predictions[,cc])
	      }
	    }
	    write.csv2(conf,con)
	  }
	)

	#review of selected feature after cross-validation with classification
	output$Title.after<-renderUI({
	  if(is.null(datei()))return()
	  if(is.null(teil.datei()))return()
	  if(is.null(miss.datei()))return()
	  h4("Cross-selected features")
	})

	output$header.after.result<-renderUI({

	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()



	  isolate({
	    h3("Features",downloadButton("down.after.result","Download"))
	  })
	})

	output$res.after.result<-renderDataTable({

	  if(is.null(input$action.cross))return()
	  if(input$action.cross==0)return()



	  isolate({
	    vrem=currentFib()

	    out=vrem$out


	    if(length(out$no.selected[,1])<=1)
	    {
	      flist=out$no.selected[,1]
	    }
	    else
	    {
	      flist=sort(out$no.selected[,1],decreasing=T)
	      flist=subset(flist,flist!=0)
	    }

	    if(length(out$no.selected[,1])<=1)
	    {
	      res=data.frame(rownames(out$no.selected),flist)
	    }
	    else
	    {
	      res=data.frame(names(flist),flist)
	    }
	    rownames(res)<-NULL
	    names(res) <- c("Feature names","Number of selections")
	    data.frame(res)
	  })
	},options=list(lengthMenu = c(40,100)))


	output$down.after.result<- downloadHandler(
	  filename=function(){
	    inFile <- input$File
	    title0<-Sub.filename(inFile$name)
	    paste(title0,"_after.csv",sep="")
	  },
	  content=function(con){
	    vrem=currentFib()
	    
	    out=vrem$out
	    
	    
	    if(length(out$no.selected[,1])<=1)
	    {
	      flist=out$no.selected[,1]
	    }
	    else
	    {
	      flist=sort(out$no.selected[,1],decreasing=T)
	      flist=subset(flist,flist!=0)
	    }
	    
	    if(length(out$no.selected[,1])<=1)
	    {
	      res=data.frame(rownames(out$no.selected),flist)
	    }
	    else
	    {
	      res=data.frame(names(flist),flist)
	    }
	    rownames(res)<-NULL
	    names(res) <- c("Feature names","Number of selections")

	    write.csv2(res,con,row.names=F)
	  })
})
