library('bnlearn')
library('rhandsontable')
library('shiny')
library('shinydashboard')
library('dplyr')
library('visNetwork')
library('shinyWidgets')
library('tools')
library('shinyalert')
library('shinycssloaders')
library('rintrojs')
library('arules')
library('psych')
library("DT")
library("linkcomm")
library('igraph')
library("shinyBS")
library("leaflet")
library("raster")
source('error.bar.R')
source('graph.custom.R')
source('custom.Modules.R')
source('dashboardthemes.R')
source('dependency.R')


shinyServer(function(input, output,session) {
  withProgress(message = "Starting app for the first time, installing one-time dependencies, please be patient...", value = 0, {
    dependency()
  })
  library('gRain')
  withProgress(message = "Initializing Dashboard", value = 0, {
  #Data upload limit and other options
  options(shiny.maxRequestSize=1500*1024^2)
  options(warn=-1)
  options("getSymbols.warning4.0"=FALSE)
  #Structure Initialization
  DiscreteData <- get(load("data.RData"))
  bn.hc.boot.average<<-get(load("structure.RData"))
  #Sanity check
  sanity<-1
  confidence<-1
  check<-1
  exactCheck<-1


  #Initialization

  plotValue <<- reactiveValues(probs = NULL)

  #Map Initialization

  #Load Map

  ind <- readRDS("state.rds")
  ind2 <- readRDS("district.rds")

  #Map Initialization End

  rvs <<- reactiveValues(evidence = list(),values = list(),evidenceObserve = list(),valueObserve = list())
  insertedV <- c()
  inserted <- c()
  rvs$evidence <- c()
  rvs$value <- c()
  rvs$evidenceObserve <- c()
  rvs$valueObserve <- c()
  nodeNames <- c()
  EventNode <- c()
  EvidenceNode <- c()
  shapeVector<- c()
  communities<-NULL
  #structure initialization
  bn.hc.boot.fit <<- bn.fit(bn.hc.boot.average,DiscreteData[,names(bn.hc.boot.average$nodes)],method = "bayes")
  NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
  nodeNames <- names(bn.hc.boot.average$nodes)
  EventNode <- nodeNames[1]
  EvidenceNode <- c()
  shapeVector<- rep('dot',length(nodeNames))
  updateSelectInput(session,'event',choices = nodeNames)
  updateSelectizeInput(session,'varselect',choices = nodeNames)
  updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                    "ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                     "ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'varshape3',choices = c( "dot","square", "triangle", "box", "circle", "star","ellipse", "database", "text", "diamond"))
  updateSelectInput(session,'modGroup',choices = "")
  updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
  updateSelectInput(session,'paramSelect',choices = nodeNames)
  updateSelectInput(session,"moduleSelection",choices = "graph")
  graph<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
  updateSelectInput(session,"neighbornodes",choices = "")
  updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
  output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,input$bayesFont)})
  })
  #observe events
  observeEvent(input$paramSelect,{
    tryCatch({
      output$parameterPlot<-renderPlot({bn.fit.barchart(bn.hc.boot.fit[[input$paramSelect]])})
    },error = function(e){
      shinyalert(toString(e), type = "error")
    })
  })
  observeEvent(input$insertBtn, {
    withProgress(message = "Inserting Evidence", value = 0, {
      tryCatch({
        btn <<- input$insertBtn
        typeof(btn)
        id <- paste0('Evidence', btn)
        idL <- paste("Evidence", btn)
        idV <- paste0('Value', btn)
        idVL <- paste("Value", btn)
        insertUI(selector = '#placeholder1',
                 ui = tags$div(selectInput(id,'Evidence',nodeNames),
                               id = id
                 )
        )
        insertUI(selector = '#placeholder2',
                 ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,nodeNames[1]])),
                               id = idV
                 )
        )
        inserted <<- c(id, inserted)
        insertedV <<- c(idV,insertedV)
        rvs$evidence <<- c(rvs$evidence,id)
        rvs$value <<- c(rvs$value,id)
        rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
          tryCatch({
            valID = insertedV[which(inserted == id)]
            updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]))
          },error = function(e){
            shinyalert(toString("Construct bayesian network for taking decisions"), type = "error")
          })
        }))

      },error = function(e){
        shinyalert(toString(e), type = "error")
      })
    })
  })

  observeEvent(input$removeBtn, {
    tryCatch({
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', inserted[length(inserted)])
      )
      inserted <<- inserted[-length(inserted)]
      removeUI(
        ## pass in appropriate div id
        selector = paste0('#', insertedV[length(insertedV)])
      )
      insertedV <<- insertedV[-length(insertedV)]
      rvs$evidence <<- rvs$evidence[-length(inserted)]
      rvs$value <<- rvs$value[-length(insertedV)]
      rvs$evidenceObserve <<- rvs$evidenceObserve[-length(inserted)]
      rvs$valueObserve <<- rvs$valueObserve[-length(insertedV)]
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
  })
  observeEvent(input$event,{
    tryCatch({
      if(input$event=="")
      {
        updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
      }
      else
      {
        updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,input$event]),value = nlevels(DiscreteData[,input$event]))
      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
  })
  observeEvent(input$exactInference,{
    tryCatch({
      withProgress(message = "Learning Exact Inference", value = 0, {
        bn.jtree <<- compile(as.grain(bn.hc.boot.fit))
        exactCheck<<-2
        shinyalert("Exact inferences learned",type = "success")
      })
    },error=function(e)
    {
      exactCheck<<-1
      shinyalert(e,type="error")
    })
  })
  observeEvent(input$plotBtn,{
    withProgress(message = "Learning Inference", value = 0, {
      tryCatch({
        confidence<<-1
        str1 <<- ""
        str2<<-""
        str3<<-""
        count =1
        for(elem in inserted)
        {
          vid = insertedV[which(inserted == elem)]
          str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
          str2<<-paste(str2,input[[elem]],", ")
          str3<<-paste0(str3, input[[elem]], "='", input[[vid]], "'")
          if(count!=length(inserted))
          {
            str1 <<- paste0(str1," & ")
            str3 <<- paste0(str3,",")
          }
          count = count + 1
        }
        str3<<-paste0("list(",str3,")")
        if(input$exact==T)
        {
          if(exactCheck==2)
          {
            evidenceV = setEvidence(bn.jtree, evidence=eval(parse(text = str3)))
            probs = ((querygrain(evidenceV,nodes=input$event))[[input$event]])[1:input$NumBar]
          }
          else
          {
            shinyalert("Learn exact inferences on the new or modified structure",type = "info")
          }
        }
        else
        {
          probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))))[1:input$NumBar]
        }
        output$distPlot = renderPlot({par(mar=c(5,3,3,3))
          plotValue$probs <<- probs
          par(oma=c(5,3,3,3))
          barx<<-barplot(probs,
                         col = "lightblue",
                         main = paste("probability of ",input$event," conditioned on ",substr(str2,1,(nchar(str2)-2))),
                         border = NA,
                         xlab = "",
                         ylab = "Probabilities",
                         ylim = c(0,1),
                         las=2,
                         cex.names=as.numeric(input$plotFont))
          text(x = barx,y = round(probs,digits = 4),label = round(probs,digits = 4), pos = 3, cex = as.numeric(input$valueFont), col = "black")
        })
        updateRadioGroupButtons(session,'bayesianOption',selected = "Infer Decisions")
      },error = function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
    })
  })
  observeEvent(input$plotStrengthBtn,{
    withProgress(message = "Learning Inference", value = 0, {
      tryCatch({
        confidence<<-2
        probT = c()
        for(i in 1:input$plotStrengthBtn)
        {
          str1 <<- ""
          str2<<-""
          str3<<-""
          count =1
          for(elem in inserted)
          {
            vid = insertedV[which(inserted == elem)]
            str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
            str2<<-paste(str2,input[[elem]],", ")
            str3<<-paste0(str3, input[[elem]], "='", input[[vid]], "'")
            if(count!=length(inserted))
            {
              str1 <<- paste0(str1," & ")
              str3 <<- paste0(str3,",")
            }
            count = count + 1
          }
          str3<<-paste0("list(",str3,")")
          if(input$exact==T)
          {
            if(exactCheck==2)
            {
              evidenceV = setEvidence(bn.jtree, evidence=eval(parse(text = str3)))
              probs = ((querygrain(evidenceV,nodes=input$event))[[input$event]])
            }
            else
            {
              shinyalert("Learn exact inferences on the new or modified structure",type = "info")
            }
          }
          else
          {
            probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))))[1:input$NumBar]
          }
          probT = rbind(probT,probs)
        }
        ee = 1
        ee$mean = colMeans(probT)
        ee$sd = apply(probT, 2, sd)
        output$distPlot = renderPlot({par(mar=c(5,3,3,3))
	  plotValue$probs <<- data.frame(names(ee$mean), ee$mean)
          par(oma=c(5,3,3,3))
          barx <<-barplot(ee$mean[1:input$NumBar],
                          col = "lightblue",
                          main = paste("probability of ",input$event," conditioned on ",substr(str2,1,(nchar(str2)-2))),
                          border = NA,
                          xlab = "",
                          ylab = "Probabilities",
                          ylim = c(0,1),
                          las=2,
                          cex.names=as.numeric(input$plotFont))
          text(x = barx,y = round(ee$mean[1:input$NumBar],digits = 4),label = round(ee$mean[1:input$NumBar],digits = 4), pos = 3, cex = as.numeric(input$valueFont), col = "black")
          error.bar(barx,ee$mean[1:input$NumBar], 1.96*ee$sd[1:input$NumBar]/sqrt(input$plotStrengthBtn))})
        updateRadioGroupButtons(session,'bayesianOption',selected = "Infer Decisions")

      },error = function(e){
        shinyalert::shinyalert(toString(e), type = "error")
      })
    })
  })
  observeEvent(input$sortPlot,{
    withProgress(message = "Learning Inference", value = 0, {
      if(confidence==1)
      {
        tryCatch({
          confidence<<-1
          str1 <<- ""
          str2<<-""
          str3<<-""
          count =1
          for(elem in inserted)
          {
            vid = insertedV[which(inserted == elem)]
            str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
            str2<<-paste(str2,input[[elem]],", ")
            str3<<-paste0(str3, input[[elem]], "='", input[[vid]], "'")
            if(count!=length(inserted))
            {
              str1 <<- paste0(str1," & ")
              str3 <<- paste0(str3,",")
            }
            count = count + 1
          }
          str3<<-paste0("list(",str3,")")
          if(input$exact==T)
          {
            if(exactCheck==2)
            {
              evidenceV = setEvidence(bn.jtree, evidence=eval(parse(text = str3)))
              probs = sort(((querygrain(evidenceV,nodes=input$event))[[input$event]]),decreasing = T)[1:input$NumBar]
            }
            else
            {
              shinyalert("Learn exact inferences on the new or modified structure",type = "info")
            }
          }
          else
          {
            probs = sort(prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1))))),decreasing = T)[1:input$NumBar]
          }
          output$distPlot = renderPlot({par(mar=c(5,3,3,3))
          	plotValue$probs <<- probs
            par(oma=c(5,3,3,3))
            barx<<-barplot(probs,
                           col = "lightblue",
                           main = paste("probability of ",input$event," conditioned on ",substr(str2,1,(nchar(str2)-2))),
                           border = NA,
                           xlab = "",
                           ylab = "Probabilities",
                           ylim = c(0,1),
                           las=2,
                           cex.names=as.numeric(input$plotFont))
            text(x = barx,y = round(probs,digits = 4),label = round(probs,digits = 4), pos = 3, cex = as.numeric(input$valueFont), col = "black")
          })
          updateRadioGroupButtons(session,'bayesianOption',selected = "Infer Decisions")

        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")
        })
      }
      else
      {
        tryCatch({
          confidence<<-2
          probT = c()
          for(i in 1:input$plotStrengthBtn)
          {
            str1 <<- ""
            str2<<-""
            str3<<-""
            count =1
            for(elem in inserted)
            {
              vid = insertedV[which(inserted == elem)]
              str1 <<- paste0(str1,"(", input[[elem]], "=='", input[[vid]], "')")
              str2<<-paste(str2,input[[elem]],", ")
              str3<<-paste0(str3, input[[elem]], "='", input[[vid]], "'")
              if(count!=length(inserted))
              {
                str1 <<- paste0(str1," & ")
                str3 <<- paste0(str3,",")
              }
              count = count + 1
            }
            if(input$exact==T)
            {
              if(exactCheck==2)
              {
                evidenceV = setEvidence(bn.jtree, evidence=eval(parse(text = str3)))
                probs = ((querygrain(evidenceV,nodes=input$event))[[input$event]], decreasing = T)[1:input$NumBar]
              }
              else
              {
                shinyalert("Learn exact inferences on the new or modified structure",type = "info")
              }
            }
            else
            {
              probs = prop.table(table(cpdist(bn.hc.boot.fit,input$event,evidence = eval(parse(text = str1)))), decreasing = T)[1:input$NumBar]
            }
            probT = rbind(probT,probs)
          }
          ee = 1
          ee$mean = colMeans(probT)
          ee$sd = apply(probT, 2, sd)
          nm = names(sort(ee$mean,decreasing = T))[1:input$NumBar]
          output$distPlot = renderPlot({par(mar=c(5,3,3,3))
	    plotValue$probs <<- data.frame(names(ee$mean), ee$mean)
            par(oma=c(5,3,3,3))
            barx <<-barplot(ee$mean[nm],
                            col = "lightblue",
                            main = paste("probability of ",input$event," conditioned on ",substr(str2,1,(nchar(str2)-2))),
                            border = NA,
                            xlab = "",
                            ylab = "Probabilities",
                            ylim = c(0,1),
                            las=2,
                            cex.names=as.numeric(input$plotFont))
            text(x = barx,y = round(ee$mean[nm],digits = 4),label = round(ee$mean[nm],digits = 4), pos = 3, cex = as.numeric(input$valueFont), col = "black")
            error.bar(barx,ee$mean[nm], 1.96*ee$sd[nm]/sqrt(input$plotStrengthBtn))})
          updateRadioGroupButtons(session,'bayesianOption',selected = "Infer Decisions")
        },error = function(e){
          shinyalert::shinyalert(toString(e), type = "error")
        })
      }
    })
  })
  observeEvent(input$moduleSelection,{
    withProgress(message = "Loading Module", value = 0, {
      tryCatch({
        if(input$moduleSelection!='graph')
        {
          selectedNodes<<-communities[[lengthCom[input$moduleSelection]]]
          from<-c()
          to<-c()
          for(i in 1:length(data.frame(directed.arcs(bn.hc.boot.average))[,1]))
          {
            if(is.element(data.frame(directed.arcs(bn.hc.boot.average))[i,1],selectedNodes))
            {
              from<-c(from,i)
            }
            if(is.element(data.frame(directed.arcs(bn.hc.boot.average))[i,2],selectedNodes))
            {
              to<-c(to,i)
            }
          }
          pruneGraph<<-data.frame(directed.arcs(bn.hc.boot.average))[intersect(from,to),]
          NetworkGraph<<-pruneGraph
          shapeVector<<-rep('dot',length(communities[[input$moduleSelection]]))
          for(elem in 1:length(inserted))
          {
            removeUI(
              selector = paste0('#', inserted[elem])
            )

          }
          inserted <<- c()
          for(elem2 in 1:length(insertedV))
          {
            removeUI(
              selector = paste0('#', insertedV[elem2])
            )

          }
          insertedV <<- c()
          rvs$evidence <<- c()
          rvs$value <<- c()
          rvs$evidenceObserve <<- c()
          rvs$valueObserve <<- c()
          output$distPlot <<- renderPlot(NULL)
          nodeNames <<- selectedNodes
          EventNode <<- nodeNames[1]
          EvidenceNode <<- c()
          shapeVector<<- rep('dot',length(nodeNames))
          updateSelectInput(session,'event',choices = nodeNames)
          output$netPlot<-renderVisNetwork({graph.custom(pruneGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,input$bayesFont)})
          updateSelectizeInput(session,'varselect',choices = nodeNames)
          updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                            "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
          updateSelectInput(session,'paramSelect',choices = nodeNames)
          graph<<-graph_from_edgelist(as.matrix(pruneGraph),directed = TRUE)
          updateSelectInput(session,"neighbornodes",choices = "")
          updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
        }
        else
        {
          for(elem in 1:length(inserted))
          {
            removeUI(
              selector = paste0('#', inserted[elem])
            )

          }
          inserted <<- c()
          for(elem2 in 1:length(insertedV))
          {
            removeUI(
              selector = paste0('#', insertedV[elem2])
            )

          }
          insertedV <<- c()
          rvs$evidence <<- c()
          rvs$value <<- c()
          rvs$evidenceObserve <<- c()
          rvs$valueObserve <<- c()
          output$distPlot <<- renderPlot(NULL)
          NetworkGraph <<- data.frame(directed.arcs(bn.hc.boot.average))
          nodeNames <<- names(bn.hc.boot.average$nodes)
          EventNode <<- nodeNames[1]
          EvidenceNode <<- c()
          shapeVector<<- rep('dot',length(nodeNames))
          updateSelectInput(session,'event',choices = nodeNames)
          output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,input$bayesFont)})
          updateSelectInput(session,'event',choices = nodeNames)
          updateSelectizeInput(session,'varselect',choices = nodeNames)
          updateSelectInput(session,'varshape',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                            "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'varshape2',choices = c( "dot","square", "triangle", "box", "circle", "star",
                                                             "ellipse", "database", "text", "diamond"))
          updateSelectInput(session,'graph_layout',choices = c("layout_nicely","layout_as_star","layout_as_tree","layout_in_circle","layout_with_sugiyama","layout_on_sphere","layout_randomly","layout_with_fr","layout_with_kk","layout_with_lgl","layout_with_mds","layout_on_grid","layout_with_graphopt","layout_with_gem","layout_with_dh"))
          updateSelectInput(session,'paramSelect',choices = nodeNames)
          graph<<-graph_from_edgelist(as.matrix(NetworkGraph),directed = TRUE)
          updateSelectInput(session,"neighbornodes",choices = "")
          updateSliderInput(session,"NumBar",min = 1, max = nlevels(DiscreteData[,nodeNames[1]]),value = nlevels(DiscreteData[,nodeNames[1]]))
        }
      },error=function(e){
        shinyalert(toString(e), type = "error")
      })
    })
  })
  observeEvent(input$current_node_id,{
    tryCatch({
      if(!is.null(input$current_node_id))
      {
        if(input$degreeN>1)
        {
          nlist<<-ego(graph,input$degreeN,nodes = input$current_node_id, mode = c("all", "out", "in"),mindist = 0)
          nlistP<<-ego(graph,input$degreeN-1,nodes = input$current_node_id, mode = c("all", "out", "in"),mindist = 0)
          diffList<<-setdiff(nlist[[1]]$name,nlistP[[1]]$name)
          updateSelectInput(session,"neighbornodes",choices = diffList)
        }
        else
        {
          nlist<<-ego(graph,input$degreeN,nodes = input$current_node_id, mode = c("all", "out", "in"),mindist = 0)
          updateSelectInput(session,"neighbornodes",choices = setdiff(nlist[[1]]$name,input$current_node_id))
        }

      }
    },error=function(e){
      shinyalert(toString(e), type = "error")
    })
  })
  observeEvent(input$Bcommunities,{
    tryCatch({
      communities<<-custom.Modules(NetworkGraph,input$moduleAlgo)
      names(communities)<<-paste("Module",c(1:length(communities)),sep=" ")
      lengthCom<<-c()
      for(n in names(communities))
      {
        lengthCom<<-c(lengthCom,length(communities[[n]]))
      }
      lengthCom<<-order(lengthCom,decreasing = T)
      names(lengthCom)<<-paste("Module",c(1:length(communities)),sep=" ")
      updateSelectInput(session,"moduleSelection",choices = c("graph",names(communities)))
      updateSelectInput(session,'modGroup',choices = names(communities))
      shinyalert("Module detection successfull",type="success")
    },error=function(e){
      print(e)
      shinyalert("Module detection failed",type="error")
      updateSelectInput(session,"moduleSelection",choices = "graph")
      updateSelectInput(session,'modGroup',choices = "")
    })
  })
  observeEvent(input$degree,{
    tryCatch({
      for(elem in inserted)
      {
        EvidenceNode = c(EvidenceNode,input[[elem]])
      }
      if(sanity==1)
      {
        EventNode = nodeNames[1]
        sanity=sanity + 1
      }
      else
      {
        EventNode = input$event
      }
      output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,input$bayesFont)})
      updateSelectInput(session,"neighbornodes",choices = "")
    },error = function(e){
      shinyalert(toString(e), type = "error")

    })
  })
  observeEvent(input$graph_layout,{
    tryCatch({
      for(elem in inserted)
      {
        EvidenceNode = c(EvidenceNode,input[[elem]])
      }
      if(sanity==1)
      {
        EventNode = nodeNames[1]
        sanity=sanity + 1
      }
      else
      {
        EventNode = input$event
      }
      output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,input$bayesFont)})
      updateSelectInput(session,"neighbornodes",choices = "")
    },error = function(e){
      shinyalert(toString(e), type = "error")

    })
  })
  observeEvent(input$graphBtn,{
    tryCatch({
      for(elem in inserted)
      {
        EvidenceNode = c(EvidenceNode,input[[elem]])
      }
      if(sanity==1)
      {
        EventNode = nodeNames[1]
        sanity=sanity + 1
      }
      else
      {
        EventNode = input$event
      }
      output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,input$bayesFont)})
      updateSelectInput(session,"neighbornodes",choices = "")
    },error = function(e){
      shinyalert(toString(e), type = "error")

    })
  })
  observeEvent(input$group,{
    tryCatch({
      shapeVector[which(nodeNames %in% input$varselect)] <<- input$varshape
      for(elem in inserted)
      {
        EvidenceNode = c(EvidenceNode,input[[elem]])
      }
      if(sanity==1)
      {
        EventNode = nodeNames[1]
        sanity=sanity + 1
      }
      else
      {
        EventNode = input$event
      }
      output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,input$bayesFont)})
      updateSelectInput(session,"neighbornodes",choices = "")
    },error = function(e){
      shinyalert(toString(e), type = "error")

    })
  })
  observeEvent(input$group2,{
    tryCatch({
      shapeVector<<-shapeVector[1:length(nodeNames)]
      shapeVector[eval(parse(text = input$varselectvector))] <<- input$varshape2
      for(elem in inserted)
      {
        EvidenceNode = c(EvidenceNode,input[[elem]])
      }
      if(sanity==1)
      {
        EventNode = nodeNames[1]
        sanity=sanity + 1
      }
      else
      {
        EventNode = input$event
      }
      output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,input$bayesFont)})
      updateSelectInput(session,"neighbornodes",choices = "")
    },error = function(e){
      shinyalert(toString(e), type = "error")

    })
  })
  observeEvent(input$group3,{
    if(input$modGroup!="")
    {
      tryCatch({
        selectedNodes<<-communities[[lengthCom[input$modGroup]]]
        shapeVector[which(nodeNames %in% selectedNodes)] <<- input$varshape3
        for(elem in inserted)
        {
          EvidenceNode = c(EvidenceNode,input[[elem]])
        }
        if(sanity==1)
        {
          EventNode = nodeNames[1]
          sanity=sanity + 1
        }
        else
        {
          EventNode = input$event
        }
        output$netPlot<-renderVisNetwork({graph.custom(NetworkGraph,nodeNames,shapeVector,EvidenceNode,EventNode,input$degree,input$graph_layout,input$bayesFont)})
        updateSelectInput(session,"neighbornodes",choices = "")
      },error = function(e){
        shinyalert(toString(e), type = "error")

      })
    }
  })

  #Map Plotting

  #Label for initial map
  labels <- sprintf(
    "<strong>%s</strong><br/>",
    ind$NAME_1
  ) %>% lapply(htmltools::HTML)


  bins <- c(1:36)
  pal <- colorBin(topo.colors(18), domain = as.numeric(factor(ind$NAME_1)), bins = bins)

  #Initial Map plot

  output$myPlot2 <- renderLeaflet(leaflet(ind) %>%
                                    addPolygons(fillColor = pal(as.numeric(factor(ind$NAME_1))), fillOpacity = 0.7, layerId = ind$NAME_1, dashArray = "3", weight = 2, color = "white", highlight = highlightOptions(
                                      weight = 4, color = "#666", dashArray = "", bringToFront = TRUE), label = labels, labelOptions = labelOptions(
                                        style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
                                    addLegend(colors = pal(as.numeric(factor(ind$NAME_1))), labels = ind$NAME_1, title = "States", position = "topright"))
  #Reactive code for redrawing map with evidence as "STATE_STATEID"

  observeEvent(plotValue$probs, {
    foo <- plotValue$probs
    foo <- data.frame(foo)
    # ihds <- as.character(foo$Var1)
    foo$Var1 <- as.character(foo$Var1)
    foo$Freq <- as.numeric(foo$Freq)
    # print(foo)
    foo$Freq[which(foo$Freq == 0)] <- NA
    # print(foo)
    # print(ihds)
    # print(input$event)
    # print(input$event == "STATE_STATEID")
    # print(input$event == "Census_state_id_district_id_labeled_DISTRICT")
    if(input$event == "STATE_STATEID"){
      colnames(foo) <- c("NAME_1", "Freq")
      ind_map <- merge(ind, foo, by = "NAME_1")

      #Label for plotValue map states

      labels2 <- sprintf(
     "<strong>%s</strong><br/>%g",
      ind_map$NAME_1, ind_map$Freq
      ) %>% lapply(htmltools::HTML)

      pal <- colorBin("YlOrRd", domain = ind_map$Freq, bins = length(ind_map$Freq)/8)

      #Redrawn Map Plot

      leafletProxy("myPlot2") %>%
        clearControls() %>%
        clearShapes() %>%
        addPolygons(data = ind_map, fillColor = pal(ind_map$Freq), fillOpacity = 0.7, layerId = ind_map$NAME_1, dashArray = "3", weight = 2, color = "white", highlight = highlightOptions(
          weight = 4, color = "#666", dashArray = "", bringToFront = TRUE), label = labels2, labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
        addLegend(pal = pal, values = ind_map$Freq, labels = ind_map$Freq, title = substr(str2,1,(nchar(str2)-2)), position = "topright")
    }

    # District Data still to be implemented

    else if(input$event == "Census_state_id_district_id_labeled_DISTRICT"){
      colnames(foo) <- c("NAME_2", "Freq")
      ind_map <- merge(ind2, foo, by = "NAME_2")

      #Label for plotValue map district

       labels3 <- sprintf(
      "<strong>%s</strong><br/>%g",
       ind_map$NAME_2, ind_map$Freq
       ) %>% lapply(htmltools::HTML)

       pal <- colorBin("YlOrRd", domain = ind_map$Freq, bins = 5)

       #Redrawn Map Plot

       leafletProxy("myPlot2") %>%
         clearControls() %>%
         clearShapes() %>%
         addPolygons(data = ind_map, fillColor = pal(ind_map$Freq), fillOpacity = 0.7, layerId = ind_map$NAME_2, dashArray = "3", weight = 2, color = "white", highlight = highlightOptions(
           weight = 4, color = "#666", dashArray = "", bringToFront = TRUE), label = labels3, labelOptions = labelOptions(
             style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) %>%
         addLegend(pal = pal, values = ind_map$Freq, labels = ind_map$Freq, title = substr(str2,1,(nchar(str2)-2)), position = "topright")


    }


  })


  #Add state as event on clicking

  observeEvent(input$myPlot2_shape_click,{
          tryCatch({
        btn <<- btn + 1
        id <- paste0('Evidence', btn)
        idL <- paste("Evidence", btn)
        idV <- paste0('Value', btn)
        idVL <- paste("Value", btn)
        insertUI(selector = '#placeholder1',
                 ui = tags$div(selectInput(id,'Evidence',nodeNames,selected = "STATE_STATEID"),
                               id = id
                 )
        )
        insertUI(selector = '#placeholder2',
                 ui = tags$div(selectInput(idV,'Value',levels(DiscreteData[,"STATE_STATEID"])),
                               id = idV
                 )
        )
        inserted <<- c(id, inserted)
        insertedV <<- c(idV,insertedV)
        rvs$evidence <<- c(rvs$evidence,id)
        rvs$value <<- c(rvs$value,id)
        rvs$evidenceObserve <<- c(rvs$evidenceObserve,observeEvent(input[[id]],{
          tryCatch({
            valID = insertedV[which(inserted == id)]
            updateSelectInput(session,valID, choices = levels(DiscreteData[,input[[id]]]), selected = input$myPlot2_shape_click$id)
          },error = function(e){
            shinyalert(toString("Construct bayesian network for taking decisions"), type = "error")
          })
        }))

      },error = function(e){
        shinyalert(toString(e), type = "error")
      })
        })



})
