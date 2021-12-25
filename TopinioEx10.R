#' This is an R Shiny Web Application that contains the UI implementation for the Quadratic Spline Interpolation and the Simplex Method
#' @author Mark Genesis C. Topinio
#' @created_date 2021-11-23 21:55

#The template used for this Web App is from a YouTube Channel called "Data Professor"
#These are his social media references:
####################################
# Data Professor                   #
# http://youtube.com/dataprofessor #
# http://github.com/dataprofessor  #
####################################

# Modified from Winston Chang, 
# https://shiny.rstudio.com/gallery/shiny-theme-selector.html

# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(shiny)
library(shinythemes)


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Exercise 10 - CMSC 150 (B)",
                  tabPanel("Quadratic Spline Interpolation",
                           sidebarPanel(
                             tags$h3("Inputs: (comma-delimited)"),
                             textInput("txt1", "X-Values:", ""),
                             textInput("txt2", "Y-Values:", ""),
                             textInput("txt3", "Value to Estimate:", ""),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("QSI Outputs:"),
                             
                             h4("Data Points:"),
                             uiOutput("dataOut"),
                             
                             h4("Value to Estimate:"),
                             verbatimTextOutput("estOut"),
                             
                             h4("Answer:"),
                             verbatimTextOutput("estimate"),
                             
                             h4("Polynomials:"),
                             verbatimTextOutput("polyOut"),
                             
                           )
                           
                  ), # Navbar 1, tabPanel; mainPanel for Quadratic Spline Interpolation
                  tabPanel("Simplex Method Calculator",
                           sidebarPanel(
                             tags$h3("Inputs: (comma-delimited)"),
                             textInput("txt4", "Objective Function Coefficients:", ""),
                             textInput("txt5", "Decision Variable Coefficients (Please separate each row of coeffcients with spaces):", ""),
                             textInput("txt6", "Constraint Coefficients (Please sort according to each rows of Decision Variable Coefficients):", ""),
                             textInput("txt7", "For Maximization? [TRUE/FALSE]:", ""),
                             textInput("txt8", "For DIVOC Shipping Analysis Problem? [TRUE/FALSE]:", ""),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h1("Simplex Method Output:"),
                             
                             h4("Final Tableau:"),
                             uiOutput("tableau"),
                             
                             h4("Basic Solution:"),
                             uiOutput("basicSoln"),
                             
                             h4("Maximum/Minimum Cost:"),
                             verbatimTextOutput("opt.val"),
                             
                             h4("Number of Items for Shipping:"),
                             uiOutput("shipping"),
                           )
                           
                  ), #panel for Simplex Method
                  tabPanel("About the Developer",
                           mainPanel(
                             h1("Name: Mark Genesis C. Topinio"),
                             h1("Age: 21"),
                             h1("School:"),
                             h3("University of the Philippines Los Banos"),
                             h1("Course:"),
                             h3("BS Computer Science"),
                             h1("Residence:"),
                             h3("Cabatuan, Isabela"),
                             h1("References:"),
                             tags$pre(h4("Web App Template by Data Professor:\n\thttp://youtube.com/dataprofessor\n\thttp://github.com/dataprofessor"),
                                      h4("CMSC 150 - Course Materials:\n\t(Institute of Computer Science, CAS, UPLB)"),
                                      h4("Big M - Method Reference Video by Kauser Wise:\n\thttps://www.youtube.com/watch?v=MZ843Vvia0A&list=LL&index=10&t=783s&ab_channel=KauserWise")
                                      )
                           )
                  )
                  
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
  output$dataOut <- renderTable({
    x = as.numeric(unlist(strsplit(input$txt1,",")))
    y = as.numeric(unlist(strsplit(input$txt2,",")))
    validate(
      need(length(x) != 0 && length(y) != 0 && length(x)==length(y), "Please enter valid values (w/ estimate within range) to show the data points.")
    )
    data = c(x,y)
    dataPoints = matrix(data, ncol=2, nrow = length(x), byrow=F)
    colnames(dataPoints) = c("x","f(x)")
    dataPoints
  })
  
  output$estOut <- renderText({
    paste( input$txt3, sep = " " )
    
  })
  
  output$estimate <- renderText({ #printing the expected estimate for QSI
    x = as.numeric(unlist(strsplit(input$txt1,",")))
    y = as.numeric(unlist(strsplit(input$txt2,",")))
    val = as.numeric(input$txt3)
    validate(
      need(length(x) != 0 && length(y) != 0 && is.na(val) != TRUE && !is.na(poly.qsi(list(x,y),val)), "Please enter valid values (w/ estimate within range) to show the estimated value.")
    )
    data = list(x,y)
    result = poly.qsi(data,val)
    paste(round(result$y,4))
    
  })
  
  output$polyOut <- renderText({ #Printing the expected polynomials for QSI
    x = as.numeric(unlist(strsplit(input$txt1,",")))
    y = as.numeric(unlist(strsplit(input$txt2,",")))
    val = as.numeric(input$txt3)
    validate(
      need(length(x) != 0 && length(y) != 0 && is.na(val) != TRUE && !is.na(poly.qsi(list(x,y),val)), "Please enter valid values (w/ estimate within range) to show polynomial equations.")
    )
    data = list(x,y)
    result = poly.qsi(data,val)
    result = unlist(result$qsi.fxns)
    result = as.character(result)
    poly = NULL
    for(i in 1:length(result)){
      temp = unlist(strsplit(result[i], ")"))
      temp = temp[2]
      temp = paste(temp,"= 0")
      poly = append(poly,temp)
    }
    paste(poly)
    
  })
  
  output$opt.val <- renderText({
    objective = as.numeric(unlist(strsplit(input$txt4,",")))
    
    dvLines = unlist(strsplit(input$txt5," "))
    decision = list()
    for(i in 1:length(dvLines)){ #Getting the appropriate list of vectors for the decision variables. Extracting using a pattern of string manipulation (must have seen instructions)
      decision = c(decision,list(as.numeric(unlist(strsplit(dvLines[i],",")))))
    }
    
    constraints = as.numeric(unlist(strsplit(input$txt6,",")))
    isMax = as.logical(input$txt7)
    problem = as.logical(input$txt8)
    
    validate(
      need(is.na(objective)!=T && is.na(decision)!=T && is.na(isMax)!=T && is.na(problem)!=T, "Please enter valid values (Please follow the input formatting)")
    )
    
    result = simplex(tableauMaker(objective,decision,constraints,isMax,problem), isMax,problem)
    if(is.na(result)==T){
      paste("NA")
    }else{
      paste(result[[3]])
    }
  })
  
  output$tableau <- renderTable({
    objective = as.numeric(unlist(strsplit(input$txt4,",")))
    
    dvLines = unlist(strsplit(input$txt5," "))
    decision = list()
    for(i in 1:length(dvLines)){ #Getting the appropriate list of vectors for the decision variables. Extracting using a pattern of string manipulation (must have seen instructions)
      decision = c(decision,list(as.numeric(unlist(strsplit(dvLines[i],",")))))
    }
    
    constraints = as.numeric(unlist(strsplit(input$txt6,",")))
    isMax = as.logical(input$txt7)
    problem = as.logical(input$txt8)
    
    validate(
      need(is.na(objective)!=T && is.na(decision)!=T && is.na(isMax)!=T && is.na(problem)!=T, "Please enter valid values (Please follow the input formatting)")
    )
    
    result = simplex(tableauMaker(objective,decision,constraints,isMax,problem), isMax,problem)
    if(is.na(result)==T){
      M = matrix("NA", ncol=1,nrow=1)
      colnames(M) = "NA"
      M
    }else{
      M = result[[1]]
      M
    }
  })
  
  output$basicSoln <- renderTable({
    objective = as.numeric(unlist(strsplit(input$txt4,",")))
    
    dvLines = unlist(strsplit(input$txt5," "))
    decision = list()
    for(i in 1:length(dvLines)){ #Getting the appropriate list of vectors for the decision variables. Extracting using a pattern of string manipulation (must have seen instructions)
      decision = c(decision,list(as.numeric(unlist(strsplit(dvLines[i],",")))))
    }
    
    constraints = as.numeric(unlist(strsplit(input$txt6,",")))
    isMax = as.logical(input$txt7)
    problem = as.logical(input$txt8)
    
    validate(
      need(is.na(objective)!=T && is.na(decision)!=T && is.na(isMax)!=T && is.na(problem)!=T, "Please enter valid values (Please follow the input formatting)")
    )
    
    result = simplex(tableauMaker(objective,decision,constraints,isMax,problem), isMax,problem)
    if(is.na(result)==T){
      B = matrix("NA", ncol=1,nrow=1)
      colnames(B) = "NA"
      B
    }else{
      B = result[[2]]
      B
    }
  })
  
  output$shipping <- renderTable({
    objective = as.numeric(unlist(strsplit(input$txt4,",")))
    
    dvLines = unlist(strsplit(input$txt5," "))
    decision = list()
    for(i in 1:length(dvLines)){ #Getting the appropriate list of vectors for the decision variables. Extracting using a pattern of string manipulation (must have seen instructions)
      decision = c(decision,list(as.numeric(unlist(strsplit(dvLines[i],",")))))
    }
    
    constraints = as.numeric(unlist(strsplit(input$txt6,",")))
    isMax = as.logical(input$txt7)
    problem = as.logical(input$txt8)
    
    validate(
      need(is.na(objective)!=T && is.na(decision)!=T && is.na(isMax)!=T && is.na(problem)!=T, "Please enter valid values (Please follow the input formatting)")
    )
    
    result = simplex(tableauMaker(objective,decision,constraints,isMax,problem), isMax,problem)
    if(is.na(result)==T){
      P = matrix("NA", ncol=1,nrow=1)
      colnames(P) = "NA"
      P
    } else if(problem == F){
      P = matrix("Not Applicable", ncol=1,nrow=1)
      colnames(P) = "NA"
      P
    }
    else{
      P = result[[4]]
      P
    }
  })
} # server




##### FUNCTION IMPLEMENTATIONS #####

#This function sorts out the x-values in ascending order. It also orders the y-values according to the sorted x-values.
sortData = function(x,y){
  if(length(x) != length(y)){
    return(NA)
  }
  else{
    temp = data.frame(row.names = 1:length(x), key = x, val = y)
    x = x[order(x)]
    val = c()
    for(i in 1:length(x)){
      foundKey  = temp[match(x[i],temp$key),]
      val[i] = foundKey$val
    }
    result = list(x,val)
    return(result)
  }
}

#This is the main function for Quadratic Spline Interpolation that takes in a list of vectors containing the x-values and y-values (data points).
#It returns a list containing all the polynomials per interval. It also gives the estimated value of f_n(x) using the appropriate function for an interval
#based on the value of x.
poly.qsi = function(data, x){
  data = sortData(data[[1]], data[[2]])
  X = data[[1]] #The data points
  X = X[order(X)] #Sorting in ascending order, if not ordered
  Y = data[[2]] #The f(x)'s or values of x's

  #If the value we're trying to approximate is not within the range of values of x, return NA
  if(x<X[1] || x>X[length(X)]){return(NA)}

  dPoints = length(X) #The number of data points
  intervals = dPoints-1

  # totalC1 = 2*n -1 #Number of equations in Condition 1
  varHolder = c("*a_","*b_","+1*c_")
  # vars = NULL
  # for(z in 1:totalC1/2){
  #   vars = append(vars,varHolder)
  # }
  stringEqns = c()

  #Equations from internal Knots
  for(i in 2:intervals){
    f_x = Y[i] #Holds the current f(x) on the current ith index
    a = (X[i])^2; b = X[i];
    currLine = ""
    for(j in 1:length(varHolder)){
      index = i-1
      var = gsub(" ","",paste(varHolder[j],index))

      if(varHolder[j] == "*a_"){
        term = gsub(" ","", paste(a,var))
        currLine = paste0(currLine,term)
      }

      else if(varHolder[j] == "*b_"){
        term = gsub(" ","", paste(b,var))
        term = paste("+",term)
        currLine = paste(currLine,term)
      }

      else if(varHolder[j] == "+1*c_"){
        term = paste(var,paste0("+ -",f_x))
        currLine = paste(currLine, term)
        stringEqns = append(stringEqns,currLine)
        currLine = ""
      }
    }
    for(j in 1:length(varHolder)){
      index = i
      var = gsub(" ","",paste(varHolder[j],index))

      if(varHolder[j] == "*a_"){
        term = gsub(" ","", paste(a,var))
        currLine = paste0(currLine,term)
      }

      else if(varHolder[j] == "*b_"){
        term = gsub(" ","", paste(b,var))
        term = paste("+",term)
        currLine = paste(currLine,term)
      }

      else if(varHolder[j] == "+1*c_"){
        term = paste(var,paste0("+ -",f_x))
        currLine = paste(currLine, term)
        stringEqns = append(stringEqns,currLine)
        currLine = ""
      }
    }
  } #End of extracting Internal Knots equations

  endPoints = c(1,dPoints)
  stringEndPoints = c()
  varHolder = c("*a_","*b_","+1*c_")
  for(k in 1:length(endPoints)){ #Extracting the Endpoint equations
    currLine = "";
    a = (X[endPoints[k]])^2; b = (X[endPoints[k]]); f_x = Y[endPoints[k]];

    for(l in 1:length(varHolder)){
      index = endPoints[k]
      if(index==endPoints[2]){index=dPoints-1}
      var = gsub(" ","",paste(varHolder[l],index))

      if(varHolder[l]=="*a_"){
        term = gsub(" ","", paste(a,var))
        currLine = paste0(currLine,term)
      }
      else if(varHolder[l]=="*b_"){
        term = gsub(" ","", paste(b,var))
        term = paste("+",term)
        currLine = paste(currLine,term)
      }
      else if(varHolder[j] == "+1*c_") {
        term = paste(var,paste0("+ -",f_x))
        currLine = paste(currLine, term)
        stringEndPoints = append(stringEndPoints,currLine)
        currLine = ""
        break;
      }
    }
  } #End of extracting the Endpoint equations
  stringEqns = append(stringEqns,stringEndPoints)

  finalEqns = c()
  varHolder = c("*a_","+ 1*b_")

  for(m in 2:intervals){ #Extracting the last equations based on Condition 3
    a = (X[m])*2
    currLine = ""

    for(j in 1:length(varHolder)){
      index = m-1
      var = gsub(" ","",paste(varHolder[j],index))

      if(varHolder[j] == "*a_"){
        term = gsub(" ","", paste(a,var))
        currLine = paste0(currLine,term)
      }

      else if(varHolder[j] == "+ 1*b_"){ #after adding a, b is the end of the string equation. Thus, we also add the right hand side
        varRight = gsub(" ","", paste(a,paste(varHolder[1],index+1))) #adding the RHS of the equation
        varRight = paste0("+ -",varRight)

        varRight = paste0(varRight, paste(gsub("+ ","-", varHolder[2])), index+1)
        term = paste(var,varRight)

        currLine = paste(currLine, term)
        finalEqns = append(finalEqns,currLine)
        currLine = ""
        break;
      }

    }
  }
  stringEqns = append(stringEqns,finalEqns)

  varList = c("a_","b_","c_")
  finalVarList = list() #list of vectors for the 3n-1 unknowns
  subscript = 1 #initially at one
  for(i in 1:intervals){

    temp = c()
    for(j in 1:length(varList)){
      if(i == 1 && varList[j]=="a_"){} #do nothing if a_1
      else{
        temp = append(temp, gsub(" ","",paste(varList[j],subscript)))
      }
    }
    subscript = subscript+1;
    finalVarList = append(finalVarList,temp) #holds the list of variables (e.g. b_1,c_2,...etc.)
  }

  f_name = toString(unlist(finalVarList))
  f_name = paste0(paste("function",paste0("(",f_name)),")")

  qsi.fxns = list()
  for(i in 1:length(stringEqns)){
    qsi.fxns[[i]] = paste(f_name, stringEqns[i])
    qsi.fxns[[i]] = eval(parse(text=qsi.fxns[[i]]))
  }
  #END OF GETTING ALL POLYNOMIALS PER INTERVAL

  min; check=FALSE;
  for(i in 1:length(X)){ #Finding the minimum range for x
    if(x>=X[i] && x<=X[i+1]){
      min = X[i]
      check=TRUE;
      break;
    }
  }

  temp = data.frame(key = X, val = Y)
  foundKey  = temp[match(min,temp$key),]
  key = foundKey$key
  whichDegree = match(key,X)

  solved = GaussJordan(qsiCorrector(data))
  solved = solved[[1]] #Getting the solved values
  solved = c(0,solved) #adding a_1, which is 0, to the array of values

  choices = list()
  count = 1;
  for(i in seq(1,length(solved),3)){
    a = solved[i]; b = solved[i+1]; c = solved[i+2];
    choices[[count]] = c(a,b,c)
    count = count+1;
  }

  choice = as.vector(choices[[whichDegree]])
  count = 2; iter = 1;
  funct=c();degree = 2;
  for(i in 1:length(choice)){
    if(degree==2){
      term = paste0("*x^",degree)
      term = paste0(choice[i],term)
      funct = append(funct, term)
      degree = degree-1
    }
    else{
      if(degree==0){
        term = paste("+",choice[i])
      }
      else{
        term = paste0("*x^",degree)
        term = paste0(choice[i],term)
        term = paste("+",term)
      }
      funct = append(funct, term)
      degree = degree-1
    }
  }
  funct = paste(funct,collapse=" ")
  funct = eval(parse(text=paste("function (x)",funct)))

  result = list("qsi.fxns"=qsi.fxns,"y"=funct(x))
  return(result)
}

########################## END OF poly.qsi() function ###################################################################

#This function returns the correct polynomial functions (i.e. excluding a_1) based on the given data points.
qsiCorrector = function(data){
  data = sortData(data[[1]], data[[2]])
  X = data[[1]] #The data points
  X = X[order(X)] #Sorting in ascending order, if not ordered
  Y = data[[2]] #The f(x)'s

  dPoints = length(X) #The number of data points
  intervals = dPoints-1


  varHolder = c("*a_","*b_","+1*c_")

  stringEqns = c()

  #Equations from internal Knots
  for(i in 2:intervals){
    f_x = Y[i] #Holds the current f(x) on the current ith index
    a = (X[i])^2; b = X[i];
    currLine = ""
    for(j in 1:length(varHolder)){
      index = i-1
      var = gsub(" ","",paste(varHolder[j],index))

      if(varHolder[j] == "*a_" && index != 1){
        term = gsub(" ","", paste(a,var))
        currLine = paste0(currLine,term)
      }

      else if(varHolder[j] == "*b_"){
        term = gsub(" ","", paste(b,var))
        if(index==1){}
        else{term = paste("+",term)}
        currLine = paste(currLine,term)
      }

      else if(varHolder[j] == "+1*c_"){
        term = paste(var,paste0("+ -",f_x))
        currLine = paste(currLine, term)
        stringEqns = append(stringEqns,currLine)
        currLine = ""
      }
    }
    for(j in 1:length(varHolder)){
      index = i
      var = gsub(" ","",paste(varHolder[j],index))

      if(varHolder[j] == "*a_"){
        term = gsub(" ","", paste(a,var))
        currLine = paste0(currLine,term)
      }

      else if(varHolder[j] == "*b_"){
        term = gsub(" ","", paste(b,var))
        if(index==1){}
        else{term = paste("+",term)}
        currLine = paste(currLine,term)
      }

      else if(varHolder[j] == "+1*c_"){
        term = paste(var,paste0("+ -",f_x))
        currLine = paste(currLine, term)
        stringEqns = append(stringEqns,currLine)
        currLine = ""
      }
    }
  } #End of extracting Internal Knots equations

  endPoints = c(1,dPoints)
  stringEndPoints = c()
  varHolder = c("*a_","*b_","+1*c_")
  for(k in 1:length(endPoints)){ #Extracting the Endpoint equations
    currLine = "";
    a = (X[endPoints[k]])^2; b = (X[endPoints[k]]); f_x = Y[endPoints[k]];

    for(l in 1:length(varHolder)){
      index = endPoints[k]
      if(index==endPoints[2]){index=dPoints-1}
      var = gsub(" ","",paste(varHolder[l],index))
      if(var == "*a_1"){next}
      if(varHolder[l]=="*a_"){
        term = gsub(" ","", paste(a,var))
        currLine = paste0(currLine,term)
      }
      else if(varHolder[l]=="*b_"){
        term = gsub(" ","", paste(b,var))
        if(index==1){}
        else{term = paste("+",term)}
        currLine = paste(currLine,term)
      }
      else if(varHolder[j] == "+1*c_"){
        term = paste(var,paste0("+ -",f_x))
        currLine = paste(currLine, term)
        stringEndPoints = append(stringEndPoints,currLine)
        currLine = ""
        break;
      }
    }

  } #End of extracting the Endpoint equations
  stringEqns = append(stringEqns,stringEndPoints)

  finalEqns = c()
  varHolder = c("*a_","+ 1*b_")

  for(m in 2:intervals){
    a = (X[m])*2
    currLine = ""

    for(j in 1:length(varHolder)){
      index = m-1
      var = gsub(" ","",paste(varHolder[j],index))

      if(varHolder[j] == "*a_" && index != 1){
        term = gsub(" ","", paste(a,var))
        currLine = paste0(currLine,term)
      }

      else if(varHolder[j] == "+ 1*b_"){ #after adding a, b is the end of the string equation. Thus, we also add the right hand side

        varRight = gsub(" ","", paste(a,paste(varHolder[1],index+1))) #adding the RHS of the equation
        varRight = paste0("+ -",varRight)

        varRight = paste0(varRight, paste(gsub("+ ","-", varHolder[2])), index+1)
        if(index==1){term = paste("1*b_1",varRight)}
        else {term = paste0(var,varRight)}

        currLine = paste(currLine, term)
        finalEqns = append(finalEqns,currLine)
        currLine = ""
        break;
      }

    }
  }
  stringEqns = append(stringEqns,finalEqns)
  # print(stringEqns)
  varList = c("a_","b_","c_")
  finalVarList = list() #list of vectors for the 3n-1 unknowns
  subscript = 1 #initially at one
  for(i in 1:intervals){

    temp = c()
    for(j in 1:length(varList)){
      if(i == 1 && varList[j]=="a_"){} #do nothing if a_1
      else{
        temp = append(temp, gsub(" ","",paste(varList[j],subscript)))
      }
    }
    subscript = subscript+1;
    finalVarList = append(finalVarList,temp) #holds the list of variables (e.g. b_1,c_2,...etc.)
  }

  f_name = toString(unlist(finalVarList))
  f_name = paste0(paste("function",paste0("(",f_name)),")")

  qsi.fxns = list()
  for(i in 1:length(stringEqns)){
    qsi.fxns[[i]] = paste(f_name, stringEqns[i])
    qsi.fxns[[i]] = eval(parse(text=qsi.fxns[[i]]))
  }
  return(qsi.fxns)
}
#'
#' ############################## HELPER FUNCTIONS #######################################################

# This function below checks the list of variables from the function parameters and helps determine if the system of equations
# have an identical set of variables. It returns TRUE if the equations have a similar set variables to one another. Otherwise, returns FALSE.
compareList = function(li){
  stopifnot(length(li) > 1)
  l <- length(li)
  res <- lapply(li[-1],function(X,x) identical(X,x),x=li[[1]])
  res <- all(unlist(res))
  res
}

# This function below constructs the augmented matrix. It uses various string manipulation to extract the coefficients, variables, and
# constants from a system of equations.
AugCoeffMatrix = function(system){
  system_size = length(system) #This variable contains the number of equations from the system
  varList = list() #The varList shall contain the list of vectors containing the variables from each function parameter.

  #To get the variables, first we deparse the functions and take the first element after deparsing. Then, we remove the spaces and parentheses.
  #Then we sort the vectors of variables from each function alphabetically.
  for(i in 1:system_size){
    varList[[i]] = unlist(strsplit(gsub("\\)","", substring(gsub(" ","", deparse(system[i], width.cutoff = 500)[1]), 15)), ","))
    # varList[[i]] = varList[[i]][order(nchar(varList[[i]]), varList[[i]])] #Sorts the variable lists alphabetically
  }
  if(compareList(varList)==TRUE){ #If all equations have similar variables, we proceed with extracting the coefficients
    finalVar = varList[[1]] #This variable holds the final set of variables that will be used to extract the coefficients
    finalVar = finalVar[!grepl(paste0("a_1", collapse = "|"), finalVar)]

    # The coeffs variable shall contain the list of vectors containing the coefficients per function.
    # Its size is determined by the final set of variables found.
    coeffs <- vector("list", length = length(finalVar))
    constants = c() #This variable shall contain the constants


    for(j in 1:system_size){
      #The rawCoeffs vector contains the terms per function that will be used to extract the coefficients/constants
      #First thing to do is to deparse the function and get the second element. Then, we remove the parentheses and split the terms by removing the "+" sign.
      rawCoeffs = unlist(strsplit(gsub("\\)", "", gsub(" ","",deparse(system[j], width.cutoff = 500)[2])), "\\+"))
      if(length(grep("_", rawCoeffs, invert=TRUE))==0){ #If the constant is not found, we append a zero (0) to the constants vector.
        constants = append(constants,0)
      }
      else if(grep("_", rawCoeffs, invert = TRUE)>0){ #If the constant is found, we extract the index from the rawCoeffs and access the constant and muliply it by -1.
        constants = append(constants, eval(parse(text = paste0("-1*",(rawCoeffs[grep("_", rawCoeffs, invert = TRUE)])))))
      }

      for(k in 1:length(finalVar)){
        elementVar = grep(finalVar[k], rawCoeffs, value=TRUE) #The elementVar variable will hold a vector of terms excluding the constant.
        elementVar =elementVar[!grepl(paste0("a_1", collapse = "|"), elementVar)]
        if(length(elementVar)>0 && is.character(elementVar)==TRUE){#If coefficient is found depending on the variable, it is added to the coeffs list of variables.
          coeffs[[k]] = append(coeffs[[k]],eval(parse(text=gsub("\\*","",gsub(finalVar[k],"",elementVar)))))
        }
        else if(length(elementVar)==0 && is.character(elementVar)==TRUE){ #If the coefficient for a specific variable is not found, we append a zero (0) with respect to the variable.
          coeffs[[k]] = append(coeffs[[k]],0)
        }

      }

    }
    #The newCOl variable will contain the appropriate column names for the augmented matrix, including the RHS or Right Hand Side column
    newCol = finalVar
    newCol = append(newCol, "RHS")

    #augmList is a vector containing the unlisted coefficients and constants
    augmList = unlist(coeffs)
    augmList = append(augmList, constants)

    #coeffMatrix contains the actual matrix for coefficients
    coeffMatrix = matrix(augmList, ncol = length(finalVar), nrow = length(finalVar))
    constMatrix = matrix(constants)

    #augMatrix is the created augmented matrix output, including the appropriate row/column names.
    augMatrix = cbind(coeffMatrix,constMatrix)

    dimnames(augMatrix) = list(1:length(finalVar), newCol)

    #The result list is the actual returned value and it contains the augmented matrix and the set of variables found.
    result = list("variables" = finalVar, "augcoeffMatrix" = augMatrix)
    return(result)
  }
  else{
    print("NA")
  }
}

#This function below swaps certain rows from a given matrix. This is essentially important for employing pivoting.
swapRows = function(mat, i, j){
  matrix = mat
  rows = nrow(mat)

  temp = matrix[i,]
  matrix[i,] = matrix[j,]
  matrix[j,] = temp

  return(matrix)
}

#This function below is an implementation of Gaussian elimination for solving a system of linear equations
GaussianElim = function(system){
  a = AugCoeffMatrix(system)
  varList = a[[1]] #This is the list of variables from the given system of linear equations
  a = a[[2]] #This holds the augmented matrix
  n = nrow(a)

  for(i in 1:n){
    #Finding the pivot row
    pivotRow = as.vector(which(abs(a[,i]) == max(abs(a[,i])))) #tells which row index of the ith column has the max (absolute) value
    pivotRow = pivotRow[1]

    if(a[pivotRow, i]==0){ #If the max value of the pivot row is zero (0), the function stops since it has no solution
      print("No unique solution exists.")
      break
    }
    if(pivotRow == as.vector(which(a[,i] == a[i,i]))[1]){ #If the main diagonal already holds the pivot row, there's no need for swapping rows
      a = swapRows(a, pivotRow, i)
    }

    for(j in i+1:n){ #This algorithm below essentially turns the matrix into an upper triangle matrix
      if(j>n)break;
      pivotElement = a[i,i]
      multiplier = a[j,i]/pivotElement
      normalizedRow = multiplier * a[i,]
      a[j,] = a[j,] - normalizedRow

      a = round(a,4) #Rounding the values of the matrix to a minimum of 4 decimals
    }
  }
  solvedValues = c() #This variable will hold the solved variable values when employing Backward Substitution

  for(i in n:1){#Employing Backward Substitution after the Forward Elimination
    solvedValues[i] = round((a[i,n+1] - sum(a[i, (i+1):n] * solvedValues[(i+1):n])) / a[i,i], 4)
  }

  result = list("solutionSet"=solvedValues, "Variables"=varList, "matrix"=a)

  return(result)
}

#This function below is an implementation of the Gauss-Jordan elimination for solving a system of linear equations
GaussJordan = function(system){
  a = AugCoeffMatrix(system)
  varList = a[[1]] #This is the list of variables from the given system of linear equations
  a = a[[2]] #This holds the augmented matrix
  n = nrow(a)
  x = 1;
  for(i in x:n){
    if(x != n){
      pivotR_index = which(abs(a[,x]) == max(abs(a[i:n,x]))) #tells which row index of the ith column has the max (absolute) value
      pivotR_index = pivotR_index[1]

      pivotR_index = as.integer(pivotR_index)
      pivotRow = a[pivotR_index,]
      if(a[pivotR_index, x]==0){ #If the max value of the pivot row is zero (0), the function stops since it has no solution
        print("No unique solution exists.")
        break
      }
      a = swapRows(a, pivotR_index, x)
    }

    a[i,] = a[i,]/a[i,i]
    for(j in 1:n){ #This algorithm below essentially turns the matrix into an identity matrix
      if(i==j)next;
      normalizedRow = a[j,i] * a[i,]
      a[j,] = a[j,] - normalizedRow
    }
    x = x+1;
  }
  rows = nrow(a)
  cols = ncol(a)
  solvedValues = a[,cols] #This variable will hold the solved variable values
  # a[,ncol(a)] = round(a[,ncol(a)], 4) #Rounding the values to 4 decimal places
  result = list("solutionSet"=solvedValues, "Variables"=varList, "matrix"=a)
  return(result)
}

#=====================================START of SIMPLEX======================================
simplex = function(tableau, isMax, problem){
  if(problem == TRUE && is.list(tableau)==TRUE || problem == TRUE && is.na(tableau)==TRUE){ #Must have used the tableauMaker in order to get the results from the problem
    result = tableau
    return(result)
  }
  
  if(isMax == TRUE && is.list(tableau) == FALSE){
    tempTab = tableau # Assuming that the tableau is a matrix already AND in the form from the tableau as seen from Simplex PDF File
  }
  else if(isMax == FALSE && is.list(tableau)==FALSE){ #For Minimization without TableauMaker
    #Assuming that the user entered a tableau without the tableauMaker and the tableau looks like a Maximization tableau (no transposing yet)
    # tableau = tableau[[1]]
    tableau = tableau
    varNum = (ncol(tableau)-2)/2
    extractObj = c()
    extractObj = tableau[nrow(tableau),]
    colnames(extractObj) = NULL
    rownames(extractObj) = NULL
    extractObj = as.vector(extractObj)
    objective = c()
    
    for(i in 1:varNum){
      objective = append(objective,extractObj[i])
    }
    objective = -1*objective
    
    constraints = list()
    
    constNum = nrow(tableau)-1
    for(i in 1:constNum){
      line = c()
      for(j in 1:varNum){
        line = append(line,tableau[i,j])
      }
      constraints = c(constraints,list(line))
    }
    
    solutions = tableau[,ncol(tableau)]
    colnames(solutions) = NULL
    rownames(solutions) = NULL
    solutions = solutions[-length(solutions)]
    solutions = as.vector(solutions)
    
    tableau = tableauMaker(objective,constraints,solutions, FALSE,FALSE)
    tempTab = tableau[[1]]
  } else{
    tempTab = tableau[[1]]
  }
  basicVars = tableau[[2]]
  
  while(TRUE){
    lastRow = tempTab[nrow(tempTab),][-length(tempTab[nrow(tempTab),])]
    
    negatives = all(lastRow>=0) #tells if all values in the last row are negative or not
    if(negatives==TRUE){break}
    
    
    
    #Start of Solving per Iteration
    
    #Get the index of the negative value with the largest magnitude in the bottom row
    pcIndex = max(abs(which(lastRow<0)))
    
    tRatios = tempTab[,ncol(tempTab)][-length(tempTab[,ncol(tempTab)])] #Copies the RHS excluding the last bottom row
    
    tRatios = tRatios/tempTab[,pcIndex][-length(tempTab[,pcIndex])] #Getting the test ratios
    
    prIndex = which.max(1/tRatios) #tells the smallest nonnegative value's index for the test ratios
    
    pivotE = tempTab[prIndex,pcIndex]
    pivotRow = tempTab[prIndex,]
    nPR = pivotRow/pivotE #normalization
    tempTab[prIndex,] = nPR
    
    #Elimination Part
    for(i in 1:nrow(tempTab)){
      if(i==prIndex){}#do nothing for normalized pivot row
      else{
        C = tempTab[i,pcIndex]
        tempTab[i,] = (-1)*C*nPR + tempTab[i,]
      }
    }
    
    if(isMax == TRUE){
      basicSoln = c() #<--Critical point Minimization/Minimization Basic Solution
      for(i in 1:ncol(tempTab)){
        if(sum(tempTab[,i])==1){ #If the column is active, find its solution
          basicSoln = append(basicSoln,tempTab[,ncol(tempTab)][grep(1,tempTab[,i])])
        }
        else{
          basicSoln = append(basicSoln,0) #Else if it's inactive, append a zero to the basic solution
        }
      }
      basicSoln = basicSoln[-length(basicSoln)] #Removing the last unnecessary element for the basic solution
      tempTab = round(tempTab,4)
    }
    else{
      basicSoln = c()
      basicSoln = tempTab[nrow(tempTab),]
      basicSoln = basicSoln[-length(basicSoln)] #Removing the last unnecessary element for the basic solution
      basicSoln[length(basicSoln)] = tempTab[nrow(tempTab),ncol(tempTab)]
      tempTab = round(tempTab,4)
      
    }
    
  } #End of while loop
  
  basicSoln = unname(basicSoln)
  basicSoln = matrix(basicSoln, ncol=length(basicSoln), nrow=1)
  basicVars = basicVars[-length(basicVars)]
  colnames(basicSoln) = basicVars
  
  opt = basicSoln[length(basicSoln)]
  opt = as.vector(opt)
  
  result = list("final.tableau"=tempTab,"basic.solution"=basicSoln, "opt.val"=opt)
  return(result)
  
  # } #End of Else condition
}

tableauMaker = function(objective,constraints,solutions,isMax, problem){
  if((var(lengths(constraints))==0)==FALSE && problem==F){return(NA)}
  else if(length(objective) != length(constraints[[1]]) && problem==F){return(NA)}
  else if(length(solutions) != length(constraints) && problem==F){return(NA)}
  else if(problem==TRUE){
    m.Objective = objective
    
    m.Constraints = list( c(1,1,1,0,0,0,0,0,0,0,0,0,0,0,0),c(0,0,0,1,1,1,0,0,0,0,0,0,0,0,0),
                          c(0,0,0,0,0,0,1,1,1,0,0,0,0,0,0),c(0,0,0,0,0,0,0,0,0,1,1,1,0,0,0),
                          c(0,0,0,0,0,0,0,0,0,0,0,0,1,1,1),c(1,0,0,1,0,0,1,0,0,1,0,0,1,0,0),
                          c(0,1,0,0,1,0,0,1,0,0,1,0,0,1,0),c(0,0,1,0,0,1,0,0,1,0,0,1,0,0,1)
    )
    
    m.Solution = solutions
    
    m.Objective = -1*m.Objective
    
    M = 10000000 #Initializing M for Big M implementation
    
    #Creating an empty matrix for the preliminary tableau
    m.InitTableau = matrix(NA,ncol=15,nrow=9)
    
    for(i in 1:length(m.Constraints)){ #Filling up the initial matrix (without Slacks and Artificials)
      m.InitTableau[i,] = m.Constraints[[i]]
    }
    m.InitTableau[nrow(m.InitTableau),] = m.Objective
    
    
    
    #Creating an empty matrix for the right side of the preliminary tableau
    m.OtherVars = matrix(NA,ncol=13,nrow=9)
    m.vars = list( c(-1,0,0,0,0,0,0,0,1,0,0,0,0),c(0,-1,0,0,0,0,0,0,0,1,0,0,0),
                   c(0,0,-1,0,0,0,0,0,0,0,1,0,0),c(0,0,0,-1,0,0,0,0,0,0,0,1,0),
                   c(0,0,0,0,-1,0,0,0,0,0,0,0,1),c(0,0,0,0,0,1,0,0,0,0,0,0,0),
                   c(0,0,0,0,0,0,1,0,0,0,0,0,0),c(0,0,0,0,0,0,0,1,0,0,0,0,0)
    )
    
    for(i in 1:length(m.vars)){ #Filling up the other part of the matrix (with Slacks and Artificials)
      m.OtherVars[i,] = m.vars[[i]]
    }
    m.OtherVars[nrow(m.OtherVars),] = c(0,0,0,0,0,0,0,0,M,M,M,M,M)
    
    #Combining the parts of the preliminary matrix
    m.Solution = append(m.Solution,0)
    m.OtherVars = cbind(m.OtherVars,m.Solution)
    colnames(m.OtherVars) = NULL
    
    m.Tableau = cbind(m.InitTableau, m.OtherVars)
    
    #Naming the Columns
    m.ColNames = c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","s1","s2","s3","s4","s5","s6","s7","s8",
                   "a1","a2","a3","a4","a5", "Solutions")
    colnames(m.Tableau) = m.ColNames
    
    
    hasM = TRUE
    while(hasM){
      #Eliminating the M's from the Artificial Variables
      artifIndex = 0
      for(i in 1:5){#Check which artificial variable index has the first instance of M
        check = m.Tableau[,23+i]
        if((check[length(check)] == M)){
          artifIndex = 23+i
          hasM = TRUE
          break;
        }
        else hasM = FALSE
      }
      if(hasM==FALSE)break;
      m.Eliminate = which(m.Tableau[,artifIndex]==1) # tells which row index to base our M elimination
      
      m.Tableau[nrow(m.Tableau),] = -M*m.Tableau[m.Eliminate,] + m.Tableau[nrow(m.Tableau),] #Changing the last row only
    }
    
    #Check if solution exists
    basicVars=c()
    nonBasicVars=c()
    
    for(i in 1:ncol(m.Tableau)){
      if(sum(m.Tableau[,i])==1){
        basicVars = append(basicVars,colnames(m.Tableau)[i]) #get the column name of the column that has only one non-negative value
      }
      else{
        nonBasicVars = append(nonBasicVars,colnames(m.Tableau)[i])
      }
    }
    nonBasicVars = nonBasicVars[-length(nonBasicVars)] #removing "solutions" colname from non-basicVars
    
    #Now checking if the basicVars are all non-negative to assure that the solution exist
    checkBasicVals = c()
    for(i in 1:length(basicVars)){
      for(j in 1:ncol(m.Tableau)){
        if(colnames(m.Tableau)[j]==basicVars[i]){
          checkBasicVals = append(checkBasicVals, m.Tableau[which(m.Tableau[,j]==1),ncol(m.Tableau)])
        }
      }
    }
    checkBasicVals = as.vector(checkBasicVals); checkBasicVals = checkBasicVals[-length(checkBasicVals)]
    if(all(checkBasicVals>=0)){
      #Do nothing if we have a solution
    } else{
      return(NA)
    }
    
    #If there's a solution, set the row names
    rownames(m.Tableau) = c(basicVars[4:8],basicVars[1:3],"Z")
    m.Tableau = m.Tableau[-nrow(m.Tableau),]
    #Start of Simplex procedure based on Tableau
    
    #initial cJ and cB
    cJ = append(m.Objective,c(0,0,0,0,0,0,0,0,M,M,M,M,M)); cJ = matrix(cJ,nrow=1,ncol=length(cJ),byrow=TRUE)
    colnames(cJ) = m.ColNames[-length(m.ColNames)]
    cJ[1:15]=cJ[1:15]*-1
    Entering = NULL; Departing = NULL; keyElement=NULL;
    iter=0
    finalSoln=c()
    while(TRUE){
      if(iter==0){
        cB = c(M,M,M,M,M,0,0,0)
        iter = iter+1
        
        zJ = c()
        for(i in 1:ncol(m.Tableau)){
          zJ = append(zJ,sum(m.Tableau[,i][1:8]*cB)) #Getting zJ per column
        }
        # zJ = zJ[-length(zJ)]
        tempzJ = zJ[-length(zJ)]
        minusCjZj = tempzJ-cJ #Excluding the solution column
        
        positives = all(minusCjZj<=0) #Optimality for Minimization
        if(positives==TRUE){break}
        
        #Get the index of the negative value with the largest magnitude in the bottom row
        pcIndex = which(minusCjZj==max(minusCjZj,na.rm = T))[1]
        
        tRatios = m.Tableau[,ncol(m.Tableau)][-length(m.Tableau[,ncol(m.Tableau)])] #Copies the RHS excluding the last bottom row
        colnames(tRatios)=NULL
        tRatios = as.vector(tRatios)
        tRatios = tRatios/m.Tableau[,pcIndex][-length(m.Tableau[,pcIndex])] #Getting the test ratios
        
        prIndex = which.max(1/tRatios) #tells the smallest nonnegative value's index for the test ratios
        
        pivotE = m.Tableau[prIndex,pcIndex]
        pivotRow = m.Tableau[prIndex,]
        
        Entering = pcIndex
        Departing = prIndex
        keyElement = pivotE
        
      } else{
        
        #Now we have to leave blank the Departing variable in the tableau
        departName = rownames(m.Tableau)[Departing]
        if(length(grep("s",departName))==0){
          leaveBlank = which(colnames(m.Tableau)==departName) #the index of the emptying column
          m.Tableau[,leaveBlank] = NA
          cJ[,leaveBlank] = NA
        }else{iter=iter+1} #do nothing if slack variable is the departing variable
        
        #Switching the variable names from row and column (Entering and Leaving Variables)
        rownames(m.Tableau)[Departing] = colnames(m.Tableau)[Entering]
        
        #Normalization is turning other elements other than pivot element as zeroes
        for(i in 1:length(m.Tableau[,Entering])){
          # if(i==length(m.Tableau[,Entering])) break;
          if((m.Tableau[,Entering])[i] > 0 && i!=Departing[1]){
            m.Tableau[i,] = m.Tableau[i,]-m.Tableau[Departing,]
          }
          else if((m.Tableau[,Entering])[i] < 0 && i!=Departing[1]){
            m.Tableau[i,] = m.Tableau[i,]+m.Tableau[Departing,]
          }
        }
        
        cB = c() #Coefficient of the Basic Variables
        for(i in 1:length(rownames(m.Tableau))){
          cJindex = which(colnames(cJ)==rownames(m.Tableau)[i])
          cB = append(cB,cJ[cJindex])
        }
        zJ = c()
        for(i in 1:ncol(m.Tableau)){
          zJ = append(zJ,sum(m.Tableau[,i][1:8]*cB)) #Getting zJ per column
        }
        tempzJ = zJ[-length(zJ)]
        minusCjZj = tempzJ-cJ #Excluding the solution column
        
        positives = all(minusCjZj<=0,na.rm = T) #Optimality for Minimization
        if(positives==TRUE){
          finalSoln = zJ;
          break;}
        
        
        
        #Get the index of the negative value with the largest magnitude in the bottom row
        pcIndex = which(minusCjZj==max(minusCjZj,na.rm = T))[1]
        
        tRatios = m.Tableau[,ncol(m.Tableau)]#Copies the RHS excluding the last bottom row
        colnames(tRatios)=NULL
        tRatios = as.vector(tRatios)
        tRatios = tRatios/m.Tableau[,pcIndex]
        
        prIndex = which.max(1/tRatios)  #tells the smallest nonnegative value's index for the test ratios
        pivotE = m.Tableau[prIndex,pcIndex]
        pivotRow = m.Tableau[prIndex,]
        
        Entering = pcIndex
        Departing = prIndex
        keyElement = pivotE
        
      }
    }
    if(finalSoln[length(finalSoln)]>M*100){return(NA)
    } else{#Making the matrix for the number of items, if feasible
      checkRowNames = c("\\bx1\\b","\\bx2\\b","\\bx3\\b",
                        "\\bx4\\b","\\bx5\\b","\\bx6\\b",
                        "\\bx7\\b","\\bx8\\b","\\bx9\\b",
                        "\\bx10\\b","\\bx11\\b","\\bx12\\b","\\bx13\\b","\\bx14\\b","\\bx15\\b")
      optSoln = m.Tableau[,ncol(m.Tableau)]
      optSoln = matrix(optSoln, ncol = length(optSoln), nrow=1, byrow=T)
      
      colnames(optSoln) = rownames(m.Tableau)
      finalOptSoln = c()
      for(i in 1:15){
        whichIndex = grep(checkRowNames[i],colnames(optSoln))
        if(length(whichIndex)>0){ #check if a variable is included in the final optimal values
          finalOptSoln[i] = optSoln[whichIndex]
        } else{
          finalOptSoln[i] = 0
        }
      }
      shipping.num = matrix(finalOptSoln,ncol=5,nrow=3,byrow = F) #Getting the shipping.num
      colnames(shipping.num) = c("SAC", "SL", "ALB", "CHI", "NYC")
      rownames(shipping.num) = c("DEN","PHO","DAL")
      
      finalOptSoln = matrix(finalOptSoln, ncol = length(finalOptSoln), nrow=1,byrow = T)
      colnames(finalOptSoln) = m.ColNames[1:15]
      final.tableau = rbind(m.Tableau,zJ)
      problem.result = list("final.tableau"=final.tableau,"basic.solution" = finalOptSoln,"opt.val"=finalSoln[length(finalSoln)], "shipping.num" = shipping.num)
      return(problem.result)
    }
  }
  
  else{
    if(isMax==TRUE){
      #make a copy of the inputs
      oB = objective
      cO = constraints
      sO = solutions
    }
    
    else{
      #make a copy of the inputs
      oB = objective
      cO = constraints
      sO = solutions
      
      sO = append(sO,1) #adding a 1 for the objective function's value
      tempMat = matrix(unlist(cO), nrow=length(cO), ncol=length(cO[[1]]), byrow=TRUE)
      tempMat = rbind(tempMat, oB)
      rownames(tempMat) = NULL
      tempMat = cbind(tempMat, sO)
      colnames(tempMat) = NULL
      
      tempMat = t(tempMat) #Transposing the matrix
      
      #Extracting all constraints first
      constraints = list()
      for(i in 1:nrow(tempMat)-1){
        tempVec = tempMat[i,]; tempVec = tempVec[-length(tempVec)]; #Vector per row except last element
        constraints = c(constraints, list(tempVec)) #appending to new constraints
      }
      constraints[[1]] = NULL
      objective = tempMat[nrow(tempMat),]; objective = objective[-length(objective)]
      solutions = tempMat[,ncol(tempMat)]; solutions = solutions[-length(solutions)]
      
      #End of Turning Minimization Problem to Maximization
      oB = objective
      cO = constraints
      sO = solutions
    }
    
    objective = objective*-1
    solutions = c(solutions,0) #appending the initial solution value for the objective
    
    lenConstraints = length(lengths(constraints))
    n = lenConstraints+1 #Objective function + # of constraints
    
    initSoln = c()
    for(i in 0:lenConstraints+1){ initSoln = append(initSoln,0) } #initializing the initial slack + Z columns
    
    for(i in 1:lenConstraints){ #appending the appropriate initSoln for the constraints
      temp = initSoln
      temp[i] = 1
      constraints[[i]] = c(constraints[[i]],temp)
    }
    
    temp = initSoln #Adding the last initial solution row to the matrix
    temp[n] = 1
    objective = c(objective,temp) #appending the appropriate initSoln for the objective
    
    tableau=c();
    for(i in 1:lenConstraints){ #Binding all constraints all together
      tableau = rbind(tableau,constraints[[i]])
    } 
    tableau = rbind(tableau,objective) #Binding the objective to the last row of matrix
    rownames(tableau) = NULL
    tableau = cbind(tableau,solutions) #Lastly, binding the solution values
    colnames(tableau) = NULL
    
    #Naming the rows and columns
    vars = c()
    slacks = c()
    constr = c()
    #append solution and objective to last column and row respectively
    
    if(isMax == TRUE){
      for(i in 1:length(cO[[1]])){
        vars = append(vars,paste0("x",i))
      }
      
      for(i in 1:length(cO)){
        slacks = append(slacks,paste0("s",i))
        constr = append(constr,paste0("constraints_",i))
      }
      
      
      #Also append "Z" to slacks
      slacks = append(slacks,"Z")
      slacks = append(slacks,"solution")
      constr = append(constr, "objective")
    }
    else{
      for(i in 1:length(cO[[1]])){
        slacks = append(slacks,paste0("s",i))
      }
      
      for(i in 1:length(cO)){
        vars = append(vars,paste0("x",i))
        constr = append(constr,paste0("constraints_",i))
      }
      
      vars = append(vars,"Z")
      vars = append(vars,"solution")
      constr = append(constr, "objective")
    }
    
    if(isMax == TRUE) final_colnames = append(vars,slacks) #<-- Critical point for colnames in Max/Min
    else  final_colnames = append(slacks,vars)
    
    #Start of naming rows/cols
    colnames(tableau) = final_colnames
    rownames(tableau) = constr
    
    result = list("tableau" = tableau, "variables" = final_colnames)
  }
  return(result)
}

# Create Shiny object
shinyApp(ui = ui, server = server)