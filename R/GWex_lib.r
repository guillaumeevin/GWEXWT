###===============================###===============================###
### Guillaume Evin
### 18/03/2016, Grenoble
###  LTHE
### guillaume.evin@irstea.fr
###
###  Class functions for GWex: multi-site stochastic model for
### temperature and precipitation data.
###===============================###===============================###


#==============================================================================
# get.version.GWex
#
# Return the current version of EXARGWEX package
#
# @export
#
# @return \item{char}{version}
#
# @author Guillaume Evin
get.version.GWex = function(){
  return('v1.0.0')
}


#==============================================================================
#' Class Gwex
#'
#' Defines a generic \code{\linkS4class{Gwex}} object.
#' GWex objects contain two slots:
#' - the version ('vX.X.X')
#' - the type of variable ('Prec')
#'
#' @name Gwex-class
#' @rdname Gwex-class
#' @exportClass Gwex
#'
#' @author Guillaume Evin
setClass(
  # Set the name for the class
  "Gwex",
  
  # Define the slots
  slots = c(variable = "character",version = "character"),
  
  # Set the default values for the slots
  prototype=list(
    variable = 'Prec',
    version = get.version.GWex()
  ),
  
  # integrity checks
  validity=function(object){
    if(!(object@variable %in% c('Prec'))){
      return("variable must be Prec")
    }else{
      return(TRUE)
    }
  }
)


#==============================================================================
# Method getTypeVar
#
# Create a method to get the value of the variable
# @name getTypeVar
# @rdname getTypeVar-methods
# @exportMethod getTypeVar
# @param theObject object of class \code{\linkS4class{Gwex}}
setGeneric(name="getTypeVar",
           def=function(theObject)
           {
             standardGeneric("getTypeVar")
           }
)

# @rdname getTypeVar-methods
# @aliases getTypeVar,Gwex-method
setMethod(f="getTypeVar",
          signature="Gwex",
          definition=function(theObject)
          {
            return(theObject@variable)
          }
)


#==============================================================================
# is.Gwex
#
# Check if an object is of class \code{\linkS4class{Gwex}}
#
# @export
#
# @param obj a GWex object
#
# @return \item{TRUE}{if an object is of class \code{\linkS4class{Gwex}}}
#
# @author Guillaume Evin
is.Gwex <- function(obj) is(obj, 'Gwex')

#==============================================================================
# myShowGwex
#
# print an object \code{\linkS4class{Gwex}}
#
# @export
#
# @param object a GWex object
# @author Guillaume Evin
myShowGwex =  function(object){
  cat("*** Class GWex *** \n")
  cat("* type of variable = "); cat(object@variable)
  cat("\n* Version = "); cat(object@version)
  cat("\n")
}

#' print-methods: Create a method to print Gwex objects.
#' @rdname print-methods
#' @export
#' @aliases print,Gwex-method
#' @param x \code{\linkS4class{Gwex}} object
#' @examples
#' # Format dates corresponding to daily observations of precipitation
#' vecDates = seq(from=as.Date("01/01/2005",format="%d/%m/%Y"),
#' to=as.Date("31/12/2014",format="%d/%m/%Y"),by='day')
#' 
#' # build GwexObs object with temperature data
#' myObsPrec = GwexObs(variable='Prec',date=vecDates,obs=dailyPrecipGWEX)
#' 
#' # print GwexObs object
#' myObsPrec
setMethod ("print",signature("Gwex"),
           function(x){
             myShowGwex(x)
           }
)

#' show-methods: Create a method to show Gwex objects.
#' @rdname show-methods
#' @aliases show,Gwex-method
#' @param object \code{\linkS4class{Gwex}} object
#' @examples
#' # Format dates corresponding to daily observations of precipitation and temperature
#' vecDates = seq(from=as.Date("01/01/2005",format="%d/%m/%Y"),
#' to=as.Date("31/12/2014",format="%d/%m/%Y"),by='day')
#' 
#' # build GwexObs object with temperature data
#' myObsPrec = GwexObs(variable='Prec',date=vecDates,obs=dailyPrecipGWEX)
#' 
#' # show GwexObs object
#' myObsPrec 
setMethod ("show",signature("Gwex"),myShowGwex)


#==============================================================================
# Method getNbStations: Create a method to get the number of stations.
# @name getNbStations
# @rdname getNbStations-methods
# @exportMethod getNbStations
# @param theObject object of class \code{\linkS4class{GwexObs}}, \code{\linkS4class{GwexFit}} or \code{\linkS4class{GwexSim}}
setGeneric(name="getNbStations",
           def=function(theObject)
           {
             standardGeneric("getNbStations")
           }
)

##############################################################
#                 GwexObs class
##############################################################

#==============================================================================
#' Class \code{\linkS4class{GwexObs}}
#'
#' Defines a \code{\linkS4class{GwexObs}} object which is a \code{\linkS4class{Gwex}} object containing dates and a matrix of observations.
#'
#' @name GwexObs-class
#' @rdname GwexObs-class
#' @exportClass GwexObs
#'
#' @examples
#' # Format dates corresponding to daily observations of precipitation and temperature
#' vecDates = seq(from=as.Date("01/01/2005",format="%d/%m/%Y"),
#' to=as.Date("31/12/2014",format="%d/%m/%Y"),by='day')
#' 
#' # build GwexObs object with precipitation data
#' myObsPrec = GwexObs(variable='Prec',date=vecDates,obs=dailyPrecipGWEX)
#' 
#' # print GwexObs object
#' myObsPrec 
#' @author Guillaume Evin
setClass(
  # Set the name for the class
  "GwexObs",
  
  # Define the slots
  slots = c(date="Date", obs="matrix"),
  
  # Set the default values for the slots
  prototype=list(),
  
  # integrity checks
  validity=function(object){
    if(class(object@date) != 'Date'){
      return("date must be a vector of 'Date'")
    }else if(length(dim(object@obs)) != 2){
      return("obs must be a 2-dimensional matrix nDay x nStation")
    }else{
      return(TRUE)
    }
  },
  
  # Set the inheritance for this class
  contains = "Gwex"
)


#==============================================================================
#' Constructor of class [\code{\linkS4class{GwexObs}}]
#' @title Constructor
#' @param variable 'Prec' or 'Temp'
#' @param date vector of class 'Date'
#' @param obs matrix nTime x nStations of observations
#' @return An object of class [\code{\linkS4class{GwexObs}}]
#' @export
#' @examples
#' # Format dates corresponding to daily observations of precipitation
#' vecDates = seq(from=as.Date("01/01/2005",format="%d/%m/%Y"),
#' to=as.Date("31/12/2014",format="%d/%m/%Y"),by='day')
#' 
#' # build GwexObs object with precipitation data
#' myObsPrec = GwexObs(variable='Prec',date=vecDates,obs=dailyPrecipGWEX)
#' 
#' # print GwexObs object
#' myObsPrec
GwexObs <- function(variable,date,obs) {
  obj <- new("GwexObs",variable=variable,date=date,obs=obs)
  return(obj)
}


#==============================================================================
# is.GwexObs
#
# Check if an object is of class \code{\linkS4class{GwexObs}}
#
# @export
#
# @param obj an object
#
# @return \item{TRUE}{if an object is of class \code{\linkS4class{GwexObs}}}
#
# @author Guillaume Evin
is.GwexObs <- function(obj) is(obj, 'GwexObs')


# @rdname getNbStations-methods
# @aliases getNbStations,GwexObs-method
setMethod(f="getNbStations",
          signature="GwexObs",
          definition=function(theObject)
          {
            ncol(theObject@obs)
          }
)

#==============================================================================
# myShowGwexObs
#
# Print an object of class \code{\linkS4class{GwexObs}}
#
# @export
#
# @param obj an object of class \code{\linkS4class{GwexObs}}
# @author Guillaume Evin
myShowGwexObs = function(obj){
  cat("Observations:", "\n")
  cat("____________", "\n")
  cat(paste0("period: ",min(obj@date),' -> ',max(obj@date)),"\n")
  cat(paste0("number of stations: ",getNbStations(obj)),"\n")
}

#' @rdname print-methods
#' @aliases print,GwexObs-method
setMethod ("print","GwexObs",
           function(x){
             myShowGwexObs(x)
           }
)

#' @rdname show-methods
#' @aliases show,GwexObs-method
setMethod ("show","GwexObs",
           function(object){
             myShowGwex(object)
             myShowGwexObs(object)
           }
)


##############################################################
#              Create the GwexFit class
##############################################################

#==============================================================================
#' Class GwexFit
#'
#' Defines a \code{\linkS4class{GwexFit}} object which is a \code{\linkS4class{Gwex}} object containing 'fit', a list containing the fitted parameters, and 'p', the number of stations.
#' See \link[GWEX]{fitGwexModel} for some examples.
#'
#' @name GwexFit-class
#' @rdname GwexFit-class
#' @exportClass GwexFit
#'
#' @author Guillaume Evin
setClass(
  # Set the name for the class
  "GwexFit",
  
  # Define the slots: fit is a list that will contain the fitted parameters
  slots = c(fit = "list", p="numeric"),
  
  # Set the default values for the slots
  prototype=list(),
  
  # Set the inheritance for this class
  contains = "Gwex"
)

# Constructor of class [\code{\linkS4class{GwexFit}}]
# @title Constructor
# @param variable 'Prec' or 'Temp'
# @param fit List containing the fitted parameters
# @param p The number of station
# @return An object of class [\code{\linkS4class{GwexFit}}]
# @export
GwexFit <- function(variable,fit,p) {
  obj <- new("GwexFit",variable=variable,fit=fit,p=p)
  return(obj)
}

#==============================================================================
# is.GwexFit
#
# Check if an object is of class GwexFit
#
# @export
#
# @param obj an object
#
# @return \item{TRUE}{if an object is of class \code{\linkS4class{GwexFit}}}
#
# @author Guillaume Evin
is.GwexFit <- function(obj) is(obj, 'GwexFit')


#==============================================================================
#' fitGwexModel: fit a GWex model to observations.
#'
#' @export
#'
#' @param objGwexObs an object of class \code{\linkS4class{GwexObs}}
#' @param listOption a list with the following fields:
#' \itemize{
#'   \item \strong{th}: threshold value in mm above which precipitation observations are considered to be non-zero (=0.2 by default)
#'   \item \strong{nLag}: order of the Markov chain for the transitions between dry and wet states (=2 by default)
#'   \item \strong{typeMargin}: 'EGPD' (Extended GPD) or 'mixExp' (Mixture of Exponentials). 'EGPD' by default
#'   \item \strong{xiHat}: pre-determined values for the xi parameters of the EGPD distribution on precipitation amounts
#'   \item \strong{copulaInt}: 'Gaussian' or 'Student': type of dependence for amounts (='Student' by default)
#'   \item \strong{isMAR}: logical value, do we apply a Autoregressive Multivariate Autoregressive model (order 1) =TRUE by default
#'   \item \strong{is3Damount}: logical value, do we apply the model on 3D-amount. =FALSE by default
#'   \item \strong{nChainFit}: integer, length of the runs which are generated during the fitting procedure. =100000 by default
#'   \item \strong{nCluster}: integer, number of clusters which can be used for the parallel computation
#' }
#'
#' @return Return an object of class \code{\linkS4class{GwexFit}} with:
#' \itemize{
#'   \item \strong{p}: The number of station,
#'   \item \strong{version}: package version,
#'   \item \strong{variable}: the type of variable,
#'   \item \strong{fit}: a list containing the list of options \code{listOption} and the list of estimated parameters \code{listPar}.
#' }
#' @param vecClass vector of class (char)
#' 
#' @author Guillaume Evin
#' @examples
#' # Format dates corresponding to daily observations of precipitation and temperature
#' vecDates = seq(from=as.Date("01/01/2005",format="%d/%m/%Y"),
#' to=as.Date("31/12/2014",format="%d/%m/%Y"),by='day')
#' 
#' ###############################################################
#' #               FIT THE PRECIPITATION MODEL
#' ###############################################################
#' 
#' # Format observations: create a G-Wex object
#' myObsPrec = GwexObs(variable='Prec',date=vecDates,obs=dailyPrecipGWEX[,1:2])
#'
#' # parMargin: list with one element per class, with a matrix nStation x 3
#' parMargin = list(DJFMAM=rbind(c(1,1,0),c(1,1,0)),JJASON=rbind(c(1,1,1),c(1,1,0)))
#'
#' # Options: specify the threshold for precipitation (0.5 mm) to distinguish wet and 
#' # dry states (th), xi values for the DGPD distribution (xiHat), a Student copula for 
#' # the spatial dependence (copulaInt), a model based on 3-day aggregated values 
#' # (is3Damount), a MAR(1) process (isMAR), a EGPD distribution for marginal intensities
#' # ('typeMargin'), and 200 replicates for the runs used during the fitting process
#' # (this value being 100,000 by default, in order to obtain a reasonable precision of the 
#' # estimates) 
#' list.options = list(th=0.5,nLag=1,xiHat=xiReg,copulaInt='Student',is3Damount=TRUE,isMAR=TRUE,
#'                    typeMargin='EGPD',nChainFit=200,isParallel=FALSE)
#'
#' vec.month = as.numeric(strftime(vecDates, "%m"))
#' vecClass = vector(length=length(vec.month))
#' vecClass[vec.month%in%c(12,1,2,3,4,5)] = "DJFMAM"
#' vecClass[vec.month%in%c(6,7,8,9,10,11)] = "JJASON"
#'
#' # Fit precipitation model
#' # myParPrec = fitGwexModel(myObsPrec,coord, parMargin, list.options, vecClass)) # fit model
#' # myParPrec # print object
fitGwexModel <- function(objGwexObs,coord,parMargin,listOption=NULL,vecClass=NULL){
  if(!is.GwexObs(objGwexObs)) stop('GwexFit: argument must be a GwexObs object')
  
  # type of variable
  typeVar = getTypeVar(objGwexObs)
  
  # number of stations
  p = getNbStations(objGwexObs)
  
  # vecClass
  if(!is.null(vecClass)){
    if(length(vecClass)!=length(objGwexObs@date)){
      stop("vecClass must be of the same length as observations (i.e. they must correspond to the same dates")
    }
  }
  
  # call general constructor and fitting function
  print("Fit generator")
  objGwexFit <- GwexFit(variable=typeVar,fit = fit.GWex.prec(objGwexObs,coord,parMargin,listOption,vecClass),p=p)
  
  return(objGwexFit)
}


#==============================================================================
#' @rdname getNbStations-methods
#' @export
#' @aliases getNbStations,GwexFit-method
setMethod(f="getNbStations",
          signature="GwexFit",
          definition=function(theObject)
          {
            theObject@p
          }
)

#==============================================================================
#' myShowGwexFit
#'
#' Print an object of class \code{\linkS4class{GwexFit}}
#'
#' @export
#'
#' @param obj an object of class \code{\linkS4class{GwexFit}}
#' 
#' @author Guillaume Evin
myShowGwexFit = function(obj){
  cat("\n")
  
  opt = obj@fit$listOption
  cat("#### Options for the occurrence process ####\n")
  cat(paste0("threshold: ",opt$th),"\n")
  cat(paste0("order of the Markov chain: ",opt$nLag),"\n")
  cat("\n")
  cat("#### Options for the amount process ####\n")
  cat(paste0("given values for Xi: ",!is.null(opt$xiHat)),"\n")
  cat(paste0("spatial dependence: ",opt$copulaInt," copula"),"\n")
  cat(paste0("apply a MAR(1) process: ",opt$isMAR),"\n")
  cat(paste0("apply a master Markov chain: ",opt$isState),"\n")
}


#' @rdname print-methods
#' @aliases print,GwexFit-method
setMethod ("print","GwexFit",
           function(x){
             myShowGwexFit(x)
           }
)

#' @rdname show-methods
#' @aliases show,GwexFit-method
setMethod ("show","GwexFit",
           function(object){
             myShowGwex(object)
             myShowGwexFit(object)
           }
)



##############################################################
#              Create the GwexSim class
##############################################################

#==============================================================================
#' Defines a \code{\linkS4class{GwexSim}} object which is a \code{\linkS4class{Gwex}} object containing 'sim', an array containing the simulations, and 'dates', a vector of dates.
#' See \link[GWEX]{simGwexModel} for some examples.
#'
#' @name GwexSim-class
#' @rdname GwexSim-class
#' @exportClass GwexFit
#'
#' @author Guillaume Evin
setClass(
  # Set the name for the class
  "GwexSim",
  
  # Define the slots: fit is a list that will contain the fitted parameters
  slots = c(
    listOption = "ANY",
    date = "Date",
    sim = "array"),
  
  # Set the default values for the slots
  prototype=list(),
  
  # Set the inheritance for this class
  contains = "Gwex"
)

# Constructor of class [\code{\linkS4class{GwexSim}}]
# @title Constructor
# @param variable 'Prec'
# @param listOption list of options
# @param date vector of dates
# @param sim array of simulated values
# @return An object of class [\code{\linkS4class{GwexSim}}]
# @export
GwexSim <- function(variable,listOption,date,sim) {
  obj <- new("GwexSim",variable=variable,listOption=listOption,date=date,sim=sim)
  return(obj)
}


#==============================================================================
# is.GwexSim
#
# Check if an object is of class GwexSim
#
# @export
#
# @param obj an object
#
# @return \item{TRUE}{if an object is of class \code{\linkS4class{GwexSim}}}
#
# @author Guillaume Evin
is.GwexSim <- function(obj) is(obj, 'GwexSim')


#==============================================================================
#' simGwexModel
#'
#' Simulate from a GWex model
#'
#' @export
#'
#' @param objGwexFit an object of class \code{\linkS4class{GwexFit}}
#' @param nb.rep number of repetitions of scenarios
#' @param vecDates vector of continuous dates
#' @param vecClass vector of classification (char) of the same length as vecDates
#' @param objGwexObs optional: an object of class \code{\linkS4class{GwexObs}} if we need the observations to simulate (disaggregation prec 3D -> 1D)
#' @param prob.class vector of probabilities indicating class of "similar" mean intensities
#' @param nCluster optional, number of clusters which can be used for the parallel computation
#'
#' @return \item{GwexSim}{an object of class \code{\linkS4class{GwexSim}}. Contains sim (3D-array with the simulations) and a vector of dates}
#'
#' @author Guillaume Evin
#' @examples
#' # vector of dates
#' vecDates = seq(from=as.Date("01/01/2005",format="%d/%m/%Y"),
#' to=as.Date("31/12/2014",format="%d/%m/%Y"),by='day')
#' 
#' vecClass = vector(length=length(vecDates))
#' vecMonth = as.numeric(strftime(vecDates, "%m"))
#' vecClass[vecMonth%in%c(12,1,2)] = "DJF"
#' vecClass[vecMonth%in%c(3,4,5)] = "MAM"
#' vecClass[vecMonth%in%c(6,7,8)] = "JJA"
#' vecClass[vecMonth%in%c(9,10,11)] = "SON"
#' 
#' ###############################################################
#' #               FIT AND SIMULATE FROM THE PRECIPITATION MODEL
#' ###############################################################
#' # Format observations: create a G-Wex object
#' myObsPrec = GwexObs(variable='Prec',date=vecDates,obs=dailyPrecipGWEX[,1:2])
#'
#' # default options except for 'nChainFit'
#' list.options = list(nChainFit=1000)
#' 
#' # Fit precipitation model
#' # myParPrec = fitGwexModel(myObsPrec,list.options) # fit model
#'
#' # generate 2 scenarios for one year, using a existing 'GwexFit' object
#' # mySimPrec = simGwexModel(objGwexFit=myParPrec, nb.rep=2, vecDates, vecClass)
#' # mySimPrec # print object
simGwexModel <- function(objGwexFit, nb.rep = 10, vecDates, vecClass,
                         objGwexObs=NULL, prob.class=c(0.5,0.75,0.9,0.99), nCluster=1){
  # check objGwexFit
  if(!is.GwexFit(objGwexFit)) stop('objGwexFit: objGwexFit argument must be a GwexFit object')
  
  # type of variable
  typeVar = getTypeVar(objGwexFit)
  
  # number of stations
  p = getNbStations(objGwexFit)
  
  # number of dates
  n = length(vecDates)
  if(length(vecClass)!=n) stop("vecClass and vecDates must have the same length")
  
  # prepare parallelization
  print("Generate scenarios")
  cl <- parallel::makeCluster(nCluster)
  doParallel::registerDoParallel(cl)
  acomb <- function(...) abind::abind(..., along=3)
  
  # simulate from GWex model
  iSim = NULL
  sim.GWex.out <- foreach(iSim=1:nb.rep, .combine='acomb', .multicombine=TRUE) %dopar% {
    sim.GWex.1it = sim.GWex.prec.1it(objGwexFit,vecDates,vecClass,
                                     myseed=iSim,objGwexObs=objGwexObs,prob.class=prob.class)
    return(sim.GWex.1it)
  }
  
  # close cluster
  parallel::stopCluster(cl)
  
  # call constructor of GwexSim objects
  objGwexSim <- GwexSim(variable = typeVar,
                        listOption = objGwexFit@fit$listOption,
                        date = vecDates,
                        sim = sim.GWex.out)
  
  return(objGwexSim)
}



#==============================================================================
# @rdname getNbStations-methods
# @aliases getNbStations,GwexSim-method
setMethod(f="getNbStations",
          signature="GwexSim",
          definition=function(theObject)
          {
            dim(theObject@sim)[2]
          }
)

#==============================================================================
# myShowGwexSim
#
# Print an object of class \code{\linkS4class{GwexSim}}
#
# @export
#
# @param obj an object of class \code{\linkS4class{GwexSim}}
#
# @author Guillaume Evin
myShowGwexSim = function(obj){
  cat("Generated scenarios:", "\n")
  cat("__________________", "\n", "\n")
  
  # print options
  cat("#### Options for the occurrence process ####\n")
  opt = obj@listOption
  cat(paste0("threshold: ",opt$th),"\n")
  cat(paste0("order of the Markov chain: ",opt$nLag),"\n")
  cat("\n")
  cat("#### Options for the amount process ####\n")
  cat(paste0("given values for Xi: ",!is.null(opt$xiHat)),"\n")
  cat(paste0("spatial dependence: ",opt$copulaInt," copula"),"\n")
  cat(paste0("apply a MAR(1) process: ",opt$isMAR),"\n")
  cat(paste0("apply a master Markov chain: ",opt$isState),"\n")
  cat("\n")
  
  # print dimensions of simulated scenarios
  cat("#### Simulations ####\n")
  cat(paste0("period: ",min(obj@date),' -> ',max(obj@date)),"\n")
  cat(paste0("number of scenarios: ",dim(obj@sim)[3]),"\n")
  cat(paste0("number of stations: ",getNbStations(obj),"\n"))
}


#' @rdname print-methods
#' @aliases print,GwexSim-method
setMethod (f="print",signature="GwexSim",definition=function(x){
  myShowGwexSim(x)
}
)

#' @rdname show-methods
#' @aliases show,GwexSim-method
setMethod (f="show",signature="GwexSim",definition=function(object){
  myShowGwex(object)
  myShowGwexSim(object)
}
)
