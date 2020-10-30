## INFO
##
## 28 October 2020
## script to read in sequence metadata from climb
## and combine with case data from ONS
## to give the coverage of sequences per case
## in LTLAs and the sequences' corresponding weights

## TO USE
##
## Rscript compute_weights.R --outroot a0- --postcode postcode_to_la.csv --days 14
##
## outroot becomes a prefix to output files
## postcode is the name (including path) of the file containing postcodes and LAs
## days is the number of days prior over which to sum the numbers of cases and samples
##

#######################################################################
## function from https://coronavirus-staging.data.gov.uk/details/developers-guide#params-page
#' Extracts paginated data by requesting all of the pages
#' and combining the results.
#'
#' @param filters    API filters. See the API documentations for 
#'                   additional information.
#'                   
#' @param structure  Structure parameter. See the API documentations 
#'                   for additional information.
#'                   
#' @return list      Comprehensive list of dictionaries containing all 
#'                   the data for the given ``filter`` and ``structure`.`
get_paginated_data <- function (filters, structure) {
  
  endpoint     <- "https://api.coronavirus.data.gov.uk/v1/data"
  results      <- list()
  current_page <- 1
  
  repeat {
    
    httr::GET(
      url   = endpoint,
      query = list(
        filters   = paste(filters, collapse = ";"),
        structure = jsonlite::toJSON(structure, auto_unbox = TRUE),
        page      = current_page
      ),
      timeout(10)
    ) -> response
    
    # Handle errors:
    if ( response$status_code >= 400 ) {
      err_msg = httr::http_status(response)
      stop(err_msg)
    } else if ( response$status_code == 204 ) {
      break
    }
    
    # Convert response from binary to JSON:
    json_text <- content(response, "text")
    dt        <- jsonlite::fromJSON(json_text)
    results   <- rbind(results, dt$data)
    
    if ( is.null( dt$pagination$`next` ) ){
      break
    }
    
    current_page <- current_page + 1;
    
  }
  
  return(results)
  
}

## libraries ##########################################################
require( lubridate,quietly=T,warn.conflicts=F ) 
require( magrittr,quietly=T,warn.conflicts=F ) 
require( httr,quietly=T,warn.conflicts=F ) 
require( curl,quietly=T,warn.conflicts=F )

## read in arguments ######################################################
w <- 14 # time interval for aggregation, days 
postcodefile <- 'postcode_to_la.csv'
outroot <- 'a0-'
args = commandArgs(trailingOnly=TRUE)
#if(length(args)==0) args <- c('--outroot', 'a0-','--postcode','postcode_to_la.csv','--days','14')
inds <- which(grepl('--',args))
for(i in inds){
  if(args[i]=='--outroot')
    outroot <- args[i+1]
  if(args[i]=='--postcode')
    postcodefile <- args[i+1]
  if(args[i]=='--days')
    w <- as.numeric(args[i+1])
}
cat(paste0('\nCall:\n  Rscript compute_weights.R --outroot ',outroot,' --postcode ',postcodefile,' --days ',w,'\n\n'))

## read in metadata #######################################################
cat(' -- Reading postcode mapping;\n')
pcdf = read.csv( postcodefile , stringsAs=FALSE, header=TRUE)
pcdf <- pcdf[grepl('W|N|E|S',pcdf$LTLA19CD),] # remove islands

# add city of london to hackney and isle of scilly to cornwall
miss <- c("E09000001", "E06000053")
new <- c("Hackney","Cornwall")
newcd <- sapply(new,function(x)subset(pcdf,LTLA19NM==x)$LTLA19CD[1])
for(i in 1:2){
  olds <- which(pcdf$LTLA19CD==miss[i])
  pcdf$LTLA19CD[olds] <- pcdf$UTLA19CD[olds] <- newcd[i] 
  pcdf$LTLA19NM[olds] <- pcdf$UTLA19NM[olds] <- new[i] 
}
pcdf$postcode <- tolower(pcdf$postcode)

cat(' -- Reading metadata;\n')
phedf = read.csv('/cephfs/covid/bham/artifacts/published/majora.latest.metadata.tsv',sep='\t', stringsAs=FALSE)
phedf$outerpostcode <- tolower(phedf$adm2_private)

#phedf = read.csv( 'COG_Metadata_EpiCell_Merged_20200915.csv' , stringsAs=FALSE)

#phedf$date <- dmy( phedf$sampledate  )
cat(' -- Extracting date from metadata;\n')
phedf$date <- ymd( phedf$collection_date ) #sampledate  )
#phedf$ltlacd = pcdf$LTLA19CD[ match( phedf$outerpostcode, pcdf$postcode) ]
cat(' -- Cleaning postcodes;\n')
# fix a few known problems
phedf$outerpostcode <- gsub('s040','so40',phedf$outerpostcode)
phedf$outerpostcode <- gsub('c04','co4',phedf$outerpostcode)
phedf$outerpostcode <- gsub('c09','co9',phedf$outerpostcode)
phedf$outerpostcode <- gsub('y01','yo1',phedf$outerpostcode)
phedf$outerpostcode <- gsub(' ','',phedf$outerpostcode)
# recode missing as empty string
phedf$outerpostcode <- gsub('unknown','',phedf$outerpostcode)
phedf$outerpostcode[is.na(phedf$outerpostcode)] <- ''
missing1 <- sum(phedf$outerpostcode=='')
cat(paste0(missing1,' postcodes missing\n'))
# omit islands
cat(paste0(sum(grepl('im|je|zz|gy',phedf$outerpostcode)),' postcodes from IM, JE, ZZ, GY removed\n'))
phedf$outerpostcode[grepl('im|je|zz|gy',phedf$outerpostcode)] <- ''

missing <- missing0 <- !phedf$outerpostcode%in%pcdf$postcode & phedf$outerpostcode!=''
maxlen <- max(sapply(phedf$outerpostcode[missing],nchar))
for(i in 1:(maxlen-2)){
  phedf$outerpostcode[missing] <- sapply(phedf$outerpostcode[missing],function(x)substr(x,1,nchar(x)-1))
  missing0 <- missing0 & !phedf$outerpostcode%in%pcdf$postcode
  missing <- !phedf$outerpostcode%in%pcdf$postcode & phedf$outerpostcode!=''
}
missing2 <- sum(phedf$outerpostcode=='') - missing1
cat(paste0(sum(missing)+missing2,' postcodes were not matched\n'))
cat(sort(unique(phedf$adm2_private[missing0])))
cat('\n')

phedf$ltlacd = pcdf$LTLA19CD[ match( phedf$outerpostcode, pcdf$postcode) ]

#~ '
#~ variable to map codes, names, ltlas, utlas 
#~ '
utlacds <- unique( pcdf$UTLA19CD )
utlacd2ltlacd <- lapply( utlacds, function(x) pcdf$LTLA19CD[ pcdf$UTLA19CD == x ]  )
ltla2ltlacd =  setNames( pcdf$LTLA19CD, pcdf$LTLA19NM )  
ltlacd2ltla = setNames( names( ltla2ltlacd  ), ltla2ltlacd )
ltlacds = unique( pcdf$LTLA19CD ) 


## read in ons data ###################################################
cat(' -- Setting up API call;\n')
set_config( config( ssl_verifypeer = 0L ) )
endpoint <- "https://api.coronavirus.data.gov.uk/v1/data"

# Create the structure as a list or a list of lists:
structure <- list(
  date = "date", 
  name = "areaName", 
  code = "areaCode", 
  cases = "newCasesBySpecimenDate"#,
  #pubcases = "newCasesByPublishDate"
)

# Create filters:
query_filters <- c(
  "areaType=ltla"
)

cat(' -- API call;\n')
# set sequential=T to go through ltlas one by one
sequential <- F
casesdf <- list()
if(sequential){
  stored_results <- list()
  if(file.exists('stored_results.Rds'))
    stored_results <- readRDS('stored_results.Rds')
  las_as_list <- list()
  for(cdi in 1:length(ltlacds)){
    cd <- ltlacds[cdi]
    filters <- c(
      sprintf("areaType=%s", 'ltla'),
      sprintf("areaCode=%s", cd) 
    )
    # The "httr::GET" method automatically encodes the URL and its parameters:
    httr::GET(
      # Concatenate the filters vector using a semicolon.
      url = endpoint,
      # Convert the structure to JSON (ensure that "auto_unbox" is set to TRUE).
      query = list(
        filters = paste(filters, collapse = ";"),
        structure = jsonlite::toJSON(structure, auto_unbox = TRUE)
      ),
      # The API server will automatically reject any requests that take longer than 10 seconds to process.
      timeout(10)
    ) -> response
    # Handle errors:
    if (response$status_code > 200) {
      err_msg = httr::http_status(response)
      print(c(cd))
      print(err_msg)
      las_as_list[[cd]] <- stored_results[[cd]]
    }else{
      # Convert response from binary to JSON:
      json_text <- content(response, "text")
      data = jsonlite::fromJSON(json_text)
      las_as_list[[cd]] <-   data$data 
    }
  }
  saveRDS(las_as_list,'stored_results.Rds')
  casesdf <- do.call(rbind,las_as_list)
  casesdf$date <- as.Date( casesdf$date )
}else{
  casesdf <- get_paginated_data(query_filters, structure)
}
if(!file.exists('api_store.Rds')) saveRDS(casesdf,'api_store.Rds')

## read in and update store
stored <- readRDS('api_store.Rds')
las_to_append <- unique(stored$code)[!unique(stored$code)%in%casesdf$code]
casesdf <- rbind(casesdf,subset(stored,code%in%las_to_append))
saveRDS(casesdf,'api_store.Rds')

## join data to compute rolling coverage & weights ####################
cat(' -- Joining sample data to case data by postcode;\n')
## samples over confirmed 
dates = sort( unique( as.Date( casesdf$date )  )  ) 
dates1 <- dates [ dates > as.Date('2020-03-14' ) ]
dates2 <- as.numeric(dates1)
#w = 14 

phedf2 <- phedf[!is.na( phedf$date )& !is.na( phedf$ltlacd ) ,]
casesdf2 <- casesdf[!is.na( casesdf$cases ) ,]
ltla2covg <- lapply(ltlacds, function( cd ) {
  pdts <- as.numeric(phedf2$date[phedf2$ltlacd == cd])
  cdts <-  as.numeric(ymd(casesdf2$date[casesdf2$code == cd ]))
  ccase <- casesdf2$cases[ casesdf2$code == cd ]
  cc <- sc <- c()
  for(i in 1:length(dates2)){
    d <- dates2[i]
    cc[i] = sum(  ccase[ cdts >  d - w & cdts <= d]  )
    sc[i] <- sum( pdts >  d - w & pdts <= d)
  }
  #sapply ( dates1, function(d) {
  #  cc = sum(  ccase[ cdts >  d - w & cdts <= d]  )
  #  sc <- sum( pdts >  d - w & pdts <= d)
  #  c( cases = cc, samples = sc )
  #}) -> res 
  res = data.frame( cases = cc, samples = sc  )
  res$date <- dates1 
  res$coverage = with ( res , samples / cases )
  res
}) 

names( ltla2covg ) <- ltlacds 
saveRDS( ltla2covg, file = paste0(outroot,'ltla2covg.rds') )
coverage2week<- do.call( rbind , lapply( ltlacds , function(cd){
  x = ltla2covg[[cd]] 
  x$LTLA19CD = cd 
  x$LTLA19NM = ltlacd2ltla[cd][1]
  x
}))
coverage2week$coverage[ is.infinite( coverage2week$coverage ) ] <- NA 
coverage2week$coverage[ is.nan( coverage2week$coverage ) ] <- NA 
coverage2week$coverage <- pmin ( 1, coverage2week$coverage )
#saveRDS( coverage2week, file = paste0(outroot,'coverage.rds') )
covfile <- paste0(outroot,'coverage-',today(),'.csv')
write.csv( coverage2week, file = covfile, row.names=FALSE)
cat(paste0(' -- Coverage written to ',covfile,' \n'))

## compute weights ##################################################
cat(' -- Computing weights;\n')
phedfs <- split( phedf, phedf$date  )
weightdfs = lapply( phedfs , function( .phedf ){
  #.phedf <- .phedf [ !duplicated( .phedf$patientid ) , ]
  las <- .phedf$ltlacd
  d = .phedf$date[ 1 ]
  coverage2weekcov <- coverage2week$coverage[ coverage2week$date==d]
  coverage2weekla <- coverage2week$LTLA19CD[coverage2week$date==d]
  ws= sapply( 1:nrow(.phedf) , function(k){
    cd = las[ k ] 
    if ( is.na( cd ))
      return(NA)
    cdi <- which(coverage2weekla==cd)
    cvg <- coverage2weekcov[cdi]
    #x = ltla2covg[[ cd ]] 
    #cvg = x$coverage[ x$date == d ]
    if ( length( cvg ) == 0)
      return (1) 
    1/cvg 
  })	
  ws = tryCatch(  ws/mean( ws,na.rm=T ) , warning=function(w) {},
                 error = function(e) cat(paste0('Error on date ',d,';\n',e,'\n')) )
  data.frame( central_sample_id  = .phedf$central_sample_id, coverage_weight = unname( ws ) , stringsAsFactors=FALSE )
})
wdf = do.call( rbind, weightdfs )
weightfile <- paste0(outroot,'weightsdf-',today(),'.csv') 
write.csv(  wdf, row.names=F , file = weightfile) 
cat(paste0(' -- Weights written to ',weightfile,' \n'))

# '
# for each date
# 	for each utla
# 		for each ltla
# 			cases with past x days
# 			samples within past x days
# 			sum
# 		compute ratio
# weights:
# for each sample
# 	get date
# 	get utla 
# 	mean coverage at that date / coverage of utla at that date 
# '
