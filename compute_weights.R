## INFO
##
## 23 November 2020
## script to read in sequence metadata from climb
## and combine with case data from ONS
## to give the coverage of sequences per case
## in LTLAs and the sequences' corresponding weights

## weight normalisation, saved in weight_options.csv, by:
# just time; wk_weight
# just LA; la_weight
# time localised to month; mn_weight
# time localised to month and LA; lamn_weight

## TO USE
## Rscript compute_weights.R --outroot a0- --postcode postcode_to_la.csv --days 14 --phepath .
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
require( dplyr,quietly=T,warn.conflicts=F )

## read in arguments ######################################################
w <- 14 # time interval for aggregation, days
postcodefile <- 'postcode_to_la.csv'
outroot <- 'a0-'
path_to_phe_data <- NULL
args = commandArgs(trailingOnly=TRUE)
#if(length(args)==0) args <- c('--outroot', 'a0-','--postcode','postcode_to_la.csv','--days','14','--phepath','./')
inds <- which(grepl('--',args))
for(i in inds){
  if(args[i]=='--outroot')
    outroot <- args[i+1]
  if(args[i]=='--postcode')
    postcodefile <- args[i+1]
  if(args[i]=='--days')
    w <- as.numeric(args[i+1])
  if(args[i]=='--phepath')
    path_to_phe_data <- paste0(args[i+1],'/')
}
cat(paste0('\nCall:\n  Rscript compute_weights.R --outroot ',outroot,' --postcode ',postcodefile,' --days ',w,' --phepath ',path_to_phe_data,'\n\n'))

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
path <- '/cephfs/covid/bham/results/phylogenetics/latest/alignments/'
ext <- list.files(path)
# alignment: 
md_fn = paste0(path,ext[grepl('metadata',ext)&grepl('all',ext)])
phedf = read.csv(md_fn, stringsAs=FALSE)

# supplement/replace with phe postcodes
if(!is.null(path_to_phe_data)){
  phe_files <- list.files(path_to_phe_data)
  phe_file <- phe_files[which.max(file.mtime(paste0(path_to_phe_data,phe_files)))]
  phedf2 = read.csv( paste0(path_to_phe_data,phe_file) , stringsAs=FALSE)
  phedf2 <- subset(phedf2,central_sample_id%in%phedf$central_sample_id)
  phedf$outer_postcode[match(phedf2$central_sample_id,phedf$central_sample_id)] <- phedf2$outer_postcode
}

phedf$outerpostcode <- tolower(phedf$outer_postcode)

#phedf$date <- dmy( phedf$sampledate  )
cat(' -- Extracting date from metadata;\n')
phedf$date <- ymd( phedf$collection_date ) #sampledate  )
#phedf$ltlacd = pcdf$LTLA19CD[ match( phedf$outerpostcode, pcdf$postcode) ]
cat(' -- Cleaning postcodes;\n')
# fix a few known problems
phedf$outerpostcode <- gsub('s040','so40',phedf$outerpostcode)
phedf$outerpostcode <- gsub('c04','co4',phedf$outerpostcode)
phedf$outerpostcode <- gsub('p01','po1',phedf$outerpostcode)
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
cat(sort(unique(phedf$outer_postcode[missing0])))
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
casesdf$cases[is.na(casesdf$cases)] <- 0
if(!file.exists('api_store.Rds')) saveRDS(casesdf,'api_store.Rds')

## read in and update store
stored <- readRDS('api_store.Rds')
las_to_append <- unique(stored$code)[!unique(stored$code)%in%casesdf$code]
casesdf <- rbind(casesdf,subset(stored,code%in%las_to_append))
saveRDS(casesdf,'api_store.Rds')
cat(paste0(' -- Cases by LA and date written to api_store.Rds \n'))

## join data to compute rolling coverage & weights ####################
cat(' -- Joining sample data to case data by postcode;\n')
## samples over confirmed 
dates = sort( unique( as.Date( casesdf$date )  )  )
# start at 21 to avoid zero cases
dates1 <- dates [ dates > as.Date('2020-03-21' ) ]
dates2 <- decimal_date(dates1)
#w = 14

phedf2 <- phedf[!is.na( phedf$date )& !is.na( phedf$ltlacd ) ,]
casesdf2 <- casesdf[!is.na( casesdf$cases ) ,]
ltla2covg <- lapply(ltlacds, function( cd ) {
  pdts <- decimal_date(phedf2$date[phedf2$ltlacd == cd])
  cdts <-  decimal_date(ymd(casesdf2$date[casesdf2$code == cd ]))
  ccase <- casesdf2$cases[ casesdf2$code == cd ]
  cc <- sc <- c()
  for(i in 1:length(dates2)){
    d <- dates2[i]
    cc[i] = sum(  ccase[ cdts >  d - w/366 & cdts <= d]  ) 
    sc[i] <- sum( pdts >  d - w/366 & pdts <= d) 
    ## forward fill cases
    if(cc[i]==0 && sc[i] > 0 && i>1 ){
      last_positive <- rev(cc[cc>0])[1]
      cc[i] <- last_positive
    }
  }
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
  x$LTLA19NM = ltlacd2ltla[cd]
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

dfsubset <- sapply(phedfs,function(x)sum(!is.na(x$ltlacd))>0)
weightdfs = lapply( c(phedfs[dfsubset]) , function( .phedf ){
  #.phedf <- .phedf [ !duplicated( .phedf$patientid ) , ]
  las <- .phedf$ltlacd
  ulas <- unique(las)[!is.na(unique(las))]
  d = .phedf$date[ 1 ]
  coverage2weekcov <- coverage2week$coverage[ coverage2week$date==d]
  coverage2weekla <- coverage2week$LTLA19CD[coverage2week$date==d]
  if(length(coverage2weekcov)>0){
  data.frame(ltlacd=sapply(ulas,function(cd)rep(cd,sum(coverage2weekla==cd))),
             weight=sapply(ulas,function(cd){
               1/coverage2weekcov[which(coverage2weekla==cd)] 
               }),date=as.character(d))
  }else{
    NULL
    }
}
)
wdf = do.call( rbind, weightdfs )
weightfile <- paste0(outroot,'laweightsdf-',today(),'.csv') 
write.csv(  wdf, row.names=F , file = weightfile) 

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

#################################################################
# augment with lots of weight options.
## normalisation by:
# just time; wk_weight
# just LA; la_weight
# time localised to month; mn_weight
# time localised to month and LA; lamn_weight
coverage2week$wk <- week(coverage2week$date)
wks <- unique(coverage2week$wk)
las <- unique(coverage2week$LTLA19CD)

## tabulate samples and cases by LAs and weeks
tab_samples <- sapply(wks,function(y)sapply(las,function(x)sum(subset(coverage2week,LTLA19CD==x&wk==y)$samples)))
tab_cases <- sapply(wks,function(y)sapply(las,function(x)sum(subset(coverage2week,LTLA19CD==x&wk==y)$cases)))
sum_samples <- t(apply(tab_samples,1,function(y){
  sapply(1:length(y),function(x){
    mx <- x+0:2;  mx <- mx[mx<=length(y)];
    mn <- x-1:2;  mn <- mn[mn>=0];
    sum(y[c(mn,mx)])
  })
}))
sum_cases <- t(apply(tab_cases,1,function(y){
  sapply(1:length(y),function(x){
    mx <- x+0:2;  mx <- mx[mx<=length(y)];
    mn <- x-1:2;  mn <- mn[mn>=0];
    sum(y[c(mn,mx)])
  })
}))
sum_samples <- apply(sum_samples,2,function(x)rep(sum(x),length(x)))
sum_cases <- apply(sum_cases,2,function(x)rep(sum(x),length(x)))
repre <- sum_cases/sum_samples
weights <- tab_cases/tab_samples/repre
weights[is.na(weights)] <- 0
weights[!is.finite(weights)] <- 0

colnames(weights) <- wks

la_order <- match(coverage2week$LTLA19CD,rownames(weights))
wk_order <- match(coverage2week$wk,colnames(weights))
coverage2week$lamn_weight <- weights[cbind(la_order,wk_order)]

## by la
la_samples <- sapply(las,function(x)sum(subset(coverage2week,LTLA19CD==x)$samples))
la_cases <- sapply(las,function(x)sum(subset(coverage2week,LTLA19CD==x)$cases))
la_cases <- la_cases/(sum(la_cases,na.rm=T)/sum(la_samples,na.rm=T))
la_cases[la_samples==0] <- 0
la_weights <- data.frame(LTLA19CD=las,la_weight=la_cases/la_samples)

## by week
wk_samples <- sapply(wks,function(x)sum(subset(coverage2week,wk==x)$samples))
wk_cases <- sapply(wks,function(x)sum(subset(coverage2week,wk==x)$cases))
wk_cases <- wk_cases/(sum(wk_cases)/sum(wk_samples))
wk_weights <- data.frame(wk=wks,wk_weight=wk_cases/wk_samples)
mn_cases <- sapply(wks,function(x)sum(subset(coverage2week,wk==x)$cases))
mn_cases <- mn_cases/sapply(1:length(wks),function(x){
  mx <- x+0:2;  mx <- mx[mx<=length(wks)];
  mn <- x-0:2;  mn <- mn[mn>=0];
  sum(mn_cases[c(mn,mx)])/sum(wk_samples[c(mn,mx)])})
wk_weights <- data.frame(wk=wks,mn_weight=mn_cases/wk_samples,wk_weight=wk_cases/wk_samples)

coverage2week <- left_join(coverage2week,la_weights,by='LTLA19CD')
coverage2week <- left_join(coverage2week,wk_weights,by='wk')
coverage2week$wk_weight[is.na(coverage2week$wk_weight)] <- 0
coverage2week$lamn_weight[is.na(coverage2week$lamn_weight)] <- 0
coverage2week$la_weight[is.na(coverage2week$la_weight)] <- 0

## append to phedf
weighted_phedf <- phedf[,colnames(phedf)%in%c('central_sample_id','date','ltlacd')]
colnames(weighted_phedf)[3] <- 'LTLA19CD'
weighted_phedf <- left_join(weighted_phedf,
                            coverage2week[,colnames(coverage2week)%in%c('lamn_weight','la_weight','wk_weight','mn_weight','LTLA19CD','date')],
                            by=c('LTLA19CD','date'))
weighted_phedf <- left_join(weighted_phedf,wdf,by='central_sample_id')
saveRDS(weighted_phedf,'weight_by_week_la.Rds')

write.csv(weighted_phedf[,colnames(weighted_phedf)%in%c('lamn_weight','la_weight','wk_weight','mn_weight','central_sample_id')],
          'weight_options.csv',row.names = F)

