library(RCurl)
library(XML)
library(RJSONIO)

getIndeed = function(url, BaseURL, con, category){

  #first query
  b = getURLContent(url, curl = con)
  c = htmlParse(b)   

  ans <- procjob(c, BaseURL, con, category)

  nxt <- getNodeSet(c, "//link[@rel = 'next']/@href")

  nx <- getRelativeURL(nxt[[1]], BaseURL)

  count <- 1
  while (count < 30 && length(nxt)!= 0){
    
    bb = getURLContent(nx, curl = con)

    cc = htmlParse(bb) 

    tmp <- procjob(cc, BaseURL, con, category)
    

    ans <- rbind(ans, tmp)

    nxt <- getNodeSet(cc, "//link[@rel = 'next']/@href")

    nx <- getRelativeURL(nxt[[1]], BaseURL)


    count <- count+1

  }

 

  return(ans)
}


procjob = function(doc, BaseURL, con, category) {
  h = getNodeSet(doc, "//table[starts-with(@class, 'jobCard_mainContent')]")

  jobslen = as.integer(length(h))

  JobTitle <- vector()
  CompanyName <-vector()
  Ratings <- vector()
  Location <- vector()
  Salary <- vector()
  EmploymentInfo <-vector()
  Remote <- vector()
  EstimatedSalary <- vector()
  JobLinks <- vector()
  YearFounded <- vector()
  CompanySize <- vector()
  DatePosted <- vector()
  JobCategory <- vector()

  for( i in 1:jobslen){
    JobCategory <- append(JobCategory, category)
    #Job Title
    jobtitle1 = xpathSApply(h[[i]], ".//span[starts-with(@id, 'jobTitle')]", xmlValue)

    JobTitle <- append(JobTitle, jobtitle1)

    
    #company name
    companynames = xpathSApply(h[[i]], ".//span[@class = 'companyName']", xmlValue)

    CompanyName <- append(CompanyName, companynames)

    
    #ratings
    ratingnumber = xpathSApply(h[[i]], ".//span[@class = 'ratingNumber']", xmlValue)

    if (length(ratingnumber) == 0){
      ratingnumber = 'NA'
    }

    Ratings <- append(Ratings, ratingnumber)

    #location
    location1 = xpathSApply(h[[i]], ".//div[@class = 'companyLocation']", xmlValue)

    if (grepl("(.)*Remote(.)*", location1)){
      Remote <- append(Remote, "Remote")
    }
    else if (grepl("(.)*Hybrid(.)*", location1)){
      Remote <- append(Remote, "Hybrid")
    }
    else{
      Remote <- append(Remote, "In-Person")}
    location1 = gsub("^(.)*in ", "", location1)
    location1 = gsub("\\+[0-9]+ locations$", "", location1)
    location1 = gsub("\\+[0-9] location$", "", location1)
    Location <- append(Location, location1)
    

    #salary
    salaryinfo = xpathSApply(h[[i]], ".//td[@class='resultContent']//div[starts-with(@class, 'heading6') and ends-with(@class, 'salaryOnly')]/div[1]",xmlValue)

    if (length(salaryinfo) == 0){
      salaryinfo = 'NA'
    }

    fulltimeinfo = xpathSApply(h[[i]], ".//td[@class='resultContent']//div[starts-with(@class, 'heading6') and ends-with(@class, 'salaryOnly')]/div[2]",xmlValue)

    checksalaryinfo = grepl("(.)*[0-9](.)*", salaryinfo)

    if (length(fulltimeinfo) == 0 ){
      fulltimeinfo = 'NA'
    }

    
    if (grepl("Estimated|estimated", salaryinfo)){
      EstimatedSalary <- append(EstimatedSalary, TRUE)
    }
    else{
      EstimatedSalary <- append(EstimatedSalary, FALSE)
    }
    

    salaryinfo = gsub("Estimated|estimated|a year", "", salaryinfo)
    

    
    if(!checksalaryinfo){
      fulltimeinfo = salaryinfo
      Salary <- append(Salary, 'NA')
    }
    
    
    else{
      Salary <- append(Salary, salaryinfo)
    }
    

    EmploymentInfo <- append(EmploymentInfo, fulltimeinfo)
    

    
    #Links to main job pages
    j = getNodeSet(h[[i]], ".//a[@class  = 'jcs-JobTitle']/@href")

    p = getRelativeURL(j[[1]], BaseURL)

    JobLinks <- append(JobLinks, p)

    
     pp = getURLContent(p, curl = con)

     doc2 = htmlParse(pp)

     
     # year founded
     compnode = getNodeSet(doc2, "//div[@class = 'icl-u-lg-mr--sm icl-u-xs-mr--xs']//a[@target='_blank']/@href")

     if(length(compnode) !=0){
     cplink = getRelativeURL(compnode[[1]], BaseURL)

     cpp = getURLContent(cplink, curl = con)

     doc3 = htmlParse(cpp)

     cyear = xpathSApply(doc3, "//li[@data-testid = 'companyInfo-founded']/div[2]", xmlValue)

     if (is.null(cyear)){
       YearFounded <- append(YearFounded, 'NA')
     }
     else{
       YearFounded <- append(YearFounded, cyear)
     }
     

     
     
     #company size
     csize = xpathSApply(doc3, "//li[@data-testid = 'companyInfo-employee']/div[2]", xmlValue)
     

     
     if (is.null(csize)){
       CompanySize <- append(CompanySize, 'NA')
     }
     else{
       CompanySize <- append(CompanySize, csize)
     }
     }
     else{
       CompanySize <- append(CompanySize, 'NA')
       YearFounded <- append(YearFounded, 'NA')
     }
  

     #date posted
     postdate = xpathSApply(doc2, "//span[@class='jobsearch-HiringInsights-entry--text']", xmlValue)

      if (is.null(postdate)){
       DatePosted <- append(DatePosted, 'NA')
     }
     
     else if (length(postdate)==2){
       DatePosted <- append(DatePosted, postdate[2])
     }
     else if (length(postdate) == 0){
       DatePosted <- append(DatePosted, 'NA')
     }
     
     else{
       DatePosted <- append(DatePosted, postdate)
       
     }

     
  }
  
  
  
  df <- data.frame(CompanyName, JobCategory, JobTitle, Location, DatePosted, Remote, Salary,EstimatedSalary, Ratings, YearFounded, CompanySize, EmploymentInfo, JobLinks)
  
  
  return(df)
}




