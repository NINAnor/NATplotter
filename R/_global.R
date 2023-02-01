
# Load data set (see R/dataRAW.R)

dir <- substr(getwd(), 1,2)

path <- ifelse(dir == "C:", 
               "P:/41001581_egenutvikling_anders_kolstad/data/",
               "/data/Egenutvikling/41001581_egenutvikling_anders_kolstad/data/")

naturtyper <- readRDS(paste0(path, "naturtyper.rds"))
# 5-7 sec load time



# Costim ggplot theme

#Define gppr_theme() function

theme_anders <- function(){ 
  font <- "Georgia"   #assign font family up front
  
  theme_bw() %+replace%    #replace elements we want to change
    
    theme(
      
    )
}
