#---- 1. Working directory ----
user<-Sys.info()[["user"]]
wd = paste0("C:/Users/",user,"/OneDrive/3_Work_Research/uon-msc-thesis")
# Input Folders
hmda.folder <- "/Data/RawData-HMDA/"
tri.folder = "/Data/RawData-TRI/"
census.folder = "/Data/US-census/"
# Output Folders
panel.folder = "/Data/PanelData/"
graph.folder = "/Graphs/"

#---- 2. Economic Constants
## Avg 30-year US Treasuary yield
US30Y.2018=3.11
US30Y.2019=2.58
US30Y.2020=1.56
US30Y.2021=2.06

## FHFA Conforming loan limit value for one-unit property
CONF.LOAN.LIMIT.2018=453100
CONF.LOAN.LIMIT.2019=510400
CONF.LOAN.LIMIT.2020=548250
CONF.LOAN.LIMIT.2021=647200

## Circle area of impact
RADIUS.1KM = 1000^2*pi
RADIUS.2KM = 2000^2*pi
RADIUS.5KM = 5000^2*pi
RADIUS.10KM = 10000^2*pi
RADIUS.20KM = 20000^2*pi
