
#### Loading the test datasets
Data1 <- read.csv(here::here("tests/testthat/testsets", "Study1.csv"), sep = ";", dec = ",")
Data2 <- read.csv(here::here("tests/testthat/testsets", "Study2.csv"), sep = ";", dec = ",")
Data3 <- read.csv(here::here("tests/testthat/testsets", "Study3.csv"), sep = ";", dec = ",")
Data4 <- read.csv(here::here("tests/testthat/testsets", "Study4.csv"), sep = ";", dec = ",")

Data1$Sex <- as.factor(Data1$Sex)
Data2$Sex <- as.factor(Data2$Sex)
Data3$Sex <- as.factor(Data3$Sex)
Data4$Sex <- as.factor(Data4$Sex)

#### Defining the server-side data

dslite.server <- DSLite::newDSLiteServer(tables=list(Data1=Data1,
                                             Data2=Data2,
                                             Data3=Data3,
                                             Data4=Data4))



#### Defining the server-side packages
dslite.server$config(DSLite::defaultDSConfiguration(include=c("dsBase")))
dslite.server$profile()

#### Building the 4 different DSLite Servers with the different datasets
logindata.dslite.data <- data.frame(server = c("Server1", "Server2", "Server3", "Server4"),
                                    url = c("dslite.server", "dslite.server", "dslite.server", "dslite.server"),
                                    user = "",
                                    password = "",
                                    table = c("Data1", "Data2", "Data3", "Data4"),
                                    options = "",
                                    driver = c("DSLiteDriver", "DSLiteDriver", "DSLiteDriver", "DSLiteDriver"))


#### Login to the 4 different DSLite Servers
conns <- DSI::datashield.login(logindata.dslite.data, assign=TRUE, symbol = "D")
