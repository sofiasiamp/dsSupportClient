
#### Loading the test datasets
Data1 <- read.csv(here::here("tests/testthat/testsets", "Study1.csv"), sep = ";")
Data2 <- read.csv(here::here("tests/testthat/testsets", "Study2.csv"), sep = ";")
Data3 <- read.csv(here::here("tests/testthat/testsets", "Study3.csv"), sep = ";")
Data4 <- read.csv(here::here("tests/testthat/testsets", "Study4.csv"), sep = ";")


#### Defining the server-side data
dslite.server <- newDSLiteServer(tables=list(Data1=Data1,
                                             Data2=Data2,
                                             Data3=Data3,
                                             Data4=Data4))

#### Defining the server-side packages
dslite.server$config(defaultDSConfiguration(include=c("dsBase")))
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
conns <- datashield.login(logindata.dslite.data, assign=T, symbol = "D")
