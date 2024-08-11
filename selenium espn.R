library(RSelenium)
library(rvest)
library(xml2)
library(tidyverse)
#docker run -d -p 4445:4444 selenium/standalone-firefox

remdr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4445L,
                      browserName = "firefox")

remdr$open()
remdr$navigate("https://espn.com/login")#Entering our URL gets the browser to navigate to the page

webElem <- remdr$findElement("css", "iframe")
remdr$switchToFrame(webElem)
remdr$findElement(using = "xpath", value = "/html/body/div[1]/div/div")
user <- remdr$findElement(using = "xpath", value = "/html/body/div[1]/div/div/section/section/form/section/div[1]/div/label/span[2]/input")
user$highlightElement()
user$sendKeysToElement(list("pauljbeach@gmail.com"))
pass <- remdr$findElement(using = "xpath", value = "/html/body/div[1]/div/div/section/section/form/section/div[2]/div/label/span[2]/input")
pass$highlightElement()
pass$sendKeysToElement(list("@Montucky72"))

logbut <- remdr$findElement(using = "xpath", "/html/body/div[1]/div/div/section/section/form/section/div[3]/button")
logbut$highlightElement()
logbut$clickElement()
remdr$navigate("https://fantasy.espn.com/football/draft?leagueId=14819806&seasonId=2022&teamId=1&memberId={1505CFD7-906A-426B-85CF-D7906AD26B75}")
remdr$screenshot(display = T)
remdr$refresh()
recon <-remdr$findElement(using = "xpath", "/html/body/div[1]/div[1]/section/div/div[2]/main/div/div/div[1]/section/div/div/div[3]/button[2]")
recon$highlightElement()
recon$clickElement()
table <- remdr$findElement(using = "xpath", "/html/body/div[1]/div[1]/section/div/div[2]/main/div/div/div[3]/div[2]/div[2]/div/div/div[1]/section/div[3]/div/div")
table <- remdr$findElement(using = "id", "players-table")
remdr$screenshot(display = T)
data_table_html <- table$getPageSource()
page <- read_html(data_table_html %>% unlist())
df <- html_table(page) %>% .[[2]]
all_data <- rbindlist(list(all_data, df))
table$highlightElement()
table$clickElement()
table$getPageSource()
rvest::html_form_set()
rvest::session(remdr$)

session("espn.com/login")


# remdr$getCurrentUrl()
# remdr$switchToFrame(NA)
