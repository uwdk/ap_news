#ap news scraper
library(data.table)
library(magrittr)
library(rvest)
library(shiny)
library(DT)
library(shinythemes)
library(lubridate)

topic_url <- fread(text = "subject, url
top,https://apnews.com/apf-topnews
sports,https://apnews.com/apf-sports
business,https://apnews.com/apf-business
mlb,https://apnews.com/MLB
nfl,https://apnews.com/NFL
tech,https://apnews.com/apf-technology
seattle,https://apnews.com/Seattle
coronavirus,https://apnews.com/VirusOutbreak"
)

scrape_ap <- function(topic = "top"){
	get_url <- topic_url[subject == topic, (url)]

	pg <- read_html(get_url)

	#NOTE 03/25/2020 - fix b/c AP photo links don't have same setup and causing mismatch on url
	# card_headline_items <- pg %>% html_nodes("a") %>% html_attr("data-key")
	# url_items <- pg %>% html_nodes("a") %>% html_attr("href")

	# urls <- url_items[grep("card-headline", card_headline_items)]

	all_url <- pg %>% html_nodes("a")

	url_valid <- all_url %>% html_attr("class") %>% grep("Component-headline|Component-link", .)

	urls <- all_url[url_valid] %>% html_attr("href")

	#NOTE 03/21/2020 - fix b/c of extra sponsored links;
	# headlines <- pg %>% html_nodes("h1") %>% html_text %>% .[-1]
	true_headlines <- pg %>% html_nodes("h1") %>% html_attr("data-key") %>% is.na()
	headlines <- pg %>% html_nodes("h1") %>% html_text %>% .[true_headlines]

	timestamp <- pg %>% html_nodes("span") %>% html_attr("data-source") %>% Filter(Negate(is.na), .) %>% ymd_hms()

	#add preview - NOTE - issue appears to be that not all AP Photos have a preview; drop off?
	# preview <- pg %>% html_nodes("p") %>% html_text
	# page_div <- pg %>% html_nodes("div")

	# preview_valid <- page_div %>% html_attr("class") %>% grep("content text", .)

	# preview <- page_div[preview_valid] %>% html_text

	#try w/adding preview at end after remove photos? --- still doesn't work b/c some photos have a preview
	table <- data.table(headlines, urls, timestamp) #, preview)

	table[, urls:= paste0("https://apnews.com", urls)]

	table[, link:= paste0("<a href=\"", urls, "\"target=\"_blank\">", headlines, "</a>")]

	output <- table[, .(timestamp, link)]#, preview)]
}

ui <- fluidPage(theme = shinytheme("slate"),
  titlePanel("AP news"),

    sidebarLayout(
		sidebarPanel(width = 3,
			uiOutput("type_output"),
			sliderInput("rows_to_display", "Set rows to display", min = 0, max = 50, value = 10, step = 5, ticks = FALSE)
		),
	mainPanel(DTOutput("results"))
	)
)

server <- function(input, output) {

	output$type_output <- renderUI({
		radioButtons("type_input", "Select Type",
		topic_url[, subject] %>% unique)
	})

	output$results <- renderDT({
		DT::datatable(scrape_ap(req(input$type_input)), escape = FALSE, rownames = FALSE, filter = "bottom", selection = "none", class = "compact stripe", style = "bootstrap", options = list(pageLength = input$rows_to_display, dom = 'tp', lengthChange = FALSE)) %>%
			formatDate("timestamp", method = 'toLocaleString')
		})
}

# for shiny web app
shinyApp(ui = ui, server = server)
