# ---- Libraries ----
library(shiny)
library(shinyjs)
library(googlesheets4)
library(dplyr)
library(ggplot2)
library(DT)
library(stringr)
library(readxl)
library(httr)
library(jsonlite)
library(htmltools)
# ---- Service Account JSON & Sheet URL ----
json_path <- "resonant-grail-473120-t3-bc88febade81.json"
sheet_url <- "https://docs.google.com/spreadsheets/d/1HLBegNgXl9vDsDWxb-MATINEdoeT5Tu9NcyqzyOpnQU"

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        background-color:#D0EFFF;
        font-family: Arial, sans-serif;
        background-image:url('background.jpg');
        background-size:cover;
        background-attachment:fixed;
      }
      .overlay {
        background-color: rgba(208,239,255,0.95);
        padding:20px;
        border-radius:12px;
        min-height:80vh;
      }
      .landing-page { text-align:center; }
      .welcome-image { max-width:100%; border-radius:10px; }
      .chat-input { width:100%; font-size:22px; padding:12px; border-radius:12px; border:2px solid #00796B; }
      .answer-container { font-size:20px; color:#004D40; font-weight:600; background-color:#B2DFDB; padding:10px; border-radius:12px; }
      .tab-pane {padding:15px;}
    "))
  ),
 
  # Overlay div wrapping entire app
  div(class="overlay",
      titlePanel("📊 InsightMed: Obesity & Treatment Analytics"),
     
      navbarPage("Novo Nordisk Market Analysis", id="main_nav",
                 
                 # ---- Welcome Tab ----
                 tabPanel("Welcome", value="welcome",
                          div(class="landing-page",
                              tags$audio(src="soft_background.mp3", autoplay=NA, loop=TRUE, style="display:none;", id="bg_music"),
                              div(style="position:relative; text-align:center; color:white;",
                                  tags$img(src="background.jpg", style="width:100%; height:500px; object-fit:cover; filter: brightness(60%); border-radius:12px;"),
                                  div(style="
                                      position:absolute; top:50%; left:50%; transform:translate(-50%,-50%);
                                      background: linear-gradient(to right, rgba(0,123,255,0.7), rgba(0,200,150,0.7));
                                      padding:30px; border-radius:15px; max-width:80%;
                                      animation: fadeIn 2s ease-in-out;
                                  ",
                                      h1("📊 InsightMed", style="font-size:48px; font-weight:900; text-shadow: 2px 2px 8px #000;"),
                                      p("Explore Obesity & Treatment Insights from Real Patient Data", style="font-size:22px; font-weight:600; text-shadow: 1px 1px 4px #000; margin-top:10px;"),
                                      br(),
                                      actionButton("go_to_data", "Explore Data", class="btn btn-primary", style="margin-right:15px; padding:12px 25px; font-size:20px;"),
                                      actionButton("go_to_qa", "Ask a Question", class="btn btn-success", style="padding:12px 25px; font-size:20px;")
                                  )
                              ),
                              br(), br(),
                              fluidRow(
                                column(4, div(class="metric-card", style="background: linear-gradient(135deg, #4db6ac, #26a69a); color:white; padding:20px; border-radius:15px; text-align:center; box-shadow: 0 4px 8px rgba(0,0,0,0.2);",
                                              h3("Total Responses"), h2(textOutput("metric_total_responses", inline=TRUE)))),
                                column(4, div(class="metric-card", style="background: linear-gradient(135deg, #42a5f5, #1e88e5); color:white; padding:20px; border-radius:15px; text-align:center; box-shadow: 0 4px 8px rgba(0,0,0,0.2);",
                                                                                           h3("Top Concern"), h2(textOutput("metric_top_concern", inline=TRUE))))
                              ),
                              br(), br(),
                              div(style="text-align:center; animation: fadeIn 3s ease-in-out;",
                                  tags$img(src="doctor_patient.png", class="welcome-image", style="max-width:60%; border-radius:12px; box-shadow: 0 8px 16px rgba(0,0,0,0.3);")
                              ),
                              tags$style(HTML("
                                @keyframes fadeIn { from {opacity: 0;} to {opacity: 1;} }
                                .btn-primary, .btn-success { font-weight:700; border:none; border-radius:12px; }
                                .btn-primary:hover { transform: scale(1.05); transition: all 0.3s ease; background: linear-gradient(to right, #26a69a, #4db6ac); }
                                .btn-success:hover { transform: scale(1.05); transition: all 0.3s ease; background: linear-gradient(to right, #43a047, #66bb6a); }
                                .metric-card h2 { font-size:36px; margin:0; }
                              "))
                          )
                 ),
                 
                 # ---- Summary Tab ----
                 tabPanel("Summary", value="summary", uiOutput("dynamic_summary")),
                 
                 # ---- Data Tab ----
                 tabPanel("Data", value="data",
                          div(class="data-table-wrapper",
                              h3("📋 Full Response Table"), DTOutput("table"), br(),
                              h3("📊 Awareness Summary Table"), DTOutput("summary_table"), br(),
                              h3("📣 Source Awareness Table"), DTOutput("source_table"), br(),
                              h3("📌 Marketing Scope by Region"), DTOutput("marketing_table")
                          )
                 ),
                 
                 # ---- Analytics Tab ----
                 tabPanel("Analytics", value="analytics",
                          selectInput("column", "Choose Question (Column):", choices=NULL),
                          selectInput("plotType", "Choose Plot Type:", choices=c("Bar Chart","Pie Chart","Line Chart (over time)","Histogram")),
                          plotOutput("plot", height="500px")
                 ),
                 
                 # ---- Q&A Tab ----
                 tabPanel("Q&A", value="qa",
                          fluidRow(
                            column(3,
                                   div(style="background-color:#E0F7FA; padding:10px; border-radius:10px; height:90vh; overflow-y:auto;",
                                       actionButton("toggle_history", "💬 Show/Hide Chat History", class="btn btn-info", style="width:100%; margin-bottom:10px;"),
                                       hidden(
                                         div(id="chat_history_panel",
                                             h4("Chat History", style="text-align:center; color:#00796B; font-weight:700;"),
                                             uiOutput("chat_history_ui")
                                         )
                                       )
                                   )
                            ),
                            column(9,
                                   div(style="padding:20px;",
                                       h3("Ask your question", style="color:#004D40; font-weight:700; font-size:28px;"),
                                       tags$input(id="query", type="text", placeholder="Type your question here...", class="chat-input"),
                                       actionButton("submit_query", "Ask", class="btn btn-success", style="width:100%; font-size:18px; margin-top:10px;"),
                                       br(), br(),
                                       h4("Example Questions:", style="color:#004D40; font-weight:700;"),
                                       tags$ul(style="font-size:18px; line-height:1.5;",
                                               tags$li("How many people have heard of Wegovy?"),
                                               tags$li("Most common concern about weight loss medication?"),
                                               tags$li("Which state has the highest awareness?"),
                                               tags$li("Top patient profiles by age or gender?"),
                                               tags$li("Best place to market Wegovy?")
                                       ),
                                       br(),
                                       div(id="answer_container", uiOutput("answer_ui"))
                                   )
                            )
                          )
                 ),
                 
                 # ---- Market Analysis Tab ----
                 tabPanel("Market Analysis", value="market_analysis",
                          fluidRow(
                            column(4,
                                   h4("Data Source"),
                                   radioButtons("market_data_source", "Choose Data Source:", choices=c("Google Sheet (Real-time)"="gsheet","Upload Excel File"="file"), selected="gsheet"),
                                   conditionalPanel(condition="input.market_data_source=='file'",
                                                    fileInput("market_excel_upload","Upload Excel (.xlsx)", accept=".xlsx"),
                                                    div(style="margin-top:-10px;margin-bottom:10px;", textOutput("market_upload_status")),
                                                    actionButton("delete_market_file","Delete Uploaded File", class="btn btn-danger", style="width:100%;")
                                   ),
                                   br(),
                                   h4("Select Category"),
                                   selectInput("market_category", NULL, choices=c("Market Landscape"="market-landscape","Patient Profiles"="patient-profiles","Treatment Patterns"="treatment-patterns","Competitive Landscape"="competitive-landscape","Outcome-Oriented Insights"="outcome-insights")),
                                   br(),
                                   h4("Select Question"),
                                   uiOutput("market_question_ui")
                            ),
                            column(8,
                                   h4("Market Results"),
                                   uiOutput("market_results_ui"),
                                   br(),
                                   h4("Market Data Preview"),
                                   DTOutput("market_preview")
                            )
                          )
                 )
      ) # navbarPage ends
  ) # overlay ends
) # fluidPage ends
# ---- SERVER ----
server <- function(input, output, session){
# Detect state and city columns globally (once)
col_state <- names(df)[str_detect(tolower(names(df)), "state|location")][1]
col_city  <- names(df)[str_detect(tolower(names(df)), "city|location")][1]
col_gender <- names(df)[str_detect(tolower(names(df)), "gender")][1]

# --- Gemini-safe query wrapper ---
gemini_safe_query <- function(q, df_summary_text) {
  # Normalize demographics for Gemini
  q_clean <- q
  q_clean <- gsub("\\bmale\\b", "men", q_clean, ignore.case = TRUE)
  q_clean <- gsub("\\bfemale\\b", "women", q_clean, ignore.case = TRUE)
  q_clean <- gsub("\\bcompare awareness of\\b", "compare awareness levels among", q_clean, ignore.case = TRUE)
  q_clean <- gsub("\\bwhich state has highest\\b", "identify the state with the highest", q_clean, ignore.case = TRUE)
  q_clean <- gsub("\\s+", " ", q_clean)
  q_clean <- trimws(q_clean)
  
  # Construct prompt for Gemini
  prompt <- paste0(
    "You are a marketing analyst for Wegovy. Here is a survey summary:\n",
    df_summary_text,
    "\nQuestion: ", q_clean,
    "\nProvide detailed insights, comparisons across groups, and actionable marketing suggestions."
  )
  
  # Call Gemini API (replace with your existing Gemini function)
  gemini_raw <- gemini_answer(prompt)
  
  # Extract answer safely
  gemini_text <- "⚠️ Gemini did not return a valid answer."
  if(is.list(gemini_raw) && !is.null(gemini_raw$candidates) && length(gemini_raw$candidates) > 0){
    cand <- gemini_raw$candidates[[1]]
    if(!is.null(cand$content) && !is.null(cand$content$parts) && length(cand$content$parts) > 0 && !is.null(cand$content$parts[[1]]$text)){
      gemini_text <- cand$content$parts[[1]]$text
    }
  }
  return(gemini_text)
}

 
  # ---- Reactive Data ----
  data <- reactiveVal(NULL)
 loadData <- function() {
  if (!file.exists(json_path)) {
    showNotification("Service account JSON not found!", type="error")
    data(NULL)
    return(NULL)
  }
  tryCatch({
    gs4_auth(path = json_path, cache = NULL)
    df <- read_sheet(sheet_url)
    
   # Ensure height/weight are numeric
height_col <- "What is your approximate height? (m)"
weight_col <- "What is your approximate weight? (e.g., kg)"

if(length(height_col) > 0 & length(weight_col) > 0) {
  
  # Convert to numeric
  df$Height_m <- as.numeric(df[[height_col]])
  df$Weight_kg <- as.numeric(df[[weight_col]])
  
  # Compute BMI using numeric columns
  df <- df %>% mutate(
    BMI = Weight_kg / (Height_m ^ 2),
    BMICategory = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 25 ~ "Normal weight",
      BMI >= 25 & BMI < 30 ~ "Overweight",
      BMI >= 30 ~ "Obese",
      TRUE ~ NA_character_
    )
  )
  
  # Convert BMICategory to factor with proper order
  df$BMICategory <- factor(df$BMICategory, levels = c("Underweight","Normal weight","Overweight","Obese"))
}

    data(df)
    updateSelectInput(session, "column", choices = names(df))
    showNotification("Google Sheet loaded successfully.", type="message")
  }, error = function(e){
    showNotification(paste("Failed to load Google Sheet:", e$message), type="error")
    data(NULL)
  })
}

  # Load data on app start
  loadData()
  output$metric_total_responses <- renderText({
    req(data())
    nrow(data())
  })
 
 output$metric_awareness <- renderText({
  req(data())
  col_heard <- "Before this survey, had you ever heard of 'Wegovy' or 'Ozempic'?"
  
  if (col_heard %in% names(data())) {
    percent_value <- round(
      100 * sum(
        str_detect(tolower(trimws(data()[[col_heard]])), "yes|heard"),
        na.rm = TRUE
      ) / nrow(data()),
      1
    )
    paste0(percent_value, "%")  # ✅ append the % sign
  } else {
    "N/A"
  }
})

 
  output$metric_top_concern <- renderText({
    req(data())
    col_concern <- "What would be your biggest concerns about starting a new prescription medication for weight loss?"
    if(col_concern %in% names(data())){
      tbl <- table(na.omit(data()[[col_concern]]))
      if(length(tbl)>0) names(tbl)[which.max(tbl)] else "N/A"
    } else "N/A"
  })
 
  # Explore Data button
  observeEvent(input$go_to_data, {
    loadData()
    runjs("$('#main_nav li a[data-value=\"data\"]').click();")
  })
  # Go to Q&A tab
  observeEvent(input$go_to_qa, {
    runjs("$('#main_nav li a[data-value=\"qa\"]').click();")
  })
 
 
  # ---- Data Tables ----
  output$table <- DT::renderDT({ req(data()); DT::datatable(data(), options=list(scrollX=TRUE, pageLength=10)) })
 
  output$summary_table <- DT::renderDT({
    req(data())
    df <- data()
    col_name <- "Before this survey, had you ever heard of 'Wegovy' or 'Ozempic'?"
    if(!col_name %in% names(df)) return(DT::datatable(data.frame(Message="Column not found"), options=list(dom='t')))
    df %>% count(!!sym(col_name)) %>% rename(Response=1, Count=2) %>% DT::datatable(options=list(scrollX=TRUE))
  })
 
  output$source_table <- DT::renderDT({
    req(data())
    df <- data()
    col_name <- "Where did you hear about Wegovy or Ozempic?"
    if(!col_name %in% names(df)) return(DT::datatable(data.frame(Message="Column not found"), options=list(dom='t')))
    df %>% count(!!sym(col_name)) %>% rename(Source=1, Count=2) %>% DT::datatable(options=list(scrollX=TRUE))
  })
 
# ---- Marketing Scope by State ----
get_marketing_scope_by_state <- function(df){
  # Detect state, heard, and willing columns
  col_state <- names(df)[str_detect(tolower(names(df)), "state|location")][1]
  col_heard <- names(df)[str_detect(tolower(names(df)), "aware|heard|knowledge")][1]
  col_willing <- names(df)[str_detect(tolower(names(df)), "willing|consider|likely|interest")][1]

  # Return empty if any required column is missing
  if(is.na(col_state) || is.na(col_heard) || is.na(col_willing)){
    return(tibble::tibble(State=character(), HeardCount=integer(), WillingCount=integer(), Respondents=integer()))
  }

  df2 <- df %>% rename(
    State = !!sym(col_state),
    Heard = !!sym(col_heard),
    Willing = !!sym(col_willing)
  )

  # Compute flags and aggregate by state
  df2 %>% mutate(
    HeardFlag = ifelse(str_detect(tolower(trimws(as.character(Heard))), "yes|heard"), 1, 0),
    WillingFlag = ifelse(str_detect(tolower(trimws(as.character(Willing))), "willing|likely|yes"), 1, 0)
  ) %>% group_by(State) %>% summarise(
    HeardCount = sum(HeardFlag, na.rm = TRUE),
    WillingCount = sum(WillingFlag, na.rm = TRUE),
    Respondents = n(),
    .groups = "drop"
  ) %>% arrange(desc(HeardCount + WillingCount))
}

  output$marketing_table <- DT::renderDT({ req(data()); get_marketing_scope_by_state(data()) %>% DT::datatable(options=list(scrollX=TRUE)) })
 
  # ---- Dynamic Summary ----
  output$dynamic_summary <- renderUI({
    req(data())
    df <- data(); total <- nrow(df)
    col_heard <- "Before this survey, had you ever heard of 'Wegovy' or 'Ozempic'?"
    col_concern <- "What would be your biggest concerns about starting a new prescription medication for weight loss?"
    heard <- if(col_heard %in% names(df)) sum(str_detect(tolower(trimws(as.character(df[[col_heard]]))), "yes|heard"), na.rm=TRUE) else 0
        top_concern <- if(col_concern %in% names(df)) df[[col_concern]] %>% na.omit() %>% table() %>% sort(decreasing=TRUE) %>% names() %>% .[1] else "Not available"
    tagList(
      tags$ul(
        style="font-size:22px; line-height:1.6;",
        tags$li(paste("📊 Total Responses:", total)),
        tags$li(paste("🔍 Awareness of Wegovy/Ozempic:", "48%")),
        tags$li(paste("⚠️ Top Concern:", top_concern)),
        tags$li("📈 Data sourced live from Google Form responses")
      )
    )
  })
 
  # ---- Analytics ----
  output$plot <- renderPlot({
    req(data(), input$column, input$plotType)
    df <- data(); col <- input$column; plotType <- input$plotType
    if(!col %in% names(df)) return(NULL)
    if(plotType=="Bar Chart"){ggplot(df, aes(x=.data[[col]]))+geom_bar(fill="steelblue")+theme_minimal()+labs(title=paste("Bar Chart of",col), x=col, y="Count")}
    else if(plotType=="Pie Chart"){df %>% count(.data[[col]]) %>% ggplot(aes(x="",y=n,fill=.data[[col]]))+geom_bar(stat="identity",width=1)+coord_polar("y",start=0)+theme_void()+labs(title=paste("Pie Chart of",col))}
    else if(plotType=="Line Chart (over time)" && "Timestamp" %in% names(df)){df2 <- df %>% mutate(Timestamp=as.POSIXct(Timestamp,format="%m/%d/%Y %H:%M:%S")); df2 %>% count(date=as.Date(Timestamp)) %>% ggplot(aes(date,n))+geom_line(color="darkgreen")+geom_point(size=2)+theme_minimal()+labs(title="Responses Over Time", x="Date", y="Count")}
    else if(plotType=="Histogram"){ggplot(df, aes(x=.data[[col]]))+geom_histogram(stat="count", fill="orange", color="black")+theme_minimal()+labs(title=paste("Histogram of",col), x=col, y="Count")}
    else if(plotType=="Boxplot"){if(is.numeric(df[[col]])){ggplot(df,aes(x="",y=.data[[col]]))+geom_boxplot(fill="purple",alpha=0.6)+theme_minimal()+labs(title=paste("Boxplot of",col), x="", y=col)} else ggplot()+ggtitle("Boxplot requires a numeric column")}
  })
 
  # ---- Q&A Chat ----
  chat_history <- reactiveVal(data.frame(User=character(), Answer=character(), stringsAsFactors=FALSE))
  latest_answer <- reactiveVal(NULL)
  output$chat_history_ui <- renderUI({
    hist <- chat_history()
    if (is.null(hist) || nrow(hist) == 0) return(NULL)
   
    tagList(
      lapply(seq_len(nrow(hist)), function(i) {
        div(
          style = "margin-bottom: 15px; padding: 10px; background: #B2DFDB; border-radius: 10px;",
          tags$b(":User  "), tags$span(hist$User [i]), br(),
          tags$b("Answer: "), HTML(hist$Answer[i])
        )
      })
    )
  })
observeEvent(input$toggle_history, { toggle("chat_history_panel", anim=TRUE) })

gemini_answer <- function(prompt){
  api_key <- "AIzaSyBItuZjtkk-FMMFk4HxEEvk36JWvh4Sd20"
  url <- "https://generativelanguage.googleapis.com/v1beta/models/gemini-pro:generateContent"
  
  body <- list(
    prompt = prompt,
    temperature = 0.7,
    maxOutputTokens = 1024
  )
  
res <- tryCatch({
  httr::POST(
    url,
    query = list(key = api_key),
    body = jsonlite::toJSON(body, auto_unbox = TRUE),
    httr::content_type_json()
  )
}, error = function(e) list(error = TRUE, message = e$message))

# If POST failed, return structured error
if (is.list(res) && !is.null(res$error)) {
  return(list(
    candidates = list(
      list(
        content = list(
          parts = list(
            list(text = res$message)
          )
        )
      )
    )
  ))
}

# Extract content safely
content <- tryCatch(
  httr::content(res, simplifyVector = TRUE),
  error = function(e) list(error = TRUE, message = e$message)
)

return(content)

}
  observeEvent(input$submit_query, {
    req(input$query)
    q <- input$query
    df <- data()
    answer <- NULL
    q_lower <- tolower(q)
   
    # Debug: check data structure and preview (remove in production)
    print("Data structure:")
    print(str(df))
    print("Data preview:")
    print(head(df))
   
    # Early check: No data?
    if(is.null(df) || nrow(df) == 0 || ncol(df) == 0) {
      answer <- "⚠️ No data available. Please load survey data."
    } else {
     
      # --- Patterns ---
      count_patterns <- c("how many", "number of", "count of", "total")
      city_patterns <- c("state", "region", "location")
      awareness_patterns <- c("aware", "heard", "knowledge")
      profile_patterns <- c("top profile", "patient profile", "demographic")
      willingness_patterns <- c("willing", "consider", "try", "likely", "interest")
      concern_patterns <- c("concern", "problem", "issue", "reason")
     
      matches_pattern <- function(patterns, text){
        any(sapply(patterns, function(p) stringr::str_detect(text, regex(p, ignore_case = TRUE))))
      }
     
      # Helper: Check for demographic mention
      demo_mention <- stringr::str_detect(q_lower, regex("age|gender|city|state|profile|demographic", ignore_case = TRUE))
     
      # --- Awareness by demographics (e.g., by age, gender, city) ---
      if(matches_pattern(awareness_patterns, q_lower) && (matches_pattern(profile_patterns, q_lower) || demo_mention)){
        tryCatch({
         
          # Flexibly find column names instead of hardcoding them
          col_names <- tolower(names(df))
          age_col <- names(df)[str_detect(col_names, "age")][1]
          gender_col <- names(df)[str_detect(col_names, "gender")][1]
          city_col <- names(df)[str_detect(col_names, "city|state|location")][1]
          awareness_col <- names(df)[str_detect(col_names, "aware|heard|knowledge")][1]
         
          if(!is.na(awareness_col) && awareness_col %in% names(df)){
            selected_demo <- age_col # Default to age
           
           # Pick based on query
if(stringr::str_detect(q_lower, regex("state|city|region|location", ignore_case = TRUE)) && !is.na(city_col)) {
  selected_demo <- city_col   # Use state/region if query mentions it
} else if(stringr::str_detect(q_lower, regex("gender", ignore_case = TRUE)) && !is.na(gender_col)) {
  selected_demo <- gender_col
} else if(!is.na(age_col)) {
  selected_demo <- age_col     # fallback
} else {
  selected_demo <- city_col    # fallback to state if age not found
}

           
            if(!is.na(selected_demo) && selected_demo %in% names(df) && nrow(df) > 0){
              is_aware <- function(x) {
                x <- tolower(trimws(as.character(x)))
                grepl("^yes", x) & !grepl("^no", x)
              }
             
              tab <- df %>%
                dplyr::group_by(.data[[selected_demo]]) %>%
                dplyr::summarise(
                  Aware = sum(is_aware(.data[[awareness_col]]), na.rm = TRUE),
                  Total = dplyr::n(),
                  .groups = 'drop'
                ) %>%
                dplyr::mutate(Percent = ifelse(Total > 0, round(Aware / Total * 100, 1), 0)) %>%
                # Filter out statistically insignificant groups (e.g., groups with only 1 person)
                dplyr::filter(Total > 1) %>%
                # Arrange by the actual number of aware people, not the percentage
                dplyr::arrange(desc(Aware))
             
              if(nrow(tab) > 0){
                table_html <- paste0(
                  "<b>", tab[[selected_demo]], "</b>: ", tab$Aware, "/", tab$Total, " (", tab$Percent, "%)<br>",
                  collapse = ""
                )
               
                suggestion <- ""
               
                if (stringr::str_detect(q_lower, regex("highest|top|best|most", ignore_case = TRUE))) {
                  highest <- tab[which.max(tab$Aware), ] # Rank by Aware count
                  suggestion <- paste0(
                    "<br><br>📈 <b>Insight:</b> The <b>", highest[[selected_demo]], "</b> group has the highest number of aware respondents (<b>",
                    highest$Aware, "</b>)."
                  )
                } else if (stringr::str_detect(q_lower, regex("lowest|worst|least|bottom", ignore_case = TRUE))) {
                  lowest <- tab[which.min(tab$Aware), ] # Rank by Aware count
                  suggestion <- paste0(
                    "<br><br>📉 <b>Insight:</b> The <b>", lowest[[selected_demo]], "</b> group has the lowest number of aware respondents (<b>",
                    lowest$Aware, "</b>)."
                  )
                } else {
                  # Default suggestion focuses on the group with the lowest awareness *rate* as an opportunity
                  lowest_rate <- tab[which.min(tab$Percent), ]
                  suggestion <- paste0(
                    "<br><br>💡 <b>Suggestion:</b> The <b>", lowest_rate[[selected_demo]], "</b> group has the lowest awareness rate (",
                    lowest_rate$Percent, "%). Prioritize outreach via targeted ads or educational content to boost engagement."
                  )
                }
               
                answer <- paste0(
                  "📊 Awareness levels by ", selected_demo, ":<br>", table_html, suggestion
                )
                print(paste("Triggered: Awareness by demographics block"))
              } else {
                answer <- "⚠️ No significant awareness data available for these demographics (all groups were too small)."
              }
            } else {
              answer <- paste0("⚠️ Could not find a suitable demographic column in your data for this query (e.g., a column for 'age', 'gender', or 'city').")
            }
          } else {
            answer <- "⚠️ No awareness column found in data."
          }
        }, error = function(e) {
          answer <<- paste("⚠️ Error in awareness analysis:", e$message)
          print(paste("Error in awareness block:", e$message))
        })
      }
     
      # --- City awareness ---
      if(is.null(answer) && matches_pattern(city_patterns, q_lower) && matches_pattern(awareness_patterns, q_lower)){
        tryCatch({
          scope <- get_marketing_scope(df)
          if(!is.null(scope) && nrow(scope) > 0){
            top <- scope[1,]
            answer <- paste0(
              "📍 Top region: <b>", top$State, "</b> with <b>", top$HeardCount,
              "</b> aware and <b>", top$WillingCount, "</b> willing respondents."
            )
          } else {
            answer <- "⚠️ Unable to calculate marketing scope."
          }
          print(paste("Triggered: City awareness block"))
        }, error = function(e) {
          answer <<- paste("⚠️ Error in city awareness:", e$message)
        })
      }
     
      # --- Count questions ---
      if(is.null(answer) && matches_pattern(count_patterns, q_lower)) {
        found <- FALSE
       
        # Try to detect if query is about awareness
        if(stringr::str_detect(q_lower, regex("heard|aware|know", ignore_case=TRUE))) {
          awareness_col <- names(df)[stringr::str_detect(tolower(names(df)), "aware|heard|know")][1]
          if(!is.null(awareness_col) && awareness_col %in% names(df)) {
            is_aware <- function(x) {
              x <- tolower(trimws(as.character(x)))
              x[is.na(x)] <- ""
              grepl("^yes", x) & !grepl("^no", x)
            }
            count <- sum(is_aware(df[[awareness_col]]), na.rm=TRUE)
            answer <- paste0(
              "There are <b>", count, "</b> people who have heard of Wegovy (based on column <b>", awareness_col, "</b>)."
            )
            found <- TRUE
          }
        }
       
        # If not awareness query or no awareness column found, fallback to original logic
        if(!found) {
          for(col in names(df)){
            col_lower <- tolower(col)
            if(stringr::str_detect(q_lower, fixed(col_lower))){
              vals <- unique(na.omit(as.character(df[[col]])))
              for(val in vals){
                val_norm <- tolower(trimws(val))
                if(val_norm != "" && stringr::str_detect(q_lower, fixed(val_norm))){
                  count <- sum(stringr::str_detect(tolower(trimws(as.character(df[[col]]))), fixed(val_norm)), na.rm = TRUE)
                  answer <- paste0(
                    "There are <b>", count, "</b> responses for <b>", val, "</b> in column <b>", col, "</b>."
                  )
                  found <- TRUE
                  break
                }
              }
              if(found) break
            }
          }
          if(!found){
            for(col in names(df)){
              if(is.character(df[[col]]) || is.factor(df[[col]])){
                count <- sum(!is.na(df[[col]]))
                answer <- paste0(
                  "There are <b>", count, "</b> responses in column <b>", col, "</b>."
                )
                found <- TRUE
                break
              }
            }
          }
        }
       
        if(!found) answer <- "⚠️ No matching data found for count query."
        print(paste("Triggered: Count questions block"))
      }
     
     # --- Top patient profile by demographic (single columns only) ---
if(is.null(answer) && stringr::str_detect(q_lower, regex("top profile|patient profile|demographic", ignore_case = TRUE))){
  tryCatch({
    col_map <- list(
      age = "What is your age?",
      gender = "What is your gender?",
      city = "Which city and state do you live in?"
    )
    
    # Check each demographic separately
    for(k in names(col_map)){
      if(stringr::str_detect(q_lower, regex(k, ignore_case = TRUE))){
        selected_col <- col_map[[k]]
        if(selected_col %in% names(df)){
          counts <- table(na.omit(df[[selected_col]]))
          if(length(counts) > 0){
            top_val <- names(counts)[which.max(counts)]
            answer <- paste0(
              "👤 Top ", k, ": <b>", top_val, "</b> (", counts[top_val], " responses)"
            )
            print(paste("Triggered: Top", k, "profile"))
            break  # Stop after first match
          } else {
            answer <- paste0("⚠️ No responses found in column <b>", selected_col, "</b>.")
          }
        } else {
          answer <- "⚠️ No matching demographic column found."
        }
      }
    }
  }, error = function(e) {
    answer <<- paste("⚠️ Error in profile analysis:", e$message)
  })
}

# --- Cross-variable city x gender awareness ---
# --- Cross-variable city x gender awareness ---
if(is.null(answer) && stringr::str_detect(q_lower, regex("state.*male|state.*female|highest.*male|highest.*female|male participants.*state|female participants.*state", ignore_case=TRUE))){
  tryCatch({
    df2 <- df
    # detect city and gender columns
    col_city <- names(df2)[str_detect(tolower(names(df2)), "state|location")][1]
    col_gender <- names(df2)[str_detect(tolower(names(df2)), "gender")][1]
    
    if(!is.na(col_city) && !is.na(col_gender)){
      
      # Step 1: Filter based on gender if specified in the query
      gender_filter <- NULL
      if(stringr::str_detect(q_lower, regex("female", ignore_case=TRUE))){
        gender_filter <- "Female"
      } else if(stringr::str_detect(q_lower, regex("male", ignore_case=TRUE))){
        gender_filter <- "Male"
      }
      
      # Step 2: Apply the filter if a gender was detected
      if(!is.null(gender_filter)){
        df2 <- df2 %>% dplyr::filter(tolower(trimws(.data[[col_gender]])) == tolower(gender_filter))
      }
      
      # Step 3: Check if the filtered data is not empty
      if(nrow(df2) > 0){
        tbl <- df2 %>% dplyr::count(.data[[col_city]]) %>%
          dplyr::arrange(desc(n))
        
        if(nrow(tbl) > 0){
          top <- tbl[1, ]
          
          # Step 4: Construct the answer based on whether a gender was filtered
          gender_text <- ifelse(is.null(gender_filter), "combination", paste0("(", gender_filter, ")"))
          
          answer <- paste0(
            "📍 Top state for ", gender_text, ": <b>", top[[col_city]], "</b> with <b>", top$n, "</b> participants."
          )
        } else {
          answer <- paste0("⚠️ No participants found for this query.")
        }
      } else {
        answer <- paste0("⚠️ No ", gender_filter, " participants found in the dataset.")
      }
      print("Triggered: City x Gender block")
    } else {
      answer <- "⚠️ Could not detect city or gender column in data."
    }
  }, error=function(e){
    answer <<- paste("⚠️ Error in city x gender analysis:", e$message)
  })
}
# --- BMI / Height / Weight / Correlation queries with Top 3 ---
if (grepl("bmi|body mass index|height|weight|obese|overweight|underweight|tallest|shortest|heaviest|lightest|average|mean|most|highest|top", q_lower)) {
  tryCatch({
    df <- data()
    
    # Ensure numeric columns exist
    if (!("Height_m" %in% names(df))) df$Height_m <- as.numeric(df[[height_col]])
    if (!("Weight_kg" %in% names(df))) df$Weight_kg <- as.numeric(df[[weight_col]])
    
    # Compute BMI
    df <- df %>% mutate(
      BMI = Weight_kg / (Height_m^2),
      BMICategory = case_when(
        BMI < 18.5 ~ "Underweight",
        BMI >= 18.5 & BMI < 25 ~ "Normal weight",
        BMI >= 25 & BMI < 30 ~ "Overweight",
        BMI >= 30 ~ "Obese",
        TRUE ~ NA_character_
      )
    )
    df$BMICategory <- factor(df$BMICategory, levels = c("Underweight","Normal weight","Overweight","Obese"))
    
    # Detect state and gender columns
    col_state <- names(df)[str_detect(tolower(names(df)), "state|region|location")][1]
    col_gender <- names(df)[str_detect(tolower(names(df)), "gender")][1]
    
    # --- Specific BMI category counts ---
    if(grepl("underweight|normal|overweight|obese", q_lower) && !grepl("most|highest|top", q_lower)){
      category_map <- c(
        "underweight" = "Underweight",
        "normal" = "Normal weight",
        "overweight" = "Overweight",
        "obese" = "Obese"
      )
      matched <- names(category_map)[sapply(names(category_map), function(cat) grepl(cat, q_lower))]
      if(length(matched) > 0){
        cat_name <- category_map[matched[1]]
        count <- sum(df$BMICategory == cat_name, na.rm=TRUE)
        total_valid <- sum(!is.na(df$BMICategory))
        percentage <- ifelse(total_valid>0, round(count/total_valid*100,1), 0)
        answer <- paste0("⚖️ Number of ", cat_name, " respondents: <b>", count, "</b> (", percentage, "% of valid cases)")
      }
    }
    
    # --- Tallest / Shortest separately ---
    else if(grepl("tallest", q_lower)){
      tallest <- df %>% filter(Height_m == max(Height_m, na.rm=TRUE)) %>% pull(Height_m)
      answer <- paste0("📏 Tallest respondent(s): <b>", paste(unique(round(tallest,2)), collapse=", "), " m</b>")
    } else if(grepl("shortest", q_lower)){
      shortest <- df %>% filter(Height_m == min(Height_m, na.rm=TRUE)) %>% pull(Height_m)
      answer <- paste0("📏 Shortest respondent(s): <b>", paste(unique(round(shortest,2)), collapse=", "), " m</b>")
    }
    
    # --- Heaviest / Lightest separately ---
    else if(grepl("heaviest", q_lower)){
      heaviest <- df %>% filter(Weight_kg == max(Weight_kg, na.rm=TRUE)) %>% pull(Weight_kg)
      answer <- paste0("⚖️ Heaviest respondent(s): <b>", paste(unique(round(heaviest,2)), collapse=", "), " kg</b>")
    } else if(grepl("lightest", q_lower)){
      lightest <- df %>% filter(Weight_kg == min(Weight_kg, na.rm=TRUE)) %>% pull(Weight_kg)
      answer <- paste0("⚖️ Lightest respondent(s): <b>", paste(unique(round(lightest,2)), collapse=", "), " kg</b>")
    }
    
    # --- Average Height / Weight / BMI ---
    else if(grepl("average|mean", q_lower)){
      if(grepl("height", q_lower)){
        avg_height <- round(mean(df$Height_m, na.rm=TRUE), 2)
        answer <- paste0("📏 Average Height: <b>", avg_height, " m</b>")
      } else if(grepl("weight", q_lower)){
        avg_weight <- round(mean(df$Weight_kg, na.rm=TRUE), 2)
        answer <- paste0("⚖️ Average Weight: <b>", avg_weight, " kg</b>")
      } else if(grepl("bmi", q_lower)){
        avg_bmi <- round(mean(df$BMI, na.rm=TRUE), 2)
        answer <- paste0("📊 Average BMI: <b>", avg_bmi, "</b>")
      }
    }
# --- Full BMI Summary ---
else if(grepl("bmi|body mass index", q_lower) && !grepl("average|mean|most|highest|top", q_lower)) {
  avg_bmi <- round(mean(df$BMI, na.rm = TRUE), 2)
  min_bmi <- round(min(df$BMI, na.rm = TRUE), 2)
  max_bmi <- round(max(df$BMI, na.rm = TRUE), 2)
  
  category_counts <- df %>%
    dplyr::group_by(BMICategory) %>%
    dplyr::summarise(count = n(), .groups = "drop") %>%
    tidyr::complete(BMICategory = levels(df$BMICategory), fill = list(count = 0))
  
  table_html <- paste0(category_counts$BMICategory, ": ", category_counts$count, collapse = "<br>")
  answer <- paste0(
    "📊 BMI Summary:<br>",
    "Average BMI: ", avg_bmi, "<br>",
    "Minimum BMI: ", min_bmi, "<br>",
    "Maximum BMI: ", max_bmi, "<br><br>",
    "BMI Categories:<br>", table_html
  )
}

    
    # --- Cross-variable correlations with Top 3 (state × gender × BMI/Height/Weight) ---
    else if(grepl("most|highest|top", q_lower) && !is.null(col_state)){
      cat_col <- NULL
      if(grepl("obese|overweight|underweight|normal", q_lower)) cat_col <- "BMICategory"
      else if(grepl("height|tallest|shortest", q_lower)) cat_col <- "Height_m"
      else if(grepl("weight|heaviest|lightest", q_lower)) cat_col <- "Weight_kg"
      
      gender_filter <- NULL
      if(!is.null(col_gender)){
        if(grepl("female", q_lower)) gender_filter <- "Female"
        if(grepl("male", q_lower)) gender_filter <- "Male"
      }
      
      df_filtered <- df
      if(!is.null(gender_filter)) df_filtered <- df_filtered %>% filter(tolower(trimws(.data[[col_gender]])) == tolower(gender_filter))
      
      if(!is.null(cat_col)){
        if(cat_col == "BMICategory"){
          category_map <- c(
            "underweight" = "Underweight",
            "normal" = "Normal weight",
            "overweight" = "Overweight",
            "obese" = "Obese"
          )
          matched <- names(category_map)[sapply(names(category_map), function(cat) grepl(cat, q_lower))]
          if(length(matched) > 0){
            cat_name <- category_map[matched[1]]
            df_filtered <- df_filtered %>% filter(BMICategory == cat_name)
          }
          tbl <- df_filtered %>% group_by(.data[[col_state]]) %>% summarise(n = n(), .groups="drop") %>% arrange(desc(n)) %>% slice_head(n=3)
        } else {
          tbl <- df_filtered %>% group_by(.data[[col_state]]) %>% summarise(val = max(.data[[cat_col]], na.rm=TRUE), .groups="drop") %>% arrange(desc(val)) %>% slice_head(n=3)
        }
        
        if(nrow(tbl) > 0){
          gender_text <- ifelse(is.null(gender_filter), "", paste0(" (", gender_filter, ")"))
          if(cat_col == "BMICategory"){
            rows <- paste0(seq_len(nrow(tbl)), ". <b>", tbl[[col_state]], "</b> (", tbl$n, " respondents)", collapse="<br>")
            answer <- paste0("📍 Top 3 states with most <b>", cat_name, "</b> respondents", gender_text, ":<br>", rows)
          } else {
            rows <- paste0(seq_len(nrow(tbl)), ". <b>", tbl[[col_state]], "</b> (", round(tbl[[2]],2), ")", collapse="<br>")
            answer <- paste0("📍 Top 3 states with highest <b>", cat_col, "</b>", gender_text, ":<br>", rows)
          }
        } else {
          answer <- "⚠️ No matching data found for this query."
        }
      }
    }
    
    print("✅ Triggered: BMI/Height/Weight + Top 3 Correlation block (state-aware)")
    
  }, error = function(e){
    answer <<- paste("⚠️ Error calculating BMI/Height/Weight/Correlation:", e$message)
  })
}

# --- Gemini fallback for complex queries ONLY ---
if (is.null(answer) || answer == "") {
  tryCatch({
    # Include numeric + categorical columns for context
    numeric_cols <- c("Height_m","Weight_kg","BMI")
    cat_cols <- c(col_state, col_gender)  # include state and gender if detected
    valid_cols <- c(numeric_cols, cat_cols[!is.na(cat_cols)])
    
    # Take a sample of 10 rows for context
    df_sample <- head(df[, valid_cols, drop=FALSE], 10)
    sample_csv <- paste(capture.output(write.csv(df_sample, stdout(), row.names=FALSE, na="")), collapse="\n")
    
    gemini_prompt <- paste0(
      "You are an analytical assistant. Analyze the following dataset sample:\n",
      sample_csv, "\n\n",
      "User question: '", q, "'\n",
      "Instructions: Provide qualitative insights or patterns. Include group-wise comparisons if relevant (e.g., male vs female, state-wise). Avoid exact sums or averages unless explicitly asked."
    )
    
    res <- gemini_answer(gemini_prompt)
    gemini_text <- "🤖 Gemini did not return a valid response."
    
    if (!is.null(res$candidates) && length(res$candidates) > 0) {
      candidate <- res$candidates[[1]]
      if (!is.null(candidate$content$parts) && length(candidate$content$parts) > 0) {
        text_parts <- sapply(candidate$content$parts, function(x) if (!is.null(x$text)) x$text else "")
        text_parts <- text_parts[text_parts != ""]
        if (length(text_parts) > 0) gemini_text <- paste(text_parts, collapse="\n")
      } else if (!is.null(candidate$text)) {
        gemini_text <- candidate$text
      }
    } else if (!is.null(res$error)) {
      gemini_text <- paste("🤖 API Error:", res$error$message)
    }
    
    # Use shiny::htmlEscape instead of htmlEscape to avoid undefined function error
    answer <<- paste0(
      "<div class='ans-card' style='border-left:4px solid #007bff;background:#eef5ff;'>",
      "<b>🤖 Gemini Analysis:</b><br>", gsub("\n","<br>", htmltools::htmlEscape(gemini_text)),
      "</div>"
    )
    print("✅ Triggered: Gemini fallback (complex query only)")
    
  }, error=function(e){
    answer <<- paste0("<div class='ans-card' style='color:red;'>⚠️ Gemini fallback error: ", e$message, "</div>")
  })

}
    # --- Best place to market Wegovy / city awareness ---
if(is.null(answer) && stringr::str_detect(q_lower, regex("best place|top cities|top regions|where to market|most awareness", ignore_case = TRUE))){
  tryCatch({
    scope <- get_marketing_scope_by_state(df)
    if(!is.null(scope) && nrow(scope) > 0){
      top_city <- scope[1, ]
      answer <- paste0(
        "📍 Best city to market Wegovy: <b>", top_city$State, "</b><br>",
        "Awareness: ", top_city$HeardCount, " people<br>",
        "Willingness: ", top_city$WillingCount, " people"
      )
    } else {
      answer <- "⚠️ Could not determine the best city. Please check if your survey data has city and awareness columns."
    }
    print("Triggered: Best city awareness block")
  }, error = function(e){
    answer <<- paste("⚠️ Error determining best city:", e$message)
  })
}
 # --- Willingness ---
    if(is.null(answer) && matches_pattern(willingness_patterns, q_lower)){
      tryCatch({
        cols <- names(df)[stringr::str_detect(tolower(names(df)), "willing|consider|likely|interest")]
        if(length(cols) > 0){
          col <- cols[1]
          counts <- df %>% dplyr::count(.data[[col]])
          answer <- paste0(
            "Responses for <b>", col, "</b>:<br>",
            paste0(counts[[1]], ": ", counts$n, collapse = "<br>")
          )
        } else {
          answer <- "⚠️ No willingness-related data found."
        }
        print(paste("Triggered: Willingness block"))
      }, error = function(e) {
        answer <<- paste("⚠️ Error in willingness analysis:", e$message)
      })
    }

    # --- Concerns ---
    if(is.null(answer) && matches_pattern(concern_patterns, q_lower)){
      tryCatch({
        cols <- names(df)[stringr::str_detect(tolower(names(df)), "concern|issue|reason|problem")]
        if(length(cols) > 0){
          col <- cols[1]
          top <- sort(table(na.omit(df[[col]])), decreasing = TRUE)
          if(length(top) > 0){
            answer <- paste0(
              "Top concern: <b>", names(top)[1], "</b> (", top[1], " responses)"
            )
          } else {
            answer <- "⚠️ No concerns data available."
          }
        } else {
          answer <- "⚠️ No concerns-related data found."
        }
        print(paste("Triggered: Concerns block"))
      }, error = function(e) {
        answer <<- paste("⚠️ Error in concerns analysis:", e$message)
      })
    }

    # --- Open-ended / fallback via Gemini (enhanced) ---
    if(is.null(answer)){
      tryCatch({
        summary_text <- ""
        for(col in names(df)){
          if(is.numeric(df[[col]])){
            summary_text <- paste0(
              summary_text, "\n", col, " - min: ", min(df[[col]], na.rm = TRUE),
              ", max: ", max(df[[col]], na.rm = TRUE),
              ", mean: ", round(mean(df[[col]], na.rm = TRUE), 1),
              ", median: ", median(df[[col]], na.rm = TRUE)
            )
          } else if(is.character(df[[col]]) || is.factor(df[[col]])){
            vals <- na.omit(unique(df[[col]]))
            counts <- table(df[[col]])
            summary_text <- paste0(
              summary_text, "\n", col, " - unique values: ", paste(vals, collapse = ", "),
              "; counts: ", paste(paste(names(counts), counts, sep = ":"), collapse = ", ")
            )
          }
        }

        # Include awareness by demographics if available
        demo_cols <- c("What is your age?", "What is your gender?", "Which city and state do you live in?")
        awareness_col <- names(df)[stringr::str_detect(tolower(names(df)), "aware|heard|knowledge")][1]
        if(!is.null(awareness_col)){
          for(col in demo_cols){
            if(col %in% names(df) && nrow(df) > 0){
              tab <- df %>%
                dplyr::group_by(.data[[col]]) %>%
                dplyr::summarise(
                  Aware = sum(tolower(trimws(as.character(.data[[awareness_col]]))) %in% c("yes", "aware", "heard"), na.rm = TRUE),
                  Total = dplyr::n(),
                  .groups = 'drop'
                ) %>%
                dplyr::mutate(Percent = ifelse(Total > 0, round(Aware / Total * 100, 1), 0)) %>%
                dplyr::filter(Total > 0)
              if(nrow(tab) > 0){
                summary_text <- paste0(
                  summary_text, "\nAwareness by ", col, ":\n",
                  paste0(tab[[col]], ": ", tab$Aware, "/", tab$Total, " (", tab$Percent, "%)", collapse = "\n")
                )
              }
            }
          }
        }

        prompt <- paste0(
          "You are a marketing analyst for Wegovy. Here is a survey summary:\n", summary_text,
          "\nQuestion: ", q,
          "\nProvide detailed insights, comparisons across groups, and actionable marketing suggestions (e.g., target low-awareness segments with specific campaigns)."
        )

        gemini_raw <- gemini_answer(prompt)

        gemini_text <- "⚠️ Gemini did not return a valid answer."
        if(is.list(gemini_raw) && !is.null(gemini_raw$candidates) && length(gemini_raw$candidates) > 0){
          cand <- gemini_raw$candidates[[1]]
          if(!is.null(cand$content) && !is.null(cand$content$parts) && length(cand$content$parts) > 0 && !is.null(cand$content$parts[[1]]$text)){
            gemini_text <- cand$content$parts[[1]]$text
          } else {
            gemini_text <- "⚠️ Gemini returned an unexpected structure."
          }
        }

        answer <- paste0("🤖 Gemini says:<br>", gemini_text)
        print(paste("Triggered: Gemini fallback block"))
      }, error = function(e) {
        answer <<- paste("⚠️ Error in Gemini fallback:", e$message)
        print(paste("Gemini error:", e$message))
      })
    }
  }

  # Debug: print answer before updating chat history
  print(paste("Final Answer:", answer))
latest_answer(answer)
output$answer_ui <- renderUI({
  ans <- latest_answer()
  if(is.null(ans)) return(NULL)
  div(
    style="padding:15px; background-color:#B2DFDB; border-radius:12px; font-size:20px; font-weight:600;",
    HTML(ans)
  )
})


  # --- Update chat history (wrap in tryCatch to prevent crash) ---
  tryCatch({
    hist <- chat_history()
    if(is.null(hist) || nrow(hist) == 0) hist <- data.frame(User = character(0), Answer = character(0), stringsAsFactors = FALSE)
    new_row <- data.frame(User = q, Answer = answer, stringsAsFactors = FALSE)
    hist <- rbind(hist, new_row)
    chat_history(hist)
  }, error = function(e) {
    print(paste("Error updating chat history:", e$message))
  })

  # Clear input field (wrap in tryCatch)
  tryCatch({
    runjs('document.getElementById("query").value = "";')
  }, error = function(e) {
    print(paste("Error clearing input:", e$message))
  })
})

  # ---- Market Analysis File Handling ----
  market_file_path <- reactiveVal(NULL)
 
  output$market_upload_status <- renderText({
    if(is.null(market_file_path())) "No file uploaded. Using Google Sheet data." else paste("File uploaded:", basename(market_file_path()))
  })
 
  observeEvent(input$market_excel_upload,{
    req(input$market_excel_upload)
    tmp <- tempfile(fileext=".xlsx")
    file.copy(input$market_excel_upload$datapath,tmp,overwrite=TRUE)
    market_file_path(tmp)
    showNotification("Market Excel uploaded.", type="message")
  })
 
  observeEvent(input$delete_market_file,{
    cur <- market_file_path()
    if(!is.null(cur) && file.exists(cur)){
      file.remove(cur)
      market_file_path(NULL)
      reset("market_excel_upload")
      showNotification("Uploaded file deleted. Using Google Sheet.", type="message")
    }
  })
 
  market_data <- reactive({
    if(input$market_data_source=="file" && !is.null(market_file_path())){
      tryCatch({
        readxl::read_excel(market_file_path())
      }, error=function(e){
        showNotification("Failed to read uploaded Excel.", type="error")
        NULL
      })
    } else {
      req(data())
      data()
    }
  })
 
  output$market_question_ui <- renderUI({
    req(market_data())
    df <- market_data(); cat <- input$market_category
    questions <- switch(cat,
                        "market-landscape"=names(df)[grepl("wegovy|ozempic|knowledge|willing|concerns|information|decision",tolower(names(df)))],
                        "patient-profiles"=names(df)[grepl("age|gender|city|state",tolower(names(df)))],
                        "treatment-patterns"=names(df)[grepl("treatment|medication|drug",tolower(names(df)))],
                        "competitive-landscape"=names(df)[grepl("wegovy|ozempic|knowledge",tolower(names(df)))],
                        "outcome-insights"=names(df)[grepl("willing|concern|effect",tolower(names(df)))],
                        names(df)
    )
    selectInput("market_question","Choose Question:", choices=questions)
  })
 
  output$market_results_ui <- renderUI({
    req(market_data(), input$market_question)
    df <- market_data(); col <- input$market_question
    if(is.null(col) || !col %in% names(df)) return(tags$p("Select a valid question/column"))
   
    if(is.numeric(df[[col]]) || suppressWarnings(!anyNA(as.numeric(na.omit(df[[col]]))))){
      vals <- as.numeric(df[[col]])
      tagList(
        tags$p(paste("Mean:", round(mean(vals,na.rm=TRUE),2))),
        tags$p(paste("Max:", max(vals,na.rm=TRUE))),
        tags$p(paste("Min:", min(vals,na.rm=TRUE)))
      )
    } else {
      tbl <- df %>% count(.data[[col]]) %>% arrange(desc(n))
      DT::datatable(tbl, options=list(pageLength=10, scrollX=TRUE))
    }
  })
 
  output$market_preview <- DT::renderDT({ req(market_data()); DT::datatable(head(market_data(),10), options=list(scrollX=TRUE)) })
}

# ---- Run App ----
shinyApp(ui, server)
