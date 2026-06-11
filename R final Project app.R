install.packages(c(
"shiny",
"bslib",
"plotly",
"DT",
"dplyr",
"tidyr",
"scales",
"shinyWidgets",
"readxl",
"fontawesome"
)) 

# ================================================================
#  UNICEF Implementing Partners Dashboard  |  R Shiny
#  Dataset: UNICEF IP List for Publication 2025
#  Run: shiny::runApp("app.R")
#  Install: install.packages(c("shiny","bslib","plotly","DT",
#           "dplyr","tidyr","scales","shinyWidgets","readxl"))
# ================================================================


library(shiny)
library(bslib)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(scales)
library(shinyWidgets)
library(readxl)
library(fontawesome)

# ── 1. Palette ──────────────────────────────────────────────────
P <- list(
  dark   = "#0A2342",
  mid    = "#1565C0",
  royal  = "#1976D2",
  bright = "#42A5F5",
  pale   = "#BBDEFB",
  ice    = "#E3F2FD",
  white  = "#FFFFFF",
  lgray  = "#F4F6F9",
  gray   = "#90A4AE",
  dgray  = "#455A64",
  accent = "#00ACC1",
  warn   = "#FF8F00"
)
seq_pal <- colorRampPalette(c(P$dark, P$bright, P$pale))
risk_col <- c(Low="#1565C0", Moderate="#42A5F5", Significant="#FF8F00", High="#C62828")

region_labels <- c(
  EAPR="East Asia & Pacific", ECAR="Europe & Central Asia",
  ESAR="Eastern & Southern Africa", HQ="Headquarters",
  LACR="Latin America & Caribbean", MENAR="Middle East & North Africa",
  SAR="South Asia", WCAR="West & Central Africa"
)

# ── 2. Load Data ─────────────────────────────────────────────────
load_data <- function() {
  candidates <- c(
    "UNICEF IP List for Publication 2025.xlsx",
    "UNICEF_IP_List_2025.xlsx",
    "UNICEF_IP_List_2025_sample.xlsx"
  )
  for (f in candidates) {
    if (file.exists(f)) {
      df <- tryCatch(read_excel(f), error = function(e) NULL)
      if (!is.null(df) && nrow(df) > 0) { message("Loaded: ", f); return(df) }
    }
  }
  # Built-in sample
  message("Using built-in sample data")
  set.seed(42)
  mk <- function(region, countries, n) {
    adj  <- c("National","International","Global","Regional","Community","Child","Women","Social","Youth")
    noun <- c("Development","Welfare","Rights","Support","Care","Aid","Relief","Protection")
    suf  <- c("Foundation","Society","Institute","Organization","Ministry","Agency","Fund","Alliance")
    data.frame(
      `Partner Name`             = paste(sample(adj,n,T),sample(noun,n,T),sample(suf,n,T)),
      `Partner Type`             = sample(c("Government","NGO","UN Agency","Academic","Private"),n,T,prob=c(.3,.4,.1,.1,.1)),
      Country                    = sample(countries,n,T),
      Region                     = region,
      `Programme Area`           = sample(c("Health","Education","Child Protection","WASH","Nutrition","Social Policy","Gender Equality","Emergency Response","HIV/AIDS","Early Childhood"),n,T),
      `Cash Transfer 2023 (USD)` = round(rlnorm(n,11,1.8),2),
      `Cash Transfer 2024 (USD)` = round(rlnorm(n,11,1.8),2),
      `Risk Rating`              = sample(c("Low","Moderate","Significant","High"),n,T,prob=c(.4,.35,.15,.1)),
      check.names=FALSE, stringsAsFactors=FALSE
    )
  }
  rc <- list(
    EAPR =c("Indonesia","Philippines","Thailand","Vietnam","Myanmar","Cambodia","Timor-Leste"),
    ECAR =c("Ukraine","Turkey","Kazakhstan","Uzbekistan","Georgia","Armenia","Tajikistan"),
    ESAR =c("Ethiopia","Kenya","Tanzania","Uganda","Mozambique","Zimbabwe","Zambia","Malawi","Rwanda"),
    HQ   =c("USA","Switzerland"),
    LACR =c("Brazil","Colombia","Haiti","Guatemala","Honduras","Bolivia","Peru","Ecuador"),
    MENAR=c("Yemen","Syria","Iraq","Jordan","Lebanon","Morocco","Tunisia","Egypt"),
    SAR  =c("India","Pakistan","Bangladesh","Afghanistan","Nepal","Sri Lanka"),
    WCAR =c("Nigeria","DRC","Ghana","Cameroon","Senegal","Mali","Niger","Burkina Faso","Chad")
  )
  sz <- c(EAPR=80,ECAR=80,ESAR=80,HQ=20,LACR=80,MENAR=80,SAR=80,WCAR=80)
  df <- do.call(rbind, mapply(mk, names(rc), rc, sz, SIMPLIFY=FALSE))
  df$`Partner ID` <- paste0("IP-", seq_len(nrow(df))+999)
  df
}

raw <- load_data()

# ── 3. Normalise columns ─────────────────────────────────────────
find_col <- function(cols, patterns) {
  m <- grep(paste(patterns,collapse="|"), cols, ignore.case=TRUE, value=TRUE)
  if (length(m)) m[1] else NA_character_
}
cn <- names(raw)
cm <- list(
  name    = find_col(cn, c("partner name","organisation","org name","name")),
  type    = find_col(cn, c("partner type","type")),
  country = find_col(cn, "country"),
  region  = find_col(cn, "region"),
  area    = find_col(cn, c("programme area","program area","thematic","sector","output")),
  c2024   = find_col(cn, c("2024","cash transfer 2024")),
  c2023   = find_col(cn, c("2023","cash transfer 2023")),
  risk    = find_col(cn, "risk")
)
gc <- function(col) if (!is.na(col) && col %in% names(raw)) raw[[col]] else rep(NA, nrow(raw))

dat <- tibble(
  Name    = gc(cm$name),
  Type    = gc(cm$type),
  Country = gc(cm$country),
  Region  = gc(cm$region),
  Area    = gc(cm$area),
  C2024   = suppressWarnings(as.numeric(gc(cm$c2024))),
  C2023   = suppressWarnings(as.numeric(gc(cm$c2023))),
  Risk    = gc(cm$risk)
) %>% mutate(
  C2024   = replace_na(C2024, 0),
  C2023   = replace_na(C2023, 0),
  YoY     = ifelse(C2023 > 0, (C2024-C2023)/C2023*100, NA_real_),
  RegLbl  = recode(Region, !!!region_labels, .default=Region)
)

all_reg   <- sort(unique(dat$Region[!is.na(dat$Region)]))
all_type  <- sort(unique(dat$Type[!is.na(dat$Type)]))
all_area  <- sort(unique(dat$Area[!is.na(dat$Area)]))
all_risk  <- c("Low","Moderate","Significant","High")
all_risk  <- all_risk[all_risk %in% dat$Risk]
max_cash  <- max(dat$C2024, na.rm=TRUE)

# ── 4. CSS ───────────────────────────────────────────────────────
css <- paste0("
@import url('https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;600;700;800&family=Syne:wght@700;800&display=swap');
body,.navbar{font-family:'Plus Jakarta Sans',sans-serif!important;}
h1,h2,h3,h4,h5,.card-title,.nav-link{font-family:'Syne',sans-serif!important;}
.navbar{background:linear-gradient(135deg,",P$dark," 0%,",P$royal," 100%)!important;border-bottom:3px solid ",P$accent,";}
.navbar-brand{color:white!important;font-weight:800!important;font-size:1.2rem!important;}
.nav-link{color:rgba(255,255,255,.8)!important;font-size:.85rem!important;}
.nav-link.active{color:white!important;border-bottom:3px solid ",P$accent,"!important;}
body{background:",P$lgray,"!important;}

/* KPI */
.kpi-wrap{display:flex;gap:14px;flex-wrap:wrap;margin-bottom:22px;}
.kpi{flex:1;min-width:155px;background:white;border-radius:14px;padding:18px 20px;
  box-shadow:0 2px 14px rgba(0,0,0,.07);border-top:4px solid ",P$mid,";
  transition:transform .18s,box-shadow .18s;}
.kpi:hover{transform:translateY(-4px);box-shadow:0 8px 24px rgba(0,0,0,.13);}
.kpi.a1{border-top-color:",P$accent,";} .kpi.a2{border-top-color:",P$dark,";}
.kpi.a3{border-top-color:",P$warn,";} .kpi.a4{border-top-color:",P$bright,";}
.kpi-ico{font-size:1.4rem;color:",P$bright,";margin-bottom:5px;}
.kpi-v{font-family:'Syne',sans-serif;font-size:1.6rem;font-weight:800;color:",P$dark,";}
.kpi-l{font-size:.7rem;text-transform:uppercase;letter-spacing:.07em;color:",P$gray,";margin-top:2px;}

/* Cards */
.card{border:none!important;border-radius:14px!important;box-shadow:0 2px 14px rgba(0,0,0,.07)!important;}
.card-header{background:linear-gradient(135deg,",P$dark,",",P$royal,")!important;
  border-radius:14px 14px 0 0!important;color:white!important;
  font-family:'Syne',sans-serif!important;font-weight:700!important;font-size:.92rem!important;
  padding:12px 18px!important;letter-spacing:.03em;}

/* Page banner */
.pg-banner{background:linear-gradient(135deg,",P$dark," 0%,",P$royal," 55%,",P$bright," 100%);
  border-radius:14px;padding:24px 30px;margin-bottom:20px;color:white;
  box-shadow:0 4px 24px rgba(10,35,66,.25);position:relative;overflow:hidden;}
.pg-banner::before{content:'';position:absolute;right:-50px;top:-50px;
  width:200px;height:200px;border-radius:50%;background:rgba(255,255,255,.06);}
.pg-banner h4{font-family:'Syne',sans-serif;font-size:1.45rem;font-weight:800;margin:0 0 4px;}
.pg-banner p{margin:0;opacity:.82;font-size:.87rem;}

/* Insight cards */
.ins{background:white;border-radius:12px;padding:18px 22px;margin-bottom:14px;
  border-left:5px solid ",P$mid,";box-shadow:0 2px 12px rgba(0,0,0,.07);transition:transform .18s;}
.ins:hover{transform:translateX(5px);}
.ins.c2{border-color:",P$accent,";} .ins.c3{border-color:",P$warn,";}
.ins.c4{border-color:",P$dark,";} .ins.c5{border-color:#7B1FA2;}
.ins-num{display:inline-flex;align-items:center;justify-content:center;
  width:28px;height:28px;border-radius:50%;background:",P$mid,";
  color:white;font-weight:800;font-size:.82rem;margin-right:9px;flex-shrink:0;}
.ins-t{font-family:'Syne',sans-serif;font-weight:700;font-size:.94rem;
  color:",P$dark,";display:flex;align-items:center;margin-bottom:7px;}
.ins-b{color:",P$dgray,";font-size:.84rem;line-height:1.65;}

/* Filter panel */
.filter-panel{background:white;border-radius:14px;padding:18px;
  box-shadow:0 2px 14px rgba(0,0,0,.07);margin-bottom:18px;}
.filter-panel h6{font-family:'Syne',sans-serif;color:",P$dark,";
  font-size:.8rem;text-transform:uppercase;letter-spacing:.06em;margin-bottom:12px;}

/* DT */
.dataTables_wrapper{font-size:.82rem;}
table.dataTable thead tr{background:",P$dark,"!important;}
table.dataTable thead th{color:white!important;border-right:1px solid ",P$royal,"!important;}
table.dataTable tbody tr:hover{background:",P$ice,"!important;}
::-webkit-scrollbar{width:5px;}
::-webkit-scrollbar-thumb{background:",P$pale,";border-radius:3px;}
")

# ── 5. UI ────────────────────────────────────────────────────────
ui <- page_navbar(
  title = tags$span(
    tags$b("UNICEF"), tags$span(style="font-weight:300;margin-left:5px;opacity:.85;", "IP Dashboard")
  ),
  theme = bs_theme(
    version   = 5,
    bg        = P$lgray,
    fg        = P$dgray,
    primary   = P$mid,
    secondary = P$gray,
    base_font = font_google("Plus Jakarta Sans"),
    heading_font = font_google("Syne")
  ),
  tags$head(tags$style(HTML(css))),
  fillable = FALSE,

  # ── Tab 1: Overview ──────────────────────────────────────────
  nav_panel("Overview", icon = icon("globe"),
    div(style="padding:20px;",
      div(class="pg-banner",
        tags$h4(icon("globe"), " UNICEF Implementing Partners · 2025"),
        tags$p("Donor-funded cash transfers to implementing partners across all UNICEF regions and programme areas.")),
      uiOutput("kpi_row"),
      layout_columns(
        col_widths = c(8, 4),
        card(card_header("Cash Transfers by Region (2024)"), plotlyOutput("bar_region", height="320px")),
        card(card_header("Partner Type Distribution"),       plotlyOutput("donut_type",  height="320px"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Programme Area Breakdown (2024)"), plotlyOutput("bar_area", height="290px")),
        card(card_header("Year-on-Year Change by Region (%)"), plotlyOutput("yoy_bar", height="290px"))
      )
    )
  ),

  # ── Tab 2: Partner Analysis ──────────────────────────────────
  nav_panel("Partner Analysis", icon = icon("handshake"),
    div(style="padding:20px;",
      div(class="pg-banner",
        tags$h4(icon("handshake"), " Partner-Level Analysis"),
        tags$p("Top recipients, type breakdowns, and year-on-year transfer comparisons.")),
      layout_columns(
        col_widths = c(8, 4),
        card(card_header("Top 20 Partners by 2024 Cash Transfer"), plotlyOutput("top_partners",       height="420px")),
        card(card_header("Partners by Type & Region"),             plotlyOutput("heatmap_type_region", height="420px"))
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Cash Transfer Distribution (Log Scale)"),  plotlyOutput("hist_cash",    height="270px")),
       card(card_header("2023 vs 2024 Scatter — Per Partner"), plotlyOutput("scatter_yoy", height="270px"))
      )
    )
  ),

  # ── Tab 3: Regional Deep Dive ────────────────────────────────
  nav_panel("Regional Deep Dive", icon = icon("map"),
    div(style="padding:20px;",
      div(class="pg-banner",
        tags$h4(icon("map"), " Regional Deep Dive"),
        tags$p("Select a region to explore its programme priorities, countries, and risk profile.")),
      fluidRow(column(4,
        selectInput("sel_region", "Select Region:",
          choices = setNames(all_reg, recode(all_reg, !!!region_labels, .default=all_reg)),
          selected = all_reg[1])
      )),
      layout_columns(
        col_widths = c(6, 6),
        card(card_header("Programme Areas in Region"),         plotlyOutput("reg_area_pie",    height="300px")),
        card(card_header("Top Countries by 2024 Transfers"),   plotlyOutput("reg_country_bar", height="300px"))
      ),
      layout_columns(
        col_widths = c(5, 7),
        card(card_header("Risk Rating Breakdown"),  plotlyOutput("reg_risk_donut", height="270px")),
        card(card_header("Partner Type Mix"),       plotlyOutput("reg_type_bar",   height="270px"))
      )
    )
  ),

  # ── Tab 4: Risk & Compliance ─────────────────────────────────
  nav_panel("Risk & Compliance", icon = icon("shield-halved"),
    div(style="padding:20px;",
      div(class="pg-banner",
        tags$h4(icon("shield-halved"), " Risk & Compliance"),
        tags$p("HACT risk ratings and compliance overview across the partner portfolio.")),
      layout_columns(
        col_widths = c(5, 7),
        card(card_header("Risk Distribution — All Regions"),          plotlyOutput("risk_donut",    height="310px")),
        card(card_header("Avg Cash Transfer by Risk Rating (2024)"),  plotlyOutput("risk_cash_bar", height="310px"))
      ),
      layout_columns(
        col_widths = c(7, 5),
        card(card_header("Risk Heatmap: Region × Risk Level"),  plotlyOutput("risk_heatmap",    height="290px")),
        card(card_header("High-Risk Partner Summary"),           DTOutput("high_risk_table"))
      )
    )
  ),

  # ── Tab 5: Data Table ────────────────────────────────────────
  nav_panel("Data Table", icon = icon("table"),
    div(style="padding:20px;",
      div(class="pg-banner",
        tags$h4(icon("table"), " Full Partner Data Table"),
        tags$p("Search, sort, and export all implementing partner records.")),
      card(
        card_header("UNICEF Implementing Partners — 2025"),
        DTOutput("main_table"),
        tags$p(style="font-size:.74rem;color:#90A4AE;margin:8px 0 0;font-style:italic;",
          "Source: UNICEF Transparency Portal · open.unicef.org · IP List for Publication 2025")
      )
    )
  ),

  # ── Tab 6: Insights ──────────────────────────────────────────
  nav_panel("Insights", icon = icon("lightbulb"),
    div(style="padding:20px;",
      div(class="pg-banner",
        tags$h4(icon("lightbulb"), " Key Findings & Insights"),
        tags$p("Data-driven analysis of UNICEF's implementing partner funding landscape.")),
      layout_columns(
        col_widths = c(8, 4),
        uiOutput("insight_cards"),
        tagList(
          card(card_header("Summary Statistics"), tableOutput("stats_tbl")),
          card(card_header("Funding Concentration (Lorenz Curve)"), plotlyOutput("lorenz", height="220px"))
        )
      )
    )
  ),

  # ── Sidebar filters (nav_panel style) ───────────────────────
  nav_spacer(),
  nav_panel("Filters", icon = icon("filter"),
    div(style="padding:20px;max-width:500px;",
      div(class="filter-panel",
        tags$h6("Global Filters — affect all tabs"),
        pickerInput("f_reg","Region:", choices=all_reg, selected=all_reg, multiple=TRUE,
          options=list(`actions-box`=TRUE,`selected-text-format`="count > 2",`count-selected-text`="{0} regions")),
        pickerInput("f_type","Partner Type:", choices=all_type, selected=all_type, multiple=TRUE,
          options=list(`actions-box`=TRUE,`selected-text-format`="count > 2",`count-selected-text`="{0} types")),
        pickerInput("f_area","Programme Area:", choices=all_area, selected=all_area, multiple=TRUE,
          options=list(`actions-box`=TRUE,`selected-text-format`="count > 2",`count-selected-text`="{0} areas")),
        pickerInput("f_risk","Risk Rating:", choices=all_risk, selected=all_risk, multiple=TRUE,
          options=list(`actions-box`=TRUE)),
        sliderInput("f_cash","Cash Transfer 2024 Range (USD M):",
          min=0, max=ceiling(max_cash/1e6), value=c(0,ceiling(max_cash/1e6)),
          step=0.5, pre="$", post="M")
      )
    )
  )
)

# ── 6. Server ────────────────────────────────────────────────────
server <- function(input, output, session) {

  fd <- reactive({
    dat %>% filter(
      (is.na(Region)  | Region %in% input$f_reg),
      (is.na(Type)    | Type   %in% input$f_type),
      (is.na(Area)    | Area   %in% input$f_area),
      (is.na(Risk)    | Risk   %in% input$f_risk),
      C2024 >= input$f_cash[1]*1e6,
      C2024 <= input$f_cash[2]*1e6
    )
  })

  pc  <- function(p) config(p, displayModeBar=FALSE)
  fmm <- function(x) paste0("$", formatC(x/1e6,  format="f", digits=2), "M")
  fmb <- function(x) if(x>=1e9) paste0("$",formatC(x/1e9,format="f",digits=2),"B") else fmm(x)

  bl <- function(p) p %>% layout(
    paper_bgcolor="white", plot_bgcolor=P$lgray,
    font=list(family="Plus Jakarta Sans,sans-serif", color=P$dgray),
    xaxis=list(gridcolor="#DDEEFF"), yaxis=list(gridcolor="#DDEEFF")
  )

  # KPI
  output$kpi_row <- renderUI({
    d <- fd()
    tot  <- sum(d$C2024, na.rm=TRUE)
    np   <- nrow(d)
    nc   <- n_distinct(d$Country[!is.na(d$Country)])
    ph   <- if(np>0) round(mean(d$Risk=="High",na.rm=TRUE)*100,1) else 0
    avg  <- if(np>0) mean(d$C2024, na.rm=TRUE) else 0
    div(class="kpi-wrap",
      div(class="kpi",    div(class="kpi-ico",icon("dollar-sign")), div(class="kpi-v",fmb(tot)),           div(class="kpi-l","Total 2024 Transfers")),
      div(class="kpi a1", div(class="kpi-ico",icon("users")),       div(class="kpi-v",comma(np)),           div(class="kpi-l","Implementing Partners")),
      div(class="kpi a2", div(class="kpi-ico",icon("globe")),       div(class="kpi-v",nc),                  div(class="kpi-l","Countries Covered")),
      div(class="kpi a3", div(class="kpi-ico",icon("triangle-exclamation")), div(class="kpi-v",paste0(ph,"%")), div(class="kpi-l","High-Risk Partners")),
      div(class="kpi a4", div(class="kpi-ico",icon("chart-line")),  div(class="kpi-v",fmm(avg)),            div(class="kpi-l","Avg Transfer / Partner"))
    )
  })

  # Overview: regional bar
  output$bar_region <- renderPlotly({
    d <- fd() %>% filter(!is.na(Region)) %>%
      group_by(Region) %>% summarise(Tot=sum(C2024,na.rm=TRUE),N=n(),.groups="drop") %>%
      arrange(desc(Tot)) %>%
      mutate(Lbl=recode(Region,!!!region_labels,.default=Region), Col=seq_pal(n())[rank(-Tot)])
    req(nrow(d)>0)
    plot_ly(d, x=~Lbl, y=~Tot/1e6, type="bar",
      marker=list(color=d$Col,line=list(color="white",width=1.5)),
      hovertemplate="<b>%{x}</b><br>$%{y:.2f}M<extra></extra>") %>%
      layout(xaxis=list(title="",tickfont=list(size=10)),
             yaxis=list(title="USD Millions",tickprefix="$",ticksuffix="M",gridcolor="#DDEEFF"),
             paper_bgcolor="white",plot_bgcolor=P$lgray,
             font=list(family="Plus Jakarta Sans")) %>% pc()
  })

  # Overview: donut type
  output$donut_type <- renderPlotly({
    d <- fd() %>% filter(!is.na(Type)) %>% count(Type) %>% arrange(desc(n))
    req(nrow(d)>0)
    plot_ly(d, labels=~Type, values=~n, type="pie", hole=0.52,
      marker=list(colors=seq_pal(nrow(d)),line=list(color="white",width=2)),
      textinfo="label+percent",
      hovertemplate="<b>%{label}</b><br>%{value} partners<extra></extra>") %>%
      layout(showlegend=FALSE, paper_bgcolor="white",
             font=list(family="Plus Jakarta Sans"),
             annotations=list(list(text=paste0("<b>",nrow(fd()),"</b><br>Partners"),
               x=.5,y=.5,showarrow=FALSE,
               font=list(size=13,color=P$dark,family="Syne")))) %>% pc()
  })

  # Overview: area bar
  output$bar_area <- renderPlotly({
    d <- fd() %>% filter(!is.na(Area)) %>%
      group_by(Area) %>% summarise(Tot=sum(C2024,na.rm=TRUE),.groups="drop") %>% arrange(desc(Tot))
    req(nrow(d)>0)
    plot_ly(d, y=~reorder(Area,Tot), x=~Tot/1e6, type="bar", orientation="h",
      marker=list(color=seq_pal(nrow(d))),
      text=~paste0("$",formatC(Tot/1e6,format="f",digits=1),"M"), textposition="outside",
      hovertemplate="<b>%{y}</b><br>$%{x:.2f}M<extra></extra>") %>%
      layout(xaxis=list(title="USD M",tickprefix="$",ticksuffix="M",gridcolor="#DDEEFF"),
             yaxis=list(title=""),paper_bgcolor="white",plot_bgcolor=P$lgray,
             font=list(family="Plus Jakarta Sans")) %>% pc()
  })

  # Overview: YoY
  output$yoy_bar <- renderPlotly({
    d <- fd() %>% filter(!is.na(Region),C2023>0) %>%
      group_by(Region) %>% summarise(Y=mean(YoY,na.rm=TRUE),.groups="drop") %>%
      arrange(Y) %>%
      mutate(Lbl=recode(Region,!!!region_labels,.default=Region),
             Col=ifelse(Y>=0,P$mid,P$warn))
    req(nrow(d)>0)
    plot_ly(d, x=~Lbl, y=~Y, type="bar",
      marker=list(color=d$Col,line=list(color="white",width=1)),
      hovertemplate="<b>%{x}</b><br>YoY: %{y:.1f}%<extra></extra>") %>%
      layout(xaxis=list(title="",tickfont=list(size=10)),
             yaxis=list(title="Avg YoY (%)",gridcolor="#DDEEFF",
                        zeroline=TRUE,zerolinecolor=P$gray,zerolinewidth=2),
             paper_bgcolor="white",plot_bgcolor=P$lgray,
             font=list(family="Plus Jakarta Sans")) %>% pc()
  })

  # Partners: top 20
  output$top_partners <- renderPlotly({
    d <- fd() %>% filter(!is.na(Name)) %>% arrange(desc(C2024)) %>% slice_head(n=20)
    req(nrow(d)>0)
    plot_ly(d, y=~reorder(Name,C2024), x=~C2024/1e6, type="bar", orientation="h",
      marker=list(color=seq_pal(nrow(d)),line=list(color="white",width=1)),
      text=~paste0("$",formatC(C2024/1e6,format="f",digits=2),"M"), textposition="outside",
      customdata=~paste0(Region," · ",Area),
      hovertemplate="<b>%{y}</b><br>$%{x:.2f}M<br>%{customdata}<extra></extra>") %>%
      layout(xaxis=list(title="Cash Transfer 2024 (USD M)",tickprefix="$",ticksuffix="M",gridcolor="#DDEEFF"),
             yaxis=list(title="",tickfont=list(size=9)),
             paper_bgcolor="white",plot_bgcolor=P$lgray,
             font=list(family="Plus Jakarta Sans")) %>% pc()
  })

  # Partners: heatmap
  output$heatmap_type_region <- renderPlotly({
    d <- fd() %>% filter(!is.na(Type),!is.na(Region)) %>%
      count(Type,Region) %>% complete(Type,Region,fill=list(n=0))
    req(nrow(d)>0)
    plot_ly(d, x=~Region, y=~Type, z=~n, type="heatmap",
      colorscale=list(c(0,P$ice),c(0.5,P$bright),c(1,P$dark)),
      hovertemplate="<b>%{y}</b> in %{x}<br>%{z} partners<extra></extra>") %>%
      layout(xaxis=list(title=""),yaxis=list(title=""),
             paper_bgcolor="white",font=list(family="Plus Jakarta Sans")) %>% pc()
  })

  # Partners: histogram
  output$hist_cash <- renderPlotly({
    d <- fd() %>% filter(C2024>0)
    req(nrow(d)>0)
    plot_ly(d, x=~log10(C2024), type="histogram", nbinsx=30,
      marker=list(color=P$mid,line=list(color="white",width=0.8)),
      hovertemplate="log10 USD: %{x:.1f}<br>Count: %{y}<extra></extra>") %>%
      layout(xaxis=list(title="Cash Transfer 2024 (log10 USD)"),
             yaxis=list(title="Partners",gridcolor="#DDEEFF"),
             paper_bgcolor="white",plot_bgcolor=P$lgray,
             font=list(family="Plus Jakarta Sans")) %>% pc()
  })
output$scatter_yoy <- renderPlotly({

  d <- fd() %>% 
    filter(!is.na(C2023), !is.na(C2024), C2023 > 0, C2024 > 0)

  req(nrow(d) > 1)

  maxv <- max(c(d$C2023, d$C2024), na.rm = TRUE)
  minv <- min(c(d$C2023, d$C2024), na.rm = TRUE)

  line_df <- data.frame(
    x = c(minv, maxv),
    y = c(minv, maxv)
  )

  plot_ly() %>%
    add_markers(
      data = d,
      x = ~C2023 / 1e6,
      y = ~C2024 / 1e6,
      text = ~Name,
      marker = list(
        size = 7,
        color = P$bright,
        opacity = 0.65,
        line = list(color = P$dark, width = 0.8)
      ),
      hovertemplate = "<b>%{text}</b><br>2023: $%{x:.2f}M<br>2024: $%{y:.2f}M<extra></extra>"
    ) %>%
    add_lines(
      data = line_df,
      x = ~x / 1e6,
      y = ~y / 1e6,
      line = list(color = P$warn, dash = "dot", width = 1.5),
      inherit = FALSE,
      showlegend = FALSE
    ) %>%
    layout(
      xaxis = list(title = "2023 (USD M)", gridcolor = "#DDEEFF"),
      yaxis = list(title = "2024 (USD M)", gridcolor = "#DDEEFF"),
      paper_bgcolor = "white",
      plot_bgcolor = P$lgray,
      showlegend = FALSE,
      font = list(family = "Plus Jakarta Sans")
    ) %>%
    pc()
})

  # Regional reactives
  rfd <- reactive({ fd() %>% filter(Region==input$sel_region) })

  output$reg_area_pie <- renderPlotly({
    d <- rfd() %>% filter(!is.na(Area)) %>%
      group_by(Area) %>% summarise(Tot=sum(C2024,na.rm=TRUE),.groups="drop")
    req(nrow(d)>0)
    plot_ly(d,labels=~Area,values=~Tot,type="pie",
      marker=list(colors=seq_pal(nrow(d)),line=list(color="white",width=2)),
      textinfo="label+percent",
      hovertemplate="<b>%{label}</b><br>$%{value:,.0f}<extra></extra>") %>%
      layout(showlegend=FALSE,paper_bgcolor="white",font=list(family="Plus Jakarta Sans")) %>% pc()
  })

  output$reg_country_bar <- renderPlotly({
    d <- rfd() %>% filter(!is.na(Country)) %>%
      group_by(Country) %>% summarise(Tot=sum(C2024,na.rm=TRUE),.groups="drop") %>%
      arrange(desc(Tot)) %>% slice_head(n=12)
    req(nrow(d)>0)
    plot_ly(d,y=~reorder(Country,Tot),x=~Tot/1e6,type="bar",orientation="h",
      marker=list(color=seq_pal(nrow(d))),
      hovertemplate="<b>%{y}</b><br>$%{x:.2f}M<extra></extra>") %>%
      layout(xaxis=list(title="USD M",tickprefix="$",ticksuffix="M",gridcolor="#DDEEFF"),
             yaxis=list(title=""),paper_bgcolor="white",plot_bgcolor=P$lgray,
             font=list(family="Plus Jakarta Sans")) %>% pc()
  })

  output$reg_risk_donut <- renderPlotly({
    d <- rfd() %>% filter(!is.na(Risk)) %>% count(Risk)
    req(nrow(d)>0)
    plot_ly(d,labels=~Risk,values=~n,type="pie",hole=0.5,
      marker=list(colors=risk_col[d$Risk],line=list(color="white",width=2)),
      textinfo="label+percent",
      hovertemplate="<b>%{label}</b><br>%{value}<extra></extra>") %>%
      layout(showlegend=FALSE,paper_bgcolor="white",font=list(family="Plus Jakarta Sans")) %>% pc()
  })

  output$reg_type_bar <- renderPlotly({
    d <- rfd() %>% filter(!is.na(Type)) %>%
      group_by(Type) %>% summarise(Tot=sum(C2024,na.rm=TRUE),N=n(),.groups="drop") %>% arrange(desc(Tot))
    req(nrow(d)>0)
    plot_ly(d,x=~Type,y=~Tot/1e6,type="bar",
      marker=list(color=seq_pal(nrow(d))),
      text=~paste0(N," IPs"),textposition="outside",
      hovertemplate="<b>%{x}</b><br>$%{y:.2f}M<extra></extra>") %>%
      layout(xaxis=list(title=""),
             yaxis=list(title="USD M",tickprefix="$",ticksuffix="M",gridcolor="#DDEEFF"),
             paper_bgcolor="white",plot_bgcolor=P$lgray,
             font=list(family="Plus Jakarta Sans")) %>% pc()
  })

  # Risk
  output$risk_donut <- renderPlotly({
    d <- fd() %>% filter(!is.na(Risk)) %>% count(Risk) %>%
      mutate(Risk=factor(Risk,levels=c("Low","Moderate","Significant","High"))) %>% arrange(Risk)
    req(nrow(d)>0)
    plot_ly(d,labels=~Risk,values=~n,type="pie",hole=0.55,
      marker=list(colors=risk_col[as.character(d$Risk)],line=list(color="white",width=2)),
      textinfo="label+percent",
      hovertemplate="<b>%{label}</b><br>%{value} partners<extra></extra>") %>%
      layout(showlegend=FALSE,paper_bgcolor="white",
             font=list(family="Plus Jakarta Sans"),
             annotations=list(list(text=paste0("<b>",nrow(fd()),"</b><br>Total"),
               x=.5,y=.5,showarrow=FALSE,font=list(size=13,color=P$dark,family="Syne")))) %>% pc()
  })

  output$risk_cash_bar <- renderPlotly({
    d <- fd() %>% filter(!is.na(Risk)) %>%
      group_by(Risk) %>% summarise(Avg=mean(C2024,na.rm=TRUE),N=n(),.groups="drop") %>%
      mutate(Risk=factor(Risk,levels=c("Low","Moderate","Significant","High"))) %>% arrange(Risk)
    req(nrow(d)>0)
    plot_ly(d,x=~Risk,y=~Avg/1e6,type="bar",
      marker=list(color=risk_col[as.character(d$Risk)],line=list(color="white",width=1.5)),
      text=~paste0("$",formatC(Avg/1e6,format="f",digits=2),"M (",N," IPs)"),textposition="outside",
      hovertemplate="<b>%{x}</b><br>Avg: $%{y:.2f}M<extra></extra>") %>%
      layout(xaxis=list(title="Risk Rating"),
             yaxis=list(title="Avg Transfer (USD M)",tickprefix="$",ticksuffix="M",gridcolor="#DDEEFF"),
             paper_bgcolor="white",plot_bgcolor=P$lgray,font=list(family="Plus Jakarta Sans")) %>% pc()
  })

  output$risk_heatmap <- renderPlotly({
    d <- fd() %>% filter(!is.na(Region),!is.na(Risk)) %>%
      count(Region,Risk) %>%
      complete(Region,Risk=c("Low","Moderate","Significant","High"),fill=list(n=0))
    req(nrow(d)>0)
    plot_ly(d,x=~Risk,y=~Region,z=~n,type="heatmap",
      colorscale=list(c(0,P$ice),c(0.5,P$pale),c(1,"#C62828")),
      hovertemplate="<b>%{y}</b> · %{x}<br>%{z} partners<extra></extra>") %>%
      layout(xaxis=list(title="Risk Level",categoryorder="array",
               categoryarray=c("Low","Moderate","Significant","High")),
             yaxis=list(title=""),paper_bgcolor="white",
             font=list(family="Plus Jakarta Sans")) %>% pc()
  })

  output$high_risk_table <- renderDT({
    d <- fd() %>% filter(Risk=="High") %>% arrange(desc(C2024)) %>%
      transmute(Partner=Name, Country, Region, Area, `2024 (M)`=round(C2024/1e6,2)) %>%
      slice_head(n=15)
    datatable(d,rownames=FALSE,options=list(dom="t",pageLength=15),class="stripe compact hover") %>%
      formatStyle("2024 (M)",fontWeight="bold",color=P$dark)
  })

  # Main table
  output$main_table <- renderDT({
    d <- fd() %>%
      transmute(Name, Type, Country, Region, Area,
                `2023 (USD)`=formatC(C2023,format="f",digits=2,big.mark=","),
                `2024 (USD)`=formatC(C2024,format="f",digits=2,big.mark=","),
                `YoY %`=ifelse(is.na(YoY),"—",paste0(formatC(YoY,format="f",digits=1),"%")),
                Risk)
    datatable(d,rownames=FALSE,extensions="Buttons",
      options=list(dom="Blfrtip",buttons=c("csv","excel"),pageLength=15,scrollX=TRUE),
      class="stripe compact hover") %>%
      formatStyle("Risk",
        backgroundColor=styleEqual(c("Low","Moderate","Significant","High"),
          c(P$pale,P$bright,P$warn,"#EF9A9A")),
        fontWeight="700",borderRadius="4px")
  })

  # Insights
  output$insight_cards <- renderUI({
    d <- fd()
    tot   <- sum(d$C2024,na.rm=TRUE)
    top5  <- d %>% arrange(desc(C2024)) %>% slice_head(n=5)
    p5    <- round(sum(top5$C2024,na.rm=TRUE)/tot*100,1)
    ph    <- round(mean(d$Risk=="High",na.rm=TRUE)*100,1)
    treg  <- d %>% group_by(Region) %>% summarise(T=sum(C2024,na.rm=TRUE),.groups="drop") %>% slice_max(T,n=1)
    tarea <- d %>% group_by(Area)   %>% summarise(T=sum(C2024,na.rm=TRUE),.groups="drop") %>% slice_max(T,n=1)
    med   <- median(d$C2024,na.rm=TRUE)
    tagList(
      div(class="ins",
        div(class="ins-t",span(class="ins-num","1"),paste0("Top 5 Partners Hold ",p5,"% of Total Funding")),
        div(class="ins-b","The five largest implementing partners collectively received ",tags$b(fmb(sum(top5$C2024,na.rm=TRUE))),
          " out of a total of ",tags$b(fmb(tot)),". This extreme concentration signals reliance on a small number of large-scale partners and warrants diversification review.")),
      div(class="ins c2",
        div(class="ins-t",span(class="ins-num","2"),paste0(recode(treg$Region,!!!region_labels,.default=treg$Region)," Leads Regional Funding")),
        div(class="ins-b","Region ",tags$b(treg$Region)," received ",tags$b(fmb(treg$T)),
          " — more than any other region. This reflects both UNICEF's programmatic priorities and the scale of humanitarian need in crisis-affected geographies.")),
      div(class="ins c3",
        div(class="ins-t",span(class="ins-num","3"),paste0(ph,"% of Partners Are Rated High Risk")),
        div(class="ins-b","Under UNICEF's HACT framework, high-risk partners require more frequent spot checks and assurance activities. ",
          "High-risk contexts often coincide with emergency operations where partner vetting is most challenging.")),
      div(class="ins c4",
        div(class="ins-t",span(class="ins-num","4"),paste0(tarea$Area," is the Top-Funded Programme Area")),
        div(class="ins-b",tags$b(tarea$Area)," received ",tags$b(fmb(tarea$T)),
          " in 2024. This reflects donor earmarking patterns and UNICEF's strategic priorities across multiple regions.")),
      div(class="ins c5",
        div(class="ins-t",span(class="ins-num","5"),paste0("Median Transfer of ",fmm(med)," Masks Wide Disparity")),
        div(class="ins-b","The median cash transfer is ",tags$b(fmm(med)),
          " but the distribution is highly right-skewed. A few large partners receive tens of millions while the majority operate on small grants — typical of UN partner portfolios spanning both community NGOs and government ministries."))
    )
  })

  output$stats_tbl <- renderTable({
    d <- fd()
    data.frame(
      Metric=c("Total Partners","Countries","Total 2024","Mean Transfer",
               "Median Transfer","Max Transfer","Std Deviation","High Risk %"),
      Value=c(comma(nrow(d)), n_distinct(d$Country[!is.na(d$Country)]),
              fmb(sum(d$C2024,na.rm=TRUE)), fmm(mean(d$C2024,na.rm=TRUE)),
              fmm(median(d$C2024,na.rm=TRUE)), fmm(max(d$C2024,na.rm=TRUE)),
              fmm(sd(d$C2024,na.rm=TRUE)),
              paste0(round(mean(d$Risk=="High",na.rm=TRUE)*100,1),"%"))
    )
  }, striped=TRUE,hover=TRUE,spacing="xs",align="lr",colnames=FALSE,width="100%")

  output$lorenz <- renderPlotly({
    d <- fd() %>% filter(C2024>0) %>% arrange(C2024)
    req(nrow(d)>0)
    n <- nrow(d)
    lz <- data.frame(x=c(0,seq_len(n)/n), y=c(0,cumsum(d$C2024)/sum(d$C2024)))
    plot_ly() %>%
      add_lines(data=lz,x=~x,y=~y,line=list(color=P$mid,width=2.5),name="Lorenz Curve") %>%
      add_lines(x=c(0,1),y=c(0,1),line=list(color=P$gray,dash="dot",width=1.5),name="Equality") %>%
      layout(xaxis=list(title="Cumulative Share of Partners",tickformat=".0%",gridcolor="#DDEEFF"),
             yaxis=list(title="Cumulative Share of Transfers",tickformat=".0%",gridcolor="#DDEEFF"),
             paper_bgcolor="white",plot_bgcolor=P$lgray,showlegend=FALSE,
             font=list(family="Plus Jakarta Sans",size=10)) %>% pc()
  })
}


# ── 7. Run App ──────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
