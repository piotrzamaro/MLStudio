# app.R ---------------------------------------------------------------
# 1. Pakiety ----------------------------------------------------------
library(shiny)
library(dplyr)
library(shinyWidgets)
library(partykit)
library(survival)
library(bslib)
library(shinyjs)
library(knitr)
library(RColorBrewer)
library(rpart)
library(ggplot2)
library(pROC)
library(grid)
library(tidyr)
library(officer)
library(flextable)

# (NOWE) Modele zespołowe
library(randomForest)
library(xgboost)
library(httr)

# 2. Ustawienia globalne ----------------------------------------------
options(shiny.maxRequestSize = 500 * 1024^2)
options(scipen = 999)

calc_skewness <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m3 <- sum((x - mean(x))^3) / n
  s3 <- (sqrt(var(x)))^3
  if (is.na(s3) || s3 == 0) return(NA_real_)
  m3 / s3
}

# Motyw aplikacji -----------------------------------------------------
app_theme <- bs_theme(
  version = 5,
  primary = "#004A7F",
  secondary = "#00A3C4",
  success = "#2E7D32",
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat")
)

# 3. UI ---------------------------------------------------------------
ui <- fluidPage(
  theme = app_theme,
  shinyjs::useShinyjs(),
  
  tags$head(
    tags$link(rel = "icon", type = "image/PNG", href = "logo_ml.PNG"),
    tags$style(HTML("
      body { background-color: #f8f9fa; }

      /* Landing Page */
      #landing_page {
        display: flex; flex-direction: column; justify-content: center; align-items: center;
        min-height: 100vh; background: radial-gradient(1100px circle at 20% 10%, rgba(0,163,196,0.12), transparent 55%),
                            radial-gradient(900px circle at 85% 25%, rgba(0,74,127,0.14), transparent 55%),
                            linear-gradient(135deg, #f5f7fb 0%, #e9eef7 100%);
        text-align: left; padding: 26px;
      }
      .landing-wrap { width: 100%; max-width: 1120px; }
      .landing-top {
        background: #ffffff;
        border-radius: 22px;
        box-shadow: 0 18px 45px rgba(0,74,127, 0.14);
        border: 1px solid rgba(0,74,127, 0.08);
        padding: 34px;
        margin-bottom: 18px;
      }
      .landing-badge {
        display:inline-flex; align-items:center; gap:10px;
        background: rgba(0,74,127,0.06);
        border: 1px solid rgba(0,74,127,0.10);
        color:#004A7F;
        padding: 8px 12px;
        border-radius: 999px;
        font-weight: 900;
        letter-spacing: 0.6px;
        text-transform: uppercase;
        font-size: 11px;
        margin-bottom: 14px;
      }
      .landing-title { font-size: 38px; font-weight: 900; color: #004A7F; margin: 0 0 10px 0; line-height:1.15; }
      .landing-subtitle { margin: 0 0 16px 0; color:#00A3C4; font-weight: 700; letter-spacing: 1px; }
      .landing-desc { font-size: 15px; color: #555; margin-bottom: 16px; line-height: 1.7; max-width: 980px; }
      .landing-kpis { display:flex; gap:12px; flex-wrap:wrap; margin-top: 12px; }
      .landing-kpi {
        background: #ffffff;
        border: 1px solid #eef1f5;
        border-radius: 16px;
        padding: 14px 16px;
        min-width: 220px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.03);
      }
      .landing-kpi .kpi-h { font-weight: 900; color:#004A7F; margin:0; font-size: 13px; text-transform:uppercase; letter-spacing:1px; }
      .landing-kpi .kpi-p { margin:6px 0 0 0; color:#6c757d; font-size: 13px; line-height:1.5; }

      .landing-grid { display:grid; grid-template-columns: 1fr 1fr 1fr; gap: 14px; }
      @media (max-width: 992px) { .landing-grid { grid-template-columns: 1fr; } }

      .landing-card {
        background: #fff;
        border: 1px solid #eef1f5;
        border-radius: 18px;
        padding: 18px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.03);
        min-height: 165px;
      }
      .landing-card h3 {
        margin: 0 0 10px 0;
        color:#004A7F;
        font-weight: 900;
        font-size: 16px;
      }
      .landing-card p, .landing-card li { color:#6c757d; font-size: 13px; line-height: 1.6; }
      .landing-card ul { padding-left: 18px; margin: 0; }

      .landing-cta {
        display:flex; align-items:center; justify-content:space-between; gap:14px; flex-wrap:wrap;
        background: linear-gradient(135deg, rgba(0,74,127,0.10), rgba(0,163,196,0.08));
        border: 1px solid rgba(0,74,127,0.12);
        border-radius: 18px;
        padding: 16px 18px;
        margin-top: 14px;
      }
      .landing-cta-left { display:flex; align-items:center; gap:14px; }
      .landing-cta-note { color:#6c757d; font-size: 13px; margin:0; line-height:1.5; }
      .landing-cta-note b { color:#004A7F; }

      .btn-start-app {
        padding: 12px 44px; font-size: 16px; font-weight: 900; border-radius: 999px;
        background-color: #004A7F; border: none; transition: all 0.25s;
        box-shadow: 0 6px 18px rgba(0,74,127, 0.28); color: white;
      }
      .btn-start-app:hover { background-color: #003359; transform: translateY(-2px); }

      /* Containers / Sections */
      .section-container {
        background-color: #fff; border: 1px solid #e9ecef; border-radius: 10px;
        padding: 22px; margin-bottom: 18px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.03);
      }
      .section-header {
        border-bottom: 2px solid #004A7F; padding-bottom: 10px; margin-bottom: 14px;
        color: #004A7F; font-weight: 800; font-size: 1.25rem;
        display: flex; align-items: center; justify-content: space-between;
      }
      .text-muted-small { font-size: 0.92em; color: #6c757d; line-height: 1.55; }

      /* Input boxes */
      .input-box {
        background-color: #fdfdfe; border: 1px solid #dce4ec; border-left: 5px solid #004A7F;
        border-radius: 10px; padding: 16px; height: 100%;
      }
      .input-box h4 { margin-top: 0; font-size: 16px; font-weight: 900; color: #333; margin-bottom: 12px; }
      .input-box h5 { font-size: 14px; font-weight: 800; color: #004A7F; margin-bottom: 8px; margin-top: 14px;}
      .input-box p { font-size: 12px; color: #666; margin-bottom: 10px; }

      /* Data KPIs */
      .kpi-row { margin-bottom: 10px; }
      .kpi-box {
        text-align: center; padding: 16px; background: white; border-radius: 10px;
        border: 1px solid #eef1f5; box-shadow: 0 2px 8px rgba(0,0,0,0.03); height: 100%;
      }
      .kpi-val { font-size: 24px; font-weight: 900; color: #004A7F; }
      .kpi-lbl { font-size: 12px; text-transform: uppercase; color: #777; letter-spacing: 1px; }

      /* Chart containers */
      .chart-container { min-height: 340px; border: 1px solid #f0f0f0; border-radius: 10px; padding: 12px; background: white; }

      /* KPI Summary Bars (Model) */
      .kpi-summary { margin-bottom: 18px; }
      .kpi-subtitle { margin: 0 0 14px 0; color: #6c757d; font-size: 14px; line-height: 1.55; }
      .kpi-row-bar { display: grid; grid-template-columns: 120px 1fr 80px; align-items: center; gap: 12px; margin: 10px 0; }
      .kpi-metric-name { font-size: 12px; font-weight: 900; text-transform: uppercase; letter-spacing: 1px; color: #333; }
      .kpi-bar-bg { height: 10px; background: #eef1f5; border-radius: 999px; overflow: hidden; }
      .kpi-bar-fill { height: 10px; background: #004A7F; border-radius: 999px; width: 0%; }
      .kpi-value { text-align: right; font-weight: 900; color: #004A7F; font-size: 12px; }

      /* Czytelność opisów inputów */
      .algo-help { font-size: 11px; color: #6c757d; line-height: 1.35; margin: 4px 0 8px 0; }
      .algo-label { font-weight: 900; color: #004A7F; margin-top: 10px; display:block; }

      /* NAVBAR (Bootstrap 5) */
      .navbar {
        border-radius: 0 !important;
        background-color: #004A7F !important;
        border: 0 !important;
        box-shadow: 0 4px 18px rgba(0,0,0,0.15);
        padding: 0 !important;
        min-height: 80px !important;
        display: flex;
        align-items: center;
      }
      .navbar .container-fluid {
        padding-left: 25px;
        padding-right: 25px;
      }
      .navbar-brand {
        display: flex !important;
        align-items: center;
        gap: 15px;
        font-size: 22px !important;
        font-weight: 900 !important;
        color: #ffffff !important;
        padding-top: 10px !important;
        padding-bottom: 10px !important;
        margin-right: 30px !important;
        letter-spacing: 0.5px;
      }
      .navbar-nav {
        align-items: center;
        height: 60px;
      }
      .navbar-nav .nav-link {
        color: #ffffff !important;
        font-size: 15px !important;
        font-weight: 700 !important;
        letter-spacing: 0.3px;
        opacity: 0.85;
        padding: 0 18px !important;
        margin: 0 5px;
        height: 40px;
        display: flex;
        align-items: center;
        border-radius: 50px;
        transition: all 0.2s ease-in-out;
      }
      .navbar-nav .nav-link:hover {
        opacity: 1;
        background-color: rgba(255,255,255,0.15) !important;
        transform: translateY(-1px);
      }
      .navbar-nav .nav-link.active,
      .navbar-nav .show > .nav-link {
        opacity: 1;
        background-color: #ffffff !important;
        color: #004A7F !important;
        box-shadow: 0 2px 8px rgba(0,0,0,0.1);
      }

      .cv-result-box { background: #f8f9fa; border: 1px solid #ddd; padding: 12px; border-radius: 10px; margin-bottom: 15px; }

      /* Inline checkbox group */
      .inline-checks {
        display:flex;
        flex-wrap:wrap;
        gap:16px;
        align-items:center;
        margin-top: 6px;
      }
      .inline-checks .checkbox { margin: 0; }

      /* AI Gemini chat */
      .ai-chat-panel {
        background: #ffffff;
        border: 1px solid #eef1f5;
        border-radius: 14px;
        padding: 16px;
        box-shadow: 0 6px 18px rgba(0,74,127,0.08);
      }
      .ai-chat-history {
        max-height: 520px;
        overflow-y: auto;
        margin-bottom: 14px;
        padding-right: 6px;
      }
      .ai-chat-message {
        display: grid;
        grid-template-columns: 44px 1fr;
        gap: 12px;
        padding: 12px;
        border: 1px solid #eef1f5;
        border-radius: 12px;
        background: #f8fbff;
        margin-bottom: 10px;
      }
      .ai-chat-message.model {
        background: linear-gradient(135deg, rgba(0,74,127,0.05), rgba(0,163,196,0.04));
        border-color: rgba(0,74,127,0.15);
      }
      .ai-chat-avatar {
        height: 44px;
        width: 44px;
        border-radius: 12px;
        display: flex;
        align-items: center;
        justify-content: center;
        background: #004A7F;
        color: #fff;
        font-weight: 900;
        box-shadow: 0 6px 14px rgba(0,74,127,0.16);
      }
      .ai-chat-avatar.model { background: #00A3C4; }
      .ai-chat-meta { font-size: 11px; color: #6c757d; letter-spacing: 0.2px; text-transform: uppercase; }
      .ai-chat-body { color: #2f3b52; font-size: 13px; line-height: 1.6; white-space: pre-wrap; }
      .ai-chat-actions { display: flex; gap: 10px; flex-wrap: wrap; }
    "))
  ),
  
  # --- 1. Landing Page (DUŻE LOGO NA STRONIE TYTUŁOWEJ) ---
  div(
    id = "landing_page",
    div(
      class = "landing-wrap",
      
      div(
        class = "landing-top",
        
        # DUŻE logo.PNG
        div(
          style = "display:flex; align-items:center; justify-content:center; margin-bottom: 18px;",
          tags$img(src = "logo.PNG", style = "height:120px; width:auto;")
        ),
        
        div(
          class = "landing-badge",
          tags$img(src = "logo.PNG", style = "height:80px; width:auto;"),
          span("ML Studio • Healthcare Decision Classifier")
        ),
        h1("Klasyfikator decyzyjny ML", class = "landing-title"),
        div("Uczenie nadzorowane + eksploracja danych + raportowanie wyników w jednym miejscu", class = "landing-subtitle"),
        p(
          "Aplikacja prowadzi przez pełny workflow: od przygotowania danych i definicji targetu, przez eksplorację (PCA, k-średnich), aż po budowę modeli (drzewo, Random Forest, XGBoost), ocenę jakości i interpretację predyktorów.",
          class = "landing-desc"
        ),
        
        div(
          class = "landing-kpis",
          div(class="landing-kpi",
              tags$p("Szybki start", class="kpi-h"),
              tags$p("Wczytaj dane, wybierz target i predyktory – reszta jest prowadzona krok po kroku.", class="kpi-p")
          ),
          div(class="landing-kpi",
              tags$p("Interpretowalność", class="kpi-h"),
              tags$p("Macierz konfuzji, ważność predyktorów, oraz wizualizacja drzewa (lub surrogate).", class="kpi-p")
          ),
          div(class="landing-kpi",
              tags$p("Raportowanie", class="kpi-h"),
              tags$p("Zebrane parametry, metryki i wykresy w jednej sekcji raportu.", class="kpi-p")
          )
        ),
        
        div(
          class = "landing-cta",
          div(
            class = "landing-cta-left",
            tags$div(
              tags$p(class="landing-cta-note",
                     HTML("<b>Rekomendacja:</b> zacznij od zakładki <b>1. Dane</b> i uzupełnij <b>Etykiety klas</b> — będą używane w wykresach."))
            )
          ),
          actionButton("start_app", "Rozpocznij analizę", class = "btn-start-app")
        )
      ),
      
      div(
        class = "landing-grid",
        div(
          class = "landing-card",
          h3("Co jest analizowane?"),
          tags$ul(
            tags$li("Zmienne wejściowe (predyktory) wskazane przez użytkownika"),
            tags$li("Klasa wynikowa z możliwością nadania czytelnych etykiet"),
            tags$li("Relacje zmiennych z targetem już na etapie przygotowania danych")
          )
        ),
        div(
          class = "landing-card",
          h3("Jakie metody są dostępne?"),
          tags$ul(
            tags$li("PCA: redukcja wymiarowości i interpretacja zmiennych"),
            tags$li("K-średnich: segmentacja obserwacji bez targetu"),
            tags$li("Modele: drzewo / Random Forest / XGBoost (binary)")
          )
        ),
        div(
          class = "landing-card",
          h3("Co otrzymasz na końcu?"),
          tags$ul(
            tags$li("Metryki jakości (Accuracy, Precision, Recall, F1)"),
            tags$li("Macierz konfuzji + ROC (dla 2 klas)"),
            tags$li("Ważność predyktorów + wizualizacja drzewa / surrogate"),
            tags$li("Sekcja Raport z opisem i wizualizacjami")
          )
        )
      ),
      
      div(
        style = "text-align:center; margin-top: 14px; color:#94a3b8; font-size:12px;",
        "© 2025 AOTMiT. Wszelkie prawa zastrzeżone."
      )
    )
  ),
  
  # --- 2. Main App ---
  hidden(
    div(
      id = "main_app",
      navbarPage(
        id = "main_tabs",
        title = div(
          tags$img(src = "logo.PNG", style = "height: 80px; width: auto;"),
          span("Healthcare Decission Classifier", style="margin-left: 10px;")
        ),
        theme = app_theme,
        
        # --- START TAB (LOGO_ML.PNG PO PRAWEJ) ---
        tabPanel(
          "Start",
          value = "start",
          div(
            style = "padding: 26px;",
            div(
              class = "landing-wrap",
              div(
                class = "landing-top",
                
                fluidRow(
                  column(
                    8,
                    div(
                      class = "landing-badge",
                      tags$img(src = "logo.PNG", style = "height:22px; width:auto;"),
                      span("ML Studio • Healthcare Decision Classifier")
                    ),
                    h1("Środowisko analityczne do klasyfikacji i interpretacji decyzji", class = "landing-title"),
                    div("Uczenie nadzorowane + eksploracja danych + raportowanie wyników w jednym miejscu", class = "landing-subtitle"),
                    p(
                      "Aplikacja prowadzi przez pełny workflow: od przygotowania danych i definicji targetu, przez eksplorację (PCA, k-średnich), aż po budowę modeli (drzewo, Random Forest, XGBoost), ocenę jakości i interpretację predyktorów.",
                      class = "landing-desc"
                    )
                  ),
                  column(
                    4,
                    div(
                      style = "display:flex; justify-content:flex-end; align-items:flex-start;",
                      tags$img(src = "logo_ml.PNG", style = "max-width: 100%; height: 140px; width:auto;")
                    )
                  )
                ),
                
                div(
                  class = "landing-kpis",
                  div(class="landing-kpi",
                      tags$p("Szybki start", class="kpi-h"),
                      tags$p("Wczytaj dane, wybierz target i predyktory – reszta jest prowadzona krok po kroku.", class="kpi-p")
                  ),
                  div(class="landing-kpi",
                      tags$p("Interpretowalność", class="kpi-h"),
                      tags$p("Macierz konfuzji, ROC (dla 2 klas) i ważność predyktorów.", class="kpi-p")
                  ),
                  div(class="landing-kpi",
                      tags$p("Raportowanie", class="kpi-h"),
                      tags$p("Zebrane parametry, metryki i wykresy w sekcji Raport.", class="kpi-p")
                  )
                ),
                div(
                  class = "landing-cta",
                  div(
                    class = "landing-cta-left",
                    tags$div(
                      tags$p(class="landing-cta-note",
                             HTML("<b>Rekomendacja:</b> zacznij od zakładki <b>1. Dane</b> i uzupełnij <b>Etykiety klas</b> — będą używane w wykresach."))
                    )
                  ),
                  actionButton("start_app_tab", "Przejdź do 1. Dane", class = "btn-start-app")
                )
              ),
              div(
                style = "text-align:center; margin-top: 14px; color:#94a3b8; font-size:12px;",
                "© 2025 AOTMiT. Wszelkie prawa zastrzeżone."
              )
            )
          )
        ),
        
        tabPanel(
          "Wprowadzenie",
          div(class = "container-fluid",
              br(),
              
              # =================== WPROWADZENIE ===================
              div(class = "section-container",
                  div(class = "section-header", "Wprowadzenie"),
                  div(class = "text-muted-small",
                      p(
                        "ML Studio – Healthcare Decision Classifier jest interaktywnym środowiskiem analitycznym 
                 wspierającym proces analizy danych obserwacyjnych w ochronie zdrowia. 
                 Aplikacja umożliwia przejście przez pełny, uporządkowany workflow analityczny: 
                 od przygotowania danych, przez eksplorację i modelowanie, aż po ocenę jakości 
                 oraz interpretację wyników."
                      ),
                      p(
                        "Narzędzie zostało zaprojektowane z myślą o analizach wspierających decyzje 
                 kliniczne, organizacyjne i zdrowia publicznego, w szczególności tam, 
                 gdzie kluczowe znaczenie ma interpretowalność modeli oraz przejrzystość procesu analizy."
                      )
                  )
              ),
              
              # =================== UCZENIE MASZYNOWE ===================
              div(class = "section-container",
                  div(class = "section-header", "Czym jest uczenie maszynowe?"),
                  div(class = "text-muted-small",
                      
                      p(
                        "Uczenie maszynowe (machine learning, ML) jest dziedziną analizy danych, 
           której celem jest budowa modeli zdolnych do identyfikowania wzorców 
           oraz zależności w danych na podstawie obserwacji empirycznych, 
           bez konieczności jawnego programowania reguł decyzyjnych. 
           
           W przeciwieństwie do klasycznych metod statystycznych, w których 
           struktura modelu jest z góry narzucona, algorytmy uczenia maszynowego 
           uczą się relacji pomiędzy zmiennymi bezpośrednio z danych. 
           W analizach zdrowotnych ML pełni rolę narzędzia wspierającego 
           wnioskowanie, eksplorację danych oraz predykcję wyników.
           
           W zależności od dostępności zmiennej wynikowej (targetu) oraz celu analizy, 
           metody uczenia maszynowego dzieli się przede wszystkim na 
           uczenie nadzorowane oraz uczenie nienadzorowane."
                      ),
                      
                      # -------- UCZENIE NADZOROWANE --------
                      p(tags$b("Uczenie nadzorowane "),
                        
                        "polega na budowie modelu na podstawie danych, 
           dla których znana jest zmienna wynikowa (target), 
           np. zgon/przeżycie, wystąpienie zdarzenia klinicznego 
           lub przynależność do określonej grupy.
           
           Celem uczenia nadzorowanego jest predykcja wartości zmiennej wynikowej 
           dla nowych, wcześniej nieobserwowanych przypadków. 
           Modele te umożliwiają również identyfikację czynników 
           najsilniej związanych z analizowanym wynikiem.
           
           W aplikacji ML Studio uczenie nadzorowane wykorzystywane jest 
           do budowy modeli klasyfikacyjnych, takich jak drzewo decyzyjne 
           oraz modele zespołowe, których wyniki oceniane są za pomocą 
           miar jakości predykcji (np. accuracy, precision, recall, AUC)."
                      ),
                      
                      # -------- UCZENIE NIENADZOROWANE --------
                      p(tags$b("Uczenie nienadzorowane "),
                        
                        "to podejście, w którym algorytm analizuje dane 
           bez wykorzystania zmiennej wynikowej. 
           Celem nie jest predykcja konkretnego wyniku, 
           lecz identyfikacja struktury, podobieństw oraz wzorców 
           występujących w zbiorze danych.
           
           W analizach zdrowotnych oraz HTA metody nienadzorowane pełnią 
           przede wszystkim funkcję eksploracyjną. 
           Umożliwiają wstępne rozpoznanie heterogeniczności populacji pacjentów, 
           ocenę zależności pomiędzy zmiennymi oraz identyfikację 
           potencjalnych segmentów populacji."
                      ),
                      
                      # -------- PCA --------
                      p(tags$b("Analiza głównych składowych (PCA)")),
                      
                      p(
                        "Analiza głównych składowych (Principal Component Analysis, PCA) 
           jest metodą redukcji wymiarowości, 
           której celem jest przekształcenie oryginalnych, 
           często silnie skorelowanych zmiennych 
           w nowy zbiór nieskorelowanych składowych głównych."
                      ),
                      
                      tags$ul(
                        tags$li(
                          "Pierwsze składowe wyjaśniają największą część całkowitej wariancji danych."
                        ),
                        tags$li(
                          "Każda składowa jest liniową kombinacją zmiennych wejściowych."
                        ),
                        tags$li(
                          "Wysokie ładunki (loadings) wskazują zmienne 
             najsilniej różnicujące obserwacje."
                        )
                      ),
                      
                      p(
                        "PCA jest wykorzystywana do uproszczenia struktury danych, 
           wizualizacji wielowymiarowych zależności (np. biplot) 
           oraz identyfikacji zmiennych dostarczających 
           zbliżonej informacji analitycznej."
                      ),
                      
                      
                      # -------- K-MEANS --------
                      p(tags$b("Grupowanie k-średnich (k-means)")),
                      
                      p(
                        "Grupowanie k-średnich (k-means clustering) 
           jest algorytmem segmentacji, 
           którego celem jest podział obserwacji 
           na z góry określoną liczbę grup (k) 
           w taki sposób, aby obserwacje w obrębie grup 
           były do siebie jak najbardziej podobne."
                      ),
                      
                      tags$ul(
                        tags$li(
                          "Algorytm minimalizuje sumę kwadratów odległości 
             obserwacji od centroidów grup."
                        ),
                        tags$li(
                          "Do analizy wykorzystywane są wyłącznie zmienne numeryczne."
                        ),
                        tags$li(
                          "Każda obserwacja zostaje przypisana do dokładnie jednej grupy."
                        )
                      ),
                      
                      p(
                        "Wyniki grupowania k-średnich umożliwiają 
           eksploracyjną identyfikację potencjalnych segmentów populacji, 
           które mogą różnić się profilem klinicznym lub organizacyjnym. 
           Uzyskane grupy nie stanowią jednak formalnej klasyfikacji klinicznej 
           i powinny być interpretowane pomocniczo."
                      )
                  )
              ),
              
              
              # =================== METODYKA ===================
              div(class = "section-container",
                  div(class = "section-header", "Metodyka analizy"),
                  div(class = "text-muted-small",
                      p(
                        "Analiza prowadzona w aplikacji opiera się na klasycznym schemacie 
                 data science stosowanym w badaniach obserwacyjnych i analizach HTA:"
                      ),
                      tags$ul(
                        tags$li(
                          tags$b("Przygotowanie danych: "),
                          "selekcja zmiennych, konwersja typów, usuwanie braków danych (na.omit)."
                        ),
                        tags$li(
                          tags$b("Eksploracja (EDA): "),
                          "analiza rozkładów, PCA (redukcja wymiarowości), 
                   k-średnich (segmentacja bez nadzoru)."
                        ),
                        tags$li(
                          tags$b("Modelowanie (uczenie nadzorowane): "),
                          "drzewo decyzyjne, Random Forest lub XGBoost (klasyfikacja binarna)."
                        ),
                        tags$li(
                          tags$b("Walidacja: "),
                          "podział train/test oraz opcjonalna walidacja krzyżowa K-Fold (dla drzewa)."
                        )
                      ),
                      p(
                        "Dobór metod został świadomie ograniczony do algorytmów umożliwiających 
                 interpretację wyników, co jest szczególnie istotne w analizach zdrowotnych."
                      )
                  )
              ),
              
              
              
              
              # =================== OCENA JAKOŚCI ===================
              div(class = "section-container",
                  div(class = "section-header", "Ocena jakości modelu"),
                  div(class = "text-muted-small",
                      p(
                        "Jakość modelu oceniana jest na zbiorze testowym, 
                 niewykorzystanym w procesie uczenia."
                      ),
                      tags$ul(
                        tags$li(tags$b("Accuracy:"), " ogólna poprawność klasyfikacji."),
                        tags$li(tags$b("Precision:"), " wiarygodność predykcji klasy."),
                        tags$li(tags$b("Recall:"), " zdolność wykrywania obserwacji danej klasy."),
                        tags$li(tags$b("F1 Score:"), " kompromis pomiędzy precision i recall.")
                      ),
                      p(
                        "Dodatkowo prezentowana jest macierz konfuzji oraz krzywa ROC wraz z AUC 
                 (dla klasyfikacji binarnej), co umożliwia ocenę zachowania modelu 
                 przy różnych progach decyzyjnych."
                      )
                  )
              ),
              
              # =================== INTERPRETACJA ===================
              div(class = "section-container",
                  div(class = "section-header", "Interpretacja wyników"),
                  div(class = "text-muted-small",
                      p(
                        "Interpretacja modelu stanowi kluczowy element aplikacji i obejmuje:"
                      ),
                      tags$ul(
                        tags$li(
                          tags$b("Ważność predyktorów: "),
                          "identyfikację zmiennych najsilniej wpływających na decyzję modelu."
                        ),
                        tags$li(
                          tags$b("Strukturę drzewa decyzyjnego: "),
                          "bezpośrednią interpretację reguł decyzyjnych 
                   (lub drzewa zastępczego dla modeli zespołowych)."
                        )
                      ),
                      p(
                        "Należy podkreślić, że prezentowane zależności mają charakter predykcyjny, 
                 a nie przyczynowy. Interpretacja wyników powinna być zawsze zestawiona 
                 z wiedzą dziedzinową oraz kontekstem klinicznym lub organizacyjnym."
                      )
                  )
              ),
              
              # =================== INSTRUKCJA ===================
              div(class = "section-container",
                  div(class = "section-header", "Instrukcja korzystania z aplikacji"),
                  div(class = "text-muted-small",
                      tags$ol(
                        tags$li(
                          tags$b("Wczytanie danych: "),
                          "W zakładce „1. Dane” załaduj plik CSV oraz określ separator i obecność nagłówka."
                        ),
                        tags$li(
                          tags$b("Definicja targetu: "),
                          "Wybierz zmienną wynikową (klasę decyzyjną). 
                   Opcjonalnie nadaj klasom czytelne etykiety wykorzystywane w wizualizacjach."
                        ),
                        tags$li(
                          tags$b("Wybór predyktorów: "),
                          "Zaznacz zmienne objaśniające, które mają być użyte w analizie."
                        ),
                        tags$li(
                          tags$b("Eksploracja danych: "),
                          "Skorzystaj z PCA oraz k-średnich w zakładce „2. Eksploracja danych”, 
                   aby zrozumieć strukturę danych i potencjalne segmenty obserwacji."
                        ),
                        tags$li(
                          tags$b("Budowa modelu: "),
                          "W zakładce „3. Klasyfikator decyzyjny” wybierz algorytm i parametry, 
                   a następnie zbuduj model."
                        ),
                        tags$li(
                          tags$b("Ocena i interpretacja: "),
                          "Przeanalizuj metryki jakości, macierz konfuzji, ROC oraz ważność predyktorów."
                        ),
                        tags$li(
                          tags$b("Raport: "),
                          "W zakładce „5. Raport” zapoznaj się z całościowym podsumowaniem analizy 
                   lub pobierz raport."
                        )
                      )
                  )
              )
              
          )
        ),
        
        # --- 1. Dane i Zmienne ---
        tabPanel(
          "Dane",
          div(class = "container-fluid",
              br(),
              
              div(class = "section-container",
                  
                  div(class = "text-muted-small",
                      tags$h4("Instrukcja",
                              style="color:#004A7F; font-weight:900; margin:0 0 10px 0;"),
                      p("Uczenie maszynowe w ochronie zdrowia pełni rolę narzędzia wspierającego wnioskowanie na podstawie danych obserwacyjnych. Modele klasyfikacyjne uczą się zależności pomiędzy cechami pacjentów a wynikiem klinicznym lub organizacyjnym, dzięki czemu mogą porządkować ryzyko, wskazywać segmenty populacji i ułatwiać interpretację czynników różnicujących."),
                      p("W niniejszej aplikacji podstawowym modelem jest drzewo decyzyjne – struktura reguł, która dzieli populację na podgrupy o odmiennym prawdopodobieństwie przynależności do klas targetu. Poprawność klasyfikacji oceniana jest na zbiorze testowym, a opcjonalnie także w walidacji krzyżowej.")
                  ),
                  
                  hr(style="margin: 14px 0; border-top: 1px solid #eef1f5;"),
                  div(class = "section-header", "Przygotowanie danych"),
                  br(),
                  
                  fluidRow(
                    column(
                      4,
                      div(class = "input-box",
                          h4("1) Wczytaj plik"),
                          p("Obsługiwane formaty: .csv, .txt"),
                          fileInput("file", NULL, buttonLabel = "Wybierz plik…", placeholder = "Nie wybrano pliku", accept = c(".csv", "text/csv")),
                          fluidRow(
                            column(6, radioButtons("sep", "Separator:", c("Przecinek (,)" = ",", "Średnik (;)" = ";", "Tabulator" = "\t"), selected = ",")),
                            column(6, checkboxInput("header", "Nagłówek", TRUE))
                          )
                      )
                    ),
                    
                    column(
                      8,
                      div(class = "input-box", style = "border-left-color: #2E7D32;",
                          h4("2) Konfiguracja zmiennych do modelu"),
                          p("Zdefiniuj zmienną wynikową, wybierz predyktory i nadaj etykiety klas."),
                          fluidRow(
                            column(6,
                                   h5("Klasa"),
                                   uiOutput("var_target_ui"),
                                   br(),
                                   h5("Etykiety klas"),
                                   p("Możesz zmienić nazwy klas na bardziej czytelne.", style="font-size:11px; color:#999; margin-bottom:5px;"),
                                   uiOutput("class_labels_ui")
                            ),
                            column(6,
                                   h5("Predyktory"),
                                   p("Zaznacz zmienne objaśniające (features).", style="font-size:11px; color:#999; margin-bottom:5px;"),
                                   uiOutput("var_predictors_ui")
                            )
                          )
                      )
                    )
                  )
              ),
              
              div(class = "section-container",
                  div(class = "section-header", "Podsumowanie danych"),
                  uiOutput("data_kpi_ui")
              ),
              
              div(class = "section-container",
                  div(class = "section-header", "Analiza relacji zmiennych"),
                  div(class="text-muted-small",
                      p("Ten panel umożliwia szybki podgląd rozkładu targetu oraz relacji dowolnej zmiennej z targetem już na etapie przygotowania danych. Etykiety klas są pobierane z sekcji „Etykiety klas”.")
                  ),
                  
                  fluidRow(
                    column(
                      12,
                      div(style="margin-bottom: 12px; display:flex; align-items:center; gap:12px;",
                          h6("Wybierz zmienną predykcyjną:", style="margin:0; color:#004A7F; font-weight:900;"),
                          div(style="width: 360px;", uiOutput("var_explore_selector_ui"))
                      )
                    )
                  ),
                  hr(),
                  
                  fluidRow(
                    column(4,
                           div(class = "chart-container",
                               h5("Rozkład zmiennej wynikowej", style="color:#004A7F; font-weight:900; margin-bottom:12px;"),
                               plotOutput("target_dist_plot", height = "300px")
                           )
                    ),
                    column(4,
                           div(class = "chart-container",
                               h5("Rozkład zmiennej predykcyjnej", style="color:#004A7F; font-weight:900; margin-bottom:12px;"),
                               plotOutput("variable_dist_plot", height = "300px")
                           )
                    ),
                    column(4,
                           div(class = "chart-container",
                               h5("Relacja zmiennej predykcyjnej i wynikowej", style="color:#004A7F; font-weight:900; margin-bottom:4px;"),
                               p("Udział % klas targetu w grupach / kategoriach.", style="font-size:11px; color:#777; margin-bottom:10px;"),
                               plotOutput("variable_target_plot", height = "280px")
                           )
                    )
                  )
              ),
              
              div(class = "section-container",
                  div(class = "section-header", "Podgląd wprowadzonych danych"),
                  div(style = "overflow-x:auto;", tableOutput("data_head_table"))
              ),
              
              div(class = "section-container",
                  div(class = "section-header", "Statystyki opisowe"),
                  div(style = "overflow-x:auto;", tableOutput("continuous_vars_table"))
              )
          )
        ),
        
        # --- 2. Eksploracja danych (NAPRAWA: OSOBNE OUTPUTY, ŻEBY WYKRESY SIĘ WYŚWIETLAŁY) ---
        tabPanel(
          "Eksploracja",
          div(class = "section-container",
              div(class = "text-muted-small",
                  div(class = "section-header", "Metody uczenia nienadzrowanego"),
                  p("Uczenie nienadzorowane (unsupervised learning) to rodzaj uczenia maszynowego, w którym nie posiadamy etykiety wyniku (targetu). Algorytm szuka struktury w samych cechach: grupuje podobne obserwacje (np. k-średnich) albo redukuje wymiarowość i ujawnia główne kierunki zmienności (np. PCA). W praktyce pomaga wykryć segmenty pacjentów, redundancję informacji oraz zmienne różnicujące populację."),
                  
                  p(strong("Analiza głównych składowych (PCA)")),
                  p("PCA redukuje wymiarowość danych, tworząc nieskorelowane składowe wyjaśniające maksymalną wariancję. Pomaga wykryć zmienne różnicujące obserwacje i ograniczyć redundancję informacji."),
                  p(strong("Grupowanie k-średnich")),
                  p("Analizowane są tylko predyktory numeryczne; zmienne stałe (zerowa wariancja) są automatycznie pomijane.")
                  
                  
              ),
              
              fluidRow(
                column(3,
                       div(class = "input-box",
                           h4("Parametry PCA"),
                           checkboxInput("pca_scale", "Standaryzuj zmienne (zalecane)", TRUE),
                           numericInput("pca_top_n", "Pokaż TOP N zmiennych:", 10, min = 3, max = 30),
                           sliderInput("pca_fontsize", "Rozmiar czcionki (biplot):", min = 10, max = 22, value = 14, step = 1),
                           # --- DODAJ POD pca_fontsize ---
                           sliderInput("pca_pt_size", "Rozmiar punktów:", min = 1, max = 6, value = 2, step = 0.5),
                           sliderInput("pca_pt_alpha", "Przezroczystość punktów:", min = 0.05, max = 1, value = 0.45, step = 0.05),
                           
                           checkboxInput("pca_show_ellipse", "Pokaż elipsy grup", TRUE),
                           sliderInput("pca_ellipse_alpha", "Przezroczystość elips:", min = 0.00, max = 0.60, value = 0.10, step = 0.02),
                           
                           checkboxInput("pca_show_loadings", "Pokaż wektory zmiennych (ładunki)", TRUE),
                           sliderInput("pca_arrow_size", "Grubość strzałek:", min = 0.1, max = 1.5, value = 0.6, step = 0.1),
                           sliderInput("pca_arrow_len", "Długość grotu strzałki (cm):", min = 0.05, max = 0.40, value = 0.20, step = 0.01),
                           
                           sliderInput("pca_label_size", "Rozmiar etykiet zmiennych:", min = 2, max = 8, value = 4, step = 0.5),
                           checkboxInput("pca_check_overlap", "Ukrywaj nachodzące etykiety (check_overlap)", TRUE),
                           hr(),
                           uiOutput("pca_info_ui")
                       )
                ),
                column(9,
                       div(class = "input-box", style = "border-left: none;",
                           h4("Biplot PCA"),
                           plotOutput("pca_scores_plot_expl", height = "520px")
                       )
                )
              ),
              
              fluidRow(
                column(6,
                       div(class="section-container", style="padding:15px;",
                           h5("Wariancja wyjaśniona (scree)"),
                           plotOutput("pca_scree_plot_expl", height="300px"),
                           uiOutput("pca_commentary_ui")
                       )
                ),
                column(6,
                       div(class="section-container", style="padding:15px;",
                           h5("Ranking ważności zmiennych (PCA)"),
                           tableOutput("pca_top_table_expl")
                       )
                )
              )
          )
          
          
          
          
        ),
        
        # --- 3. Klasyfikator decyzyjny (NAPRAWA: MACIERZ + NOWA TABELA WAŻNOŚCI + CV OBOK) ---
        tabPanel(
          "Model decyzyjny",
          div(class = "container-fluid",
              br(),
              
              div(class = "section-container",
                  div(class = "section-header", "Budowa modelu"),
                  div(class = "text-muted-small",
                      p("Klasyfikator (uczenie nadzorowane) uczy się przypisywać obserwacje do klas na podstawie zestawu cech (predyktorów). W tej aplikacji możesz wybrać model: drzewo decyzyjne, random forest lub XGBoost.")
                  ),
                  
                  fluidRow(
                    column(4,
                           div(class = "input-box", style="border-left: 5px solid #004A7F;",
                               
                               tags$label(h5("Model"), class = "control-label algo-label"),
                               div(class = "algo-help", "Wybierz algorytm: drzewo, las losowy lub XGBoost."),
                               selectInput(
                                 "model_type", NULL,
                                 choices = c(
                                   "Drzewo decyzyjne (ctree / rpart)" = "tree",
                                   "Random Forest" = "rf",
                                   "XGBoost (binary)" = "xgb"
                                 ),
                                 selected = "tree"
                               ),
                               
                               conditionalPanel(
                                 "input.model_type == 'tree'",
                                 tags$label(h5("Metoda podziału"), class = "control-label algo-label"),
                                 div(class = "algo-help", "Kryterium wyboru podziału węzła."),
                                 selectInput(
                                   "split_metric", NULL,
                                   choices = c(
                                     "Test statystyczny (ctree)" = "ctree",
                                     "Indeks GINI (rpart)" = "gini",
                                     "Entropia (rpart)" = "information"
                                   ),
                                   selected = "ctree"
                                 )
                               ),
                               
                               conditionalPanel(
                                 "input.model_type == 'rf'",
                                 tags$label(h5("Parametry Random Forest"), class = "control-label algo-label"),
                                 div(class="algo-help", tags$b("Liczba drzew (ntree):"), " im więcej, tym stabilniej, ale wolniej."),
                                 numericInput("rf_ntree", NULL, value = 500, min = 50, step = 50),
                                 div(class="algo-help", tags$b("mtry:"), " liczba zmiennych losowanych do podziału (0 = auto)."),
                                 numericInput("rf_mtry", NULL, value = 0, min = 0, step = 1)
                               ),
                               
                               conditionalPanel(
                                 "input.model_type == 'xgb'",
                                 tags$label(h5("Parametry XGBoost (binary)"), class = "control-label algo-label"),
                                 div(class="algo-help", tags$b("Liczba rund (nrounds):"), " ile iteracji boostingu."),
                                 numericInput("xgb_nrounds", NULL, value = 200, min = 50, step = 25),
                                 div(class="algo-help", tags$b("Eta (learning rate):"), " mniejsze = stabilniej, wolniej."),
                                 numericInput("xgb_eta", NULL, value = 0.05, min = 0.01, max = 0.3, step = 0.01),
                                 div(class="algo-help", tags$b("Max depth:"), " złożoność pojedynczych drzewek w boosting."),
                                 numericInput("xgb_maxdepth", NULL, value = 4, min = 1, step = 1)
                               ),
                               
                               br(),
                               
                               tags$label(h5("Maksymalna głębokość drzewa"), class = "control-label algo-label"),
                               numericInput("maxdepth", NULL, value = 5, min = 1),
                               
                               tags$label(h5("Minimalna liczba obserwacji w węźle"), class = "control-label algo-label"),
                               numericInput("minsplit", NULL, value = 20, min = 2),
                               
                               br(),
                               
                               tags$label(h5("Podział zbioru na dane testowe i treningowe"), class = "control-label algo-label"),
                               div(class = "algo-help", tags$b("Udział danych użytych do treningu modelu")),
                               sliderInput("train_prop", NULL, min = 0.5, max = 0.9, value = 0.7, step = 0.05),
                               
                               br(),
                               checkboxInput("do_cv", "Walidacja krzyżowa (K-Fold)", value = FALSE),
                               
                               conditionalPanel(
                                 "input.do_cv == true",
                                 div(class = "algo-help",
                                     tags$b("Liczba k:"), " ile foldów w cross-validation."
                                 ),
                                 numericInput("cv_folds", NULL, value = 5, min = 2, max = 10)
                               ),
                               
                               br(),
                               
                               div(class = "algo-help",
                                   tags$b("Seed:"), " ziarno losowania; pozwala odtwarzać ten sam podział train/test i wyniki."
                               ),
                               numericInput("seed", NULL, value = 123),
                               
                               hr(),
                               actionButton("run_model", "ZBUDUJ MODEL", class = "btn btn-primary w-100", style = "font-weight: 900;")
                           )
                    ),
                    
                    column(8,
                           div(class = "input-box", style="border-left: none;",
                               h4("Podsumowanie jakości (test)", style="color:#004A7F; font-weight:900;"),
                               uiOutput("kpi_summary_ui"),
                               uiOutput("cv_results_ui_main"),
                               h4("Macierz konfuzji", style="color:#004A7F; font-weight:900;"),
                               plotOutput("cm_heatmap_main", height = "360px")
                           )
                    )
                  )
                  
              ),
              
              div(class = "section-container",
                  div(class = "section-header", "Ocena jakości modelu"),
                  fluidRow(
                    column(
                      4,
                      div(class="chart-container",
                          h5("Krzywa ROC", style="color:#004A7F; font-weight:900; margin-bottom:12px;"),
                          plotOutput("roc_plot_main", height = "280px")
                      )
                    ),
                    column(
                      4,
                      div(class="chart-container",
                          h5("Prawdopodobieństwa", style="color:#004A7F; font-weight:900; margin-bottom:12px;"),
                          plotOutput("prob_plot_main", height = "280px")
                      )
                    ),
                    column(
                      4,
                      div(class="chart-container",
                          h5("Ważność zmiennych", style="color:#004A7F; font-weight:900; margin-bottom:12px;"),
                          plotOutput("var_imp_plot_main", height = "280px")
                      )
                    )
                  ),
                  
                  hr(),
                  
                  # (NOWE) Tabela ważności + CV obok
                  fluidRow(
                    column(
                      7,
                      div(class="section-container", style="padding:15px;",
                          h5("Ważność predyktorów (tabela)", style="color:#004A7F; font-weight:900;"),
                          div(style="overflow-x:auto;", tableOutput("var_imp_table_main"))
                      )
                    ),
                    column(
                      5,
                      div(class="section-container", style="padding:15px;",
                          uiOutput("cv_results_ui_inline")
                      )
                    )
                  )
              )
          )
        ),
        
        # --- Drzewo decyzyjne ---
        tabPanel(
          "Reguły decyzyjne",
          div(class = "container-fluid",
              br(),
              
              div(class = "section-container",
                  div(class = "section-header", "Struktura drzewa decyzyjnego"),
                  
                  div(class = "input-box", style="margin-bottom: 14px;",
                      
                      tags$div(
                        class = "inline-checks",
                        
                        checkboxInput("bar_beside", "Oddzielne słupki (beside)", FALSE),
                        checkboxInput("show_ids", "Pokaż ID węzła i n", TRUE),
                        
                        conditionalPanel(
                          "input.model_type == 'tree' && input.split_metric == 'ctree'",
                          checkboxInput("show_pval", "Pokaż p-value", TRUE)
                        ),
                        
                        div(style="display:flex; align-items:center; gap:10px;",
                            tags$label("Rozmiar czcionki:", style="margin:0; font-weight:700; color:#004A7F;"),
                            div(style="width: 220px;",
                                sliderInput("tree_fontsize", NULL, min = 6, max = 20, value = 12, step = 1)
                            )
                        )
                      )
                  ),
                  
                  plotOutput("tree_plot", height = "680px"),
                  hr(),
                  verbatimTextOutput("model_summary")
              )
          )
        ),
        
        # --- 4. Jakość modelu ---
        tabPanel(
          "Jakość modelu",
          div(class = "container-fluid",
              br(),
              
              div(class = "section-container",
                  div(class = "section-header", "Ocena jakości i dokładności klasyfikacji"),
                  div(class = "text-muted-small",
                      p("Ocena wykonywana jest na zbiorze testowym (niewykorzystanym w uczeniu), aby oszacować generalizację modelu."),
                      p(strong("Metryki: "), "Accuracy – poprawność ogólna; Precision – wiarygodność predykcji; Recall – wykrywalność; F1 – kompromis precision/recall.")
                  )
              ),
              
              fluidRow(
                column(
                  6,
                  div(class = "section-container",
                      div(class = "section-header", "Metryki i macierz pomyłek (test)"),
                      fluidRow(
                        column(6, tableOutput("metrics_table")),
                        column(6, tableOutput("conf_matrix"))
                      )
                  )
                )
              )
          )
        ),
        
        # --- 5. Raport (NAPRAWA: OSOBNE OUTPUTY, ŻEBY NIC NIE ZNIKAŁO) ---
        tabPanel(
          "Raport",
          div(class = "container-fluid",
              br(),
              
              div(class = "section-container",
                  div(class = "section-header", "Raport z analizy: opis + wizualizacje"),
                  div(class="text-muted-small",
                      p("Sekcja raportowa zbiera kluczowe elementy analizy w uporządkowanej strukturze: cel, przygotowanie danych, metody, eksplorację (PCA i k-średnich), budowę modelu oraz wyniki."),
                      p("Wizualizacje i tabele są generowane na podstawie aktualnych ustawień aplikacji.")
                  )
              ),
              
              div(class="section-container",
                  div(class="section-header", "1. Cel analizy"),
                  div(class="text-muted-small",
                      p("Celem analizy jest zbudowanie modelu klasyfikacyjnego, który przypisuje obserwacje do klas targetu na podstawie wybranych predyktorów."),
                      p("Wynikiem jest zarówno ocena jakości predykcji (metryki i macierz konfuzji), jak i interpretacja: jakie cechy najsilniej różnicują klasy.")
                  )
              ),
              
              div(class="section-container",
                  div(class="section-header", "2. Przygotowanie danych"),
                  div(class="text-muted-small",
                      p("Dane są wczytywane z pliku CSV, następnie wybierany jest target oraz predyktory. Zmienne tekstowe w predyktorach są konwertowane do typu factor, a wiersze z brakami danych w zestawie modelowym są usuwane (na.omit)."),
                      p("Etykiety klas (z sekcji „Etykiety klas”) są używane w prezentacji wykresów oraz wyników modelu.")
                  )
              ),
              
              div(class="section-container",
                  div(class="section-header", "3. Zastosowane metody"),
                  div(class="text-muted-small",
                      tags$ul(
                        tags$li(tags$b("Eksploracja:"), " PCA (redukcja wymiarowości) oraz k-średnich (segmentacja)."),
                        tags$li(tags$b("Klasyfikacja:"), " drzewo decyzyjne (ctree/rpart) lub model zespołowy (Random Forest / XGBoost)."),
                        tags$li(tags$b("Ewaluacja:"), " metryki (Accuracy/Precision/Recall/F1), macierz konfuzji, ROC i AUC (dla targetu binarnego)."),
                        tags$li(tags$b("Interpretacja:"), " ważność predyktorów oraz wizualizacja drzewa (dla RF/XGB: drzewo surrogate).")
                      )
                  )
              ),
              
              div(class="section-container",
                  div(class="section-header", "4. Budowa modelu"),
                  div(class="text-muted-small",
                      p("Model budowany jest na zbiorze treningowym po losowym podziale train/test zgodnie z parametrem proporcji treningowej. Dla drzewa można ograniczyć złożoność przez maxdepth i minsplit. Opcjonalnie dostępna jest walidacja krzyżowa K-Fold (dla modelu drzewnego).")
                  ),
                  fluidRow(
                    column(6,
                           h4("Parametry modelu", style="color:#004A7F; font-weight:900;"),
                           uiOutput("report_params_ui")
                    ),
                    column(6,
                           h4("Metryki (test)", style="color:#004A7F; font-weight:900;"),
                           tableOutput("metrics_table_report")
                    )
                  )
              ),
              
              div(class="section-container",
                  div(class="section-header", "5. Eksploracja danych"),
                  div(class="text-muted-small",
                      p("Eksploracja służy zrozumieniu struktury danych, wykryciu redundancji oraz potencjalnych segmentów obserwacji. Poniżej prezentowane są wyniki PCA i k-średnich.")
                  )
              ),
              
              div(class="section-container",
                  div(class="section-header", "5.1 PCA"),
                  div(class="text-muted-small",
                      p("Biplot pokazuje rozkład obserwacji w przestrzeni dwóch pierwszych składowych oraz wektory zmiennych (ładunki). Wysoka współliniowość predyktorów często objawia się skupieniem wektorów w podobnych kierunkach.")
                  ),
                  plotOutput("pca_scores_plot_report", height = "520px"),
                  fluidRow(
                    column(6, plotOutput("pca_scree_plot_report", height="280px")),
                    column(6, tableOutput("pca_top_table_report"))
                  )
              ),
              
              div(class="section-container",
                  div(class="section-header", "6. Wyniki"),
                  div(class="text-muted-small",
                      p("Poniżej przedstawiono wyniki jakości modelu oraz elementy interpretacyjne.")
                  )
              ),
              
              div(class="section-container",
                  div(class="section-header", "6.1 Skuteczność modelu"),
                  uiOutput("kpi_summary_ui_report"),
                  uiOutput("cv_results_ui_report")
              ),
              
              div(class="section-container",
                  div(class="section-header", "6.2 Macierz konfuzji"),
                  div(class="text-muted-small",
                      p("Macierz konfuzji pokazuje liczbę poprawnych i błędnych klasyfikacji. Dla klasyfikacji binarnej w komórkach oznaczono TP/TN/FP/FN.")
                  ),
                  plotOutput("cm_heatmap_report", height = "420px"),
                  div(style="overflow-x:auto;", tableOutput("conf_matrix"))
              ),
              
              div(class="section-container",
                  div(class="section-header", "6.3 Drzewo decyzyjne"),
                  div(class="text-muted-small",
                      p("Wizualizacja przedstawia strukturę drzewa. Dla modeli zespołowych (Random Forest / XGBoost) prezentowane jest drzewo surrogate, które przybliża logikę decyzji modelu.")
                  ),
                  plotOutput("tree_plot_report", height = "640px")
              ),
              
              div(class="section-container",
                  div(class="section-header", "6.4 Ważność predyktorów"),
                  div(class="text-muted-small",
                      p("Ranking ważności predyktorów wskazuje zmienne najbardziej wpływające na decyzje modelu. Dla drzewa liczona jest varimp, dla RF: importance(), dla XGB: gain.")
                  ),
                  plotOutput("var_imp_plot_report", height = "360px"),
                  hr(),
                  h5("Ważność predyktorów (tabela)", style="color:#004A7F; font-weight:900;"),
                  div(style="overflow-x:auto;", tableOutput("var_imp_table_report"))
              ),
              
              div(class="section-container",
                  div(class="section-header", "Dodatkowo: ROC / AUC (dla 2 klas)"),
                  plotOutput("roc_plot_report", height = "320px")
              ),
              
              div(class="section-container",
                  div(class="section-header", "7. Wnioski"),
                  div(class="text-muted-small",
                      p("1) Jeżeli metryki są stabilne oraz macierz konfuzji nie wskazuje dominujących błędów jednej klasy, model można traktować jako użyteczny predyktor w badanym problemie."),
                      p("2) Jeżeli klasy są niezbalansowane, kluczowe jest interpretowanie F1/Recall oraz analiza rozkładu prawdopodobieństw predykcji."),
                      p("3) Interpretacja (ważność predyktorów + drzewo/surrogate) powinna być zestawiona z wiedzą dziedzinową, aby uniknąć wnioskowania przyczynowego z korelacji.")
                  )
              ),
              
          ),
          fluidRow(
            column(
              12,downloadButton("download_report_docx", "POBIERZ RAPORT DOCX")

            )
          )

        )

        # --- Asystent Gemini ---
        ,
        tabPanel(
          "Asystent AI",
          div(class = "container-fluid",
              br(),

              div(class = "section-container",
                  div(class = "section-header", "Asystent Gemini dla analityków"),
                  div(class = "text-muted-small",
                      p("Profesjonalny chat wspierany przez modele Gemini pomaga w interpretacji wyników, przygotowaniu raportu i szukaniu pomysłów na kolejne analizy."),
                      p(tags$b("Dane uwaga:"), " klucz API nie jest zapisywany i pozostaje tylko w tej sesji przeglądarki.")
                  )
              ),

              fluidRow(
                column(
                  4,
                  div(class = "section-container",
                      h4("Konfiguracja", style="color:#004A7F; font-weight:900;"),
                      passwordInput("gemini_api_key", "Klucz API Gemini", placeholder = "wpisz klucz..."),
                      selectInput("gemini_model", "Model", choices = c("gemini-1.5-flash", "gemini-1.5-pro"), selected = "gemini-1.5-flash"),
                      textAreaInput(
                        "ai_system_prompt", "Kontekst rozmowy",
                        value = "Jesteś analitycznym asystentem ML w ochronie zdrowia. Odpowiadasz rzeczowo, podając klarowne rekomendacje kroków analitycznych.",
                        rows = 5, resize = "vertical"
                      ),
                      div(class = "ai-chat-actions",
                          actionButton("clear_ai_chat", "Wyczyść wątek", icon = icon("trash"), class = "btn-secondary")
                      )
                  )
                ),
                column(
                  8,
                  div(class = "ai-chat-panel",
                      div(class = "section-header", "Okno rozmowy"),
                      div(class = "ai-chat-history", uiOutput("ai_chat_history")),
                      textAreaInput("ai_user_message", "Twoja wiadomość", placeholder = "Opisz problem, poproś o podsumowanie wyników lub wygenerowanie sugestii raportu...", rows = 4, resize = "vertical"),
                      div(class = "ai-chat-actions",
                          actionButton("send_ai_message", "Wyślij do Gemini", icon = icon("paper-plane"), class = "btn-primary"),
                          actionButton("insert_last_metrics", "Wklej ostatnie metryki", icon = icon("paste"), class = "btn-outline-primary")
                      )
                  )
                )
              )
          )
        )
      )
    )
  )
)

# 4. SERVER -----------------------------------------------------------
server <- function(input, output, session) {
  
  observeEvent(input$start_app, {
    shinyjs::hide("landing_page", anim = TRUE, animType = "fade")
    shinyjs::show("main_app", anim = TRUE, animType = "fade")
    updateTabsetPanel(session, "main_tabs", selected = "1. Dane")
  })
  
  observeEvent(input$start_app_tab, {
    updateTabsetPanel(session, "main_tabs", selected = "Dane")
  })
  
  # --- Dane ---
  dane <- reactive({
    req(input$file)
    tryCatch({
      df <- read.csv(input$file$datapath, header = input$header, sep = input$sep, stringsAsFactors = FALSE)
      names(df) <- make.names(names(df), unique = TRUE)
      df
    }, error = function(e) {
      showNotification(paste("Błąd wczytania:", e$message), type = "error")
      NULL
    })
  })
  
  observeEvent(dane(), {
    df <- dane(); req(df)
    N <- nrow(df)
    updateNumericInput(session, "minsplit", value = max(20, floor(N * 0.05)))
  })
  
  output$data_kpi_ui <- renderUI({
    df <- dane()
    if (is.null(df)) return(NULL)
    na_pct <- round(sum(is.na(df)) / (nrow(df) * ncol(df)) * 100, 1)
    fluidRow(class = "kpi-row",
             column(4, div(class="kpi-box", div(class="kpi-val", nrow(df)), div(class="kpi-lbl", "Obserwacji"))),
             column(4, div(class="kpi-box", div(class="kpi-val", ncol(df)), div(class="kpi-lbl", "Zmiennych"))),
             column(4, div(class="kpi-box", div(class="kpi-val", paste0(na_pct, "%")), div(class="kpi-lbl", "Braków danych")))
    )
  })
  
  output$var_target_ui <- renderUI({
    df <- dane(); req(df)
    is_cat <- sapply(df, function(x) length(unique(x)) < 10)
    sel <- names(df)[1]
    if ("zgon" %in% names(df)) sel <- "zgon"
    else if (any(is_cat)) sel <- names(df)[is_cat][1]
    selectInput("target", NULL, choices = names(df), selected = sel, width = "100%")
  })
  
  output$var_predictors_ui <- renderUI({
    df <- dane(); req(df, input$target)
    choices <- setdiff(names(df), input$target)
    
    nrows <- nrow(df)
    safe <- c()
    for (col in choices) {
      n_unique <- length(unique(df[[col]]))
      is_id_candidate <- (n_unique == nrows) || (n_unique > 0.9 * nrows && nrows > 50)
      is_text_many_levels <- (is.character(df[[col]]) || is.factor(df[[col]])) && n_unique > 50
      if (!is_id_candidate && !is_text_many_levels) safe <- c(safe, col)
    }
    
    pickerInput(
      "predictors", NULL,
      choices = choices,
      selected = safe,
      multiple = TRUE,
      options = list(`actions-box` = TRUE, `live-search` = TRUE),
      width = "100%"
    )
  })
  
  class_info <- reactive({
    df <- dane(); req(df, input$target)
    y <- df[[input$target]]
    if (!is.factor(y)) y <- factor(y)
    data.frame(level = levels(y), id = paste0("lbl_", seq_along(levels(y))), stringsAsFactors = FALSE)
  })
  
  output$class_labels_ui <- renderUI({
    ci <- class_info()
    tagList(lapply(seq_len(nrow(ci)), function(i) {
      textInput(ci$id[i], paste0("Klasa: ", ci$level[i]), value = ci$level[i], width = "100%")
    }))
  })
  
  class_labels_map <- reactive({
    ci <- class_info()
    if (is.null(ci) || nrow(ci) == 0) return(NULL)
    new_lvls <- ci$level
    for (i in seq_along(new_lvls)) {
      val <- input[[ci$id[i]]]
      if (!is.null(val) && nzchar(val)) new_lvls[i] <- val
    }
    new_lvls
  })
  
  apply_class_labels <- function(y_fac) {
    ci <- class_info()
    new_lvls <- class_labels_map()
    if (is.null(ci) || is.null(new_lvls)) return(y_fac)
    levels(y_fac) <- new_lvls
    y_fac
  }
  
  output$data_head_table <- renderTable({
    df <- dane(); req(df)
    head(df, 10)
  }, bordered = TRUE)
  
  # --- Szybka analiza (wybór zmiennej) ---
  output$var_explore_selector_ui <- renderUI({
    df <- dane(); req(df)
    selectInput("explore_var", NULL, choices = names(df), selected = names(df)[1], width = "100%")
  })
  
  is_discrete_logic <- function(x) !is.numeric(x) || length(unique(x)) < 15
  
  output$target_dist_plot <- renderPlot({
    df <- dane(); req(df, input$target)
    y <- df[[input$target]]
    if (!is.factor(y)) y <- factor(y)
    y <- apply_class_labels(y)
    
    ggplot(data.frame(y), aes(x = y)) +
      geom_bar(fill = "#004A7F", alpha = 0.85) +
      theme_minimal(base_size = 14) +
      labs(x = NULL, y = "Liczebność") +
      theme(axis.text.x = element_text(face = "bold"))
  })
  
  output$variable_dist_plot <- renderPlot({
    df <- dane(); req(df, input$explore_var)
    v <- input$explore_var
    x <- df[[v]]
    is_discrete <- is_discrete_logic(x)
    
    if (!is_discrete) {
      ggplot(df, aes(x = .data[[v]])) +
        geom_histogram(fill = "#00A3C4", color = "white", bins = 30, alpha = 0.8) +
        theme_minimal(base_size = 14) +
        labs(x = v, y = "Liczebność")
    } else {
      ggplot(df, aes(x = as.factor(.data[[v]]))) +
        geom_bar(fill = "#2E7D32", alpha = 0.85) +
        theme_minimal(base_size = 14) +
        labs(x = v, y = "Liczebność") +
        theme(axis.text.x = element_text(face = "bold"))
    }
  })
  
  output$variable_target_plot <- renderPlot({
    df <- dane(); req(df, input$explore_var, input$target)
    if (input$explore_var == input$target) return(NULL)
    
    v <- input$explore_var
    target <- input$target
    df[[target]] <- as.factor(df[[target]])
    df[[target]] <- apply_class_labels(df[[target]])
    
    ggplot(df, aes(x = factor(.data[[v]]), fill = .data[[target]])) +
      geom_bar(position = "fill", alpha = 0.9, color = "white") +
      scale_fill_brewer(palette = "Set1") +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal(base_size = 14) +
      labs(y = "Udział %", x = v, fill = "Target") +
      theme(legend.position = "bottom", axis.text = element_text(face = "bold"))
  })
  
  # --- Statystyki opisowe ---
  output$continuous_vars_table <- renderTable({
    df <- dane(); req(df)
    nums <- df %>% select(where(is.numeric))
    if (ncol(nums) == 0) return(data.frame(Komunikat = "Brak zmiennych numerycznych."))
    
    tidyr::pivot_longer(nums, cols = everything()) %>%
      group_by(name) %>%
      summarise(
        N = sum(!is.na(value)),
        NA_pct = paste0(round(mean(is.na(value)) * 100, 1), "%"),
        Mean = round(mean(value, na.rm = TRUE), 2),
        Median = round(median(value, na.rm = TRUE), 2),
        SD = round(sd(value, na.rm = TRUE), 2),
        Min = round(min(value, na.rm = TRUE), 2),
        Max = round(max(value, na.rm = TRUE), 2),
        Skew = round(calc_skewness(value), 2),
        .groups = "drop"
      ) %>%
      rename(Zmienna = name)
  }, bordered = TRUE, hover = TRUE)
  
  # --- PCA ---
  pca_prep <- reactive({
    df <- dane(); req(df, input$target, input$predictors)
    validate(need(length(input$predictors) > 0, "Wybierz predyktory."))
    
    df2 <- df %>% select(all_of(c(input$target, input$predictors))) %>% na.omit()
    validate(need(nrow(df2) > 10, "Za mało obserwacji po usunięciu braków (wymagane > 10)."))
    
    X <- df2 %>% select(all_of(input$predictors)) %>% select(where(is.numeric))
    validate(need(ncol(X) >= 2, "PCA wymaga min. 2 predyktorów numerycznych."))
    
    vars <- sapply(X, var, na.rm = TRUE)
    X <- X[, vars > 1e-6, drop = FALSE]
    validate(need(ncol(X) >= 2, "Po usunięciu zmiennych stałych PCA wymaga min. 2 zmiennych o niezerowej wariancji."))
    
    list(X = X, y = factor(df2[[input$target]]))
  })
  
  pca_res <- reactive({
    d <- pca_prep()
    prcomp(d$X, center = TRUE, scale. = isTRUE(input$pca_scale))
  })
  
  output$pca_info_ui <- renderUI({
    res <- pca_res()
    expl <- (res$sdev^2) / sum(res$sdev^2)
    if (length(expl) < 2) return(NULL)
    div(class="text-muted-small",
        p(paste0("PC1 wyjaśnia ", round(expl[1] * 100, 1), "%, a PC2 wyjaśnia ", round(expl[2] * 100, 1), "% wariancji.")))
  })
  
  output$pca_commentary_ui <- renderUI({
    res <- pca_res()
    eigs <- res$sdev^2
    variance_explained <- eigs / sum(eigs)
    cumulative_variance <- cumsum(variance_explained)
    kaiser <- sum(eigs > 1)
    
    if (kaiser >= 2) {
      total <- round(cumulative_variance[kaiser] * 100, 1)
      HTML(paste0("<div class='text-muted-small'><b>Kryterium Kaisera:</b> zachowaj <b>", kaiser,
                  "</b> składowych (łącznie <b>", total, "%</b> wariancji).</div>"))
    } else if (kaiser == 1) {
      HTML("<div class='text-muted-small'><b>Kryterium Kaisera:</b> tylko 1 składowa > 1. PCA może mieć ograniczoną użyteczność.</div>")
    } else {
      HTML("<div class='text-muted-small'><b>Kryterium Kaisera:</b> brak składowych > 1 – redukcja wymiarowości może być nieefektywna.</div>")
    }
  })
  
  # Wspólna funkcja biplotu PCA (używana w eksploracji i raporcie)
  output$pca_scores_plot <- renderPlot({
    tryCatch({
      res <- pca_res()
      d <- pca_prep()
      
      scores <- data.frame(res$x[, 1:2, drop = FALSE], Target = d$y)
      colnames(scores)[1:2] <- c("PC1", "PC2")
      
      load <- as.data.frame(res$rotation[, 1:2, drop = FALSE])
      load$var <- rownames(load)
      
      mult <- 0.8 * min(
        (max(scores$PC1) - min(scores$PC1)) / (max(load$PC1) - min(load$PC1) + 1e-9),
        (max(scores$PC2) - min(scores$PC2)) / (max(load$PC2) - min(load$PC2) + 1e-9)
      )
      
      base_fs <- if (!is.null(input$pca_fontsize)) input$pca_fontsize else 14
      
      p <- ggplot(scores, aes(PC1, PC2, color = Target)) +
        geom_point(alpha = input$pca_pt_alpha, size = input$pca_pt_size) +
        theme_minimal(base_size = base_fs) +
        scale_color_brewer(palette = "Set1")
      
      if (isTRUE(input$pca_show_ellipse)) {
        p <- p +
          stat_ellipse(alpha = input$pca_ellipse_alpha, geom = "polygon",
                       aes(fill = Target), show.legend = FALSE) +
          scale_fill_brewer(palette = "Set1")
      }
      
      if (isTRUE(input$pca_show_loadings)) {
        p <- p +
          geom_segment(
            data = load,
            aes(x = 0, y = 0, xend = PC1 * mult, yend = PC2 * mult),
            arrow = arrow(length = unit(input$pca_arrow_len, "cm")),
            linewidth = input$pca_arrow_size,
            inherit.aes = FALSE
          ) +
          geom_text(
            data = load,
            aes(x = PC1 * mult, y = PC2 * mult, label = var),
            inherit.aes = FALSE,
            size = input$pca_label_size,
            check_overlap = isTRUE(input$pca_check_overlap),
            vjust = "outward"
          )
      }
      
      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste0("PCA: ", e$message))
    })
  })
  
  
  pca_scree_plot_obj <- reactive({
    res <- pca_res()
    vars <- (res$sdev^2) / sum(res$sdev^2)
    dfp <- data.frame(PC = seq_along(vars), Var = vars, Cum = cumsum(vars))
    dfp <- dfp[1:min(10, nrow(dfp)), ]
    
    ggplot(dfp, aes(x = PC, y = Var)) +
      geom_col(fill = "#004A7F", alpha = 0.9) +
      geom_line(aes(y = Cum, group = 1)) +
      geom_point(aes(y = Cum)) +
      scale_y_continuous(labels = scales::percent) +
      theme_minimal(base_size = 14) +
      labs(x = "Składowa", y = "Udział wariancji")
  })
  
  pca_top_table_obj <- reactive({
    res <- pca_res()
    rot <- as.data.frame(res$rotation)
    max_pc <- min(5, ncol(rot))
    imp <- rowMeans(abs(rot[, 1:max_pc, drop = FALSE]))
    out <- data.frame(Zmienna = rownames(rot), Waznosc_PCA = round(imp, 4))
    out <- out[order(-out$Waznosc_PCA), ]
    head(out, input$pca_top_n)
  })
  
  # Eksploracja: PCA
  output$pca_scores_plot_expl <- renderPlot({
    tryCatch({
      res <- pca_res()
      d <- pca_prep()
      
      scores <- data.frame(res$x[, 1:2, drop = FALSE], Target = d$y)
      colnames(scores)[1:2] <- c("PC1", "PC2")
      
      load <- as.data.frame(res$rotation[, 1:2, drop = FALSE])
      load$var <- rownames(load)
      
      mult <- 0.8 * min(
        (max(scores$PC1) - min(scores$PC1)) / (max(load$PC1) - min(load$PC1) + 1e-9),
        (max(scores$PC2) - min(scores$PC2)) / (max(load$PC2) - min(load$PC2) + 1e-9)
      )
      
      base_fs <- if (!is.null(input$pca_fontsize)) input$pca_fontsize else 14
      
      p <- ggplot(scores, aes(PC1, PC2, color = Target)) +
        geom_point(alpha = input$pca_pt_alpha, size = input$pca_pt_size) +
        theme_minimal(base_size = base_fs) +
        scale_color_brewer(palette = "Set1")
      
      if (isTRUE(input$pca_show_ellipse)) {
        p <- p +
          stat_ellipse(alpha = input$pca_ellipse_alpha, geom = "polygon",
                       aes(fill = Target), show.legend = FALSE) +
          scale_fill_brewer(palette = "Set1")
      }
      
      if (isTRUE(input$pca_show_loadings)) {
        p <- p +
          geom_segment(
            data = load,
            aes(x = 0, y = 0, xend = PC1 * mult, yend = PC2 * mult),
            arrow = arrow(length = unit(input$pca_arrow_len, "cm")),
            linewidth = input$pca_arrow_size,
            inherit.aes = FALSE
          ) +
          geom_text(
            data = load,
            aes(x = PC1 * mult, y = PC2 * mult, label = var),
            inherit.aes = FALSE,
            size = input$pca_label_size,
            check_overlap = isTRUE(input$pca_check_overlap),
            vjust = "outward"
          )
      }
      
      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste0("PCA: ", e$message))
    })
  })
  output$pca_scree_plot_expl  <- renderPlot({ pca_scree_plot_obj() })
  output$pca_top_table_expl   <- renderTable({ pca_top_table_obj() }, bordered = TRUE, hover = TRUE)
  
  # Raport: PCA
  output$pca_scores_plot_report <- renderPlot({
    tryCatch({
      res <- pca_res()
      d <- pca_prep()
      
      scores <- data.frame(res$x[, 1:2, drop = FALSE], Target = d$y)
      colnames(scores)[1:2] <- c("PC1", "PC2")
      
      load <- as.data.frame(res$rotation[, 1:2, drop = FALSE])
      load$var <- rownames(load)
      
      mult <- 0.8 * min(
        (max(scores$PC1) - min(scores$PC1)) / (max(load$PC1) - min(load$PC1) + 1e-9),
        (max(scores$PC2) - min(scores$PC2)) / (max(load$PC2) - min(load$PC2) + 1e-9)
      )
      
      base_fs <- if (!is.null(input$pca_fontsize)) input$pca_fontsize else 14
      
      p <- ggplot(scores, aes(PC1, PC2, color = Target)) +
        geom_point(alpha = input$pca_pt_alpha, size = input$pca_pt_size) +
        theme_minimal(base_size = base_fs) +
        scale_color_brewer(palette = "Set1")
      
      if (isTRUE(input$pca_show_ellipse)) {
        p <- p +
          stat_ellipse(alpha = input$pca_ellipse_alpha, geom = "polygon",
                       aes(fill = Target), show.legend = FALSE) +
          scale_fill_brewer(palette = "Set1")
      }
      
      if (isTRUE(input$pca_show_loadings)) {
        p <- p +
          geom_segment(
            data = load,
            aes(x = 0, y = 0, xend = PC1 * mult, yend = PC2 * mult),
            arrow = arrow(length = unit(input$pca_arrow_len, "cm")),
            linewidth = input$pca_arrow_size,
            inherit.aes = FALSE
          ) +
          geom_text(
            data = load,
            aes(x = PC1 * mult, y = PC2 * mult, label = var),
            inherit.aes = FALSE,
            size = input$pca_label_size,
            check_overlap = isTRUE(input$pca_check_overlap),
            vjust = "outward"
          )
      }
      
      p
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, paste0("PCA: ", e$message))
    })
  })
  output$pca_scree_plot_report  <- renderPlot({ pca_scree_plot_obj() })
  output$pca_top_table_report   <- renderTable({ pca_top_table_obj() }, bordered = TRUE, hover = TRUE)
  
  
  bool_to_tak_nie <- function(x) {
    if (is.logical(x)) {
      factor(
        ifelse(x, "Tak", "Nie"),
        levels = c("Nie", "Tak")
      )
    } else if (is.factor(x) && all(levels(x) %in% c(FALSE, TRUE))) {
      factor(
        ifelse(x == TRUE, "Tak", "Nie"),
        levels = c("Nie", "Tak")
      )
    } else {
      x
    }
  }
  
  # --- Modelowanie ---------------------------------------------------
  split_data <- eventReactive(input$run_model, {
    df <- dane(); req(df, input$target, input$predictors)
    validate(need(length(input$predictors) > 0, "Wybierz predyktory."))
    
    y <- df[[input$target]]
    if (!is.factor(y)) y <- factor(y)
    
    ci <- class_info()
    new_lvls <- levels(y)
    for (i in seq_along(new_lvls)) {
      val <- input[[ci$id[i]]]
      if (!is.null(val) && nzchar(val)) new_lvls[i] <- val
    }
    levels(y) <- new_lvls
    df[[input$target]] <- y
    
    for (col in input$predictors) {
      
      # tekst → factor
      if (is.character(df[[col]])) {
        df[[col]] <- as.factor(df[[col]])
      }
      
      # binarne numeryczne (0/1) → TRUE/FALSE
      if (is.numeric(df[[col]]) && length(unique(df[[col]])) == 2) {
        df[[col]] <- df[[col]] > 0
      }
      
      # TRUE/FALSE → „Tak / Nie”
      df[[col]] <- bool_to_tak_nie(df[[col]])
    }
    
    
    dfm <- df %>% select(all_of(c(input$target, input$predictors))) %>% na.omit()
    validate(need(nrow(dfm) >= 30, "Za mało danych po usunięciu braków (zalecane >= 30)."))
    
    set.seed(input$seed)
    idx <- sample(seq_len(nrow(dfm)), size = floor(input$train_prop * nrow(dfm)))
    list(train = dfm[idx, , drop = FALSE], test = dfm[-idx, , drop = FALSE], full = dfm)
  })
  
  build_xgb_matrices <- function(train_df, test_df) {
    X_train <- model.matrix(~ . - 1, data = train_df)
    X_test  <- model.matrix(~ . - 1, data = test_df)
    
    cols_all <- union(colnames(X_train), colnames(X_test))
    add_missing <- function(M, cols_all) {
      missing <- setdiff(cols_all, colnames(M))
      if (length(missing) > 0) {
        Z <- matrix(0, nrow = nrow(M), ncol = length(missing))
        colnames(Z) <- missing
        M <- cbind(M, Z)
      }
      M <- M[, cols_all, drop = FALSE]
      M
    }
    X_train <- add_missing(X_train, cols_all)
    X_test  <- add_missing(X_test, cols_all)
    
    list(X_train = X_train, X_test = X_test)
  }
  
  model_obj <- eventReactive(input$run_model, {
    d <- split_data()
    train <- d$train
    f <- as.formula(paste(input$target, "~ ."))
    
    if (input$model_type == "tree") {
      if (input$split_metric == "ctree") {
        partykit::ctree(
          f,
          data = train,
          control = partykit::ctree_control(minsplit = input$minsplit, maxdepth = input$maxdepth)
        )
      } else {
        m <- rpart::rpart(
          f,
          data = train,
          parms = list(split = input$split_metric),
          control = rpart::rpart.control(minsplit = input$minsplit, maxdepth = input$maxdepth, cp = 0.001)
        )
        partykit::as.party(m)
      }
    } else if (input$model_type == "rf") {
      mtry_val <- if (isTRUE(input$rf_mtry > 0)) input$rf_mtry else floor(sqrt(ncol(train) - 1))
      randomForest::randomForest(
        formula = f,
        data = train,
        ntree = input$rf_ntree,
        mtry = max(1, mtry_val),
        importance = TRUE
      )
    } else if (input$model_type == "xgb") {
      validate(need(nlevels(train[[input$target]]) == 2, "XGBoost: w tej wersji obsługiwany jest target binarny (2 klasy)."))
      
      train_x <- train[, input$predictors, drop = FALSE]
      test_x  <- d$test[, input$predictors, drop = FALSE]
      mats <- build_xgb_matrices(train_x, test_x)
      
      y_fac <- train[[input$target]]
      y <- as.numeric(y_fac) - 1
      
      xgboost::xgboost(
        data = mats$X_train,
        label = y,
        objective = "binary:logistic",
        nrounds = input$xgb_nrounds,
        eta = input$xgb_eta,
        max_depth = input$xgb_maxdepth,
        subsample = 0.8,
        colsample_bytree = 0.8,
        verbose = 0
      )
    } else {
      stop("Nieznany model_type")
    }
  })
  
  surrogate_tree <- eventReactive(input$run_model, {
    req(input$model_type %in% c("rf", "xgb"))
    d <- split_data()
    train <- d$train
    test  <- d$test
    m <- model_obj()
    
    if (input$model_type == "rf") {
      pred_train <- predict(m, newdata = train, type = "response")
    } else {
      validate(need(nlevels(train[[input$target]]) == 2, "Surrogate dla XGB wymaga 2 klas."))
      mats <- build_xgb_matrices(train[, input$predictors, drop = FALSE],
                                 test[, input$predictors, drop = FALSE])
      p_train <- as.numeric(predict(m, mats$X_train))
      lev <- levels(train[[input$target]])
      pred_train <- factor(ifelse(p_train >= 0.5, lev[2], lev[1]), levels = lev)
    }
    
    df_sur <- train
    df_sur$._PRED_SURROGATE_ <- factor(pred_train)
    
    partykit::ctree(
      ._PRED_SURROGATE_ ~ .,
      data = df_sur,
      control = partykit::ctree_control(minsplit = input$minsplit, maxdepth = min(4, input$maxdepth))
    )
  })
  
  output$model_summary <- renderPrint({
    req(model_obj())
    m <- model_obj()
    
    if (input$model_type == "tree") {
      cat("Model: drzewo\n")
      cat("Metoda:", input$split_metric, "\n")
      print(m)
      if (input$split_metric == "ctree") {
        cat("\n--- Summary (ctree) ---\n")
        print(summary(m))
      }
    } else if (input$model_type == "rf") {
      cat("Model: Random Forest\n")
      print(m)
      cat("\nUwaga: zakładka 'Drzewo decyzyjne' pokazuje drzewo zastępcze (surrogate) opisujące decyzje RF.\n")
    } else if (input$model_type == "xgb") {
      cat("Model: XGBoost (binary)\n")
      print(m)
      cat("\nUwaga: zakładka 'Drzewo decyzyjne' pokazuje drzewo zastępcze (surrogate) opisujące decyzje XGB.\n")
    }
  })
  
  # CV (tylko dla drzewa)
  cv_res <- eventReactive(input$run_model, {
    if (!isTRUE(input$do_cv)) return(NULL)
    validate(need(input$model_type == "tree", "Walidacja K-Fold jest dostępna tylko dla modelu drzewnego w tej wersji."))
    
    d <- split_data()$full
    k <- input$cv_folds
    f <- as.formula(paste(input$target, "~ ."))
    
    set.seed(input$seed)
    folds <- sample(rep(1:k, length.out = nrow(d)))
    accs <- numeric(k)
    
    withProgress(message = "Walidacja krzyżowa (K-Fold)...", value = 0, {
      for (i in 1:k) {
        test_idx <- which(folds == i)
        train_d <- d[-test_idx, , drop = FALSE]
        test_d  <- d[test_idx, , drop = FALSE]
        
        if (input$split_metric == "ctree") {
          m <- partykit::ctree(
            f, data = train_d,
            control = partykit::ctree_control(minsplit = input$minsplit, maxdepth = input$maxdepth)
          )
          p <- predict(m, newdata = test_d, type = "response")
        } else {
          m <- rpart::rpart(
            f, data = train_d,
            control = rpart::rpart.control(minsplit = input$minsplit, maxdepth = input$maxdepth, cp = 0.001)
          )
          p <- predict(m, newdata = test_d, type = "class")
        }
        accs[i] <- mean(p == test_d[[input$target]])
        incProgress(1 / k)
      }
    })
    
    list(mean = mean(accs), sd = sd(accs), k = k)
  })
  
  # (NAPRAWA) osobne UI outputy, żeby nie znikały w różnych miejscach
  make_cv_box <- function(res) {
    if (is.null(res)) return(NULL)
    div(class="cv-result-box",
        h4("Wyniki walidacji krzyżowej (K-Fold)", style="margin:0; color:#004A7F; font-weight:900;"),
        h3(paste0(round(res$mean * 100, 2), "% ± ", round(res$sd * 100, 2), "%"), style="margin:5px 0; font-weight:900;"),
        p(paste0("Średnia dokładność ± SD na ", res$k, " foldach."), style="margin:0; font-size:12px; color:#666")
    )
  }
  
  output$cv_results_ui_main <- renderUI({ make_cv_box(cv_res()) })
  output$cv_results_ui_quality <- renderUI({ make_cv_box(cv_res()) })
  output$cv_results_ui_report <- renderUI({ make_cv_box(cv_res()) })
  output$cv_results_ui_inline <- renderUI({
    res <- cv_res()
    if (is.null(res)) {
      div(class="text-muted-small",
          p(strong("Wyniki walidacji krzyżowej (K-Fold)")),
          p("Aby wyświetlić wyniki, zaznacz „Walidacja krzyżowa (K-Fold)” i zbuduj model (dostępne dla drzewa).")
      )
    } else {
      make_cv_box(res)
    }
  })
  
  # --- Drzewo (drzewo albo surrogate) ---
  output$tree_plot <- renderPlot({
    req(model_obj())
    
    if (input$model_type == "tree") {
      m <- model_obj()
      
      pred_levels <- tryCatch(levels(predict(m)), error = function(e) NULL)
      n_classes <- if (is.null(pred_levels)) 3 else max(3, length(pred_levels))
      pal <- RColorBrewer::brewer.pal(min(12, n_classes), "Set1")
      
      tp <- partykit::node_barplot(m, beside = isTRUE(input$bar_beside), col = pal)
      
      show_p <- if (input$split_metric == "ctree") isTRUE(input$show_pval) else FALSE
      ip <- partykit::node_inner(m, id = isTRUE(input$show_ids), pval = show_p)
      
      plot(m, terminal_panel = tp, inner_panel = ip, gp = grid::gpar(fontsize = input$tree_fontsize))
      
    } else {
      req(surrogate_tree())
      m <- surrogate_tree()
      
      tp <- partykit::node_barplot(m, beside = TRUE)
      ip <- partykit::node_inner(m, id = TRUE)
      
      plot(m, terminal_panel = tp, inner_panel = ip, gp = grid::gpar(fontsize = input$tree_fontsize))
    }
  })
  
  output$tree_plot_report <- renderPlot({
    req(model_obj())
    
    if (input$model_type == "tree") {
      m <- model_obj()
      
      pred_levels <- tryCatch(levels(predict(m)), error = function(e) NULL)
      n_classes <- if (is.null(pred_levels)) 3 else max(3, length(pred_levels))
      pal <- RColorBrewer::brewer.pal(min(12, n_classes), "Set1")
      
      tp <- partykit::node_barplot(m, beside = isTRUE(input$bar_beside), col = pal)
      show_p <- if (input$split_metric == "ctree") isTRUE(input$show_pval) else FALSE
      ip <- partykit::node_inner(m, id = isTRUE(input$show_ids), pval = show_p)
      
      plot(m, terminal_panel = tp, inner_panel = ip, gp = grid::gpar(fontsize = input$tree_fontsize))
      
    } else {
      req(surrogate_tree())
      m <- surrogate_tree()
      
      tp <- partykit::node_barplot(m, beside = TRUE)
      ip <- partykit::node_inner(m, id = TRUE)
      
      plot(m, terminal_panel = tp, inner_panel = ip, gp = grid::gpar(fontsize = input$tree_fontsize))
    }
  })
  
  # Test + metryki ----------------------------------------------------
  test_res <- reactive({
    req(model_obj(), split_data())
    d <- split_data()
    train <- d$train
    test <- d$test
    m <- model_obj()
    
    actual <- factor(test[[input$target]])
    prob <- NULL
    
    if (input$model_type == "tree") {
      pred <- predict(m, newdata = test, type = "response")
      pred <- factor(pred, levels = levels(actual))
      prob <- tryCatch(predict(m, newdata = test, type = "prob"), error = function(e) NULL)
      
    } else if (input$model_type == "rf") {
      pred <- predict(m, newdata = test, type = "response")
      pred <- factor(pred, levels = levels(actual))
      prob <- tryCatch(predict(m, newdata = test, type = "prob"), error = function(e) NULL)
      
    } else if (input$model_type == "xgb") {
      validate(need(nlevels(train[[input$target]]) == 2, "XGBoost: ROC/PROB dostępne tylko dla 2 klas."))
      mats <- build_xgb_matrices(train[, input$predictors, drop = FALSE],
                                 test[, input$predictors, drop = FALSE])
      p <- as.numeric(predict(m, mats$X_test))
      lev <- levels(actual)
      pred <- factor(ifelse(p >= 0.5, lev[2], lev[1]), levels = lev)
      
      prob <- cbind(1 - p, p)
      colnames(prob) <- lev
    }
    
    # Ujednolicenie formatu prob
    if (!is.null(prob)) {
      if (is.list(prob) && !is.matrix(prob)) {
        prob <- tryCatch(do.call(rbind, prob), error = function(e) NULL)
      }
      if (is.matrix(prob)) {
        levs <- levels(actual)
        if (!all(levs %in% colnames(prob))) {
          if (ncol(prob) == length(levs)) colnames(prob) <- levs
        } else {
          prob <- prob[, levs, drop = FALSE]
        }
      } else {
        prob <- NULL
      }
    }
    
    conf <- table(actual, pred)
    acc <- sum(diag(conf)) / sum(conf)
    
    prec_vals <- diag(conf) / pmax(1, colSums(conf))
    rec_vals  <- diag(conf) / pmax(1, rowSums(conf))
    f1_vals <- ifelse((prec_vals + rec_vals) == 0, NA_real_, 2 * prec_vals * rec_vals / (prec_vals + rec_vals))
    
    metrics <- list(
      acc  = acc,
      prec = mean(prec_vals, na.rm = TRUE),
      rec  = mean(rec_vals,  na.rm = TRUE),
      f1   = mean(f1_vals,   na.rm = TRUE)
    )
    
    list(pred = pred, actual = actual, prob = prob, conf = conf, metrics = metrics)
  })
  
  render_kpi_summary <- function(metrics) {
    if (is.null(metrics)) return(NULL)
    
    pct <- function(x) round(x * 100, 2)
    items <- list(
      list(name = "Accuracy",  value = metrics$acc,  desc = "Poprawność ogólna klasyfikacji na zbiorze testowym."),
      list(name = "F1 Score",  value = metrics$f1,   desc = "Kompromis precision/recall; istotny przy nierównych klasach."),
      list(name = "Precision", value = metrics$prec, desc = "Wiarygodność predykcji klasy (średnio po klasach)."),
      list(name = "Recall",    value = metrics$rec,  desc = "Wykrywalność klasy.")
    )
    
    div(
      class = "kpi-summary",
      br(),
      div("Zestaw metryk w formie podsumowania. Wartości prezentowane jako %.", class = "kpi-subtitle"),
      lapply(items, function(it) {
        width <- max(0, min(100, pct(it$value)))
        tagList(
          div(class = "kpi-row-bar",
              div(class = "kpi-metric-name", it$name),
              div(class = "kpi-bar-bg",
                  div(class = "kpi-bar-fill", style = paste0("width:", width, "%;"))
              ),
              div(class = "kpi-value", paste0(pct(it$value), "%"))
          ),
          div(style="margin:-4px 0 8px 0; color:#6c757d; font-size:14px; line-height:1.55;", it$desc),
          tags$hr(style="margin:10px 0; border-top:1px solid #eef1f5;")
        )
      })
    )
  }
  
  output$kpi_summary_ui <- renderUI({
    r <- test_res()
    render_kpi_summary(r$metrics)
  })
  
  output$kpi_summary_ui_report <- renderUI({
    r <- test_res()
    render_kpi_summary(r$metrics)
  })
  
  output$conf_matrix <- renderTable({
    r <- test_res()
    as.data.frame.matrix(r$conf)
  }, rownames = TRUE)
  
  output$metrics_table <- renderTable({
    r <- test_res()
    m <- r$metrics
    data.frame(
      Metryka = c("Accuracy", "Precision", "Recall", "F1"),
      Wartość = c(
        paste0(round(m$acc  * 100, 2), "%"),
        paste0(round(m$prec * 100, 2), "%"),
        paste0(round(m$rec  * 100, 2), "%"),
        paste0(round(m$f1   * 100, 2), "%")
      )
    )
  }, rownames = FALSE)
  
  # --- Macierz konfuzji (NAPRAWA: OSOBNE OUTPUTY main/report) ---
  cm_heatmap_plot <- reactive({
    r <- test_res()
    conf <- r$conf
    
    df <- as.data.frame(conf)
    colnames(df) <- c("Actual", "Pred", "Freq")
    
    df$Tag <- ""
    if (nrow(conf) == 2 && ncol(conf) == 2) {
      neg <- rownames(conf)[1]
      pos <- rownames(conf)[2]
      
      df$Tag <- ifelse(df$Actual == neg & df$Pred == neg, "TN",
                       ifelse(df$Actual == neg & df$Pred == pos, "FP",
                              ifelse(df$Actual == pos & df$Pred == neg, "FN",
                                     ifelse(df$Actual == pos & df$Pred == pos, "TP", ""))))
    }
    
    df$Label <- ifelse(df$Tag == "", as.character(df$Freq), paste0(df$Freq, "\n", df$Tag))
    df$FillTag <- ifelse(df$Tag %in% c("TP", "TN"), df$Tag,
                         ifelse(df$Tag %in% c("FP", "FN"), df$Tag, "OTHER"))
    df$FillTag <- factor(df$FillTag, levels = c("TP", "TN", "FP", "FN", "OTHER"))
    
    ggplot(df, aes(x = Pred, y = Actual, fill = FillTag)) +
      geom_tile(color = "white") +
      geom_text(aes(label = Label), color = "black", size = 6, fontface = "bold", lineheight = 0.95) +
      scale_fill_manual(
        values = c(
          TP = "#1B5E20",
          TN = "#66BB6A",
          FP = "#C62828",
          FN = "#EF5350",
          OTHER = "#9E9E9E"
        ),
        drop = FALSE,
        na.value = "#9E9E9E"
      ) +
      theme_minimal(base_size = 14) +
      labs(x = "Predykcja", y = "Rzeczywiste", title = "Macierz konfuzji") +
      theme(
        axis.text = element_text(face = "bold"),
        legend.position = "none",
        plot.title = element_text(face = "bold", color = "#004A7F", size = 16, hjust = 0.5)
      )
  })
  
  output$cm_heatmap_main <- renderPlot({ cm_heatmap_plot() })
  output$cm_heatmap_report <- renderPlot({ cm_heatmap_plot() })
  
  # --- ROC (OSOBNE OUTPUTY main/report) ---
  roc_plot_obj <- reactive({
    r <- test_res()
    
    if (nlevels(r$actual) != 2) {
      return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "Krzywa ROC dostępna tylko dla 2 klas."))
    }
    if (is.null(r$prob) || !is.matrix(r$prob) || ncol(r$prob) < 2) {
      return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "Brak prawdopodobieństw – nie można policzyć ROC."))
    }
    
    lev <- levels(r$actual)
    pos_col <- lev[2]
    if (!pos_col %in% colnames(r$prob)) {
      return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = "Nie można dopasować kolumny prawdopodobieństw do klas."))
    }
    
    probs <- as.numeric(r$prob[, pos_col])
    
    tryCatch({
      roc_obj <- pROC::roc(r$actual, probs, levels = lev, direction = "<")
      auc_val <- round(as.numeric(pROC::auc(roc_obj)), 3)
      
      pROC::ggroc(roc_obj, colour = "#004A7F", size = 1.2) +
        geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1), linetype = "dashed", color = "grey") +
        annotate("text", x = 0.35, y = 0.25, label = paste0("AUC = ", auc_val), size = 6, color = "#004A7F") +
        theme_minimal(base_size = 14) +
        labs(x = "1 - Specificity", y = "Sensitivity")
    }, error = function(e) {
      ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = paste("Błąd ROC:", e$message))
    })
  })
  
  output$roc_plot_main <- renderPlot({ roc_plot_obj() })
  output$roc_plot_report <- renderPlot({ roc_plot_obj() })
  
  # --- Prob plot (OSOBNE OUTPUTY main/report jeśli trzeba, tu wystarcza main) ---
  output$prob_plot_main <- renderPlot({
    r <- test_res()
    if (is.null(r$prob)) {
      plot.new(); text(0.5, 0.5, "Brak prawdopodobieństw."); return()
    }
    dfp <- data.frame(
      Prob = apply(r$prob, 1, max),
      Class = r$actual
    )
    ggplot(dfp, aes(x = Prob, fill = Class)) +
      geom_density(alpha = 0.5) +
      theme_minimal(base_size = 14) +
      labs(x = "Pewność predykcji (max prob)", y = "Gęstość", fill = "Rzeczywista klasa") +
      scale_fill_brewer(palette = "Set1") +
      xlim(0, 1)
  })
  
  # --- Ważność predyktorów: DF (do tabeli) + wykres (main/report) ---
  var_imp_df <- reactive({
    req(model_obj(), split_data())
    m <- model_obj()
    
    if (input$model_type == "tree") {
      imp_vec <- tryCatch(partykit::varimp(m), error = function(e) NULL)
      if (is.null(imp_vec)) return(data.frame(Komunikat = "Nie udało się policzyć varimp() dla drzewa."))
      df_imp <- data.frame(Zmienna = names(imp_vec), Waga = as.numeric(imp_vec), stringsAsFactors = FALSE)
      df_imp <- df_imp[order(-df_imp$Waga), ]
      df_imp
    } else if (input$model_type == "rf") {
      imp <- tryCatch(randomForest::importance(m), error = function(e) NULL)
      if (is.null(imp)) return(data.frame(Komunikat = "Brak importance() dla RF."))
      imp_vec <- if (is.matrix(imp)) imp[, 1] else imp
      df_imp <- data.frame(Zmienna = names(imp_vec), Waga = as.numeric(imp_vec), stringsAsFactors = FALSE)
      df_imp <- df_imp[order(-df_imp$Waga), ]
      df_imp
    } else if (input$model_type == "xgb") {
      d <- split_data()
      mats <- build_xgb_matrices(d$train[, input$predictors, drop = FALSE],
                                 d$test[, input$predictors, drop = FALSE])
      imp <- tryCatch(xgboost::xgb.importance(model = m, feature_names = colnames(mats$X_train)), error = function(e) NULL)
      if (is.null(imp) || nrow(imp) == 0) return(data.frame(Komunikat = "Brak xgb.importance() dla XGB."))
      df_imp <- data.frame(Zmienna = imp$Feature, Waga = imp$Gain, stringsAsFactors = FALSE)
      df_imp <- df_imp[order(-df_imp$Waga), ]
      df_imp
    } else {
      data.frame(Komunikat = "Nieznany model_type.")
    }
  })
  
  var_imp_plot_obj <- reactive({
    df_imp <- var_imp_df()
    if ("Komunikat" %in% names(df_imp)) {
      return(ggplot() + theme_void() + annotate("text", x = 0.5, y = 0.5, label = df_imp$Komunikat[1]))
    }
    df_top <- head(df_imp, 10)
    ggplot(df_top, aes(x = reorder(Zmienna, Waga), y = Waga)) +
      geom_col(fill = "#00A3C4", alpha = 0.9) +
      coord_flip() +
      theme_minimal(base_size = 14) +
      labs(x = NULL, y = "Ważność")
  })
  
  output$var_imp_plot_main <- renderPlot({ var_imp_plot_obj() })
  output$var_imp_plot_report <- renderPlot({ var_imp_plot_obj() })
  
  output$var_imp_table_main <- renderTable({
    df_imp <- var_imp_df()
    if ("Komunikat" %in% names(df_imp)) return(df_imp)
    head(df_imp, 25)
  }, bordered = TRUE, hover = TRUE)
  
  output$var_imp_table_report <- renderTable({
    df_imp <- var_imp_df()
    if ("Komunikat" %in% names(df_imp)) return(df_imp)
    head(df_imp, 25)
  }, bordered = TRUE, hover = TRUE)
  
  # --- Raport parametry/metyki ---
  output$report_params_ui <- renderUI({
    req(input$target, input$predictors)
    tags$ul(
      tags$li(tags$b("Target: "), input$target),
      tags$li(tags$b("Liczba predyktorów: "), length(input$predictors)),
      tags$li(tags$b("Model: "), input$model_type),
      tags$li(tags$b("Metoda (drzewo): "), ifelse(is.null(input$split_metric), "-", input$split_metric)),
      tags$li(tags$b("minsplit: "), input$minsplit),
      tags$li(tags$b("maxdepth: "), input$maxdepth),
      tags$li(tags$b("Train/Test: "), paste0(round(input$train_prop * 100), "% / ", round((1 - input$train_prop) * 100), "%")),
      tags$li(tags$b("Seed: "), input$seed)
    )
  })
  
  output$metrics_table_report <- renderTable({
    r <- test_res()
    m <- r$metrics
    data.frame(
      Metryka = c("Accuracy", "Precision", "Recall", "F1"),
      Wartość = c(round(m$acc, 4), round(m$prec, 4), round(m$rec, 4), round(m$f1, 4))
    )
  }, rownames = FALSE)

  # --- Asystent Gemini (chat) ---
  chat_history <- reactiveVal(list())

  render_chat_bubble <- function(role, text) {
    role_class <- ifelse(role == "model", "model", "user")
    avatar_text <- ifelse(role_class == "model", "G", "TY")
    title <- ifelse(role_class == "model", "Gemini", "Użytkownik")

    div(
      class = paste("ai-chat-message", role_class),
      div(class = paste("ai-chat-avatar", role_class), avatar_text),
      div(
        div(class = "ai-chat-meta", title),
        div(class = "ai-chat-body", text)
      )
    )
  }

  output$ai_chat_history <- renderUI({
    history <- chat_history()
    if (length(history) == 0) {
      return(div(class = "text-muted-small", "Czekam na pierwszą wiadomość..."))
    }

    tagList(lapply(history, function(msg) {
      render_chat_bubble(msg$role, msg$text)
    }))
  })

  call_gemini_api <- function(conversation, api_key, model, system_prompt) {
    url <- paste0(
      "https://generativelanguage.googleapis.com/v1beta/models/",
      model,
      ":generateContent?key=",
      api_key
    )

    body <- list(
      contents = lapply(conversation, function(msg) {
        list(
          role = ifelse(msg$role == "model", "model", "user"),
          parts = list(list(text = msg$text))
        )
      })
    )

    if (!is.null(system_prompt) && nzchar(trimws(system_prompt))) {
      body$system_instruction <- list(parts = list(list(text = system_prompt)))
    }

    res <- httr::POST(url, body = body, encode = "json", httr::timeout(30))

    if (httr::http_error(res)) {
      detail <- tryCatch(httr::content(res, as = "text", encoding = "UTF-8"), error = function(e) NULL)
      stop(paste("Błąd API", httr::status_code(res), detail))
    }

    parsed <- httr::content(res, as = "parsed", type = "application/json")
    candidate <- tryCatch(parsed$candidates[[1]]$content$parts[[1]]$text, error = function(e) NULL)

    if (is.null(candidate) || identical(candidate, character(0))) {
      stop("Brak odpowiedzi od Gemini.")
    }

    candidate
  }

  observeEvent(input$send_ai_message, {
    msg <- trimws(input$ai_user_message)
    if (is.null(msg) || !nzchar(msg)) {
      showNotification("Wpisz wiadomość przed wysłaniem do Gemini.", type = "warning")
      return()
    }

    api_key <- trimws(input$gemini_api_key)
    if (is.null(api_key) || !nzchar(api_key)) {
      showNotification("Wprowadź klucz API Gemini, aby wysłać wiadomość.", type = "warning")
      return()
    }

    current_history <- chat_history()
    new_history <- append(current_history, list(list(role = "user", text = msg)))
    chat_history(new_history)
    updateTextAreaInput(session, "ai_user_message", value = "")

    model_choice <- if (!is.null(input$gemini_model) && nzchar(input$gemini_model)) input$gemini_model else "gemini-1.5-flash"
    system_prompt <- if (is.null(input$ai_system_prompt)) "" else input$ai_system_prompt

    tryCatch({
      reply <- call_gemini_api(new_history, api_key, model_choice, system_prompt)
      chat_history(append(new_history, list(list(role = "model", text = reply))))
    }, error = function(e) {
      showNotification(paste("Gemini:", e$message), type = "error")
    })
  }, ignoreInit = TRUE)

  observeEvent(input$clear_ai_chat, {
    chat_history(list())
    updateTextAreaInput(session, "ai_user_message", value = "")
  }, ignoreInit = TRUE)

  observeEvent(input$insert_last_metrics, {
    res <- tryCatch(test_res(), error = function(e) NULL)
    if (is.null(res)) {
      showNotification("Brak obliczonych metryk – uruchom model, aby wstawić podsumowanie.", type = "warning")
      return()
    }

    m <- res$metrics
    summary_line <- paste0(
      "Accuracy: ", round(m$acc * 100, 2), "%, ",
      "Precision: ", round(m$prec * 100, 2), "%, ",
      "Recall: ", round(m$rec * 100, 2), "%, ",
      "F1: ", round(m$f1 * 100, 2), "%"
    )

    conf_text <- tryCatch(
      paste(capture.output(print(as.data.frame.matrix(res$conf))), collapse = "\n"),
      error = function(e) NULL
    )

    snippet <- paste(
      "Podsumowanie jakości modelu:",
      summary_line,
      if (!is.null(conf_text)) paste0("Macierz konfuzji:\n", conf_text) else NULL,
      sep = "\n"
    )

    current <- input$ai_user_message
    if (!is.null(current) && nzchar(trimws(current))) {
      snippet <- paste(snippet, "\n\n", current)
    }

    updateTextAreaInput(session, "ai_user_message", value = snippet)
  }, ignoreInit = TRUE)

  fmt_pct <- function(x, digits = 2) {
    if (is.null(x) || is.na(x)) return("–")
    paste0(round(x * 100, digits), "%")
  }
  
  fmt_num <- function(x, digits = 3) {
    if (is.null(x) || is.na(x)) return("–")
    round(x, digits)
  }
  
  make_ft <- function(df) {
    flextable(df) |>
      autofit() |>
      theme_vanilla() |>
      fontsize(size = 10, part = "all") |>
      align(align = "center", part = "all")
  }
  
  generate_report_docx <- function(
    path,
    input,
    dane,
    test_res,
    var_imp_df
  ) {
    
    df <- dane()
    res <- test_res()
    imp <- var_imp_df()
    
    conf <- res$conf
    m <- res$metrics
    
    # --- metadane ---
    target <- input$target
    model  <- input$model_type
    preds  <- input$predictors
    n_obs  <- nrow(df)
    n_pred <- length(preds)
    
    train_pct <- round(input$train_prop * 100)
    test_pct  <- 100 - train_pct
    
    # --- macierz konfuzji ---
    conf_df <- as.data.frame.matrix(conf)
    
    # --- ważność predyktorów ---
    imp_tbl <- head(imp, 10)
    
    top3 <- paste(head(imp$Zmienna, 3), collapse = ", ")
    
    # --- dokument ---
    doc <- read_docx()
    
    # =====================================================
    # TYTUŁ
    # =====================================================
    doc <- doc |>
      body_add_par("RAPORT Z ANALIZY KLASYFIKACYJNEJ", style = "heading 1") |>
      body_add_par("ML Studio – Healthcare Decision Classifier", style = "Normal") |>
      body_add_par(format(Sys.Date()), style = "Normal")
    
    # =====================================================
    # 1. CEL ANALIZY
    # =====================================================
    doc <- doc |>
      body_add_par("1. Cel analizy", style = "heading 2") |>
      body_add_par(
        paste0(
          "Celem analizy było zbudowanie interpretowalnego modelu decyzyjnego ",
          "klasyfikującego obserwacje do klas zmiennej wynikowej „", target,
          "” na podstawie wybranych predyktorów. ",
          "Analiza miała na celu ocenę skuteczności klasyfikacji ",
          "oraz identyfikację czynników najsilniej różnicujących klasy."
        ),
        style = "Normal"
      )
    
    # =====================================================
    # 2. PRZYGOTOWANIE DANYCH
    # =====================================================
    doc <- doc |>
      body_add_par("2. Przygotowanie danych", style = "heading 2") |>
      body_add_par(
        paste0(
          "Do analizy wykorzystano zbiór danych zawierający ", n_obs,
          " obserwacji oraz ", n_pred, " predyktorów. ",
          "Dane zostały wczytane z pliku CSV i poddane wstępnemu przetwarzaniu. ",
          "Zmienne tekstowe przekonwertowano do postaci kategorycznej, ",
          "a zmienne binarne ujednolicono do formatu „Tak / Nie”. ",
          "Obserwacje zawierające braki danych w zestawie modelowym ",
          "zostały usunięte (na.omit). ",
          "Zbiór danych podzielono losowo na część treningową (",
          train_pct, "%) oraz testową (", test_pct, "%)."
        ),
        style = "Normal"
      )
    
    # =====================================================
    # 3. ZASTOSOWANE METODY
    # =====================================================
    doc <- doc |>
      body_add_par("3. Zastosowane metody", style = "heading 2") |>
      body_add_par(
        paste0(
          "Do klasyfikacji wykorzystano model typu ", model,
          ", należący do metod uczenia nadzorowanego. ",
          "Model uczony był na zbiorze treningowym, ",
          "a jego skuteczność oceniano na zbiorze testowym. ",
          "Jako miary jakości wykorzystano accuracy, precision, recall ",
          "oraz wskaźnik F1-score. ",
          "Dodatkowo przeanalizowano macierz konfuzji ",
          "oraz ranking ważności predyktorów."
        ),
        style = "Normal"
      )
    
    # =====================================================
    # 4. WYNIKI
    # =====================================================
    doc <- doc |>
      body_add_par("4. Wyniki", style = "heading 2")
    
    # --- 4.1 Metryki ---
    doc <- doc |>
      body_add_par("4.1 Skuteczność modelu", style = "heading 3") |>
      body_add_par(
        paste0(
          "Model osiągnął dokładność klasyfikacji na poziomie ",
          fmt_pct(m$acc), ". ",
          "Średnia precyzja wyniosła ", fmt_pct(m$prec),
          ", czułość ", fmt_pct(m$rec),
          ", a wartość wskaźnika F1-score ", fmt_pct(m$f1), "."
        ),
        style = "Normal"
      )
    
    # --- tabela metryk ---
    metrics_tbl <- data.frame(
      Metryka = c("Accuracy", "Precision", "Recall", "F1-score"),
      Wartość = c(
        fmt_pct(m$acc),
        fmt_pct(m$prec),
        fmt_pct(m$rec),
        fmt_pct(m$f1)
      )
    )
    
    doc <- doc |>
      body_add_flextable(make_ft(metrics_tbl))
    
    # --- 4.2 Macierz konfuzji ---
    doc <- doc |>
      body_add_par("4.2 Macierz konfuzji", style = "heading 3") |>
      body_add_par(
        "Macierz konfuzji przedstawia liczbę poprawnych i błędnych klasyfikacji "
        ,
        style = "Normal"
      ) |>
      body_add_flextable(make_ft(conf_df))
    
    # --- 4.3 Ważność predyktorów ---
    doc <- doc |>
      body_add_par("4.3 Ważność predyktorów", style = "heading 3") |>
      body_add_par(
        paste0(
          "Największy wpływ na decyzje modelu miały zmienne: ",
          top3, "."
        ),
        style = "Normal"
      ) |>
      body_add_flextable(make_ft(imp_tbl))
    
    # =====================================================
    # 5. WNIOSKI
    # =====================================================
    doc <- doc |>
      body_add_par("5. Wnioski", style = "heading 2") |>
      body_add_par(
        paste0(
          "Uzyskane wyniki potwierdzają możliwość skutecznej klasyfikacji ",
          "obserwacji z wykorzystaniem interpretowalnych modeli decyzyjnych. ",
          "Model charakteryzuje się dobrą skutecznością predykcji oraz ",
          "czytelną strukturą decyzyjną, co zwiększa jego użyteczność ",
          "w analizach wspierających podejmowanie decyzji."
        ),
        style = "Normal"
      )
    
    
    print(doc, target = path)
  }
  
  output$download_report_docx <- downloadHandler(
    filename = function() {
      paste0("Raport_ML_", Sys.Date(), ".docx")
    },
    content = function(file) {
      generate_report_docx(
        path       = file,
        input      = input,
        dane       = dane,
        test_res   = test_res,
        var_imp_df = var_imp_df
      )
    }
  )
  
  
  
}

# 5. Start ------------------------------------------------------------
shinyApp(ui = ui, server = server)
