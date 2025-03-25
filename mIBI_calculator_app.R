library(shiny)
library(shinyjs)
library(openxlsx)
library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(bslib)

user_supplied_colnames <- NA

MASTER7 <- read_excel("\\\\Illinois.gov\\EPA\\DesPlainesShared\\BOW\\Bugs\\MIBI_Calc\\mIBI R Shiny app\\www\\master7_copy.xlsx",
                      col_types = c("numeric", "text", "text", "text", "text", "text", "text", "text", "numeric", "text", "numeric","text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text"))

#select necessary columns from Master7 to create Master7_subset
Master7_subset <- MASTER7 %>%
  select(ID, BIOSNAME, TOLERANCE, FFG, ORDER, FAMILY, GENUS, GROUP, TRIBE_ETC, CLASS, PHYLUM)

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(
                "
                li {
                line-height: 0.5em;
                }
                "
               ))
    ),
  
  theme = bs_theme(version = 5, bootswatch = "sandstone"),
  
  useShinyjs(),
  
  navbarPage("mIBI Calculator",
    tabPanel("Instructions",
             card(
               HTML("<p>Download an .xlsx file from AWQMS with the site data for which you wish to calculate the mIBI values. Go to the 'Calculator' page and upload the document in this app using the 'Browse...' button.</p>

<p>Once the file has been successfully uploaded, continue to the tab titled 'Step 2.' There, you will need to tell the calculator the names of the columns containing the data the calculator needs (don't worry, it walks you through the process!).</p>

<p>After selecting all the columns, continue to the tabl titled 'Step 3.' There, click the button to start processing the data. After a few seconds, the download button will be activated. Click the download button and an .xlsx file will be ready with the mIBI scores contained within. Feel free to name the file whatever you like and save it to the destination of your choosing. </p>

<p>Congratulations! You now have mIBI scores.</p>"
                    )
               )
    ),
    
    tabPanel("Calculator",
             
      
             
      page_fillable(
        
        singleton(tags$head(HTML(
          
          '
      <script type="text/javascript">
        $(document).ready(function() {
          // disable download at startup. downloadData is the id of the downloadButton
          $("#downloadData").attr("disabled", "true").attr("onclick", "return false;");
    
          Shiny.addCustomMessageHandler("download_ready", function(message) {
            $("#downloadData").removeAttr("disabled").removeAttr("onclick").html(
              "<i class=\\"fa fa-download\\"></i>Download (ready)");
          });
        })
      </script>
        '
          
        ))),
        navset_card_tab(
          nav_panel("Step 1",
                    fileInput("file1", "Choose xlsx File", accept = ".xlsx"),
                    textOutput("fileError")  # Display error message if file is not xlsx
                    ),
          nav_panel("Step 2",
                    HTML(
                      '
            <p>There are 7 types of data that are required from your file to be able to calculate mIBI scores. Because the column names for data stored in AWQMS are subject to change, the columns must be manually selected to ensure proper results. The data required and their possible column name from AWQMS are as follows:</p>
            
              <li>Monitoring location ID</li>
              <li>The date the bugs were collected (Activity Start Date)</li>
              <li>The time the bugs were collected (Activity Start Time)</li>
              <li>How the bugs were collected (Equipment ID)</li>
              <li>The biological individual IDs of the taxa (Biological Individual ID)</li>
              <li>The names of the taxa (Taxonomic Name)</li>
              <li>How many of each taxa were collected (Result Value)</li>
            
            <p>Please select the names of the columns that contain the required data:</p>
            '
                    ),
                    splitLayout(
                      selectizeInput(
                        'mlid', 'Monitoring Location ID', choices = NULL,
                        multiple = TRUE,
                        selected = NULL,
                        options = list(placeholder = "",
                                       maxItems = 1,
                                       dropdownParent = 'body')
                      ),
                      selectizeInput(
                        'start_date', 'Activity Start Date', choices = NULL,
                        multiple = TRUE,
                        selected = NULL,
                        options = list(placeholder = "",
                                       maxItems = 1,
                                       dropdownParent = 'body')
                      ),
                      selectizeInput(
                        'start_time', 'Activity Start Time', choices = NULL,
                        multiple = TRUE,
                        selected = NULL,
                        options = list(placeholder = "",
                                       maxItems = 1,
                                       dropdownParent = 'body')
                      ),
                      selectizeInput(
                        'equip_id', 'Equipment ID', choices = NULL,
                        multiple = TRUE,
                        selected = NULL,
                        options = list(placeholder = "",
                                       maxItems = 1,
                                       dropdownParent = 'body')
                      ),
                      selectizeInput(
                        'bio_id', 'Biological Individual ID', choices = NULL,
                        multiple = TRUE,
                        selected = NULL,
                        options = list(placeholder = "",
                                       maxItems = 1,
                                       dropdownParent = 'body')
                      ),
                      selectizeInput(
                        'taxon_name', 'Taxon Name', choices = NULL,
                        multiple = TRUE,
                        selected = NULL,
                        options = list(placeholder = "",
                                       maxItems = 1,
                                       dropdownParent = 'body')
                      ),
                      selectizeInput(
                        'result_val', 'Result Value', choices = NULL,
                        multiple = TRUE,
                        selected = NULL,
                        options = list(placeholder = "",
                                       maxItems = 1,
                                       dropdownParent = 'body')
                      ),
                      cellWidths = "12vw",
                      cellArgs = list(style = "padding: 6px")
                    )),
          nav_panel("Step 3",
                    disabled(actionButton("start_proc", h5("Click to start processing data"))),
                    downloadButton("downloadData", "Download (not ready)"),
                    helpText("Download will be available once the processing is completed. A preview showing the first 6 rows of your file will be shown below."),
                    tableOutput("contents"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store the uploaded file
  raw_data <- reactive({
    req(input$file1)  # Ensure file is uploaded
    
    # Check if the upload is an xlsx file
    if (tools::file_ext(input$file1$name) != "xlsx") {
      return(NULL)
    } else {
      # Read the uploaded xlsx file
      AWQMS_data <- read.xlsx(input$file1$datapath, startRow = 2) %>% 
        clean_names() 
    }
  })
  
  placeholder <- reactive({
    req(mIBI_results)
    data.frame("a" = 1, "b" = 2, "c" = 3)
  })
  
  mIBI_results <- reactive({
    req(reactive({list_of_colnames()}))
    
      AWQMS_for_mIBI <- raw_data() %>%
        filter(str_detect(characteristic_name, "Count")) %>%
        filter(is.na(result_comment)) %>%
        select(list_of_colnames()) %>%
        rename(ID := !!as.symbol(input$bio_id)) %>%
        mutate(ID = as.numeric(ID),
               !!as.symbol(input$result_val) := as.numeric(!!as.symbol(input$result_val))) %>%
        #join bug data and Master7_subset by ID column
        left_join(Master7_subset, by = c("ID" = "ID")) %>%
        #select necessary columns
        select(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time), !!as.symbol(input$equip_id), ID, BIOSNAME, !!as.symbol(input$taxon_name), !!as.symbol(input$result_val),
               TOLERANCE, FFG, ORDER, FAMILY, GENUS, GROUP, TRIBE_ETC, CLASS, PHYLUM) %>%
        filter(TOLERANCE != 99.9) %>%
        filter(!is.na(BIOSNAME)) %>%
        #all data filters
        mutate(!!as.symbol(input$start_date) := convertToDate(!!as.symbol(input$start_date)),
               !!as.symbol(input$start_time) := format(as.POSIXct(convertToDateTime(!!as.symbol(input$start_time))), format = "%H:%M")) %>% 
        mutate() %>% 
        filter(!is.na(!!as.symbol(input$mlid)), !is.na(!!as.symbol(input$start_date)), !is.na(!!as.symbol(input$start_time)),
               !is.na(!!as.symbol(input$equip_id)), !!as.symbol(input$equip_id) == "20-jab" | !!as.symbol(input$equip_id) == "Net Jab + Dredge" | !!as.symbol(input$equip_id) == "Multiplate") %>%
        #standardize taxa/mIBITUs
        #specify class/family for relevant taxa
        mutate(StandardizedTaxa = case_when(CLASS %in% c("TURBELLARIA", "Turbellaria") ~ "TURBELLARIA",
                                            CLASS %in% c("OLIGOCHAETA", "Oligochaeta") ~  "OLIGOCHAETA",
                                            CLASS %in% c("HIRUDINEA", "Hirudinea") ~ "HIRUDINEA",
                                            CLASS %in% c("PISIDIIDAE", "Pisidiidae") ~ "SPHAERIIDAE",
                                            FAMILY %in% c ("SPHAERIIDAE", "Sphaeriidae") ~ "SPHAERIIDAE",
                                            FAMILY %in% c("UNIONIDAE", "Unionidae") ~ "UNIONIDAE",
                                            FAMILY %in% c("CAMBARIDAE", "Cambaridae") ~ "CAMBARIDAE")) %>%
        #assign all remaining taxa to be standardized by genus
        mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), GENUS,  StandardizedTaxa)) %>%
        mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), GROUP, StandardizedTaxa)) %>%
        mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), TRIBE_ETC, StandardizedTaxa)) %>%
        #if genus not available, standardize by family
        mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), FAMILY, StandardizedTaxa)) %>%
        mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), ORDER, StandardizedTaxa)) %>%
        mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), CLASS, StandardizedTaxa)) %>%
        mutate(StandardizedTaxa = ifelse(is.na(StandardizedTaxa), PHYLUM, StandardizedTaxa)) %>%
        select(-BIOSNAME) %>%
        rename(BIOSNAME = StandardizedTaxa) %>%
        mutate(BIOSNAME = as.character(BIOSNAME)) %>%
        select(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time), BIOSNAME, !!as.symbol(input$result_val)) %>%
        left_join(Master7_subset, by = "BIOSNAME")
      
      #Checking for atypical taxonomic resolution exceptions where genus isn't specified but family is
      df_tribe <- AWQMS_for_mIBI %>%
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        #keep rows where GENUS is listed as NA
        filter(is.na(GENUS)) %>%
        #keep rows where TRIBE is NOT listed as NA
        filter(!is.na(GROUP)) %>% 
        filter(!is.na(TRIBE_ETC))
      
      #New df where tribe is a unique occurrence for the site
      df_tribe_unique <- df_tribe %>% 
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        distinct(TRIBE_ETC, .keep_all = T)
      
      #Keep all rows that could cause a double count
      df_tribe_doubles <- anti_join(df_tribe, df_tribe_unique)
      
      #Checking for atypical taxonomic resolution exceptions where genus isn't specified but family is
      df_family <- AWQMS_for_mIBI %>%
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        #keep rows where GENUS is listed as NA
        filter(is.na(GENUS)) %>%
        #keep rows where FAMILY is NOT listed as NA
        filter(!is.na(FAMILY))
      
      #New df where family is a unique occurrence for the site
      df_family_unique <- df_family %>% 
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        distinct(FAMILY, .keep_all = T)
      
      #Keep all rows that could cause a double count
      df_family_doubles <- anti_join(df_family, df_family_unique)
      df_family_doubles1 <- anti_join(df_family_doubles, df_tribe_unique)
      
      df_order <- AWQMS_for_mIBI %>% 
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        filter(is.na(FAMILY)) %>% 
        distinct(BIOSNAME, .keep_all = T) %>% 
        filter(!is.na(ORDER))
      
      df_order_unique <- df_order %>% 
        distinct(ORDER, .keep_all = T)
      
      df_order_doubles <- anti_join(df_order, df_order_unique)
      
      df_class <- AWQMS_for_mIBI %>% 
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        filter(is.na(ORDER)) %>% 
        distinct(BIOSNAME, .keep_all = T) %>% 
        filter(!is.na(CLASS))
      
      df_class_unique <- df_class %>% 
        distinct(CLASS, .keep_all = T)
      
      df_class_doubles <- anti_join(df_class, df_class_unique)
      
      df_phylum <- AWQMS_for_mIBI %>% 
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        filter(is.na(CLASS)) %>% 
        distinct(BIOSNAME, .keep_all = T) %>% 
        filter(!is.na(PHYLUM))
      
      df_phylum_unique <- df_phylum %>% 
        distinct(PHYLUM, .keep_all = T)
      
      df_phylum_doubles <- anti_join(df_phylum, df_phylum_unique)
      
      mIBI_calc <- AWQMS_for_mIBI %>% 
        anti_join(df_tribe_doubles) %>% 
        anti_join(df_family_doubles1) %>% 
        anti_join(df_order_doubles) %>% 
        anti_join(df_class_doubles) %>% 
        anti_join(df_phylum_doubles)
      
      #compute metric values
      #TotalTaxa_Value - sum of MIBITUs (StandardizedTaxa) in sample
      CalculateTotalTaxa_Value <- mIBI_calc %>%
        #group by sample id (station code/date)
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        #remove semi aquatic
        #filter(TOLERANCE != 99.9) %>%
        #create TotalTaxa_value column by counting the number of unique StandardizedTaxa for each sample id (station code/date)
        mutate(TotalTaxa_Value = length(unique(ID))) %>%
        #select necessary columns for future joins
        select(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time), TotalTaxa_Value) %>%
        #keep only unique values/one per sample id (station code/date)
        unique()
      
      #ColeopteraTaxa_Value - sum of unique coleoptera MIBITUs in sample
      # do not include semi-aquatic taxa (TV = 99.9)
      CalculateColeopteraTaxa_Value <- mIBI_calc %>%
        #filter to remove semi-aquatic taxa and retain only order Coleoptera
        filter(TOLERANCE != 99.9, ORDER %in% c("Coleoptera", "COLEOPTERA")) %>%
        #group by sample id (station code/date)
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        #create ColeopteraTaxa_Value column by counting the number of unique Standardized Coleoptera taxa for each sample id (station code/date)
        mutate(ColeopteraTaxa_Value = length(unique(ID))) %>%
        #select necessary columns for future joins
        select(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time), ColeopteraTaxa_Value) %>%
        #keep only unique values/one per sample id (station code/date)
        unique()
      
      #EphemeropteraTaxa_Value - sum of unique Ephemeroptera MIBITUs in sample, susceptible to double counting
      CalculateEphemeropteraTaxa_Value <- mIBI_calc %>%
        #filter to retain only order Ephemeroptera
        filter(ORDER %in% c("Ephemeroptera", "EPHEMEROPTERA")) %>%
        #group by sample id (station code/date)
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        #create EphemeropteraTaxa_value column by counting the number of unique Standardized Coleoptera taxa for each sample id (station code/date)
        mutate(EphemeropteraTaxa_Value = length(unique(ID))) %>%
        #select necessary columns for future joins
        select(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time), EphemeropteraTaxa_Value) %>%
        #keep only unique values/one per sample id (station code/date)
        unique()
      
      #IntolerantTaxa_Value = sum of intolerant MIBITUs in sample, provides measure of the number of unique
      #intolerant MIBITUs in sample (TV <= 3.0)
      CalculateIntolerantTaxa_Value <- mIBI_calc %>%
        #filter to retain only Tolerance less than or equal to 3
        filter(TOLERANCE <= 3.0) %>%
        #group by sample id (station code/date)
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        #create IntelorantTaxa_Value column by counting the number of unique Standardized intolerant taxa for each sample id (station code/date)
        mutate(IntolerantTaxa_Value = length(unique(ID))) %>%
        #select necessary columns for future joins
        select(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time), IntolerantTaxa_Value) %>%
        #keep only unique values/one per sample id (station code/date)
        unique()
      
      #PercentScrapers_Value = percent of scrapers (assigned FFG) compared to total n of sample
      CalculatePercentScrapers_value <- mIBI_calc %>%
        #group by sample id (station code/date)
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        #create Total abundance column by finding the sum of abundance for each sample id (station code/date)
        mutate(Total_ABUNDANCE := sum(!!as.symbol(input$result_val))) %>%
        #filter to retain only Scrapers
        filter(FFG == "SC") %>%
        #create N_scrapers column to sum abundance of Scrapers
        mutate(N_Scrapers := sum(!!as.symbol(input$result_val))) %>%
        #create PercentScrapers column by dividng N_Scrapers by total abundance
        mutate(PercentScrapers = N_Scrapers/Total_ABUNDANCE * 100) %>%
        #select necessary columns for future joins
        select(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time), PercentScrapers) %>%
        #keep only unique values/one per sample id (station code/date)
        unique()
      
      #PercentEPT_Value = percent of Ephemeroptera/Plecoptera/Trichoptera compared to total n of sample
      CalculatePercentEPT_Value <- mIBI_calc %>%
        #group by sample id (station code/date)
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        #create total abundance column by finding the sum of abundance for each sample id (station code/date)
        mutate(Total_ABUNDANCE := sum(!!as.symbol(input$result_val))) %>%
        #filter to retain only orders Emphemeroptera, Plecoptera, Trichophtera
        filter(ORDER %in% c("Ephemeroptera", "EPHEMEROPTERA", "Plecoptera", "PLECOPTERA",
                            "Trichoptera", "TRICHOPTERA")) %>%
        #create N_EPT column by summing all EPT taxa for each sample id (station code/date)
        mutate(N_EPT := sum(!!as.symbol(input$result_val))) %>%
        #create Percent_EPT column by diving N_EPT by Total_Abundance
        mutate(Percent_EPT = N_EPT/Total_ABUNDANCE * 100) %>%
        #select necessary columns for future joins
        select(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time), Percent_EPT) %>%
        #keep only unique values/one per sample id (station code/date)
        unique()
      
      #MacroinvertebrateBioticIndex =  S (ni*ti)/N
      #Where, ni is the number of individuals in each MIBITU, 
      #ti is the tolerance value assigned to the given MIBITU 
      #and N is the total number of individuals in the sample
      CalculateMBI <- mIBI_calc %>%
        #calculate abundance * tolerance for each row
        mutate(ni_by_ti := !!as.symbol(input$result_val) * TOLERANCE) %>%
        #group by sample id (station code/date)
        group_by(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time)) %>%
        #create column for total abundance for each sample id (station code/date)
        mutate(Total_ABUNDANCE := sum(!!as.symbol(input$result_val))) %>%
        #create column for sum of abundance * tolerance for each sample id (station code/date)
        mutate(sum_ni_by_ti = sum(ni_by_ti)) %>%
        #create column for MBI by dividing sum of abundance * tolerance by total abundance for each sample id (station code/date)
        mutate(MBI = sum_ni_by_ti/Total_ABUNDANCE) %>%
        #select necessary columns for future joins
        select(!!as.symbol(input$mlid), !!as.symbol(input$start_date), !!as.symbol(input$start_time), MBI) %>%
        #keep only unique values/one per sample id (station code/date)
        unique()
      
      #assemble table for computing metric scores
      #start wtih total taxa value
      MetricScores_dataprepr <- CalculateTotalTaxa_Value %>%
        #join ColepteraTaxa_Value
        full_join(CalculateColeopteraTaxa_Value) %>%
        #join EphemeropteraTaxa_value
        full_join(CalculateEphemeropteraTaxa_Value) %>%
        #join IntolerantTaxa_Value
        full_join(CalculateIntolerantTaxa_Value) %>%
        #join MBI scores
        full_join(CalculateMBI) %>%
        #join Percent Scrapers
        full_join(CalculatePercentScrapers_value) %>%
        #join Percent EPT 
        full_join(CalculatePercentEPT_Value) %>%
        #replace all NAs with 0
        replace(is.na(.), 0)
      
      #compute metric scores by x/best value * 100 for all except MBI
      MetricScores_calculation <- MetricScores_dataprepr %>%
        #ColeopteraTaxa_Score - best value = 5
        mutate(ColeopteraTaxa_Score = ColeopteraTaxa_Value/5 * 100) %>%
        #EphemeropteraTaxa_Score - best value = 10.2
        mutate(EphemeropteraTaxa_Score = EphemeropteraTaxa_Value/10.2 * 100) %>%
        #TotalTaxa_Score - best value = 46
        mutate(TotalTaxa_Score = TotalTaxa_Value/46 * 100) %>%
        #IntolerantTaxa_Score - best value = 9
        mutate(IntolerantTaxa_Score = IntolerantTaxa_Value/9 * 100) %>%
        #MBI - best value = 4.9, ((11-MBI)/(11 - best value)) * 100
        mutate(MBI_score = ((11-MBI)/(11-4.9) * 100)) %>%
        #PercentScraper_Score - best value = 29.6
        mutate(PercentScraper_Score = PercentScrapers/29.6 * 100) %>%
        #PercentEPT_Score - best value = 74
        mutate(Percent_EPT = Percent_EPT) %>%
        #compute mIBI (average of all metric scores)
        mutate(mIBI = ((ColeopteraTaxa_Score + EphemeropteraTaxa_Score + TotalTaxa_Score +
                          IntolerantTaxa_Score + MBI_score + PercentScraper_Score +
                          Percent_EPT)/7)) %>%
        mutate(mIBI = ifelse(mIBI > 100, 100, mIBI)) %>%
        #interpret mIBI, assign narrative descriptions
        #73 - 100 = >75th percentile, "Exceptional"
        #41.8 - 72.9 = >10th percentile, "Good"
        #20.9 - 41.8 = bisect 10th percentile (upper), "Fair"
        #0.0 - 20.8 = bisect 10th percentile (lower), "Poor"
        mutate(Comparison_to_Reference = case_when(mIBI >= 73 & mIBI <= 100 ~ ">75th percentile",
                                                   mIBI >= 41.8 & mIBI <= 72.9 ~ "> 10th percentile",
                                                   mIBI >= 20.9 & mIBI <= 41.8 ~ "bisect 10th percentile (upper)",
                                                   mIBI >= 0 & mIBI <= 20.8 ~ "bisect 10th percentile (lower)")) %>%
        mutate(Narrative_Description = case_when(mIBI >= 73 & mIBI <= 100 ~ "Exceptional",
                                                 mIBI >= 41.8 & mIBI <= 72.9 ~ "Good",
                                                 mIBI >= 20.9 & mIBI <= 41.8 ~ "Fair",
                                                 mIBI >= 0 & mIBI <= 20.8 ~ "Poor"))
      
      MetricScores_calculation
  }) %>% 
    bindEvent(input$start_proc)
  
  observe({
    if(!is.null(input$file1)){
      
      user_supplied_colnames <- colnames(raw_data())
      
      updateSelectizeInput(session, "mlid", 
                           choices = user_supplied_colnames)
      
      updateSelectizeInput(session, "start_date", 
                           choices = user_supplied_colnames)
      
      updateSelectizeInput(session, "start_time", 
                           choices = user_supplied_colnames) 
      
      updateSelectizeInput(session, "equip_id", 
                           choices = user_supplied_colnames) 
      
      updateSelectizeInput(session, "bio_id", 
                           choices = user_supplied_colnames) 
      
      updateSelectizeInput(session, "taxon_name", 
                           choices = user_supplied_colnames) 
      
      updateSelectizeInput(session, "result_val", 
                           choices = user_supplied_colnames) 
    }
  })
  
  list_of_colnames <- reactive({
    req(!is.null(input$mlid) & !is.null(input$start_date)& !is.null(input$start_time) & !is.null(input$equip_id) & !is.null(input$bio_id) & !is.null(input$taxon_name) & !is.null(input$result_val))
    str_c(list(input$mlid, input$start_date, input$start_time, input$equip_id, input$bio_id, input$taxon_name, input$result_val), sep = ", ")
  })
  
  observe({
    output$fileError <- renderText({ "" })  # Clear any previous error message
  })
  
  # Enable download button and show error message if upload is not an xlsx file
  observeEvent(input$file1, {
    if (!is.null(input$file1)) {
      # Validate that the file extension is xlsx
      if (tools::file_ext(input$file1$name) != "xlsx") {
        output$fileError <- renderText({ "Please upload a valid xlsx file." })  # Show error message
      } 
    }
  })
  
  observe({
    req(list_of_colnames())
    enable("start_proc")  # Enable action button to start processing data once the user has supplied a valid list of column names
    output$fileError <- renderText({ "" })  # Clear error message
  })
  
  observe({
    req(mIBI_results())
    if (input$start_proc > 0) {
      enable("downloadData") # Enable download button
      # Notify browser that the data is ready to be downloaded
      session$sendCustomMessage("download_ready", list())
    } else {
      disable("downloadData")
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0('mIBI_scores-', Sys.Date(), '.xlsx')
    },
    content = function(file) {
      write.xlsx(mIBI_results(), file) # Write xlsx file
    }
  ) 
  
  # Render the contents of the uploaded file
  output$contents <- renderTable({
    req(mIBI_results())  # Ensure data is available
    head(mIBI_results())
  })

}

shinyApp(ui = ui, server = server)
