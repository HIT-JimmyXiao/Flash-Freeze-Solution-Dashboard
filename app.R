# ============================================================================
# Flash-Freeze Solution Dashboard
# 闪冻解决方案仪表板
# Version: 1.0
# Last Updated: 2025-10-31
# ============================================================================

# ============================================================================
# 1. Load Required Packages
# ============================================================================
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(DT)
library(plotly)
library(ggplot2)
library(lubridate)
library(shinyWidgets)
library(scales)

# ============================================================================
# 2. Data Loading and Processing Functions
# ============================================================================

# Safe CSV reading function
read_csv_safely <- function(path) {
  tryCatch({
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  }, error = function(e) {
    stop("无法加载数据文件: ", path, "\n错误: ", e$message)
  })
}

# Data processing function
load_and_process_data <- function() {
  cat("正在加载 Flash-Freeze 数据...\n")
  
  # 读取CSV
  raw_data <- read_csv_safely("flash_freeze_data.csv")
  
  # 按表类型分离
  regions <- raw_data %>% 
    filter(table_type == "region") %>%
    select(region_id, region_name, avg_distance_km, farmers_count, facilities_count)
  
  farmers <- raw_data %>% 
    filter(table_type == "farmer") %>%
    select(farmer_id, farmer_name, region_id, contact)
  
  facilities <- raw_data %>% 
    filter(table_type == "facility") %>%
    select(facility_id, facility_name, region_id, capacity_per_day)
  
  inventory <- raw_data %>% 
    filter(table_type == "inventory") %>%
    select(inventory_id, farmer_id, fruit_type, surplus_kg, price_per_kg, created_at)
  
  batches <- raw_data %>% 
    filter(table_type == "batch") %>%
    select(batch_id, source_inventory_id, facility_id, status, qty_kg, start_at, last_update_at)
  
  products <- raw_data %>% 
    filter(table_type == "product") %>%
    select(product_id, batch_id, fruit_type, bag_size_kg, bags_available, price_per_bag, released_at)
  
  orders <- raw_data %>% 
    filter(table_type == "order") %>%
    select(order_id, product_id, bags_ordered, order_status, order_at, buyer_type)
  
  cat("数据加载完成: 
    区域: ", nrow(regions), "
    农户: ", nrow(farmers), "
    设施: ", nrow(facilities), "
    库存: ", nrow(inventory), "
    批次: ", nrow(batches), "
    成品: ", nrow(products), "
    订单: ", nrow(orders), "\n")
  
  list(
    regions = regions,
    farmers = farmers,
    facilities = facilities,
    inventory = inventory,
    batches = batches,
    products = products,
    orders = orders
  )
}

# ============================================================================
# 3. UI Definition
# ============================================================================

ui <- page_navbar(
  title = div(
    icon("snowflake"), 
    "Flash-Freeze Solution Dashboard",
    style = "font-weight: bold;"
  ),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#3498db",
    success = "#2ecc71",
    info = "#1abc9c",
    warning = "#f39c12",
    danger = "#e74c3c"
  ),
  
  # Custom CSS
  tags$head(
    tags$style(HTML("
      /* Smooth transitions */
      * {
        transition: all 0.3s ease;
      }
      
      /* Value boxes enhancement */
      .bslib-value-box {
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        border-radius: 10px;
        transition: transform 0.3s ease;
      }
      
      .bslib-value-box:hover {
        transform: translateY(-5px);
        box-shadow: 0 6px 12px rgba(0,0,0,0.15);
      }
      
      /* Card enhancement */
      .card {
        border-radius: 10px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.08);
        border: none;
      }
      
      /* Plotly container */
      .plotly {
        border-radius: 8px;
      }
      
      /* Sidebar */
      .bslib-sidebar-layout > .sidebar {
        background: linear-gradient(135deg, #ffffff 0%, #f8f9fa 100%);
        border-right: 2px solid #e9ecef;
      }
      
      /* Buttons */
      .btn-primary {
        border-radius: 20px;
        font-weight: 600;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      .btn-primary:hover {
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(0,0,0,0.15);
      }
      
      /* DataTables */
      table.dataTable thead {
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
        color: white;
      }
      
      table.dataTable tbody tr:hover {
        background-color: #f8f9fa !important;
      }
    "))
  ),
  
  # Global sidebar filters
  sidebar = sidebar(
    width = 280,
    
    h4(icon("filter"), " Global Filters", 
       style = "color: #2c3e50; font-weight: bold; border-bottom: 2px solid #3498db; padding-bottom: 10px;"),
    
    pickerInput(
      "filter_fruit",
      label = tagList(icon("apple-alt"), " Fruit Types"),
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        selectAllText = "Select all",
        deselectAllText = "Deselect all",
        noneSelectedText = "All fruits",
        selectedTextFormat = "count > 2",
        liveSearch = TRUE
      )
    ),
    
    pickerInput(
      "filter_region",
      label = tagList(icon("map-marker-alt"), " Regions"),
      choices = NULL,
      selected = NULL,
      multiple = TRUE,
      options = pickerOptions(
        actionsBox = TRUE,
        selectAllText = "Select all",
        deselectAllText = "Deselect all",
        noneSelectedText = "All regions",
        selectedTextFormat = "count > 2"
      )
    ),
    
    dateRangeInput(
      "filter_date",
      label = tagList(icon("calendar"), " Date Range"),
      start = Sys.Date() - 30,
      end = Sys.Date(),
      language = "en",
      format = "yyyy-mm-dd",
      separator = " to "
    ),
    
    hr(),
    
    actionButton(
      "refresh_data",
      label = tagList(icon("sync-alt"), " Refresh Data"),
      class = "btn-primary w-100",
      style = "margin-bottom: 10px;"
    ),
    
    tags$div(
      class = "alert alert-info",
      style = "font-size: 12px; padding: 8px; margin-top: 15px;",
      icon("info-circle"), " Real-time Monitoring",
      br(),
      textOutput("last_update_time", inline = TRUE)
    )
  ),
  
  # ===== Tab 1: Overview Dashboard =====
  nav_panel(
    title = tagList(icon("dashboard"), " Overview"),
    value = "overview",
    
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      
      value_box(
        title = "Total Farmer Inventory",
        value = textOutput("kpi_inventory_total"),
        showcase = icon("seedling"),
        theme = "success",
        showcase_layout = "left center"
      ),
      
      value_box(
        title = "Active Batches",
        value = textOutput("kpi_batches_active"),
        showcase = icon("truck"),
        theme = "info",
        showcase_layout = "left center"
      ),
      
      value_box(
        title = "Available Product Bags",
        value = textOutput("kpi_products_available"),
        showcase = icon("shopping-cart"),
        theme = "primary",
        showcase_layout = "left center"
      ),
      
      value_box(
        title = "Completed Orders",
        value = textOutput("kpi_orders_completed"),
        showcase = icon("check-circle"),
        theme = "warning",
        showcase_layout = "left center"
      )
    ),
    
    br(),
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
      card_header(
          icon("project-diagram"), " Supply Chain Pipeline Overview",
          class = "bg-primary text-white"
        ),
        card_body(
          plotlyOutput("overview_pipeline_flow", height = "350px")
        )
      ),
      
      card(
      card_header(
          icon("map"), " Regional Distribution Overview",
          class = "bg-success text-white"
        ),
        card_body(
          plotlyOutput("overview_region_distribution", height = "350px")
        )
      )
    )
  ),
  
  # ===== Tab 2: Farmer Inventory =====
  nav_panel(
    title = tagList(icon("warehouse"), " Farmer Inventory"),
    value = "inventory",
    
    card(
      card_header(
        icon("chart-bar"), " Farmer Fruit Inventory Analysis",
        class = "bg-success text-white"
      ),
      card_body(
        plotlyOutput("farmer_inventory_chart", height = "450px")
      )
    ),
    
    br(),
    
    card(
      card_header(
        icon("table"), " Inventory Details",
        class = "bg-info text-white"
      ),
      card_body(
        DTOutput("farmer_inventory_table")
      )
    )
  ),
  
  # ===== Tab 3: Freezing Pipeline =====
  nav_panel(
    title = tagList(icon("snowflake"), " Freezing Pipeline"),
    value = "pipeline",
    
    card(
      card_header(
        icon("tasks"), " Batch Status Distribution",
        class = "bg-primary text-white"
      ),
      card_body(
        plotlyOutput("pipeline_status_chart", height = "450px")
      )
    ),
    
    br(),
    
    card(
      card_header(
        icon("clipboard-list"), " Batch Details",
        class = "bg-info text-white"
      ),
      card_body(
        DTOutput("pipeline_batches_table")
      )
    )
  ),
  
  # ===== Tab 4: Products & Sales =====
  nav_panel(
    title = tagList(icon("box-open"), " Products & Sales"),
    value = "sales",
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
      card_header(
          icon("pie-chart"), " Available Products Distribution",
          class = "bg-success text-white"
        ),
        card_body(
          plotlyOutput("products_distribution_chart", height = "350px")
        )
      ),
      
      card(
      card_header(
          icon("chart-line"), " Order Status Summary",
          class = "bg-warning text-white"
        ),
        card_body(
          plotlyOutput("orders_status_chart", height = "350px")
        )
      )
    ),
    
    br(),
    
    layout_columns(
      col_widths = c(6, 6),
      
      card(
      card_header(
        icon("boxes"), " Product List",
          class = "bg-primary text-white"
        ),
        card_body(
          DTOutput("products_table")
        )
      ),
      
      card(
      card_header(
        icon("receipt"), " Order Details",
          class = "bg-info text-white"
        ),
        card_body(
          DTOutput("orders_table")
        )
      )
    )
  ),
  
  # ===== Tab 5: Data Management =====
  nav_panel(
    title = tagList(icon("database"), " Data Management"),
    value = "data",
    
    card(
      card_header(
        icon("chart-pie"), " Data Summary Overview",
        class = "bg-dark text-white"
      ),
      card_body(
        h4("Table Record Counts", style = "color: #2c3e50; margin-bottom: 20px;"),
        tableOutput("data_summary_table"),
        hr(),
        downloadButton(
          "download_all_data", 
          label = tagList(icon("download"), " Download Full Data"),
          class = "btn-success",
          style = "border-radius: 20px; padding: 10px 30px; font-weight: 600;"
        )
      )
    )
  ),
  
  # Footer
  nav_spacer(),
  nav_item(
    tags$div(
      style = "padding: 8px 15px; color: #7f8c8d; font-size: 12px;",
      icon("university"), " Lab Work 3.5 | Group 14 | Inspired by FAO Crops & Livestock Dashboard (v2.3, 2025) | Maintainer: bowenzhao917"
    )
  )
)

# ============================================================================
# 4. Server Logic
# ============================================================================

server <- function(input, output, session) {
  
  # ===== Load Data =====
  data <- load_and_process_data()
  
  # ===== Initialize Filter Options =====
  observe({
    updatePickerInput(
      session,
      "filter_fruit",
      choices = sort(unique(data$inventory$fruit_type))
    )
    
    updatePickerInput(
      session,
      "filter_region",
      choices = sort(unique(data$regions$region_name))
    )
  })
  
  # ===== Last Update Time =====
  output$last_update_time <- renderText({
    paste("Updated at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  })
  
  # ===== Reactive Data Filtering =====
  filtered_data <- reactive({
    req(data)
    
    inv <- data$inventory
    bat <- data$batches
    prod <- data$products
    ord <- data$orders
    
    # 水果过滤
    if (!is.null(input$filter_fruit) && length(input$filter_fruit) > 0) {
      inv <- inv %>% filter(fruit_type %in% input$filter_fruit)
      prod <- prod %>% filter(fruit_type %in% input$filter_fruit)
    }
    
    # 区域过滤
    if (!is.null(input$filter_region) && length(input$filter_region) > 0) {
      region_ids <- data$regions %>%
        filter(region_name %in% input$filter_region) %>%
        pull(region_id)
      
      farmer_ids <- data$farmers %>%
        filter(region_id %in% region_ids) %>%
        pull(farmer_id)
      
      inv <- inv %>% filter(farmer_id %in% farmer_ids)
    }
    
    # 日期过滤
    if (!is.null(input$filter_date)) {
      inv <- inv %>%
        filter(
          as.Date(created_at) >= input$filter_date[1],
          as.Date(created_at) <= input$filter_date[2]
        )
    }
    
    # 关联批次、成品、订单
    if (nrow(inv) > 0) {
      bat <- bat %>% filter(source_inventory_id %in% inv$inventory_id)
      
      if (nrow(bat) > 0) {
        prod <- prod %>% filter(batch_id %in% bat$batch_id)
        
        if (nrow(prod) > 0) {
          ord <- ord %>% filter(product_id %in% prod$product_id)
        }
      }
    }
    
    list(
      inventory = inv,
      batches = bat,
      products = prod,
      orders = ord
    )
  })
  
  # ===== KPI Cards =====
  output$kpi_inventory_total <- renderText({
    total <- sum(filtered_data()$inventory$surplus_kg, na.rm = TRUE)
    paste0(format(round(total), big.mark = ","), " kg")
  })
  
  output$kpi_batches_active <- renderText({
    active <- filtered_data()$batches %>%
      filter(status != "Ready for Sale") %>%
      nrow()
    format(active, big.mark = ",")
  })
  
  output$kpi_products_available <- renderText({
    total <- sum(filtered_data()$products$bags_available, na.rm = TRUE)
    format(round(total), big.mark = ",")
  })
  
  output$kpi_orders_completed <- renderText({
    completed <- filtered_data()$orders %>%
      filter(order_status == "Completed") %>%
      nrow()
    format(completed, big.mark = ",")
  })
  
  # ===== Overview: Pipeline Flow =====
  output$overview_pipeline_flow <- renderPlotly({
    df <- filtered_data()$batches %>%
      count(status) %>%
      mutate(status = factor(status, levels = c(
        "Awaiting Pickup", "In Transit", "At Freezing Facility",
        "Freezing in Progress", "Quality Check", "Ready for Sale"
      )))
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "暂无数据"))
    }
    
    p <- ggplot(df, aes(x = status, y = n, fill = status)) +
      geom_col(alpha = 0.8) +
      geom_text(aes(label = n), vjust = -0.5, fontface = "bold", size = 4) +
      scale_fill_brewer(palette = "Spectral") +
      labs(x = NULL, y = "Batch Count") +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1),
        legend.position = "none",
        plot.margin = margin(10, 10, 10, 10)
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, hovermode = "closest")
  })
  
  # ===== Overview: Region Distribution =====
  output$overview_region_distribution <- renderPlotly({
    df <- data$regions %>%
      select(region_name, farmers_count, facilities_count) %>%
      pivot_longer(
        cols = c(farmers_count, facilities_count),
        names_to = "type", 
        values_to = "count"
      ) %>%
      mutate(type = ifelse(type == "farmers_count", "农户", "设施"))
    
    p <- ggplot(df, aes(x = region_name, y = count, fill = type)) +
      geom_col(position = "dodge", alpha = 0.85) +
      geom_text(
        aes(label = count), 
        position = position_dodge(width = 0.9),
        vjust = -0.5, 
        size = 3.5,
        fontface = "bold"
      ) +
      scale_fill_manual(values = c("农户" = "#2ecc71", "设施" = "#3498db")) +
      labs(x = NULL, y = "Count", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "top"
      )
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  # ===== Farmer Inventory Chart =====
  output$farmer_inventory_chart <- renderPlotly({
    df <- filtered_data()$inventory %>%
      left_join(data$farmers %>% select(farmer_id, farmer_name), by = "farmer_id") %>%
      group_by(farmer_name, fruit_type) %>%
      summarise(total_kg = sum(surplus_kg, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_kg)) %>%
      head(30)
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "暂无数据"))
    }
    
    p <- ggplot(df, aes(x = reorder(farmer_name, total_kg), y = total_kg, fill = fruit_type)) +
      geom_col(alpha = 0.85) +
      coord_flip() +
      labs(x = NULL, y = "Inventory (kg)", fill = "Fruit Type") +
      scale_y_continuous(labels = comma) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = c("y", "fill")) %>%
      layout(hovermode = "y unified")
  })
  
  # ===== Farmer Inventory Table =====
  output$farmer_inventory_table <- renderDT({
    df <- filtered_data()$inventory %>%
      left_join(data$farmers %>% select(farmer_id, farmer_name, region_id), by = "farmer_id") %>%
      left_join(data$regions %>% select(region_id, region_name), by = "region_id") %>%
      select(farmer_name, region_name, fruit_type, surplus_kg, price_per_kg, created_at) %>%
      rename(
        "Farmer" = farmer_name,
        "Region" = region_name,
        "Fruit" = fruit_type,
        "Surplus (kg)" = surplus_kg,
        "Price ($/kg)" = price_per_kg,
        "Created At" = created_at
      )
    
    datatable(
      df,
      options = list(
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = "top",
      class = "display nowrap"
    )
  })
  
  # ===== Pipeline Status Chart =====
  output$pipeline_status_chart <- renderPlotly({
    df <- filtered_data()$batches %>%
      count(status) %>%
      mutate(status = factor(status, levels = c(
        "Awaiting Pickup", "In Transit", "At Freezing Facility",
        "Freezing in Progress", "Quality Check", "Ready for Sale"
      ))) %>%
      arrange(status)
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "暂无数据"))
    }
    
    colors <- c("#e74c3c", "#e67e22", "#f39c12", "#3498db", "#9b59b6", "#2ecc71")
    
    plot_ly(
      df,
      x = ~n,
      y = ~status,
      type = "bar",
      orientation = "h",
      marker = list(color = colors),
      text = ~paste0(n, " 批次"),
      textposition = "outside",
      hovertemplate = "<b>%{y}</b><br>数量: %{x}<extra></extra>"
    ) %>%
      layout(
        xaxis = list(title = "批次数量"),
        yaxis = list(title = ""),
        margin = list(l = 180),
        hovermode = "closest"
      )
  })
  
  # ===== Pipeline Batches Table =====
  output$pipeline_batches_table <- renderDT({
    df <- filtered_data()$batches %>%
      left_join(data$facilities %>% select(facility_id, facility_name), by = "facility_id") %>%
      left_join(
        filtered_data()$inventory %>% select(inventory_id, fruit_type, farmer_id),
        by = c("source_inventory_id" = "inventory_id")
      ) %>%
      left_join(data$farmers %>% select(farmer_id, farmer_name), by = "farmer_id") %>%
      select(batch_id, farmer_name, fruit_type, facility_name, status, qty_kg, start_at, last_update_at) %>%
      rename(
        "Batch ID" = batch_id,
        "Farmer" = farmer_name,
        "Fruit" = fruit_type,
        "Facility" = facility_name,
        "Status" = status,
        "Qty (kg)" = qty_kg,
        "Start Time" = start_at,
        "Last Update" = last_update_at
      )
    
    datatable(
      df,
      options = list(
        pageLength = 15,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      rownames = FALSE,
      filter = "top",
      class = "display nowrap"
    )
  })
  
  # ===== Products Distribution Chart =====
  output$products_distribution_chart <- renderPlotly({
    df <- filtered_data()$products %>%
      group_by(fruit_type) %>%
      summarise(total_bags = sum(bags_available, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(total_bags))
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No data"))
    }
    
    plot_ly(
      df,
      labels = ~fruit_type,
      values = ~total_bags,
      type = "pie",
      textinfo = "label+percent",
      hovertemplate = "<b>%{label}</b><br>袋数: %{value}<br>占比: %{percent}<extra></extra>",
      marker = list(
        colors = RColorBrewer::brewer.pal(max(3, nrow(df)), "Set3")
      )
    ) %>%
      layout(title = "Available Bags by Fruit Type")
  })
  
  # ===== Orders Status Chart =====
  output$orders_status_chart <- renderPlotly({
    df <- filtered_data()$orders %>%
      count(order_status) %>%
      arrange(desc(n))
    
    if (nrow(df) == 0) {
      return(plotly_empty() %>% layout(title = "No data"))
    }
    
    p <- ggplot(df, aes(x = reorder(order_status, n), y = n, fill = order_status)) +
      geom_col(alpha = 0.85) +
      geom_text(aes(label = n), hjust = -0.3, fontface = "bold", size = 4) +
      coord_flip() +
      scale_fill_brewer(palette = "Pastel1") +
      labs(x = NULL, y = "Order Count") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(showlegend = FALSE, hovermode = "closest")
  })
  
  # ===== Products Table =====
  output$products_table <- renderDT({
    df <- filtered_data()$products %>%
      select(product_id, fruit_type, bag_size_kg, bags_available, price_per_bag, released_at) %>%
      rename(
        "Product ID" = product_id,
        "Fruit" = fruit_type,
        "Bag Size (kg)" = bag_size_kg,
        "Available Bags" = bags_available,
        "Price ($)" = price_per_bag,
        "Released At" = released_at
      )
    
    datatable(
      df,
      options = list(
        pageLength = 10
      ),
      rownames = FALSE,
      filter = "top",
      class = "display nowrap"
    )
  })
  
  # ===== Orders Table =====
  output$orders_table <- renderDT({
    df <- filtered_data()$orders %>%
      left_join(
        filtered_data()$products %>% select(product_id, fruit_type, price_per_bag),
        by = "product_id"
      ) %>%
      mutate(total_amount = bags_ordered * price_per_bag) %>%
      select(order_id, fruit_type, bags_ordered, total_amount, order_status, buyer_type, order_at) %>%
      rename(
        "Order ID" = order_id,
        "Fruit" = fruit_type,
        "Bags Ordered" = bags_ordered,
        "Total Amount ($)" = total_amount,
        "Order Status" = order_status,
        "Buyer Type" = buyer_type,
        "Order Time" = order_at
      )
    
    datatable(
      df,
      options = list(
        pageLength = 10
      ),
      rownames = FALSE,
      filter = "top",
      class = "display nowrap"
    ) %>%
      formatCurrency("Total Amount ($)", currency = "$", digits = 2)
  })
  
  # ===== Data Summary Table =====
  output$data_summary_table <- renderTable({
    data.frame(
      "Table" = c("Regions", "Farmers", "Facilities", "Inventory", "Batches", "Products", "Orders"),
      "Rows" = c(
        nrow(data$regions),
        nrow(data$farmers),
        nrow(data$facilities),
        nrow(data$inventory),
        nrow(data$batches),
        nrow(data$products),
        nrow(data$orders)
      )
    )
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%")
  
  # ===== Download Data =====
  output$download_all_data <- downloadHandler(
    filename = function() {
      paste0("flash_freeze_data_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write_csv(read_csv_safely("flash_freeze_data.csv"), file)
    }
  )
  
  # ===== Refresh Data Button =====
  observeEvent(input$refresh_data, {
    showNotification(
      "Data refreshed!",
      type = "message",
      duration = 2,
      closeButton = TRUE
    )
  })
}

# ============================================================================
# 5. Run Application
# ============================================================================

shinyApp(ui = ui, server = server)
