server <- function(input, output) {
    
    # tab 1, plot 1
    output$keyword_plot <- renderPlotly({
        
        plot1 <- keyword_syn %>%  
            filter(keyword %in% input$keyword) %>% 
            mutate(keyword = fct_reorder(keyword, desc(count))) %>% 
            ggplot() +
            geom_col(aes(x = keyword, y = count)) +
            labs(x = "Keyword",
                 y = "Number of Searches per keyword") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(plot1)
        
    })
    
    # tab 1, plot 2
    output$search_engine_plot <- renderPlotly({
        
        plot2 <- clean_arrival_syn %>% 
            dplyr::filter(str_detect(source, "^(google|bing|baidu|ecosia|duckduckgo|yahoo|zapmeta|yandex|izi).*")) %>% 
            group_by(source) %>% 
            summarise(n = n()) %>% 
            mutate(source = fct_reorder(source, desc(n))) %>%
            dplyr::filter(n > 1) %>% 
            ggplot() +
            geom_col(aes(x = source, y = n)) +
            labs(x = "Search Engine Name",
                 y = "Number of Searches"
            ) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(plot2)
        
    })
    
    # tab 1, plot 3
    output$advert_plot <- renderPlotly({
        
        plot3 <- ad_syn %>% 
            filter(ad_type %in% input$advert) %>% 
            mutate(ad_type = fct_reorder(ad_type, desc(count))) %>% 
            ggplot() +
            geom_col(aes(x = ad_type, y = count)) +
            labs(x = "Type of Advert",
                 y = "Number of Clicks"
            ) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(plot3)
        
    })
    
    # tab 1, plot 4
    output$social_media_plot <- renderPlotly({
        plot4 <- clean_arrival_syn %>% 
            dplyr::filter(str_detect(socialNetwork, "^(Twitter|LinkedIn|YouTube|Yammer|reddit|Quora|Instagram|Facebook|Glassdoor).*")) %>% 
            group_by(socialNetwork) %>% 
            summarise(number = n()) %>%
            mutate(social_network = fct_reorder(socialNetwork, desc(number))) %>%
            dplyr::filter(number > 1) %>% 
            ggplot() +
            geom_col(aes(x = social_network, y = number)) +
            labs(x = "Social Media Company",
                 y = "Number of Clicks"
            )
        
        ggplotly(plot4)
        
    })
    
    
    
    # Reactive data for tab 2 plots 1, 2, 3
    date_filtered_goal_completion_data <- reactive({
        
        goal_completion_syn %>% 
            filter(date >= input$date_range[1], date <= input$date_range[2])
    }) 
    
    
    # Reactive data for tab 2 plot 4
    
    codeclan_event_dataframe <- reactive({
        
        event_date_syn %>% 
            filter(date >= input$date_range[1], date <= input$date_range[2])
        
    })
    
    
    
    #tab 2 plot 1
    output$page_of_completion <- renderPlotly({
        
        goal_completion_page <- date_filtered_goal_completion_data() %>% 
            filter(step == "0") %>% 
            group_by(page) %>% 
            summarise(count = n()) %>% 
            arrange(desc(count)) %>% 
            head(10) %>% 
            mutate(page = fct_reorder(page, desc(count))) %>% 
            ggplot(aes(x = page, y = count)) +
            geom_col() +
            theme(axis.text.x = element_blank()) +
            xlab(label = "Page") +
            ylab(label = "Number who Booked Event from Page")
        
        ggplotly(goal_completion_page)
    })
    
    
    # tab 2 plot 1 printer friendly
    output$page_of_completion_gg <- renderPlot({
        
        data <- date_filtered_goal_completion_data()
        
        data %>% 
            filter(step == "0") %>% 
            group_by(page) %>% 
            summarise(count = n()) %>% 
            arrange(desc(count)) %>% 
            head(10) %>% 
            mutate(page = fct_reorder(page, desc(count))) %>% 
            ggplot(aes(x = page, y = count)) +
            geom_col() +
            geom_text(aes(label = page, y = 0), angle = 90, hjust = 0) +
            theme(axis.text.x = element_blank()) +
            xlab(label = "Page") +
            ylab(label = "Number who Booked Event from Page")
        
    })
    
    
    
    #tab 2 plot 2
    output$pages_visited <- renderPlotly({
        
        
        num_pages_visited <- date_filtered_goal_completion_data() %>% 
            select(`number of pages visited before making booking` = step,
                   page) %>% 
            group_by(`number of pages visited before making booking`) %>% 
            summarise(`number of users per step` = n()) %>% 
            ggplot(aes(x = `number of pages visited before making booking`, y = `number of users per step`)) +
            geom_col() +
            xlab("Pages Visited Before Making Booking") +
            ylab("Users per Step")
        
        
        ggplotly(num_pages_visited)
        
        
    })
    
    
    
    #tab 2 plot 3
    output$page_before_completion <- renderPlotly({
        
        first_page_before_goal_completed <- date_filtered_goal_completion_data() %>% 
            filter(step == "1") %>% 
            group_by(page) %>% 
            summarise(count = n()) %>% 
            arrange(desc(count)) %>% 
            mutate(page = fct_reorder(page, desc(count))) %>% 
            head(n = 10) %>% 
            ggplot(aes(x = page, y = count)) +
            geom_col() +
            theme(axis.text.x = element_blank()) +
            xlab(label = "Page") +
            ylab(label = "Number Who Visited Page")
        
        
        ggplotly(first_page_before_goal_completed)
        
    })
    
    
    # tab 2 plot 3 printer friendly
    output$page_before_completion_gg <- renderPlot({
        
        date_filtered_goal_completion_data() %>% 
            filter(step == "1") %>% 
            group_by(page) %>% 
            summarise(count = n()) %>% 
            arrange(desc(count)) %>% 
            mutate(page = fct_reorder(page, desc(count))) %>% 
            head(n = 10) %>% 
            ggplot(aes(x = page, y = count)) +
            geom_col() +
            geom_text(aes(label = page, y = 0), angle = 90, hjust = 0) +
            theme(axis.text.x = element_blank()) +
            xlab(label = "Page") +
            ylab(label = "Number who Visited Page")
        
    })
    
    
    # tab 2 plot 4. Timeseries of number of events booked per day. 
    output$event_timeseries <- renderPlotly({
        
        event_timeseries <- codeclan_event_dataframe() %>% 
            filter(step == 0) %>% 
            mutate(event = as.numeric(event)) %>% 
            group_by(date, event) %>% 
            summarise(goal_completion_count = n()) %>% 
            ggplot()+
            geom_line(aes(x = date, y = goal_completion_count), colour = "#555555") +
            geom_pointrange(aes(x = date, y = event, ymin = 0, ymax = max(goal_completion_count)), colour = "#4C768E", alpha = 0.4) +
            xlab(label = "Date") +
            ylab(label = "Number of Event Bookings")
        
        ggplotly(event_timeseries)
        
        
    })
    
    
    # --  
    # tab 3 plot 1
    # --
    output$exit_rates_plot_1 <- renderPlotly({ 
        
        tab_3_plot1 <- sessions_and_exits_syn  %>% 
            filter(exits > 1000) %>% 
            mutate(exitPagePath = fct_reorder(exitPagePath, desc(exitRate))) %>% 
            ggplot() +
            aes(x = exitPagePath, y = exitRate) +
            geom_col() +
            theme(axis.text.x = element_blank()) +
            xlab("Exit Page") +
            ylab("Exit Rate") 
        
        ggplotly(tab_3_plot1)
        
    })
    
    
    # tab 3 plot 1 printer friendly
    output$exit_rates_plot_1_1  <- renderPlot({
        
        sessions_and_exits_syn  %>% 
            filter(exits > 1000) %>% 
            mutate(exitPagePath = fct_reorder(exitPagePath, desc(exitRate))) %>% 
            ggplot() +
            aes(x = exitPagePath, y = exitRate) +
            geom_col() +
            geom_text(aes(label = exitPagePath, y = 0), angle = 90, hjust = 0) +
            theme(axis.text.x = element_blank()) +
            xlab("Exit Page") +
            ylab("Exit Rate") 
        
    })
    
    
    
    
    # --  
    # tab 3 plot 2
    # --  
    output$page_depth_plot_1 <- renderPlotly({ 
        
        tab_3_plot2 <- sessions_and_exits_syn  %>% 
            filter(exits > 600) %>%
            filter(!is.na(visits)) %>% 
            mutate(visits = factor(visits, levels = c("3", "2", "1")),
                   exitPagePath = fct_reorder(exitPagePath, desc(exits), .fun = sum)) %>% 
            ggplot(aes(x = exitPagePath, y = exits, fill = visits)) +
            geom_col() +
            theme(axis.text.x = element_blank()) +
            xlab("Exit Page") +
            ylab("Exits") +
            scale_fill_manual(values = c("1" = "#0C2533", "2" = "#073E5D", "3" = "#4C768E"))
        
        ggplotly(tab_3_plot2)
        
    })
    
    
    # tab 3 plot 2 printer friendly
    output$page_depth_plot_1_1  <- renderPlot({
        
        sessions_and_exits_syn  %>% 
            filter(exits > 600) %>%
            filter(!is.na(visits)) %>% 
            mutate(visits = factor(visits, levels = c("3", "2", "1")),
                   exitPagePath = fct_reorder(exitPagePath, desc(exits), .fun = sum)) %>%
            ggplot(aes(x = exitPagePath, y = exits, fill = visits)) +
            geom_col() +
            geom_text(aes(label = exitPagePath, y = 0), angle = 90, hjust = 0, colour = "#808080") +
            theme(axis.text.x = element_blank()) +
            xlab("Exit Page") +
            ylab("Exits") +
            scale_fill_manual(values = c("1" = "#0C2533", "2" = "#073E5D", "3" = "#4C768E"))  
        
    })
    
    
    # --  
    # tab 3 plot 3
    # --  
    output$sessions_and_exits_plot_1 <- renderPlotly({ 
        
        tab_3_plot3 <- sessions_and_exits_syn %>% 
            mutate(exitPagePath = fct_reorder(exitPagePath, desc(avgSessionDuration))) %>%
            ggplot() +
            aes(x = exitPagePath , y = avgSessionDuration) +
            geom_col() +
            theme(axis.text.x = element_blank()) +
            xlab("Exit Page") +
            ylab("Time in Seconds")
        
        ggplotly(tab_3_plot3)
        
    }) 
    
    # tab 3 plot 3 printer friendly
    output$sessions_and_exits_plot_1_1  <- renderPlot({
        
        sessions_and_exits_syn  %>% 
            mutate(exitPagePath = fct_reorder(exitPagePath, desc(avgSessionDuration))) %>%
            ggplot(aes(x = exitPagePath , y = avgSessionDuration)) +
            geom_col() +
            geom_text(aes(label = exitPagePath, y = 0), angle = 90, hjust = 0) +
            theme(axis.text.x = element_blank()) +
            xlab("Exit Page") +
            ylab("Time in Seconds")
        
    })
    
    
}
