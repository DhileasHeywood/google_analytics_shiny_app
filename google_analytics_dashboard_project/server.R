server <- function(input, output) {
    
    
    
    # Reactive data for tab 1 plot 1
    
    date_filtered_keyword_syn <- reactive({
        
        keyword_syn %>% 
            filter(date >= input$date_range[1], date <= input$date_range[2])
        
    })
    
    # Reactive data for tab 1 plots 2, 4
    date_filtered_arrival_syn <- reactive({
        
        clean_arrival_syn %>% 
            filter(date >= input$date_range[1], date <= input$date_range[2])
    }) 
    
    
    # Reactive data for tab 1 plot 3
    
    date_filtered_ad_syn <- reactive({
        
        ad_syn %>% 
            filter(date >= input$date_range[1], date <= input$date_range[2])
        
    })
    
    
    # tab 1, plot 1
    output$keyword_plot <- renderPlotly({
        
        plot1 <- date_filtered_keyword_syn() %>%  
            filter(str_detect(keyword, fixed(input$keyword))) %>% 
            group_by(keyword) %>% 
            summarise(count = n()) %>% 
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
        
        plot2 <- date_filtered_arrival_syn() %>% 
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
        
        plot3 <- date_filtered_ad_syn() %>% 
            filter(str_detect(adContent, fixed(input$advert))) %>% 
            group_by(adContent) %>% 
            summarise(count = n()) %>% 
            arrange(desc(count)) %>% 
            head(10) %>% 
            mutate(adContent = fct_reorder(adContent, desc(count))) %>% 
            ggplot() +
            geom_col(aes(x = adContent, y = count)) +
            labs(x = "Content of Advert",
                 y = "Number of Clicks"
            ) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        ggplotly(plot3)
        
    })
    
    # tab 1, plot 4
    output$social_media_plot <- renderPlotly({
        plot4 <- date_filtered_arrival_syn() %>% 
            dplyr::filter(str_detect(source, "^(twitter|linkedin|youtube|yammer|reddit|quora|instagram|facebook|glassdoor).*")) %>% 
            group_by(source) %>% 
            summarise(number = n()) %>%
            mutate(source = fct_reorder(source, desc(number))) %>%
            dplyr::filter(number > 1) %>% 
            ggplot() +
            geom_col(aes(x = source, y = number)) +
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
    
    
    
    # Reactive data for tab 3 plots 1, 2, 3
    date_filtered_sessions_and_exits_syn <- reactive({
        
        sessions_and_exits_syn %>% 
            filter(date >= input$date_range[1], date <= input$date_range[2])
    }) 
    
    
    
    
    # --  
    # tab 3 plot 1
    # --
    output$exit_rates_plot_1 <- renderPlotly({ 
        
        tab_3_plot1 <- date_filtered_sessions_and_exits_syn()  %>% 
            group_by(exit_page_path) %>% 
            summarise(exits = sum(exits), exit_rate = mean(exit_rate)) %>% 
            filter(exits > 1000) %>% 
            mutate(exit_page_path = fct_reorder(exit_page_path, desc(exit_rate))) %>% 
            ggplot(aes(x = exit_page_path, y = exit_rate)) +
            geom_col() +
            theme(axis.text.x = element_blank()) +
            xlab("Exit Page") +
            ylab("Exit Rate") 
        
        ggplotly(tab_3_plot1)
        
    })
    
    
    # tab 3 plot 1 printer friendly
    output$exit_rates_plot_1_1  <- renderPlot({
        
        date_filtered_sessions_and_exits_syn()  %>% 
            group_by(exit_page_path) %>% 
            summarise(exits = sum(exits), exit_rate = mean(exit_rate)) %>% 
            filter(exits > 1000) %>% 
            mutate(exit_page_path = fct_reorder(exit_page_path, desc(exit_rate))) %>% 
            ggplot(aes(x = exit_page_path, y = exit_rate)) +
            geom_col() +
            geom_text(aes(label = exit_page_path, y = 0), angle = 90, hjust = 0) +
            theme(axis.text.x = element_blank()) +
            xlab("Exit Page") +
            ylab("Exit Rate") 
        
    })
    
    
    
    
    # --  
    # tab 3 plot 2
    # --  
    output$page_depth_plot_1 <- renderPlotly({ 
        
        tab_3_plot2 <- date_filtered_sessions_and_exits_syn()  %>% 
            group_by(exit_page_path, page_depth) %>% 
            summarise(exits = sum(exits), exit_rate = mean(exit_rate)) %>% 
            ungroup() %>%
            filter(exits > 600) %>%
            filter(!is.na(page_depth)) %>% 
            mutate(page_depth = factor(page_depth, levels = seq(from = max(page_depth), to = 1, by = -1)),
                   exit_page_path = fct_reorder(exit_page_path, desc(exits), .fun = sum)) %>% 
            ggplot(aes(x = exit_page_path, y = exits, fill = page_depth)) +
            geom_col() +
            theme(axis.text.x = element_blank()) +
            xlab("Exit Page") +
            ylab("Exits") +
            scale_fill_viridis_d()
        
        ggplotly(tab_3_plot2)
        
    })
    
    
    # tab 3 plot 2 printer friendly
    output$page_depth_plot_1_1  <- renderPlot({
        
        date_filtered_sessions_and_exits_syn()  %>% 
            group_by(exit_page_path, page_depth) %>% 
            summarise(exits = sum(exits), exit_rate = mean(exit_rate)) %>% 
            ungroup() %>% 
            filter(exits > 600) %>%
            filter(!is.na(page_depth)) %>% 
            mutate(page_depth = factor(page_depth, levels = seq(from = max(page_depth), to = 1, by = -1)),
                   exit_page_path = fct_reorder(exit_page_path, desc(exits), .fun = sum)) %>%
            ggplot(aes(x = exit_page_path, y = exits, fill = page_depth)) +
            geom_col() +
            geom_text(aes(label = exit_page_path, y = 0), angle = 90, hjust = 0, colour = "#3b3b3b") +
            theme(axis.text.x = element_blank()) +
            xlab("Exit Page") +
            ylab("Exits") +
            scale_fill_viridis_d()
        
    })
    
    
    # --  
    # tab 3 plot 3
    # --  
    output$sessions_and_exits_plot_1 <- renderPlotly({ 
        
        tab_3_plot3 <- date_filtered_sessions_and_exits_syn()  %>% 
            group_by(exit_page_path) %>% 
            summarise(avg_session_duration = mean(avg_session_duration)) %>% 
            arrange(desc(avg_session_duration)) %>% 
            mutate(exit_page_path = fct_reorder(exit_page_path, desc(avg_session_duration))) %>%
            ggplot() +
            aes(x = exit_page_path , y = avg_session_duration) +
            geom_col() +
            theme(axis.text.x = element_blank()) +
            xlab("Exit Page") +
            ylab("Time in Seconds")
        
        ggplotly(tab_3_plot3)
        
    }) 
    
    # tab 3 plot 3 printer friendly
    output$sessions_and_exits_plot_1_1  <- renderPlot({
        
        date_filtered_sessions_and_exits_syn()  %>% 
            group_by(exit_page_path) %>% 
            summarise(avg_session_duration = mean(avg_session_duration)) %>% 
            arrange(desc(avg_session_duration)) %>% 
            mutate(exit_page_path = fct_reorder(exit_page_path, desc(avg_session_duration))) %>%
            ggplot(aes(x = exit_page_path , y = avg_session_duration)) +
            geom_col() +
            geom_text(aes(label = exit_page_path, y = 0), angle = 90, hjust = 0) +
            theme(axis.text.x = element_blank()) +
            xlab("Exit Page") +
            ylab("Time in Seconds")
        
    })
    
    
}
