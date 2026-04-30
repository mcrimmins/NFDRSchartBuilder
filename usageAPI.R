# ==========================================
# FEMS App Usage & Telemetry Master Script
# ==========================================

library(connectapi)
library(dplyr)
library(lubridate)
library(ggplot2)

# 1. AUTHENTICATE & FETCH DATA
# ------------------------------------------
client <- connect()
app_guid <- Sys.getenv("FEMS_DEV_APP_GUID") # Replace with your app's GUID

print("Fetching raw usage data from Posit Connect...")
all_raw_usage <- get_usage_shiny(
  client,
  content_guid = app_guid,
  from = as.Date("2023-01-01"), # Go far back to catch all retained data
  limit = Inf 
)

server_users <- get_users(client) %>% select(guid, username)

# 2. DATA ENGINEERING (The "Master" Table)
# ------------------------------------------
master_usage <- all_raw_usage %>%
  left_join(server_users, by = c("user_guid" = "guid")) %>%
  mutate(
    # Handle missing usernames (users who were deleted or anonymous)
    username = ifelse(is.na(username), "anonymous_or_deleted", username),
    
    # Calculate durations and time periods
    session_length_mins = as.numeric(difftime(ended, started, units = "mins")),
    
    # THE FIX: Added tidyr:: right here
    session_length_mins = tidyr::replace_na(session_length_mins, 0), 
    
    date = as.Date(started),
    week_start = floor_date(date, "week"),
    month_start = floor_date(date, "month"),
    hour_of_day = hour(started),
    day_of_week = wday(started, label = TRUE)
  ) %>%
  # Filter out impossibly long sessions (someone left their laptop open for a week)
  filter(session_length_mins < 1440) 

print("Data engineering complete. Generating insights...")


# 3. INSIGHT 1: Overall Traffic Trends & Spikes (Daily)
# ------------------------------------------
daily_traffic <- master_usage %>%
  group_by(date) %>%
  summarize(
    total_sessions = n(),
    unique_users = n_distinct(username),
    .groups = "drop"
  )

# Plot the Trend Line
traffic_plot <- ggplot(daily_traffic, aes(x = date, y = total_sessions)) +
  geom_line(color = "firebrick", size = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "blue", linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "App Usage Spikes Over Time",
    subtitle = "Blue dashed line indicates overall trend",
    x = "Date",
    y = "Total Daily Sessions"
  )
print(traffic_plot)


# 4. INSIGHT 2: User Base Growth (Weekly Active Users)
# ------------------------------------------
# Is the tool spreading to new people, or is it the same group?
weekly_growth <- master_usage %>%
  group_by(week_start) %>%
  summarize(
    weekly_active_users = n_distinct(username), # <-- THE FIX: Removed the " (WAU)"
    total_time_spent_hrs = sum(session_length_mins) / 60,
    .groups = "drop"
  )

print("--- Weekly Active Users (WAU) ---")
print(tail(weekly_growth, 5)) # Show the last 5 weeks


# 5. INSIGHT 3: Operational Routine (Heatmap of Usage)
# ------------------------------------------
# When are they actually looking at the dashboard?
routine_summary <- master_usage %>%
  group_by(day_of_week, hour_of_day) %>%
  summarize(total_logins = n(), .groups = "drop")

routine_plot <- ggplot(routine_summary, aes(x = hour_of_day, y = day_of_week, fill = total_logins)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkorange") +
  theme_minimal() +
  labs(
    title = "Operational Heatmap: When is the app used?",
    x = "Hour of Day (24hr clock)",
    y = "Day of the Week",
    fill = "Total Logins"
  )
print(routine_plot)


# 6. INSIGHT 4: Engagement Depth (The "Bounce Rate")
# ------------------------------------------
# Are they actually using it, or opening and closing it immediately?
engagement_summary <- master_usage %>%
  mutate(
    engagement_level = case_when(
      session_length_mins < 1 ~ "Bounce (<1 min)",
      session_length_mins >= 1 & session_length_mins < 10 ~ "Quick Briefing (1-10 mins)",
      session_length_mins >= 10 ~ "Deep Analysis (10+ mins)"
    )
  ) %>%
  count(engagement_level) %>%
  mutate(percentage = round((n / sum(n)) * 100, 1))

print("--- User Engagement Depth ---")
print(engagement_summary)


# 7. INSIGHT 5: Top 10 Power Users
# ------------------------------------------
power_users <- master_usage %>%
  group_by(username) %>%
  summarize(
    total_sessions = n(),
    avg_session_mins = round(mean(session_length_mins), 1),
    first_seen = min(date),
    last_seen = max(date),
    .groups = "drop"
  ) %>%
  arrange(desc(total_sessions)) %>%
  head(10)

print("--- Top 10 Power Users ---")
print(power_users)