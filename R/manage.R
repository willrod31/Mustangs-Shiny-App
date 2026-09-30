# The "Add & manage" tab for coaches and the admin.
#
# The tab is only inserted for staff, but every server action still starts
# with req(is_staff(user())) (or is_admin for logins and publishing), because
# inputs can be sent from the browser console by anyone who is logged in.

manage_ui <- function(admin = FALSE) {
  tabs <- list(
    nav_panel("Game files", value = "game_files", manage_game_files_ui()),
    nav_panel("Scouting library", value = "library", manage_library_ui()),
    nav_panel("TrackMan files", value = "trackman", manage_trackman_ui()),
    nav_panel("Schedule", value = "schedule", manage_schedule_ui())
  )
  if (admin) tabs <- c(tabs, list(nav_panel("Logins", value = "logins", manage_logins_ui())))
  tagList(
    uiOutput("publish_bar"),
    do.call(navset_card_underline, c(tabs, list(id = "admin_tabs")))
  )
}

manage_game_files_ui <- function() {
  layout_columns(
    col_widths = breakpoints(sm = c(12, 12), lg = c(5, 7)),
    div(
      selectInput("a_game", "Game", choices = NULL, width = "100%"),
      selectInput("a_type", "What is it", choices = REPORT_TYPES, width = "100%"),
      radioButtons("a_aud", "Who is it for", inline = TRUE, choices = c(
        "Whole team" = "team", "Coaches only" = "coaches", "One player" = "player"
      )),
      conditionalPanel(
        "input.a_aud == 'player'",
        selectizeInput("a_player", "Player", choices = NULL, width = "100%",
                       options = list(create = TRUE, placeholder = "Pick or type a name"))
      ),
      uiOutput("a_file_ui"),
      textInput("a_result", "Result", placeholder = "e.g. W 6-3", width = "100%"),
      actionButton("a_save", "Save", class = "btn-primary"),
      p(class = "text-muted small mt-2 mb-0",
        "Players see whole-team files and their own. Coaches see everything. A new box score or ",
        "pitcher, hitter or umpire report replaces the old one for the same audience.")
    ),
    div(
      h6("Already posted for this game"),
      DTOutput("a_files_table", height = "auto"),
      actionButton("a_file_del", "Delete selected file", class = "btn-outline-danger btn-sm mt-2")
    )
  )
}

manage_library_ui <- function() {
  layout_columns(
    col_widths = breakpoints(sm = c(12, 12), lg = c(7, 5)),
    div(
      textInput("up_title", "Title", width = "100%"),
      layout_column_wrap(
        width = "200px", fill = FALSE,
        selectInput("up_category", "Category", choices = REPORT_CATEGORIES),
        selectizeInput("up_opponent", "Opponent", choices = NULL,
                       options = list(create = TRUE, placeholder = "Pick or type a team")),
        textInput("up_player", "Player"),
        dateInput("up_date", "Date", value = Sys.Date())
      ),
      radioButtons("up_visibility", "Who can see it", inline = TRUE,
                   choices = c("Whole team" = "Team", "Coaches only" = "Coaches")),
      p(class = "text-muted small",
        "Naming a player makes a Whole team report visible only to that player and the coaches. ",
        "Leave Player blank for reports the whole team should see."),
      textAreaInput("up_notes", "Notes", rows = 3, width = "100%"),
      fileInput("up_file", "File (up to 50 MB)", width = "100%"),
      actionButton("up_btn", "Upload", class = "btn-primary")
    ),
    div(
      h6("Delete a report"),
      selectInput("del_id", "Report", choices = NULL, width = "100%"),
      actionButton("del_btn", "Delete selected report", class = "btn-outline-danger btn-sm")
    )
  )
}

manage_trackman_ui <- function() {
  layout_columns(
    col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
    div(
      fileInput("tmf_files", "TrackMan CSV files", multiple = TRUE, accept = ".csv", width = "100%"),
      actionButton("tmf_add", "Add to season", class = "btn-primary"),
      p(class = "text-muted small mt-2 mb-0",
        "Each file needs the columns ", paste(TM_REQUIRED, collapse = ", "),
        ". A file with the same name replaces the old one.")
    ),
    div(
      h6("Loaded files"),
      DTOutput("tmf_table", height = "auto"),
      actionButton("tmf_del", "Delete selected file", class = "btn-outline-danger btn-sm mt-2")
    )
  )
}

manage_schedule_ui <- function() {
  layout_columns(
    col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
    div(
      h6("Add a game"),
      dateInput("sg_date", "Date", value = Sys.Date(), width = "100%"),
      textInput("sg_opp", "Opponent", width = "100%"),
      radioButtons("sg_ha", NULL, inline = TRUE, choices = c("Home", "Away")),
      textInput("sg_time", "Time", placeholder = "7:00 PM", width = "100%"),
      textInput("sg_loc", "Location", width = "100%"),
      actionButton("sg_add", "Add game", class = "btn-primary"),
      hr(),
      h6("Replace the whole schedule"),
      p(class = "text-muted small",
        "A CSV with at least date, opponent and home_away columns (time, location and result optional)."),
      fileInput("sg_csv", NULL, accept = ".csv", width = "100%"),
      actionButton("sg_replace", "Replace schedule", class = "btn-outline-danger btn-sm")
    ),
    div(
      h6("Season"),
      p(class = "text-muted small", "Double-click a Time, Location or Result cell, type, then press Enter. Each edit saves right away."),
      DTOutput("sg_table", height = "auto"),
      actionButton("sg_remove", "Remove selected game", class = "btn-outline-danger btn-sm mt-2")
    )
  )
}

manage_logins_ui <- function() {
  layout_columns(
    col_widths = breakpoints(sm = c(12, 12), lg = c(4, 8)),
    div(
      textInput("u_username", "Username", width = "100%"),
      textInput("u_name", "Full name", width = "100%"),
      selectInput("u_role", "Role", choices = c("Player" = "player", "Coach" = "coach", "Admin" = "admin"),
                  width = "100%"),
      textInput("u_tm_name", "Name in TrackMan", placeholder = "e.g. Hollis, Jace", width = "100%"),
      passwordInput("u_pass", "Password (6+ characters)", width = "100%"),
      actionButton("u_save", "Save login", class = "btn-primary"),
      p(class = "text-muted small mt-2 mb-0",
        "Saving an existing username resets that person's password. Name in TrackMan links a ",
        "player to his stats and reports; leave it blank to use the full name.")
    ),
    div(
      h6("Logins"),
      DTOutput("u_table", height = "auto"),
      actionButton("u_remove", "Remove selected login", class = "btn-outline-danger btn-sm mt-2")
    )
  )
}

# "Jun 04 vs Wilson"
game_labels <- function(s) paste(format(s$date, "%b %d"), s$matchup)

# Compact selectable table for the manage tab
manage_table <- function(df, empty = "Nothing yet", ...) {
  datatable(
    df, rownames = FALSE, selection = "single", class = "compact hover",
    options = list(
      dom = if (nrow(df) > 10) "tp" else "t", pageLength = 10, scrollX = TRUE,
      ordering = FALSE, language = list(emptyTable = empty)
    ),
    ...
  )
}

# Server -----------------------------------------------------------------------
#
# sched, listing, tm_all and lib_all are the app's reactives. The *_bump
# reactiveVals force them to reload right after a change.
manage_server <- function(input, output, session, user, sched, sched_bump, listing,
                          files_bump, tm_all, tm_bump, lib_all, lib_bump) {

  bump <- function(rv) rv(rv() + 1)
  fail <- function(what, e) showNotification(paste0(what, ": ", conditionMessage(e)), type = "error")

  # A file input keeps its value on the server after it is used, so a second
  # click would save the same file again. upload() returns NULL for a file
  # that was already used; used() marks it.
  used_uploads <- character()
  upload <- function(id) {
    f <- input[[id]]
    if (is.null(f) || all(f$datapath %in% used_uploads)) NULL else f
  }
  used <- function(id) used_uploads <<- c(used_uploads, input[[id]]$datapath)

  # Publish bar ----------------------------------------------------------
  output$publish_bar <- renderUI({
    req(is_staff(user()))
    if (on_server()) {
      return(div(class = "alert alert-warning mb-3",
        strong("You're on the website. "),
        "Changes made here can be wiped when the site restarts. Add things on the laptop and publish from there."))
    }
    can_publish <- is_admin(user()) && requireNamespace("rsconnect", quietly = TRUE) &&
      isTRUE(tryCatch(nrow(rsconnect::accounts()) > 0, error = function(e) FALSE))
    div(
      class = "alert alert-info mb-3 d-flex flex-wrap align-items-center gap-2",
      span("Changes save to this computer. Publish when you're done so the website gets them."),
      if (can_publish) {
        actionButton("publish_btn", "Publish to website", class = "btn-primary btn-sm ms-auto")
      } else if (is_admin(user())) {
        span(class = "small ms-auto",
             "To publish from here, set up rsconnect once as described at the top of deploy.R.")
      }
    )
  })

  observeEvent(input$publish_btn, {
    req(is_admin(user()), !on_server())
    id <- showNotification("Publishing... this can take a few minutes.", duration = NULL, type = "message")
    res <- tryCatch({
      source("deploy.R", local = new.env())
      TRUE
    }, error = function(e) e)
    removeNotification(id)
    if (isTRUE(res)) {
      showNotification("Published. The website has your changes.", type = "message", duration = 8)
    } else {
      showNotification(paste("Publish failed:", conditionMessage(res)), type = "error", duration = NULL)
    }
  })

  # Game files -----------------------------------------------------------
  file_reset <- reactiveVal(0) # re-renders the file input to clear it
  output$a_file_ui <- renderUI({
    file_reset()
    fileInput("a_file", "File (PDF works best)", width = "100%")
  })

  observe({
    req(is_staff(user()))
    s <- sched()
    past <- s$game_id[!is.na(s$date) & s$date <= Sys.Date()]
    current <- isolate(input$a_game)
    selected <- if (!is.null(current) && current %in% s$game_id) {
      current
    } else if (length(past)) {
      past[which.max(s$date[s$game_id %in% past])]
    } else {
      s$game_id[1]
    }
    updateSelectInput(session, "a_game", choices = setNames(s$game_id, game_labels(s)), selected = selected)
  })

  observeEvent(input$a_type, {
    req(is_staff(user()))
    updateRadioButtons(session, "a_aud", selected = if (input$a_type == "Box score") "team" else "coaches")
  })

  observeEvent(input$a_game, {
    req(is_staff(user()))
    g <- sched() |> filter(game_id == input$a_game)
    updateTextInput(session, "a_result", value = if (nrow(g)) g$result else "")
  })

  observe({
    req(is_staff(user()))
    users <- load_users()
    choices <- sort(unique(c(users$tm_name[users$role == "player"], our_trackman_names(tm_all()))))
    choices <- choices[!is.na(choices) & nzchar(choices)]
    updateSelectizeInput(session, "a_player", choices = choices,
                         selected = isolate(input$a_player) %||% "", server = FALSE)
  })

  a_files <- reactive({
    req(is_staff(user()), input$a_game)
    game_files(input$a_game, user(), listing())
  })

  output$a_files_table <- renderDT({
    f <- a_files()
    manage_table(transmute(f, Type = type, File = name, `Who sees it` = who), "No files for this game yet")
  })

  observeEvent(input$a_save, {
    req(is_staff(user()))
    gid <- input$a_game
    g <- sched() |> filter(game_id == gid)
    req(nrow(g) == 1)
    f <- upload("a_file")
    result <- trimws(input$a_result %||% "")
    result_changed <- !identical(result, g$result)
    if (is.null(f) && !result_changed) {
      showNotification("Choose a file or change the result first.", type = "warning")
      return()
    }
    audience <- input$a_aud
    if (identical(audience, "player")) {
      audience <- trimws(input$a_player %||% "")
      if (!nzchar(player_slug(audience))) {
        showNotification("Pick the player this file is for.", type = "warning")
        return()
      }
    }
    saved <- character()
    if (!is.null(f)) {
      res <- tryCatch(post_game_file(gid, input$a_type, audience, f$datapath, f$name),
                      error = function(e) e)
      if (inherits(res, "error")) return(fail("Could not save the file", res))
      used("a_file")
      saved <- input$a_type
      bump(files_bump)
      bump(file_reset)
    }
    if (result_changed) {
      res <- tryCatch(update_game(gid, "result", result), error = function(e) e)
      if (inherits(res, "error")) return(fail("Could not save the result", res))
      saved <- c(saved, "result")
      bump(sched_bump)
    }
    showNotification(paste("Saved:", paste(saved, collapse = " + ")), type = "message")
  })

  observeEvent(input$a_file_del, {
    req(is_staff(user()))
    i <- input$a_files_table_rows_selected
    f <- a_files()
    if (length(i) != 1 || i > nrow(f)) {
      showNotification("Select a file in the table first.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Delete this file?",
      p(strong(f$name[i]), paste0(" (", f$type[i], ", ", f$who[i], ")")),
      footer = tagList(modalButton("Cancel"), actionButton("a_file_del_ok", "Delete", class = "btn-danger"))
    ))
  })

  observeEvent(input$a_file_del_ok, {
    req(is_staff(user()))
    i <- input$a_files_table_rows_selected
    f <- a_files()
    req(length(i) == 1, i <= nrow(f))
    unlink(f$path[i]) # path comes from the server-side listing, never the browser
    removeModal()
    bump(files_bump)
    showNotification(paste("Deleted", f$name[i]), type = "message")
  })

  # Scouting library -----------------------------------------------------
  observe({
    req(is_staff(user()))
    opps <- sort(unique(c(sched()$opponent, lib_all()$opponent)))
    updateSelectizeInput(session, "up_opponent", choices = c("", opps[opps != ""]),
                         selected = isolate(input$up_opponent))
  })

  observe({
    req(is_staff(user()))
    idx <- lib_all() |> arrange(desc(uploaded_at))
    labels <- paste0(idx$title, " (", idx$category, ifelse(idx$date != "", paste0(", ", idx$date), ""), ")")
    updateSelectInput(session, "del_id", choices = setNames(idx$id, labels))
  })

  observeEvent(input$up_btn, {
    req(is_staff(user()))
    f <- upload("up_file")
    if (!nzchar(trimws(input$up_title))) {
      showNotification("Add a title.", type = "warning")
      return()
    }
    if (is.null(f)) {
      showNotification("Choose a file first.", type = "warning")
      return()
    }
    result <- tryCatch(
      add_library_item(
        f$datapath, f$name,
        title = input$up_title, category = input$up_category,
        opponent = input$up_opponent %||% "", player = input$up_player,
        date = if (length(input$up_date)) format(input$up_date) else "",
        visibility = input$up_visibility, notes = input$up_notes,
        uploaded_by = user()$username
      ),
      error = function(e) e
    )
    if (inherits(result, "error")) return(fail("Could not upload", result))
    used("up_file")
    updateTextInput(session, "up_title", value = "")
    updateTextInput(session, "up_player", value = "")
    updateTextAreaInput(session, "up_notes", value = "")
    bump(lib_bump)
    showNotification("Report added to the library.", type = "message")
  })

  observeEvent(input$del_btn, {
    req(is_staff(user()), input$del_id)
    item <- lib_all() |> filter(id == input$del_id)
    req(nrow(item) == 1)
    showModal(modalDialog(
      title = "Delete this report?",
      p(strong(item$title)),
      p("This removes the file and its library entry. It cannot be undone."),
      footer = tagList(modalButton("Cancel"), actionButton("del_confirm", "Delete", class = "btn-danger"))
    ))
  })

  observeEvent(input$del_confirm, {
    req(is_staff(user()), input$del_id)
    delete_library_item(input$del_id)
    removeModal()
    bump(lib_bump)
    showNotification("Report deleted.", type = "message")
  })

  # TrackMan files -------------------------------------------------------
  tm_table <- reactive({
    req(is_staff(user()))
    trackman_file_table(tm_all())
  })

  output$tmf_table <- renderDT(manage_table(tm_table(), "No TrackMan files yet"))

  observeEvent(input$tmf_add, {
    req(is_staff(user()))
    f <- upload("tmf_files")
    if (is.null(f)) {
      showNotification("Choose one or more CSV files first.", type = "warning")
      return()
    }
    used("tmf_files")
    dir.create(TRACKMAN_DIR, recursive = TRUE, showWarnings = FALSE)
    bad <- character()
    added <- 0
    for (i in seq_len(nrow(f))) {
      missing <- trackman_missing_cols(f$datapath[i])
      if (length(missing)) {
        bad <- c(bad, paste0(f$name[i], " (missing ", paste(missing, collapse = ", "), ")"))
        next
      }
      name <- sanitize_name(f$name[i])
      if (!grepl("\\.csv$", name, ignore.case = TRUE)) name <- paste0(name, ".csv")
      if (file.copy(f$datapath[i], file.path(TRACKMAN_DIR, name), overwrite = TRUE)) added <- added + 1
    }
    if (added) {
      bump(tm_bump)
      showNotification(paste("Added", added, if (added == 1) "file" else "files", "to the season."),
                       type = "message")
    }
    if (length(bad)) {
      showNotification(HTML(paste0("Not added:<br>", paste(htmltools::htmlEscape(bad), collapse = "<br>"))),
                       type = "error", duration = NULL)
    }
  })

  observeEvent(input$tmf_del, {
    req(is_staff(user()))
    i <- input$tmf_table_rows_selected
    t <- tm_table()
    if (length(i) != 1 || i > nrow(t)) {
      showNotification("Select a file in the table first.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Delete this TrackMan file?",
      p(strong(t$File[i])),
      p("Its pitches leave the TrackMan and Season stats tabs. It cannot be undone."),
      footer = tagList(modalButton("Cancel"), actionButton("tmf_del_ok", "Delete", class = "btn-danger"))
    ))
  })

  observeEvent(input$tmf_del_ok, {
    req(is_staff(user()))
    i <- input$tmf_table_rows_selected
    t <- tm_table()
    req(length(i) == 1, i <= nrow(t))
    path <- trackman_files()[basename(trackman_files()) == t$File[i]]
    unlink(path)
    removeModal()
    bump(tm_bump)
    showNotification(paste("Deleted", t$File[i]), type = "message")
  })

  # Schedule -------------------------------------------------------------
  sg_rows <- reactive({
    req(is_staff(user()))
    sched() |> arrange(date)
  })

  sg_fields <- c("date", "opponent", "home_away", "time", "location", "result")

  output$sg_table <- renderDT({
    s <- sg_rows()
    display <- tibble::tibble(Date = format(s$date, "%Y-%m-%d"), Opponent = s$opponent,
                              `Home/Away` = s$home_away, Time = s$time,
                              Location = s$location, Result = s$result)
    datatable(
      display, rownames = FALSE, selection = "single", class = "compact hover",
      editable = list(target = "cell", disable = list(columns = 0:2)),
      # DT saves an edit when the box loses focus; make Enter do the same
      callback = JS("table.on('keyup', 'td input', function(e) { if (e.keyCode === 13) $(this).blur(); });"),
      options = list(dom = "ftp", pageLength = 15, scrollX = TRUE, ordering = FALSE,
                     language = list(emptyTable = "No games yet"))
    )
  })

  # DT gives row 1-based and col 0-based when rownames = FALSE
  observeEvent(input$sg_table_cell_edit, {
    req(is_staff(user()))
    e <- input$sg_table_cell_edit
    s <- sg_rows()
    req(e$row >= 1, e$row <= nrow(s))
    field <- sg_fields[e$col + 1]
    res <- tryCatch(update_game(s$game_id[e$row], field, e$value), error = function(e) e)
    if (inherits(res, "error")) return(fail("Could not save", res))
    bump(sched_bump)
    showNotification(paste0("Saved ", field, " for ", game_labels(s[e$row, ])), type = "message")
  })

  observeEvent(input$sg_add, {
    req(is_staff(user()))
    res <- tryCatch(add_game(input$sg_date, input$sg_opp, input$sg_ha, input$sg_time, input$sg_loc),
                    error = function(e) e)
    if (inherits(res, "error")) return(fail("Could not add the game", res))
    updateTextInput(session, "sg_opp", value = "")
    bump(sched_bump)
    showNotification(paste("Added", res), type = "message")
  })

  observeEvent(input$sg_remove, {
    req(is_staff(user()))
    i <- input$sg_table_rows_selected
    s <- sg_rows()
    if (length(i) != 1 || i > nrow(s)) {
      showNotification("Select a game in the table first.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Remove this game?",
      p(strong(game_labels(s[i, ]))),
      p("It leaves the schedule. Its report files stay on disk."),
      footer = tagList(modalButton("Cancel"), actionButton("sg_remove_ok", "Remove", class = "btn-danger"))
    ))
  })

  observeEvent(input$sg_remove_ok, {
    req(is_staff(user()))
    i <- input$sg_table_rows_selected
    s <- sg_rows()
    req(length(i) == 1, i <= nrow(s))
    remove_game(s$game_id[i])
    removeModal()
    bump(sched_bump)
    showNotification(paste("Removed", game_labels(s[i, ])), type = "message")
  })

  observeEvent(input$sg_replace, {
    req(is_staff(user()))
    if (is.null(upload("sg_csv"))) {
      showNotification("Choose a schedule CSV first.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Replace the whole schedule?",
      p("Every game is replaced by the games in ", strong(input$sg_csv$name),
        ". Report files stay on disk."),
      footer = tagList(modalButton("Cancel"), actionButton("sg_replace_ok", "Replace", class = "btn-danger"))
    ))
  })

  observeEvent(input$sg_replace_ok, {
    req(is_staff(user()), upload("sg_csv"))
    removeModal()
    used("sg_csv")
    n <- tryCatch(replace_schedule(input$sg_csv$datapath), error = function(e) e)
    if (inherits(n, "error")) return(fail("Could not replace the schedule", n))
    bump(sched_bump)
    bump(files_bump)
    showNotification(paste("Schedule replaced with", n, "games."), type = "message")
  })

  # Logins (admin only) --------------------------------------------------
  users_bump <- reactiveVal(0)
  users_rows <- reactive({
    req(is_admin(user()))
    users_bump()
    load_users() |> arrange(role, name)
  })

  output$u_table <- renderDT({
    u <- users_rows()
    manage_table(tibble::tibble(Username = u$username, Name = u$name, Role = u$role,
                                `Name in TrackMan` = u$tm_name), "No logins")
  })

  observeEvent(input$u_save, {
    req(is_admin(user()))
    res <- tryCatch(
      save_login(input$u_username, input$u_name, input$u_role, input$u_pass, input$u_tm_name,
                 by = user()$username),
      error = function(e) e
    )
    if (inherits(res, "error")) return(fail("Could not save the login", res))
    tm_name <- trimws(input$u_tm_name)
    if (!nzchar(tm_name)) tm_name <- trimws(input$u_name)
    if (input$u_role == "player" && !tm_name %in% our_trackman_names(tm_all())) {
      showNotification(paste0("Saved, but \"", tm_name, "\" isn't in any loaded TrackMan file yet. ",
                              "Check the spelling against TrackMan."), type = "warning", duration = 10)
    } else {
      showNotification(paste(if (isTRUE(res)) "Password reset for" else "Saved login for",
                             trimws(input$u_username)), type = "message")
    }
    updateTextInput(session, "u_pass", value = "")
    bump(users_bump)
  })

  observeEvent(input$u_remove, {
    req(is_admin(user()))
    i <- input$u_table_rows_selected
    u <- users_rows()
    if (length(i) != 1 || i > nrow(u)) {
      showNotification("Select a login in the table first.", type = "warning")
      return()
    }
    if (is_owner_name(u$username[i])) {
      showNotification("The owner login can't be removed.", type = "error")
      return()
    }
    if (tolower(u$username[i]) == tolower(user()$username)) {
      showNotification("You can't remove your own login.", type = "error")
      return()
    }
    remove_login(u$username[i])
    bump(users_bump)
    showNotification(paste("Removed", u$username[i]), type = "message")
  })
}
