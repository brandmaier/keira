

library(officer)
library(tidyverse)
library(stringr)

#'
#' Converts an item pool from Word to exams/LaTeX format
#'
#' @param exlude_tags
#' @param include_tags
#'
#'
converter <- function(input_file,
                      subdir = "temp/",
                      exclude_tags = c(),
                      include_tags = c(),
                      debug = FALSE) {
  doc <- officer::read_docx(input_file)

  content <- doc %>% officer::docx_summary()

  #
  # get all questions
  #
  q <- doc %>% officer::docx_summary() %>% dplyr::filter(style_name == "List Paragraph")

  if (nrow(q)==0) {
    stop("Error! Did not find any elements of type list paragraph! Please make sure all items are lists!")
  }

  items_text <- q$text


  num_elements <- nrow(content)
  items <- list()

  current_item_text <- ""
  current_item_answers <- c()
  current_item_correct <- c()
  current_answer_id <- 1
  current_mode <- "q"

  for (i in 1:num_elements) {
    cur_element <- content %>% dplyr::filter(doc_index == i)

    # skip empty lines
    if (cur_element$text=="") { next; }

    level <- cur_element$level
    if (is.na(level))
      level <- -1

    if (debug)
      cat(
        "level ",
        level,
        ": ",
        cur_element$text,
        "; has (X):",
        endsWith(cur_element$text, "(x)"),
        "\n"
      )

    if (level == 1) {
      # store old question info
      if (i != 1) {
        # check if question is valid

        # extract hash tags
        tokens <- strsplit(current_item_text, "\\s+")[[1]]
        regex <- "#[[:alpha:]:]+"
        matches <- grep(regex, tokens, value = TRUE)

        # check exclusion criteria
        skip_question <- any(matches %in% exclude_tags)

        # check inclusion
        if (!skip_question) {
          if (length(include_tags) > 0) {
            skip_question <- !(any(matches %in% include_tags))
          }
        }

        # add current question to pool
        if (!skip_question)
          items <-
          append(
            items,
            list(
              current_item_text,
              current_item_answers,
              current_item_correct
            )
          )
        # reset items
        current_item_answers <- c()
        current_item_correct <- c()
        current_answer_id <- 1
      }
      # new question
      current_item_text = cur_element$text
    } else if (level == 2) {
      answer <- cur_element$text
      answer <- trimws(answer, which = "both")
      if (endsWith(answer, "(x)")) {
        current_item_correct <- c(current_item_correct, current_answer_id)
        answer <- substr(answer, 0, nchar(answer) - 3)
      }
      current_answer_id <- current_answer_id + 1
      # this is another answer
      current_item_answers <- c(current_item_answers, answer)
    } else {
      # ignore
    }

  }

  convert_latex <- function(x) {
    x <- stringr::str_replace_all(x, "Ä", "\\\\\\\\\"A")
    x <- stringr::str_replace_all(x, "Ö", "\\\\\\\\\"O")
    x <- stringr::str_replace_all(x, "Ü", "\\\\\\\\\"U")
    x <- stringr::str_replace_all(x, "ä", "\\\\\\\\\"a")
    x <- stringr::str_replace_all(x, "ö", "\\\\\\\\\"o")
    x <- stringr::str_replace_all(x, "ü", "\\\\\\\\\"u")
    x <- stringr::str_replace_all(x, "ß", "{\\\\\\\\ss}")
    x <- stringr::str_replace_all(x, "%", "{\\\\\\\\%}")
    x <- stringr::str_replace_all(x, "#", "{\\\\\\\\#}")
    #x <- stringr::str_replace_all(x, "$", "{\\\\\\\\$}")
    x <- stringr::str_replace_all(x, "&", "{\\\\\\\\&}")
    x
  }

  # add last item
  #items <- c(items, list(current_item_text, current_item_answers, current_item_correct))
  items <-
    append(items,
           list(
             current_item_text,
             current_item_answers,
             current_item_correct
           ))


  # write files

  file_template <- "
\\begin{question}
ITEM_TEXT

\\begin{answerlist}
ANSWERS
\\end{answerlist}
\\end{question}

\\exname{EXNAME}
\\extype{mchoice}
\\exsolution{EXSOLUTION}
\\exshuffle{EXSHUFFLE}
"

  print(subdir)

  if (subdir != "") {
    if (!dir.exists(subdir))
      dir.create(subdir)

    if (!endsWith(subdir, "/"))
      subdir <- paste0(subdir, "/", sep = "", collapse = "")

  }

  if (!(length(items) %% 3 == 0))
    stop("Error in parsing file!")

  num_items <- length(items) / 3

  for (i in 1:num_items) {
    cur_item <- items[i]
    cur_file <- file_template

    current_item_text <- items[(i - 1) * 3 + 1][[1]]



    # remove hash tags
    current_item_text <- gsub("#[[:alpha:]:]+", "", current_item_text)


    responses <- items[(i - 1) * 3 + 2][[1]]
    correct_response <- items[(i - 1) * 3 + 3][[1]]
    if (is.null(correct_response)) {
      stop(
        paste0(
          "In item #",
          i,
          ",Missing information about correct item (out of ",
          length(responses),
          " answers) for item: ",
          current_item_text,
          "!"
        )
      )
    }

    exsolution <- rep(0, length(responses))
    exsolution[correct_response] <- 1
    exsolution <- paste0(exsolution, sep = "", collapse = "")

    exname <- paste0("Item", i, sep = "", collapse = "")



    if (length(responses) == 0)
      stop(paste0("Item #", i, " has no responses: ",
                  current_item_text, "!"))

    rsp <- ""
    for (j in 1:length(responses)) {
      rsp <- paste0(rsp, "\\\\item ", convert_latex(responses[j]), "\n")
    }

    cur_file <-
      stringr::str_replace(cur_file, "ITEM_TEXT", convert_latex(current_item_text))
    cur_file <-
      stringr::str_replace(cur_file, "EXSOLUTION", exsolution)
    cur_file <- stringr::str_replace(cur_file, "ANSWERS", rsp)
    cur_file <-
      stringr::str_replace(cur_file, "EXSHUFFLE", as.character(length(responses)))
    cur_file <- stringr::str_replace(cur_file, "EXNAME", exname)


    writeLines(cur_file, paste0(subdir, "item", i, ".Rnw"))
  }

}
