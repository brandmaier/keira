
# PARSER
# if cur==1: complete_prev_questions; add new temp q
# if cur==2: add item information



convert_latex_simple <- function(x) {
  x <- stringr::str_replace_all(x, "Ä", "\\\"A")
  x <- stringr::str_replace_all(x, "Ö", "\\\"O")
  x <- stringr::str_replace_all(x, "Ü", "\\\"U")
  x <- stringr::str_replace_all(x, "ä", "\\\"a")
  x <- stringr::str_replace_all(x, "ö", "\\\"o")
  x <- stringr::str_replace_all(x, "ü", "\\\"u")
  x <- stringr::str_replace_all(x, "ß", "{\\ss}")
  x <- stringr::str_replace_all(x, "\n", " \\\\ ")
  x <- stringr::str_replace_all(x, "&", "{\\\\&}")
  x <- stringr::str_replace_all(x, "\"", " \\\\grqq{}")
  x
}


replace_math_with_placeholders <- function(input_text) {
  # Regular expression to match substrings between $$
  regex <- "\\$\\$.*?\\$\\$"

  # Find all matches
  matches <- regmatches(input_text, gregexpr(regex, input_text))[[1]]

  # Replace each match with a unique placeholder
  if (length(matches)>0)
    placeholders <- paste0("latex_math_placeholder_", seq_along(matches))
  else
    placeholders <- NULL

  replaced_text <- input_text

  for (i in seq_along(matches)) {
    replaced_text <- gsub(matches[i], placeholders[i], replaced_text, fixed = TRUE)
  }

  return(list(original_text = input_text,
              replaced_text = replaced_text,
              placeholders = placeholders,
              matches = matches))
}

replace_with_original <- function(replaced_text,
                                  placeholders,
                                  original_strings) {

  if (length(placeholders)==0) return(replaced_text)

  for (i in seq_along(placeholders)) {

    ostr<-original_strings[i]
    # remove trailing and leading $ to convert $$ to inline math
    ostr <- substr(ostr, 2, nchar(ostr)-1)
    # double every backslash such that they do not get lost
    ostr <- gsub("\\\\", "\\\\\\\\", ostr)

    replaced_text <- gsub(placeholders[i],
                          ostr,
                          replaced_text, fixed = TRUE)
  }
  return(replaced_text)
}

#'
#'
#' @export
convert_latex <- function(x) {

  # replace all LaTeX math parts with placeholders
  # to avoid damage
  math_objects <- replace_math_with_placeholders(x)
  x <- math_objects$replaced_text

  x <- stringr::str_replace_all(x, "ß", "{\\\\\\\\ss}")
  x <- stringr::str_replace_all(x, "%", "{\\\\\\\\%}")
  #x <- stringr::str_replace_all(x, "#", "{\\\\\\\\#}")
  #x <- stringr::str_replace_all(x, "$", "{\\\\\\\\$}")
  x <- stringr::str_replace_all(x, "&", "{\\\\\\\\&}")
  x <- stringr::str_replace_all(x, "…", "$\\\\\\\\ldots$")
  x <- stringr::str_replace_all(x, "„", "{\\\\\\\\glqq}")
  x <- stringr::str_replace_all(x, "“", "\\\\\\\\grqq{}")
  x <- stringr::str_replace_all(x, "”", "\\\\\\\\grqq{}")
  x <- stringr::str_replace_all(x, "\"", "\\\\\\\\grqq{}")
  x <- stringr::str_replace_all(x, "’","'")
  x <- stringr::str_replace_all(x,stringi::stri_unescape_unicode("\\u00A0"),"\\\\\\\\,") # non-breaking space U+00A0
  x <- stringr::str_replace_all(x,stringi::stri_unescape_unicode("\\u0094"),"\\\\\\\\grqq{}")

  x <- stringr::str_replace_all(x, "—","---")
  x <- stringr::str_replace_all(x, "–","--")
  x <- stringr::str_replace_all(x, "~","\\\\\\\\tilde")
  x <- stringr::str_replace_all(x, "°","\\\\\\\\textdegree")

  # those have to come after the quotation marks to avoid replacing the LaTeX codes from above
  x <- stringr::str_replace_all(x, "Ä", "\\\\\\\\\"A")
  x <- stringr::str_replace_all(x, "Ö", "\\\\\\\\\"O")
  x <- stringr::str_replace_all(x, "Ü", "\\\\\\\\\"U")
  x <- stringr::str_replace_all(x, "ä", "\\\\\\\\\"a")
  x <- stringr::str_replace_all(x, "ö", "\\\\\\\\\"o")
  x <- stringr::str_replace_all(x, "ü", "\\\\\\\\\"u")
  x <- stringr::str_replace_all(x, "₵", "\\\\\\\\textcolonmonetary")

  # greek letters
  x <- stringr::str_replace_all(x, "α", "$\\\\\\\\alpha$")
  x <- stringr::str_replace_all(x, "β", "$\\\\\\\\beta$")
  x <- stringr::str_replace_all(x, "μ", "$\\\\\\\\mu$")
  #str.decode("utf-8").replace(u"\u2022", "*")

  # restore math to inline math equations
  x <- replace_with_original(x,
                             math_objects$placeholders,
                             math_objects$matches)

  x
}

replace_symbol_at_position <- function(input_string, symbol, pos) {
  if (pos < 1 || pos > nchar(input_string)) {
    stop("Invalid position")
  }

  chars <- strsplit(input_string, "")[[1]]
  chars[pos] <- symbol
  new_string <- paste(chars, collapse = "")

  return(new_string)
}

#'
#' Converts an item pool from Word to exams/LaTeX format
#'
#' @param exlude_tags
#' @param include_tags
#'
#' @export
converter <- function(input_file,
                      output_directory = "items/",
                      exclude_tags = c(),
                      include_tags = c(),
                      debug = FALSE,
                      file_prefix="item",
                      add_mchoice_instruction=FALSE,
                      check_for_missing_solutions=TRUE,
                      ignore_unknown_characters=TRUE,
                      shuffle_items=TRUE) {

  doc <- officer::read_docx(input_file)

  content <- doc %>% officer::docx_summary()

  # add a fake final line (needed for all questions to be complete)
  content <- content %>% dplyr::add_row(doc_index= max(content$doc_index)+1, text="END OF DOCUMENT (Must not be empty; you should never read this line; if so, something went wrong.",
                                 content_type="paragraph",style_name="List Paragraph",level=1)

  # crazy Word hack now
  # for when all elements are on list level 1 ...
  if (all(na.omit(unique(content$level))==1)) {
    warning("Warning! All paragraphs are on level 1. Using heuristic to determine correct levels.")
    selector<-!is.na(content$level)
    guess_qid <- (content$num_id[selector])[1]
    q_select <- content$num_id == guess_qid
    resp_select <- content$num_id != guess_qid
    # reset events
    content$level[resp_select] <-2
  }

  #

  #
  # get all questions
  #
  q <- content %>% dplyr::filter(style_name == "List Paragraph")

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
  item_counter <- 1

  prev_level <- -1

  for (i in 1:num_elements) {
    cur_element <- content %>% dplyr::filter(doc_index == i)

    # skip empty lines
    if (cur_element$text=="") { next; }

    # check for non-ASCII characters
    cur_text <- cur_element$text
    utf_hits <- findNonASCII(string = cur_text)
    if (!is.null(utf_hits)) {
      chars <- paste0(sapply(utf_hits, function(pos){ substr(cur_text,pos,pos) }),sep="",collapse = " ")
      warning(paste0("Found non-ASCII characters in response ",current_answer_id," of item ",
                     (item_counter+1),", which may not show up in final exams:",
                     chars, "(item starts with:'",substr(cur_text,1,15),"')","\n"))
      flush.console()
      if (!ignore_unknown_characters) {
        #browser()
       for (k in 1:length(utf_hits)) {
        cur_text <-  replace_symbol_at_position(cur_text, "(UNKNOWN CHARACTER)",k)
       }
      }
    }

    # if no level is defined, set it to previous level
    level <- cur_element$level
    if (is.na(level))
      level <- prev_level

    # debug output
    if (debug)
      cat(
        "level ",
        level,
        ": ",
        cur_element$text,
        "; has (X):",
        endsWith(cur_element$text, "(x)"),
        "line #", i,
        "\n"
      )

    if (level == 1) {
      # store old question info

      # --> beginning of a new question block (after at least one
      # is completed)
      # => add previous question block
      if (prev_level == 2) {
        # check if question is valid

        # extract hash tags
        tokens <- strsplit(current_item_text, "\\s+")[[1]]
        regex <- "#[[:alnum:]:\\.]+"
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
          item_counter = item_counter + 1
        if (debug) {
          cat("--> Adding question with ", length(current_item_answers)," answers.\n")
        }
        if (debug) {
          if (is.null(current_item_answers)) { cat("\n\n/!\ Achtung: Keine Antworten gefunden!\n") }
          cat("------\n")
        }
        # reset items
        current_item_answers <- c()
        current_item_correct <- c()
        current_answer_id <- 1
        # start new question text body
        current_item_text = cur_element$text

      } else if (prev_level == 1) {
        # we are again at level 1 (or -1 if document begin or previous error), so
        # continue the text
        current_item_text = paste0(current_item_text,"\n \\\\\\\\ \n",cur_element$text)
      } else { # (prev_level == -1)
        current_item_text = paste0(current_item_text,"",cur_element$text)
      }


    } else if (level == 2) {
      answer <- cur_element$text
      answer <- trimws(answer, which = "both")
      if (endsWith(answer, "(x)") || endsWith(answer, "(X)")) {
        current_item_correct <- c(current_item_correct, current_answer_id)
        answer <- substr(answer, 0, nchar(answer) - 3)
      }
      current_answer_id <- current_answer_id + 1
      # this is another answer
      current_item_answers <- c(current_item_answers, answer)
    } else {
      # ignore
    }

  prev_level = level

  }

# some checks

# (x) are there duplicates?
num_items <- length(items) / 3
for (i in 1:num_items) {
  for (j in i:num_items) {
    if (i==j) next;
    current_item_text_a <- items[(i - 1) * 3 + 1][[1]]
    current_item_text_b <- items[(j - 1) * 3 + 1][[1]]

    if (current_item_text_a==current_item_text_b) {
      warning("Aufgaben ",i," und ",j," scheinen gedoppelt zu sein!")
    }
  }
}

# define file template

  file_template <- "
\\begin{question}
ITEM_TEXT

\\begin{answerlist}
ANSWERS
\\end{answerlist}
\\end{question}

ITEM_POINTS
\\exname{EXNAME}
\\extype{EXTYPE}
\\exsolution{EXSOLUTION}
\\exshuffle{EXSHUFFLE}
"

  subdir <- output_directory

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

    # get hash tags
    tokens <- strsplit(current_item_text, "\\s+")[[1]]
    regex <- "#[[:alnum:]:\\.]+"
    matches <- grep(regex, tokens, value = TRUE)

    # remove hash tags
    current_item_text <- gsub("#[[:alnum:]:\\.]+", "", current_item_text)

    # get image tags
    pattern_full <- "\\[img:.*?\\]"
    pattern <- "\\[img:(.*?)\\]"

    # Use regex match to extract the desired parts
    img_matches <- regmatches(current_item_text ,
                          gregexpr(pattern, current_item_text ))[[1]]
    #cat("Matches for IMG: ", matches, "\n")
    # Extract the captured group
    extracted_text <- gsub("\\[img:|\\]", "", img_matches)
    # add path
    extracted_text <- paste0(here::here(),.Platform$file.sep, extracted_text)

    incl_text <- paste0("\n\n \\\\\\\\includegraphics{",extracted_text,"}\n\n")
    current_item_text <- gsub(pattern_full, incl_text, current_item_text)



    responses <- items[(i - 1) * 3 + 2][[1]]
    correct_response <- items[(i - 1) * 3 + 3][[1]]
    if (check_for_missing_solutions && is.null(correct_response)) {
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
    if (!is.null(correct_response)) {
      exsolution[correct_response] <- 1
    }
    exsolution <- paste0(exsolution, sep = "", collapse = "")

    exname <- paste0("Item", i, sep = "", collapse = "")



    if (length(responses) == 0)
      stop(paste0("Item #", i, " has no responses: ",
                  current_item_text, "!"))

    rsp <- ""
    for (j in 1:length(responses)) {
      rsp <- paste0(rsp, "\\\\item ", convert_latex(responses[j]), "\n")
    }

    # are there specific points per exam?
    if (length(matches)>0) {
      browser()
      mtep <- unlist(sapply(matches, extract_points_from_hashtag))
      if (!all(is.null(mtep))) {
      extracted_points <- max(mtep)
      if (!is.null(extracted_points)) {
        cur_file <-
          stringr::str_replace(cur_file, "ITEM_POINTS",
                               paste0("\\\\expoints{",extracted_points,"}"))
      }

      }
    }
    cur_file <-
      stringr::str_replace(cur_file, "ITEM_POINTS",
                           "")


    # type inference
    extype <- "schoice"
    if (any(matches %in% "#mchoice")) {
      extype <- "mchoice"
    } else if ((any(matches %in% "#schoice"))) {
      extype <- "schoice"
    } else {
      if (length(correct_response)==1) {
        extype <- "schoice"
      } else {
        extype <- "mchoice"
      }
    }

    if (extype=="mchoice") {
      if (add_mchoice_instruction) {
        current_item_text <- paste0(current_item_text,"\\\\newline","\\\\emph{Mehrere Antworten können korrekt sein.}")
      }
    }

    cur_file <-
      stringr::str_replace(cur_file, "ITEM_TEXT", convert_latex(current_item_text))
    cur_file <-
      stringr::str_replace(cur_file, "EXSOLUTION", exsolution)
    cur_file <- stringr::str_replace(cur_file, "ANSWERS", rsp)
    if (shuffle_items) {
      cur_file <-
        stringr::str_replace(cur_file, "EXSHUFFLE", as.character(length(responses)))
    } else {
      cur_file <-
        stringr::str_replace(cur_file, "EXSHUFFLE", "FALSE")
    }
    cur_file <- stringr::str_replace(cur_file, "EXNAME", exname)
    cur_file <- stringr::str_replace(cur_file, "EXTYPE", extype)

    fullnr <- paste0(paste0(rep(0,max(0,3-nchar(i))),collapse=""),i,collapse="")

    writeLines(cur_file, paste0(subdir, file_prefix, fullnr, ".Rnw"))
  }

}
