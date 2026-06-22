# -----------------------------------------------------------------------------
# Scrape PPSH correspondence links from the Albanian State Archive catalogue
# -----------------------------------------------------------------------------
# Purpose:
#   This script collects catalogue records related to the correspondence between
#   the Party of Labour of Albania (PPSH) and communist parties / Marxist-Leninist
#   groups abroad. It then produces:
#     1. a cleaned CSV dataset of catalogue records,
#     2. a frequency plot by country and year,
#     3. a Gephi-ready edge list for network analysis.
#
# Notes:
#   - Update the paths in the "Configuration" section before running the script.
#   - If you have a manually prepared party metadata file, set PARTY_METADATA_PATH.
#   - The script uses polite delays between requests. Increase REQUEST_DELAY if the
#     archive server responds slowly or blocks repeated requests.
# -----------------------------------------------------------------------------

# ---- 1. Packages -------------------------------------------------------------

required_packages <- c(
  "rvest", "xml2", "dplyr", "purrr", "stringr", "tibble", "tidyr",
  "readr", "data.table", "ggplot2", "ggthemes", "hrbrthemes", "tidygraph"
)

missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  stop(
    "Please install the following packages before running the script: ",
    paste(missing_packages, collapse = ", ")
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))

# ---- 2. Configuration --------------------------------------------------------

BASE_URL <- "https://katalogu.arkiva.gov.al"

START_URL <- paste0(
  BASE_URL,
  "/public/categories/1323209-komiteti-qendror-i-partise-punes-shqiperise-",
  "marredheniet-me-partite-komuniste-dhe-grupet-marksiste-leniniste"
)

# Optional metadata file with party names and countries.
# Expected minimum structure: party, country, number/category_id.
# Leave as NA_character_ if you want the script to rely only on parsed catalogue data.
PARTY_METADATA_PATH <- NA_character_

OUTPUT_DIR <- "data/processed"
PLOT_DIR <- "figures"

CSV_OUTPUT <- file.path(OUTPUT_DIR, "lidhjet_partise_komuniste_ppsh_jashte.csv")
PLOT_OUTPUT <- file.path(PLOT_DIR, "correspondence_ppsh_global_communist.png")
GEPHI_EDGES_OUTPUT <- file.path(OUTPUT_DIR, "lidhjet_ppsh_gephi_edges.csv")

REQUEST_DELAY <- 0.5

# Create output folders if they do not exist.
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- 3. Helper functions -----------------------------------------------------

read_page_safely <- function(url, delay = REQUEST_DELAY) {
  # Read an HTML page and return NULL instead of stopping the whole script if one
  # request fails.
  message("Reading: ", url)
  Sys.sleep(delay)

  tryCatch(
    rvest::read_html(url),
    error = function(e) {
      warning("Could not read page: ", url, " | ", conditionMessage(e))
      NULL
    }
  )
}

extract_category_code <- function(url_or_path) {
  # Extract the catalogue category path, e.g. "categories/1323209-...".
  stringr::str_extract(url_or_path, "categories/[^/?#]+")
}

extract_category_id <- function(category_code) {
  # Extract the numeric category id from a category code.
  stringr::str_match(category_code, "categories/([0-9]+)")[, 2]
}

extract_slug <- function(category_code) {
  # Convert a catalogue slug into a readable label.
  category_code %>%
    stringr::str_remove("^categories/[0-9]+-") %>%
    stringr::str_replace_all("-", " ") %>%
    stringr::str_squish() %>%
    stringr::str_to_sentence()
}

extract_year <- function(text) {
  # Extract a four-digit year from a URL, category code, or record title.
  stringr::str_extract(text, "(?<![0-9])(18|19|20)[0-9]{2}(?![0-9])")
}

extract_category_links <- function(page, base_url) {
  # Extract all archive category links from a page.
  if (is.null(page)) {
    return(tibble())
  }

  page %>%
    rvest::html_elements("a") %>%
    tibble::tibble(
      link_text = rvest::html_text2(.),
      href = rvest::html_attr(., "href")
    ) %>%
    dplyr::filter(!is.na(href)) %>%
    dplyr::mutate(
      url = xml2::url_absolute(href, base_url),
      category_code = extract_category_code(url),
      category_id = extract_category_id(category_code),
      category_slug = extract_slug(category_code),
      year = extract_year(category_code)
    ) %>%
    dplyr::filter(!is.na(category_code), !is.na(category_id)) %>%
    dplyr::distinct(category_code, .keep_all = TRUE)
}

extract_document_links <- function(page, base_url) {
  # Extract catalogue document links. The archive often marks files with "DO";
  # the regex below also keeps common variants such as "Dosje".
  if (is.null(page)) {
    return(tibble())
  }

  page %>%
    rvest::html_elements("a") %>%
    tibble::tibble(
      document_title = rvest::html_text2(.),
      href = rvest::html_attr(., "href")
    ) %>%
    dplyr::mutate(
      document_url = xml2::url_absolute(href, base_url),
      record_year = extract_year(document_title)
    ) %>%
    dplyr::filter(
      !is.na(document_title),
      stringr::str_detect(document_title, stringr::regex("\\bDO\\b|\\bD\\.?O\\.?\\b|\\bDosje\\b", ignore_case = TRUE))
    ) %>%
    dplyr::distinct(document_title, document_url, .keep_all = TRUE)
}

standardise_party_metadata <- function(path) {
  # Read and standardise a manually prepared party metadata table.
  # The original working file appears to use a table with party, country, and Nr.
  # This function keeps that structure but renames it more clearly.
  if (is.na(path) || !file.exists(path)) {
    return(NULL)
  }

  metadata <- data.table::fread(path, data.table = FALSE)

  if (ncol(metadata) < 3) {
    stop("PARTY_METADATA_PATH must contain at least three columns: party, country, number/category id.")
  }

  names(metadata)[1:3] <- c("party", "country", "number")

  if (!"code_2" %in% names(metadata)) {
    metadata$code_2 <- metadata$number
  }

  metadata %>%
    dplyr::mutate(code_2 = as.character(code_2)) %>%
    dplyr::distinct(code_2, .keep_all = TRUE)
}

# ---- 4. Read the start page and collect party/category links -----------------

start_page <- read_page_safely(START_URL)
root_category_id <- extract_category_id(extract_category_code(START_URL))

party_links <- extract_category_links(start_page, START_URL) %>%
  dplyr::filter(category_id != root_category_id) %>%
  dplyr::transmute(
    code_2 = as.character(category_id),
    party_url = url,
    party_from_url = category_slug
  ) %>%
  dplyr::distinct(code_2, .keep_all = TRUE)

if (nrow(party_links) == 0) {
  stop("No party/category links were found on the start page. Check START_URL or the page structure.")
}

# ---- 5. Add optional party metadata -----------------------------------------

party_metadata <- standardise_party_metadata(PARTY_METADATA_PATH)

party_lookup <- party_links

if (!is.null(party_metadata)) {
  party_lookup <- party_lookup %>%
    dplyr::left_join(party_metadata, by = "code_2") %>%
    dplyr::mutate(
      party = dplyr::coalesce(.data$party, .data$party_from_url),
      country = dplyr::if_else(is.na(.data$country), NA_character_, as.character(.data$country))
    )
} else {
  party_lookup <- party_lookup %>%
    dplyr::mutate(
      party = party_from_url,
      country = NA_character_
    )
}

# ---- 6. Collect year/subcategory links for each party ------------------------

party_year_links <- purrr::pmap_dfr(
  party_lookup %>% dplyr::select(code_2, party_url, party),
  function(code_2, party_url, party) {
    page <- read_page_safely(party_url)

    extract_category_links(page, party_url) %>%
      dplyr::filter(category_id != code_2) %>%
      dplyr::transmute(
        code_2 = code_2,
        party = party,
        code = category_code,
        year_category_id = category_id,
        year_category_url = url,
        year = year
      )
  }
)

if (nrow(party_year_links) == 0) {
  stop("No year/subcategory links were found. Check whether the archive page structure has changed.")
}

# ---- 7. Scrape catalogue records from each year/subcategory page -------------

archive_records <- purrr::pmap_dfr(
  party_year_links %>%
    dplyr::select(code_2, party, code, year, year_category_url),
  function(code_2, party, code, year, year_category_url) {
    page <- read_page_safely(year_category_url)

    extract_document_links(page, year_category_url) %>%
      dplyr::mutate(
        code_2 = code_2,
        party = party,
        code = code,
        year_category_url = year_category_url,
        year = dplyr::coalesce(year, record_year),
        .before = 1
      )
  }
)

if (nrow(archive_records) == 0) {
  stop("No archive records were found. Check the document-title filter in extract_document_links().")
}

# ---- 8. Build final cleaned dataset -----------------------------------------

lidhjet_partite <- archive_records %>%
  dplyr::left_join(
    party_lookup %>% dplyr::select(code_2, country, party_from_url),
    by = "code_2"
  ) %>%
  dplyr::mutate(
    party = dplyr::coalesce(.data$party, .data$party_from_url),
    year = as.integer(.data$year)
  ) %>%
  dplyr::select(
    country,
    party,
    year,
    document_title,
    document_url,
    year_category_url,
    code_2,
    code
  ) %>%
  dplyr::arrange(country, party, year, document_title)

readr::write_csv(lidhjet_partite, CSV_OUTPUT)
message("Saved cleaned dataset to: ", CSV_OUTPUT)

# ---- 9. Plot correspondence frequency by year -------------------------------

frequency_by_country_year <- lidhjet_partite %>%
  dplyr::filter(!is.na(year)) %>%
  dplyr::mutate(target_name = dplyr::coalesce(country, party)) %>%
  dplyr::count(target_name, year, name = "frequency")

frequency_plot <- frequency_by_country_year %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = frequency)) +
  ggplot2::geom_col() +
  ggthemes::theme_clean() +
  ggplot2::labs(
    title = "Frequency of correspondence",
    subtitle = "Between the Party of Labour of Albania and communist parties / groups abroad",
    caption = "Source: Albanian State Archive catalogue; scraped from public catalogue pages.",
    x = "Year",
    y = "Frequency"
  ) +
  hrbrthemes::theme_ipsum(
    axis_text_size = 8,
    plot_title_size = 12,
    subtitle_size = 10
  )

print(frequency_plot)

ggplot2::ggsave(
  filename = PLOT_OUTPUT,
  plot = frequency_plot,
  dpi = 300,
  width = 9,
  height = 5
)
message("Saved plot to: ", PLOT_OUTPUT)

# ---- 10. Export Gephi-ready edge list ---------------------------------------

# Gephi can import a simple CSV edge list. Each row represents a connection from
# Albania to a country in a specific year. The edge weight is the number of
# catalogue records for that country-year pair.
gephi_edges <- frequency_by_country_year %>%
  dplyr::mutate(
    source = "Albania",
    target = target_name,
    label = paste(source, target, year, sep = " - ")
  ) %>%
  dplyr::select(source, target, year, weight = frequency, label)

readr::write_csv(gephi_edges, GEPHI_EDGES_OUTPUT)
message("Saved Gephi edge list to: ", GEPHI_EDGES_OUTPUT)

# Optional: create a tidygraph object for further network analysis inside R.
lidhjet_graf <- tidygraph::as_tbl_graph(
  gephi_edges %>% dplyr::select(from = source, to = target, weight, year),
  directed = TRUE
) %>%
  tidygraph::activate(nodes) %>%
  dplyr::mutate(
    title = stringr::str_to_title(name),
    label = stringr::str_replace_all(title, " ", "\n")
  )

# ---- 11. Quick summary -------------------------------------------------------

message("Number of unique countries: ", dplyr::n_distinct(lidhjet_partite$country, na.rm = TRUE))
message("Number of unique records: ", dplyr::n_distinct(lidhjet_partite$document_title, na.rm = TRUE))
message("Done.")
