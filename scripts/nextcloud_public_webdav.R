#!/usr/bin/env Rscript

# WebDAV helpers for either a public Nextcloud share or an authenticated account.
# Secrets stay only in the Shiny session and are never written to disk or displayed.

require_nextcloud_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    stop("Package '", package, "' is required. Install it with install.packages('", package, "').")
  }
}

empty_nextcloud_contents <- function() {
  data.frame(
    name = character(),
    path = character(),
    is_folder = logical(),
    size = numeric(),
    modified = character(),
    url = character(),
    stringsAsFactors = FALSE
  )
}

nextcloud_scalar_character <- function(value) {
  if (is.null(value) || length(value) == 0 || is.na(value[[1]])) {
    return("")
  }
  as.character(value[[1]])
}

nextcloud_env_flag <- function(name) {
  value <- tolower(trimws(Sys.getenv(name, unset = "")))
  value %in% c("1", "true", "yes", "on")
}

nextcloud_is_production <- function() {
  tolower(trimws(Sys.getenv("APP_ENV", unset = ""))) %in% c("production", "prod")
}

nextcloud_share_token_from_url <- function(url) {
  raw_url <- trimws(nextcloud_scalar_character(url))
  share_match <- regexec("/s/([^/?#]+)", raw_url, perl = TRUE)
  share_parts <- regmatches(raw_url, share_match)[[1]]
  if (length(share_parts) < 2) "" else share_parts[[2]]
}

nextcloud_url_host <- function(url) {
  require_nextcloud_package("httr")
  parsed <- httr::parse_url(nextcloud_scalar_character(url))
  host <- parsed$hostname
  if (is.null(host) || length(host) == 0 || is.na(host[[1]]) || !nzchar(host[[1]])) {
    stop("L'URL Nextcloud ne contient pas d'hote valide.")
  }
  tolower(host[[1]])
}

nextcloud_allowed_hosts <- function() {
  raw_hosts <- trimws(Sys.getenv("NEXTCLOUD_ALLOWED_HOSTS", unset = ""))
  hosts <- if (nzchar(raw_hosts)) {
    trimws(strsplit(raw_hosts, ",", fixed = TRUE)[[1]])
  } else {
    character()
  }
  hosts <- tolower(hosts[nzchar(hosts)])

  # A configured default URL also safely restricts the application to its host.
  if (length(hosts) == 0) {
    default_url <- trimws(Sys.getenv("NEXTCLOUD_PUBLIC_URL", unset = ""))
    if (nzchar(default_url)) {
      host <- tryCatch(nextcloud_url_host(default_url), error = function(e) "")
      if (nzchar(host)) hosts <- host
    }
  }
  unique(hosts)
}

normalize_nextcloud_subpath <- function(path) {
  path <- trimws(nextcloud_scalar_character(path))
  path <- gsub("^/+|/+$", "", path)
  if (!nzchar(path)) {
    return("")
  }
  segments <- strsplit(path, "/", fixed = TRUE)[[1]]
  segments <- segments[nzchar(segments)]
  if (any(segments %in% c(".", ".."))) {
    stop("Le chemin Nextcloud ne peut pas contenir '.' ou '..'.")
  }
  paste(segments, collapse = "/")
}

nextcloud_base_url <- function(url) {
  raw_url <- trimws(nextcloud_scalar_character(url))
  if (!nzchar(raw_url)) {
    stop("L'URL Nextcloud est requise.")
  }

  normalized_url <- sub("[?#].*$", "", raw_url)
  normalized_url <- sub("/(?:apps|remote\\.php|public\\.php|s)(?:/.*)?$", "", normalized_url, perl = TRUE)
  normalized_url <- sub("/+$", "", normalized_url)
  secure_scheme <- grepl("^https://", normalized_url, ignore.case = TRUE)
  insecure_scheme <- grepl("^http://", normalized_url, ignore.case = TRUE)
  if (!secure_scheme && !(insecure_scheme && nextcloud_env_flag("ALLOW_INSECURE_NEXTCLOUD_HTTP"))) {
    stop("L'URL Nextcloud doit utiliser https://. HTTP n'est autorise qu'en developpement avec ALLOW_INSECURE_NEXTCLOUD_HTTP=true.")
  }
  if (!secure_scheme && !insecure_scheme) {
    stop("L'URL Nextcloud doit commencer par https://.")
  }

  host <- nextcloud_url_host(normalized_url)
  allowed_hosts <- nextcloud_allowed_hosts()
  if (nextcloud_is_production() && length(allowed_hosts) == 0) {
    stop("En production, configure NEXTCLOUD_ALLOWED_HOSTS avec l'hote Nextcloud autorise.")
  }
  if (length(allowed_hosts) > 0 && !host %in% allowed_hosts) {
    stop("L'hote Nextcloud n'est pas autorise par NEXTCLOUD_ALLOWED_HOSTS.")
  }
  normalized_url
}

nextcloud_initial_path_from_url <- function(url) {
  raw_url <- nextcloud_scalar_character(url)
  match <- regexec("[?&]dir=([^&#]*)", raw_url, perl = TRUE)
  parts <- regmatches(raw_url, match)[[1]]
  if (length(parts) < 2) {
    return("")
  }
  normalize_nextcloud_subpath(utils::URLdecode(parts[[2]]))
}

nextcloud_basic_authorization_header <- function(username, password, allow_empty_password = FALSE) {
  username <- nextcloud_scalar_character(username)
  password <- nextcloud_scalar_character(password)
  if (!nzchar(username) || (!isTRUE(allow_empty_password) && !nzchar(password))) {
    stop("Un identifiant et un mot de passe d'application Nextcloud sont requis.")
  }
  require_nextcloud_package("jsonlite")
  paste0("Basic ", jsonlite::base64_enc(charToRaw(paste0(username, ":", password))))
}

nextcloud_public_config <- function(base_url, share_token = "") {
  raw_url <- trimws(nextcloud_scalar_character(base_url))
  token <- trimws(nextcloud_scalar_character(share_token))
  if (!nzchar(raw_url)) {
    stop("L'URL Nextcloud est requise.")
  }

  if (!nzchar(token)) {
    token <- nextcloud_share_token_from_url(raw_url)
  }
  if (!nzchar(token)) {
    stop("Le jeton de partage Nextcloud est requis.")
  }

  list(
    access_mode = "public",
    base_url = nextcloud_base_url(raw_url),
    share_token = token,
    username = "",
    authorization_header = nextcloud_basic_authorization_header(token, "", allow_empty_password = TRUE),
    initial_path = nextcloud_initial_path_from_url(raw_url)
  )
}

nextcloud_account_config <- function(base_url, username, app_password) {
  username <- trimws(nextcloud_scalar_character(username))
  app_password <- nextcloud_scalar_character(app_password)
  if (!nzchar(username)) {
    stop("L'identifiant Nextcloud est requis.")
  }
  if (!nzchar(app_password)) {
    stop("Le mot de passe d'application Nextcloud est requis.")
  }

  list(
    access_mode = "account",
    base_url = nextcloud_base_url(base_url),
    share_token = "",
    username = username,
    authorization_header = nextcloud_basic_authorization_header(username, app_password),
    initial_path = nextcloud_initial_path_from_url(base_url)
  )
}

nextcloud_connection_config <- function(access_mode, base_url, share_token = "", username = "", app_password = "") {
  access_mode <- match.arg(nextcloud_scalar_character(access_mode), c("public", "account"))
  if (identical(access_mode, "public")) {
    return(nextcloud_public_config(base_url, share_token))
  }
  nextcloud_account_config(base_url, username, app_password)
}

nextcloud_http_user_agent <- function() {
  paste0(
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 ",
    "(KHTML, like Gecko) Chrome/126.0.0.0 Safari/537.36 Observatoire-HRMS/0.1"
  )
}

nextcloud_url_path <- function(path) {
  normalized_path <- normalize_nextcloud_subpath(path)
  if (!nzchar(normalized_path)) {
    return("")
  }
  segments <- strsplit(normalized_path, "/", fixed = TRUE)[[1]]
  paste(vapply(segments, utils::URLencode, character(1), reserved = TRUE), collapse = "/")
}

nextcloud_webdav_url <- function(config, subpath = "") {
  subpath <- nextcloud_url_path(subpath)
  prefix <- if (identical(config$access_mode, "public")) {
    paste0(config$base_url, "/public.php/webdav")
  } else {
    paste0(config$base_url, "/remote.php/dav/files/", utils::URLencode(config$username, reserved = TRUE))
  }
  if (!nzchar(subpath)) {
    return(paste0(prefix, "/"))
  }
  paste0(prefix, "/", subpath, "/")
}

nextcloud_file_url <- function(config, relative_path) {
  encoded_path <- nextcloud_url_path(relative_path)
  if (identical(config$access_mode, "public")) {
    encoded_token <- utils::URLencode(config$share_token, reserved = TRUE)
    return(paste0(config$base_url, "/public.php/dav/files/", encoded_token, "/", encoded_path))
  }
  encoded_username <- utils::URLencode(config$username, reserved = TRUE)
  paste0(config$base_url, "/remote.php/dav/files/", encoded_username, "/", encoded_path)
}

# Compatibility wrappers retained for the existing public-share tests and callers.
nextcloud_public_webdav_url <- function(base_url, subpath = "") {
  nextcloud_webdav_url(nextcloud_public_config(base_url, share_token = "placeholder"), subpath)
}

nextcloud_public_file_url <- function(base_url, share_token, relative_path) {
  nextcloud_file_url(nextcloud_public_config(base_url, share_token), relative_path)
}

xml_text_or_na <- function(node) {
  if (inherits(node, "xml_missing") || length(node) == 0) {
    return(NA_character_)
  }
  value <- xml2::xml_text(node)
  if (is.na(value) || !nzchar(value)) NA_character_ else value
}

nextcloud_relative_path_from_href <- function(config, href) {
  decoded_href <- utils::URLdecode(href)
  if (identical(config$access_mode, "public")) {
    relative_path <- sub("^.*?/public\\.php/(?:webdav|dav/files/[^/]+)/?", "", decoded_href, perl = TRUE)
  } else {
    relative_path <- sub("^.*?/remote\\.php/dav/files/[^/]+/?", "", decoded_href, perl = TRUE)
  }
  normalize_nextcloud_subpath(relative_path)
}

parse_nextcloud_propfind_config <- function(xml_text, config, subpath = "") {
  require_nextcloud_package("xml2")
  subpath <- normalize_nextcloud_subpath(subpath)
  document <- xml2::read_xml(xml_text)
  responses <- xml2::xml_find_all(document, ".//*[local-name()='response']")
  if (length(responses) == 0) {
    return(empty_nextcloud_contents())
  }

  items <- lapply(responses, function(response) {
    href <- xml_text_or_na(xml2::xml_find_first(response, ".//*[local-name()='href']"))
    if (is.na(href)) {
      return(NULL)
    }
    href_path <- nextcloud_relative_path_from_href(config, href)
    if (!nzchar(href_path) || identical(href_path, subpath)) {
      return(NULL)
    }

    name <- basename(href_path)
    if (!nzchar(name)) {
      return(NULL)
    }
    resource_type <- xml2::xml_find_first(response, ".//*[local-name()='resourcetype']")
    is_folder <- !inherits(
      xml2::xml_find_first(resource_type, ".//*[local-name()='collection']"),
      "xml_missing"
    )
    size_text <- xml_text_or_na(xml2::xml_find_first(response, ".//*[local-name()='getcontentlength']"))
    modified <- xml_text_or_na(xml2::xml_find_first(response, ".//*[local-name()='getlastmodified']"))

    data.frame(
      name = name,
      path = href_path,
      is_folder = is_folder,
      size = suppressWarnings(as.numeric(size_text)),
      modified = modified,
      url = if (is_folder) NA_character_ else nextcloud_file_url(config, href_path),
      stringsAsFactors = FALSE
    )
  })
  items <- Filter(Negate(is.null), items)
  if (length(items) == 0) {
    return(empty_nextcloud_contents())
  }

  result <- do.call(rbind, items)
  result[order(!result$is_folder, tolower(result$name)), , drop = FALSE]
}

parse_nextcloud_propfind <- function(xml_text, base_url, share_token, subpath = "") {
  config <- nextcloud_public_config(base_url, share_token)
  parse_nextcloud_propfind_config(xml_text, config, subpath)
}

nextcloud_http_headers <- function(config) {
  c(
    "User-Agent" = nextcloud_http_user_agent(),
    "Authorization" = as.character(config$authorization_header)
  )
}

list_nextcloud_contents <- function(config, subpath = "", timeout_seconds = 20) {
  require_nextcloud_package("httr")
  webdav_url <- nextcloud_webdav_url(config, subpath)
  body <- paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<d:propfind xmlns:d="DAV:"><d:allprop/></d:propfind>'
  )

  response <- httr::VERB(
    verb = "PROPFIND",
    url = webdav_url,
    httr::add_headers(
      Authorization = config$authorization_header,
      Depth = "1",
      `Content-Type` = "application/xml",
      `X-Requested-With` = "XMLHttpRequest"
    ),
    httr::user_agent(nextcloud_http_user_agent()),
    body = body,
    httr::timeout(timeout_seconds)
  )
  status <- httr::status_code(response)
  if (!status %in% c(200, 207)) {
    advice <- if (identical(status, 401L) && identical(config$access_mode, "account")) {
      "Authentification refusee : verifie l'identifiant et le mot de passe d'application."
    } else if (identical(status, 403L) && identical(config$access_mode, "account")) {
      "Acces interdit : verifie les droits du compte sur ce dossier."
    } else if (identical(status, 404L) && identical(config$access_mode, "account")) {
      "Dossier introuvable via WebDAV : essaie le dossier parent pour repartir de la racine."
    } else if (identical(config$access_mode, "account")) {
      "Verifie l'identifiant, le mot de passe d'application et les droits du dossier."
    } else {
      "Verifie l'URL et le jeton de partage."
    }
    stop("Impossible de lire le dossier Nextcloud (HTTP ", status, "). ", advice)
  }

  parse_nextcloud_propfind_config(
    httr::content(response, as = "text", encoding = "UTF-8"),
    config,
    subpath = subpath
  )
}

list_nextcloud_public_contents <- function(base_url, share_token, subpath = "", timeout_seconds = 20) {
  list_nextcloud_contents(nextcloud_public_config(base_url, share_token), subpath, timeout_seconds)
}
