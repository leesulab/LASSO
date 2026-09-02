#!/usr/bin/env Rscript

test_file <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[[1]])
project_root <- normalizePath(file.path(dirname(test_file), ".."), mustWork = TRUE)
source(file.path(project_root, "scripts", "nextcloud_public_webdav.R"))

security_env <- c(
  "APP_ENV",
  "NEXTCLOUD_ALLOWED_HOSTS",
  "NEXTCLOUD_PUBLIC_URL",
  "ALLOW_INSECURE_NEXTCLOUD_HTTP"
)
saved_security_env <- Sys.getenv(security_env, unset = NA_character_)
on.exit({
  for (name in security_env) {
    value <- saved_security_env[[name]]
    if (is.na(value)) {
      Sys.unsetenv(name)
    } else {
      args <- list(value)
      names(args) <- name
      do.call(Sys.setenv, args)
    }
  }
}, add = TRUE)
Sys.unsetenv(security_env)

config <- nextcloud_public_config("https://cloud.example.test/s/demoToken", "")
stopifnot(identical(config$base_url, "https://cloud.example.test"))
stopifnot(identical(config$share_token, "demoToken"))
stopifnot(identical(
  nextcloud_public_webdav_url(config$base_url, "2024/pos"),
  "https://cloud.example.test/public.php/webdav/2024/pos/"
))
stopifnot(grepl(
  "2024/pos/sample%20one.parquet$",
  nextcloud_public_file_url(config$base_url, config$share_token, "2024/pos/sample one.parquet")
))

account_config <- nextcloud_account_config(
  "https://cloud.example.test/apps/files/files/1351130?dir=%2Fobservatoire-db",
  "student",
  "demo-app-password"
)
stopifnot(identical(account_config$base_url, "https://cloud.example.test"))
stopifnot(identical(account_config$initial_path, "observatoire-db"))
stopifnot(startsWith(account_config$authorization_header, "Basic "))
account_headers <- nextcloud_http_headers(account_config)
stopifnot(identical(names(account_headers), c("User-Agent", "Authorization")))
stopifnot(startsWith(account_headers[["User-Agent"]], "Mozilla/5.0"))
stopifnot(identical(
  nextcloud_webdav_url(account_config, "observatoire-db/2024"),
  "https://cloud.example.test/remote.php/dav/files/student/observatoire-db/2024/"
))
stopifnot(identical(
  nextcloud_file_url(account_config, "observatoire-db/2024/sample.parquet"),
  "https://cloud.example.test/remote.php/dav/files/student/observatoire-db/2024/sample.parquet"
))

xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<d:multistatus xmlns:d="DAV:">',
  '<d:response><d:href>/public.php/webdav/2024/</d:href><d:propstat><d:prop><d:resourcetype><d:collection/></d:resourcetype></d:prop></d:propstat></d:response>',
  '<d:response><d:href>/public.php/webdav/2024/pos/</d:href><d:propstat><d:prop><d:resourcetype><d:collection/></d:resourcetype></d:prop></d:propstat></d:response>',
  '<d:response><d:href>/public.php/webdav/2024/sample%20one.parquet</d:href><d:propstat><d:prop><d:resourcetype/><d:getcontentlength>1234</d:getcontentlength><d:getlastmodified>Tue, 28 Jul 2026 10:00:00 GMT</d:getlastmodified></d:prop></d:propstat></d:response>',
  '</d:multistatus>'
)
contents <- parse_nextcloud_propfind(xml, config$base_url, config$share_token, "2024")
stopifnot(nrow(contents) == 2)
stopifnot(identical(contents$name, c("pos", "sample one.parquet")))
stopifnot(isTRUE(contents$is_folder[[1]]))
stopifnot(identical(contents$size[[2]], 1234))
stopifnot(grepl("demoToken", contents$url[[2]], fixed = TRUE))

account_xml <- paste0(
  '<?xml version="1.0" encoding="UTF-8"?>',
  '<d:multistatus xmlns:d="DAV:">',
  '<d:response><d:href>/remote.php/dav/files/student/observatoire-db/</d:href><d:propstat><d:prop><d:resourcetype><d:collection/></d:resourcetype></d:prop></d:propstat></d:response>',
  '<d:response><d:href>/remote.php/dav/files/student/observatoire-db/2024/</d:href><d:propstat><d:prop><d:resourcetype><d:collection/></d:resourcetype></d:prop></d:propstat></d:response>',
  '<d:response><d:href>/remote.php/dav/files/student/observatoire-db/sample.parquet</d:href><d:propstat><d:prop><d:resourcetype/><d:getcontentlength>4321</d:getcontentlength></d:prop></d:propstat></d:response>',
  '</d:multistatus>'
)
account_contents <- parse_nextcloud_propfind_config(account_xml, account_config, "observatoire-db")
stopifnot(nrow(account_contents) == 2)
stopifnot(identical(account_contents$name, c("2024", "sample.parquet")))
stopifnot(isTRUE(account_contents$is_folder[[1]]))
stopifnot(grepl("remote.php/dav/files/student/observatoire-db/sample.parquet$", account_contents$url[[2]]))

Sys.setenv(APP_ENV = "production", NEXTCLOUD_ALLOWED_HOSTS = "cloud.example.test")
stopifnot(identical(nextcloud_base_url("https://cloud.example.test/apps/files/files/1"), "https://cloud.example.test"))
blocked_host <- tryCatch({
  nextcloud_base_url("https://other.example.test/s/token")
  FALSE
}, error = function(e) grepl("n'est pas autorise", conditionMessage(e), fixed = TRUE))
stopifnot(isTRUE(blocked_host))

Sys.unsetenv("NEXTCLOUD_ALLOWED_HOSTS")
Sys.unsetenv("NEXTCLOUD_PUBLIC_URL")
missing_production_allowlist <- tryCatch({
  nextcloud_base_url("https://cloud.example.test")
  FALSE
}, error = function(e) grepl("NEXTCLOUD_ALLOWED_HOSTS", conditionMessage(e), fixed = TRUE))
stopifnot(isTRUE(missing_production_allowlist))

Sys.setenv(APP_ENV = "", NEXTCLOUD_ALLOWED_HOSTS = "")
insecure_url <- tryCatch({
  nextcloud_base_url("http://cloud.example.test")
  FALSE
}, error = function(e) grepl("https://", conditionMessage(e), fixed = TRUE))
stopifnot(isTRUE(insecure_url))
Sys.setenv(ALLOW_INSECURE_NEXTCLOUD_HTTP = "true")
stopifnot(identical(nextcloud_base_url("http://cloud.example.test"), "http://cloud.example.test"))

invalid_subpath <- tryCatch({
  normalize_nextcloud_subpath("2024/../secret")
  FALSE
}, error = function(e) grepl("'..'", conditionMessage(e), fixed = TRUE))
stopifnot(isTRUE(invalid_subpath))

cat("test_nextcloud_webdav: OK\n")
