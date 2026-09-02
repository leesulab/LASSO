#!/usr/bin/env bash

set -euo pipefail

project_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
env_file="${project_root}/.env"

if [[ -f "${env_file}" ]]; then
  set -a
  # shellcheck disable=SC1090
  source "${env_file}"
  set +a
fi

export PROJECT_ROOT="${PROJECT_ROOT:-${project_root}}"
export PORT="${PORT:-7660}"

if [[ -z "${DATA_PATH:-}" ]]; then
  echo "DATA_PATH is required. Copy .env.example to .env and configure it." >&2
  exit 1
fi

if [[ ! -d "${DATA_PATH}" ]]; then
  echo "DATA_PATH does not exist: ${DATA_PATH}" >&2
  exit 1
fi

cd "${project_root}"
exec Rscript -e 'shiny::runApp("app", host = "127.0.0.1", port = as.integer(Sys.getenv("PORT", "7660")), launch.browser = FALSE)'
