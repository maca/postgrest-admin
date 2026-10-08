{ pkgs }:

{
  # Fetch BlueBox sample database SQL files from GitHub.
  # NB: pinned to commit 988c7ce7a81a3205c799d26a9934ec6c9df01914 (2025-11-03):
  # the bluebox_schema_v0.4.sql / bluebox_dataonly_v0.4.sql.zip files were later
  # removed from the default branch (the repo was restructured in Feb 2026), so
  # "main" is a moving target. These hashes match that commit's files exactly.
  schema = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/ryanbooz/bluebox/988c7ce7a81a3205c799d26a9934ec6c9df01914/bluebox_schema_v0.4.sql";
    sha256 = "sha256-1LgfPkaVXG08rlusEclOHKsKaTOzLW8i+vzC0zRnxbM=";
  };

  data = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/ryanbooz/bluebox/988c7ce7a81a3205c799d26a9934ec6c9df01914/bluebox_dataonly_v0.4.sql.zip";
    sha256 = "sha256-TXm47I3UWReyZo1KbhBNTTYMsccAYgnBZcTWfcRnGUE=";
  };
}
