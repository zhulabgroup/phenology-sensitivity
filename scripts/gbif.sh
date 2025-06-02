#!/bin/bash

URL_FILE="multimedia.txt"
OUTPUT_DIR="downloads"
mkdir -p "$OUTPUT_DIR"

# Function to download using the gbifID as the filename
download() {
    local url="$1"
    local gbif_id="$2"
    local ext="${url##*.}"
    ext="${ext%%\?*}"  # Remove query params
    wget -q --show-progress -O "${OUTPUT_DIR}/${gbif_id}.${ext}" "$url"
}

export -f download
export OUTPUT_DIR

# Get column numbers for Identifier and gbifID
header=$(head -1 "$URL_FILE")
id_col=$(echo "$header" | tr '\t' '\n' | grep -nx "identifier" | cut -d: -f1)
gbif_col=$(echo "$header" | tr '\t' '\n' | grep -nx "gbifID" | cut -d: -f1)

if [ -z "$id_col" ] || [ -z "$gbif_col" ]; then
    echo "Error: Required columns not found. Make sure 'identifier' and 'gbifID' exist in the header."
    exit 1
fi

# Extract the needed columns: Identifier (URL) and gbifID (filename)
tail -n +2 "$URL_FILE" | \
    awk -F '\t' -v id="$id_col" -v gbif="$gbif_col" '{print $id "\t" $gbif}' | \
    parallel -j 8 --colsep '\t' download {1} {2}
