#!/bin/bash

if ! command -v ffmpeg &> /dev/null; then
    echo "Error: ffmpeg is not installed."
    exit 1
fi

if [ -z "$1" ]; then
    echo "Usage: ./mpv_to_gif.sh video_file.mp4"
    exit 1
fi

INPUT_FILE="$1"
OUTPUT_FILE="${INPUT_FILE%.*}.gif"

echo "Converting $INPUT_FILE to $OUTPUT_FILE..."

ffmpeg -i "$INPUT_FILE" -vf "fps=15,split[s0][s1];[s0]palettegen[p];[s1][p]paletteuse=dither=sierra2_4a" -loop 0 "$OUTPUT_FILE"

if [ $? -eq 0 ]; then
    echo "Success! File generated: $OUTPUT_FILE"
else
    echo "An error occurred during conversion."
fi