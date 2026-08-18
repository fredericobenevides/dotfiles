#!/bin/bash

if ! command -v ffmpeg &> /dev/null; then
    echo "Error: ffmpeg is not installed."
    exit 1
fi

if [ -z "$1" ]; then
    echo "Usage: ./mpv_to_gif.sh video_file.mp4 [width] [fps]"
    echo "  width: scale width in px (default: 640)"
    echo "  fps:   frames per second (default: 15)"
    exit 1
fi

INPUT_FILE="$1"
OUTPUT_FILE="${INPUT_FILE%.*}.gif"
WIDTH="${2:-640}"
FPS="${3:-15}"

echo "Converting $INPUT_FILE to $OUTPUT_FILE (${WIDTH}px, ${FPS}fps)..."

ffmpeg -i "$INPUT_FILE" \
    -vf "scale=${WIDTH}:-1:flags=lanczos,fps=${FPS},split[s0][s1];[s0]palettegen=max_colors=128:stats_mode=diff[p];[s1][p]paletteuse=dither=sierra2_4a" \
    -loop 0 "$OUTPUT_FILE"

if [ $? -eq 0 ]; then
    SIZE=$(du -h "$OUTPUT_FILE" | cut -f1)
    echo "Success! File generated: $OUTPUT_FILE ($SIZE)"
else
    echo "An error occurred during conversion."
fi