ffmpeg -framerate 100 \
  -i output/frame%d.ppm \
  -c:v libx264 \
  -crf 25 \
  -vf "scale=36:330,format=yuv420p" \
  -movflags +faststart \
  output.mp4
