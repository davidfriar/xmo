#! /bin/bash

 # feh --bg-scale $(http "https://api.unsplash.com/photos/random?client_id=5043befc1a6d4ca70390eeebf3896e6395ee8a774a7fd3b45ca3dd15949a89f3&w=1920&h=1080" | jq -r '.urls.custom')

 wget -O .wallpaper https://unsplash.it/1920/1080/?random
 feh --bg-scale .wallpaper

