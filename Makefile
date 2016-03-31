textures = build/texture/grass.jpg build/texture/soil.jpg build/texture/heightmap.png

default: build output/elm.js output/terrain.html
	cp -r src/texture build
	@echo "\n*** Run 'make serve' and visit http://localhost:4079/terrain.html in your browser ***"

dev: 
	nodemon --exec "elm-make --warn --output src/elm.js" ./src/terrain.elm

output/elm.js: src/Terrain.elm
	elm-make src/Terrain.elm --output build/elm.js

output/terrain.html: src/terrain.html
	cp src/terrain.html build/terrain.html 

build:
	mkdir build

serve: 
	@cd build && python -m SimpleHTTPServer 4079

