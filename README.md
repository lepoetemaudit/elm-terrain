# Elm Terrain
A proof-of-concept terrain renderer using Elm WebGL

# Demo

See http://lepoetemaudit.github.io/elm-terrain

# Running locally

Clone the repository and run `make` followed by `make serve`, then navigate 
to http://localhost:4079/terrain.html

# Potential improvements

- Display sections of the terrain at lower LOD - this would allow showing
  much greater terrain distances
- Add different detail and splat textures
- Water
- Vegetation
- Better mouselook

# Credits

Textures are from http://opengameart.org/content/miramar-skybox for the skybox,
and http://seamless-pixels.blogspot.co.uk/ for the detail texture. The heightmap
was generated using L3DT - https://www.bundysoft.com/L3DT/