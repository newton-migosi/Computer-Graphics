module Scene where

import Light
import RayTracing

lights = [
    AmbientLight { intensity = 0.2},
    PointLight { intensity = 0.6, position = (Position 2 1 0)},
    DirectionalLight { intensity = 0.2, direction = (Vector 1 4 4)}
    ]

objects = [
    Sphere { center= (0,-1,3), radius=1, color=(255,0,0), material=Shiny{specularity=500}},
    Sphere { center= (2,0,4), radius=1, color=(0,0,255), material=Shiny{specularity=500}},
    Sphere { center= (-2,0,4), radius=1, color=(0,255,0), material=Shiny{specularity=10}},
    Sphere { center= (0,-5001,0), radius=5000, color=(255,255,0), material=Shiny{specularity=1000}},
]
