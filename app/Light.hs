module Light where

data Light = AmbientLight { intensity :: Double }
            | PointLight { intensity :: Double, position :: Position }
            | DirectionalLight { intensity :: Double, direction :: Vector }

lights = [
    AmbientLight { intensity = 0.2},
    PointLight { intensity = 0.6, position = (Position 2 1 0)},
    DirectionalLight { intensity = 0.2, direction = (Vector 1 4 4)}
    ]

computeLight point normal = loop total_intensity lights
    where
        loop total_intensity [] = intensity
        loop total_intensity (light : lights) =
            case light of
                Ambient { intensity } -> loop (total_intensity + intensity ) lights
                PointLight { intensity, position } -> 
                    let l = vectorFrom position point
                        n_dot_l     = if l `dot` normal > 0 then l `dot` normal else 0
                        normalizing_factor = (length n) * (length l)
                        intensity'  = intensity * (n_dot_l / normalizing_factor)
                    in loop (total_intensity+intensity') lights
                DirectionalLight { intensity, direction } ->
                    let l                   = direction
                        n_dot_l             = if l `dot` normal > 0 then l `dot` normal else 0
                        normalizing_factor  = (length n) * (length l)
                        intensity'          = intensity * (n_dot_l / normalizing_factor)
                    in loop (total_intensity+intensity') lights

