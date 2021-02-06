module Light where

data Light = AmbientLight { intensity :: Double }
            | PointLight { intensity :: Double, position :: Position }
            | DirectionalLight { intensity :: Double, direction :: Vector }



computeLight point normal view_vector material = loop total_intensity lights
    where
        loop total_intensity [] = intensity
        loop total_intensity (light : lights) =
            case light of
                Ambient { intensity } -> loop (total_intensity + intensity ) lights

                PointLight { intensity, position } -> 
                    let l = vectorFrom position point
                        intensity'  = find_intensity l intensity
                    in loop (total_intensity+intensity') lights
                
                DirectionalLight { intensity, direction } ->
                    let l                   = direction
                        intensity'          = find_intensity l intensity
                    in loop (total_intensity+intensity') lights

        find_intensity incident_ray intensity = 
            case material, shadow of
                _, True                 -> 0
                Matte,_                 -> diffuse_intensity
                Shiny{specularity},_    -> diffuse_intensity + specular_intensity
            where
                l                   = incident_ray
                n_dot_l             = if l `dot` normal > 0 then l `dot` normal else 0
                normalizing_factor  = (length n) * (length l)
                diffuse_intensity   = intensity * (n_dot_l / normalizing_factor)

                reflected_ray = 2 * normal * n_dot_l - l
                r = reflected_ray
                v = view_vector
                r_dot_v = if r `dot` v > 0 then r `dot` v else 0
                r_dot_v' = r_dot_v / (length r * length v)
                specular_intensity = intensity * (r_dot_v' `raisedTo` specularity)

                shadow = case closest_intersection point l 0.001 t_max of
                            Just _ -> True
                            Nothing -> False


