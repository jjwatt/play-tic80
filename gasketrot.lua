#!lua
local WIDTH = 320.0
local HEIGHT = 200.0
local NUMPOINTS = 20000
local angle = 0.0  -- Current rotation angle
local rotationSpeed = 0.50  -- Speed of rotation in radians per frame
local centerX = WIDTH / 2.0  -- Center of rotation X
local centerY = HEIGHT / 2.0 -- Center of rotation Y
-- Original triangle vertices
local vertices = {
    {0.0, 0.0},
    {WIDTH/2.0, 200.0},
    {WIDTH, 0.0}
}
-- 2D table of points
local points = {}
-- Function to rotate a point around a center
function rotatePoint(x, y, cx, cy, angle)
    -- Translate point to origin
    local translatedX = x - cx
    local translatedY = y - cy    
    -- Rotate
    local rotatedX = translatedX * cos(angle) - translatedY * sin(angle)
    local rotatedY = translatedX * sin(angle) + translatedY * cos(angle)
    -- Translate back & return x,y
    return { rotatedX + cx, rotatedY + cy
    }
end

function setup()
    cls(0)
    local p = {50.0, 50.0}
    -- Generate NUMPOINTS new points
    for k = 1, NUMPOINTS do
        -- Pick a random vertex
        local j = 1 + rand() % 3        
        -- Calculate midpoint
        p[1] = (p[1] + vertices[j][1]) / 2.0
        p[2] = (p[2] + vertices[j][2]) / 2.0
        
        -- Store the point
        points[k] = {p[1], p[2]}
    end
end
function loop()
    -- Clear the screen each frame
    cls(0)   
    -- Update rotation angle
    angle = angle + rotationSpeed    
    -- Draw all points with rotation
    for k = 1, NUMPOINTS do
        local rotated = rotatePoint(points[k][1], points[k][2], centerX, centerY, angle)
        pset(20, rotated[1], rotated[2])
    end
end
