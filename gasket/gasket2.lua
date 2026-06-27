-- title:    gasket2
-- author:   jwatt
-- script:   lua

local WIDTH=240.0
local HEIGHT=136.0
local NUMPOINTS=5000
-- build 2D table
-- 3 2 tuples of vertices
-- the first arbitrary triangle
local vertices = { {0.0, 0.0},
                 {WIDTH/2.0, 200.0},
                 {WIDTH, 0.0} }
-- 2D table of points
local points = {}

function BOOT()
  cls(0)
  local i,j
  local p = {}
  p[1] = 50.0
  p[2] = 50.0
  -- generte NUMPOINTS new points
  for k=1,NUMPOINTS do
    -- pick a number between 1 and 3
    j = 1 + math.random() % 3
    -- subdivide the random edge into two new points
    p[1] = (p[1]+vertices[j][1])/2.0
    p[2] = (p[2]+vertices[j][2])/2.0
    -- write the points to memory
    points[k] = {}
    points[k][1] = p[1]
    points[k][2] = p[2]
  end
end

function TIC()
  for k=1,NUMPOINTS do
    ix = points[k][1]
    iy = points[k][2]
    pix(-ix+WIDTH, -iy+HEIGHT, 2)
  end
end

