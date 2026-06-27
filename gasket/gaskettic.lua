-- title:    gasket2
-- author:   jwatt
-- script:   lua

WIDTH=240.0
HEIGHT=136.0
NUMPOINTS=5000
-- 2D table of points
local points = {}
function BOOT()
		-- 3 2 tuples of vertices
		-- the first arbitrary triangle
  local	vertices = { {0.0, 0.0},
     {WIDTH/2.0, HEIGHT},
     {WIDTH, 0.0} }
  local i,j = 0
  local p = { 50.0, 50.0 }
  -- generate new points
  for k=1, NUMPOINTS do
  	j = math.random(1, 3)
   p[1] = (p[1]+vertices[j][1])/2.0
   p[2] = (p[2]+vertices[j][2])/2.0
   points[k] = {p[1], p[2]}
  end
end
function TIC()
  cls(0)
  for k=1, NUMPOINTS do
  	ix = points[k][1]
   iy = points[k][2]
   pix(-ix+WIDTH, -iy+HEIGHT, 1)
  end
end

-- <TILES>
-- 001:eccccccccc888888caaaaaaaca888888cacccccccacc0ccccacc0ccccacc0ccc
-- 002:ccccceee8888cceeaaaa0cee888a0ceeccca0ccc0cca0c0c0cca0c0c0cca0c0c
-- 003:eccccccccc888888caaaaaaaca888888cacccccccacccccccacc0ccccacc0ccc
-- 004:ccccceee8888cceeaaaa0cee888a0ceeccca0cccccca0c0c0cca0c0c0cca0c0c
-- 017:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
-- 018:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
-- 019:cacccccccaaaaaaacaaacaaacaaaaccccaaaaaaac8888888cc000cccecccccec
-- 020:ccca00ccaaaa0ccecaaa0ceeaaaa0ceeaaaa0cee8888ccee000cceeecccceeee
-- </TILES>

-- <WAVES>
-- 000:00000000ffffffff00000000ffffffff
-- 001:0123456789abcdeffedcba9876543210
-- 002:0123456789abcdef0123456789abcdef
-- </WAVES>

-- <SFX>
-- 000:000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000304000000000
-- </SFX>

-- <TRACKS>
-- 000:100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
-- </TRACKS>

-- <PALETTE>
-- 000:1a1c2c5d275db13e53ef7d57ffcd75a7f07038b76425717929366f3b5dc941a6f673eff7f4f4f494b0c2566c86333c57
-- </PALETTE>

