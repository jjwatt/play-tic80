-- title:   spiralnoise2
-- author:  game developer, email, etc.
-- desc:    short description
-- site:    website link
-- license: MIT License (change this to your license of choice)
-- version: 0.1
-- script:  lua

t=0
radius = 20
centerX = 120
centerY = 75
numPoints = 150

function customRandom()
	return 1 - math.pow(math.random(), 5)
end

function mySpiral(centerx, centery, radius, color)
	startradius = radius / 10
	lastx = -999
	lasty = -999
 for angle = 0, 1440, 5 do
  startradius = startradius + 0.25
 	radians = math.rad(angle)
  local x = centerx + startradius * math.cos(radians)
  local y = centery + startradius * math.sin(radians)
  if lastx > -999 then
  	line(x, y, lastx, lasty, color)
  end
  lastx = x
  lasty = y
 end
end

function myNoiseSpiral(centerx, centery, radius, color)
	for i = 0, 10 do
		startradius = math.random(1, radius)
	 radiusNoise = math.random(10)
  startangle = math.random(0, 90)
  endangle = 360*4 + math.random(360*4)
  anglestep = 2 + math.random(1, 2)
		lastx, lasty = -999, -999
 	for angle = startangle, endangle, anglestep do
  		radiusNoise = radiusNoise + 0.08
  		thisradius = startradius
  																	+ (radiusNoise 
  			              * (1 - customRandom()))
  		startradius = startradius + 0.05 + (1-customRandom())
 			radians = math.rad(angle)
  		local x = centerx + thisradius * math.cos(radians)
  		local y = centery + thisradius * math.sin(radians)
  		if lastx > -999 then
  				line(x, y, lastx, lasty, color)
  		end
  		lastx, lasty = x, y
 		end
	end
end

function TIC() 
	-- cls(1)
	-- circb(centerX, centerY, radius+10, 14)
	-- myNoiseSpiral(centerX, centerY, radius, 4)
	-- print(math.random(), 0, 0)
	-- print(1 - customRandom(), 0, 10)
	cls(1)
 myNoiseSpiral(centerX, centerY, radius, math.random(0, 15))
	t = t + 1
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

