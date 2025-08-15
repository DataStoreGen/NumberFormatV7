--!strict
local Cleaner = require(script.ModuleCleaner).new()
local floor    = math.floor
local log      = math.log
local log10    = math.log10
local abs      = math.abs
local pow      = math.pow
local sqrt     = math.sqrt
local huge     = math.huge
local fmt      = string.format
local insert   = table.insert
local concat   = table.concat
local sub      = string.sub
local tstr     = tostring
local small = {'k', 'm', 'b'}
local first  = Cleaner:RegisterTable("first",  {"", "U", "D", "T", "Qd", "Qn", "Sx", "Sp", "Oc", "No"})
local second = Cleaner:RegisterTable("second", {"", "De", "Vt", "Tg", "qg", "Qg", "sg", "Sg", "Og", "Ng"})
local third  = Cleaner:RegisterTable("third",  {"", "Ce"})
local INF = math.huge
local NAN = 0/0
function suffixPart(index: number): string
	local hun = math.floor(index/100)
	index = index%100
	local ten, one = math.floor(index/10), index % 10
	return (first[one+1] or '') ..(second[ten+1] or '') .. (third[hun+1] or '')
end
function shortInv(man: number, ind: number): string
	if ind < 1 then return tstr(man) end
	ind = ind > #small and #small or ind
	return '1/' .. man .. small[ind]
end
function round2(val: number): number
	return floor(val*100+0.001)*0.01
end
local tableCache = {}
function Fast(val: number): {number}
	local cached = tableCache[val]
	if cached then return cached end
	if val == 0 then
		tableCache[val] = {0,0}
		return tableCache[val]
	end
	local exp = floor(log10(val))
	local res = {val/10^exp, exp}
	tableCache[val] = res
	return res
end
local Number = {}
function Number.me(v1: number, v2: number): boolean return v1 > v2 end
function Number.le(v1: number, v2: number): boolean return v1 < v2 end
function Number.eq(v1: number, v2: number): boolean return v1 == v2 end
function Number.leeq(v1: number, v2: number): boolean return v1 <= v2 end
function Number.meeq(v1: number, v2: number): boolean return v1 >= v2 end
function Number.floor(v: number): number return round2(v) end
function Number.add(v1: number, v2: number, r: boolean?) local v = v1 + v2 return r and round2(v) or v end
function Number.sub(v1: number, v2: number, r: boolean?) local v = v1 - v2 if v <= 0 then return 0 end return r and round2(v) or v end
function Number.mul(v1: number, v2: number, r: boolean?) local v = v1 * v2 return r and round2(v) or v end
function Number.div(v1: number, v2: number, r: boolean?) local v = v1 / v2 return r and round2(v) or v end
function Number.mod(v1: number, v2: number, r: boolean?) local v = v1 % v2 return r and round2(v) or v end
function Number.pow(v1: number, v2: number, r: boolean?) local v = v1^v2 return r and round2(v) or v end
function Number.log(v: number, r: boolean?) local res = log(v) return r and round2(res) or res end
function Number.logx(v1: number, v2: number, r: boolean?) local res = log(v1, v2) return r and round2(res) or res end
function Number.log10(v: number, r: boolean?) return Number.logx(v, 10, r) end
function Number.clamp(val: number, m1: number, m2: number)
	if m1 > m2 then m1, m2 = m2, m1 end
	if val < m1 then return m1 elseif val > m2 then return m2 end
	return val
end
function Number.min(v1: number, v2: number) return v1 < v2 and v1 or v2 end
function Number.max(v1: number, v2: number) return v1 > v2 and v1 or v2 end
function Number.fact(v: number)
	if v == 0 then return 1 end
	local res = 1
	for i =2, v do res*=i end
	return res
end
function Number.Comma(val: number): string
	if val < 1e3 then return tstr(val) end
	local s = tstr(floor(val))
	local n = #s
	local out = {}
	local c = 0
	for i = n, 1, -1 do
		c+=1
		out[#out+1] = sub(s, i, i)
		if c == 3 and i > 1 then
			out[#out+1] = ','
			c=0
		end
	end
	for i = 1, #out // 2 do
		out[i], out[#out-i+1] = out[#out-i+1], out[i]
	end
	return concat(out)
end
function Number.toTable(v: number): {number} return Fast(v) end
function Number.toNumber(v: {number}): number return v[1] * (10^v[2]) end
function Number.toNotation(val, r: boolean?): string
	local man, exp = table.unpack(Fast(val))
	return (r and tstr(round2(man)) or tstr(man)) .. 'e' .. tstr(exp)
end
function Number.between(v1: number, v2: number, v3: number): boolean return v1 > v2 and v1 < v3 end
function Number.short(v: number): string
	if v == 0 then return '0' end
	local man, exp = table.unpack(Fast(abs(v)))
	if abs(v) < 1 then
		local absExp = -exp
		if absExp < 3 then return '1/' .. round2(abs(v)*10^absExp) end
		local ind = floor(absExp/3)
		local rem = absExp%3
		if ind > 101 then return '1/inf' end
		local m = round2(man*10^rem)
		return shortInv(m, ind)
	else
		if exp < 3 then return tstr(round2(v)) end
		local ind = floor(exp/3)-1
		local rem = exp%3
		man = round2(man*10^rem)
		if ind > 101 then return 'inf' end
		if ind < #small -1 then
			return tstr(man) .. small[ind+1]
		else
			return tstr(man) .. suffixPart(ind)
		end
	end
end
function Number.shortE(v: number, r: boolean?, n: number?)
	n = n or 1e6
	return abs(v) >= n :: number and Number.toNotation(v, r:: boolean):gsub("nane", "") or Number.short(v)
end
function Number.lbencode(v: number): number
	if v ~= v then return 4.5e18 elseif v == INF then return 4.6e18 elseif v == -INF then return 4.7e18 elseif v == 0 then return 4e18 end
	local mode = (v>0 and 2) or 1
	local sign_v = abs(v)
	local e = floor(log10(sign_v))
	local m = sign_v/pow(10, e)
	if m >= 10 then
		m*=0.1;e+=1
	elseif m < 1 then
		m*=10;e-=1
	end
	local encoded = mode*1e18+(e*1e14)+(log10(m)*1e13)
	if mode == 1 then
		encoded=1e17-encoded
	end
	return encoded
end
function Number.lbdecode(v: number): number
	if v == 4.5e18 then return NAN	elseif v == 4.6e18 then return INF elseif v == 4.7e18 then return -INF elseif v == 4e18 then return 0 end
	local mode = floor(v / 1e18)
	if mode == 1 then
		local t = 1e17 - v
		local e = floor(t / 1e14)
		local m = pow(10, (t % 1e14) / 1e13)
		return -m * pow(10, e)
	elseif mode == 2 then
		local t = v - 2e18
		local e = floor(t / 1e14)
		local m = pow(10, (t % 1e14) / 1e13)
		return m * pow(10, e)
	end
	return INF
end
function Number.getCurrent(val: number, oldVal: number)
	local new = val
	if oldVal then
		local old = Number.lbdecode(oldVal)
		new = Number.max(new, old)
	end
	return Number.lbencode(new)
end
Cleaner:RegisterTable('Number', Number)
return Number