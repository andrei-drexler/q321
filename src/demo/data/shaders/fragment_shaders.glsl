uniform vec4 Time, Cam;
uniform sampler2D Texture0, Texture1;

in vec3 Pos, Nor, Ref;
in vec2 UV, LUV;
in vec4 Clr;

out vec4 FCol;

////////////////////////////////////////////////////////////////

#define smoothen(x) ((x)*(x)*(3.-2.*(x)))
#define sqr(x)		(x)*(x)
#define lsq(x)		dot(x, x)
#define sat(x)		clamp(x, 0., 1.)
#define RGB(r,g,b)	(vec3(r,g,b)/255.)

float
	PI		= 3.1415927,
	TAU		= 2. * PI,
	PHI		= 1.618034;

// http://extremelearning.com.au/unreasonable-effectiveness-of-quasirandom-sequences/
vec2 R2(float i) {
	float G = 1.324718;
	return fract(.5 + i / vec2(G, G * G));
}

float R1(float i) {
	return fract(.5 + i * PHI);
}

////////////////////////////////////////////////////////////////

float mn(vec2 v) {
	return min(v.x, v.y);
}

float mn(vec3 v) {
	return min(v.x, min(v.y, v.z));
}

float mn(vec4 v) {
	return min(min(v.x, v.y), min(v.z, v.w));
}

float mx(vec2 v) {
	return max(v.x, v.y);
}

float mx(vec3 v) {
	return max(v.x, max(v.y, v.z));
}

float mx(vec4 v) {
	return max(max(v.x, v.y), max(v.z, v.w));
}

float sum(vec2 v) {
	return v.x + v.y;
}

////////////////////////////////////////////////////////////////

vec2 safe_normalize(vec2 v) {
	float l = dot(v, v);
	return l > 0. ? v/sqrt(l) : v;
}

////////////////////////////////////////////////////////////////

float tri(float center, float max_dist, float x) {
	return 1. - sat(abs(x - center) / max_dist);
}

float ls(float lo, float hi, float x) {
	return sat((x - lo) / (hi - lo));
}

////////////////////////////////////////////////////////////////

vec2 sc(float x) {
	return vec2(sin(x), cos(x));
}

mat2 rot(float x) {
	vec2 v = sc(radians(x));
	return mat2(v.y, v.x, -v.x, v.y);
}

// UV distortions //////////////////////////////////////////////

// p = integral number of half periods
// s = scale
vec2 wavy(vec2 uv, float p, float s) {
	return uv + sin(uv.yx * PI * p) * s;
}

// t = time offset
vec2 wavy(vec2 uv, float t, float p, float s) {
	return uv + sin(uv.yx * PI * p + t) * s;
}

float mirr(float v, float m) {
	return m - abs(v - m);
}

vec2 mirr(vec2 v, float m) {
	v.x = mirr(v.x, m);
	return v;
}

// Running bond: s = (rows, columns)
vec2 brick(vec2 uv, vec2 s) {
	uv.x += floor(uv.y * s.y) * (.5 / s.x);
	return fract(uv) * s;
}

// Inputs:
//   - uv in [0..1]
//   - r = radius, in [0..1]
// Output:
//   - xy = offset
//   - z  = edge ratio in [0..1]
vec3 brick_edge(vec2 uv, float r) {
	return vec3(uv -= clamp(uv, r, 1. - r), length(uv) / r);
}

// Inputs:
//   - uv in [0..1]
//   - s = (rows, columns)
//   - r = radius, in [0..1]
// Output:
//   - xy = offset
//   - z  = edge ratio in [0..1]
vec3 brick_edge(vec2 uv, vec2 s, float r) {
	s = s.yx / mn(s);
	uv *= s;
	return vec3(uv -= clamp(uv, vec2(r), s - r), length(uv) / r);
}

////////////////////////////////////////////////////////////////

// uint Hu(uvec4 u) {
//     u *= 0x45d9f3bu;
//     uint r = u.x;
// 	r ^= u.y + 0x9e3779b9u + (r << 6) + (r >> 2);
// 	r ^= u.z + 0x9e3779b9u + (r << 6) + (r >> 2);
// 	r ^= u.w + 0x9e3779b9u + (r << 6) + (r >> 2);
//     // xorshift step
//     //r ^= r << 13;
//     //r ^= r >> 17;
//     //r ^= r << 5;
//     return r;
// }

// float Hu(vec4 v) {
//     uint k = 4095u, u = Hu(floatBitsToUint(v)) & k;
//     return float(u & k) / float(k);
// }

// Dave Hoskins/Hash without Sine
// https://www.shadertoy.com/view/4djSRW

float H(vec2 p) {
	vec3 q = fract(p.xyx * .09153);
	q += dot(q, q.yzx + 19.19);
	return fract((q.x + q.y) * q.z);
}

float H(float p) {
	p = fract(p * .1031);
	p *= p + 33.33;
	p *= p + p;
	return fract(p);
}

vec3 H3(float p) {
   vec3 p3 = fract(vec3(p) * vec3(.1031, .1030, .0973));
   p3 += dot(p3, p3.yzx+33.33);
   return fract((p3.xxy+p3.yzz)*p3.zyx); 
}

vec2 H2(vec2 p) {
	vec3 p3 = fract(vec3(p.xyx) * vec3(.1031, .1030, .0973));
	p3 += dot(p3, p3.yzx+33.33);
	return fract((p3.xx+p3.yz)*p3.zy);
}

vec4 H4(float p) {
	vec4 p4 = fract(vec4(p) * vec4(.1031, .1030, .0973, .1099));
	p4 += dot(p4, p4.wzxy + 33.33);
	return fract((p4.xxyz + p4.yzzw) * p4.zywx);
}

vec4 H4(vec2 p) {
	vec4 p4 = fract(vec4(p.xyxy) * vec4(.1031, .1030, .0973, .1099));
	p4 += dot(p4, p4.wzxy+33.33);
	return fract((p4.xxyz+p4.yzzw)*p4.zywx);
}

float HT(float x, float p) {
	return H(mod(x, p));
}

float N(float x) {
	float i;
	return mix(H(i = floor(x)), H(i + 1.), smoothen(x - i));
}

float NT(float x, float p) {
	float i;
	return mix(HT(i = floor(x), p), HT(i + 1., p), x - i);
}

////////////////////////////////////////////////////////////////
//  Cellular noise code by Brian Sharpe
//  https://briansharpe.wordpress.com/
//  https://github.com/BrianSharpe/GPU-Noise-Lib
//
//  Modified to add tiling
////////////////////////////////////////////////////////////////

//
//	FAST32_hash
//	A very fast hashing function.  Requires 32bit support.
//	http://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
//
//	The 2D hash formula takes the form....
//	hash = mod( coord.x * coord.x * coord.y * coord.y, SOMELARGEfloat ) / SOMELARGEfloat
//	We truncate and offset the domain to the most interesting part of the noise.
//	SOMELARGEfloat should be in the range of 400.0->1000.0 and needs to be hand picked.  Only some give good results.
//	A 3D hash is achieved by offsetting the SOMELARGEfloat value by the Z coordinate
//

// const vec2
//     F32OFS = vec2( 26, 161 ),
//     F32SLF = vec2( 951.135664, 642.949883 );

// void F32HT( vec2 gridcell, vec2 gridsize, out vec4 h0, out vec4 h1 ) {
//     //    gridcell is assumed to be an integer coordinate
//     vec4 p = mod(gridcell.xyxy + vec4(0, 0, 1, 1), gridsize.xyxy) + F32OFS.xyxy;
//     p *= p;
//     p = p.xzxz * p.yyww;
//     h0 = fract( p / F32SLF.x );
//     h1 = fract( p / F32SLF.y );
// }

// vec4 F32HT( vec2 gridcell, vec2 gridsize ) {
//     //    gridcell is assumed to be an integer coordinate
//     vec4 p = mod(gridcell.xyxy + vec4(0, 0, 1, 1), gridsize.xyxy) + F32OFS.xyxy;
//     p *= p;
//     return fract( p.xzxz * p.yyww / F32SLF.y );
// }

// float F32NT(vec2 p, vec2 grid) {
//     p *= grid;
//     vec2 i = floor(p), f = smoothen(p - i);
//     vec4 h = F32HT(i, grid);
//     return mix(mix(h.x, h.y, f.x), mix(h.z, h.w, f.x), f.y);
// }

// float FBMT(vec2 p, vec2 scale, float gain, float lac, int lyrs) {
//     float acc = F32NT(p, scale), ow = 1., tw = 1.;
// 	for (int i=0; i<lyrs; ++i) {
// 		scale *= lac; ow *= gain;
// 		acc += F32NT(p, scale) * ow;
// 		tw += ow;
// 	}
//     return acc / tw;
// }

// float FBMT(vec2 p, vec2 scale, float gain, float lac) {
//     return FBMT(p, scale, gain, lac, 3);
// }

////////////////////////////////////////////////////////////////

// Dave Hoskins/Hash without Sine
// https://www.shadertoy.com/view/4djSRW

// Based on ^^^
// float H(vec4 p4) {
//     p4 = fract(p4 * 3.1031);
//     p4 += dot(p4, p4.wzxy + 33.33);
//     return fract((p4.x + p4.y) * p4.z);
// }

// const vec4 HASHSCALE = vec4(.1031, .1030, .0973, .1099);

// float H(vec2 p) {
// 	vec3 q = fract(p.xyx * HASHSCALE.x);
// 	q += dot(q, q.yzx + 19.19);
// 	return fract((q.x + q.y) * q.z);
// }

// vec3 H3(vec3 p) {
// 	p = fract(p * HASHSCALE.xyz);
// 	p += dot(p, p.yxz + 19.19);
// 	return fract((p.xxy + p.yxx) * p.zyx);
// }

// vec3 H3(vec2 p) {
// 	vec3 q = fract(p.xyx * HASHSCALE.xyz);
// 	q += dot(q, q.yxz + 19.19);
// 	return fract((q.xxy + q.yzz) * q.zyx);
// }

// vec3 H3(float p) {
// 	vec3 q = fract(p * HASHSCALE.xyz);
// 	q += dot(q, q.yzx + 19.19);
// 	return fract((q.xxy + q.yzz) * q.zyx); 
// }

// vec4 H4(float p) {
// 	vec4 q = fract(p * HASHSCALE);
// 	q += dot(q, q.wzxy + 19.19);
// 	return fract((q.xxyz + q.yzzw) * q.zywx);
// }

// vec4 H4(vec2 p) {
// 	vec4 q = fract(p.xyxy * HASHSCALE);
// 	q += dot(q, q.wzxy + 19.19);
// 	return fract((q.xxyz + q.yzzw) * q.zywx);
// }

////////////////////////////////////////////////////////////////



// float N(vec2 p) {
// 	vec2 i = floor(p);
// 	p -= i;
// 	p *= p * (3. - 2.*p);
// 	float	s00 = H(i),
// 			s01 = H(i + vec2(1, 0)),
// 			s10 = H(i + vec2(0, 1)),
// 			s11 = H(i + vec2(1, 1));
// 	return mix(mix(s00, s01, p.x), mix(s10, s11, p.x), p.y);
// }

// float FBM(vec2 p, float g, float lac) {
// 	float acc = N(p), ow = g, tw = 1.;
// 	for (int i=0; i<3; ++i) {
// 		p *= lac; ow *= g;
// 		acc += N(p) * ow;
// 		tw += ow;
// 	}
// 	return acc / tw;
// }

////////////////////////////////////////////////////////////////

// float HT(vec2 p, vec2 s) {
//     p *= TAU / s;
//     return H(vec4(sc(p.x), sc(p.y)));
// }

float HT(vec2 p, vec2 s) {
	return H(mod(p, s));
}

float NT(vec2 p, vec2 s) {
	p *= s;
	vec2 i = floor(p);
	p -= i;
	p *= p * (3. - 2. * p);
	float s00 = HT(i + vec2(0, 0), s);
	float s01 = HT(i + vec2(0, 1), s);
	float s11 = HT(i + vec2(1, 1), s);
	float s10 = HT(i + vec2(1, 0), s);
	return mix(mix(s00, s10, p.x), mix(s01, s11, p.x), p.y);
}

float FBMT(vec2 p, vec2 scale, float gain, float lac, int lyrs) {
	float acc = NT(p, scale), ow = 1., tw = 1.;
	for (int i=0; i<lyrs; ++i) {
		p = fract(p + PHI);
		scale *= lac; ow *= gain;
		acc += NT(p, scale) * ow;
		tw += ow;
	}
	return acc / tw;
}

float FBMT(vec2 p, vec2 scale, float gain, float lac) {
	return FBMT(p, scale, gain, lac, 4);
}

#define ridged(v) tri(.5,.5,v)

float FBMT_ridged(vec2 p, vec2 scale, float gain, float lac, int lyrs) {
	float acc = ridged(NT(p, scale)), ow = 1., tw = 1.;
	for (int i=0; i<lyrs; ++i) {
		p = fract(p + PHI);
		scale *= lac; ow *= gain;
		acc += ridged(NT(p, scale)) * ow;
		tw += ow;
	}
	return acc / tw;
}

////////////////////////////////////////////////////////////////

vec2 seg(vec2 p, vec2 a, vec2 b) {
	vec2 ab = b-a, ap = p-a;
	float t = sat(dot(ap, ab)/dot(ab, ab));
	return ab*t + a;
}

float box(vec2 p, vec2 b) {
	vec2 d = abs(p) - b;
	return min(max(d.x, d.y), 0.) + length(max(d, 0.));
}

float box1(vec2 p, vec2 b) {
	return mx(abs(p) - b);
}

float circ(vec2 p, float r) {
	return length(p) - r;
}

float elips(vec2 p, vec2 r) {
	return circ(p/r, 1.) / min(r.x, r.y);
}

float exclude(float a, float b) {
	return max(a, -b);
}

// polynomial smooth min
// http://www.iquilezles.org/www/articles/smin/smin.htm
float smin(float a, float b, float k) {
	float h = sat(.5 + 0.5 * (b - a) / k);
	return mix(b, a, h) - k * h * (1. - h);
}

vec2 grad(float x) {
	vec2 d = vec2(dFdx(x), dFdy(x));
	return d / max(length(d), 1e-8);
}

float msk(float s, float d) {
	return sat(1. - s/d);
}

float msk(float s) {
	return sat(1. - s/fwidth(s));
}

////////////////////////////////////////////////////////////////

// Hardware derivates are computed at half resolution (pixel quads).
// To get full-resolution results, we need to evaluate 3 taps.
// The macro below helps automate that process a bit.
//
// Arguments:
// - res = name of vec3 output variable to receive the results:
//   .xy = normalized gradient
//   .z  = function value
// - uv  = 2d point where function is to be evaluated

#define EVAL_GRAD(res, uv, code)								 	\
	{																\
		vec2 p[3];													\
	 	float r[3];													\
		p[0] = uv;													\
		p[1] = uv + dFdx(uv);										\
		p[2] = uv + dFdy(uv);										\
	for (int i = 0; i < 3; ++i)										\
		r[i] = code;												\
		res = vec3(safe_normalize(vec2(r[1], r[2]) - r[0]), r[0]);	\
	}

////////////////////////////////////////////////////////////////

// Output: xyz = normal, w = unclamped normalized distance
vec4 rivet(vec2 uv, float s) {
	return vec4(uv/=s, sqrt(sat(1.-lsq(uv))), length(uv) - 1.);
}

float top_light(vec3 n) {
	float l = sum(n.yz) * .7;
	return pow(sat(l), 4.) + l;
}

float rivet_shadow(vec2 uv, float s) {
	uv /= s;
	uv.y += .06;
	uv.x *= 2.;
	return ls(.3, .0, length(uv));
}

vec3 add_rivet(vec3 c, vec2 uv, float s) {
	vec4 b = rivet(uv, s);
	c *= 1. + top_light(b.xyz) * msk(b.w) * .5;
	c *= 1. - sqr(rivet_shadow(uv, 20. * s)) * (1. - msk(b.w)) * .3;
	return c;
}

////////////////////////////////////////////////////////////////

// "Asymmetric Blocks" by Shane
// https://www.shadertoy.com/view/Ws3GRs

vec3 pattern(vec2 p, float sc, float bv) {
	vec3 e = vec3(-1, 0, 1), r = vec3(1e5);
	vec2 ip = floor(p*sc), tileID = e.yy;
	p -= (ip + .5) / sc; 

	float h11 = .5 * HT(ip + e.yy, vec2(sc));
	float h10 = .5 * HT(ip + e.xy, vec2(sc));
	float h01 = .5 * HT(ip + e.yz, vec2(sc));
	float h12 = .5 * HT(ip + e.zy, vec2(sc));
	float h21 = .5 * HT(ip + e.yx, vec2(sc));
	float h00 = .5 * HT(ip + e.xz, vec2(sc));
	float h02 = .5 * HT(ip + e.zz, vec2(sc));
	float h22 = .5 * HT(ip + e.zx, vec2(sc));
	float h20 = .5 * HT(ip + e.xx, vec2(sc));

	vec2[4] ctr, l;
	if (mod((ip.x + ip.y), 2.) < .5) { 
		l[0] = 1. + vec2(h01 - h10, h00 - h11);
		l[1] = 1. + vec2(-h01 + h12, h02 - h11);
		l[2] = 1. + vec2(-h21 + h12, -h22 + h11);
		l[3] = 1. + vec2(h21 - h10, -h20 + h11);
		ctr[0] = vec2(h01, h11) + l[0]*vec2(-.5, .5);
		ctr[1] = vec2(h01, h11) + l[1]*vec2(.5, .5);
		ctr[2] = vec2(h21, h11) + l[2]*vec2(.5, -.5);
		ctr[3] = vec2(h21, h11) + l[3]*vec2(-.5, -.5); 
	} else { 
		l[0] = 1. + vec2(-h00 + h11, h01 - h10);
		l[1] = 1. + vec2(h02 - h11, h01 - h12);
		l[2] = 1. + vec2(h22 - h11, -h21 + h12);
		l[3] = 1. + vec2(-h20 + h11, -h21 + h10);
		ctr[0] = vec2(h11, h10) + l[0]*vec2(-.5, .5);
		ctr[1] = vec2(h11, h12) + l[1]*vec2(.5, .5);
		ctr[2] = vec2(h11, h12) + l[2]*vec2(.5, -.5);
		ctr[3] = vec2(h11, h10) + l[3]*vec2(-.5, -.5); 
	}

	for (int i=0; i<4; i++) {
		l[i] /= sc;
		float bx = box1(p - ctr[i]/sc, l[i]/2. - bv/sc);
		if (bx < r.x)
			r = vec3(bx, ip + ctr[i]);
	}

	return r;
}

vec2 voro1(vec2 p, vec2 grid) {
	p *= grid;
	vec2 n = floor(p), f = p - n, mr, g, o, r;

	float md = 8.0, d;
	for (int i=0; i<9; ++i) {
		g = vec2(i % 3 - 1, i / 3 - 1);
		o = H2(mod(n + g, grid));
		r = g + o - f;
		d = sum(abs(r));

		if (d < md) {
			md = d;
			mr = r;
		}
	}

	return mr;
}

float env(vec3 p) {
	p = normalize(p);
	vec3 a = mod(degrees(atan(p, p.yzx)), 360.);
	return NT(a.x / 8., 45.) * ls(.9, .0, abs(p.z)) + NT(a.y / 8., 45.) * ls(.7, .0, abs(p.x));
}

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

vec2 uvmap(vec3 p, int ax) {
	return (ax==0) ? p.yz : (ax==1) ? p.xz : p.xy;
}

int dom(vec3 n) {
	n = abs(n) + vec3(.01, .02, .03);
	float m = mx(n);
	return (m==n.x) ? 0 : (m==n.y) ? 1 : 2;
}

// https://www.shadertoy.com/view/MsS3Wc
vec3 hsv(vec3 c) {
	vec3 rgb = sat(abs(mod(c.x * 6. + vec3(0, 4, 2), 6.) - 3.) - 1.);
	rgb *= rgb * (3. - 2. * rgb); // cubic smoothing	
	return c.z * mix(vec3(1.), rgb, c.y);
}

vec3 Light() {
	vec3 d = Cam.xyz - Pos;
	float
		b = FBMT(d.xy/256.*rot(Cam.w), vec2(3), .7, 3., 4),
		l = 1. - ls(14., -6., length(d.xy) - b * 8.) * ls(128., 48., d.z) * step(.1, Nor.z);
	return texture(Texture1, LUV).xyz * 3. * l;
}

// $protect ^void[ \t]+([_a-zA-Z][_a-zA-Z0-9]*)\(\)
// $protect ^TEX[A]?\(([a-z][_a-z0-9]*)\)

#define TEX(name)	vec3 name(vec2); void name() { FCol = vec4(name(UV), 1); } vec3 name(vec2 uv)
#define TEXA(name)	vec4 name(vec2); void name() { FCol = name(UV); } vec4 name(vec2 uv)

TEX(cmet52) {
	float b = FBMT(uv, vec2(5), .9, 3., 4);
	vec3 c = mix(RGB(48, 41, 33), RGB(103, 101, 104), b);
	return c;
}

TEX(ptrshn) {
	float b = FBMT(uv, vec2(3), .9, 3., 4);
	vec3 c = mix(RGB(49, 45, 43), RGB(81, 75, 78), b * b);
	return c;
}

TEX(dmnd2c) {
	float b = FBMT(uv, vec2(7), .9, 3.);
	uv.x *= -1.5;
	uv.y += uv.x * .5;
	uv.x = 1. - uv.x + uv.y;
	uv = fract(uv * 28.);
	float f = sat(1. - length(.1 - uv));
	f *= ls(.6, .2, length(.6 - uv));
	f *= ls(.6, .8, length(.1 - uv));
	f *= ls(.2, .6, b) * 2. + 1.;
	float l = 1. - ls(.2, b + 2., mx(abs(uv - .5)));
	return vec3((f + 1.) * mix(.21, .29, b * b) * l);
}

TEX(dmnd2cow) {
	float b = FBMT(uv, vec2(7), .9, 3.);
	vec3 c = dmnd2c(uv);
	float r = length(uv - .5);
	c = mix(c, c * RGB(70, 61, 53), ls(.5, .2, r + b*b*b));
	return c;
}

// uv in [-0.5..0.5]
float pentagram(vec2 uv, float s) {
	float d = 1e6, i = 0.;
	for (/**/; i < 5.; ++i) {
		vec2 p = vec2(0, -s) * rot(i * 72.);
		d = min(d, length(uv - seg(uv, p, p * rot(144.))));
	}
	return d;
}

TEXA(dmnd2pnt) {
	vec3 c = dmnd2cow(uv);
	uv = fract(uv) - .5;
	float b = FBMT(uv, vec2(3), .9, 3.), d = min(abs(length(uv) - .4), pentagram(uv, .35));
	return vec4(c, msk(d - .02 + b * .02, .01));
}

void dmnd2pnt_m() {
	vec4 c = texture(Texture0, UV);
	FCol = vec4(c.xyz * Light() + RGB(111, 55, 0) * c.w * (sin(Time.x * PI) * .5 + .5), 1);
}

TEX(dmnd2cjp) {
	float b = FBMT(uv, vec2(7), .9, 3., 4);
	vec3 c = dmnd2c(uv);
	float r = length(uv - .5);
	float m = ls(.46, .45, r);
	float l = 1.5 - 1.5 * ls(.0, .3, r * r);
	l = mix(l, 2.5, tri(.42, .07, r));
	l = mix(l, 3.5, tri(.44, .05, r));
	l = mix(l, 2.6, tri(.36, .03, r));
	float n = .3 + .2 * ls(.35, .30, r);
	l *= 1. - n * ls(.3, .7, b);
	l *= 1. - .3 * sqr(ls(.13, .05, r));
	l = mix(l, 2.5, ls(.04, .01, r));
	l -= l * tri(.03, .01, r) * .7;
	c = mix(c, RGB(68, 66, 54) * l, m);
	c *= 1.-sqr(tri(.34, .02, r));
	c *= 1.-sqr(tri(.46, .03, r));
	c *= 1.-tri(.41, .03, r) * .7;
	return c;
}

vec2 knob(vec2 uv, float s) {
	return vec2(1. - length(uv) / s, msk(length(uv) - s));
}

TEXA(lpdmnd) {
	float b = FBMT(uv, vec2(5), .9, 3.), t, o, k, r;
	vec3 c = dmnd2c(uv);
	vec2 u, v;
	u.x = abs(uv.x - .5);
	u.y = min(uv.y, .4);
	r = length(u - vec2(0, .4)) - (.18 - .06 * ls(.4, 1., uv.y));
	k = .25
		- .15 * ls(.9, .96, uv.y)
		+ .03 * sqr(ls(.82, .86, uv.y))
		+ .07 * ls(.8, .2, uv.y)
		+ .07 * sqr(ls(.35, .22, uv.y))
		- .07 * ls(.22, .0, uv.y);
	o = box(uv - vec2(.5, .5), vec2(k, .46));
	o = max(o, -box(u, vec2(.15, .03)) + .06);
	c = mix(c, vec3(.6, .55, .55) - uv.y * .3 + b * .2, msk(o));
	c *= 1. - .7 * tri(.0, .013, o);
	c *= 1. - (r / .5 - .1) * msk(o);
	t = max(r, uv.y - .96);
	o = abs(t - .02) - .03;
	o = max(o, uv.y - 1. + u.x * .5);
	o = max(o, uv.y - .96);
	c = mix(c, vec3(1, 1, .9) - uv.y * .55, tri(-.01, .01, o));
	c = mix(c, vec3(.2 * b + .1), msk(t, .01));
	c *= 1. - .2 * tri(.0, .05, t) * msk(o);
	v = knob(u = uv - vec2(.5, .4), .02);
	c *= 1. + RGB(111, 80, 70) * tri(.03, .01, length(u));
	//c *= 1. - ls(.04, .02, length(u)) * clamp(u.y / .02, -1., 1.);
	c *= 1. - .5 * tri(.02, .01, length(u));
	c = mix(c, RGB(111, 66, 44) * (v.x * 1.5 + .2), v.y);
	return vec4(c, msk(t - .03, .02));
}

void lpdmnd_m() {
	vec4 c = texture(Texture0, UV);
	vec2 uv = fract(UV);
	uv.x = abs(.5 - uv.x);
	float
		t = fract(-Time.x),
		r = length(uv - vec2(0, .4)),
		l = t * pow(max(0., 1. - r), 4.) * c.w;
   	if (t > .75)
		l += ls(.03, .01, abs(fract(uv.y + uv.x * .5 + t * 2.) - .45)) * ls(.1, .08, uv.x);
	FCol = vec4(c.xyz * Light() + RGB(180, 150, 5) * l, 1);
}

TEX(mtlfw10) {
	float b = FBMT(uv, vec2(5), .9, 3., 4);
	vec3 c = mix(RGB(44, 14, 16), RGB(93, 63, 63), b * b);
	return c;
}

// xy = tile id, z = edge
vec3 mtlfw15_d(vec2 uv) {
	float e = 3e-3, a = 0.;
	vec2 g = vec2(6), r = voro1(uv, g);
	for (int i=0; i<9; ++i)
		a += sum(abs(voro1(vec2(i % 3 - 1, i / 3 - 1) * e + uv, g) - r));
	return vec3(uv + r.xy / g, a);
}

TEX(mtlfw15) {
	float b = FBMT(uv, vec2(3), .9, 3., 4);
	vec3 c = mix(RGB(80, 70, 72), RGB(128, 120, 120), b * b);
	vec3 v = mtlfw15_d(uv);
	c *= mix(.95, 1.1, NT(v.xy, vec2(6)));
	c = mix(c, RGB(168, 128, 120), ls(.5, 1., v.z) * b * .7);
	return c;
}

TEXA(mtlfw15ow) {
	float b = FBMT(uv, vec2(3), .9, 3., 4);
	vec3 c = mix(RGB(80, 70, 72), RGB(128, 120, 120), b * b);
	vec3 v = mtlfw15_d(uv);
	float m = ls(.5, 1., v.z);
	float r = ls(.4, .2, length(.5 - fract(v.xy)));
	c *= mix(.95, 1.1, NT(v.xy, vec2(6))) - 2. * r * b * b;
	c = mix(c, RGB(168, 128, 120), m * b * .7);
	return vec4(c, m * r);
}

void mtlfw15ow_m() {
	vec4 c = texture(Texture0, UV);
	FCol = vec4(c.xyz * Light() + tri(.5, .125, fract(UV.y * .5 + Time.x * .5)) * c.w * .3, 1);
}

TEX(mtlfb3) {
	float b = FBMT(uv, vec2(5), .9, 3., 4);
	vec3 pt = pattern(uv, 8., .31);
	vec3 c = mix(RGB(66, 58, 55), RGB(118, 107, 105), b);
	float l = 1. - .5 * ls(.034, .036, pt.x);
	l = mix(l, 1.4, tri(.033, .004, pt.x));
	return c * l;
}

// Base SDF
float mtltech(vec2 uv) {
	float b = NT(uv, vec2(64)), f = 0., d = 1e6;
	for (/**/; f < 11.; ++f)
		d = smin(d, abs(length(.5 - abs(uv - R2(f))) - mix(.36, .29, R1(f + .7))) - mix(.015, .03, b), .01);
	return d * 1e2;
}

// Gradient (xy) + SDF (z)
vec3 mtltech_d(vec2 uv) {
	vec3 s, p;
	for (int i=0; i<3; ++i) {
		p = vec3(uv, 0);
		p[i] += 1e-4;
		s[i] = mtltech(p.xy);
	}
	return vec3(normalize(s.xy - s.z), s.z);
}

TEX(mtlt12f) {
	float b = FBMT(uv, vec2(5), .9, 3., 4), l;
	vec3 c = mix(RGB(51, 46, 43), RGB(165, 147, 143), b * b), d = mtltech_d(uv);
	l = 1. - .5 * (d.y - d.x) * tri(.5, 3., d.z) * ls(1., .0, d.z);
	return c * l * .8;
}

TEX(mtlt6f) {
	float b = FBMT(uv, vec2(3), 1.1, 3., 4), l;
	vec3 c = mix(RGB(51, 46, 43), RGB(165, 147, 143), b * b), d = mtltech_d(uv);
	l = 1. - .5 * (d.y - d.x) * tri(.5, 3., d.z) * ls(1., .0, d.z);
	return c * l;
}

// base_wall/metalblack03
TEX(mtlbk03) {
	float
		b = FBMT(uv, vec2(5), .9, 3., 4),
		l = .18 * (.7 + b * b);
	vec3 g;
	uv = wavy(uv, 13., .007);
	EVAL_GRAD(
		g, uv,
		sqr(ls(.3 + b * .2, .9, FBMT(p[i], vec2(23), .5, 2., 4)))
	);
	return vec3(l * (1. - g.y * g.z));
}

// gothic_wall/iron01_e
TEX(giron01e) {
	float
		b = FBMT(uv, vec2(5), .9, 3., 4),
		l = .18 * (.7 + b * b);
	vec3
		c = mix(RGB(77, 55, 53), RGB(62, 48, 48), NT(uv, vec2(128, 13))) * (.7 + b * b),
		g;
	uv = wavy(uv, 13., .007);
	EVAL_GRAD(
		g, uv,
		sqr(ls(.6 + b * .3, .95, NT(p[i], vec2(47,23))))
	);
	c *= ls(1.3, .9, g.z);
	return vec3(c * (1. + g.y * g.z));
}

float fender(vec2 uv, vec2 s) {
	uv.y = max(uv.y, 0.);
	return elips(uv, s);
}

vec3 techno(vec2 uv) {
	return vec3(.06 + .1 * H(uv * 133.7));
}

vec3 add_techpipe(vec3 c, vec3 b, vec2 uv, float h, float s) {
	float
		y = (uv.y - h) / s,
		p = 1. - y * y;
	c *= 1. - tri(-1., 1., y);
	if (p > 0.)
		c = b * (p * (.8 + .2 * tri(.5, .25, fract(uv.x / s)))) *
			(.7 + sqr(tri(.2, .7, y)));
	return c;
}

// TODO: PCB/chips?
vec3 techno(vec2 uv, float n) {
	return vec3(n * n * .4);
}

// gothic_wall/iron01_ntech3
TEX(giron01nt3) {
	uv.x *= .5; // fix aspect ratio (texture is 128 x 256)

	float
		b = FBMT(uv * vec2(2, 1), vec2(3, 5), .9, 3., 4), // base FBM, tileable
		n = .8 + .8 * b * b, // texture intensity
		t = uv.y + .2 * min(.5, tri(.5, .375, fract(uv.x * 4.))), // top alternating pattern
		f1 = fender(uv - vec2(.25, .62), vec2(3, 2) / 32.),
		f2 = fender(uv - vec2(.25, .55), vec2(3, 2) / 48.),
		r;

	vec3
		c = mix(RGB(66, 50, 51), RGB(111, 66, 44), sqrt(tri(.31, .01, uv.y))),
		c2;

	vec2 p = uv, q;
	p.x = fract(p.x * 4.);
	if (uv.y > .3)
		c = add_rivet(c, vec2(4. * abs(p.x - .5) - 1.6, fract(uv.y * 16.) - .5), .07);
	r = abs(p.x - .5);
	// panel shadow/highlight
	c *= 1. - .3 * ls(.31, .33, uv.y) *
		(ls(.035, .03, .5 - r) + tri(.48, .01, r) - tri(.46, .02, r));

	// techno jumble
	c = mix(c * n, techno(uv, b), max(ls(.31, .3, uv.y), msk(f2)));
	c *= ls(1.5, .7, uv.y);
	if (uv.y < .306)
		c *= 1. - tri(.3, .05, uv.y) * msk(-f2 + 10., 20.); // panel shadow
	c *= 1. - tri(.316, .004, uv.y) * msk(-f2);

	// bottom part - cables?
	if (uv.y < .1)
		c *= .0;
	q = uv;
	q.y += tri(.1, .01, mod(q.x, .33)) / 2e2;
	c = add_techpipe(c, 2. * b * RGB(93, 84, 79), uv, .185, .015);
	c = add_techpipe(c, 2. * b * RGB(138, 77, 48), uv, .13, .025);
	c = add_techpipe(c, 2. * b * RGB(112, 71, 51), uv, .09, .015);
	c = add_techpipe(c, 2. * b * RGB(138, 77, 48), q, .05, .015);

	// rectangular gizmos on top of cables
	p.x = abs(fract(uv.x * 6. - .5) - .5) / 6.;
	c *= 1.+ .5 * ls(.04, .03, p.x) * tri(.18, .03, p.y);
	r = box1(p - vec2(0, .12), vec2(.03, .01));
	r = exclude(r, box1(p - vec2(0, .11), vec2(.01)));
	c *= 1. - sqr(tri(.0, .04, r));
	c = mix(c, RGB(166, 99, 77) * 2. * b * (.75 + .5 * sqr(tri(.125, .01, uv.y))), msk(r));

	// transformer coils?
	q = p;
	q.y -= .07;
	r = circ(q, .03);
	c *= 1. - sqr(tri(.0, .07, r)); // outer shadow
	c = mix(c, RGB(127, 83, 72) * b * 2. * ls(.01, -.005, r), ls(.005, .0, r));
	q.y -= .004;
	r = circ(q, .015);
	c *= sqr(ls(-.01, .01, r)); // inner shadow
	q.y += .013;
	r = circ(q, .05);
	c += RGB(67, 38, 30) * 4. * sqrt(b) * sqr(tri(-.02, .015, r) * tri(.023, .02, uv.y)); // specular?

	// fender?
	r = exclude(f1, f2);
	r = exclude(r, (uv.y - .3) * 3e2);
	c *= 1. - .5 * tri(-2., 17., f2) * ls(.26, .3, uv.y); // inner fender shadow

	c2 = RGB(67, 39, 17) * n;
	c2 = mix(c2, vec3(n * .2), tri(0., 4., r) * b); // desaturate edges
	c2 *= 1. - .4 * pow(tri(.0, 3., r), 4.); // darken outer edge
	c2 += (c2 + .6) * sqrt(b) * sqr(tri(-6., 8., r) * tri(.66, .04, uv.y)) * msk(r); // specular
	if (uv.y < .55)
		c2 = add_rivet(c2, vec2(24. * abs(uv.x - .25) - 1.85, fract(uv.y * 24. + .5) - .5), .15);

	// top part - we also fill in the bottom row to avoid bilinear artifacts
	// (even if the original texture doesn't)
	c = mix(c, giron01e(uv), ls(.85, .9, t) + step(uv.y, 1./256.));
	c *= 1. + tri(.88, .015, t) - sqr(tri(.87, .03, t));

	return mix(c, c2, ls(1., .1, r));
}

vec3 gmtlbg6_layer(vec3 c, vec2 uv, int w, int h) {
	float b = FBMT(uv, vec2(w, h), .5, 2., 2);
	c *= .9 - .3 * ls(.15, .1, abs(b - .5));
	return mix(c, RGB(145, 140, 137), tri(.5, .1, b));
}

// gothic_floor/metalbridge06
TEX(gmtlbg6) {
	uv = wavy(uv, 9., .005);
	int i = 0, l[] = int[](13, 43, 17, 47, 23, 59, 27, 63);
	float b = FBMT(uv, vec2(19), .7, 2., 4);
	vec3 c = RGB(40, 50, 60) * (.5 + b);
	for (/**/; i < 8; i += 2)
		c = gmtlbg6_layer(c, uv, l[i], l[i+1]);
	return c;
}

TEX(glrgbk3b) {
	float
		b = FBMT(wavy(uv, 5., .02), vec2(5), 1., 3., 5),
		n = NT(uv, vec2(13));
	vec2
		p = brick(uv, vec2(8)),
		q = fract(p),
		id = p - q;
	vec3 c = RGB(91, 61, 42) * (.8 + .8 * b * b);
	c = mix(c, RGB(70, 30, 15) * (.8 + .8 * b * b), brick_edge(q, .1).z * .3);
	c *= 1. - sqr(brick_edge(q, .01 + b * .05).z) * n * b * b;
	c *= 1. + tri(.4, .3, brick_edge(q, .01 + b * .07).z) * sqrt(b) * .3;
	c *= .9 + .2 * H(id) * (1. - brick_edge(q, .1).z);
	c *= .9 + .4 * pow(FBMT_ridged(uv - H(id / 8.), vec2(5), .6, 2., 4), 4.);
	return c;
}

TEX(gblks15) {
	float
		b = FBMT(uv, vec2(5), .9, 3., 4),
		t = FBMT_ridged(wavy(uv, 4., .01), vec2(7), .5, 3., 5),
		n = NT(wavy(uv, 4., .05), vec2(9)),
		id, e;
	vec3
		pt = pattern(uv, 4., .1 + n * t * .05),
		c;
	vec2 d = grad(pt.x);
	id = H(fract(pt.yz));
	c = RGB(74, 65, 62) * (.8 + .8 * b * b);
	c += tri(.6, .3, n) * ls(.3, .9, b * t) * .2;
	c *= 1. - tri(.5, .4, n) * ls(.5, .7, t) * .1;
	c = mix(c, RGB(86, 74, 78), tri(.5, .1, b) * tri(.7, .3, id) * .7);
	c = mix(c, RGB(105, 90, 70), tri(.3, .1, t) * tri(.3, .3, id) * .3);
	e = tri(.015, .005 + .015 * n, pt.x) + tri(.4, .1, n * t) * .4;
	c *= 1. - b * ls(.015, .05, pt.x) * .7;
	c *= 1. + e * b * (d.y - .5) * .7;
	c *= .9 + .2 * id;
	c *= .9 + .2 * ridged(NT(uv - pt.yx, vec2(5)));
	return c;
}

// gothic_trim/pitted_rust3
TEX(gtprst3) {
	float
		b = FBMT(uv, vec2(13), .9, 3., 4),
		n = FBMT(uv, vec2(7), .9, 3., 4);
	vec3
		c = mix(RGB(60, 50, 50), RGB(87, 47, 37), sqr(ls(.7, .25, n))) * (.7 + .8 * b * b),
		g;

	uv = wavy(uv, 31., .003);
	EVAL_GRAD(
		g, uv,
		sqrt(ls(.0, .9, NT(p[i], vec2(93))))
	);

	c *= 1. - (g.y + .4) * sqr(b * g.z) * g.z;
	return c;
}

TEX(skcpthrt) {
	float b = FBMT(wavy(uv, 7., .01), vec2(9), .7, 2., 4);
	vec3 c = RGB(127, 70, 55) * (.85 + .3 * b);
	c *= 1. - .2 * sqr(ls(.2, .05, b * b));
	c *= 1. + .3 * ls(.6, .9, b);
	c *= 1. - .2 * sqr(tri(.6, .3, FBMT(wavy(uv, 5., .03), vec2(6), .6, 2., 4)));
	return c;
}

TEX(gskull4) {
	float b = FBMT(uv, vec2(13), .9, 3., 4);
	vec3 c = RGB(60, 50, 46) * (.875 + b * b);
	return c;
}

TEX(gmtlspsld) {
	float
		b = FBMT(uv, vec2(7), .9, 3., 4),
		n = FBMT(uv, vec2(5), .9, 3., 4);
	vec3 c = mix(RGB(103, 56, 53), RGB(73, 58, 71), smoothstep(.4, .5, n)) * (.75 + b * b);
	return c;
}

TEX(gmtlsp4b) {
	float
		b = FBMT(uv, vec2(13), .9, 3., 4),
		n = NT(wavy(uv, 5., .05), vec2(9)),
		d = ridged(fract(uv.x * 4.)),
		m = ls(.1, .15, d) * ls(1., .99, uv.y);
	vec3 c = RGB(51, 44, 44);
	c = mix(c, RGB(73, 55, 52), ls(.2, .2, b) * n * m);
	c = mix(c, RGB(69, 60, 66), ls(.7, .1, b) * b * m);
	c = mix(c, RGB(99, 77, 77), ls(.1, .5, n) * n * m * b * b * .3);
	c *= .6 + .3 * b + .3 * b * b;
	c *= 1. + .9 * sqr(tri(.21, .02 + .1 * n, d + b * .05)) * m * b;
	c *= 1. - sqr(ls(.49, .5, abs(uv.y - .5)));
	c *= 1. - ls(.05, .2, d) * ls(.16, .1, d);
	c *= 1. + tri(.99, .007, uv.y);
	return add_rivet(c, vec2(d - .4, fract(uv.y * 8.) - .5), .07);
}

vec3 gspbdrbb_v(vec2 uv, float s)  {
	float
		b = FBMT(uv, vec2(3, 1. + s + s), .7, 2., 4),
		d = ridged(uv.x),
		m;
	uv.y *= 2.;
	vec3 c = mix(RGB(71, 60, 58), RGB(110, 88, 77), ls(.1, .05, d)) * (.7 + .6 * b);
	c *= 1. - ls(.05, .0, uv.x) * (1. - b * b);
	c *= 1. + .5 * tri(.05, .02, uv.x);
	vec2 p = vec2(d - .35, fract(uv.y * s) - .5);
	vec4 k = rivet(p, .11);
	m = msk(k.w);
	c *= 1. - .7 * rivet_shadow(p, 1.1) * (1. - m);
	c = mix(c,
			(k.y > .0 ? RGB(128, 105, 88) : RGB(200,111,66) * ls(-.2, .7, k.z)) *
			(.4 + 2. * b * pow(sat(sum(k.yz * .7)), 4.)) *
			(1. - .6 * tri(-.1, .4, k.y)),
			m);
	return c;
}

// gothic_wall/supportborder_blue_b
TEX(gspbdrbb) {
	return gspbdrbb_v(uv, 4.);
}

// gothic_trim/km_arena1tower4_a
TEX(gkarntwr4a) {
	return gspbdrbb(uv.yx);
}

// gothic_trim/km_arena1tower_short
TEX(gkarntwrst) {
	return gspbdrbb_v(uv, 1.);
}

TEX(gxstrtop4) {
	float b = FBMT(uv, vec2(40, 5), .9, 3., 4);
	vec3 c = RGB(110, 110, 98) * (.8 + .8 * b * b);
	if (uv.y < 1./4.)
		c *= .5;
	c *= 1.
		- .4 * ls(.4, .0, b)
		+ .5 * ls(.02, .0, uv.y)
		+ .2 * tri(.24, .01, uv.y)
		;
	return c;
}

TEX(gwdclg1a) {
	vec2 p = uv, q;
	p.y *= 22.;
	q = fract(p);
	float
		b = FBMT(uv, vec2(3, 23), 1., 2., 6),
		n = FBMT(uv, vec2(3, 33), .7, 3., 4),
		id = H(p.y - q.y);
	vec3 c = RGB(92, 67, 53) * (.8 + .8 * b * b);
	c *= 1. - sqr(ls(.1, .0, min(q.y, 1. - q.y))) * b;
	c *= 1. - .2 * smoothstep(.3, .7, n);
	//c *= 1. - .5 * sqr(tri(.5, .01, fract(p.x + id))) * b;
	c *= .8 + .3 * b * id;
	return c;
}

TEX(gwdclg1bd) {
	float
		b = FBMT(uv, vec2(13), .9, 3., 4),
		x = uv.x * 16./3.;
	vec3 c = gwdclg1a(uv) * ls(.15, .21, uv.x);
	if (x < 1.)
		c = RGB(59, 48, 49) * (.7 + .6 * b);
	c *= 1. + .5 * tri(.05, .05, ridged(x));
	return add_rivet(c, vec2(abs(uv.x - 3./32.) - .07, mod(uv.y, .1) - .05), .004);
}

TEX(cable) {
	float
		b = FBMT(uv, vec2(5), .9, 3., 4),
		h = fract(uv.y * 10.);
	vec3 c = mix(RGB(53, 48, 42), RGB(38, 38, 36), b);
	c *= .6 + b * .8;
	c *= 1. - .5 * sqr(tri(.5, .5, h));
	c *= 1. + .5 * sqr(tri(.25, .25, h));
	c *= 1. + .5 * sqr(tri(.65, .35, h));
	return c;
}

TEX(bmtsprt) {
	float
		b = FBMT(uv, vec2(7, 3), .9, 3., 4),
		h = uv.y + b * .04,
		l = 1. - .15;
	vec3 c = mix(RGB(59, 48, 40), RGB(110, 108, 102), b*b);
	l = mix(l, .5, tri(.34, .05, uv.y));
	l = mix(l, .5, ls(.08, .05, abs(uv.y-.7)));
	l = mix(l, .3, tri(.7, .03, uv.y));
	l = mix(l, 1.5, tri(.01, .03, uv.y));
	l = mix(l, 2.2, tri(.89, .1, h));
	l = mix(l, 1.6, ls(.07, .04, abs(uv.y-.44)));
	l = mix(l, 2.5, tri(.5, .04, h));
	l = mix(l, 1.7, tri(.18, .04, h));
	return c * l;
}

TEX(brdr11b) {
	float b = FBMT(uv, vec2(5, 3), .9, 3., 4);
	vec3 c = mix(RGB(74, 66, 55), RGB(99, 90, 78), b*b);
	uv.x *= 2.;
	vec2 p = seg(uv, vec2(.5, .625), vec2(1.5, .625));
	float
		d = length(p - uv),
		m = ls(.22, .20, d),
		l = 1. - .15 * m;
	l = mix(l, .5, ls(.7, .9, uv.y) * m);
	l = mix(l, 1. - grad(d).y * .5, tri(.22, .04, d));
	l = mix(l, .6, sqr(tri(.19, .05, d)));
	l = mix(l, .5, ls(.05, 0., uv.y));
	l = mix(l, .5, tri(.26, .05, uv.y));
	l = mix(l, 1.7, ls(.93, 1., uv.y));
	l = mix(l, 1.7, tri(.23, .04, uv.y));
	return c * l;
}

TEXA(blt414k) {
	float b = FBMT(uv, vec2(1, 5), .4, 3., 4);
	vec3 c = mix(RGB(56, 49, 43), RGB(142, 136, 136), b);
	uv = .5 - abs(uv - .5);
	uv.y *= 4.;
	float
		a = tri(.0, .1, length(uv - seg(uv, vec2(.41, .5), vec2(.42, 3.5)))),
		d = mn(uv),
		l = 1. - .7 * max(0., 1. - d / .15);
	l *= 1. - .8 * ls(.24, .31, min(d, uv.y - .1));
	c += RGB(80, 80, 20) * a;
	return vec4(c * mix(l, 2.7, a), a);
}

TEXA(light5) {
	float b = FBMT(uv, vec2(1, 5), .4, 3., 4);
	vec3 c = mix(RGB(56, 49, 43), RGB(142, 136, 136), b);
	uv = .5 - abs(uv - .5);
	uv.y *= 8.;
	float
		d = length(uv - seg(uv, vec2(.27, .3), vec2(.27, 7.7))),
		a = tri(.0, .17, d),
		l = 1. - .5 * tri(.17, .07, d);
	c += RGB(80, 80, 20) * a;
	return vec4(c * mix(l, 2.7, a), a);
}

TEXA(lt2) {
	vec2 p = abs(uv - .5);
	float
		b = FBMT(uv, vec2(1), .4, 3., 4),
		r = length(p),
		a = ls(.37, .33, r) * (.5 + 2. * b),
		l = 1. + .0 * ls(.08, .03, abs(r - .41));
	vec3 c = mix(RGB(56, 49, 43), RGB(142, 136, 136), b);
	l = mix(l, 7., ls(.44, .1 * b, r));
	l *= 1. - .5 * sqr(tri(.46, .04, r));
	l *= 1. - .4 * sqr(tri(.36, .04, r));
	return vec4(c * l, a);
}

// gothic_light/pentagram_light1_1k
TEXA(gpntgmlt1k) {
	float
		b = FBMT(uv, vec2(5), .9, 3., 4),
		d = smin(pentagram(uv-=.5, .35), abs(circ(uv, .4)), .02),
		a = pow(msk(d - .02, .15), 8.),
		o = min(max(box(uv, vec2(.46)), -circ(uv, .51)), abs(circ(uv, .44)));
	vec3 c = RGB(76, 62, 47) * (.8 + .8 * b * b);
	c *= 1. + (b + .5) * msk(abs(o) - .01, .01);
	c *= 1. - ls(.1, .05, d) * msk(circ(uv, .4));
	return vec4(c + 1.*vec3(1, 1, .3) * a, a);
}

TEXA(icon) {
	uv.y -= .57;
	uv.x = abs(uv.x - .48);
	float d = elips(uv, vec2(.31, .12)) / 50.;
	d = max(d, -elips(uv - vec2(0, .01), vec2(.28, .07)) / 120.);
	d = max(d, -box(uv - vec2(.0, .1), vec2(.22, .12)));
	d = max(d, -box(uv - vec2(.0, .1), vec2(.09, .31)));
	d = min(d, box1(uv - vec2(.0, -.09), vec2(tri(-.09, .32, uv.y)*.04, .32)));
	d = min(d, box1(uv - vec2(.11, -.21), vec2(tri(-.07, .3, uv.y)*.03, .15)));
	uv.y += .07;
	float b = length(uv) - .47, m = msk(b);
	vec3 c = 1. - vec3(.7, 1, 1) * msk(max(.007 - d, b + .04));
	return vec4(c * m, m);
}

TEXA(q3bnr) {
	uv *= vec2(256, 64);
	uv.y += 2.;

	// Q
	float d = circ(uv - vec2(81, 30), 11.);
	d = max(d, uv.x - 80.);
	d = max(d, -circ(uv - vec2(84, 26), 9.));
	d = min(d, box(uv - vec2(73, 37), vec2(4, 9)) - 4.);
	d = max(d, -box(uv - vec2(73, 37), vec2(0, 7)) + 1.);
	// U
	d = min(d, box(uv - vec2(91.5, 47), vec2(4, 19)) - 4.);
	d = max(d, -box(uv - vec2(91.5, 47), vec2(0, 17.5)) + 1.);
	// A
	d = min(d, box(mirr(uv, 111.) - vec2(105. + ls(23., 50., uv.y) * 3., 43), vec2(3.5, 19)));
	d = min(d, box(uv - vec2(111, 32), vec2(4, 3)));
	// K
	d = min(d, box(uv - vec2(126, 37), vec2(3, 13)));
	d = min(d, box(uv - vec2(125.5 + ls(23., 50., uv.y) * 10., 44), vec2(3.5, 6)));
	d = min(d, box(uv - vec2(136.5 - ls(23., 50., uv.y) * 9., 32), vec2(3.5, 8)));
	// E
	d = min(d, box(uv - vec2(148.5, 37), vec2(7, 13)));
	d = max(d, -box(uv - vec2(155, 33), vec2(6, 3)));
	d = max(d, -box(uv - vec2(155, 43), vec2(6, 2)));
	// III
	d = min(d, box(uv - vec2(168, 37), vec2(3.5, 13)));
	d = min(d, box(uv - vec2(178., 37), vec2(3.5, 13)));
	d = min(d, box(uv - vec2(188, 37), vec2(3.5, 13)));

	d = max(d, uv.y - 50.);
	return vec4(msk(d, .8), 0, 0, H(uv * 511.));
}

void q3bnr_m() {
	vec3 c = texture(Texture0, UV * 2.).xyz * step(.5, fract(Time.x * .5));
	c = mix(c * Light(), vec3(.5, 0, 0), tri(fract(Time.x * 2.), 1./64., fract(UV.y)));
	FCol = vec4(c + env(Ref) * .25 + texture(Texture0, UV + H(Time.xx)).w * .1, 1);
}

void beam() {
	vec2 uv = fract(uvmap(Pos.xyz, dom(Nor)) / 128.);
	uv.x += Time.x / 33.;
	float b = FBMT(uv, vec2(7), .9, 2.), f = fract(Pos.z/128.-.375);
	FCol = vec4(2. * RGB(95, 85, 80) * f*f*f*f * mix(1., b, .5), 0.);
}

void flame() {
	vec2 uv = fract(UV), p = uv;
	p.y += p.y - Time.x;
	uv.x += sin(p.y * 7.) * .2 * uv.y;
	float
		n = FBMT(p + sin(p.yx * PI * 9. + vec2(0, Time.x * 9.)) * .015 + NT(p, vec2(5)) * .1, vec2(13), .4, 3., 4),
		b = box(uv - vec2(.5, .25), vec2(.05 * sqr(ls(.4, .2, uv.y)), .1)),
		m = sqr(msk(b + n * .25, .35));
	FCol = ls(.0, .4, m) * vec4(7, .5, .2, 0);
}

void Generic() {
	float l = dot(Nor, normalize(vec3(2,0,8)));
	l = l * .4 + .7;
	vec2 uv = uvmap(Pos, dom(Nor));
	vec3 c = vec3(.5);
	c *= hsv(vec3(fract(PHI * Time.w + .25), 1., 1.));
	FCol = vec4(c * l, 1);
}

void fixture() {
	vec4 c = texture(Texture0, UV);
	FCol = vec4(c.xyz * mix(Light(), vec3(1), c.w), 1);
}

void dmnd2cjp_m() {
	vec4 c = texture(Texture0, UV);
	float r = length(fract(UV) - .5);
	float s = mix(.4, 8., fract(Time.x * 1.5));
	FCol = vec4(c.xyz * Light() + RGB(240, 130, 5) * tri(.1, .05, r / s) * ls(.37, .32, r), 1);
}

void Lmapped() {
	vec3 c = texture(Texture0, UV).xyz;
	FCol = vec4(c * Light(), 1);
}

void shiny() {
	vec4 c = texture(Texture0, UV);
	c.xyz *= 1. + c.w * env(Ref);
	FCol = vec4(c.xyz * Light(), 1);
}

void timhel() {
	vec3 d = normalize(Pos - Cam.xyz);
	d.z = d.z * 4. + 2.;
	vec2 uv = normalize(d).xy * 2.;
	float b = ls(.2, 1., FBMT(uv - Time.x * vec2(.1, .2), vec2(5), .5, 2., 6));
	uv.y *= 1.5;
	float s = ls(.3, 1., FBMT(uv - Time.x * vec2(.1, .18), vec2(5), .6, 2., 6));
	FCol = vec4(vec3(b, 0, 0) + RGB(80, 30, 8) * s * s * 2., 1);
}

void lava() {
	vec2 uv = wavy(UV / 8., Time.x * .5, 2., .05);
	float b = FBMT(uv, vec2(7), .9, 2., 4);
	vec3 c = RGB(91, 22, 14) * (.2 + 1.6 * b);
	c = mix(c, RGB(144, 44, 0), tri(.6, .2, FBMT(uv, vec2(3), .7, 3., 4)));
	c = mix(c, RGB(244, 144, 66) * b * 2., sqr(tri(.55, .25, FBMT(uv, vec2(11), .5, 2., 4))));
	FCol = vec4(c * sat(mx(Light())), 1);
}

void lavaf() {
	lava();
}

void Loading() {
	FCol = texture(Texture0, (.5 + UV * 127.) / 128., 2.5);
	FCol.xyz *= .7 + .3 * NT(UV, .5/fwidth(UV));
}

void UI() {
	FCol = texture(Texture0, UV) * Clr;
}
