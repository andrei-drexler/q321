uniform mat4 MVP;
uniform vec4 Cam;

layout(location=0) in vec4 P;
layout(location=1) in vec4 T;
layout(location=2) in vec3 N;

out vec3 Pos, Nor, Ref;
out vec2 UV, LUV;

////////////////////////////////////////////////////////////////

void Generic() {
	 gl_Position = MVP * P;
	 Pos = P.xyz;
	 Nor = N;
	 UV = T.xy;
	 LUV = T.zw;
	 Ref = normalize(reflect((P - Cam).xyz, N));
}

void FS() {
	gl_Position = P; UV = P.xy * .5 + .5;
}

////////////////////////////////////////////////////////////////

// $protect void[ \t]+([a-zGL][_a-zA-Z0-9]*)\(\)

void cmet52() { FS(); }
void dmnd2c() { FS(); }
void dmnd2cow() { FS(); }
void dmnd2cjp() { FS(); }
void lpdmnd() { FS(); }
void ptrshn() { FS(); }
void mtlfw10() { FS(); }
void mtlfw15() { FS(); }
void mtlfw15ow() { FS(); }
void mtlfb3() { FS(); }
void mtlt12f() { FS(); }
void mtlt6f() { FS(); }
void mtlbk03() { FS(); }
void bmtsprt() { FS(); }
void brdr11b() { FS(); }
void blt414k() { FS(); }
void light5() { FS(); }
void lt2() { FS(); }
void q3bnr() { FS(); }
void Loading() { FS(); }

void Lmapped() { Generic(); }
void mtlfw15ow_m() { Generic(); }
void dmnd2cjp_m() { Generic(); }
void fixture() { Generic(); }
void beam() { Generic(); }
void q3bnr_m() { Generic(); }
void shiny() { Generic(); }