#define ATTR(l) layout(location = l) in

uniform mat4 MVP, View, World;
uniform vec4 Cam, Time;

ATTR(0) vec4 P;
ATTR(1) vec4 T;
ATTR(2) vec3 N;
ATTR(3) vec4 C;

out vec3 Pos, Nor, WNor, Ref;
out vec2 UV, LUV;
out vec4 Clr;

////////////////////////////////////////////////////////////////

void FS() {
	gl_Position = P; UV = P.xy * .5 + .5;
}

void wave(float div, float amp, float freq) {
	gl_Position += amp * MVP * vec4(Nor, 0) * sin(6.28 * (Time.x * freq + dot(P.xyz/div, vec3(1))));
}

////////////////////////////////////////////////////////////////

// $protect void[ \t]+([a-zGLU][_a-zA-Z0-9]*)\(\)

void Generic() {
	gl_Position = MVP * P;
	Pos = P.xyz;
	Nor = N;
	UV = T.xy;
	LUV = T.zw;
	Clr = C;
	Ref = normalize(reflect((P - Cam).xyz, N));
}
void misc_model() {
	Generic();
	Pos = floor(N) / 4.;
	Nor = fract(N) * 4. - 2.;
	Ref = normalize(reflect((P - Cam).xyz, Nor));
	float a = T.z, c = cos(a), s = sin(a);
	Nor.xy *= mat2(c, s, -s, c);
}
void UI() {
	gl_Position = vec4(P.xy, 1, 1);
	UV = T.xy;
	Clr = C;
	Clr.xyz *= Clr.w; // convert to premultiplied alpha
}
void teleport() { FS(); }
#pragma section
void icon() { FS(); }
void bglogo() { FS(); }
void uiframe() { FS(); }
void Loading() { FS(); }
void Lmapped() { Generic(); }
void fixture() { Generic(); }
void shiny() { Generic(); }
void cmet52() { FS(); }
void ptrshn() { FS(); }
void dmnd2c() { FS(); }
void dmnd2cow() { FS(); }
void dmnd2cow_m() { Generic(); }
void dmnd2pnt() { FS(); }
void dmnd2pnt_m() { Generic(); }
void mtlfw10() { FS(); }
void mtlfw15() { FS(); }
void mtlfw15ow() { FS(); }
void mtlfw15ow_m() { Generic(); }
void mtlfb3() { FS(); }
void mtlt12f() { FS(); }
void mtlt6f() { FS(); }
void mtlbk03() { FS(); }
void skcpthrt() { FS(); }
void skcpthrt2() { FS(); }
void skcpthrtooz() { Generic(); }
void sktongue() { FS(); }
void sksurf8()  { FS(); }
void gskull4() { FS(); }
void gcntr2trn() { FS(); }
void gcntr2trn_m() { Generic(); }
void scmpblk17() { FS(); }
void scmpblk17_m() { Generic(); }
void gskdr() { FS(); }
void gskdr_a() { FS(); }
void gskdr_b() { FS(); }
void gskdr_c() { FS(); }
void gskdr_d() { FS(); }
void gskdr_e() { FS(); }
void gskdr_f() { FS(); }
void gspbdrbb() { FS(); }
void gkarntwr4a() { FS(); }
void gkarntwrst() { FS(); }
void gkarnclma2r() { FS(); }
void gkarnarcfnltp() { FS(); }
void gkarnarcfnlmd() { FS(); }
void gkarnarcfnlbt() { FS(); }
void gkblkgmtrn() { FS(); }
void gdmnblk15fx() { FS(); }
void gtbsbrd09e() { FS(); }
void gtbsbrd09e2() { FS(); }
void giron01e() { FS(); }
void giron01nt3() { FS(); }
void gxstrtop4() { FS(); }
void gxstrtop4bbrn() { FS(); }
void gxstpbrdr3brn () { FS(); }
void gwdclg1a() { FS(); }
void gwdclg1bd() { FS(); }
void gsltrfc() { FS(); }
void gmtlbg6() { FS(); }
void gmtlbg6brk() { FS(); }
void glrgbk3b() { FS(); }
void glrgbk3bow() { FS(); }
void glrgbk3bow_m() { Generic(); }
void glrgbk3bbld() { FS(); }
void glrgbk3bbld_m() { Generic(); }
void gblks15() { FS(); }
void gblks18c() { FS(); }
void gklblki() { FS(); }
void gklblki4() { FS(); }
void gtprst3() { FS(); }
void gblks17f2() { FS(); }
void gmtlspsld() { FS(); }
void gmtlsp4b() { FS(); }
void gsklvtg02b() { FS(); }
void bmtsprt() { FS(); }
void cable() { FS(); }
#pragma section
void brdr11b() { FS(); }
void blt414k() { FS(); }
void lt2() { FS(); }
void gpntgmlt1k() { FS(); }
void light5() { FS(); }
void dmnd2cjp() { FS(); }
void dmnd2cjp_m() { Generic(); }
void lpdmnd() { FS(); }
void lpdmnd_m() { Generic(); }
void blacksky() { Generic(); }
void timhel() { Generic(); }
void lava() {
	Generic();
	wave(100., 3., .1);
}
void lavaf() { Generic(); }
void bwprtbnr() { FS(); }
void bwprtbnr_m() {
	Generic();
	wave(30., 3., .2);
	wave(100., 3., .7);
}
void miscmodel() { misc_model(); }
void miscmodel2s() { misc_model(); }
void storchtl() { misc_model(); }
void q3bnr() { FS(); }
void q3bnr_m() { Generic(); }
void menubnr() { FS(); }
void menubnr_m() { FS(); }
void beam() { Generic(); }
void kmlampwt() { misc_model(); }
void botflare2() { FS(); }
void botflare2_m() {
	misc_model();
	wave(100., 1., 9.);
}
void flare03() {
	Generic();
	// extract rotation from view matrix, transpose and multiply with sprite offset
	gl_Position += MVP * vec4(Nor * mat3(View), 0);
}
void botflare() { flare03(); }
void flame() { Generic(); }
void flame_large() { Generic(); }
void gr8torch2b() { FS(); }
void gr8torch2b_m() { miscmodel(); }
void lion() { FS(); }
void lion_m() { misc_model(); }
void lion_mouth() { misc_model(); }
void tlpnrg() { misc_model(); }
void tlptrns() { FS(); }
void tlptrns_m() { misc_model(); }
void tlppad() { FS(); }
void tlppad_m() { misc_model(); }
void botwing() { FS(); }
void botwing_m() { misc_model(); }
void botlamp() { FS(); }
void botlamp_m() { misc_model(); }
void botlamp2() { FS(); }
void botlamp2_m() { misc_model(); }
void gunmetal() { FS(); }
void rocketl() { FS(); }
void item() {
	Generic();
	WNor = normalize(mat3(World) * N);
	Ref = normalize(reflect((World * P - Cam).xyz, WNor));
}
void itemshiny() { item(); }
void rocketl_m() { item(); }
void ammobox() { item(); }
void ammoboxicon() { item(); }
void healthsphere() { item(); }
void plasma_gls() { item(); }
void shotgun_lzr() { item(); }
void energy_grn1() {
	item();
	gl_Position += MVP * vec4(N + N, 0);
}
void plasma_glo() { FS(); }
void plasma_glo_m() { item(); }
void armor() { item(); }
void mapselect() { FS(); }
void icon_health() { FS(); }
void icon_shard() { FS(); }
void icon_armor() { FS(); }
void icon_machinegun() { FS(); }
void icon_shotgun() { FS(); }
void icon_rocketl() { FS(); }
void icon_railgun() { FS(); }
void icon_plasma() { FS(); }
void icon_quad() { FS(); }
void crosshaira() { FS(); }
void crosshairb() { FS(); }
void crosshairc() { FS(); }
