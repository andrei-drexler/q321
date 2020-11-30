#pragma once

////////////////////////////////////////////////////////////////
//
// Resources are defined here so that the map compiler can include
// this file without dragging in demo/engine stuff
//
// Downside is that some of these fields don't make much sense on
// their own, you also need to look in gfx_resources.h / material.h
// 
////////////////////////////////////////////////////////////////

#define DEMO_MAPS(x)\
	/*Name,				Source*/\
	x(q3dm17,			q3dm17sample.map)\
	x(q3dm1,			q3dm1sample.map)\

#define DEMO_VERTEX_ATTRIBS(x)\
	/*Name,				Type*/\
	x(Position,			vec3)\
	x(TexCoord,			vec4)\
	x(Normal,			vec3)\
	x(Color,			vec4)\

#define DEMO_SHADERS(x)\
	/*Name,					Flags*/\
	x(Generic,				MapVertexBits)\
	x(icon,					FSVertexBits)\
	x(bglogo,				FSVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::ZTestAlways|Gfx::Shader::NoCull)\
	x(uiframe,				FSVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::ZTestAlways|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(Loading,				FSVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::ZTestAlways|Gfx::Shader::NoCull)\
	x(UI,					UIVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::ZTestAlways|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(Lmapped,				MapVertexBits)\
	x(fixture,				MapVertexBits)\
	x(shiny,				MapVertexBits)\
	x(cmet52,				FSVertexBits)\
	x(ptrshn,				FSVertexBits)\
	x(dmnd2c,				FSVertexBits)\
	x(dmnd2cow,				FSVertexBits)\
	x(dmnd2cow_m,			MapVertexBits)\
	x(dmnd2pnt,				FSVertexBits)\
	x(dmnd2pnt_m,			MapVertexBits)\
	x(mtlfw10,				FSVertexBits)\
	x(mtlfw15,				FSVertexBits)\
	x(mtlfw15ow,			FSVertexBits)\
	x(mtlfw15ow_m,			MapVertexBits)\
	x(mtlfb3,				FSVertexBits)\
	x(mtlt12f,				FSVertexBits)\
	x(mtlt6f,				FSVertexBits)\
	x(mtlbk03,				FSVertexBits)\
	x(skcpthrt,				FSVertexBits)\
	x(skcpthrt2,			FSVertexBits)\
	x(skcpthrtooz,			MapVertexBits)\
	x(sktongue,				FSVertexBits)\
	x(sksurf8,				FSVertexBits)\
	x(gskull4,				FSVertexBits)\
	x(gcntr2trn,			FSVertexBits)\
	x(gcntr2trn_m,			MapVertexBits)\
	x(scmpblk17,			FSVertexBits)\
	x(scmpblk17_m,			MapVertexBits)\
	x(gskdr_a,				FSVertexBits)\
	x(gskdr_b,				FSVertexBits)\
	x(gskdr_c,				FSVertexBits)\
	x(gskdr_d,				FSVertexBits)\
	x(gskdr_e,				FSVertexBits)\
	x(gskdr_f,				FSVertexBits)\
	x(gspbdrbb,				FSVertexBits)\
	x(gkarntwr4a,			FSVertexBits)\
	x(gkarntwrst,			FSVertexBits)\
	x(gkarnclma2r,			FSVertexBits)\
	x(gkarnarcfnltp,		FSVertexBits)\
	x(gkarnarcfnlmd,		FSVertexBits)\
	x(gkarnarcfnlbt,		FSVertexBits)\
	x(gkblkgmtrn,			FSVertexBits)\
	x(gdmnblk15fx,			FSVertexBits)\
	x(gtbsbrd09e,			FSVertexBits)\
	x(gtbsbrd09e2,			FSVertexBits)\
	x(giron01e,				FSVertexBits)\
	x(giron01nt3,			FSVertexBits)\
	x(gxstrtop4,			FSVertexBits)\
	x(gxstrtop4bbrn,		FSVertexBits)\
	x(gxstpbrdr3brn,		FSVertexBits)\
	x(gwdclg1a,				FSVertexBits)\
	x(gwdclg1bd,			FSVertexBits)\
	x(gsltrfc,				FSVertexBits)\
	x(gmtlbg6,				FSVertexBits)\
	x(gmtlbg6brk,			FSVertexBits)\
	x(glrgbk3b,				FSVertexBits)\
	x(glrgbk3bow,			FSVertexBits)\
	x(glrgbk3bow_m,			MapVertexBits)\
	x(glrgbk3bbld,			FSVertexBits)\
	x(glrgbk3bbld_m,		MapVertexBits)\
	x(gblks15,				FSVertexBits)\
	x(gblks18c,				FSVertexBits)\
	x(gklblki,				FSVertexBits)\
	x(gklblki4,				FSVertexBits)\
	x(gtprst3,				FSVertexBits)\
	x(gblks17f2,			FSVertexBits)\
	x(gmtlspsld,			FSVertexBits)\
	x(gmtlsp4b,				FSVertexBits)\
	x(gsklvtg02b,			FSVertexBits)\
	x(bmtsprt,				FSVertexBits)\
	x(cable,				FSVertexBits)\
	x(brdr11b,				FSVertexBits)\
	x(blt414k,				FSVertexBits)\
	x(lt2,					FSVertexBits)\
	x(gpntgmlt1k,			FSVertexBits)\
	x(light5,				FSVertexBits)\
	x(dmnd2cjp,				FSVertexBits)\
	x(dmnd2cjp_m,			MapVertexBits)\
	x(lpdmnd,				FSVertexBits)\
	x(lpdmnd_m,				MapVertexBits)\
	x(blacksky,				MapVertexBits)\
	x(timhel,				MapVertexBits)\
	x(lava,					MapVertexBits)\
	x(lavaf,				MapVertexBits)\
	x(bwprtbnr,				FSVertexBits)\
	x(bwprtbnr_m,			MapVertexBits|Gfx::Shader::NoCull)\
	x(miscmodel,			MapVertexBits|Attrib::ColorBit)\
	x(miscmodel2s,			MapVertexBits|Attrib::ColorBit|Gfx::Shader::NoCull)\
	x(storchtl,				MapVertexBits|Attrib::ColorBit)\
	x(q3bnr,				FSVertexBits)\
	x(q3bnr_m,				MapVertexBits)\
	x(menubnr,				FSVertexBits)\
	x(menubnr_m,			FSVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::ZTestAlways|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(beam,					MapVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(kmlampwt,				MapVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::Premultiplied)\
	x(botflare2,			FSVertexBits)\
	x(botflare2_m,			MapVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(flare03,				MapVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(flame,				MapVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(flame_large,			MapVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(gr8torch2b,			FSVertexBits)\
	x(gr8torch2b_m,			MapVertexBits|Attrib::ColorBit|Gfx::Shader::NoCull)\
	x(tlpnrg,				MapVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(tlptrns,				FSVertexBits)\
	x(tlptrns_m,			MapVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(rocketl,				FSVertexBits)\
	x(item,					ModelVertexBits)\
	x(itemshiny,			ModelVertexBits)\
	x(ammobox,				ModelVertexBits)\
	x(ammoboxicon,			ModelVertexBits)\
	x(healthsphere,			ModelVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(plasma_gls,			ModelVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::Premultiplied)\
	x(shotgun_lzr,			ModelVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::NoCull|Gfx::Shader::Premultiplied)\
	x(energy_grn1,			ModelVertexBits|Gfx::Shader::NoZWrite|Gfx::Shader::Premultiplied)\
	x(plasma_glo,			FSVertexBits)\
	x(plasma_glo_m,			ModelVertexBits)\
	x(armor,				ModelVertexBits)\
	x(mapselect,			FSVertexBits)\
	x(icon_health,			FSVertexBits)\
	x(icon_shard,			FSVertexBits)\
	x(icon_armor,			FSVertexBits)\
	x(icon_machinegun,		FSVertexBits)\
	x(icon_shotgun,			FSVertexBits)\
	x(icon_rocketl,			FSVertexBits)\
	x(icon_railgun,			FSVertexBits)\
	x(icon_plasma,			FSVertexBits)\
	x(icon_quad,			FSVertexBits)\
	x(crosshaira,			FSVertexBits)\
	x(crosshairb,			FSVertexBits)\
	x(crosshairc,			FSVertexBits)\

////////////////////////////////////////////////////////////////

#define DEMO_TILES(x)\
	/*Name/Shader,			Width,	Height*/\
	x(mapselect,			256,	256)\
	x(icon_health,			64,		64)\
	x(icon_shard,			64,		64)\
	x(icon_armor,			64,		64)\
	x(icon_machinegun,		64,		64)\
	x(icon_shotgun,			64,		64)\
	x(icon_rocketl,			64,		64)\
	x(icon_railgun,			64,		64)\
	x(icon_plasma,			64,		64)\
	x(icon_quad,			64,		64)\
	x(crosshaira,			64,		64)\
	x(crosshairb,			64,		64)\
	x(crosshairc,			64,		64)\

////////////////////////////////////////////////////////////////

#define DEMO_TEXTURES(x)\
	/*Name,				Base,				ProcGenShader,			Width,	Height,	Format,		Flags*/\
	x(White,			Gfx::InvalidID,		Gfx::InvalidID,			16,		16,		BGRA8,		Gfx::Texture::Flags::Default)\
	x(Grey,				Texture::White,		Gfx::InvalidID,			16,		16,		BGRA8,		Gfx::Texture::Flags::Default)\
	x(icon,				Texture::White,		Shader::icon,			256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(Lightmap,			Texture::White,		Gfx::InvalidID,			1024,	256,	BGRA8,		Gfx::Texture::Flags::Default|Gfx::Texture::Flags::NoMips)\
	x(LevelshotZ,		Texture::White,		Gfx::InvalidID,			512,	512,	Z32F,		Gfx::Texture::Flags::ZBuffer)\
	x(Levelshot_q3dm1,	Texture::White,		Gfx::InvalidID,			512,	512,	BGRA8,		Gfx::Texture::Flags::ZBuffer|Gfx::Texture::Flags::RenderTarget)\
	x(Levelshot_q3dm17,	Texture::White,		Gfx::InvalidID,			512,	512,	BGRA8,		Gfx::Texture::Flags::ZBuffer|Gfx::Texture::Flags::RenderTarget)\
	x(Font,				Texture::White,		Gfx::InvalidID,			1024,	1024,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(cmet52,			Texture::White,		Shader::cmet52,			128,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(dmnd2c,			Texture::White,		Shader::dmnd2c,			256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(dmnd2cow,			Texture::dmnd2c,	Shader::dmnd2cow,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(dmnd2pnt,			Texture::dmnd2c,	Shader::dmnd2pnt,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(dmnd2cjp,			Texture::dmnd2c,	Shader::dmnd2cjp,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(lpdmnd,			Texture::dmnd2c,	Shader::lpdmnd,			256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(ptrshn,			Texture::White,		Shader::ptrshn,			256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlfw10,			Texture::White,		Shader::mtlfw10,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlfw15,			Texture::White,		Shader::mtlfw15,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlfw15ow,		Texture::mtlfw15,	Shader::mtlfw15ow,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlfb3,			Texture::White,		Shader::mtlfb3,			256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlt12f,			Texture::White,		Shader::mtlt12f,		128,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlt6f,			Texture::White,		Shader::mtlt6f,			256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(mtlbk03,			Texture::White,		Shader::mtlbk03,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(skcpthrt,			Texture::White,		Shader::skcpthrt,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(skcpthrt2,		Texture::skcpthrt,	Shader::skcpthrt2,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(sktongue,			Texture::White,		Shader::sktongue,		128,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(sksurf8,			Texture::White,		Shader::sksurf8,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gskull4,			Texture::White,		Shader::gskull4,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gcntr2trn,		Texture::White,		Shader::gcntr2trn,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gblks15,			Texture::White,		Shader::gblks15,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gblks18c,			Texture::gblks15,	Shader::gblks18c,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gklblki,			Texture::gblks15,	Shader::gklblki,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gklblki4,			Texture::gblks15,	Shader::gklblki4,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gkblkgmtrn,		Texture::gblks15,	Shader::gkblkgmtrn,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gdmnblk15fx,		Texture::gblks15,	Shader::gdmnblk15fx,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gtbsbrd09e,		Texture::White,		Shader::gtbsbrd09e,		256,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gtbsbrd09e2,		Texture::gtbsbrd09e,Shader::gtbsbrd09e2,	128,	64,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(scmpblk17,		Texture::gblks15,	Shader::scmpblk17,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gskdr_a,			Texture::gblks15,	Shader::gskdr_a,		64,		256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gskdr_b,			Texture::gblks15,	Shader::gskdr_b,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gskdr_c,			Texture::gblks15,	Shader::gskdr_c,		64,		256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gskdr_d,			Texture::gblks15,	Shader::gskdr_d,		64,		128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gskdr_e,			Texture::gblks15,	Shader::gskdr_e,		256,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gskdr_f,			Texture::gblks15,	Shader::gskdr_f,		64,		128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gwdclg1a,			Texture::White,		Shader::gwdclg1a,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gwdclg1bd,		Texture::gwdclg1a,	Shader::gwdclg1bd,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gsltrfc,			Texture::White,		Shader::gsltrfc,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gmtlbg6,			Texture::White,		Shader::gmtlbg6,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gmtlbg6brk,		Texture::gmtlbg6,	Shader::gmtlbg6brk,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	/* Note: original gothic_floor/largerblock3b_ow texture was 256x256, with just a section of the source*/\
	/* Note: original gothic_block/largerblock3blood texture was 256x256, with just a section of the source*/\
	x(glrgbk3b,			Texture::White,		Shader::glrgbk3b,		512,	512,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(glrgbk3bow,		Texture::glrgbk3b,	Shader::glrgbk3bow,		512,	512,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(glrgbk3bbld,		Texture::glrgbk3b,	Shader::glrgbk3bbld,	512,	512,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gtprst3,			Texture::White,		Shader::gtprst3,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gblks17f2,		Texture::White,		Shader::gblks17f2,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gmtlspsld,		Texture::White,		Shader::gmtlspsld,		128,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gmtlsp4b,			Texture::White,		Shader::gmtlsp4b,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gsklvtg02b,		Texture::White,		Shader::gsklvtg02b,		128,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gspbdrbb,			Texture::White,		Shader::gspbdrbb,		64,		256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gkarntwr4a,		Texture::White,		Shader::gkarntwr4a,		256,	64,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gkarntwrst,		Texture::White,		Shader::gkarntwrst,		64,		64,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	/* Note: original km_arena1columna2R texture was 66x640, but since it's only used here on a patch*/\
	/* (which already has normalized UVs), the exact dimensions of the texture are not important.    */\
	x(gkarnclma2r,		Texture::White,		Shader::gkarnclma2r,	64,		512,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gkarnarcfnltp,	Texture::White,		Shader::gkarnarcfnltp,	256,	64,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gkarnarcfnlmd,	Texture::White,		Shader::gkarnarcfnlmd,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gkarnarcfnlbt,	Texture::White,		Shader::gkarnarcfnlbt,	256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(giron01e,			Texture::White,		Shader::giron01e,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(giron01nt3,		Texture::giron01e,	Shader::giron01nt3,		128,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(bwprtbnr,			Texture::White,		Shader::bwprtbnr,		128,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gxstrtop4,		Texture::White,		Shader::gxstrtop4,		256,	32,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gxstrtop4bbrn,	Texture::White,		Shader::gxstrtop4bbrn,	256,	32,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gxstpbrdr3brn,	Texture::White,		Shader::gxstpbrdr3brn,	256,	16,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(bmtsprt,			Texture::White,		Shader::bmtsprt,		256,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(cable,			Texture::White,		Shader::cable,			128,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(brdr11b,			Texture::White,		Shader::brdr11b,		64,		32,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(blt414k,			Texture::White,		Shader::blt414k,		64,		256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(lt2,				Texture::White,		Shader::lt2,			64,		64,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(gpntgmlt1k,		Texture::White,		Shader::gpntgmlt1k,		64,		64,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(plasma_glo,		Texture::White,		Shader::plasma_glo,		64,		64,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(light5,			Texture::White,		Shader::light5,			16,		128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(q3bnr,			Texture::White,		Shader::q3bnr,			512,	128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(menubnr,			Texture::White,		Shader::menubnr,		1024,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget|Gfx::Texture::Flags::NoMips)\
	x(gr8torch2b,		Texture::White,		Shader::gr8torch2b,		32,		64,		BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(botflare2,		Texture::White,		Shader::botflare2,		32,		128,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(tlptrns,			Texture::White,		Shader::tlptrns,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	x(rocketl,			Texture::White,		Shader::rocketl,		256,	256,	BGRA8,		Gfx::Texture::Flags::RenderTarget)\
	/* HACK: dummy texture needed by the flame material to get proper UV mapping...*/\
	x(_128x256,			Texture::White,		Gfx::InvalidID,			128,	256,	BGRA8,		Gfx::Texture::Flags::NoMips)\
	/* HACK: dummy texture needed by the beam material to get proper UV mapping...*/\
	x(_32x128,			Texture::White,		Gfx::InvalidID,			32,		128,	BGRA8,		Gfx::Texture::Flags::NoMips)\

////////////////////////////////////////////////////////////////

#ifdef DEVxxx
	#define DEMO_FALLBACK_MATERIAL(x)\
		x("",								Generic,		White,		Solid,		Opaque,					(0))
#else
	#define DEMO_FALLBACK_MATERIAL(x)\
		x("",								Lmapped,		cmet52,		Solid,		Opaque,					(0))
#endif

#define DEMO_MATERIALS(x)\
	/*Path										Shader			Texture			Contents	Draw					Light*/\
	DEMO_FALLBACK_MATERIAL(x)\
	x("common/caulk",							Generic,		White,			Solid,		Invisible|BlocksLight,	(0))\
	x("base_wall/c_met5_2",						Lmapped,		cmet52,			Solid,		Opaque,					(0))\
	x("base_trim/pewter_shiney",				shiny,			ptrshn,			Solid,		Opaque,					(0))\
	x("base_floor/diamond2c",					Lmapped,		dmnd2c,			Solid,		Opaque,					(0))\
	x("base_floor/diamond2c_ow",				dmnd2cow_m,		dmnd2cow,		Solid,		Opaque|NeedsUV,			(0))\
	x("sfx/pentfloor_diamond2c",				dmnd2pnt_m,		dmnd2pnt,		Solid,		Opaque|NeedsUV,			(0))\
	x("base_wall/metalfloor_wall_10",			Lmapped,		mtlfw10,		Solid,		Opaque,					(0))\
	x("base_wall/metalfloor_wall_15",			Lmapped,		mtlfw15,		Solid,		Opaque|NeedsUV,			(0))\
	x("base_wall/metalfloor_wall_15ow",			mtlfw15ow_m,	mtlfw15ow,		Solid,		Opaque|NeedsUV,			(0))\
	x("base_wall/metfloor_block_3",				Lmapped,		mtlfb3,			Solid,		Opaque,					(0))\
	x("base_wall/metaltech12final",				Lmapped,		mtlt12f,		Solid,		Opaque,					(0))\
	x("base_wall/metaltech06final",				Lmapped,		mtlt6f,			Solid,		Opaque,					(0))\
	x("base_wall/metalblack03",					Lmapped,		mtlbk03,		Solid,		Opaque,					(0))\
	x("base_trim/basemetalsupport",				Lmapped,		bmtsprt,		Solid,		Opaque,					(0))\
	x("base_trim/border11b",					Lmapped,		brdr11b,		Solid,		Opaque|NeedsUV,			(0))\
	x("base_support/cable",						Lmapped,		cable,			Solid,		Opaque,					(0))\
	x("base_light/baslt4_1_4k",					fixture,		blt414k,		Solid,		Opaque|NeedsUV,			(4000,16,15,13))\
	x("base_light/lt2_2000",					fixture,		lt2,			Solid,		Opaque|NeedsUV,			(2000,16,11,12))\
	x("base_light/lt2_8000",					fixture,		lt2,			Solid,		Opaque|NeedsUV,			(8000,16,11,12))\
	x("base_light/light5_5k",					fixture,		light5,			Solid,		Opaque|NeedsUV,			(5000))\
	x("sfx/diamond2cjumppad",					dmnd2cjp_m,		dmnd2cjp,		Solid,		Opaque|NeedsUV,			(200,100,0))\
	x("sfx/launchpad_diamond",					lpdmnd_m,		lpdmnd,			Solid,		Opaque|NeedsUV,			(0))\
	x("base_wall/main_q3abanner",				q3bnr_m,		q3bnr,			Solid,		Opaque|NeedsUV,			(20,2,0,0))\
	x("gothic_floor/metalbridge06",				Lmapped,		gmtlbg6,		Solid,		Opaque,					(0))\
	x("gothic_floor/metalbridge06broke",		Lmapped,		gmtlbg6brk,		Solid,		Opaque,					(0))\
	x("gothic_floor/largerblock3b",				Lmapped,		glrgbk3b,		Solid,		Opaque,					(0))\
	x("gothic_floor/largerblock3b_ow",			glrgbk3bow_m,	glrgbk3bow,		Solid,		Opaque,					(0))\
	x("gothic_block/largerblock3blood",			glrgbk3bbld_m,	glrgbk3bbld,	Solid,		Opaque,					(0))\
	x("gothic_block/blocks15",					Lmapped,		gblks15,		Solid,		Opaque,					(0))\
	x("gothic_block/blocks18c",					Lmapped,		gblks18c,		Solid,		Opaque,					(0))\
	x("gothic_trim/pitted_rust3",				Lmapped,		gtprst3,		Solid,		Opaque,					(0))\
	x("gothic_floor/blocks17floor2",			Lmapped,		gblks17f2,		Solid,		Opaque,					(0))\
	x("gothic_trim/metalsupsolid",				Lmapped,		gmtlspsld,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_trim/metalsupport4b",				Lmapped,		gmtlsp4b,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_trim/skullsvertgray02b",			Lmapped,		gsklvtg02b,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_wall/supportborder_blue_b",		Lmapped,		gspbdrbb,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_trim/km_arena1tower4_a",			Lmapped,		gkarntwr4a,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_trim/km_arena1tower_short",		Lmapped,		gkarntwrst,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_door/km_arena1columna2R",			Lmapped,		gkarnclma2r,	Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_door/km_arena1archfinalc_top",	Lmapped,		gkarnarcfnltp,	Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_door/km_arena1archfinald_mid",	Lmapped,		gkarnarcfnlmd,	Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_door/km_arena1archfinald_bot",	Lmapped,		gkarnarcfnlbt,	Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_block/killblockgeomtrn",			Lmapped,		gkblkgmtrn,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_block/demon_block15fx",			Lmapped,		gdmnblk15fx,	Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_trim/baseboard09_e",				Lmapped,		gtbsbrd09e,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_trim/baseboard09_e2",				Lmapped,		gtbsbrd09e2,	Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_wall/iron01_e",					Lmapped,		giron01e,		Solid,		Opaque,					(0))\
	x("gothic_wall/iron01_ntech3",				Lmapped,		giron01nt3,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_floor/xstairtop4",				Lmapped,		gxstrtop4,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_floor/xstairtop4bbrn",			Lmapped,		gxstrtop4bbrn,	Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_floor/xstepborder3brn",			Lmapped,		gxstpbrdr3brn,	Solid,		Opaque|NeedsUV,			(0))\
	x("liquids/lavahellflat_400",				lavaf,			White,			Solid,		Opaque,					(400,16,5,2))\
	x("gothic_ceiling/woodceiling1a",			Lmapped,		gwdclg1a,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_ceiling/woodceiling1b_dark",		Lmapped,		gwdclg1bd,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_block/killblock_i",				Lmapped,		gklblki,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_block/killblock_i4",				Lmapped,		gklblki4,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_wall/slateroofc",					Lmapped,		gsltrfc,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_light/pentagram_light1_1k",		fixture,		gpntgmlt1k,		Solid,		Opaque|NeedsUV,			(1000,19,17,10))\
	x("skin/tongue_trans",						Lmapped,		sktongue,		Solid,		Opaque,					(0))\
	x("skin/chapthroat",						Lmapped,		skcpthrt,		Solid,		Opaque,					(0))\
	x("skin/chapthroat2",						Lmapped,		skcpthrt2,		Solid,		Opaque,					(0))\
	x("skin/chapthroatooz",						skcpthrtooz,	skcpthrt,		Solid,		Opaque,					(0))\
	x("skin/surface8_trans",					Lmapped,		sksurf8,		Solid,		Opaque,					(0))\
	x("gothic_wall/skull4",						Lmapped,		gskull4,		Solid,		Opaque,					(0))\
	x("gothic_floor/center2trn",				gcntr2trn_m,	gcntr2trn,		Solid,		Opaque|NeedsUV,			(0))\
	x("sfx/computer_blocks17",					scmpblk17_m,	scmpblk17,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_door/skull_door_a",				Lmapped,		gskdr_a,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_door/skull_door_b",				Lmapped,		gskdr_b,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_door/skull_door_c",				Lmapped,		gskdr_c,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_door/skull_door_d",				Lmapped,		gskdr_d,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_door/skull_door_e",				Lmapped,		gskdr_e,		Solid,		Opaque|NeedsUV,			(0))\
	x("gothic_door/skull_door_f",				Lmapped,		gskdr_f,		Solid,		Opaque|NeedsUV,			(0))\
	x("liquids/lavahell_750",					lava,			White,			Solid,		Opaque,					(500,16,5,2))\
	x("base_wall/protobanner",					bwprtbnr_m,		bwprtbnr,		Solid,		Opaque|NeedsUV,			(0))\
	x("*map_model",								miscmodel,		gtprst3,		Solid,		Opaque,					(0))\
	x("models/mapobjects/teleporter/pad",		miscmodel,		ptrshn,			Solid,		Opaque,					(0))\
	x("models/mapobjects/spotlamp/spotlamp",	miscmodel2s,	gtprst3,		Solid,		Opaque,					(0))\
	x("models/mapobjects/storch/storch_tall",	storchtl,		gtprst3,		Solid,		Opaque,					(0))\
	x("models/mapobjects/spotlamp/spotlamp_l",	fixture,		lt2,			Solid,		Opaque|NeedsUV,			(0))\
	x("models/mapobjects/gratelamp/gratetorch2b",	gr8torch2b_m,	gr8torch2b,	Solid,		Opaque|NeedsUV,			(0))\
	x("*item_model",							item,			rocketl,		Solid,		Opaque,					(0))\
	x("*item_shiny",							itemshiny,		rocketl,		Solid,		Opaque,					(0))\
	x("models/powerups/ammo/rockammo",			ammobox,		rocketl,		Solid,		Opaque,					(0))\
	x("models/powerups/ammo/rockammo2",			ammoboxicon,	Font,			Solid,		Opaque,					(0))\
	x("models/powerups/armor/newred",			armor,			rocketl,		Solid,		Opaque,					(0))\
	x("models/weapons2/plasma/plasma_glo",		plasma_glo_m,	plasma_glo,		Solid,		Opaque,					(0))\
	x("skies/blacksky",							blacksky,		White,			Solid,		Sky,					(0))\
	x("skies/tim_hell",							timhel,			White,			Solid,		Sky,					(0))\
	x("sfx/beam",								beam,			_32x128,		NonSolid,	Translucent|NeedsUV,	(0))\
	x("models/powerups/armor/energy_grn1",		energy_grn1,	White,			NonSolid,	Translucent,			(0))\
	x("models/mapobjects/kmlamp_white",			kmlampwt,		White,			NonSolid,	Translucent,			(0))\
	x("models/powerups/health/red_sphere",		healthsphere,	White,			NonSolid,	Translucent,			(0))\
	x("models/mapobjects/lamps/bot_flare2",		botflare2_m,	botflare2,		NonSolid,	Translucent|NeedsUV,	(0))\
	x("models/mapobjects/lamps/flare03",		flare03,		White,			NonSolid,	Translucent|NeedsUV|Sprite,	(200,6,5,4))\
	x("sfx/flame1_hell",						flame_large,	_128x256,		NonSolid,	Translucent|NeedsUV,	(600,8,6,1))\
	x("sfx/flame2",								flame,			_128x256,		NonSolid,	Translucent|NeedsUV,	(5500,8,6,1))\
	x("sfx/flame1side",							flame,			_128x256,		NonSolid,	Translucent|NeedsUV,	(0))\
	x("models/mapobjects/teleporter/energy",	tlpnrg,			White,			NonSolid,	Translucent,			(0))\
	x("models/mapobjects/teleporter/widget",	tlptrns_m,		tlptrns,		NonSolid,	Translucent,			(0))\
	x("models/weapons2/plasma/plasma_glass",	plasma_gls,		White,			NonSolid,	Translucent,			(0))\
	x("models/weapons2/shotgun/shotgun_laser",	shotgun_lzr,	White,			NonSolid,	Translucent,			(0))\
	x("common/weapclip",						Generic,		White,			Solid,		Invisible,				(0))\
	x("common/clip",							Generic,		White,			Solid,		Invisible,				(0))\
	x("common/nodraw",							Generic,		White,			Solid,		Invisible,				(0))\
	x("common/nodrawnonsolid",					Generic,		White,			NonSolid,	Invisible,				(0))\
	x("common/trigger",							Generic,		White,			NonSolid,	Invisible,				(0))\
	x("common/nodrop",							Generic,		White,			NoDrop,		Invisible,				(0))\

#define DEMO_MATERIAL_SUBSTITUTIONS(x)\
	x("gothic_block/blocks18c_3",					"gothic_block/blocks18c")\
	x("gothic_block/killblock",						"gothic_block/blocks15")\
	x("gothic_block/blocks18b",						"gothic_block/blocks15")\
	x("gothic_block/blocks11b",						"gothic_block/blocks15")\
	x("gothic_block/blocks17",						"gothic_block/blocks15")\
	x("gothic_block/blocks1",						"gothic_block/blocks15")\
	x("gothic_trim/metaldemonkillblock",			"gothic_block/demon_block15fx")\
	x("gothic_door/skullarch_a",					"gothic_door/km_arena1archfinalc_top")\
	x("gothic_door/skullarch_b",					"gothic_door/km_arena1archfinald_mid")\
	x("gothic_door/skullarch_c",					"gothic_door/km_arena1archfinald_bot")\
	x("gothic_door/xian_tourneyarch_inside2",		"gothic_trim/pitted_rust3")\
	x("gothic_trim/pitted_rust",					"gothic_trim/pitted_rust3")\
	x("gothic_trim/pitted_rust2",					"gothic_trim/pitted_rust3")\
	x("gothic_trim/pitted_rust2_trans",				"gothic_trim/pitted_rust3")\
	x("gothic_trim/km_arena1tower4",				"gothic_wall/supportborder_blue_b")\
	x("models/mapobjects/teleporter/transparency",	"models/mapobjects/teleporter/widget")\
	x("models/mapobjects/spotlamp/beam",			"sfx/beam")\
	x("models/mapobjects/lamps/bot_flare",			"models/mapobjects/lamps/flare03")\
	x("models/powerups/armor/energy_red1",			"common/nodrawnonsolid")\
	x("models/powerups/instant/quad",				"*item_shiny")\
	x("models/powerups/health/red",					"*item_shiny")\
	x("models/powerups/health/mega2",				"*item_shiny")\
	x("models/powerups/armor/shard2",				"*item_shiny")\

////////////////////////////////////////////////////////////////

#define DEMO_MODELS(x)\
	/*Path										Name*/\
	x(models/mapobjects,						kmlamp1)\
	x(models/mapobjects/storch,					tall_torch)\
	x(models/mapobjects/gratelamp,				gratetorch)\
	x(models/mapobjects/gratelamp,				gratetorchbig)\
	x(models/mapobjects/teleporter,				teleporter)\
	x(models/mapobjects/spotlamp,				spotlamp)\
	x(models/mapobjects/wallhead,				lion)\
	x(models/mapobjects/wallhead,				wallhead02)\
	x(models/mapobjects,						statue_major)\
	x(models/mapobjects,						visor_posed)\
	x(models/mapobjects/lamps,					bot_lamp2)\
	x(models/powerups/ammo,						rocketam)\
	x(models/powerups/armor,					armor_red)\
	x(models/powerups/armor,					shard)\
	x(models/powerups/health,					large_cross)\
	x(models/powerups/health,					large_sphere)\
	x(models/powerups/health,					mega_cross)\
	x(models/powerups/instant,					quad)\
	x(models/powerups/instant,					quad_ring)\
	x(models/powerups/weapons2/gauntlet,		gauntlet)\
	x(models/powerups/weapons2/gauntlet,		gauntlet_barrel)\
	x(models/powerups/weapons2/machinegun,		machinegun)\
	x(models/powerups/weapons2/machinegun,		machinegun_barrel)\
	x(models/powerups/weapons2/plasma,			plasma)\
	x(models/powerups/weapons2/railgun,			railgun)\
	x(models/powerups/weapons2/rocketl,			rocketl)\
	x(models/powerups/weapons2/shotgun,			shotgun)\

#define DEMO_MODELS_USE_DELTA_ENCODING 1

////////////////////////////////////////////////////////////////

#define DEMO_UNIFORMS(x)\
	/*Name,					Type*/\
	x(Texture0,				Gfx::Texture::ID)\
	x(Texture1,				Gfx::Texture::ID)\
	x(MVP,					mat4)\
	x(View,					mat4)\
	x(World,				mat4)\
	x(Cam,					vec4)\
	x(Time,					vec4)\
	x(Extra,				vec4)\
	x(LightColor,			vec4)\
	x(Ambient,				vec4)\
	x(LightDir,				vec4)\

////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////

namespace Demo::Material {
	constexpr u32 Version = Hash(
		#define PP_DEMO_HASH_MATERIAL_NAME(path, shader, texture, contents, draw, light)		path "*" #texture "*" #contents "*" #draw "*"
		DEMO_MATERIALS(PP_DEMO_HASH_MATERIAL_NAME)
		#undef PP_DEMO_HASH_MATERIAL_NAME
	);
}

namespace Demo::Shader {
	constexpr u32 Version = Hash(
		#define PP_DEMO_HASH_SHADER_NAME(name, ...)		#name "*"
		DEMO_SHADERS(PP_DEMO_HASH_SHADER_NAME)
		#undef PP_DEMO_HASH_SHADER_NAME
	);
}

namespace Demo::Model {
	constexpr u32 Version = Hash(
		PP_STRINGIZE(DEMO_MODELS_USE_DELTA_ENCODING)	"@"
		#define PP_DEMO_HASH_MODEL_NAME(name, ...)		#name "*"
		DEMO_SHADERS(PP_DEMO_HASH_MODEL_NAME)
		#undef PP_DEMO_HASH_MODEL_NAME
	);
}
