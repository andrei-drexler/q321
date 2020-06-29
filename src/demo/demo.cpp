//#define ENABLE_RENDERDOC
//#define DISABLE_SHADER_CACHE
#define USE_RAW_INPUT
//#define DISABLE_PARTITION
//#define DRAW_LIGHTS
//#define DRAW_POINT_ENTITIES
//#define DRAW_CLIPPING
//#define SAVE_TEXTURE gblks15
//#define SAVE_ICON
//#define SAVE_LIGHTMAP
//#define SHOW_LIGHTMAP
//#define FULLBRIGHT
//#define ENABLE_RADIOSITY
//#define START_NOCLIP

#define START_MAP					q3dm1

////////////////////////////////////////////////////////////////

#include "../engine/demo.h"
#include "console.h"
#include "resource_def.h"
#include "material.h"
#include "entity.h"
#include "packed_map.h"
#include "gfx_resources.h"
#include "models.h"
#include "cooked/cooked_models.h"
#include "cooked/cooked_maps.h"
#include "map.h"
#include "player.h"

////////////////////////////////////////////////////////////////

namespace Demo {
	static const u32 GPUPoolSizes[Gfx::Arena::Count] = {
		8  * Mem::MB,	// permanent
		16 * Mem::MB,	// level
		16 * Mem::MB,	// dynamic
	};

	Sys::Thread				g_loading_thread;
	bool					g_updated_lightmap;
	Sys::Time				g_load_time;
	Sys::Time				g_time;

	FORCEINLINE bool IsLoading() {
		return !Sys::IsThreadReady(g_loading_thread);
	}

	void GenerateLightmap(void*) {
		g_updated_lightmap = false;
		Map::ComputeLighting(Map::LightMode::Shadows);
#ifdef ENABLE_RADIOSITY
		if (r_bounce.integer)
			Map::ComputeLighting(Map::LightMode::Bounce);
#endif
	}

	void RenderSprite(const vec3& point, float size) {
		using namespace Demo;

		vec3 pos[4];
		vec3 nor[4];
		vec2 uv[4];

		Gfx::Mesh mesh;
		memset(&mesh, 0, sizeof(mesh));

		mat4 rotation = MakeRotation(g_player.angles * Math::DEG2RAD);
		vec3 x = rotation.x.xyz * size;
		vec3 y = rotation.z.xyz * size;
		vec3 z = rotation.y.xyz;

		nor[0] = nor[1] = nor[2] = nor[3] = -z;

		pos[0] = point - x - y;
		pos[1] = point + x - y;
		pos[2] = point + x + y;
		pos[3] = point - x + y;

		uv[0] = vec2{0.f, 0.f};
		uv[1] = vec2{1.f, 0.f};
		uv[2] = vec2{1.f, 1.f};
		uv[3] = vec2{0.f, 1.f};

		u32 idx[6] = { 0, 1, 2, 0, 2, 3 };

		mesh.vertices[Attrib::Position	].SetData(pos, 4);
		mesh.vertices[Attrib::TexCoord	].SetData(uv, 4);
		mesh.vertices[Attrib::Normal	].SetData(nor, 4);
		mesh.num_vertices = 4;
		mesh.SetIndices(idx, 6);

		Gfx::SetShader(Demo::Shader::Generic);
		
		Uniform::Time.w = 0;
		Uniform::Texture0 = Demo::Texture::White;

		Gfx::UpdateUniforms();
		Gfx::Draw(mesh);
	}

	////////////////////////////////////////////////////////////////

	void DrawLights() {
		for (u16 i = 0; i < Map::num_lights; ++i) {
			auto& light = Map::lights[i];
			vec3 pos;
			pos[0] = light.position[0];
			pos[1] = light.position[1];
			pos[2] = light.position[2];
			RenderSprite(light.position, max(light.intensity / 25.f, 2.f));
		}
	}

	static constexpr u8 GetEntityModel(Entity::Type type) {
		using Type = Demo::Entity::Type;

		switch (type) {
			case Type::ammo_bullets:
			case Type::ammo_rockets:
			case Type::ammo_shells:
			case Type::ammo_slugs:
			case Type::ammo_cells:
				return 1 + Demo::Model::rocketam;

			case Type::item_health:
			case Type::item_health_large:
				return 1 + Demo::Model::large_cross;

			case Type::item_health_mega:
				return 1 + Demo::Model::mega_cross;

			case Type::weapon_railgun:
				return 1 + Demo::Model::railgun;

			case Type::weapon_rocketlauncher:
				return 1 + Demo::Model::rocketl;

			case Type::weapon_shotgun:
				return 1 + Demo::Model::shotgun;

			case Type::weapon_plasmagun:
				return 1 + Demo::Model::plasma;

			case Type::item_armor_shard:
				return 1 + Demo::Model::shard;

			case Type::item_armor_body:
			case Type::item_armor_combat:
				return 1 + Demo::Model::armor_red;

			case Type::item_quad:
				return 1 + Demo::Model::quad;

			default:
				return 0;
		}
	}

	constexpr u32 MakeColor(u8 r, u8 g, u8 b, u8 a = 255) {
		return r | (g << 8) | (b << 16) | (a << 24);
	}

	constexpr u32 GetEntityModelColor(Entity::Type type) {
		using Type = Demo::Entity::Type;

		switch (type) {
			case Type::ammo_bullets:		return MakeColor(128, 112, 2);
			case Type::ammo_rockets:		return MakeColor(128, 2, 2);
			case Type::ammo_shells:			return MakeColor(128, 64, 2);
			case Type::ammo_slugs:			return MakeColor(0, 80, 32);
			case Type::ammo_cells:			return MakeColor(112, 0, 64);

			case Type::item_health:			return MakeColor(160, 128, 48);
			case Type::item_health_large:	return MakeColor(144, 88, 56);
			case Type::item_health_mega:	return MakeColor(8, 119, 234);

			case Type::item_armor_shard:	return MakeColor(80, 88, 86);

			case Type::item_armor_body:		return MakeColor(255, 0, 16);
			case Type::item_armor_combat:	return MakeColor(255, 255, 0);

			case Type::item_quad:			return MakeColor(8, 119, 234);

			default:
				return 0;
		}
	}

	static constexpr auto EntityModels      = MakeLookupTable<Entity::Type, u8,  Entity::Type{}, Entity::Type::Count>(GetEntityModel);
	static constexpr auto EntityModelColors = MakeLookupTable<Entity::Type, u32, Entity::Type{}, Entity::Type::Count>(GetEntityModelColor);

	void DrawPointEntities() {
		for (u16 i = Map::num_brush_entities; i < Map::num_entities; ++i) {
			auto& e = Map::entities[i];
			vec3 pos;
			pos[0] = e.origin[0];
			pos[1] = e.origin[1];
			pos[2] = e.origin[2];
			RenderSprite(pos, 8.f);
		}
	}

	vec3 g_weapon_offset;

	void RenderDebug() {
#ifdef DRAW_LIGHTS
		DrawLights();
#endif

#ifdef DRAW_POINT_ENTITIES
		DrawPointEntities();
#endif
	}

	////////////////////////////////////////////////////////////////

	void RenderEntities() {
		Demo::Model::Transform transform;

		for (u32 i = Map::num_brush_entities; i < Map::num_entities; ++i) {
			using Type = Demo::Entity::Type;
			Demo::Entity& ent = Map::entities[i];
			Demo::Uniform::Time.w = float(ent.type);

			MemCopy(&transform, &Demo::Model::TransformIdentity);
			for (u16 j = 0; j < 3; ++j)
				transform.position[j] = ent.origin[j];
			float phase = (transform.position[0] + transform.position[1]) / 1024.f;
			transform.position[2] += 4.f + 4.f * sin(Math::TAU * (float(g_time) + phase));
			transform.angles[0] = ent.angle + float(g_time) * 180.f;

			if (ent.IsWeapon()) {
				transform.scale = 1.5f;
				transform.position[2] += 8.f;
			}

			int model_id = int(EntityModels[ent.type]) - 1;
			u32 model_color = EntityModelColors[ent.type];
			if (model_id != -1) {
				Demo::Uniform::Time[1] = ((model_color      ) & 255) / 255.f;
				Demo::Uniform::Time[2] = ((model_color >>  8) & 255) / 255.f;
				Demo::Uniform::Time[3] = ((model_color >> 16) & 255) / 255.f;
				Demo::Model::Draw(Demo::Model::ID(model_id), transform);
			}
		}

		MemCopy(&transform, &Demo::Model::TransformIdentity);
		transform.angles = g_player.angles;

		mat4 rotation = MakeRotation(g_player.angles * Math::DEG2RAD);
		transform.position = g_player.position;
		transform.position.z -= g_player.step;
		mix_into(transform.position, Demo::Uniform::Cam.xyz, 17.f/16.f);

		const float WeaponScale = 0.5f;
		const float WeaponSway = 0.125f;
		float speed = length(g_player.velocity.xy) * (WeaponScale / 320.f);
		float idle = WeaponSway * (sin(g_time) * .5f + .5f);

		vec3 offset = g_weapon_offset;
		offset.y += speed * sin(g_player.walk_cycle * 10.f) + idle;
		offset.z += speed * sin(g_player.walk_cycle * 20.f) * 0.25f + (WeaponSway - idle);
		transform.position += rotation * (offset * WeaponScale);
		transform.scale = WeaponScale;

		Demo::Uniform::Time.w = float(Demo::Entity::Type::weapon_rocketlauncher);
		Demo::Model::Draw(Demo::Model::ID::rocketl, transform);
	}

	////////////////////////////////////////////////////////////////

	struct Frame {
		vec3				pos;
		vec3				angles;
		float				fov;
		float				time;
		float				shadow_angle;
		Gfx::Texture::ID	render_target;
	};

	static constexpr mat4 ToYUp = {
		 0.f,  0.f, -1.f,  0.f,
		-1.f,  0.f,  0.f,  0.f,
		 0.f,  1.f,  0.f,  0.f,
		 0.f,  0.f,  0.f,  1.f,
	};

	NOINLINE void RenderWorld(const Frame& frame) {
		using namespace Math::constants;

		Uniform::Time.x = frame.time;

		mat4 rotation, translation, projection;
		transpose(MakeRotation(frame.angles * DEG2RAD), rotation);
		translation = i4x4;
		translation.SetPosition(-frame.pos);

		vec2 res = Gfx::GetResolution();
		vec2 fov(frame.fov * DEG2RAD);
		fov.y = ScaleFov(fov.x, res.y/res.x);

		MakePerspective(fov, 2.f, 8192.f, projection);
		Uniform::World = i4x4;
		Uniform::View = rotation * translation;
		Uniform::Cache::ViewProj = projection * ToYUp * Uniform::View;
		Uniform::MVP = Uniform::Cache::ViewProj;
		Uniform::Cam.xyz = frame.pos;
		Uniform::Cam.w = frame.shadow_angle * RAD2DEG;

		Gfx::SetRenderTarget(frame.render_target);
		Gfx::Clear(Gfx::ClearBit::Depth);
		Map::Render();
		RenderEntities();
		FlushDrawCalls();
	}

	FORCEINLINE void GenerateLevelShot() {
		const auto& levelshot = Map::source->levelshot;

		Frame frame;
		MemSet(&frame);

		frame.pos[0] = levelshot.position[0];
		frame.pos[1] = levelshot.position[1];
		frame.pos[2] = levelshot.position[2];
		frame.angles[0] = levelshot.angles[0];
		frame.angles[1] = levelshot.angles[1];
		frame.fov = 90.f;
		frame.render_target = Demo::Texture::Levelshot;

		RenderWorld(frame);

		Gfx::GenerateMipMaps(Demo::Texture::Levelshot);
	}

	////////////////////////////////////////////////////////////////

	namespace LoadingScreen {
		static constexpr char Text[] = 
			"LOADING "						"\0"
			"CONNECTING TO LOCALHOST"		"\0"
			"PRESS ESC TO ABORT"			"\0"
			" "								"\0"
			"LOADING... MAPS/"				"\0"
			" "								"\0"
			" "/* MAP MESSAGE */			"\0"
			"CHEATS ARE ENABLED"			"\0"
			"FREE FOR ALL"					"\0"
			"FRAGLIMIT 20"					"\0"
		;
		namespace Line {
			enum {
				MapName = 0,
				MapBSP = 4,
				MapMessage = 6,
			};
		}
	}
	static constexpr int LoadingScreenMessageLine = 6;

	FORCEINLINE void RenderLoadingScreen() {
		Gfx::SetRenderTarget(Gfx::Backbuffer);
		Gfx::SetShader(Demo::Shader::Loading);
		Demo::Uniform::Texture0 = Demo::Texture::Levelshot;
		Demo::Uniform::Time.x = g_time;
		Gfx::UpdateUniforms();
		Gfx::Clear(Gfx::ClearBit::ColorAndDepth);
		Gfx::DrawFullScreen();

		vec2 pos = Gfx::GetResolution() * vec2{0.5f, 0.125f};
		vec2 ui_scale = UI::GetScale();
		vec2 font_scale = UI::FontScale[UI::LargeFont] * ui_scale.y;

		using namespace LoadingScreen;

		int line_number = 0;
		for (const char* line = LoadingScreen::Text; *line; line = NextAfter(line), ++line_number) {
			const char* text = line;

			char buffer[256], *p = buffer;
			p += CopyString(p, line);
			p += CopyString(p, Map::source->name);

			if (line_number == Line::MapName) {
				text = buffer;
			} else if (line_number == Line::MapMessage) {
				text = Map::source->message;
			} else if (line_number == Line::MapBSP) {
				p += CopyString(p, ".BSP");
				text = buffer;
			}

			UI::PrintShadowed(text, pos, font_scale, -1, 0.5f, UI::LargeFont);
			pos.y += ui_scale.y * 80.f;
		}
		UI::FlushGeometry();
	}

	////////////////////////////////////////////////////////////////

	FORCEINLINE void RenderFrame() {
		if (IsLoading()) {
			RenderLoadingScreen();
			return;
		}

		if (!g_updated_lightmap) {
			Map::UpdateLightmapTexture();
			g_updated_lightmap = true;
			g_load_time = g_time;
		}

		Frame frame;
		frame.pos			= g_player.position;
		frame.pos.z			-= g_player.step;

		if (g_player.land_time > 0.f) {
			float land_frac;
			if (g_player.land_time > g_player.LandReturnTime) {
				land_frac = 1.f - (g_player.land_time - g_player.LandReturnTime) / g_player.LandDeflectTime;
			} else {
				land_frac = g_player.land_time / g_player.LandReturnTime;
			}
			frame.pos.z -= land_frac * g_player.land_change;
		}

		float speed = length(g_player.velocity.xy);
		float bob = speed / 640.f * sin(g_player.walk_cycle * 10.f);
		float bob_pitch = bob;

		frame.angles		= g_player.angles;
		frame.angles.y		-= abs(bob);
		frame.angles.z		+= bob;
		frame.fov			= mix(cg_fov.value, cg_zoomfov.value, g_player.zoom);
		frame.time			= g_time - g_load_time;
		frame.shadow_angle	= g_player.shadow_angle;
		frame.render_target	= Gfx::Backbuffer;

		Gfx::Sync();
		RenderWorld(frame);
		RenderDebug();
	}

	////////////////////////////////////////////////////////////////

	u32 g_screenshot_index;

	constexpr char ScreenshotFolder[] = "screenshots";
	constexpr char ScreenshotPrefix[] = "screenshots/screenshot_";
	constexpr char ScreenshotSuffix[] = ".tga";

	void TakeScreenshot() {
		if (!Sys::CreateFolder(ScreenshotFolder))
			return;

		char file_name[64];
		MemCopy(file_name, ScreenshotPrefix, size(ScreenshotPrefix) - 1);
		do {
			MemCopy(IntToString(++g_screenshot_index, file_name + (size(ScreenshotPrefix) - 1)), ScreenshotSuffix, size(ScreenshotSuffix));
			if (g_screenshot_index == 10000) {
				Sys::DebugStream << "Error: couldn't generate screenshot name.\n";
				return;
			}
		} while (Sys::FileExists(file_name));

		i32 width = Sys::g_window.width;
		i32 height = Sys::g_window.height;
		u32* pixels = Sys::Alloc<u32>(width * height);
		Gfx::ReadBack(Gfx::Backbuffer, pixels);
		Gfx::SaveTGA(file_name, pixels, width, height);
		Sys::Free(pixels);
	}

	////////////////////////////////////////////////////////////////

	float g_delta_time;
	u8 g_key_state[256];

	NOINLINE void LoadMap(Map::ID id) {
		Map::Load(id);
		Map::UpdateLightmapTexture();
		Demo::GenerateLevelShot();

		Demo::g_updated_lightmap = false;
		Demo::g_loading_thread.work = &Demo::GenerateLightmap;
		Sys::SpawnThread(Demo::g_loading_thread);

		Demo::g_player.Spawn();
	}

	FORCEINLINE void Tick(float dt) {
		assign_min(dt, 0.25f);
		if (g_delta_time == 0.f)
			g_delta_time = dt;
		dt = mix(g_delta_time, dt, 0.125f);
		g_delta_time = dt;

		Sys::UpdateKeyboardState();
		if (Sys::IsKeyFirstDown(Key::Escape))
			Sys::Exit();
		if (Sys::IsKeyFirstDown(Key::PrintScreen))
			TakeScreenshot();
		if (Sys::IsKeyFirstDown(Key::Backspace))
			g_player.Spawn();

		if (Sys::IsKeyFirstDown(Key::L))
			r_lightmap.Toggle();
		if (Sys::IsKeyFirstDown(Key::Backslash))
			g_player.flags ^= Player::Flag::NoClip;

		vec2 mouse;
		Sys::UpdateMouseState(mouse, dt);

		if (!IsLoading()) {
			float fov = mix(cg_fov.value, cg_zoomfov.value, g_player.zoom);
			float zoom_scale = tan(fov * (0.5f * Math::DEG2RAD)) / tan(cg_fov.value * (0.5f * Math::DEG2RAD));
			float mouse_scale = zoom_scale * sensitivity.value * -90.f / sqrt(Sys::g_window.width * Sys::g_window.height);
			g_player.angles.x += mouse.x * mouse_scale;
			g_player.angles.y += mouse.y * mouse_scale;
			g_player.angles.x = mod(g_player.angles.x, 360.f);
			g_player.angles.y = clamp(g_player.angles.y, -85.f, 85.f);

			g_player.Update(dt);

			if (Sys::IsKeyReleased(Key::F3)) {
				Map::ID next_map = Map::ID((u8(Map::current_id) + 1) % u8(Map::ID::Count));
				LoadMap(next_map);
			}
		}
	}

	////////////////////////////////////////////////////////////////

	static constexpr auto IconDescriptor = Texture::Descriptors[Texture::icon];
	static_assert(IconDescriptor.width == IconDescriptor.height, "Icon texture must be square");

	FORCEINLINE void UpdateWindowIcon() {
		u32* pixels = Sys::Alloc<u32>(IconDescriptor.width * IconDescriptor.height);
		Gfx::ReadBack(Texture::icon, pixels);
		Sys::SetWindowIcon(&Sys::g_window, pixels, IconDescriptor.width);
#ifdef SAVE_ICON
		Gfx::SaveTGA("icon.tga", pixels, IconDescriptor.width, IconDescriptor.height);
#endif
		Sys::Free(pixels);
	}

	////////////////////////////////////////////////////////////////

	FORCEINLINE void Init() {
		Sys::Printf("%s", "Starting up...\n");

		Mem::Init();
		Demo::Console::Init();
		Sys::InitWindow(&Sys::g_window, nullptr, "Q320");
		Sys::SetFPSMode(&Sys::g_window);
		Gfx::InitMemory(GPUPoolSizes);
		Demo::InitGfxResources();
		Map::AllocLightmap();
		Demo::UpdateWindowIcon();
		Demo::Model::LoadAll(cooked_models);

#ifdef SHOW_LIGHTMAP
		Demo::r_lightmap.Set(1);
#endif

#ifdef FULLBRIGHT
		Demo::r_fullbright.Set(1);
#endif

		auto touch = [](auto& src) { MemCopy(Mem::Alloc(sizeof(src)), &src, sizeof(src)); };

		LoadMap(Map::ID::START_MAP);

		g_weapon_offset = {-9.f, -5.f, -9.f};
	}
}

////////////////////////////////////////////////////////////////

int FORCEINLINE demo_main() {
	Demo::Init();

	auto last_tick = Sys::GetTime();
	auto next_time = last_tick;

	int code = 0;

	while (Sys::PumpMessages(&code)) {
		Demo::g_time = Sys::GetTime();
		Demo::Tick(Demo::g_time - last_tick);
		last_tick = Demo::g_time;

		Demo::RenderFrame();
		Gfx::Present();

		float fps = Demo::com_maxFps.value;
		if (fps == 0.f)
			fps = Sys::g_window.refresh;
		float interval = fps > 0.f ? 1.f / fps : 0.f;

		float wait = 0.f;
		if (interval > 0.f) {
			auto now = Sys::GetTime();
			wait = next_time - now;
			if (wait <= 0.f) {
				next_time = now + interval;
				wait = 0.f;
			} else {
				next_time += interval;
			}
		}
		Sys::PreciseSleep(wait);
	}

	return code;
}
