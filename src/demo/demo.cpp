//#define ENABLE_RENDERDOC
//#define DISABLE_SHADER_CACHE
#define USE_RAW_INPUT
//#define DISABLE_PARTITION
//#define DRAW_LIGHTS
//#define DRAW_POINT_ENTITIES
//#define DRAW_CLIPPING
//#define SAVE_TEXTURE gblks15
//#define SAVE_ICON
//#define SAVE_LIGHTGRID
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
#include "entity_def.h"
#include "packed_map.h"
#include "gfx_resources.h"
#include "models.h"
#include "cooked/cooked_models.h"
#include "cooked/cooked_maps.h"
#include "map.h"
#include "player.h"
#include "game_state.h"
#include "menu.h"

////////////////////////////////////////////////////////////////

namespace Demo {
	void RenderSprite(const vec3& point, float size) {
		using namespace Demo;

		vec3 pos[4];
		vec3 nor[4];
		vec2 uv[4];

		Gfx::Mesh mesh;
		memset(&mesh, 0, sizeof(mesh));

		mat4 rotation = MakeRotation(g_player.angles * Math::DEG2RAD);
		vec3 x = rotation.y.xyz * size;
		vec3 y = rotation.z.xyz * size;
		vec3 z = rotation.x.xyz;

		nor[0] = nor[1] = nor[2] = nor[3] = -z;

		pos[0] = point + x - y;
		pos[1] = point - x - y;
		pos[2] = point - x + y;
		pos[3] = point + x + y;

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
		for (u16 i = 0; i < Map::lights.count; ++i) {
			auto& light = Map::lights.data[i];
			vec3 pos;
			pos[0] = light.position[0];
			pos[1] = light.position[1];
			pos[2] = light.position[2];
			RenderSprite(light.position, max(light.intensity / 25.f, 2.f));
		}
	}

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

	void RenderDebug() {
#ifdef DRAW_LIGHTS
		DrawLights();
#endif

#ifdef DRAW_POINT_ENTITIES
		DrawPointEntities();
#endif
	}

	////////////////////////////////////////////////////////////////

	constexpr u32 MakeColor(u8 r, u8 g, u8 b, u8 a = 255) {
		return r | (g << 8) | (b << 16) | (a << 24);
	}

	static constexpr u8 EntityModels[] = {
		#define PP_ADD_ENTITY_TYPE_MODEL(name, desc, model, ...) 1 + model,
		DEMO_ENTITY_TYPES(PP_ADD_ENTITY_TYPE_MODEL)
		#undef PP_ADD_ENTITY_TYPE_MODEL
	};

	static constexpr u32 EntityModelColors[] = {
		#define PP_ADD_ENTITY_TYPE_COLOR(name, desc, model, r, g, b, ...) MakeColor(r, g, b, 0),
		DEMO_ENTITY_TYPES(PP_ADD_ENTITY_TYPE_COLOR)
		#undef PP_ADD_ENTITY_TYPE_COLOR
	};

	////////////////////////////////////////////////////////////////

	static constexpr StaticArray<float, 3> GetWeaponModelOffset(Entity::Type type) {
		switch (type) {
			case Entity::Type::weapon_gauntlet:			return { -9.0f, -5.0f,  -9.0f };
			case Entity::Type::weapon_machinegun:		return {  4.0f, -3.5f,  -7.0f };
			case Entity::Type::weapon_shotgun:			return { -1.0f, -2.5f,  -8.0f };
			case Entity::Type::weapon_rocketlauncher:	return { -9.0f, -5.0f,  -9.0f };
			case Entity::Type::weapon_railgun:			return {  1.5f, -5.0f, -10.5f };
			case Entity::Type::weapon_plasmagun:		return { -1.0f, -5.0f, -10.0f };
			default:
				return { 0.f, 0.f, 0.f };
		}
	};

	static constexpr auto WeaponModelOffsets = MakeLookupTable<
		Entity::Type,
		StaticArray<float, 3>,
		Entity::Type::WeaponStart,
		Entity::Type(i16(Entity::Type::WeaponStart) + i16(Demo::Entity::Type::WeaponCount) - 1)
	>(GetWeaponModelOffset);

	FORCEINLINE void RenderEntities() {
		Demo::Model::Transform transform;

		for (u32 entity_index = Map::num_brush_entities; entity_index < Map::num_entities; ++entity_index) {
			using Type = Demo::Entity::Type;
			const Demo::Entity& ent = Map::entities[entity_index];

			float spawn = 1.f - ent.respawn / Demo::Entity::RespawnAnimTime;
			if (spawn <= 0.f)
				continue;

			MemCopy(&transform, &Demo::Model::TransformIdentity);
			for (u32 i = 0; i < 3; ++i)
				transform.position[i] = ent.origin[i];
			float phase = (transform.position[0] + transform.position[1]) / 1024.f;
			transform.position[2] += 4.f + 4.f * sin(Math::TAU * (float(g_level_time) + phase));
			transform.angles[0] = ent.angle + float(g_level_time) * 180.f;

			if (ent.IsWeapon()) {
				transform.scale = 1.5f;
				transform.position[2] += 8.f;
			}
			transform.scale *= spawn;

			int model_id = int(EntityModels[(u32)ent.type]) - 1;
			u32 model_color = EntityModelColors[(u32)ent.type];
			if (model_id != -1) {
				for (u32 i = 0; i < 3; ++i, model_color >>= 8)
					Demo::Uniform::Time[i + 1] = (model_color & 255) / 255.f;
				Map::DrawLitModel(Demo::Model::ID(model_id), transform);

				if (ent.IsHealth()) {
					MemSet(&transform.angles);
					if (ent.type == Type::item_health) {
						// HACK: color >1.0 = sharper, stationary reflection
						MemCopy(&Uniform::Time[1], vec3{1.25f, 1.25f, 1.25f}.data, 3);
					}
					Map::DrawLitModel(Demo::Model::ID::large_sphere, transform);
				}

				if (ent.type == Entity::Type::item_quad) {
					transform.angles[0] *= -2.f;
					transform.position[2] += 12.f;
					Map::DrawLitModel(Demo::Model::ID::quad_ring, transform);
				}
			}
		}
	}

	vec3 g_weapon_offset;

	FORCEINLINE void RenderViewModel() {
		Demo::Model::Transform transform;
		MemCopy(&transform, &Demo::Model::TransformIdentity);
		transform.angles = g_player.angles;

		const float DefaultVerticalFov = 58.75f; // approximation
		float fov_delta = (g_fov.y / Math::DEG2RAD - DefaultVerticalFov) * 0.5f;
		if (fov_delta > 0.f)
			transform.angles[1] -= fov_delta;

		mat4 rotation = MakeRotation(transform.angles * Math::DEG2RAD);
		transform.position = g_player.position;
		transform.position.z -= g_player.step;
		mix_into(transform.position, Demo::Uniform::Cam.xyz, 17.f/16.f);

		const float WeaponScale = 0.5f;
		const float WeaponSway = 0.125f;
		float speed = length(g_player.velocity.xy) * (WeaponScale / 320.f);
		float idle = WeaponSway * (sin((float)g_level_time) * .5f + .5f);

		Entity::Type weapon = g_player.weapon;
		i32 model_id = i32(EntityModels[(u32)weapon]) - 1;
		if (u32(model_id) >= Model::Count)
			return;

		vec3 offset = *(const vec3*)&WeaponModelOffsets[g_player.weapon];
		//vec3 offset = g_weapon_offset;
		offset.y += speed * sin(g_player.walk_cycle * 10.f) + idle;
		offset.z += speed * sin(g_player.walk_cycle * 20.f) * 0.25f + (WeaponSway - idle);

		transform.position += rotation * (offset * WeaponScale);
		transform.scale = WeaponScale;

		Map::DrawLitModel(Model::ID(model_id), transform);
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
		translation.GetPosition() -= frame.pos;

		vec2 res = Gfx::GetResolution();
		g_fov.x = frame.fov * DEG2RAD;
		g_fov.y = ScaleFov(g_fov.x, res.y/res.x);

		MakePerspective(g_fov, 2.f, 8192.f, projection);
		Uniform::World = i4x4;
		Uniform::View = rotation * translation;
		Uniform::Cache::ViewProj = projection * ToYUp * Uniform::View;
		Uniform::MVP = Uniform::Cache::ViewProj;
		Uniform::Cam.xyz = frame.pos;
		Uniform::Cam.w = frame.shadow_angle * RAD2DEG;

		Gfx::SetRenderTarget(frame.render_target, &Gfx::Clear::Depth);
		Map::Render();
		RenderEntities();
		if (frame.render_target == Gfx::Backbuffer)
			RenderViewModel();
		FlushDrawCalls();
	}

	FORCEINLINE void GenerateLevelShots() {
		for (u32 map_index = 0; map_index < (u32)Map::ID::Count; ++map_index) {
			Map::Load(Map::ID(map_index));
			Map::ComputeLighting(Map::LightMode::Draft);
			Map::UpdateLightmapTexture();

			const auto& levelshot = Map::source->levelshot;

			Frame frame;
			MemSet(&frame);

			frame.pos[0] = levelshot.position[0];
			frame.pos[1] = levelshot.position[1];
			frame.pos[2] = levelshot.position[2];
			frame.angles[0] = levelshot.angles[0];
			frame.angles[1] = levelshot.angles[1];
			frame.fov = 90.f;
			frame.render_target = levelshot.texture;

			RenderWorld(frame);

			Gfx::GenerateMipMaps(levelshot.texture);
		}

		Map::Load(Map::ID::None);
	}

	////////////////////////////////////////////////////////////////

	namespace LoadingScreen {
		static constexpr char Text[] = 
			"loading "						"\0"
			"connecting to localhost"		"\0"
			"press esc to abort"			"\0"
			" "								"\0"
			"loading... maps/"				"\0"
			" "								"\0"
			" "/* map message */			"\0"
			"cheats are enabled"			"\0"
			"free for all"					"\0"
			"fraglimit 20"					"\0"
		;
		namespace Line {
			enum {
				MapName = 0,
				MapBSP = 4,
				MapMessage = 6,
			};
		}
	}

	FORCEINLINE void RenderLoadingScreen() {
		Gfx::SetRenderTarget(Gfx::Backbuffer, &Gfx::Clear::ColorAndDepth);
		Gfx::SetShader(Demo::Shader::Loading);
		Demo::Uniform::Texture0 = Map::source->levelshot.texture;
		Demo::Uniform::Time.x = (float)g_time;
		Gfx::UpdateUniforms();
		Gfx::DrawFullScreen();

		const float line_height = 80.f;
		const vec2& font_scale = UI::FontScale[UI::LargeFont];
		vec2 pos = {0.f, line_height * 5.f};

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
			pos.y -= line_height;
		}
		UI::FlushGeometry();
	}

	////////////////////////////////////////////////////////////////

	FORCEINLINE void RenderFrame() {
		if (Map::IsLoaded()) {
			if (!IsMapReady()) {
				RenderLoadingScreen();
				return;
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

			frame.angles		= g_player.angles;
			frame.angles.y		-= abs(bob);
			frame.angles.z		+= bob;
			frame.fov			= mix(cg_fov.value, cg_zoomfov.value, g_player.zoom);
			frame.time			= float(g_level_time);
			frame.shadow_angle	= g_player.shadow_angle;
			frame.render_target	= Gfx::Backbuffer;

			Gfx::Sync();
			RenderWorld(frame);
			RenderDebug();
		} else {
			Gfx::SetRenderTarget(Gfx::Backbuffer, &Gfx::Clear::ColorAndDepth);
		}

		Menu::Draw();
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

	FORCEINLINE void Tick(float dt) {
		assign_min(dt, 0.25f);
		if (g_delta_time == 0.f)
			g_delta_time = dt;
		dt = mix(g_delta_time, dt, 0.125f);
		g_delta_time = dt;

		vec2 mouse_delta;
		Sys::UpdateMouseState(mouse_delta, dt);
		Sys::UpdateKeyboardState();

		if (Sys::IsKeyFirstDown(Key::PrintScreen))
			TakeScreenshot();

		if (Menu::Update(dt))
			return;

		UpdateGameState(dt, mouse_delta);
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
		Demo::GenerateLevelShots();
		Demo::Menu::Init();

#ifdef SHOW_LIGHTMAP
		Demo::r_lightmap.Set(1);
#endif

#ifdef FULLBRIGHT
		Demo::r_fullbright.Set(1);
#endif

		[[maybe_unused]] auto touch = [](auto& src) { MemCopy(Mem::Alloc(sizeof(src)), &src, sizeof(src)); };

		g_weapon_offset = {-9.f, -5.f, -9.f}; // rocket launcher
		//g_weapon_offset = {-1.f, -2.5f, -8.f}; // shotgun
		//g_weapon_offset = {1.5f, -5.f, -10.5f}; // railgun
		//g_weapon_offset = {-1.f, -5.f, -10.f}; // plasma
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
		Demo::Tick(float(Demo::g_time - last_tick));
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
			wait = float(next_time - now);
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
