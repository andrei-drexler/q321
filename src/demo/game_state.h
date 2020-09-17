#pragma once

namespace Demo {
	Sys::Thread				g_loading_thread;
	bool					g_updated_lightmap;
	Sys::Time				g_level_time;
	Sys::Time				g_time;
	float					g_delta_time;

	////////////////////////////////////////////////////////////////

	FORCEINLINE bool IsMapReady() {
		return Sys::IsThreadReady(g_loading_thread);
	}

	void GenerateLightmap(void*) {
		g_updated_lightmap = false;
		Map::ComputeLighting(Map::LightMode::Shadows);
#ifdef ENABLE_RADIOSITY
		if (r_bounce.integer)
			Map::ComputeLighting(Map::LightMode::Bounce);
#endif
	}

	NOINLINE void LoadMap(Map::ID id) {
		Map::Unpack(id);
		if (!Map::IsUnpacked())
			return;
		Map::UpdateLightmapTexture();

		Map::lightmap.abort = false;
		g_updated_lightmap = false;
		g_loading_thread.work = &GenerateLightmap;
		Sys::SpawnThread(Demo::g_loading_thread);

		Demo::g_player.Spawn();
	}

	FORCEINLINE void LoadNextMap() {
		LoadMap(NextAfter(Map::current_id));
	}

	////////////////////////////////////////////////////////////////

	FORCEINLINE void UpdateGameState(float dt, const vec2& mouse_delta) {
		if (Map::IsUnpacked() && IsMapReady()) {
			g_level_time += dt;

			for (u32 entity_index = Map::num_brush_entities; entity_index < Map::num_entities; ++entity_index) {
				Demo::Entity& entity = Map::entities[entity_index];
				entity.respawn -= dt;
				if (entity.respawn < 0.f)
					entity.respawn = 0.f;
			}

			if (Sys::IsKeyFirstDown(Key::Backspace))
				g_player.Spawn();
			if (Sys::IsKeyFirstDown(Key::L))
				r_lightmap.Toggle();
			if (Sys::IsKeyFirstDown(Key::Backslash))
				g_player.flags ^= Player::Flag::NoClip;

			float fov = mix(cg_fov.value, cg_zoomfov.value, g_player.zoom);
			float zoom_scale = tan(fov * (0.5f * Math::DEG2RAD)) / tan(cg_fov.value * (0.5f * Math::DEG2RAD));
			float mouse_scale = zoom_scale * sensitivity.value * -90.f / sqrt(float(Sys::g_window.width * Sys::g_window.height));
			g_player.angles.x += mouse_delta.x * mouse_scale;
			g_player.angles.y += mouse_delta.y * mouse_scale;
			g_player.angles.x = mod(g_player.angles.x, 360.f);
			g_player.angles.y = clamp(g_player.angles.y, -85.f, 85.f);

			g_player.Update(dt);
		}
	}
}
