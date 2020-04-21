#pragma once

namespace Q3 {
	namespace Content {
		enum Flags : uint32_t {
			SOLID				= 1,
			LAVA				= 8,
			SLIME				= 16,
			WATER				= 32,
			FOG					= 64,
			
			AREAPORTAL			= 0x8000,

			PLAYERCLIP			= 0x10000,
			MONSTERCLIP			= 0x20000,

			//bot specific contents types
			TELEPORTER			= 0x40000,
			JUMPPAD				= 0x80000,
			CLUSTERPORTAL		= 0x100000,
			DONOTENTER			= 0x200000,
			BOTCLIP				= 0x400000,

			ORIGIN				= 0x1000000,	// removed before bsping an entity

			BODY				= 0x2000000,	// should never be on a brush, only in game
			CORPSE				= 0x4000000,
			DETAIL				= 0x8000000,	// brushes not used for the bsp
			STRUCTURAL			= 0x10000000,	// brushes used for the bsp
			TRANSLUCENT			= 0x20000000,	// don't consume surface fragments inside
			TRIGGER				= 0x40000000,
			NODROP				= 0x80000000,	// don't leave bodies or items (death fog, lava)

			DEFAULT				= SOLID | STRUCTURAL,
		};
	}

	namespace Surface {
		enum Flags : uint32_t {
			NODAMAGE			= 0x1,			// never give falling damage
			SLICK				= 0x2,			// effects game physics
			SKY					= 0x4,			// lighting from environment map
			LADDER				= 0x8,
			NOIMPACT			= 0x10,			// don't make missile explosions
			NOMARKS				= 0x20,			// don't leave missile marks
			FLESH				= 0x40,			// make flesh sounds and effects
			NODRAW				= 0x80,			// don't generate a drawsurface at all
			HINT				= 0x100,		// make a primary bsp splitter
			SKIP				= 0x200,		// completely ignore, allowing non-closed brushes
			NOLIGHTMAP			= 0x400,		// surface doesn't need a lightmap
			POINTLIGHT			= 0x800,		// generate lighting info at vertexes
			METALSTEPS			= 0x1000,		// clanking footsteps
			NOSTEPS				= 0x2000,		// no footstep sounds
			NONSOLID			= 0x4000,		// don't collide against curves with this set
			LIGHTFILTER			= 0x8000,		// act as a light filter during q3map -light
			ALPHASHADOW			= 0x10000,		// do per-pixel light shadow casting in q3map
			NODLIGHT			= 0x20000,		// don't dlight even if solid (solid lava, skies)
			DUST				= 0x40000,		// leave a dust trail when walking on this surface

			DEFAULT				= 0,
		};
	}

	struct Map {
		struct Material {
			std::string name;

			using ID = int32_t;
		};

		struct Plane : vec4 {
			vec2			shift				= {0.f, 0.f};
			float			rotation			= 0.f;
			vec2			scale				= {0.f, 0.f};
			Content::Flags	content_flags		= Content::DEFAULT;
			Surface::Flags	surface_flags		= Surface::DEFAULT;
			uint32_t		value				= 0;
			Material::ID	material			= -1;

			enum Type : uint8_t {
				XNeg, XPos,
				YNeg, YPos,
				ZNeg, ZPos,
				Unaligned,

				FIRST = XNeg,
			};

			Type					type		= Unaligned;
			bool					synthetic	= false;

			void					SetCorners(const vec3& a, const vec3& b, const vec3& c);

			static uint8_t			GetAxis(Type type) { return type >> 1; }
			static bool				IsMaxSide(Type type) { return type & 1; }
		};

		struct Brush {
			std::vector<Plane>		planes;
			AABB					bounds;
			bool					axial = false;

			void					Finalize();
		};

		struct Patch {
			struct Vertex {
				vec3 pos;
				vec2 uv;
			};
			
			std::vector<Vertex>		vertices;
			uint32_t				width = 0;
			uint32_t				height = 0;
			Material::ID			material = -1;
			AABB					bounds;
		};

		struct Entity {
			using PropertyMap = std::unordered_map<std::string, std::string>;
			PropertyMap				props;
			std::vector<Brush>		brushes;
			std::vector<Patch>		patches;
			AABB					bounds;

			string_view				GetProperty(string_view name) const {
				auto i = props.find(std::string{name}); // string copy :(
				return i != props.end() ? i->second : string_view{};
			}
		};

		std::vector<Entity>			entities;
		std::vector<Material>		materials;
		AABB						bounds;

		std::string					error;

		Map() = default;

		Material::ID				AddMaterial(string_view name);
		Material::ID				FindMaterial(string_view name) const;

		Entity&						World() { return entities[0]; }
		const Entity&				World() const { return entities[0]; }

		bool						Parse(string_view source);
		bool						ParseEntity(string_view& source);
		bool						Error(const char* format, ...);

		void						ComputeBounds();
	};
}

////////////////////////////////////////////////////////////////

bool Q3::Map::Error(const char* format, ...) {
	char buf[4096];
	va_list args;
	va_start(args, format);
	int result = vsnprintf(buf, std::size(buf), format, args);
	va_end(args);

	buf[std::size(buf)-1] = 0;
	Entity* ent = entities.empty() ? nullptr : &entities.back();
	printf("ERROR: %s (entity %zd, brush %zd/patch %zd)\n", buf,
		entities.size(), ent ? ent->brushes.size() : 0, ent ? ent->patches.size() : 0);

	error = buf;

	return false;
}

Q3::Map::Material::ID Q3::Map::FindMaterial(string_view name) const {
	for (Material::ID i=0; i<materials.size(); ++i)
		if (name == materials[i].name)
			return i;
	return -1;
}

Q3::Map::Material::ID Q3::Map::AddMaterial(string_view name) {
	if (name.empty())
		return -1;
	auto index = FindMaterial(name);
	if (index != -1)
		return index;
	auto& m = materials.emplace_back();
	m.name = name;
	return Material::ID(materials.size() - 1);
}

void Q3::Map::Plane::SetCorners(const vec3& a, const vec3& b, const vec3& c) {
	vec3 ba, bc;
	ba = a; ba -= b;
	bc = c; bc -= b;
	cross(ba, bc, this->xyz);

	float scale = dot(this->xyz, this->xyz);
	if (scale > 0.f)
		this->xyz /= sqrtf(scale);
	w = -dot(this->xyz, a);

	constexpr float AlignedThresh = 1.f - 1e-6f;
	if (abs(x) >= AlignedThresh) {
		this->type = x < 0.f ? XNeg : XPos;
		this->x = x < 0.f ? -1.f : 1.f;
		this->y = 0.f;
		this->z = 0.f;
	} else if (abs(y) >= AlignedThresh) {
		this->type = y < 0.f ? YNeg : YPos;
		this->y = y < 0.f ? -1.f : 1.f;
		this->x = 0.f;
		this->z = 0.f;
	} else if (abs(z) >= AlignedThresh) {
		this->type = z < 0.f ? ZNeg : ZPos;
		this->z = z < 0.f ? -1.f : 1.f;
		this->x = 0.f;
		this->y = 0.f;
	} else {
		this->type = Unaligned;
		for (int i=0; i<4; ++i)
			if (this->data[i] == -0.f)
				this->data[i] = 0.f;
	}
}

void Q3::Map::Brush::Finalize() {
	constexpr uint32_t AxialMask =
		(1 << Plane::XPos) | (1 << Plane::XNeg) |
		(1 << Plane::YPos) | (1 << Plane::YNeg) |
		(1 << Plane::ZPos) | (1 << Plane::ZNeg) ;

	uint32_t mask = 0;
	if (this->planes.size() == 6)
		for (auto& plane : this->planes)
			mask |= 1 << plane.type;
	
	this->axial = (mask == AxialMask);
}

bool Q3::Map::ParseEntity(string_view& source) {
	SkipWhitespace(source);
	if (source.empty())
		return false;

	if (!Consume(source, "{"sv))
		return Error("Could not find entity beginning");
	
	Entity entity;
	while (!source.empty()) {
		if (Consume(source, "}"sv)) {
			entities.push_back(std::move(entity));
			return true;
		}
		
		string_view key = ParseQuotedString(source);
		if (!key.empty()) {
			auto value = ParseQuotedString(source);
			if (value.empty())
				return Error("Missing value for key %.*s", int(key.size()), key.data());
			entity.props[std::string(key.substr(1, key.size() - 2))] = value.substr(1, value.size() - 2);
			continue;
		}

		if (Consume(source, "{"sv)) {
			if (Consume(source, "patchDef2"sv)) {
				if (!Consume(source, "{"sv))
					return Error("Could not find patch beginning");

				Patch patch;
				patch.material = AddMaterial(ParseWord(source));

				if (!Consume(source, "("sv))
					return false;

				if (!ParseValue(source, patch.height) || !ParseValue(source, patch.width))
					return false;
				int ignore = 0;
				for (int i=0; i<3; ++i)
					if (!ParseValue(source, ignore))
						return false;

				if (!Consume(source, ")"sv))
					return false;

				if (!Consume(source, "("sv))
					return false;
					
				patch.vertices.resize(patch.width * patch.height);
				for (uint32_t y=0; y<patch.height; ++y) {
					if (!Consume(source, "("sv))
						return false;
					
					for (uint32_t x=0; x<patch.width; ++x) {
						if (!Consume(source, "("sv))
							return false;

						auto& vertex = patch.vertices[y * patch.width + x];
						for (int i=0; i<3; ++i)
							if (!ParseValue(source, vertex.pos.data[i]))
								return false;
						for (int i=0; i<2; ++i)
							if (!ParseValue(source, vertex.uv.data[i]))
								return false;
						
						if (!Consume(source, ")"sv))
							return false;
					}
						
					if (!Consume(source, ")"sv))
						return false;
				}

				if (!Consume(source, ")"sv))
					return false;

				// end patch
				if (!Consume(source, "}"sv))
					return false;

				// end brush
				if (!Consume(source, "}"sv))
					return false;

				entity.patches.push_back(std::move(patch));
				continue;
			}

			Brush brush;
			brush.planes.reserve(8);
			while (!source.empty()) {
				if (Consume(source, "}"sv)) {
					brush.Finalize();
					entity.brushes.push_back(std::move(brush));
					break;
				}

				vec3 corners[3];
				for (int i=0; i<3; ++i)
					if (!ParseVector(source, corners[i]))
						return false;

				Plane plane;
				plane.material = AddMaterial(ParseWord(source));
				if (!ParseValue(source, plane.shift.x) || !ParseValue(source, plane.shift.y) ||
					!ParseValue(source, plane.rotation) ||
					!ParseValue(source, plane.scale.x) || !ParseValue(source, plane.scale.y) ||
					!ParseValue(source, plane.content_flags) || !ParseValue(source, plane.surface_flags) || !ParseValue(source, plane.value))
					return false;

				plane.rotation = fmodf(plane.rotation, 360.f);
				if (plane.rotation < 0.f)
					plane.rotation += 360.f;
				plane.SetCorners(corners[0], corners[1], corners[2]);

				brush.planes.push_back(std::move(plane));
			}
			continue;
		}

		return false;
	}

	return false;
}

bool Q3::Map::Parse(string_view source) {
	while (!source.empty()) {
		SkipWhitespace(source);
		if (source.empty())
			break;
		if (!ParseEntity(source))
			return false;
	}
	return !entities.empty();
}

void Q3::Map::ComputeBounds() {
	bounds.clear();

	for (auto& ent : entities) {
		ent.bounds.clear();

		if (&ent - entities.data() == 19) {
			int abcd = 0;
		}

		for (auto& brush : ent.brushes) {
			brush.bounds.clear();

			const size_t MAX_NUM_EDGES = 256;
			BrushEdge edges[MAX_NUM_EDGES];

			size_t num_edges = EnumerateBrushEdges(brush.planes.data(), brush.planes.size(), edges, MAX_NUM_EDGES);
			for (size_t i=0; i<num_edges; ++i) {
				brush.bounds.add(edges[i].first_point);
				brush.bounds.add(edges[i].second_point);
			}

			ent.bounds.add(brush.bounds);
		}

		for (auto& patch : ent.patches) {
			patch.bounds.clear();
			for (auto& vertex : patch.vertices)
				patch.bounds.add(vertex.pos);
			ent.bounds.add(patch.bounds);
		}

		bounds.add(ent.bounds);
	}
}
